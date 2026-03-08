import { entityKind } from "drizzle-orm/entity";
import { DefaultLogger, NoopLogger, type Logger } from "drizzle-orm/logger";
import { PgDatabase } from "drizzle-orm/pg-core/db";
import { PgDialect } from "drizzle-orm/pg-core/dialect";
import {
  PgPreparedQuery,
  PgSession,
  PgTransaction,
  type PgQueryResultHKT,
  type PgTransactionConfig,
  type PreparedQueryConfig,
} from "drizzle-orm/pg-core/session";
import type { SelectedFieldsOrdered } from "drizzle-orm/pg-core/query-builders/select.types";
import {
  type RelationalSchemaConfig,
  type TablesRelationalConfig,
  extractTablesRelationalConfig,
  createTableRelationsHelpers,
} from "drizzle-orm/relations";
import { type Query, type SQL, fillPlaceholders, sql } from "drizzle-orm/sql";
import type { Assume, DrizzleConfig } from "drizzle-orm/utils";
import type { WithCacheConfig } from "drizzle-orm/cache/core/types";

// drizzle-orm exports mapResultRow at runtime but not in its type declarations
// @ts-ignore
import { mapResultRow } from "drizzle-orm/utils";

export interface PetraDBSession {
  execute(
    sql: string,
    options?: { rowMode?: "object" | "array" },
  ): Promise<any[]>;
  prepare(sql: string): {
    execute(
      params?: any[],
      options?: { rowMode?: "object" | "array" },
    ): Promise<any[]>;
  };
  close(): Promise<void>;
}

export interface PetraDbQueryResult {
  rows: Record<string, unknown>[];
  rowCount: number;
}

class PetraDbPreparedQuery<
  T extends PreparedQueryConfig,
> extends PgPreparedQuery<T> {
  static override readonly [entityKind] = "PetraDbPreparedQuery";

  constructor(
    private petraSession: PetraDBSession,
    private queryString: string,
    private params: unknown[],
    private logger: Logger,
    private fields: SelectedFieldsOrdered | undefined,
    private _isResponseInArrayMode: boolean,
    private customResultMapper?:
      | ((rows: unknown[][]) => T["execute"])
      | undefined,
    query?: Query,
    queryMetadata?: {
      type: "select" | "update" | "delete" | "insert";
      tables: string[];
    },
    cacheConfig?: WithCacheConfig,
  ) {
    super(
      query ?? { sql: queryString, params },
      undefined,
      queryMetadata,
      cacheConfig,
    );
  }

  async execute(
    placeholderValues: Record<string, unknown> = {},
  ): Promise<T["execute"]> {
    const params = fillPlaceholders(this.params, placeholderValues);
    this.logger.logQuery(this.queryString, params);

    const {
      fields,
      queryString,
      petraSession,
      customResultMapper,
    } = this;
    // joinsNotNullableMap is set internally by drizzle query builders
    const joinsNotNullableMap = (this as any).joinsNotNullableMap;

    if (!fields && !customResultMapper) {
      // Raw execute path (INSERT/UPDATE/DELETE without RETURNING, or raw SQL)
      const results = await this._exec(queryString, params, "object");
      const resultSet = results[0];
      return {
        rows: resultSet?.rows ?? [],
        rowCount: resultSet?.rows?.length ?? 0,
      } as T["execute"];
    }

    // Query with field mapping (SELECT, or mutations with RETURNING)
    const results = await this._exec(queryString, params, "array");
    const resultSet = results[0];
    const rows: unknown[][] = resultSet?.rows ?? [];

    if (customResultMapper) {
      return customResultMapper(rows) as T["execute"];
    }

    return rows.map((row: unknown[]) =>
      mapResultRow(fields!, row, joinsNotNullableMap),
    ) as T["execute"];
  }

  private async _exec(
    queryString: string,
    params: unknown[],
    rowMode: "object" | "array",
  ): Promise<any[]> {
    if (params.length > 0) {
      const stmt = this.petraSession.prepare(queryString);
      return stmt.execute(params as any[], { rowMode });
    }
    return this.petraSession.execute(queryString, { rowMode });
  }
}

class PetraDbSession<
  TFullSchema extends Record<string, unknown>,
  TSchema extends TablesRelationalConfig,
> extends PgSession<PetraDbQueryResultHKT, TFullSchema, TSchema> {
  static override readonly [entityKind] = "PetraDbSession";

  private logger: Logger;

  constructor(
    public petraSession: PetraDBSession,
    dialect: PgDialect,
    private schema: RelationalSchemaConfig<TSchema> | undefined,
    options: { logger?: Logger } = {},
  ) {
    super(dialect);
    this.logger = options.logger ?? new NoopLogger();
  }

  prepareQuery<T extends PreparedQueryConfig = PreparedQueryConfig>(
    query: Query,
    fields: SelectedFieldsOrdered | undefined,
    name: string | undefined,
    isResponseInArrayMode: boolean,
    customResultMapper?: (rows: unknown[][]) => T["execute"],
    queryMetadata?: {
      type: "select" | "update" | "delete" | "insert";
      tables: string[];
    },
    cacheConfig?: WithCacheConfig,
  ): PgPreparedQuery<T> {
    return new PetraDbPreparedQuery<T>(
      this.petraSession,
      query.sql,
      query.params,
      this.logger,
      fields,
      isResponseInArrayMode,
      customResultMapper,
      query,
      queryMetadata,
      cacheConfig,
    );
  }

  async transaction<T>(
    transaction: (
      tx: PetraDbTransaction<TFullSchema, TSchema>,
    ) => Promise<T>,
    config?: PgTransactionConfig,
  ): Promise<T> {
    const tx = new PetraDbTransaction<TFullSchema, TSchema>(
      this.dialect,
      this,
      this.schema,
    );

    await tx.execute(
      sql`begin${config ? sql` ${(tx as any).getTransactionConfigSQL(config)}` : undefined}`,
    );
    try {
      const result = await transaction(tx);
      await tx.execute(sql`commit`);
      return result;
    } catch (error) {
      await tx.execute(sql`rollback`);
      throw error;
    }
  }

  override async count(query: SQL): Promise<number> {
    const res = await this.execute<PetraDbQueryResult>(query);
    return Number(res.rows[0]["count"]);
  }
}

class PetraDbTransaction<
  TFullSchema extends Record<string, unknown>,
  TSchema extends TablesRelationalConfig,
> extends PgTransaction<PetraDbQueryResultHKT, TFullSchema, TSchema> {
  static override readonly [entityKind] = "PetraDbTransaction";

  async transaction<T>(
    transaction: (
      tx: PetraDbTransaction<TFullSchema, TSchema>,
    ) => Promise<T>,
  ): Promise<T> {
    const savepointName = `sp${this.nestedIndex + 1}`;
    const tx = new PetraDbTransaction<TFullSchema, TSchema>(
      (this as any).dialect,
      (this as any).session,
      this.schema,
      this.nestedIndex + 1,
    );
    await tx.execute(sql.raw(`savepoint ${savepointName}`));
    try {
      const result = await transaction(tx);
      await tx.execute(sql.raw(`release savepoint ${savepointName}`));
      return result;
    } catch (err) {
      await tx.execute(sql.raw(`rollback to savepoint ${savepointName}`));
      throw err;
    }
  }
}

export class PetraDbDatabase<
  TSchema extends Record<string, unknown> = Record<string, never>,
> extends PgDatabase<PetraDbQueryResultHKT, TSchema> {
  static override readonly [entityKind] = "PetraDbDatabase";
}

export interface PetraDbQueryResultHKT extends PgQueryResultHKT {
  type: PetraDbQueryResult;
}

export function drizzle<
  TSchema extends Record<string, unknown> = Record<string, never>,
>(
  session: PetraDBSession,
  config?: DrizzleConfig<TSchema>,
): PetraDbDatabase<TSchema> & { $session: PetraDBSession } {
  const dialect = new PgDialect({ casing: config?.casing });

  let logger: Logger | undefined;
  if (config?.logger === true) {
    logger = new DefaultLogger();
  } else if (config?.logger !== false && config?.logger !== undefined) {
    logger = config.logger;
  }

  let schema: RelationalSchemaConfig<TablesRelationalConfig> | undefined;
  if (config?.schema) {
    const tablesConfig = extractTablesRelationalConfig(
      config.schema,
      createTableRelationsHelpers,
    );
    schema = {
      fullSchema: config.schema,
      schema: tablesConfig.tables,
      tableNamesMap: tablesConfig.tableNamesMap,
    };
  }

  const petraDbSession = new PetraDbSession(
    session,
    dialect,
    schema,
    { logger },
  );

  const db = new PetraDbDatabase(
    dialect,
    petraDbSession,
    schema as any,
  ) as PetraDbDatabase<TSchema> & { $session: PetraDBSession };

  db.$session = session;

  return db;
}
