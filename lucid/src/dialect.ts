// @ts-nocheck — Lucid internals are untyped; we implement DialectContract at runtime.

/**
 * PetraDB dialect for AdonisJS Lucid.
 *
 * Modeled after PgDialect since PetraDB is PostgreSQL-compatible,
 * but uses PetraDB-specific SQL (SHOW TABLES, SHOW VIEWS, etc.).
 */
export class PetraDBDialect {
  client: any;
  config: any;
  name = "petradb";
  supportsAdvisoryLocks = false;
  supportsViews = true;
  supportsTypes = true;
  supportsDomains = false;
  supportsReturningStatement = true;

  version: string | undefined;

  dateTimeFormat = "yyyy-MM-dd'T'HH:mm:ss.SSSZZ";

  constructor(client: any, config: any) {
    this.client = client;
    this.config = config;
  }

  async getAllTables(_schemas?: string[]): Promise<string[]> {
    const result = await this.client.rawQuery("SHOW TABLES");
    const rows = result.rows || result;
    return rows.map((r: any) => r.table_name || r.name);
  }

  async getAllViews(_schemas?: string[]): Promise<string[]> {
    const result = await this.client.rawQuery("SHOW VIEWS");
    const rows = result.rows || result;
    return rows.map((r: any) => r.view_name || r.name);
  }

  async getAllTypes(_schemas?: string[]): Promise<string[]> {
    const result = await this.client.rawQuery("SHOW TYPES");
    const rows = result.rows || result;
    return rows.map((r: any) => r.type_name || r.name);
  }

  async getAllDomains(_schemas?: string[]): Promise<string[]> {
    return [];
  }

  async truncate(table: string, _cascade?: boolean): Promise<void> {
    await this.client.rawQuery(`TRUNCATE TABLE "${table}"`);
  }

  async truncateAllTables(
    excludeTables?: string[],
    _schemas?: string[]
  ): Promise<void> {
    const tables = await this.getAllTables();
    const exclude = new Set(excludeTables || []);
    for (const table of tables) {
      if (!exclude.has(table)) {
        await this.truncate(table);
      }
    }
  }

  async dropAllTables(_schemas?: string[]): Promise<void> {
    const tables = await this.getAllTables();
    const ignore = this.config.wipe?.ignoreTables || [];
    for (const table of tables) {
      if (!ignore.includes(table)) {
        await this.client.rawQuery(`DROP TABLE "${table}" CASCADE`);
      }
    }
  }

  async dropAllViews(_schemas?: string[]): Promise<void> {
    const views = await this.getAllViews();
    for (const view of views) {
      await this.client.rawQuery(`DROP VIEW "${view}"`);
    }
  }

  async dropAllTypes(_schemas?: string[]): Promise<void> {
    const types = await this.getAllTypes();
    for (const type of types) {
      await this.client.rawQuery(`DROP TYPE "${type}"`);
    }
  }

  async dropAllDomains(_schemas?: string[]): Promise<void> {
    // PetraDB doesn't support domains
  }

  async getAdvisoryLock(_key: number): Promise<boolean> {
    return false;
  }

  async releaseAdvisoryLock(_key: number): Promise<boolean> {
    return false;
  }
}
