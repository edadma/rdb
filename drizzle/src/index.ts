import { drizzle as pgProxyDrizzle } from "drizzle-orm/pg-proxy";
import type { PgRemoteDatabase } from "drizzle-orm/pg-proxy";

interface PetraDBSession {
  execute(sql: string, options?: { rowMode?: "object" | "array" }): Promise<any[]>;
  prepare(sql: string): { execute(params?: any[], options?: { rowMode?: "object" | "array" }): Promise<any[]> };
  close(): Promise<void>;
}

export interface PetraDBDrizzleDatabase<TSchema extends Record<string, unknown> = Record<string, never>>
  extends PgRemoteDatabase<TSchema> {
  $session: PetraDBSession;
}

export function drizzle<TSchema extends Record<string, unknown> = Record<string, never>>(
  session: PetraDBSession,
  config?: { schema?: TSchema },
): PetraDBDrizzleDatabase<TSchema> {
  const db = pgProxyDrizzle<TSchema>(
    async (sql, params, method) => {
      let results: any[];

      if (params.length > 0) {
        const stmt = session.prepare(sql);
        results = await stmt.execute(params, { rowMode: "array" });
      } else {
        results = await session.execute(sql, { rowMode: "array" });
      }

      if (method === "execute") {
        return { rows: [] };
      }

      // method === 'all'
      const result = results[0];
      return { rows: result?.rows ?? [] };
    },
    config,
  ) as PetraDBDrizzleDatabase<TSchema>;

  db.$session = session;

  return db;
}
