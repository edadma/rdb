import { describe, it, before, after } from "node:test";
import assert from "node:assert/strict";
import { createRequire } from "node:module";

const _require = createRequire(
  new URL("../npm/package.json", import.meta.url).href
);

// Minimal logger/emitter stubs that satisfy Lucid's constructor
const noopLogger: any = {
  trace() {}, debug() {}, info() {}, warn() {}, error() {}, fatal() {},
  child() { return noopLogger; },
  level: "info",
};
const noopEmitter: any = {
  on() {}, off() {}, emit() {}, once() {},
  listenerCount() { return 0; },
};

describe("Lucid ORM smoke test", () => {
  // AdonisJS Lucid v22 has a hardcoded allowlist of client names
  // (postgres, mysql, mysql2, sqlite3, etc.) in its Connection class.
  // Custom Knex clients cannot be used directly — Lucid would need to
  // add a PetraDB dialect or provide an extension point for custom engines.
  //
  // The @petradb/knex dialect works with Knex directly. Lucid integration
  // would require either:
  //   1. An AdonisJS database driver package (@petradb/lucid)
  //   2. Lucid adding support for custom client classes

  it("Lucid rejects unknown client names on first query (known limitation)", async () => {
    const lucid = _require("@adonisjs/lucid/database");
    const Database = lucid.Database ?? lucid.default;
    const PetraDBClient = (await import("../npm/src/index.ts")).default;

    const db = new Database(
      {
        connection: "petradb",
        connections: {
          petradb: {
            client: PetraDBClient,
            connection: { storage: "memory" },
          },
        },
      },
      noopLogger,
      noopEmitter,
    );

    try {
      await db.rawQuery("SELECT 1");
      assert.fail("Expected Lucid to reject the custom client");
    } catch (err: any) {
      assert.ok(
        String(err).includes("Unsupported client") ||
        err.code === "E_UNSUPPORTED_CLIENT",
        `Expected 'Unsupported client' error, got: ${err.code || err.message}`,
      );
    }

    try { await db.manager.closeAll(); } catch {}
  });
});
