import { describe, it, before, after } from "node:test";
import assert from "node:assert/strict";
import knex, { type Knex } from "knex";

let db: Knex;

describe("Date binding coercion", () => {
  before(async () => {
    const PetraDBClient = (await import("../src/index.ts")).default;
    db = knex({
      client: PetraDBClient as any,
      connection: { storage: "memory" },
    });
    await db.raw(
      "CREATE TABLE events (id SERIAL PRIMARY KEY, name TEXT, created_at TIMESTAMP)"
    );
  });

  after(async () => {
    await db.destroy();
  });

  it("inserts a JS Date object into a TIMESTAMP column", async () => {
    const now = new Date("2026-03-06T22:56:07.000Z");
    await db("events").insert({ name: "test", created_at: now });

    const rows = await db("events").select("*");
    assert.equal(rows.length, 1);
    assert.equal(rows[0].name, "test");
    // The timestamp should round-trip — not throw a parse error
    assert.ok(rows[0].created_at, "created_at should be present");
  });

  it("uses a JS Date in a WHERE clause", async () => {
    const cutoff = new Date("2026-03-06T00:00:00.000Z");
    const rows = await db("events").where("created_at", ">=", cutoff).select("*");
    assert.equal(rows.length, 1);
  });
});
