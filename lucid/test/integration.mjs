/**
 * Integration tests for @petradb/lucid
 *
 * Tests the full Lucid ORM stack with PetraDB as the backing database.
 */

import { createRequire } from "node:module";
import path from "node:path";

const _require = createRequire(import.meta.url);

// Apply the patch before anything else
await import("../dist/index.js");

// Resolve Lucid internals via absolute paths (bypasses exports map)
const buildDir = path.dirname(_require.resolve("@adonisjs/lucid"));
const connectionMod = _require(path.join(buildDir, "src/connection/index.js"));
const { QueryClient } = await import("@adonisjs/lucid/database");
const dialectsMod = _require(path.join(buildDir, "src/dialects/index.js"));

const Connection = connectionMod.Connection;

// ---------------------------------------------------------------------------
// Test harness
// ---------------------------------------------------------------------------

let passed = 0;
let failed = 0;

async function test(name, fn) {
  try {
    await fn();
    console.log(`  PASS  ${name}`);
    passed++;
  } catch (err) {
    console.log(`  FAIL  ${name}`);
    console.log(`        ${err.message}`);
    if (err.stack) {
      const relevantLine = err.stack.split("\n").find((l) => l.includes("integration.mjs"));
      if (relevantLine) console.log(`       ${relevantLine.trim()}`);
    }
    failed++;
  }
}

function assert(condition, msg) {
  if (!condition) throw new Error(msg || "assertion failed");
}

function assertEqual(actual, expected, msg) {
  const a = JSON.stringify(actual);
  const e = JSON.stringify(expected);
  if (a !== e) throw new Error(`${msg || "not equal"}: got ${a}, expected ${e}`);
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

function createConnection() {
  const logger = { trace() {}, debug() {}, info() {}, warn() {}, error() {}, fatal() {} };
  const conn = new Connection(
    "test",
    {
      client: "petradb",
      connection: { storage: "memory" },
    },
    logger
  );
  conn.connect();
  return conn;
}

async function withConnection(fn) {
  const conn = createConnection();
  try {
    await fn(conn);
  } finally {
    await conn.disconnect();
  }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

console.log("\n--- Patch Tests ---");

await test("petradb is in clientsNames", () => {
  assert(dialectsMod.clientsNames.includes("petradb"));
});

await test("PetraDBDialect is in the mapping", () => {
  assert(dialectsMod.clientsToDialectsMapping["petradb"]);
  assertEqual(dialectsMod.clientsToDialectsMapping["petradb"].name, "PetraDBDialect");
});

await test("Connection accepts petradb client", () => {
  const conn = new Connection(
    "test",
    { client: "petradb", connection: { storage: "memory" } },
    { trace() {} }
  );
  assertEqual(conn.clientName, "petradb");
});

await test("getWriteConfig swaps string for PetraDBClient class", () => {
  const conn = new Connection(
    "test",
    { client: "petradb", connection: { storage: "memory" } },
    { trace() {} }
  );
  const config = conn.getWriteConfig();
  assertEqual(typeof config.client, "function");
  assertEqual(config.client.name, "PetraDBClient");
});

console.log("\n--- Connection Tests ---");

await test("can open and close a connection", async () => {
  await withConnection(async (conn) => {
    assert(conn.ready, "connection should be ready");
  });
});

await test("can get a knex write client", async () => {
  await withConnection(async (conn) => {
    const knex = conn.client;
    assert(knex, "should have a knex client");
  });
});

console.log("\n--- Raw Query Tests ---");

await test("raw SELECT query", async () => {
  await withConnection(async (conn) => {
    const knex = conn.client;
    const result = await knex.raw("SELECT 1 + 1 AS result");
    assert(result.rows, "should have rows");
    assertEqual(result.rows[0].result, 2);
  });
});

await test("raw CREATE TABLE and INSERT", async () => {
  await withConnection(async (conn) => {
    const knex = conn.client;
    await knex.raw("CREATE TABLE test_raw (id serial PRIMARY KEY, name text)");
    await knex.raw("INSERT INTO test_raw (name) VALUES ('hello')");
    const result = await knex.raw("SELECT * FROM test_raw");
    assertEqual(result.rows.length, 1);
    assertEqual(result.rows[0].name, "hello");
  });
});

console.log("\n--- Schema Builder Tests ---");

await test("create table via schema builder", async () => {
  await withConnection(async (conn) => {
    const knex = conn.client;
    await knex.schema.createTable("users", (table) => {
      table.increments("id");
      table.string("name", 100);
      table.string("email", 255);
      table.integer("age");
      table.boolean("active").defaultTo(true);
      table.timestamps(true, true);
    });
    const result = await knex.raw("SHOW TABLES");
    const tables = result.rows.map((r) => r.table_name || r.name);
    assert(tables.includes("users"), `tables should include 'users', got: ${tables}`);
  });
});

await test("hasTable returns true for existing table", async () => {
  await withConnection(async (conn) => {
    const knex = conn.client;
    await knex.schema.createTable("check_me", (table) => {
      table.increments("id");
    });
    const exists = await knex.schema.hasTable("check_me");
    assert(exists, "hasTable should return true");
  });
});

await test("hasTable returns false for missing table", async () => {
  await withConnection(async (conn) => {
    const knex = conn.client;
    const exists = await knex.schema.hasTable("nonexistent");
    assert(!exists, "hasTable should return false");
  });
});

await test("hasColumn", async () => {
  await withConnection(async (conn) => {
    const knex = conn.client;
    await knex.schema.createTable("col_check", (table) => {
      table.increments("id");
      table.string("name");
    });
    const hasName = await knex.schema.hasColumn("col_check", "name");
    const hasFoo = await knex.schema.hasColumn("col_check", "foo");
    assert(hasName, "should have 'name' column");
    assert(!hasFoo, "should not have 'foo' column");
  });
});

await test("drop table", async () => {
  await withConnection(async (conn) => {
    const knex = conn.client;
    await knex.schema.createTable("to_drop", (table) => {
      table.increments("id");
    });
    await knex.schema.dropTable("to_drop");
    const exists = await knex.schema.hasTable("to_drop");
    assert(!exists, "table should be dropped");
  });
});

console.log("\n--- Query Builder Tests ---");

await test("insert and select", async () => {
  await withConnection(async (conn) => {
    const knex = conn.client;
    await knex.schema.createTable("products", (table) => {
      table.increments("id");
      table.string("name");
      table.decimal("price", 10, 2);
    });
    await knex("products").insert([
      { name: "Widget", price: 9.99 },
      { name: "Gadget", price: 24.99 },
      { name: "Doohickey", price: 4.99 },
    ]);
    const rows = await knex("products").select("*").orderBy("price", "asc");
    assertEqual(rows.length, 3);
    assertEqual(rows[0].name, "Doohickey");
    assertEqual(rows[2].name, "Gadget");
  });
});

await test("where clause", async () => {
  await withConnection(async (conn) => {
    const knex = conn.client;
    await knex.schema.createTable("items", (table) => {
      table.increments("id");
      table.string("category");
      table.integer("qty");
    });
    await knex("items").insert([
      { category: "A", qty: 10 },
      { category: "B", qty: 20 },
      { category: "A", qty: 30 },
    ]);
    const rows = await knex("items").where("category", "A").orderBy("qty");
    assertEqual(rows.length, 2);
    assertEqual(rows[0].qty, 10);
    assertEqual(rows[1].qty, 30);
  });
});

await test("update", async () => {
  await withConnection(async (conn) => {
    const knex = conn.client;
    await knex.schema.createTable("counters", (table) => {
      table.string("name").primary();
      table.integer("value");
    });
    await knex("counters").insert({ name: "hits", value: 0 });
    const count = await knex("counters").where("name", "hits").update({ value: 42 });
    assertEqual(count, 1);
    const rows = await knex("counters").where("name", "hits");
    assertEqual(rows[0].value, 42);
  });
});

await test("delete", async () => {
  await withConnection(async (conn) => {
    const knex = conn.client;
    await knex.schema.createTable("logs", (table) => {
      table.increments("id");
      table.string("level");
    });
    await knex("logs").insert([
      { level: "info" },
      { level: "error" },
      { level: "info" },
    ]);
    const count = await knex("logs").where("level", "info").del();
    assertEqual(count, 2);
    const remaining = await knex("logs").select("*");
    assertEqual(remaining.length, 1);
    assertEqual(remaining[0].level, "error");
  });
});

await test("insert with returning", async () => {
  await withConnection(async (conn) => {
    const knex = conn.client;
    await knex.schema.createTable("things", (table) => {
      table.increments("id");
      table.string("name");
    });
    const result = await knex("things").insert({ name: "foo" }).returning("id");
    assert(Array.isArray(result), "returning should return an array");
    assert(result.length > 0, "should have at least one row");
    assert(result[0].id !== undefined, "should have an id");
  });
});

await test("truncate", async () => {
  await withConnection(async (conn) => {
    const knex = conn.client;
    await knex.schema.createTable("temp", (table) => {
      table.increments("id");
      table.string("val");
    });
    await knex("temp").insert([{ val: "a" }, { val: "b" }]);
    await knex("temp").truncate();
    const rows = await knex("temp").select("*");
    assertEqual(rows.length, 0);
  });
});

await test("count aggregate", async () => {
  await withConnection(async (conn) => {
    const knex = conn.client;
    await knex.schema.createTable("agg_test", (table) => {
      table.increments("id");
      table.string("color");
    });
    await knex("agg_test").insert([
      { color: "red" },
      { color: "blue" },
      { color: "red" },
    ]);
    const result = await knex("agg_test").count("* as cnt");
    assertEqual(Number(result[0].cnt), 3);
  });
});

console.log("\n--- Dialect Tests ---");

await test("dialect getAllTables", async () => {
  await withConnection(async (conn) => {
    const knex = conn.client;
    await knex.schema.createTable("tbl_a", (t) => t.increments("id"));
    await knex.schema.createTable("tbl_b", (t) => t.increments("id"));

    // Instantiate the dialect
    const { PetraDBDialect } = await import("../dist/dialect.js");
    // Create a minimal client mock that the dialect needs
    const dialect = new PetraDBDialect(
      { rawQuery: (sql) => knex.raw(sql) },
      {}
    );
    const tables = await dialect.getAllTables();
    assert(tables.includes("tbl_a"), `should have tbl_a, got: ${tables}`);
    assert(tables.includes("tbl_b"), `should have tbl_b, got: ${tables}`);
  });
});

await test("dialect truncate", async () => {
  await withConnection(async (conn) => {
    const knex = conn.client;
    await knex.schema.createTable("trunc_test", (t) => {
      t.increments("id");
      t.string("val");
    });
    await knex("trunc_test").insert([{ val: "x" }, { val: "y" }]);

    const { PetraDBDialect } = await import("../dist/dialect.js");
    const dialect = new PetraDBDialect(
      { rawQuery: (sql) => knex.raw(sql) },
      {}
    );
    await dialect.truncate("trunc_test");

    const rows = await knex("trunc_test").select("*");
    assertEqual(rows.length, 0);
  });
});

// ---------------------------------------------------------------------------
// Summary
// ---------------------------------------------------------------------------

console.log(`\n--- Results: ${passed} passed, ${failed} failed ---\n`);
process.exit(failed > 0 ? 1 : 0);
