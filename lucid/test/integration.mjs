/**
 * Integration tests for @petradb/lucid
 *
 * Tests the full Lucid ORM stack with PetraDB as the backing database.
 * Run with: node --test test/integration.mjs
 */

import { describe, it } from "node:test";
import assert from "node:assert/strict";
import { createRequire } from "node:module";
import path from "node:path";
import EventEmitter from "node:events";

const _require = createRequire(import.meta.url);

// Apply the patch before anything else
await import("../dist/index.js");

// Resolve Lucid internals via absolute paths (bypasses exports map)
const buildDir = path.dirname(_require.resolve("@adonisjs/lucid"));
const connectionMod = _require(path.join(buildDir, "src/connection/index.js"));
const dialectsMod = _require(path.join(buildDir, "src/dialects/index.js"));

const { Database } = await import("@adonisjs/lucid/database");
const { BaseModel } = await import("@adonisjs/lucid/orm");
const { BaseSchema } = await import("@adonisjs/lucid/schema");
const { BaseSeeder } = await import("@adonisjs/lucid/seeders");

const Connection = connectionMod.Connection;

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

const logger = {
  trace() {},
  debug() {},
  info() {},
  warn() {},
  error() {},
  fatal() {},
};

function createConnection() {
  const conn = new Connection(
    "test",
    { client: "petradb", connection: { storage: "memory" } },
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

function createDatabase() {
  const emitter = new EventEmitter();
  emitter.hasListeners = () => false;
  return new Database(
    {
      connection: "petradb",
      connections: {
        petradb: {
          client: "petradb",
          connection: { storage: "memory" },
        },
      },
    },
    logger,
    emitter
  );
}

async function withDatabase(fn) {
  const db = createDatabase();
  try {
    await fn(db);
  } finally {
    await db.manager.closeAll();
  }
}

/**
 * Define a model class without decorators.
 */
function defineModel(table, columns) {
  class Model extends BaseModel {
    static table = table;
  }
  Model.boot();
  for (const [name, opts] of Object.entries(columns)) {
    Model.$addColumn(name, opts || {});
  }
  return Model;
}

// =========================================================================
//  1. PATCH
// =========================================================================

describe("Patch", () => {
  it("adds petradb to clientsNames", () => {
    assert.ok(dialectsMod.clientsNames.includes("petradb"));
  });

  it("registers PetraDBDialect in the mapping", () => {
    assert.ok(dialectsMod.clientsToDialectsMapping["petradb"]);
    assert.equal(
      dialectsMod.clientsToDialectsMapping["petradb"].name,
      "PetraDBDialect"
    );
  });

  it("Connection accepts petradb client", () => {
    const conn = new Connection(
      "test",
      { client: "petradb", connection: { storage: "memory" } },
      { trace() {} }
    );
    assert.equal(conn.clientName, "petradb");
  });

  it("getWriteConfig swaps string for PetraDBClient class", () => {
    const conn = new Connection(
      "test",
      { client: "petradb", connection: { storage: "memory" } },
      { trace() {} }
    );
    const config = conn.getWriteConfig();
    assert.equal(typeof config.client, "function");
    assert.equal(config.client.name, "PetraDBClient");
  });
});

// =========================================================================
//  2. CONNECTION
// =========================================================================

describe("Connection", () => {
  it("opens and closes", async () => {
    await withConnection(async (conn) => {
      assert.ok(conn.ready);
    });
  });

  it("exposes a knex write client", async () => {
    await withConnection(async (conn) => {
      assert.ok(conn.client);
    });
  });
});

// =========================================================================
//  3. RAW QUERIES
// =========================================================================

describe("Raw queries", () => {
  it("SELECT expression", async () => {
    await withConnection(async (conn) => {
      const result = await conn.client.raw("SELECT 1 + 1 AS result");
      assert.equal(result.rows[0].result, 2);
    });
  });

  it("CREATE TABLE, INSERT, SELECT", async () => {
    await withConnection(async (conn) => {
      const knex = conn.client;
      await knex.raw(
        "CREATE TABLE test_raw (id serial PRIMARY KEY, name text)"
      );
      await knex.raw("INSERT INTO test_raw (name) VALUES ('hello')");
      const result = await knex.raw("SELECT * FROM test_raw");
      assert.equal(result.rows.length, 1);
      assert.equal(result.rows[0].name, "hello");
    });
  });
});

// =========================================================================
//  4. SCHEMA BUILDER
// =========================================================================

describe("Schema builder", () => {
  it("createTable with various column types", async () => {
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
      assert.ok(tables.includes("users"));
    });
  });

  it("hasTable true / false", async () => {
    await withConnection(async (conn) => {
      const knex = conn.client;
      await knex.schema.createTable("check_me", (t) => t.increments("id"));
      assert.equal(await knex.schema.hasTable("check_me"), true);
      assert.equal(await knex.schema.hasTable("nonexistent"), false);
    });
  });

  it("hasColumn", async () => {
    await withConnection(async (conn) => {
      const knex = conn.client;
      await knex.schema.createTable("col_check", (t) => {
        t.increments("id");
        t.string("name");
      });
      assert.equal(await knex.schema.hasColumn("col_check", "name"), true);
      assert.equal(await knex.schema.hasColumn("col_check", "foo"), false);
    });
  });

  it("dropTable", async () => {
    await withConnection(async (conn) => {
      const knex = conn.client;
      await knex.schema.createTable("to_drop", (t) => t.increments("id"));
      await knex.schema.dropTable("to_drop");
      assert.equal(await knex.schema.hasTable("to_drop"), false);
    });
  });

  it("renameTable", async () => {
    await withConnection(async (conn) => {
      const knex = conn.client;
      await knex.schema.createTable("old_name", (t) => t.increments("id"));
      await knex.schema.renameTable("old_name", "new_name");
      assert.equal(await knex.schema.hasTable("old_name"), false);
      assert.equal(await knex.schema.hasTable("new_name"), true);
    });
  });

  it("alterTable — add column", async () => {
    await withConnection(async (conn) => {
      const knex = conn.client;
      await knex.schema.createTable("alter_me", (t) => t.increments("id"));
      await knex.schema.alterTable("alter_me", (t) => t.string("nickname"));
      assert.equal(await knex.schema.hasColumn("alter_me", "nickname"), true);
    });
  });

  it("all column types: integer, bigint, text, boolean, date, timestamp, json, uuid, float", async () => {
    await withConnection(async (conn) => {
      const knex = conn.client;
      await knex.schema.createTable("all_types", (t) => {
        t.increments("id");
        t.integer("age");
        t.bigInteger("big_num");
        t.text("bio");
        t.boolean("active");
        t.date("birthday");
        t.timestamp("created_at");
        t.json("metadata");
        t.uuid("uid");
        t.float("score");
      });
      assert.equal(await knex.schema.hasTable("all_types"), true);
      assert.equal(await knex.schema.hasColumn("all_types", "metadata"), true);
      assert.equal(await knex.schema.hasColumn("all_types", "uid"), true);
    });
  });
});

// =========================================================================
//  5. QUERY BUILDER
// =========================================================================

describe("Query builder", () => {
  it("insert and select with orderBy", async () => {
    await withConnection(async (conn) => {
      const knex = conn.client;
      await knex.schema.createTable("products", (t) => {
        t.increments("id");
        t.string("name");
        t.decimal("price", 10, 2);
      });
      await knex("products").insert([
        { name: "Widget", price: 9.99 },
        { name: "Gadget", price: 24.99 },
        { name: "Doohickey", price: 4.99 },
      ]);
      const rows = await knex("products").select("*").orderBy("price", "asc");
      assert.equal(rows.length, 3);
      assert.equal(rows[0].name, "Doohickey");
      assert.equal(rows[2].name, "Gadget");
    });
  });

  it("where / orWhere / whereIn", async () => {
    await withConnection(async (conn) => {
      const knex = conn.client;
      await knex.schema.createTable("items", (t) => {
        t.increments("id");
        t.string("cat");
        t.integer("qty");
      });
      await knex("items").insert([
        { cat: "A", qty: 10 },
        { cat: "B", qty: 20 },
        { cat: "A", qty: 30 },
        { cat: "C", qty: 5 },
      ]);

      const byCategory = await knex("items").where("cat", "A").orderBy("qty");
      assert.equal(byCategory.length, 2);

      const byIn = await knex("items")
        .whereIn("cat", ["A", "C"])
        .orderBy("qty");
      assert.equal(byIn.length, 3);

      const byOr = await knex("items")
        .where("cat", "B")
        .orWhere("qty", "<", 10)
        .orderBy("qty");
      assert.equal(byOr.length, 2);
    });
  });

  it("update returns row count", async () => {
    await withConnection(async (conn) => {
      const knex = conn.client;
      await knex.schema.createTable("counters", (t) => {
        t.string("name").primary();
        t.integer("val");
      });
      await knex("counters").insert({ name: "hits", val: 0 });
      const count = await knex("counters")
        .where("name", "hits")
        .update({ val: 42 });
      assert.equal(count, 1);
      const rows = await knex("counters").where("name", "hits");
      assert.equal(rows[0].val, 42);
    });
  });

  it("update with returning", async () => {
    await withConnection(async (conn) => {
      const knex = conn.client;
      await knex.schema.createTable("upd_ret", (t) => {
        t.increments("id");
        t.string("status");
      });
      await knex("upd_ret").insert({ status: "pending" });
      const result = await knex("upd_ret")
        .where("status", "pending")
        .update({ status: "done" })
        .returning(["id", "status"]);
      assert.ok(Array.isArray(result) && result.length > 0);
      assert.equal(result[0].status, "done");
    });
  });

  it("delete returns row count", async () => {
    await withConnection(async (conn) => {
      const knex = conn.client;
      await knex.schema.createTable("logs", (t) => {
        t.increments("id");
        t.string("level");
      });
      await knex("logs").insert([
        { level: "info" },
        { level: "error" },
        { level: "info" },
      ]);
      const count = await knex("logs").where("level", "info").del();
      assert.equal(count, 2);
      const remaining = await knex("logs").select("*");
      assert.equal(remaining.length, 1);
    });
  });

  it("insert with returning", async () => {
    await withConnection(async (conn) => {
      const knex = conn.client;
      await knex.schema.createTable("things", (t) => {
        t.increments("id");
        t.string("name");
      });
      const result = await knex("things")
        .insert({ name: "foo" })
        .returning("id");
      assert.ok(Array.isArray(result) && result.length > 0);
      assert.ok(result[0].id !== undefined);
    });
  });

  it("truncate", async () => {
    await withConnection(async (conn) => {
      const knex = conn.client;
      await knex.schema.createTable("temp", (t) => {
        t.increments("id");
        t.string("val");
      });
      await knex("temp").insert([{ val: "a" }, { val: "b" }]);
      await knex("temp").truncate();
      const rows = await knex("temp").select("*");
      assert.equal(rows.length, 0);
    });
  });

  it("aggregates: count, sum, min, max, avg", async () => {
    await withConnection(async (conn) => {
      const knex = conn.client;
      await knex.schema.createTable("scores", (t) => {
        t.increments("id");
        t.integer("val");
      });
      await knex("scores").insert([{ val: 10 }, { val: 20 }, { val: 30 }]);

      assert.equal(
        Number((await knex("scores").count("* as c"))[0].c),
        3
      );
      assert.equal(
        Number((await knex("scores").sum("val as s"))[0].s),
        60
      );
      assert.equal(
        Number((await knex("scores").min("val as m"))[0].m),
        10
      );
      assert.equal(
        Number((await knex("scores").max("val as m"))[0].m),
        30
      );
      assert.equal(
        Number((await knex("scores").avg("val as a"))[0].a),
        20
      );
    });
  });

  it("select specific columns", async () => {
    await withConnection(async (conn) => {
      const knex = conn.client;
      await knex.schema.createTable("people", (t) => {
        t.increments("id");
        t.string("first_name");
        t.string("last_name");
        t.integer("age");
      });
      await knex("people").insert({
        first_name: "John",
        last_name: "Doe",
        age: 30,
      });
      const rows = await knex("people").select("first_name", "age");
      assert.deepEqual(Object.keys(rows[0]).sort(), ["age", "first_name"]);
    });
  });

  it("limit and offset", async () => {
    await withConnection(async (conn) => {
      const knex = conn.client;
      await knex.schema.createTable("nums", (t) => {
        t.increments("id");
        t.integer("n");
      });
      for (let i = 1; i <= 10; i++) await knex("nums").insert({ n: i });

      const page = await knex("nums").orderBy("n").limit(3).offset(3);
      assert.equal(page.length, 3);
      assert.equal(page[0].n, 4);
      assert.equal(page[2].n, 6);
    });
  });

  it("orderBy multiple columns", async () => {
    await withConnection(async (conn) => {
      const knex = conn.client;
      await knex.schema.createTable("multi_sort", (t) => {
        t.string("cat");
        t.integer("priority");
        t.string("name");
      });
      await knex("multi_sort").insert([
        { cat: "B", priority: 1, name: "beta" },
        { cat: "A", priority: 2, name: "alpha2" },
        { cat: "A", priority: 1, name: "alpha1" },
      ]);
      const rows = await knex("multi_sort").orderBy([
        { column: "cat", order: "asc" },
        { column: "priority", order: "asc" },
      ]);
      assert.equal(rows[0].name, "alpha1");
      assert.equal(rows[1].name, "alpha2");
      assert.equal(rows[2].name, "beta");
    });
  });

  it("join", async () => {
    await withConnection(async (conn) => {
      const knex = conn.client;
      await knex.schema.createTable("authors", (t) => {
        t.increments("id");
        t.string("name");
      });
      await knex.schema.createTable("books", (t) => {
        t.increments("id");
        t.string("title");
        t.integer("author_id");
      });
      const [author] = await knex("authors")
        .insert({ name: "Tolkien" })
        .returning("id");
      await knex("books").insert([
        { title: "The Hobbit", author_id: author.id },
        { title: "LOTR", author_id: author.id },
      ]);
      const rows = await knex("books")
        .join("authors", "books.author_id", "authors.id")
        .select("books.title", "authors.name")
        .orderBy("books.title");
      assert.equal(rows.length, 2);
      assert.equal(rows[0].title, "LOTR");
      assert.equal(rows[0].name, "Tolkien");
    });
  });

  it("subquery in where", async () => {
    await withConnection(async (conn) => {
      const knex = conn.client;
      await knex.schema.createTable("employees", (t) => {
        t.increments("id");
        t.string("name");
        t.integer("salary");
      });
      await knex("employees").insert([
        { name: "Alice", salary: 80000 },
        { name: "Bob", salary: 60000 },
        { name: "Carol", salary: 90000 },
      ]);
      const aboveAvg = await knex("employees")
        .where(
          "salary",
          ">",
          knex.raw("(SELECT AVG(salary) FROM employees)")
        )
        .orderBy("name");
      assert.equal(aboveAvg.length, 2);
      assert.equal(aboveAvg[0].name, "Alice");
      assert.equal(aboveAvg[1].name, "Carol");
    });
  });

  it("group by with having", async () => {
    await withConnection(async (conn) => {
      const knex = conn.client;
      await knex.schema.createTable("sales", (t) => {
        t.increments("id");
        t.string("product");
        t.integer("amount");
      });
      await knex("sales").insert([
        { product: "A", amount: 100 },
        { product: "A", amount: 200 },
        { product: "B", amount: 50 },
        { product: "C", amount: 300 },
        { product: "C", amount: 400 },
      ]);
      const rows = await knex("sales")
        .select("product")
        .sum("amount as total")
        .groupBy("product")
        .having(knex.raw("SUM(amount) > ?", [100]))
        .orderBy("product");
      assert.equal(rows.length, 2);
      assert.equal(rows[0].product, "A");
      assert.equal(rows[1].product, "C");
    });
  });
});

// =========================================================================
//  6. TRANSACTIONS
// =========================================================================

describe("Transactions", () => {
  it("commit persists changes", async () => {
    await withConnection(async (conn) => {
      const knex = conn.client;
      await knex.schema.createTable("bank", (t) => {
        t.string("account").primary();
        t.integer("balance");
      });
      await knex("bank").insert([
        { account: "alice", balance: 1000 },
        { account: "bob", balance: 500 },
      ]);

      const trx = await knex.transaction();
      await trx("bank").where("account", "alice").update({ balance: 900 });
      await trx("bank").where("account", "bob").update({ balance: 600 });
      await trx.commit();

      assert.equal(
        (await knex("bank").where("account", "alice").first()).balance,
        900
      );
      assert.equal(
        (await knex("bank").where("account", "bob").first()).balance,
        600
      );
    });
  });

  it("rollback reverts changes", async () => {
    await withConnection(async (conn) => {
      const knex = conn.client;
      await knex.schema.createTable("bank2", (t) => {
        t.string("account").primary();
        t.integer("balance");
      });
      await knex("bank2").insert({ account: "alice", balance: 1000 });

      const trx = await knex.transaction();
      await trx("bank2").where("account", "alice").update({ balance: 0 });
      await trx.rollback();

      assert.equal(
        (await knex("bank2").where("account", "alice").first()).balance,
        1000
      );
    });
  });

  it("callback auto-commits on success", async () => {
    await withConnection(async (conn) => {
      const knex = conn.client;
      await knex.schema.createTable("txn_auto", (t) => {
        t.increments("id");
        t.string("name");
      });
      await knex.transaction(async (trx) => {
        await trx("txn_auto").insert({ name: "inside txn" });
      });
      const rows = await knex("txn_auto").select("*");
      assert.equal(rows.length, 1);
      assert.equal(rows[0].name, "inside txn");
    });
  });

  it("callback auto-rollbacks on error", async () => {
    await withConnection(async (conn) => {
      const knex = conn.client;
      await knex.schema.createTable("txn_fail", (t) => {
        t.increments("id");
        t.string("name");
      });
      try {
        await knex.transaction(async (trx) => {
          await trx("txn_fail").insert({ name: "will disappear" });
          throw new Error("intentional failure");
        });
      } catch (_) {}
      const rows = await knex("txn_fail").select("*");
      assert.equal(rows.length, 0);
    });
  });
});

// =========================================================================
//  7. DATABASE CLASS (Lucid high-level API)
// =========================================================================

describe("Database class", () => {
  it("from() select", async () => {
    await withDatabase(async (db) => {
      const client = db.connection();
      await client.rawQuery(
        "CREATE TABLE db_test (id serial PRIMARY KEY, val text)"
      );
      await client.rawQuery("INSERT INTO db_test (val) VALUES ('hello')");
      const rows = await db.from("db_test").select("*");
      assert.equal(rows.length, 1);
      assert.equal(rows[0].val, "hello");
    });
  });

  it("rawQuery()", async () => {
    await withDatabase(async (db) => {
      const result = await db.rawQuery("SELECT 42 AS answer");
      assert.equal(result.rows[0].answer, 42);
    });
  });

  it("transaction()", async () => {
    await withDatabase(async (db) => {
      const client = db.connection();
      await client.rawQuery(
        "CREATE TABLE db_txn (id serial PRIMARY KEY, name text)"
      );
      await db.transaction(async (trx) => {
        await trx.insertQuery().table("db_txn").insert({ name: "in txn" });
      });
      const rows = await db.from("db_txn").select("*");
      assert.equal(rows.length, 1);
      assert.equal(rows[0].name, "in txn");
    });
  });

  it("from() with where/orderBy/limit", async () => {
    await withDatabase(async (db) => {
      const client = db.connection();
      await client.rawQuery(
        "CREATE TABLE db_q (id serial PRIMARY KEY, cat text, n integer)"
      );
      await client.rawQuery(
        "INSERT INTO db_q (cat, n) VALUES ('a', 3), ('b', 1), ('a', 2), ('a', 1)"
      );
      const rows = await db
        .from("db_q")
        .where("cat", "a")
        .orderBy("n", "asc")
        .limit(2);
      assert.equal(rows.length, 2);
      assert.equal(rows[0].n, 1);
      assert.equal(rows[1].n, 2);
    });
  });
});

// =========================================================================
//  8. ORM / BASEMODEL
// =========================================================================

describe("ORM / BaseModel", () => {
  it("CRUD: create, find, update, delete", async () => {
    await withDatabase(async (db) => {
      const client = db.connection();
      await client.rawQuery(
        `CREATE TABLE posts (
          id serial PRIMARY KEY,
          title text NOT NULL,
          body text,
          published boolean DEFAULT false
        )`
      );

      const Post = defineModel("posts", {
        id: { isPrimary: true },
        title: {},
        body: {},
        published: {},
      });
      Post.$adapter = db.modelAdapter();

      // Create
      const post = new Post();
      post.title = "Hello World";
      post.body = "First post";
      post.published = false;
      await post.save();
      assert.ok(post.id > 0);
      assert.ok(post.$isPersisted);

      // Find
      const found = await Post.find(post.id);
      assert.ok(found !== null);
      assert.equal(found.title, "Hello World");
      assert.equal(found.body, "First post");

      // Update
      found.published = true;
      await found.save();
      const updated = await Post.find(post.id);
      assert.equal(updated.published, true);

      // Delete
      await updated.delete();
      const deleted = await Post.find(post.id);
      assert.equal(deleted, null);
    });
  });

  it("create() static method", async () => {
    await withDatabase(async (db) => {
      const client = db.connection();
      await client.rawQuery(
        "CREATE TABLE tags (id serial PRIMARY KEY, name text NOT NULL)"
      );
      const Tag = defineModel("tags", {
        id: { isPrimary: true },
        name: {},
      });
      Tag.$adapter = db.modelAdapter();

      const tag = await Tag.create({ name: "javascript" });
      assert.ok(tag.id > 0);
      assert.equal(tag.name, "javascript");
      assert.ok(tag.$isPersisted);
    });
  });

  it("createMany()", async () => {
    await withDatabase(async (db) => {
      const client = db.connection();
      await client.rawQuery(
        "CREATE TABLE colors (id serial PRIMARY KEY, name text)"
      );
      const Color = defineModel("colors", {
        id: { isPrimary: true },
        name: {},
      });
      Color.$adapter = db.modelAdapter();

      const colors = await Color.createMany([
        { name: "red" },
        { name: "green" },
        { name: "blue" },
      ]);
      assert.equal(colors.length, 3);
      assert.ok(colors.every((c) => c.id > 0));
    });
  });

  it("query builder: where, orderBy, first", async () => {
    await withDatabase(async (db) => {
      const client = db.connection();
      await client.rawQuery(
        "CREATE TABLE fruits (id serial PRIMARY KEY, name text, sweetness integer)"
      );
      const Fruit = defineModel("fruits", {
        id: { isPrimary: true },
        name: {},
        sweetness: {},
      });
      Fruit.$adapter = db.modelAdapter();

      await Fruit.createMany([
        { name: "apple", sweetness: 7 },
        { name: "lemon", sweetness: 2 },
        { name: "mango", sweetness: 9 },
        { name: "grape", sweetness: 8 },
      ]);

      const sweet = await Fruit.query()
        .where("sweetness", ">", 6)
        .orderBy("sweetness", "desc");
      assert.equal(sweet.length, 3);
      assert.equal(sweet[0].name, "mango");

      const first = await Fruit.query().orderBy("name", "asc").first();
      assert.equal(first?.name, "apple");
    });
  });

  it("findBy and findOrFail", async () => {
    await withDatabase(async (db) => {
      const client = db.connection();
      await client.rawQuery(
        "CREATE TABLE settings (id serial PRIMARY KEY, setting_key text, setting_value text)"
      );
      const Setting = defineModel("settings", {
        id: { isPrimary: true },
        setting_key: {},
        setting_value: {},
      });
      Setting.$adapter = db.modelAdapter();

      await Setting.create({ setting_key: "theme", setting_value: "dark" });

      const found = await Setting.findBy("setting_key", "theme");
      assert.ok(found !== null);
      assert.equal(found.setting_value, "dark");

      const notFound = await Setting.findBy("setting_key", "nonexistent");
      assert.equal(notFound, null);

      await assert.rejects(() => Setting.findOrFail(99999), /Row not found/);
    });
  });

  it("hooks (beforeCreate)", async () => {
    await withDatabase(async (db) => {
      const client = db.connection();
      await client.rawQuery(
        "CREATE TABLE slugged (id serial PRIMARY KEY, title text, slug text)"
      );
      const Slugged = defineModel("slugged", {
        id: { isPrimary: true },
        title: {},
        slug: {},
      });
      Slugged.before("create", (instance) => {
        instance.slug = instance.title.toLowerCase().replace(/\s+/g, "-");
      });
      Slugged.$adapter = db.modelAdapter();

      const item = await Slugged.create({ title: "Hello World" });
      assert.equal(item.slug, "hello-world");
    });
  });

  it("pagination", async () => {
    await withDatabase(async (db) => {
      const client = db.connection();
      await client.rawQuery(
        "CREATE TABLE paginated (id serial PRIMARY KEY, n integer)"
      );
      const Paginated = defineModel("paginated", {
        id: { isPrimary: true },
        n: {},
      });
      Paginated.$adapter = db.modelAdapter();

      const items = [];
      for (let i = 1; i <= 25; i++) items.push({ n: i });
      await Paginated.createMany(items);

      const page1 = await Paginated.query().orderBy("n").paginate(1, 10);
      assert.equal(page1.total, 25);
      assert.equal(page1.perPage, 10);
      assert.equal(page1.currentPage, 1);
      assert.equal(page1.all().length, 10);
      assert.equal(page1.all()[0].n, 1);

      const page3 = await Paginated.query().orderBy("n").paginate(3, 10);
      assert.equal(page3.all().length, 5);
      assert.equal(page3.all()[0].n, 21);
    });
  });
});

// =========================================================================
//  9. MIGRATIONS (BaseSchema)
// =========================================================================

describe("Migrations (BaseSchema)", () => {
  it("up/down migration cycle", async () => {
    await withDatabase(async (db) => {
      class CreateUsersSchema extends BaseSchema {
        async up() {
          this.schema.createTable("migrated_users", (table) => {
            table.increments("id");
            table.string("email");
            table.string("password");
            table.timestamps(true, true);
          });
        }
        async down() {
          this.schema.dropTable("migrated_users");
        }
      }

      const client = db.connection();
      const knex = client.getWriteClient();

      await new CreateUsersSchema(client, "create_users.ts", false).execUp();
      assert.equal(await knex.schema.hasTable("migrated_users"), true);
      assert.equal(
        await knex.schema.hasColumn("migrated_users", "email"),
        true
      );

      await knex("migrated_users").insert({
        email: "test@example.com",
        password: "hashed",
      });
      assert.equal((await knex("migrated_users").select("*")).length, 1);

      await new CreateUsersSchema(client, "create_users.ts", false).execDown();
      assert.equal(await knex.schema.hasTable("migrated_users"), false);
    });
  });

  it("multi-step migration", async () => {
    await withDatabase(async (db) => {
      class CreateOrders extends BaseSchema {
        async up() {
          this.schema.createTable("m_orders", (t) => {
            t.increments("id");
            t.string("status");
          });
        }
        async down() {
          this.schema.dropTable("m_orders");
        }
      }
      class CreateOrderItems extends BaseSchema {
        async up() {
          this.schema.createTable("m_order_items", (t) => {
            t.increments("id");
            t.integer("order_id");
            t.string("product");
            t.integer("quantity");
          });
        }
        async down() {
          this.schema.dropTable("m_order_items");
        }
      }

      const client = db.connection();
      const knex = client.getWriteClient();

      await new CreateOrders(client, "step1.ts", false).execUp();
      await new CreateOrderItems(client, "step2.ts", false).execUp();

      assert.equal(await knex.schema.hasTable("m_orders"), true);
      assert.equal(await knex.schema.hasTable("m_order_items"), true);

      const [order] = await knex("m_orders")
        .insert({ status: "pending" })
        .returning("id");
      await knex("m_order_items").insert({
        order_id: order.id,
        product: "Widget",
        quantity: 3,
      });
      assert.equal(
        (await knex("m_order_items").where("order_id", order.id)).length,
        1
      );

      await new CreateOrderItems(client, "step2.ts", false).execDown();
      await new CreateOrders(client, "step1.ts", false).execDown();
      assert.equal(await knex.schema.hasTable("m_order_items"), false);
      assert.equal(await knex.schema.hasTable("m_orders"), false);
    });
  });

  it("alter table migration", async () => {
    await withDatabase(async (db) => {
      class CreateProfiles extends BaseSchema {
        async up() {
          this.schema.createTable("m_profiles", (t) => {
            t.increments("id");
            t.string("name");
          });
        }
        async down() {
          this.schema.dropTable("m_profiles");
        }
      }
      class AddAvatar extends BaseSchema {
        async up() {
          this.schema.alterTable("m_profiles", (t) => {
            t.string("avatar_url");
          });
        }
        async down() {
          this.schema.alterTable("m_profiles", (t) => {
            t.dropColumn("avatar_url");
          });
        }
      }

      const client = db.connection();
      const knex = client.getWriteClient();

      await new CreateProfiles(client, "create.ts", false).execUp();
      await new AddAvatar(client, "alter.ts", false).execUp();
      assert.equal(
        await knex.schema.hasColumn("m_profiles", "avatar_url"),
        true
      );

      await new CreateProfiles(client, "create.ts", false).execDown();
    });
  });
});

// =========================================================================
// 10. SEEDERS
// =========================================================================

describe("Seeders", () => {
  it("seed via raw query builder", async () => {
    await withDatabase(async (db) => {
      const client = db.connection();
      const knex = client.getWriteClient();
      await knex.schema.createTable("seed_roles", (t) => {
        t.increments("id");
        t.string("name");
      });

      class RoleSeeder extends BaseSeeder {
        async run() {
          await this.client
            .table("seed_roles")
            .multiInsert([
              { name: "admin" },
              { name: "editor" },
              { name: "viewer" },
            ]);
        }
      }

      await new RoleSeeder(client).run();
      const roles = await knex("seed_roles").orderBy("name");
      assert.equal(roles.length, 3);
      assert.equal(roles[0].name, "admin");
      assert.equal(roles[1].name, "editor");
      assert.equal(roles[2].name, "viewer");
    });
  });

  it("seed via model", async () => {
    await withDatabase(async (db) => {
      const client = db.connection();
      const knex = client.getWriteClient();
      await knex.schema.createTable("seed_categories", (t) => {
        t.increments("id");
        t.string("name");
        t.string("slug");
      });

      const Category = defineModel("seed_categories", {
        id: { isPrimary: true },
        name: {},
        slug: {},
      });
      Category.$adapter = db.modelAdapter();

      class CategorySeeder extends BaseSeeder {
        async run() {
          await Category.createMany([
            { name: "Technology", slug: "technology" },
            { name: "Science", slug: "science" },
            { name: "Art", slug: "art" },
          ]);
        }
      }

      await new CategorySeeder(client).run();
      const cats = await Category.query().orderBy("name");
      assert.equal(cats.length, 3);
      assert.equal(cats[0].name, "Art");
      assert.equal(cats[2].name, "Technology");
    });
  });
});

// =========================================================================
// 11. DIALECT
// =========================================================================

describe("Dialect", () => {
  it("getAllTables", async () => {
    await withConnection(async (conn) => {
      const knex = conn.client;
      await knex.schema.createTable("tbl_a", (t) => t.increments("id"));
      await knex.schema.createTable("tbl_b", (t) => t.increments("id"));

      const { PetraDBDialect } = await import("../dist/dialect.js");
      const dialect = new PetraDBDialect(
        { rawQuery: (sql) => knex.raw(sql) },
        {}
      );
      const tables = await dialect.getAllTables();
      assert.ok(tables.includes("tbl_a"));
      assert.ok(tables.includes("tbl_b"));
    });
  });

  it("truncate", async () => {
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
      assert.equal((await knex("trunc_test").select("*")).length, 0);
    });
  });

  it("dropAllTables", async () => {
    await withConnection(async (conn) => {
      const knex = conn.client;
      await knex.schema.createTable("drop_a", (t) => t.increments("id"));
      await knex.schema.createTable("drop_b", (t) => t.increments("id"));

      const { PetraDBDialect } = await import("../dist/dialect.js");
      const dialect = new PetraDBDialect(
        { rawQuery: (sql) => knex.raw(sql) },
        {}
      );
      await dialect.dropAllTables();
      assert.equal(await knex.schema.hasTable("drop_a"), false);
      assert.equal(await knex.schema.hasTable("drop_b"), false);
    });
  });
});
