import { describe, it, before, after } from "node:test";
import assert from "node:assert/strict";
import { createRequire } from "node:module";
import { fileURLToPath } from "node:url";
import path from "node:path";

// Resolve knex from the npm/ directory where it's installed
const npmDir = path.resolve(
  path.dirname(fileURLToPath(import.meta.url)),
  "../npm"
);
const require = createRequire(path.join(npmDir, "index.js"));
const Knex = require("knex");

const { default: PetraDBClient } = await import("../npm/src/index.ts");

function createKnex() {
  return Knex({
    client: PetraDBClient as any,
    connection: { storage: "memory" },
    useNullAsDefault: true,
  });
}

describe("Knex PetraDB dialect", () => {
  let knex: ReturnType<typeof Knex>;

  before(() => {
    knex = createKnex();
  });

  after(async () => {
    await knex.destroy();
  });

  // -----------------------------------------------------------------------
  // Schema builder
  // -----------------------------------------------------------------------
  describe("schema builder", () => {
    it("creates a table with various column types", async () => {
      await knex.schema.createTable("users", (t) => {
        t.increments("id");
        t.string("name", 100);
        t.text("bio");
        t.integer("age");
        t.boolean("active");
        t.timestamp("created_at");
      });

      const has = await knex.schema.hasTable("users");
      assert.equal(has, true);
    });

    it("hasTable returns false for non-existent table", async () => {
      const has = await knex.schema.hasTable("nonexistent");
      assert.equal(has, false);
    });

    it("hasColumn returns true for existing column", async () => {
      const has = await knex.schema.hasColumn("users", "name");
      assert.equal(has, true);
    });

    it("hasColumn returns false for non-existent column", async () => {
      const has = await knex.schema.hasColumn("users", "zzz");
      assert.equal(has, false);
    });

    it("createTable with bigIncrements, uuid, json types", async () => {
      await knex.schema.createTable("misc_types", (t) => {
        t.bigIncrements("id");
        t.uuid("uid");
        t.json("meta");
        t.jsonb("data");
        t.float("score");
        t.date("born");
        t.time("alarm");
        t.binary("blob");
      });

      const has = await knex.schema.hasTable("misc_types");
      assert.equal(has, true);
    });

    it("createTableIfNotExists is idempotent", async () => {
      await knex.schema.createTableIfNotExists("idempotent_t", (t) => {
        t.increments("id");
      });
      // Should not throw on second call
      await knex.schema.createTableIfNotExists("idempotent_t", (t) => {
        t.increments("id");
      });
      const has = await knex.schema.hasTable("idempotent_t");
      assert.equal(has, true);
    });

    it("dropTable removes a table", async () => {
      await knex.schema.createTable("to_drop", (t) => {
        t.increments("id");
      });
      await knex.schema.dropTable("to_drop");
      const has = await knex.schema.hasTable("to_drop");
      assert.equal(has, false);
    });

    it("dropTableIfExists does not throw for missing table", async () => {
      await knex.schema.dropTableIfExists("never_existed");
    });
  });

  // -----------------------------------------------------------------------
  // Query builder – writes
  // -----------------------------------------------------------------------
  describe("query builder writes", () => {
    it("inserts a single row", async () => {
      const result = await knex("users").insert({
        name: "Alice",
        bio: "Hello",
        age: 30,
        active: true,
      });
      assert.ok(result);
    });

    it("inserts multiple rows", async () => {
      await knex("users").insert([
        { name: "Bob", age: 25, active: true },
        { name: "Charlie", age: 35, active: false },
      ]);
    });

    it("insert with returning", async () => {
      const rows = await knex("users")
        .insert({ name: "Diana", age: 28, active: true })
        .returning(["id", "name"]);
      assert.ok(Array.isArray(rows));
      assert.ok(rows.length > 0);
      assert.equal(rows[0].name, "Diana");
    });

    it("updates rows with where", async () => {
      const count = await knex("users")
        .where("name", "Alice")
        .update({ age: 31 });
      assert.equal(count, 1);
    });

    it("deletes rows with where", async () => {
      await knex("users").insert({ name: "ToDelete", age: 99, active: false });
      const count = await knex("users").where("name", "ToDelete").del();
      assert.equal(count, 1);
    });
  });

  // -----------------------------------------------------------------------
  // Query builder – reads
  // -----------------------------------------------------------------------
  describe("query builder reads", () => {
    it("select all rows", async () => {
      const rows = await knex("users").select("*");
      assert.ok(Array.isArray(rows));
      assert.ok(rows.length >= 3); // Alice, Bob, Charlie, Diana
    });

    it("select with where", async () => {
      const rows = await knex("users").where("name", "Alice").select("*");
      assert.equal(rows.length, 1);
      assert.equal(rows[0].name, "Alice");
      assert.equal(rows[0].age, 31); // updated earlier
    });

    it("select with whereIn", async () => {
      const rows = await knex("users").whereIn("name", ["Alice", "Bob"]);
      assert.equal(rows.length, 2);
    });

    it("select with orderBy", async () => {
      const rows = await knex("users").orderBy("age", "asc").select("name", "age");
      for (let i = 1; i < rows.length; i++) {
        assert.ok(rows[i].age >= rows[i - 1].age);
      }
    });

    it("select with limit and offset", async () => {
      const rows = await knex("users").orderBy("id").limit(2).offset(1);
      assert.equal(rows.length, 2);
    });

    it("count aggregate", async () => {
      const [row] = await knex("users").count("* as cnt");
      assert.ok(Number(row.cnt) >= 3);
    });

    it("sum aggregate", async () => {
      const [row] = await knex("users").sum("age as total_age");
      assert.ok(Number(row.total_age) > 0);
    });

    it("min/max aggregates", async () => {
      const [minRow] = await knex("users").min("age as min_age");
      const [maxRow] = await knex("users").max("age as max_age");
      assert.ok(Number(minRow.min_age) <= Number(maxRow.max_age));
    });

    it("distinct", async () => {
      const rows = await knex("users").distinct("active");
      assert.ok(rows.length >= 1);
    });

    it("first() returns single object", async () => {
      const row = await knex("users").where("name", "Alice").first();
      assert.equal(typeof row, "object");
      assert.equal(row.name, "Alice");
    });
  });

  // -----------------------------------------------------------------------
  // Joins
  // -----------------------------------------------------------------------
  describe("joins", () => {
    before(async () => {
      await knex.schema.createTable("orders", (t) => {
        t.increments("id");
        t.integer("user_id");
        t.text("product");
      });
      // Get Alice's id
      const [alice] = await knex("users").where("name", "Alice").select("id");
      await knex("orders").insert([
        { user_id: alice.id, product: "Widget" },
        { user_id: alice.id, product: "Gadget" },
      ]);
    });

    it("inner join", async () => {
      const rows = await knex("users")
        .join("orders", "users.id", "orders.user_id")
        .where("users.name", "Alice")
        .select("users.name", "orders.product");
      assert.equal(rows.length, 2);
    });
  });

  // -----------------------------------------------------------------------
  // Group by / having
  // -----------------------------------------------------------------------
  describe("group by / having", () => {
    it("group by with count", async () => {
      const rows = await knex("users")
        .select("active")
        .count("* as cnt")
        .groupBy("active");
      assert.ok(rows.length >= 1);
    });
  });

  // -----------------------------------------------------------------------
  // Transactions
  // -----------------------------------------------------------------------
  describe("transactions", () => {
    it("commit persists changes", async () => {
      const trx = await knex.transaction();
      await trx("users").insert({ name: "TxUser", age: 50, active: true });
      await trx.commit();

      const rows = await knex("users").where("name", "TxUser");
      assert.equal(rows.length, 1);
    });

    it("rollback discards changes", async () => {
      const trx = await knex.transaction();
      await trx("users").insert({ name: "RolledBack", age: 99, active: true });
      await trx.rollback();

      const rows = await knex("users").where("name", "RolledBack");
      assert.equal(rows.length, 0);
    });

    it("callback API with automatic commit", async () => {
      await knex.transaction(async (trx) => {
        await trx("users").insert({ name: "CbUser", age: 42, active: true });
      });

      const rows = await knex("users").where("name", "CbUser");
      assert.equal(rows.length, 1);
    });
  });

  // -----------------------------------------------------------------------
  // Raw queries
  // -----------------------------------------------------------------------
  describe("raw queries", () => {
    it("knex.raw() executes SQL", async () => {
      const result = await knex.raw("SELECT 1 + 1 AS result");
      assert.ok(result);
    });

    it("knex.raw() with bindings", async () => {
      const result = await knex.raw("SELECT ? + ? AS result", [3, 4]);
      assert.ok(result);
    });
  });

  // -----------------------------------------------------------------------
  // Connection lifecycle
  // -----------------------------------------------------------------------
  describe("connection lifecycle", () => {
    it("knex.destroy() cleans up", async () => {
      const k2 = createKnex();
      await k2("users").select("*").catch(() => {});
      await k2.destroy();
    });
  });
});
