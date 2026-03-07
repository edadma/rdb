import { describe, it, before, after } from "node:test";
import assert from "node:assert/strict";
import { Session } from "@petradb/engine";
import { drizzle } from "../dist/index.js";
import { pgTable, serial, text, integer, boolean, numeric } from "drizzle-orm/pg-core";
import { eq, gt } from "drizzle-orm";

const users = pgTable("users", {
  id: serial("id").primaryKey(),
  name: text("name").notNull(),
  email: text("email").notNull(),
  age: integer("age"),
  active: boolean("active").default(true),
});

const products = pgTable("products", {
  id: serial("id").primaryKey(),
  name: text("name").notNull(),
  price: numeric("price", { precision: 10, scale: 2 }).notNull(),
  inStock: boolean("in_stock").default(true),
  quantity: integer("quantity").default(0),
});

describe("@petradb/drizzle", () => {
  let session;
  let db;

  before(async () => {
    session = new Session({ storage: "memory" });
    db = drizzle(session);

    await session.execute(`
      CREATE TABLE users (
        id SERIAL PRIMARY KEY,
        name TEXT NOT NULL,
        email TEXT NOT NULL,
        age INTEGER,
        active BOOLEAN DEFAULT true
      );
      CREATE TABLE products (
        id SERIAL PRIMARY KEY,
        name TEXT NOT NULL,
        price NUMERIC(10, 2) NOT NULL,
        in_stock BOOLEAN DEFAULT true,
        quantity INTEGER DEFAULT 0
      )
    `);
  });

  after(async () => {
    await session.close();
  });

  describe("insert", () => {
    it("inserts a single row", async () => {
      const result = await db.insert(users).values({
        name: "Alice",
        email: "alice@example.com",
        age: 30,
      });
      assert.ok(result);
    });

    it("inserts multiple rows", async () => {
      await db.insert(users).values([
        { name: "Bob", email: "bob@example.com", age: 25 },
        { name: "Charlie", email: "charlie@example.com", age: 35 },
      ]);
      const rows = await db.select().from(users);
      assert.ok(rows.length >= 3);
    });

    it("insert without returning() does not return rows", async () => {
      const result = await db.insert(users).values({
        name: "NoReturn",
        email: "noreturn@example.com",
      });
      assert.ok(!Array.isArray(result) || result.length === 0);
    });

    it("inserts and returns with returning()", async () => {
      const result = await db
        .insert(users)
        .values({ name: "Diana", email: "diana@example.com", age: 28 })
        .returning();

      assert.equal(result.length, 1);
      assert.equal(result[0].name, "Diana");
      assert.ok(result[0].id);
    });
  });

  describe("type mapping", () => {
    it("returns number for INTEGER columns, not string", async () => {
      const rows = await db.select().from(users).where(eq(users.name, "Alice"));
      assert.equal(rows.length, 1);
      assert.equal(typeof rows[0].id, "number");
      assert.equal(typeof rows[0].age, "number");
    });

    it("returns boolean true for BOOLEAN columns, not string", async () => {
      const rows = await db.select().from(users).where(eq(users.name, "Alice"));
      assert.equal(typeof rows[0].active, "boolean");
      assert.strictEqual(rows[0].active, true);
    });

    it("returns boolean false correctly, not string", async () => {
      await db.update(users).set({ active: false }).where(eq(users.name, "Bob"));
      const rows = await db.select().from(users).where(eq(users.name, "Bob"));
      assert.strictEqual(rows[0].active, false);
    });

    it("returns null for nullable columns with no value", async () => {
      const rows = await db.select().from(users).where(eq(users.name, "NoReturn"));
      assert.strictEqual(rows[0].age, null);
    });

    it("limit(1) row has correct JS types", async () => {
      const rows = await db.select().from(users).where(eq(users.name, "Alice")).limit(1);
      assert.equal(rows.length, 1);
      assert.equal(typeof rows[0].id, "number");
      assert.equal(typeof rows[0].name, "string");
      assert.equal(typeof rows[0].age, "number");
      assert.equal(typeof rows[0].active, "boolean");
    });

    it("returning() result has correct JS types", async () => {
      const result = await db
        .insert(users)
        .values({ name: "TypeTest", email: "type@example.com", age: 99 })
        .returning();
      assert.equal(typeof result[0].id, "number");
      assert.equal(typeof result[0].age, "number");
      assert.equal(typeof result[0].active, "boolean");
    });
  });

  describe("select", () => {
    it("selects all rows", async () => {
      const rows = await db.select().from(users);
      assert.ok(rows.length >= 4);
    });

    it("selects with eq() where clause", async () => {
      const rows = await db.select().from(users).where(eq(users.name, "Alice"));
      assert.equal(rows.length, 1);
      assert.equal(rows[0].age, 30);
    });

    it("selects with gt() condition", async () => {
      const rows = await db.select().from(users).where(gt(users.age, 28));
      assert.ok(rows.length >= 2);
      for (const row of rows) {
        assert.ok(row.age > 28);
      }
    });

    it("selects specific columns", async () => {
      const rows = await db
        .select({ name: users.name, email: users.email })
        .from(users)
        .where(eq(users.name, "Bob"));

      assert.equal(rows.length, 1);
      assert.equal(rows[0].name, "Bob");
      assert.equal(rows[0].id, undefined);
    });
  });

  describe("update", () => {
    it("updates rows", async () => {
      await db.update(users).set({ age: 31 }).where(eq(users.name, "Alice"));
      const rows = await db.select().from(users).where(eq(users.name, "Alice"));
      assert.equal(rows[0].age, 31);
      assert.equal(typeof rows[0].age, "number");
    });

    it("updates and returns with returning()", async () => {
      const result = await db
        .update(users)
        .set({ active: false })
        .where(eq(users.name, "Charlie"))
        .returning();

      assert.equal(result.length, 1);
      assert.strictEqual(result[0].active, false);
    });
  });

  describe("delete", () => {
    it("deletes rows", async () => {
      await db.delete(users).where(eq(users.name, "Charlie"));
      const after = await db.select().from(users).where(eq(users.name, "Charlie"));
      assert.equal(after.length, 0);
    });

    it("deletes and returns with returning()", async () => {
      const result = await db.delete(users).where(eq(users.name, "Diana")).returning();
      assert.equal(result.length, 1);
      assert.equal(result[0].name, "Diana");
    });
  });

  // Drizzle's db.transaction() is not supported by pg-proxy.
  // Use db.$session to issue BEGIN/COMMIT/ROLLBACK directly.
  describe("transactions", () => {
    it("commits via manual BEGIN/COMMIT", async () => {
      await db.$session.execute("BEGIN");
      await db.insert(users).values({ name: "Eve", email: "eve@example.com", age: 22 });
      await db.$session.execute("COMMIT");
      const rows = await db.select().from(users).where(eq(users.name, "Eve"));
      assert.equal(rows.length, 1);
    });

    it("rolls back via manual ROLLBACK", async () => {
      await db.$session.execute("BEGIN");
      await db.insert(users).values({ name: "Frank", email: "frank@example.com", age: 40 });
      await db.$session.execute("ROLLBACK");
      const rows = await db.select().from(users).where(eq(users.name, "Frank"));
      assert.equal(rows.length, 0);
    });
  });
});
