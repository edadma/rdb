import { describe, it, before, after } from "node:test";
import assert from "node:assert/strict";
import { Session } from "@petradb/engine";
import { drizzle } from "../dist/index.js";
import { pgTable, serial, text, integer, boolean } from "drizzle-orm/pg-core";
import { eq, gt } from "drizzle-orm";

const users = pgTable("users", {
  id: serial("id").primaryKey(),
  name: text("name").notNull(),
  email: text("email").notNull(),
  age: integer("age"),
  active: boolean("active").default(true),
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
      )
    `);
  });

  after(async () => {
    await session.close();
  });

  describe("insert", () => {
    it("should insert a single row", async () => {
      const result = await db.insert(users).values({
        name: "Alice",
        email: "alice@example.com",
        age: 30,
      });

      assert.ok(result);
    });

    it("should insert multiple rows", async () => {
      await db.insert(users).values([
        { name: "Bob", email: "bob@example.com", age: 25 },
        { name: "Charlie", email: "charlie@example.com", age: 35 },
      ]);

      const rows = await db.select().from(users);
      assert.ok(rows.length >= 3);
    });

    it("should insert and return with returning()", async () => {
      const result = await db
        .insert(users)
        .values({ name: "Diana", email: "diana@example.com", age: 28 })
        .returning();

      assert.equal(result.length, 1);
      assert.equal(result[0].name, "Diana");
      assert.equal(result[0].email, "diana@example.com");
      assert.ok(result[0].id);
    });
  });

  describe("select", () => {
    it("should select all rows", async () => {
      const rows = await db.select().from(users);
      assert.ok(rows.length >= 4);
      assert.ok(rows[0].id);
      assert.ok(rows[0].name);
    });

    it("should select with where clause", async () => {
      const rows = await db.select().from(users).where(eq(users.name, "Alice"));
      assert.equal(rows.length, 1);
      assert.equal(rows[0].name, "Alice");
      assert.equal(rows[0].age, 30);
    });

    it("should select with gt condition", async () => {
      const rows = await db.select().from(users).where(gt(users.age, 28));
      assert.ok(rows.length >= 2);
      for (const row of rows) {
        assert.ok(row.age > 28);
      }
    });

    it("should select specific columns", async () => {
      const rows = await db
        .select({ name: users.name, email: users.email })
        .from(users)
        .where(eq(users.name, "Bob"));

      assert.equal(rows.length, 1);
      assert.equal(rows[0].name, "Bob");
      assert.equal(rows[0].email, "bob@example.com");
      assert.equal(rows[0].id, undefined);
    });
  });

  describe("update", () => {
    it("should update rows", async () => {
      await db
        .update(users)
        .set({ age: 31 })
        .where(eq(users.name, "Alice"));

      const rows = await db.select().from(users).where(eq(users.name, "Alice"));
      assert.equal(rows[0].age, 31);
    });

    it("should update and return with returning()", async () => {
      const result = await db
        .update(users)
        .set({ active: false })
        .where(eq(users.name, "Bob"))
        .returning();

      assert.equal(result.length, 1);
      assert.equal(result[0].name, "Bob");
      assert.equal(result[0].active, false);
    });
  });

  describe("delete", () => {
    it("should delete rows", async () => {
      const before = await db.select().from(users).where(eq(users.name, "Charlie"));
      assert.equal(before.length, 1);

      await db.delete(users).where(eq(users.name, "Charlie"));

      const after = await db.select().from(users).where(eq(users.name, "Charlie"));
      assert.equal(after.length, 0);
    });

    it("should delete and return with returning()", async () => {
      const result = await db
        .delete(users)
        .where(eq(users.name, "Diana"))
        .returning();

      assert.equal(result.length, 1);
      assert.equal(result[0].name, "Diana");
    });
  });

  describe("transactions", () => {
    it("should commit via manual BEGIN/COMMIT", async () => {
      await session.execute("BEGIN");
      await db.insert(users).values({
        name: "Eve",
        email: "eve@example.com",
        age: 22,
      });
      await session.execute("COMMIT");

      const rows = await db.select().from(users).where(eq(users.name, "Eve"));
      assert.equal(rows.length, 1);
    });

    it("should rollback via manual ROLLBACK", async () => {
      await session.execute("BEGIN");
      await db.insert(users).values({
        name: "Frank",
        email: "frank@example.com",
        age: 40,
      });
      await session.execute("ROLLBACK");

      const rows = await db.select().from(users).where(eq(users.name, "Frank"));
      assert.equal(rows.length, 0);
    });
  });
});
