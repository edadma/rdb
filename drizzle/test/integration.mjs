import { describe, it, before, after } from "node:test";
import assert from "node:assert/strict";
import { fileURLToPath } from "node:url";
import { dirname, join } from "node:path";
import { Session } from "@petradb/engine";
import { drizzle, migrate } from "../dist/index.js";
import { pgTable, serial, text, integer, boolean, numeric } from "drizzle-orm/pg-core";
import { eq, gt, asc, desc, sql } from "drizzle-orm";

const __dirname = dirname(fileURLToPath(import.meta.url));

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

  describe("db.transaction()", () => {
    it("commits a transaction", async () => {
      const result = await db.transaction(async (tx) => {
        const inserted = await tx
          .insert(users)
          .values({ name: "TxCommit", email: "txcommit@example.com", age: 50 })
          .returning();
        return inserted;
      });

      assert.equal(result.length, 1);
      assert.equal(result[0].name, "TxCommit");

      const rows = await db.select().from(users).where(eq(users.name, "TxCommit"));
      assert.equal(rows.length, 1);
    });

    it("rolls back on error", async () => {
      await assert.rejects(async () => {
        await db.transaction(async (tx) => {
          await tx.insert(users).values({ name: "TxRollback", email: "txrollback@example.com", age: 60 });
          throw new Error("force rollback");
        });
      }, { message: "force rollback" });

      const rows = await db.select().from(users).where(eq(users.name, "TxRollback"));
      assert.equal(rows.length, 0);
    });

    it("rolls back via tx.rollback()", async () => {
      await assert.rejects(async () => {
        await db.transaction(async (tx) => {
          await tx.insert(users).values({ name: "TxExplicit", email: "txexplicit@example.com" });
          tx.rollback();
        });
      });

      const rows = await db.select().from(users).where(eq(users.name, "TxExplicit"));
      assert.equal(rows.length, 0);
    });

    it("supports select inside transaction", async () => {
      const rows = await db.transaction(async (tx) => {
        return tx.select().from(users).where(eq(users.name, "Alice"));
      });

      assert.equal(rows.length, 1);
      assert.equal(rows[0].name, "Alice");
    });

    it("supports update with returning() inside transaction", async () => {
      // Insert a user to update inside the transaction
      await db.insert(users).values({ name: "TxUpdate", email: "txupdate@example.com", age: 70 });

      const result = await db.transaction(async (tx) => {
        return tx
          .update(users)
          .set({ age: 71 })
          .where(eq(users.name, "TxUpdate"))
          .returning();
      });

      assert.equal(result.length, 1);
      assert.equal(result[0].age, 71);
    });

    it("supports delete with returning() inside transaction", async () => {
      const result = await db.transaction(async (tx) => {
        return tx.delete(users).where(eq(users.name, "TxUpdate")).returning();
      });

      assert.equal(result.length, 1);
      assert.equal(result[0].name, "TxUpdate");

      const rows = await db.select().from(users).where(eq(users.name, "TxUpdate"));
      assert.equal(rows.length, 0);
    });
  });

  describe("returning() with specific columns", () => {
    it("insert returning specific columns", async () => {
      const result = await db
        .insert(users)
        .values({ name: "Partial", email: "partial@example.com", age: 42 })
        .returning({ id: users.id, name: users.name });

      assert.equal(result.length, 1);
      assert.equal(result[0].name, "Partial");
      assert.equal(typeof result[0].id, "number");
      assert.equal(result[0].email, undefined);
      assert.equal(result[0].age, undefined);
    });

    it("update returning specific columns", async () => {
      const result = await db
        .update(users)
        .set({ age: 43 })
        .where(eq(users.name, "Partial"))
        .returning({ name: users.name, age: users.age });

      assert.equal(result.length, 1);
      assert.equal(result[0].name, "Partial");
      assert.equal(result[0].age, 43);
      assert.equal(result[0].id, undefined);
    });

    it("delete returning specific columns", async () => {
      const result = await db
        .delete(users)
        .where(eq(users.name, "Partial"))
        .returning({ email: users.email });

      assert.equal(result.length, 1);
      assert.equal(result[0].email, "partial@example.com");
      assert.equal(result[0].name, undefined);
    });
  });

  describe("orderBy and offset", () => {
    before(async () => {
      // Seed products via raw SQL (drizzle sends numeric as text params;
      // PetraDB doesn't yet auto-convert text→numeric in parameterized inserts)
      await session.execute(`
        INSERT INTO products (name, price, quantity) VALUES
          ('Apple', 1.50, 10),
          ('Banana', 0.75, 20),
          ('Cherry', 3.00, 5)
      `);
    });

    it("orders by ascending", async () => {
      const rows = await db.select().from(products).orderBy(asc(products.name));
      assert.equal(rows[0].name, "Apple");
      assert.equal(rows[1].name, "Banana");
      assert.equal(rows[2].name, "Cherry");
    });

    it("orders by descending", async () => {
      const rows = await db.select().from(products).orderBy(desc(products.price));
      assert.equal(rows[0].name, "Cherry");
      assert.equal(rows[rows.length - 1].name, "Banana");
    });

    it("supports offset", async () => {
      const rows = await db
        .select()
        .from(products)
        .orderBy(asc(products.name))
        .limit(2)
        .offset(1);

      assert.equal(rows.length, 2);
      assert.equal(rows[0].name, "Banana");
      assert.equal(rows[1].name, "Cherry");
    });

    it("orderBy with limit", async () => {
      const rows = await db
        .select()
        .from(products)
        .orderBy(asc(products.name))
        .limit(1);

      assert.equal(rows.length, 1);
      assert.equal(rows[0].name, "Apple");
    });
  });

  describe("joins", () => {
    before(async () => {
      await session.execute(`
        CREATE TABLE orders (
          id SERIAL PRIMARY KEY,
          user_id INTEGER NOT NULL,
          product_id INTEGER NOT NULL,
          quantity INTEGER NOT NULL
        )
      `);
    });

    // Define the orders table for drizzle
    const orders = pgTable("orders", {
      id: serial("id").primaryKey(),
      userId: integer("user_id").notNull(),
      productId: integer("product_id").notNull(),
      quantity: integer("quantity").notNull(),
    });

    it("inner join", async () => {
      // Seed an order referencing existing users and products
      const aliceRows = await db.select({ id: users.id }).from(users).where(eq(users.name, "Alice"));
      const appleRows = await db.select({ id: products.id }).from(products).where(eq(products.name, "Apple"));
      assert.ok(aliceRows.length > 0, "Alice should exist");
      assert.ok(appleRows.length > 0, "Apple should exist");

      await db.insert(orders).values({
        userId: aliceRows[0].id,
        productId: appleRows[0].id,
        quantity: 3,
      });

      const rows = await db
        .select({
          userName: users.name,
          productName: products.name,
          orderQty: orders.quantity,
        })
        .from(orders)
        .innerJoin(users, eq(orders.userId, users.id))
        .innerJoin(products, eq(orders.productId, products.id));

      assert.ok(rows.length >= 1);
      const aliceOrder = rows.find((r) => r.userName === "Alice" && r.productName === "Apple");
      assert.ok(aliceOrder);
      assert.equal(aliceOrder.orderQty, 3);
    });

    it("left join returns null for non-matching rows", async () => {
      // Bob has no orders
      const rows = await db
        .select({
          userName: users.name,
          orderId: orders.id,
        })
        .from(users)
        .leftJoin(orders, eq(users.id, orders.userId))
        .where(eq(users.name, "Bob"));

      assert.equal(rows.length, 1);
      assert.equal(rows[0].userName, "Bob");
      assert.strictEqual(rows[0].orderId, null);
    });
  });

  describe("$count", () => {
    it("counts all rows in a table", async () => {
      const count = await db.$count(users);
      assert.equal(typeof count, "number");
      assert.ok(count > 0);
    });

    it("counts with a filter", async () => {
      const count = await db.$count(users, gt(users.age, 28));
      assert.equal(typeof count, "number");
      assert.ok(count > 0);
    });
  });

  describe("numeric type mapping", () => {
    it("returns string for NUMERIC columns", async () => {
      const rows = await db.select().from(products).where(eq(products.name, "Apple"));
      assert.equal(rows.length, 1);
      assert.equal(typeof rows[0].price, "string");
      assert.equal(Number(rows[0].price), 1.5);
    });

    it("returns number for INTEGER columns in products", async () => {
      const rows = await db.select().from(products).where(eq(products.name, "Banana"));
      assert.equal(typeof rows[0].quantity, "number");
      assert.equal(rows[0].quantity, 20);
    });

    it("returns boolean for BOOLEAN columns in products", async () => {
      const rows = await db.select().from(products).where(eq(products.name, "Cherry"));
      assert.equal(typeof rows[0].inStock, "boolean");
      assert.strictEqual(rows[0].inStock, true);
    });
  });

  describe("parameterized raw SQL", () => {
    it("interpolates values in sql template", async () => {
      const name = "Alice";
      const result = await db.execute(sql`SELECT * FROM users WHERE name = ${name}`);
      assert.equal(result.rows.length, 1);
      assert.equal(result.rows[0].name, "Alice");
    });

    it("interpolates multiple values", async () => {
      const minAge = 20;
      const maxAge = 40;
      const result = await db.execute(
        sql`SELECT * FROM users WHERE age >= ${minAge} AND age <= ${maxAge}`,
      );
      assert.ok(result.rows.length > 0);
      for (const row of result.rows) {
        assert.ok(row.age >= minAge && row.age <= maxAge);
      }
    });
  });

  describe("raw SQL via db.execute()", () => {
    it("executes raw SQL and returns result", async () => {
      const result = await db.execute(sql`SELECT count(*) as count FROM users`);
      assert.ok(result.rows.length > 0);
      assert.ok(Number(result.rows[0].count) > 0);
    });
  });

  // Manual BEGIN/COMMIT/ROLLBACK still works via $session
  describe("manual transactions via $session", () => {
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

describe("migrate()", () => {
  let session;
  let db;

  before(async () => {
    session = new Session({ storage: "memory" });
    db = drizzle(session);
  });

  after(async () => {
    await session.close();
  });

  it("applies migration files and creates tables", async () => {
    await migrate(db, { migrationsFolder: join(__dirname, "migrations") });

    // Verify users table was created by migration 0000
    const [result] = await session.execute("SELECT * FROM users", { rowMode: "object" });
    assert.ok(result);

    // Verify posts table was created by migration 0001
    const [postsResult] = await session.execute("SELECT * FROM posts", { rowMode: "object" });
    assert.ok(postsResult);
  });

  it("created tables are usable via drizzle", async () => {
    const migratedUsers = pgTable("users", {
      id: serial("id").primaryKey(),
      name: text("name").notNull(),
      email: text("email").notNull(),
    });

    const migratedPosts = pgTable("posts", {
      id: serial("id").primaryKey(),
      userId: integer("user_id").notNull(),
      title: text("title").notNull(),
    });

    // Insert via drizzle
    const [user] = await db
      .insert(migratedUsers)
      .values({ name: "Alice", email: "alice@example.com" })
      .returning();
    assert.equal(user.name, "Alice");

    const [post] = await db
      .insert(migratedPosts)
      .values({ userId: user.id, title: "Hello World" })
      .returning();
    assert.equal(post.title, "Hello World");

    // Query via drizzle
    const rows = await db.select().from(migratedPosts);
    assert.equal(rows.length, 1);
    assert.equal(rows[0].title, "Hello World");
  });

  it("foreign key constraint is enforced after migration", async () => {
    const migratedPosts = pgTable("posts", {
      id: serial("id").primaryKey(),
      userId: integer("user_id").notNull(),
      title: text("title").notNull(),
    });

    // Should fail — no user with id 9999
    await assert.rejects(async () => {
      await db.insert(migratedPosts).values({ userId: 9999, title: "Bad" });
    });
  });

  it("is idempotent — running twice does not error", async () => {
    // Running migrate again should be a no-op (migrations already applied)
    await migrate(db, { migrationsFolder: join(__dirname, "migrations") });
  });

  it("tracks migrations in drizzle.__drizzle_migrations", async () => {
    const [result] = await session.execute(
      'SELECT hash, created_at FROM "drizzle"."__drizzle_migrations" ORDER BY created_at',
      { rowMode: "object" },
    );
    assert.ok(result.rows.length >= 2, "Should have at least 2 migration records");
  });
});
