import { describe, it, before } from "node:test";
import assert from "node:assert/strict";

const { ConnectSQL } = await import("../js/target/scala-3.8.1/rdb-fastopt/main.js");

describe("ConnectSQL", () => {
  let db: any;

  before(() => {
    db = new ConnectSQL();
  });

  describe("DDL results", () => {
    it("CREATE TABLE returns correct result", () => {
      const [res] = db.execute("CREATE TABLE t1 (id SERIAL, name TEXT)");
      assert.equal(res.command, "create table");
      assert.equal(res.table, "t1");
    });

    it("DROP TABLE returns correct result", () => {
      db.execute("CREATE TABLE drop_me (id INT)");
      const [res] = db.execute("DROP TABLE drop_me");
      assert.equal(res.command, "drop table");
      assert.equal(res.table, "drop_me");
    });

    it("CREATE TYPE returns correct result", () => {
      const [res] = db.execute("CREATE TYPE mood AS ENUM ('happy', 'sad', 'neutral')");
      assert.equal(res.command, "create type");
      assert.equal(res.type, "mood");
    });

    it("DROP TYPE returns correct result", () => {
      db.execute("CREATE TYPE temp_type AS ENUM ('a', 'b')");
      const [res] = db.execute("DROP TYPE temp_type");
      assert.equal(res.command, "drop type");
      assert.equal(res.type, "temp_type");
    });

    it("ALTER TABLE returns correct result", () => {
      db.execute("CREATE TABLE alter_me (id INT)");
      const [res] = db.execute("ALTER TABLE alter_me ADD COLUMN name TEXT");
      assert.equal(res.command, "alter table");
    });
  });

  describe("DML results", () => {
    it("INSERT returns correct result", () => {
      const [res] = db.execute("INSERT INTO t1 (name) VALUES ('Alice')");
      assert.equal(res.command, "insert");
      assert.equal(typeof res.result, "object");
    });

    it("UPDATE returns correct result", () => {
      const [res] = db.execute("UPDATE t1 SET name = 'Bob' WHERE name = 'Alice'");
      assert.equal(res.command, "update");
      assert.equal(typeof res.rows, "number");
      assert.equal(res.rows, 1);
    });

    it("DELETE returns correct result", () => {
      db.execute("INSERT INTO t1 (name) VALUES ('ToDelete')");
      const [res] = db.execute("DELETE FROM t1 WHERE name = 'ToDelete'");
      assert.equal(res.command, "delete");
      assert.equal(typeof res.rows, "number");
    });
  });

  describe("SELECT results", () => {
    it("returns rows as arrays", () => {
      const [res] = db.execute("SELECT * FROM t1");
      assert.equal(res.command, "select");
      assert.ok(Array.isArray(res.result));
    });
  });

  describe("value conversions", () => {
    before(() => {
      db.execute(`
        CREATE TYPE color AS ENUM ('red', 'green', 'blue');
        CREATE TABLE conversions (
          id SERIAL,
          int_col INT,
          double_col DOUBLE,
          numeric_col NUMERIC(10,2),
          text_col TEXT,
          bool_col BOOLEAN,
          uuid_col UUID DEFAULT gen_random_uuid(),
          ts_col TIMESTAMP,
          enum_col color,
          json_arr JSON,
          json_obj JSON
        )
      `);
      db.execute(`
        INSERT INTO conversions (int_col, double_col, numeric_col, text_col, bool_col, ts_col, enum_col, json_arr, json_obj)
        VALUES (42, 3.14, 99.95, 'hello', TRUE, '2025-01-15 10:30:00', 'red', '[1, 2, 3]', '{"key": "value"}')
      `);
    });

    it("INT converts to number", () => {
      const [res] = db.execute("SELECT int_col FROM conversions");
      assert.equal(res.result[0][0], 42);
      assert.equal(typeof res.result[0][0], "number");
    });

    it("DOUBLE converts to number", () => {
      const [res] = db.execute("SELECT double_col FROM conversions");
      assert.equal(res.result[0][0], 3.14);
      assert.equal(typeof res.result[0][0], "number");
    });

    it("NUMERIC converts to number", () => {
      const [res] = db.execute("SELECT numeric_col FROM conversions");
      assert.equal(res.result[0][0], 99.95);
      assert.equal(typeof res.result[0][0], "number");
    });

    it("TEXT converts to string", () => {
      const [res] = db.execute("SELECT text_col FROM conversions");
      assert.equal(res.result[0][0], "hello");
      assert.equal(typeof res.result[0][0], "string");
    });

    it("BOOLEAN converts to boolean", () => {
      const [res] = db.execute("SELECT bool_col FROM conversions");
      assert.equal(res.result[0][0], true);
      assert.equal(typeof res.result[0][0], "boolean");
    });

    it("UUID converts to string", () => {
      const [res] = db.execute("SELECT uuid_col FROM conversions");
      assert.equal(typeof res.result[0][0], "string");
      assert.match(res.result[0][0], /^[0-9a-f-]{36}$/);
    });

    it("TIMESTAMP converts to Date", () => {
      const [res] = db.execute("SELECT ts_col FROM conversions");
      assert.ok(res.result[0][0] instanceof Date);
    });

    it("ENUM converts to string label", () => {
      const [res] = db.execute("SELECT enum_col FROM conversions");
      assert.equal(res.result[0][0], "red");
      assert.equal(typeof res.result[0][0], "string");
    });

    it("JSON array converts to Array", () => {
      const [res] = db.execute("SELECT json_arr FROM conversions");
      assert.deepEqual(res.result[0][0], [1, 2, 3]);
    });

    it("JSON object converts to Object", () => {
      const [res] = db.execute("SELECT json_obj FROM conversions");
      assert.deepEqual(res.result[0][0], { key: "value" });
    });

    it("NULL converts to null", () => {
      db.execute("INSERT INTO conversions (int_col) VALUES (1)");
      const [res] = db.execute("SELECT text_col FROM conversions WHERE int_col = 1");
      assert.equal(res.result[0][0], null);
    });
  });
});
