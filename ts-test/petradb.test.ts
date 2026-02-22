import { describe, it, before } from "node:test";
import assert from "node:assert/strict";

const { ConnectSQL } = await import("../engine/js/target/scala-3.8.1/petradb-engine-opt/main.js");

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

    it("CREATE TABLE IF NOT EXISTS on existing table is no-op", () => {
      db.execute("CREATE TABLE ifne (id INT)");
      const [res] = db.execute("CREATE TABLE IF NOT EXISTS ifne (id INT)");
      assert.equal(res.command, "create table");
      assert.equal(res.table, "ifne");
    });

    it("CREATE INDEX returns correct result", () => {
      db.execute("CREATE TABLE idx_t (id INT, name TEXT)");
      const [res] = db.execute("CREATE INDEX idx_name ON idx_t (name)");
      assert.equal(res.command, "create index");
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

    it("TRUNCATE returns correct result", () => {
      db.execute("CREATE TABLE trunc_t (id SERIAL, val TEXT)");
      db.execute("INSERT INTO trunc_t (val) VALUES ('a'), ('b'), ('c')");
      const [res] = db.execute("TRUNCATE TABLE trunc_t");
      assert.equal(res.command, "truncate table");
      assert.equal(res.table, "trunc_t");
    });

    it("TRUNCATE empties table and resets serial", () => {
      db.execute("CREATE TABLE trunc2 (id SERIAL, val TEXT)");
      db.execute("INSERT INTO trunc2 (val) VALUES ('a'), ('b')");
      db.execute("TRUNCATE TABLE trunc2");
      const [sel] = db.execute("SELECT * FROM trunc2");
      assert.equal(sel.rows.length, 0);
      // Serial resets — next insert gets id=1
      db.execute("INSERT INTO trunc2 (val) VALUES ('new')");
      const [check] = db.execute("SELECT id, val FROM trunc2", { rowMode: "array" });
      assert.equal(check.rows[0][0], 1);
    });
  });

  describe("SELECT results", () => {
    it("returns rows as objects by default", () => {
      const [res] = db.execute("SELECT * FROM t1");
      assert.equal(res.command, "select");
      assert.ok(Array.isArray(res.rows));
      assert.ok(Array.isArray(res.fields));
      if (res.rows.length > 0) {
        assert.equal(typeof res.rows[0], "object");
        assert.ok(!Array.isArray(res.rows[0]));
        assert.ok("name" in res.rows[0]);
      }
    });

    it("returns fields with name and dataType", () => {
      const [res] = db.execute("SELECT * FROM t1");
      assert.ok(res.fields.length > 0);
      for (const field of res.fields) {
        assert.equal(typeof field.name, "string");
        assert.equal(typeof field.dataType, "string");
      }
    });

    it("returns rows as arrays with rowMode option", () => {
      const [res] = db.execute("SELECT * FROM t1", { rowMode: "array" });
      assert.equal(res.command, "select");
      assert.ok(Array.isArray(res.rows));
      if (res.rows.length > 0) {
        assert.ok(Array.isArray(res.rows[0]));
      }
    });
  });

  describe("constructor rowMode", () => {
    it("defaults to array mode when set in constructor", () => {
      const arrayDb = new ConnectSQL({ rowMode: "array" });
      arrayDb.execute("CREATE TABLE ctest (id INT, name TEXT)");
      arrayDb.execute("INSERT INTO ctest (id, name) VALUES (1, 'a')");
      const [res] = arrayDb.execute("SELECT * FROM ctest");
      assert.ok(Array.isArray(res.rows[0]));
    });

    it("per-call option overrides constructor default", () => {
      const arrayDb = new ConnectSQL({ rowMode: "array" });
      arrayDb.execute("CREATE TABLE ctest2 (id INT, name TEXT)");
      arrayDb.execute("INSERT INTO ctest2 (id, name) VALUES (1, 'a')");
      const [res] = arrayDb.execute("SELECT * FROM ctest2", { rowMode: "object" });
      assert.ok(!Array.isArray(res.rows[0]));
      assert.equal(typeof res.rows[0], "object");
      assert.equal(res.rows[0].name, "a");
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
      const [res] = db.execute("SELECT int_col FROM conversions", { rowMode: "array" });
      assert.equal(res.rows[0][0], 42);
      assert.equal(typeof res.rows[0][0], "number");
    });

    it("DOUBLE converts to number", () => {
      const [res] = db.execute("SELECT double_col FROM conversions", { rowMode: "array" });
      assert.equal(res.rows[0][0], 3.14);
      assert.equal(typeof res.rows[0][0], "number");
    });

    it("NUMERIC converts to number", () => {
      const [res] = db.execute("SELECT numeric_col FROM conversions", { rowMode: "array" });
      assert.equal(res.rows[0][0], 99.95);
      assert.equal(typeof res.rows[0][0], "number");
    });

    it("TEXT converts to string", () => {
      const [res] = db.execute("SELECT text_col FROM conversions", { rowMode: "array" });
      assert.equal(res.rows[0][0], "hello");
      assert.equal(typeof res.rows[0][0], "string");
    });

    it("BOOLEAN converts to boolean", () => {
      const [res] = db.execute("SELECT bool_col FROM conversions", { rowMode: "array" });
      assert.equal(res.rows[0][0], true);
      assert.equal(typeof res.rows[0][0], "boolean");
    });

    it("UUID converts to string", () => {
      const [res] = db.execute("SELECT uuid_col FROM conversions", { rowMode: "array" });
      assert.equal(typeof res.rows[0][0], "string");
      assert.match(res.rows[0][0], /^[0-9a-f-]{36}$/);
    });

    it("TIMESTAMP converts to Date", () => {
      const [res] = db.execute("SELECT ts_col FROM conversions", { rowMode: "array" });
      assert.ok(res.rows[0][0] instanceof Date);
    });

    it("ENUM converts to string label", () => {
      const [res] = db.execute("SELECT enum_col FROM conversions", { rowMode: "array" });
      assert.equal(res.rows[0][0], "red");
      assert.equal(typeof res.rows[0][0], "string");
    });

    it("JSON array converts to Array", () => {
      const [res] = db.execute("SELECT json_arr FROM conversions", { rowMode: "array" });
      assert.deepEqual(res.rows[0][0], [1, 2, 3]);
    });

    it("JSON object converts to Object", () => {
      const [res] = db.execute("SELECT json_obj FROM conversions", { rowMode: "array" });
      assert.deepEqual(res.rows[0][0], { key: "value" });
    });

    it("NULL converts to null", () => {
      db.execute("INSERT INTO conversions (int_col) VALUES (1)");
      const [res] = db.execute("SELECT text_col FROM conversions WHERE int_col = 1", { rowMode: "array" });
      assert.equal(res.rows[0][0], null);
    });

    it("values accessible by column name in object mode", () => {
      const [res] = db.execute("SELECT int_col, text_col FROM conversions WHERE int_col = 42");
      assert.equal(res.rows[0].int_col, 42);
      assert.equal(res.rows[0].text_col, "hello");
    });
  });

  describe("error handling", () => {
    it("throws on invalid SQL syntax", () => {
      assert.throws(() => db.execute("SELEKT * FROM t1"), /error|expected/i);
    });

    it("throws on unknown table", () => {
      assert.throws(() => db.execute("SELECT * FROM nonexistent"), /unknown|not found/i);
    });

    it("throws on unknown column in INSERT", () => {
      assert.throws(() => db.execute("INSERT INTO t1 (bogus) VALUES (1)"), /unknown|not found|bogus/i);
    });

    it("error is catchable with try/catch", () => {
      let caught = false;
      try {
        db.execute("NOT VALID SQL");
      } catch {
        caught = true;
      }
      assert.ok(caught);
    });
  });

  describe("multiple statements", () => {
    it("returns one result per statement", () => {
      const results = db.execute(`
        CREATE TABLE multi_a (id INT);
        CREATE TABLE multi_b (id INT)
      `);
      assert.equal(results.length, 2);
      assert.equal(results[0].command, "create table");
      assert.equal(results[0].table, "multi_a");
      assert.equal(results[1].command, "create table");
      assert.equal(results[1].table, "multi_b");
    });

    it("mixed statement types return correct results", () => {
      const results = db.execute(`
        INSERT INTO multi_a (id) VALUES (1);
        SELECT * FROM multi_a;
        DELETE FROM multi_a WHERE id = 1
      `);
      assert.equal(results.length, 3);
      assert.equal(results[0].command, "insert");
      assert.equal(results[1].command, "select");
      assert.equal(results[2].command, "delete");
    });
  });

  describe("empty results", () => {
    it("SELECT with no matching rows returns empty array", () => {
      db.execute("CREATE TABLE empty_t (id INT, name TEXT)");
      const [res] = db.execute("SELECT * FROM empty_t");
      assert.equal(res.command, "select");
      assert.ok(Array.isArray(res.rows));
      assert.equal(res.rows.length, 0);
    });

    it("UPDATE affecting zero rows returns rows = 0", () => {
      const [res] = db.execute("UPDATE empty_t SET name = 'x' WHERE id = 999");
      assert.equal(res.command, "update");
      assert.equal(res.rows, 0);
    });

    it("DELETE affecting zero rows returns rows = 0", () => {
      const [res] = db.execute("DELETE FROM empty_t WHERE id = 999");
      assert.equal(res.command, "delete");
      assert.equal(res.rows, 0);
    });
  });

  describe("INSERT result contents", () => {
    it("SERIAL value appears in insert result", () => {
      db.execute("CREATE TABLE serial_t (id SERIAL, name TEXT)");
      const [res] = db.execute("INSERT INTO serial_t (name) VALUES ('first')");
      assert.equal(typeof res.result.id, "number");
      assert.equal(res.result.id, 1);
    });

    it("SERIAL auto-increments", () => {
      const [r1] = db.execute("INSERT INTO serial_t (name) VALUES ('second')");
      const [r2] = db.execute("INSERT INTO serial_t (name) VALUES ('third')");
      assert.ok(r2.result.id > r1.result.id);
    });

    it("UUID value appears in insert result", () => {
      db.execute("CREATE TABLE uuid_t (id UUID DEFAULT gen_random_uuid(), name TEXT)");
      const [res] = db.execute("INSERT INTO uuid_t (name) VALUES ('test')");
      assert.equal(typeof res.result.id, "string");
      assert.match(res.result.id, /^[0-9a-f-]{36}$/);
    });
  });

  describe("instance isolation", () => {
    it("two instances do not share tables", () => {
      const db1 = new ConnectSQL();
      const db2 = new ConnectSQL();
      db1.execute("CREATE TABLE isolated (id INT)");
      assert.throws(() => db2.execute("SELECT * FROM isolated"), /unknown|not found/i);
    });

    it("two instances do not share data", () => {
      const db1 = new ConnectSQL();
      const db2 = new ConnectSQL();
      db1.execute("CREATE TABLE shared_name (id INT, val TEXT)");
      db2.execute("CREATE TABLE shared_name (id INT, val TEXT)");
      db1.execute("INSERT INTO shared_name (id, val) VALUES (1, 'from db1')");
      const [res] = db2.execute("SELECT * FROM shared_name");
      assert.equal(res.rows.length, 0);
    });
  });

  describe("value edge cases", () => {
    before(() => {
      db.execute("CREATE TABLE edges (id SERIAL, int_col INT, bigint_col BIGINT, num_col NUMERIC(10,2), text_col TEXT)");
    });

    it("zero", () => {
      db.execute("INSERT INTO edges (int_col) VALUES (0)");
      const [res] = db.execute("SELECT int_col FROM edges WHERE int_col = 0", { rowMode: "array" });
      assert.equal(res.rows[0][0], 0);
      assert.equal(typeof res.rows[0][0], "number");
    });

    it("negative numbers", () => {
      db.execute("INSERT INTO edges (int_col) VALUES (-42)");
      const [res] = db.execute("SELECT int_col FROM edges WHERE int_col = -42", { rowMode: "array" });
      assert.equal(res.rows[0][0], -42);
    });

    it("empty string", () => {
      db.execute("INSERT INTO edges (text_col) VALUES ('')");
      const [res] = db.execute("SELECT text_col FROM edges WHERE text_col = ''", { rowMode: "array" });
      assert.equal(res.rows[0][0], "");
      assert.equal(typeof res.rows[0][0], "string");
    });

    it("unicode text", () => {
      db.execute("INSERT INTO edges (text_col) VALUES ('\u00e9\u2603\ud83d\ude80')");
      const [res] = db.execute("SELECT text_col FROM edges WHERE text_col LIKE '\u00e9%'", { rowMode: "array" });
      assert.equal(res.rows[0][0], "\u00e9\u2603\ud83d\ude80");
    });

    it("NUMERIC preserves decimal precision", () => {
      db.execute("INSERT INTO edges (num_col) VALUES (100.10)");
      const [res] = db.execute("SELECT num_col FROM edges WHERE num_col = 100.10", { rowMode: "array" });
      assert.equal(res.rows[0][0], 100.1);
      assert.equal(typeof res.rows[0][0], "number");
    });
  });

  describe("DISTINCT", () => {
    before(() => {
      db.execute(`
        CREATE TABLE colors (id SERIAL, color TEXT);
        INSERT INTO colors (color) VALUES ('red');
        INSERT INTO colors (color) VALUES ('blue');
        INSERT INTO colors (color) VALUES ('red');
        INSERT INTO colors (color) VALUES ('green');
        INSERT INTO colors (color) VALUES ('blue')
      `);
    });

    it("removes duplicate values", () => {
      const [res] = db.execute("SELECT DISTINCT color FROM colors ORDER BY color", { rowMode: "array" });
      assert.equal(res.rows.length, 3);
      assert.equal(res.rows[0][0], "blue");
      assert.equal(res.rows[1][0], "green");
      assert.equal(res.rows[2][0], "red");
    });

    it("without DISTINCT returns all rows", () => {
      const [res] = db.execute("SELECT color FROM colors ORDER BY color");
      assert.equal(res.rows.length, 5);
    });

    it("works with LIMIT", () => {
      const [res] = db.execute("SELECT DISTINCT color FROM colors ORDER BY color LIMIT 2");
      assert.equal(res.rows.length, 2);
    });
  });

  describe("SQL compatibility", () => {
    it("case-insensitive keywords", () => {
      const d = new ConnectSQL();
      d.execute("create table ci_kw (id int)");
      d.execute("Insert Into ci_kw (id) Values (1)");
      const [res] = d.execute("select * from ci_kw");
      assert.equal(res.rows.length, 1);
    });

    it("unquoted identifiers fold to lowercase", () => {
      const d = new ConnectSQL();
      d.execute("CREATE TABLE MyTable (MyCol INT)");
      d.execute("INSERT INTO mytable (mycol) VALUES (42)");
      const [res] = d.execute("SELECT MYCOL FROM MYTABLE", { rowMode: "array" });
      assert.equal(res.rows[0][0], 42);
    });

    it("double-quoted identifiers preserve case", () => {
      const d = new ConnectSQL();
      d.execute('CREATE TABLE dq ("MixedCase" TEXT)');
      d.execute('INSERT INTO dq ("MixedCase") VALUES (\'yes\')');
      const [res] = d.execute('SELECT "MixedCase" FROM dq', { rowMode: "array" });
      assert.equal(res.rows[0][0], "yes");
    });

    it("<> operator works as not-equal", () => {
      const d = new ConnectSQL();
      d.execute("CREATE TABLE neq (id INT)");
      d.execute("INSERT INTO neq (id) VALUES (1), (2), (3)");
      const [res] = d.execute("SELECT id FROM neq WHERE id <> 2", { rowMode: "array" });
      assert.equal(res.rows.length, 2);
    });

    it("doubled single-quote string escaping", () => {
      const d = new ConnectSQL();
      d.execute("CREATE TABLE esc (val TEXT)");
      d.execute("INSERT INTO esc (val) VALUES ('it''s')");
      const [res] = d.execute("SELECT val FROM esc", { rowMode: "array" });
      assert.equal(res.rows[0][0], "it's");
    });

    it("CAST(expr AS type) syntax", () => {
      const d = new ConnectSQL();
      const [res] = d.execute("SELECT CAST('42' AS INT) AS val", { rowMode: "array" });
      assert.equal(res.rows[0][0], 42);
    });
  });

  describe("INSERT INTO ... SELECT", () => {
    it("inserts rows from a query", () => {
      const d = new ConnectSQL();
      d.execute("CREATE TABLE src (id INT, name TEXT)");
      d.execute("INSERT INTO src (id, name) VALUES (1, 'a'), (2, 'b'), (3, 'c')");
      d.execute("CREATE TABLE dst (id INT, name TEXT)");
      d.execute("INSERT INTO dst (id, name) SELECT id, name FROM src WHERE id > 1");
      const [res] = d.execute("SELECT * FROM dst ORDER BY id", { rowMode: "array" });
      assert.equal(res.rows.length, 2);
      assert.equal(res.rows[0][0], 2);
      assert.equal(res.rows[1][0], 3);
    });
  });

  describe("UPDATE ... FROM", () => {
    it("bulk updates with FROM clause", () => {
      const d = new ConnectSQL();
      d.execute(`
        CREATE TABLE targets (id INT, val TEXT, PRIMARY KEY (id));
        INSERT INTO targets (id, val) VALUES (1, 'old1'), (2, 'old2'), (3, 'old3')
      `);
      d.execute(`
        UPDATE targets
          SET val = d.val
          FROM (VALUES (1, 'new1'), (3, 'new3')) AS d (id, val)
          WHERE targets.id = d.id
      `);
      const [res] = d.execute("SELECT id, val FROM targets ORDER BY id", { rowMode: "array" });
      assert.equal(res.rows[0][1], "new1");
      assert.equal(res.rows[1][1], "old2");
      assert.equal(res.rows[2][1], "new3");
    });
  });

  describe("foreign keys", () => {
    it("ON DELETE CASCADE removes child rows", () => {
      const d = new ConnectSQL();
      d.execute(`
        CREATE TABLE parents (id INT, PRIMARY KEY (id));
        INSERT INTO parents (id) VALUES (1), (2);
        CREATE TABLE children (id INT, pid INT REFERENCES parents (id) ON DELETE CASCADE);
        INSERT INTO children (id, pid) VALUES (10, 1), (20, 1), (30, 2)
      `);
      d.execute("DELETE FROM parents WHERE id = 1");
      const [res] = d.execute("SELECT * FROM children", { rowMode: "array" });
      assert.equal(res.rows.length, 1);
      assert.equal(res.rows[0][1], 2);
    });

    it("FK violation throws error", () => {
      const d = new ConnectSQL();
      d.execute("CREATE TABLE pk_t (id INT, PRIMARY KEY (id))");
      d.execute("CREATE TABLE fk_t (ref INT REFERENCES pk_t (id))");
      assert.throws(() => d.execute("INSERT INTO fk_t (ref) VALUES (999)"));
    });
  });

  describe("prepared statements", () => {
    it("PREPARE and EXECUTE with parameters", () => {
      const d = new ConnectSQL();
      d.execute("CREATE TABLE prep (id INT, name TEXT)");
      d.execute("INSERT INTO prep (id, name) VALUES (1, 'Alice'), (2, 'Bob')");
      d.execute("PREPARE q AS SELECT name FROM prep WHERE id = $1");
      const [res] = d.execute("EXECUTE q(2)", { rowMode: "array" });
      assert.equal(res.rows[0][0], "Bob");
    });

    it("DEALLOCATE removes prepared statement", () => {
      const d = new ConnectSQL();
      d.execute("CREATE TABLE prep2 (id INT)");
      d.execute("PREPARE s AS SELECT * FROM prep2");
      d.execute("DEALLOCATE s");
      assert.throws(() => d.execute("EXECUTE s"));
    });
  });

  describe("LATERAL join", () => {
    it("correlated subquery in FROM", () => {
      const d = new ConnectSQL();
      d.execute(`
        CREATE TABLE depts (id INT, name TEXT, PRIMARY KEY (id));
        INSERT INTO depts (id, name) VALUES (1, 'Eng'), (2, 'Sales');
        CREATE TABLE emps (id INT, dept_id INT, salary INT);
        INSERT INTO emps (id, dept_id, salary) VALUES
          (1, 1, 100), (2, 1, 200), (3, 2, 150)
      `);
      const [res] = d.execute(`
        SELECT depts.name, top.salary
        FROM depts,
        LATERAL (SELECT salary FROM emps WHERE emps.dept_id = depts.id ORDER BY salary DESC LIMIT 1) AS top
        ORDER BY depts.name
      `, { rowMode: "array" });
      assert.equal(res.rows.length, 2);
      assert.equal(res.rows[0][0], "Eng");
      assert.equal(res.rows[0][1], 200);
      assert.equal(res.rows[1][0], "Sales");
      assert.equal(res.rows[1][1], 150);
    });
  });

  describe("transactions", () => {
    it("COMMIT persists changes", () => {
      const d = new ConnectSQL();
      d.execute("CREATE TABLE tx (id INT)");
      d.execute("BEGIN");
      d.execute("INSERT INTO tx (id) VALUES (1)");
      d.execute("COMMIT");
      const [res] = d.execute("SELECT * FROM tx");
      assert.equal(res.rows.length, 1);
    });

    it("ROLLBACK discards changes", () => {
      const d = new ConnectSQL();
      d.execute("CREATE TABLE tx2 (id INT)");
      d.execute("INSERT INTO tx2 (id) VALUES (1)");
      d.execute("BEGIN");
      d.execute("INSERT INTO tx2 (id) VALUES (2)");
      d.execute("ROLLBACK");
      const [res] = d.execute("SELECT * FROM tx2");
      assert.equal(res.rows.length, 1);
    });
  });
});
