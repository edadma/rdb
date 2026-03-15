package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class UserFunctionTests extends AnyFreeSpec with Matchers with Testing {

  // ══════════════════════════════════════════════════════════════════
  // REGISTER AND USE
  // ══════════════════════════════════════════════════════════════════

  "register and use" - {
    "registered function callable in SELECT" in {
      val db = new MemoryDB()
      db.registerScalarFunction("double_val", { case Seq(v) => NumberValue(v.intValue * 2) }, NumberType)
      given Session = db.connect()

      val table = executeSQL("SELECT double_val(21) AS val;").collect { case QueryResult(t) => t }.last
      table.data(0).data(0) shouldBe NumberValue(42)
    }

    "registered function callable in WHERE" in {
      val db = new MemoryDB()
      db.registerScalarFunction("is_positive", { case Seq(v) => BooleanValue(v.intValue > 0) }, BooleanType)
      given Session = db.connect()

      executeSQL("CREATE TABLE t (val INT); INSERT INTO t VALUES (-1); INSERT INTO t VALUES (5); INSERT INTO t VALUES (-3);")
      val table = executeSQL("SELECT val FROM t WHERE is_positive(val);").collect { case QueryResult(t) => t }.last
      table.data.length shouldBe 1
      table.data(0).data(0) shouldBe NumberValue(5)
    }

    "registered function with multiple arguments" in {
      val db = new MemoryDB()
      db.registerScalarFunction("add3", { case Seq(a, b, c) => NumberValue(a.intValue + b.intValue + c.intValue) }, NumberType)
      given Session = db.connect()

      val table = executeSQL("SELECT add3(10, 20, 30) AS val;").collect { case QueryResult(t) => t }.last
      table.data(0).data(0) shouldBe NumberValue(60)
    }

    "registered function returning text" in {
      val db = new MemoryDB()
      db.registerScalarFunction("greet", { case Seq(v) => TextValue(s"Hello, ${v.string}!") }, TextType)
      given Session = db.connect()

      val table = executeSQL("SELECT greet('World') AS val;").collect { case QueryResult(t) => t }.last
      table.data(0).data(0) shouldBe TextValue("Hello, World!")
    }

    "registered function handles NULL" in {
      val db = new MemoryDB()
      db.registerScalarFunction("safe_double", {
        case Seq(v) if v.isNull => NullValue()
        case Seq(v)             => NumberValue(v.intValue * 2)
      }, NumberType)
      given Session = db.connect()

      val table = executeSQL("SELECT safe_double(NULL) AS val;").collect { case QueryResult(t) => t }.last
      table.data(0).data(0).isNull shouldBe true
    }
  }

  // ══════════════════════════════════════════════════════════════════
  // USE IN TRIGGERS
  // ══════════════════════════════════════════════════════════════════

  "use in triggers" - {
    "registered function callable from trigger function" in {
      val db = new MemoryDB()
      val notifications = scala.collection.mutable.ArrayBuffer[String]()
      db.registerScalarFunction("notify_insert", { case Seq(v) =>
        notifications += s"inserted: ${v.string}"
        NumberValue(0)
      }, NumberType)
      given Session = db.connect()

      executeSQL(
        """
          |CREATE TABLE users (id SERIAL PRIMARY KEY, name TEXT NOT NULL);
          |CREATE TABLE audit (msg TEXT);
          |CREATE FUNCTION on_user_insert() RETURNS INT AS $$
          |DECLARE
          |  dummy INT;
          |BEGIN
          |  dummy := notify_insert('a user');
          |  INSERT INTO audit VALUES ('triggered');
          |  RETURN 0;
          |END $$ LANGUAGE plpgsql;
          |CREATE TRIGGER trg AFTER INSERT ON users FOR EACH ROW EXECUTE FUNCTION on_user_insert();
          |INSERT INTO users (name) VALUES ('Alice');
          |""".stripMargin
      )

      notifications.length shouldBe 1
      notifications(0) shouldBe "inserted: a user"
      val table = executeSQL("SELECT msg FROM audit;").collect { case QueryResult(t) => t }.last
      table.data(0).data(0) shouldBe TextValue("triggered")
    }
  }

  // ══════════════════════════════════════════════════════════════════
  // USE IN STORED FUNCTIONS
  // ══════════════════════════════════════════════════════════════════

  "use in stored functions" - {
    "registered function callable from PL/pgSQL function" in {
      val db = new MemoryDB()
      db.registerScalarFunction("native_multiply", { case Seq(a, b) => NumberValue(a.intValue * b.intValue) }, NumberType)
      given Session = db.connect()

      executeSQL(
        """
          |CREATE FUNCTION triple(x INT) RETURNS INT AS $$
          |BEGIN
          |  RETURN native_multiply(x, 3);
          |END $$ LANGUAGE plpgsql;
          |""".stripMargin
      )
      val table = executeSQL("SELECT triple(7) AS val;").collect { case QueryResult(t) => t }.last
      table.data(0).data(0) shouldBe NumberValue(21)
    }
  }

  // ══════════════════════════════════════════════════════════════════
  // USE IN DO BLOCKS
  // ══════════════════════════════════════════════════════════════════

  "use in DO blocks" - {
    "registered function callable from DO block" in {
      val db = new MemoryDB()
      db.registerScalarFunction("compute", { case Seq(v) => NumberValue(v.intValue * 100) }, NumberType)
      given Session = db.connect()

      executeSQL("CREATE TABLE t (val INT);")
      executeSQL(
        """
          |DO $$
          |DECLARE
          |  result INT;
          |BEGIN
          |  result := compute(5);
          |  INSERT INTO t VALUES (result);
          |END $$;
          |""".stripMargin
      )
      val table = executeSQL("SELECT val FROM t;").collect { case QueryResult(t) => t }.last
      table.data(0).data(0) shouldBe NumberValue(500)
    }
  }

  // ══════════════════════════════════════════════════════════════════
  // OVERRIDES BUILT-IN
  // ══════════════════════════════════════════════════════════════════

  "override" - {
    "user function does not override built-in" in {
      val db = new MemoryDB()
      // abs is built-in — user function with same name shouldn't replace it
      // (built-in is checked first in the lookup chain)
      given Session = db.connect()
      val table = executeSQL("SELECT abs(-42) AS val;").collect { case QueryResult(t) => t }.last
      table.data(0).data(0).intValue shouldBe 42
    }
  }

  // ══════════════════════════════════════════════════════════════════
  // NEGATIVE
  // ══════════════════════════════════════════════════════════════════

  "errors" - {
    "unregistered function fails" in {
      val db = new MemoryDB()
      given Session = db.connect()
      an[Exception] should be thrownBy {
        executeSQL("SELECT nonexistent_func(1);")
      }
    }
  }
}
