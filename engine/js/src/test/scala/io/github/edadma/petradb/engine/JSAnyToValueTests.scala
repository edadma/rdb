package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import scala.scalajs.js
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class JSAnyToValueTests extends AnyFreeSpec with Matchers {

  "js.Array conversion" - {
    "js.Array[Int] converts to ArrayValue" in {
      val arr = js.Array(1, 2, 3)
      val v = anyToValue(arr)
      v shouldBe a[ArrayValue]
      v.asInstanceOf[ArrayValue].data.map(_.intValue) shouldBe IndexedSeq(1, 2, 3)
    }

    "js.Array[String] converts to ArrayValue" in {
      val arr = js.Array("a", "b", "c")
      val v = anyToValue(arr)
      v shouldBe a[ArrayValue]
      v.asInstanceOf[ArrayValue].data.map(_.string) shouldBe IndexedSeq("a", "b", "c")
    }

    "empty js.Array converts to empty ArrayValue" in {
      val arr = js.Array[Int]()
      val v = anyToValue(arr)
      v shouldBe a[ArrayValue]
      v.asInstanceOf[ArrayValue].data shouldBe empty
    }

    "js.Array[Boolean] converts to ArrayValue" in {
      val arr = js.Array(true, false, true)
      val v = anyToValue(arr)
      v shouldBe a[ArrayValue]
      v.asInstanceOf[ArrayValue].data.map(_.asInstanceOf[BooleanValue].b) shouldBe IndexedSeq(true, false, true)
    }

    "nested js.Array converts to nested ArrayValue" in {
      val arr = js.Array(js.Array(1, 2), js.Array(3, 4))
      val v = anyToValue(arr)
      v shouldBe a[ArrayValue]
      val outer = v.asInstanceOf[ArrayValue].data
      outer should have length 2
      outer(0).asInstanceOf[ArrayValue].data.map(_.intValue) shouldBe IndexedSeq(1, 2)
      outer(1).asInstanceOf[ArrayValue].data.map(_.intValue) shouldBe IndexedSeq(3, 4)
    }
  }

  "js.Array in parameterized queries" - {
    "ANY with js.Array parameter" in {
      given session: Session = new MemoryDB().connect()
      executeSQL(
        """CREATE TABLE t (id INT, name TEXT);
          |INSERT INTO t VALUES (1, 'alice'), (2, 'bob'), (3, 'charlie');
          |""".stripMargin
      )
      val results = executeSQL("SELECT name FROM t WHERE id = ANY($1) ORDER BY id", IndexedSeq(js.Array(1, 3)))
      val table = results.collect { case QueryResult(t) => t }.last
      table.data.map(_.data(0).string) shouldBe IndexedSeq("alice", "charlie")
    }

    "ANY with empty js.Array returns no rows" in {
      given session: Session = new MemoryDB().connect()
      executeSQL(
        """CREATE TABLE t (id INT);
          |INSERT INTO t VALUES (1), (2), (3);
          |""".stripMargin
      )
      val results = executeSQL("SELECT * FROM t WHERE id = ANY($1)", IndexedSeq(js.Array[Int]()))
      val table = results.collect { case QueryResult(t) => t }.last
      table.data shouldBe empty
    }
  }
}
