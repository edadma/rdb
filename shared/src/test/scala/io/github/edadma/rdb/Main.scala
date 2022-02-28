package io.github.edadma.rdb

import scala.collection.immutable.ArraySeq
import pprint.*

import scala.util.Try

object Main extends App:

  val json =
    """
      |[
      |  {
      |    "name": "Molecule Man",
      |    "age": 29,
      |    "secretIdentity": "Dan Jukes",
      |    "powers": [
      |      "Radiation resistance",
      |      "Turning tiny",
      |      "Radiation blast"
      |    ]
      |  },
      |  {
      |    "name": "Madame Uppercut",
      |    "age": null,
      |    "secretIdentity": "Jane Wilson",
      |    "powers": [
      |      "Million tonne punch",
      |      "Damage resistance",
      |      "Superhuman reflexes"
      |    ]
      |  }
      |]
      |""".stripMargin

  println(JSONParser.parseJSON(json).toText)

//  implicit val db: DB = MemoryDB("test")
//
//  val e =
//    db.create("e", Seq(ColumnSpec("e_id", NumberType), ColumnSpec("name", TextType), ColumnSpec("m_id", NumberType)))
//
//  e.bulkInsert(
//    Seq("e_id", "name", "m_id"),
//    Seq(
//      Seq(NumberValue(1), TextValue("emp1"), NumberValue(2)),
//      Seq(NumberValue(2), TextValue("mgr1"), NumberValue(3)),
//      Seq(NumberValue(3), TextValue("mgr2"), NULL),
//      Seq(NumberValue(4), TextValue("emp2"), NumberValue(3)),
//      Seq(NumberValue(5), TextValue("emp3"), NumberValue(3))
//    )
//  )

//  val input =
//    """
//      |SELECT name, TABLE(SELECT * FROM e WHERE mgr.e_id = m_id) FROM e mgr WHERE EXISTS (SELECT * FROM e WHERE mgr.e_id = m_id)
//      |""".trim.stripMargin
//    """
//      |SELECT * FROM e JOIN e m ON m.e_id = e.m_id
//      |""".trim.stripMargin
//  """
//    |INSERT INTO e (e_id, name, m_id) VALUES (-123, 'asdf', 456)
//    |""".trim.stripMargin
//  val ast = SQLParser.parseQuery(input)
//  val rewritten = rewrite(ast)(db)
//
//  pprintln(rewritten)
//  pprintln(eval(rewritten, Nil, AggregateMode.Return))

//    pprintln(
//      executeSQL(
//        """
//      |CREATE TABLE t (
//      | c1 INTEGER PRIMARY KEY,
//      | c2 INTEGER
//      |)
//      |""".trim.stripMargin
//      )
//    )
//  pprintln(executeSQL("INSERT INTO t (c1, c2) VALUES (345, 3)"))
//  pprintln(executeSQL("INSERT INTO t (c1, c2) VALUES (356, 4)"))
//  pprintln(executeSQL("SELECT * FROM t"))
//  pprintln(
//    executeSQL(
//      """
//        |UPDATE t
//        | SET c2 = c1 + 1
//        | WHERE c1 > 345
//        |""".trim.stripMargin
//    )
//  )
//  PPrinter.BlackWhite.pprintln(executeSQL("SELECT * FROM t"))
