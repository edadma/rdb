package io.github.edadma.rdb

import scala.collection.immutable.ArraySeq
import pprint.*

object Main extends App:
  implicit val db: DB = MemoryDB("test")

  pprintln(
    executeSQL(
      """
        |CREATE TABLE "author" (
        |  "pk_author_id" BIGINT PRIMARY KEY,
        |  "name" TEXT
        |);
        |CREATE TABLE "book" (
        |  "pk_book_id" BIGINT PRIMARY KEY,
        |  "title" TEXT,
        |  "year" INTEGER,
        |  "author_id" BIGINT
        |);
        |ALTER TABLE "book" ADD FOREIGN KEY ("author_id") REFERENCES "author";
        |INSERT INTO "author" ("pk_author_id", "name") VALUES
        |  (1, 'Robert Louis Stevenson'),
        |  (2, 'Lewis Carroll'),
        |  (3, 'Charles Dickens'),
        |  (4, 'Mark Twain');
        |INSERT INTO "book" ("pk_book_id", "title", "year", "author_id") VALUES
        |  (1, 'Treasure Island', 1883, 1),
        |  (2, E'Alice\'s Adventures in Wonderland', 1865, 2),
        |  (3, 'Oliver Twist', 1838, 3),
        |  (4, 'A Tale of Two Cities', 1859, 3),
        |  (5, 'The Adventures of Tom Sawyer', 1876, 4),
        |  (6, 'Adventures of Huckleberry Finn', 1884, 4);
        |""".trim.stripMargin,
    ),
  )
  pprintln(
    executeSQL(
      """
      |SELECT 
      |    "author"."name",
      |    (TABLE(SELECT 
      |        count(*), TYPE(count(*))
      |      FROM "book" AS "author$books"
      |      WHERE "author$books"."author_id" = "author"."pk_author_id"
      |      ))
      |  FROM "author"
      |""".stripMargin,
    ),
  )

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
//      Seq(NumberValue(5), TextValue("emp3"), NumberValue(3)),
//    ),
//  )
//
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
//  PPrinter.BlackWhite.pprintln(executeSQL(input))
