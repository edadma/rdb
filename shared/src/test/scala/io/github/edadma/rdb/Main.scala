package io.github.edadma.rdb

import scala.collection.immutable.ArraySeq
import pprint.*

object Main extends App:
  implicit val db: DB = MemoryDB("test")

  pprintln(
    executeSQL(
      """
        |CREATE TABLE "students" (
        |  "id" INTEGER PRIMARY KEY,
        |  "student_name" TEXT
        |);
        |CREATE TABLE "class" (
        |  "id" INTEGER PRIMARY KEY,
        |  "name" TEXT
        |);
        |CREATE TABLE "student_class" (
        |  "studentid" INTEGER,
        |  "classid" INTEGER,
        |  "year" INTEGER,
        |  "semester" TEXT,
        |  "grade" TEXT
        |);
        |ALTER TABLE "student_class" ADD FOREIGN KEY ("studentid") REFERENCES "students";
        |ALTER TABLE "student_class" ADD FOREIGN KEY ("classid") REFERENCES "class";
        |INSERT INTO "students" ("id", "student_name") VALUES
        |  (1, 'John'),
        |  (2, 'Debbie');
        |INSERT INTO "class" ("id", "name") VALUES
        |  (1, 'English'),
        |  (2, 'Maths'),
        |  (3, 'Spanish'),
        |  (4, 'Biology'),
        |  (5, 'Science'),
        |  (6, 'Programming'),
        |  (7, 'Law'),
        |  (8, 'Commerce'),
        |  (9, 'Physical Education');
        |INSERT INTO "student_class" ("studentid", "classid", "year", "semester", "grade") VALUES
        |  (1, 3, 2019, 'fall', 'B+'),
        |  (1, 5, 2018, 'winter', 'A'),
        |  (1, 9, 2019, 'summer', 'F'),
        |  (2, 1, 2018, 'fall', 'A+'),
        |  (2, 4, 2019, 'winter', 'B-'),
        |  (2, 5, 2018, 'summer', 'A-'),
        |  (2, 9, 2019, 'fall', 'B+');
        |""".trim.stripMargin,
    ),
  )
  pprintln(
    executeSQL(
      """
      |SELECT 
      |    "student_class$student"."student_name",
      |    count(*)
      |  FROM "student_class"
      |    LEFT JOIN "students" AS "student_class$student" ON "student_class"."studentid" = "student_class$student"."id"
      |  GROUP BY "student_class$student"."student_name"
      |  ORDER BY "student_class$student"."student_name" DESC
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
