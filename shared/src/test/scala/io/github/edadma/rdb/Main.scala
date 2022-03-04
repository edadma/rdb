package io.github.edadma.rdb

import scala.collection.immutable.ArraySeq
import pprint.*

object Main extends App:
  implicit val db: DB = MemoryDB("test")

  executeSQL(
    """
      |CREATE TABLE "agent" (
      |  "agent_code" TEXT PRIMARY KEY,
      |  "agent_name" TEXT,
      |  "working_area" TEXT,
      |  "commission" NUMERIC(15, 2),
      |  "phone_no" TEXT
      |);
      |CREATE TABLE "customer" (
      |  "cust_code" TEXT PRIMARY KEY,
      |  "name" TEXT
      |);
      |CREATE TABLE "order" (
      |  "ord_num" INTEGER PRIMARY KEY,
      |  "ord_amount" NUMERIC(15, 2),
      |  "advance_amount" NUMERIC(15, 2),
      |  "ord_date" TEXT,
      |  "cust_code" TEXT,
      |  "agent_code" TEXT
      |);
      |ALTER TABLE "order" ADD FOREIGN KEY ("cust_code") REFERENCES "customer";
      |ALTER TABLE "order" ADD FOREIGN KEY ("agent_code") REFERENCES "agent";
      |INSERT INTO "agent" ("agent_code", "agent_name", "working_area", "commission", "phone_no") VALUES
      |  ('A007', 'Ramasundar', 'Bangalore', 0.15, '077-25814763'),
      |  ('A003', 'Alex', 'London', 0.13, '075-12458969'),
      |  ('A008', 'Alford', 'New York', 0.12, '044-25874365'),
      |  ('A011', 'Ravi Kumar', 'Bangalore', 0.15, '077-45625874'),
      |  ('A010', 'Santakumar', 'Chennai', 0.14, '007-22388644'),
      |  ('A012', 'Lucida', 'San Jose', 0.12, '044-52981425'),
      |  ('A005', 'Anderson', 'Brisban', 0.13, '045-21447739'),
      |  ('A001', 'Subbarao', 'Bangalore', 0.14, '077-12346674'),
      |  ('A002', 'Mukesh', 'Mumbai', 0.11, '029-12358964'),
      |  ('A006', 'McDen', 'London', 0.15, '078-22255588'),
      |  ('A004', 'Ivan', 'Torento', 0.15, '008-22544166'),
      |  ('A009', 'Benjamin', 'Hampshair', 0.11, '008-22536178');
      |INSERT INTO "customer" ("cust_code", "name") VALUES
      |  ('C00002', 'C00002 asdf'),
      |  ('C00003', 'C00003 asdf'),
      |  ('C00023', 'C00023 asdf'),
      |  ('C00007', 'C00007 asdf'),
      |  ('C00008', 'C00008 asdf'),
      |  ('C00025', 'C00025 asdf'),
      |  ('C00004', 'C00004 asdf'),
      |  ('C00021', 'C00021 asdf'),
      |  ('C00011', 'C00011 asdf'),
      |  ('C00001', 'C00001 asdf'),
      |  ('C00020', 'C00020 asdf'),
      |  ('C00006', 'C00006 asdf'),
      |  ('C00005', 'C00005 asdf'),
      |  ('C00018', 'C00018 asdf'),
      |  ('C00014', 'C00014 asdf'),
      |  ('C00022', 'C00022 asdf'),
      |  ('C00009', 'C00009 asdf'),
      |  ('C00010', 'C00010 asdf'),
      |  ('C00017', 'C00017 asdf'),
      |  ('C00024', 'C00024 asdf'),
      |  ('C00015', 'C00015 asdf'),
      |  ('C00012', 'C00012 asdf'),
      |  ('C00019', 'C00019 asdf'),
      |  ('C00016', 'C00016 asdf');
      |INSERT INTO "order" ("ord_num", "ord_amount", "advance_amount", "ord_date", "cust_code", "agent_code") VALUES
      |  (200114, 3500, 2000, '15-AUG-08', 'C00002', 'A008'),
      |  (200122, 2500, 400, '16-SEP-08', 'C00003', 'A004'),
      |  (200118, 500, 100, '20-JUL-08', 'C00023', 'A006'),
      |  (200119, 4000, 700, '16-SEP-08', 'C00007', 'A010'),
      |  (200121, 1500, 600, '23-SEP-08', 'C00008', 'A004'),
      |  (200130, 2500, 400, '30-JUL-08', 'C00025', 'A011'),
      |  (200134, 4200, 1800, '25-SEP-08', 'C00004', 'A005'),
      |  (200108, 4000, 600, '15-FEB-08', 'C00008', 'A004'),
      |  (200103, 1500, 700, '15-MAY-08', 'C00021', 'A005'),
      |  (200105, 2500, 500, '18-JUL-08', 'C00025', 'A011'),
      |  (200109, 3500, 800, '30-JUL-08', 'C00011', 'A010'),
      |  (200101, 3000, 1000, '15-JUL-08', 'C00001', 'A008'),
      |  (200111, 1000, 300, '10-JUL-08', 'C00020', 'A008'),
      |  (200104, 1500, 500, '13-MAR-08', 'C00006', 'A004'),
      |  (200106, 2500, 700, '20-APR-08', 'C00005', 'A002'),
      |  (200125, 2000, 600, '10-OCT-08', 'C00018', 'A005'),
      |  (200117, 800, 200, '20-OCT-08', 'C00014', 'A001'),
      |  (200123, 500, 100, '16-SEP-08', 'C00022', 'A002'),
      |  (200120, 500, 100, '20-JUL-08', 'C00009', 'A002'),
      |  (200116, 500, 100, '13-JUL-08', 'C00010', 'A009'),
      |  (200124, 500, 100, '20-JUN-08', 'C00017', 'A007'),
      |  (200126, 500, 100, '24-JUN-08', 'C00022', 'A002'),
      |  (200129, 2500, 500, '20-JUL-08', 'C00024', 'A006'),
      |  (200127, 2500, 400, '20-JUL-08', 'C00015', 'A003'),
      |  (200128, 3500, 1500, '20-JUL-08', 'C00009', 'A002'),
      |  (200135, 2000, 800, '16-SEP-08', 'C00007', 'A010'),
      |  (200131, 900, 150, '26-AUG-08', 'C00012', 'A012'),
      |  (200133, 1200, 400, '29-JUN-08', 'C00009', 'A002'),
      |  (200100, 1000, 600, '08-JAN-08', 'C00015', 'A003'),
      |  (200110, 3000, 500, '15-APR-08', 'C00019', 'A010'),
      |  (200107, 4500, 900, '30-AUG-08', 'C00007', 'A010'),
      |  (200112, 2000, 400, '30-MAY-08', 'C00016', 'A007'),
      |  (200113, 4000, 600, '10-JUN-08', 'C00022', 'A002'),
      |  (200102, 2000, 300, '25-MAY-08', 'C00012', 'A012');
      |""".trim.stripMargin,
  )
  pprintln(
    executeSQL(
      """
      |SELECT 
      |    "agent"."agent_code",
      |    "agent"."agent_name",
      |    "agent"."working_area",
      |    "agent"."commission",
      |    (TABLE(SELECT 
      |        sum("agent$orders"."ord_amount")
      |      FROM "order" AS "agent$orders"
      |      WHERE "agent$orders"."agent_code" = "agent"."agent_code"
      |      ))
      |  FROM "agent"
      |  WHERE "agent"."working_area" = 'Bangalore'
      |  ORDER BY "agent"."agent_code" ASC NULLS FIRST
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
