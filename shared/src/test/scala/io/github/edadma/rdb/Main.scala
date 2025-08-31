package io.github.edadma.rdb

object Main extends App:
  implicit val db: DB = new MemoryDB

  executeSQL(
    """
    CREATE TABLE test (
      id SERIAL,
      name TEXT,
      value INT,
      PRIMARY KEY (id)
    );

    INSERT INTO test (name, value) VALUES
      ('A', 10),
      ('B', 20),
      ('C', 30);
  """,
  )

  val QueryResult(basicSelect) =
    executeQuery("SELECT CASE WHEN LOWER(t.name) LIKE 'a' THEN 'asdf' ELSE t.name END, value FROM test t")
  basicSelect.data.foreach(row => println(s"Row: ${row.data.map(_.string).mkString(", ")}"))

//
//  PPrinter.BlackWhite.pprintln(
//  executeSQL(
//    """
//      |-- this is a comment
//      |
//      |CREATE TABLE s (
//      | id UUID DEFAULT gen_random_uuid(),
//      | name TEXT UNIQUE,
//      | ts TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
//      | PRIMARY KEY (id)
//      |);
//      |
//      |CREATE TABLE t (
//      | id UUID DEFAULT gen_random_uuid(),
//      | a TEXT,
//      | b INT,
//      | PRIMARY KEY (id)
//      |);
//      |
//      |INSERT INTO s (name) VALUES
//      | ('asdf');
//      |
//      |INSERT INTO t (a, b) VALUES
//      | ((SELECT id FROM s WHERE name = 'asdf'), 3),
//      | ((SELECT id FROM s WHERE name = 'asdf'), 4),
//      | ('g2', 5),
//      | ('g2', 6);
//      |""".trim.stripMargin,
//  )
//  val QueryResult(res) = executeQuery("SELECT id, a, SUM(b) AS sum FROM t GROUP BY a ORDER BY a DESC")
//
//  print(tableString(res))

//INSERT INTO t (c2) VALUES ('{"asdf": [3, -4, -5.6]}');

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
