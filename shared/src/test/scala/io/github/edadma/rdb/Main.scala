package io.github.edadma.rdb

object Main extends App:
  implicit val db: DB = new MemoryDB

  executeSQL(
    """
    CREATE TABLE test (
      id INT AUTO PRIMARY KEY,
      name TEXT,
      value INT
    );

    INSERT INTO test (name, value) VALUES
      ('a', 10),
      ('b', 20),
      ('a', 30);
  """,
  )

  println("=== Test 1: Basic SELECT (should show 3 rows) ===")
  val QueryResult(basicSelect) = executeQuery("SELECT * FROM test")
  println(s"Row count: ${basicSelect.data.length}")
  basicSelect.data.foreach(row => println(s"Row: ${row.data.map(_.string).mkString(", ")}"))

  println("\n=== Test 2: COUNT(*) - should return 1 row with count=3 ===")
  val QueryResult(countAll) = executeQuery("SELECT COUNT(*) FROM test")
  println(s"Result row count: ${countAll.data.length}")
  countAll.data.foreach(row => println(s"COUNT(*) = ${row.data.head.string}"))

  println("\n=== Test 3: COUNT with GROUP BY (should return 2 rows) ===")
  val QueryResult(countGrouped) = executeQuery("SELECT name, COUNT(*) FROM test GROUP BY name")
  println(s"Result row count: ${countGrouped.data.length}")
  countGrouped.data.foreach(row => println(s"${row.data(0).string}: COUNT(*) = ${row.data(1).string}"))

  println("\n=== Test 4: SUM(*) - should return 1 row with sum=60 ===")
  val QueryResult(sumAll) = executeQuery("SELECT SUM(value) FROM test")
  println(s"Result row count: ${sumAll.data.length}")
  sumAll.data.foreach(row => println(s"SUM(value) = ${row.data.head.string}"))

  println("\n=== Test 5: COUNT with table alias ===")
  val QueryResult(countAlias) = executeQuery("SELECT COUNT(*) FROM test t")
  println(s"Result row count: ${countAlias.data.length}")
  countAlias.data.foreach(row => println(s"COUNT(*) = ${row.data.head.string}"))

  println("\n=== Test 6: COUNT with WHERE clause ===")
  val QueryResult(countWhere) = executeQuery("SELECT COUNT(*) FROM test WHERE name = 'a'")
  println(s"Result row count: ${countWhere.data.length}")
  countWhere.data.foreach(row => println(s"COUNT(*) = ${row.data.head.string}"))

  println("\n=== Test 7: COUNT with column alias ===")
  val QueryResult(countColAlias) = executeQuery("SELECT COUNT(*) AS count FROM test")
  println(s"Result row count: ${countColAlias.data.length}")
  countColAlias.data.foreach(row => println(s"COUNT(*) = ${row.data.head.string}"))

  println("\n=== Test 8: COUNT with LIKE ===")
  val QueryResult(countLike) = executeQuery("SELECT COUNT(*) FROM test t WHERE t.name LIKE '%a%'")
  println(s"Result row count: ${countLike.data.length}")
  countLike.data.foreach(row => println(s"COUNT(*) = ${row.data.head.string}"))

//
//  PPrinter.BlackWhite.pprintln(
//  executeSQL(
//    """
//      |-- this is a comment
//      |
//      |CREATE TABLE s (
//      | id UUID AUTO PRIMARY KEY,
//      | name TEXT UNIQUE,
//      | ts TIMESTAMP DEFAULT CURRENT_TIMESTAMP
//      |);
//      |
//      |CREATE TABLE t (
//      | id UUID AUTO PRIMARY KEY,
//      | a TEXT,
//      | b INT
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
