package io.github.edadma.rdb

object Main extends App:
  implicit val db: DB = new MemoryDB

  // Test new functions
  println("=== Testing RDB New Functions ===\n")

  executeSQL(
    """
    CREATE TABLE employees (
      id SERIAL,
      name TEXT,
      salary INT,
      department TEXT,
      hire_date TIMESTAMP DEFAULT NOW(),
      PRIMARY KEY (id)
    );

    INSERT INTO employees (name, salary, department) VALUES
      ('Alice', 75000, 'Engineering'),
      ('Bob', 65000, 'Sales'),
      ('Charlie', NULL, 'Engineering'),
      ('Diana', 80000, 'Marketing'),
      ('Eve', 70000, NULL);

    CREATE TABLE sales (
      id SERIAL,
      department TEXT,
      amount INT,
      PRIMARY KEY (id)
    );
    INSERT INTO sales (department, amount) VALUES 
      ('Sales', 1000), ('Sales', 2000), ('Marketing', 500), ('Engineering', 3000);
  """,
  )

  println("1. Testing aggregate functions (MIN, MAX, AVG):")
  val QueryResult(aggregates) = executeQuery(
    "SELECT department, MIN(salary), MAX(salary), AVG(salary), COUNT(*) FROM employees GROUP BY department"
  )
  aggregates.data.foreach(row => 
    println(s"  ${row.data.map(v => Option(v).filter(!_.isNull).map(_.string).getOrElse("NULL")).mkString(" | ")}")
  )

  println("\n1b. Testing GROUP BY without HAVING (baseline):")
  val QueryResult(groupByTest) = executeQuery(
    "SELECT department, COUNT(*), SUM(salary) FROM employees GROUP BY department"
  )
  groupByTest.data.foreach(row => 
    println(s"  ${row.data.map(v => Option(v).filter(!_.isNull).map(_.string).getOrElse("NULL")).mkString(" | ")}")
  )

  println("\n1c. Testing HAVING with aggregate (COUNT(*) > 1):")
  val QueryResult(havingWithAgg) = executeQuery(
    "SELECT department, COUNT(*) FROM employees GROUP BY department HAVING COUNT(*) > 1"
  )
  havingWithAgg.data.foreach(row => 
    println(s"  ${row.data.map(v => Option(v).filter(!_.isNull).map(_.string).getOrElse("NULL")).mkString(" | ")}")
  )

  println("\n=== DEBUGGING HAVING CLAUSE ===")
  
  println("\nStep 1: Basic sales data:")
  val QueryResult(salesData) = executeQuery("SELECT * FROM sales")
  salesData.data.foreach(row => 
    println(s"  ${row.data.map(_.string).mkString(" | ")}")
  )

  println("\nStep 2: GROUP BY without HAVING:")
  val QueryResult(salesGroupBy) = executeQuery("SELECT department, SUM(amount) FROM sales GROUP BY department")
  salesGroupBy.data.foreach(row => 
    println(s"  ${row.data.map(_.string).mkString(" | ")}")
  )

  println("\nStep 3: HAVING with aggregate (should show Sales=3000, Engineering=3000):")
  try {
    val QueryResult(salesHaving) = executeQuery("SELECT department, SUM(amount) as total FROM sales GROUP BY department HAVING total > 1500")
    salesHaving.data.foreach(row => 
      println(s"  ${row.data.map(_.string).mkString(" | ")}")
    )
    println(s"Step 3 success: Found ${salesHaving.data.length} rows")
  } catch {
    case e: Exception =>
      println(s"Step 3 ERROR: ${e.getMessage}")
      e.printStackTrace()
  }

  println("\nStep 4: Test if issue is with aggregate evaluation:")
  val QueryResult(salesHavingNumber) = executeQuery("SELECT department, SUM(amount) FROM sales GROUP BY department HAVING 3000 > 1500")
  salesHavingNumber.data.foreach(row => 
    println(s"  Constant condition result: ${row.data.map(_.string).mkString(" | ")}")
  )
  
  println(s"Step 4: Found ${salesHavingNumber.data.length} rows with constant condition")
  

  println("\n2. Testing null-handling functions (COALESCE, NULLIF):")
  val QueryResult(nullHandling) = executeQuery(
    "SELECT name, COALESCE(department, 'Unknown') as dept, NULLIF(salary, 70000) as adjusted_salary FROM employees"
  )
  nullHandling.data.foreach(row =>
    println(s"  ${row.data.map(v => Option(v).filter(!_.isNull).map(_.string).getOrElse("NULL")).mkString(" | ")}")
  )

  println("\n3. Testing string functions (REVERSE, SPLIT_PART):")
  val QueryResult(stringFuncs) = executeQuery(
    "SELECT name, REVERSE(name), SPLIT_PART('a,b,c,d', ',', 3) as third_part FROM employees WHERE name IS NOT NULL"
  )
  stringFuncs.data.foreach(row =>
    println(s"  ${row.data.map(_.string).mkString(" | ")}")
  )

  println("\n4. Testing date/time functions (NOW, CURRENT_DATE, DATE_PART):")
  val QueryResult(dateFuncs) = executeQuery(
    "SELECT name, DATE_PART('year', hire_date), DATE_PART('month', hire_date), DATE_PART('day', hire_date) FROM employees LIMIT 2"
  )
  dateFuncs.data.foreach(row =>
    println(s"  ${row.data.map(_.string).mkString(" | ")}")
  )

  println("\n5. Testing type conversion functions (TO_NUMBER, TO_TEXT):")
  val QueryResult(conversionFuncs) = executeQuery(
    "SELECT TO_NUMBER('123.45'), TO_TEXT(salary), TO_TEXT(id) FROM employees LIMIT 3"
  )
  conversionFuncs.data.foreach(row =>
    println(s"  ${row.data.map(_.string).mkString(" | ")}")
  )

  println("\n6. Testing IS NULL / IS NOT NULL:")
  val QueryResult(nullChecks) = executeQuery(
    "SELECT name, salary FROM employees WHERE salary IS NOT NULL ORDER BY salary DESC"
  )
  nullChecks.data.foreach(row =>
    println(s"  ${row.data.map(v => Option(v).filter(!_.isNull).map(_.string).getOrElse("NULL")).mkString(" | ")}")
  )

  println("\n=== Testing DDL Commands ===")
  
  println("\nStep 1: Testing basic DROP TABLE:")
  try {
    executeSQL("CREATE TABLE temp_test (id SERIAL, PRIMARY KEY (id));")
    val ddlResult1 = executeSQL("DROP TABLE temp_test;")
    println(s"  Success: $ddlResult1")
  } catch {
    case e: Exception =>
      println(s"  Error: ${e.getMessage}")
  }
  
  println("\nStep 1b: Testing DROP TABLE IF EXISTS:")
  try {
    val ddlResult1b = executeSQL("DROP TABLE IF EXISTS nonexistent_table;")
    println(s"  Success: $ddlResult1b")
  } catch {
    case e: Exception =>
      println(s"  Error: ${e.getMessage}")
  }
  
  println("\nStep 2: Testing ALTER TABLE RENAME:")
  try {
    val ddlResult2 = executeSQL("CREATE TABLE test_rename (id SERIAL, PRIMARY KEY (id)); ALTER TABLE test_rename RENAME TO test_renamed;")
    println(s"  Success: $ddlResult2")
  } catch {
    case e: Exception =>
      println(s"  Error: ${e.getMessage}")
  }

  println("\n=== All tests completed! ===")