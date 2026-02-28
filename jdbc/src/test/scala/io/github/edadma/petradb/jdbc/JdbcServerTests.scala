package io.github.edadma.petradb.jdbc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import io.github.edadma.petradb.MemoryDB
import io.github.edadma.petradb.server.PetraServer
import io.github.edadma.microserve.EventLoop

class JdbcServerTests extends AnyFreeSpec with Matchers:

  private def withServer(test: Int => Unit): Unit =
    val loop   = new EventLoop
    val db     = new MemoryDB
    val server = new PetraServer(loop, db, port = 0)
    server.start()
    val port   = server.actualPort
    val thread = new Thread(() => loop.run())
    thread.setDaemon(true)
    thread.start()
    Thread.sleep(100)
    try test(port)
    finally
      server.stop(() => loop.stop())
      thread.join(3000)

  private def serverConn(port: Int) = new PetraServerConnection("localhost", port, "", "")

  "connect and close" in withServer { port =>
    val conn = serverConn(port)
    conn.isClosed shouldBe false
    conn.close()
    conn.isClosed shouldBe true
  }

  "executeQuery SELECT 1" in withServer { port =>
    val conn = serverConn(port)
    try
      val rs = conn.createStatement().executeQuery("SELECT 1 AS n")
      rs.next() shouldBe true
      rs.getInt("n") shouldBe 1
      rs.next() shouldBe false
    finally conn.close()
  }

  "CREATE TABLE, INSERT, SELECT via Statement" in withServer { port =>
    val conn = serverConn(port)
    try
      val st = conn.createStatement()
      st.execute("CREATE TABLE t (id INT, name TEXT)")
      st.execute("INSERT INTO t VALUES (1, 'Alice')")
      st.execute("INSERT INTO t VALUES (2, 'Bob')")
      val rs = st.executeQuery("SELECT * FROM t ORDER BY id")
      rs.next() shouldBe true
      rs.getInt("id")      shouldBe 1
      rs.getString("name") shouldBe "Alice"
      rs.next() shouldBe true
      rs.getInt("id")      shouldBe 2
      rs.getString("name") shouldBe "Bob"
      rs.next() shouldBe false
    finally conn.close()
  }

  "ResultSet column metadata" in withServer { port =>
    val conn = serverConn(port)
    try
      val st = conn.createStatement()
      st.execute("CREATE TABLE t (id INT, name TEXT)")
      val rs   = st.executeQuery("SELECT * FROM t")
      val meta = rs.getMetaData
      meta.getColumnCount  shouldBe 2
      meta.getColumnName(1) shouldBe "id"
      meta.getColumnName(2) shouldBe "name"
    finally conn.close()
  }

  "transaction commit: INSERT visible after commit" in withServer { port =>
    val conn = serverConn(port)
    try
      val st = conn.createStatement()
      st.execute("CREATE TABLE t (id INT)")
      conn.setAutoCommit(false)
      st.execute("INSERT INTO t VALUES (1)")
      conn.commit()
      conn.setAutoCommit(true)
      val rs = st.executeQuery("SELECT * FROM t")
      rs.next() shouldBe true
      rs.getInt("id") shouldBe 1
    finally conn.close()
  }

  "transaction rollback: INSERT not visible after rollback" in withServer { port =>
    val conn = serverConn(port)
    try
      val st = conn.createStatement()
      st.execute("CREATE TABLE t (id INT)")
      conn.setAutoCommit(false)
      st.execute("INSERT INTO t VALUES (99)")
      conn.rollback()
      conn.setAutoCommit(true)
      val rs = st.executeQuery("SELECT COUNT(*) AS c FROM t")
      rs.next() shouldBe true
      rs.getInt("c") shouldBe 0
    finally conn.close()
  }

  "prepareStatement with ? parameters" in withServer { port =>
    val conn = serverConn(port)
    try
      val st = conn.createStatement()
      st.execute("CREATE TABLE t (id INT, name TEXT)")
      val ps = conn.prepareStatement("INSERT INTO t VALUES (?, ?)")
      ps.setInt(1, 42)
      ps.setString(2, "prepared")
      ps.executeUpdate()
      val rs = st.executeQuery("SELECT * FROM t")
      rs.next() shouldBe true
      rs.getInt("id")      shouldBe 42
      rs.getString("name") shouldBe "prepared"
    finally conn.close()
  }

  "execute returns true for SELECT, false for DDL/DML" in withServer { port =>
    val conn = serverConn(port)
    try
      val st = conn.createStatement()
      st.execute("CREATE TABLE t (id INT)") shouldBe false
      st.execute("INSERT INTO t VALUES (1)") shouldBe false
      st.execute("SELECT * FROM t") shouldBe true
      val rs = st.getResultSet
      rs.next() shouldBe true
      rs.getInt(1) shouldBe 1
    finally conn.close()
  }

  "getUpdateCount after UPDATE" in withServer { port =>
    val conn = serverConn(port)
    try
      val st = conn.createStatement()
      st.execute("CREATE TABLE t (id INT, v INT)")
      st.execute("INSERT INTO t VALUES (1, 10), (2, 20), (3, 30)")
      st.execute("UPDATE t SET v = 0 WHERE id < 3")
      st.getUpdateCount shouldBe 2
    finally conn.close()
  }

  "DatabaseMetaData basics" in withServer { port =>
    val conn = serverConn(port)
    try
      val meta = conn.getMetaData
      meta.getDatabaseProductName() shouldBe "PetraDB"
      meta.getDriverName()          shouldBe "PetraDB JDBC Driver"
      meta.supportsTransactions()   shouldBe true
    finally conn.close()
  }

  "PetraDriver.connect parses server URL" in withServer { port =>
    val driver = new PetraDriver()
    driver.acceptsURL(s"jdbc:petradb://localhost:$port") shouldBe true
    driver.acceptsURL("jdbc:petradb:file::memory:")      shouldBe true
    driver.acceptsURL("jdbc:other://host")               shouldBe false
    val conn = driver.connect(s"jdbc:petradb://localhost:$port", new java.util.Properties())
    try
      val rs = conn.createStatement().executeQuery("SELECT 1 AS x")
      rs.next() shouldBe true
      rs.getInt("x") shouldBe 1
    finally conn.close()
  }

  "getTables returns all tables" in withServer { port =>
    val conn = serverConn(port)
    try
      val st = conn.createStatement()
      st.execute("CREATE TABLE authors (id SERIAL, name TEXT)")
      st.execute("CREATE TABLE books (id SERIAL, title TEXT, author_id INT)")
      val rs = conn.getMetaData.getTables(null, null, null, null)
      val names = collection.mutable.Set[String]()
      while rs.next() do names += rs.getString("TABLE_NAME")
      names shouldBe Set("authors", "books")
    finally conn.close()
  }

  "getTables on empty database returns no rows" in withServer { port =>
    val conn = serverConn(port)
    try
      val rs = conn.getMetaData.getTables(null, null, null, null)
      rs.next() shouldBe false
    finally conn.close()
  }

  "getTables filters by table name" in withServer { port =>
    val conn = serverConn(port)
    try
      val st = conn.createStatement()
      st.execute("CREATE TABLE authors (id SERIAL, name TEXT)")
      st.execute("CREATE TABLE books (id SERIAL, title TEXT)")
      val rs = conn.getMetaData.getTables(null, null, "authors", null)
      rs.next() shouldBe true
      rs.getString("TABLE_NAME") shouldBe "authors"
      rs.next() shouldBe false
    finally conn.close()
  }

  "getColumns returns all columns for a table" in withServer { port =>
    val conn = serverConn(port)
    try
      conn.createStatement().execute(
        "CREATE TABLE books (id SERIAL, title TEXT, price NUMERIC(8,2))"
      )
      val rs = conn.getMetaData.getColumns(null, null, "books", null)
      val cols = collection.mutable.ListBuffer[String]()
      while rs.next() do cols += rs.getString("COLUMN_NAME")
      cols.toSeq shouldBe Seq("id", "title", "price")
    finally conn.close()
  }

  "getColumns NOT NULL is populated" in withServer { port =>
    val conn = serverConn(port)
    try
      conn.createStatement().execute(
        "CREATE TABLE t (id SERIAL PRIMARY KEY, name TEXT NOT NULL, bio TEXT)"
      )
      val rs = conn.getMetaData.getColumns(null, null, "t", null)
      val cols = collection.mutable.Map[String, String]()
      while rs.next() do cols(rs.getString("COLUMN_NAME")) = rs.getString("IS_NULLABLE")
      cols("id")   shouldBe "NO"
      cols("name") shouldBe "NO"
      cols("bio")  shouldBe "YES"
    finally conn.close()
  }

  "getColumns on unknown table returns no rows" in withServer { port =>
    val conn = serverConn(port)
    try
      val rs = conn.getMetaData.getColumns(null, null, "nonexistent", null)
      rs.next() shouldBe false
    finally conn.close()
  }

  "getPrimaryKeys returns pk columns" in withServer { port =>
    val conn = serverConn(port)
    try
      conn.createStatement().execute(
        "CREATE TABLE t (id SERIAL PRIMARY KEY, name TEXT)"
      )
      val rs = conn.getMetaData.getPrimaryKeys(null, null, "t")
      rs.next() shouldBe true
      rs.getString("COLUMN_NAME") shouldBe "id"
      rs.getInt("KEY_SEQ")        shouldBe 1
      rs.next() shouldBe false
    finally conn.close()
  }

  "getPrimaryKeys composite key" in withServer { port =>
    val conn = serverConn(port)
    try
      conn.createStatement().execute(
        "CREATE TABLE t (a INT, b INT, c TEXT, PRIMARY KEY (a, b))"
      )
      val rs = conn.getMetaData.getPrimaryKeys(null, null, "t")
      val pks = collection.mutable.ListBuffer[(String, Int)]()
      while rs.next() do pks += ((rs.getString("COLUMN_NAME"), rs.getInt("KEY_SEQ")))
      pks.toSeq shouldBe Seq(("a", 1), ("b", 2))
    finally conn.close()
  }

  "getPrimaryKeys no pk returns empty" in withServer { port =>
    val conn = serverConn(port)
    try
      conn.createStatement().execute("CREATE TABLE t (id INT, name TEXT)")
      val rs = conn.getMetaData.getPrimaryKeys(null, null, "t")
      rs.next() shouldBe false
    finally conn.close()
  }

  "getGeneratedKeys after Statement INSERT" in withServer { port =>
    val conn = serverConn(port)
    try
      val st = conn.createStatement()
      st.execute("CREATE TABLE t (id SERIAL PRIMARY KEY, name TEXT)")
      st.executeUpdate("INSERT INTO t (name) VALUES ('Alice')")
      val rs = st.getGeneratedKeys
      rs.next() shouldBe true
      rs.getInt("id") should be > 0
      rs.next() shouldBe false
    finally conn.close()
  }

  "getGeneratedKeys after PreparedStatement INSERT" in withServer { port =>
    val conn = serverConn(port)
    try
      val st = conn.createStatement()
      st.execute("CREATE TABLE t (id SERIAL PRIMARY KEY, name TEXT)")
      val ps = conn.prepareStatement("INSERT INTO t (name) VALUES (?)")
      ps.setString(1, "Bob")
      ps.executeUpdate()
      val rs = ps.getGeneratedKeys
      rs.next() shouldBe true
      rs.getInt("id") should be > 0
      rs.next() shouldBe false
    finally conn.close()
  }

  "getGeneratedKeys via execute()" in withServer { port =>
    val conn = serverConn(port)
    try
      val st = conn.createStatement()
      st.execute("CREATE TABLE t (id SERIAL PRIMARY KEY, name TEXT)")
      st.execute("INSERT INTO t (name) VALUES ('Carol')")
      val rs = st.getGeneratedKeys
      rs.next() shouldBe true
      rs.getInt("id") should be > 0
      rs.next() shouldBe false
    finally conn.close()
  }

  "Statement batch INSERT via server" in withServer { port =>
    val conn = serverConn(port)
    try
      val st = conn.createStatement()
      st.execute("CREATE TABLE t (id INT, name TEXT)")
      st.addBatch("INSERT INTO t VALUES (1, 'Alice')")
      st.addBatch("INSERT INTO t VALUES (2, 'Bob')")
      st.addBatch("INSERT INTO t VALUES (3, 'Carol')")
      val counts = st.executeBatch()
      counts shouldBe Array(1, 1, 1)
      val rs = st.executeQuery("SELECT * FROM t ORDER BY id")
      rs.next() shouldBe true; rs.getInt("id") shouldBe 1; rs.getString("name") shouldBe "Alice"
      rs.next() shouldBe true; rs.getInt("id") shouldBe 2; rs.getString("name") shouldBe "Bob"
      rs.next() shouldBe true; rs.getInt("id") shouldBe 3; rs.getString("name") shouldBe "Carol"
      rs.next() shouldBe false
    finally conn.close()
  }

  "PreparedStatement batch INSERT via server" in withServer { port =>
    val conn = serverConn(port)
    try
      val st = conn.createStatement()
      st.execute("CREATE TABLE t (id INT, name TEXT)")
      val ps = conn.prepareStatement("INSERT INTO t VALUES (?, ?)")
      ps.setInt(1, 1); ps.setString(2, "Alice"); ps.addBatch()
      ps.setInt(1, 2); ps.setString(2, "Bob");   ps.addBatch()
      ps.setInt(1, 3); ps.setString(2, "Carol"); ps.addBatch()
      val counts = ps.executeBatch()
      counts shouldBe Array(1, 1, 1)
      val rs = st.executeQuery("SELECT * FROM t ORDER BY id")
      rs.next() shouldBe true; rs.getInt("id") shouldBe 1; rs.getString("name") shouldBe "Alice"
      rs.next() shouldBe true; rs.getInt("id") shouldBe 2; rs.getString("name") shouldBe "Bob"
      rs.next() shouldBe true; rs.getInt("id") shouldBe 3; rs.getString("name") shouldBe "Carol"
      rs.next() shouldBe false
    finally conn.close()
  }

  "getImportedKeys via server" in withServer { port =>
    val conn = serverConn(port)
    try
      val st = conn.createStatement()
      st.execute("CREATE TABLE authors (id SERIAL PRIMARY KEY, name TEXT)")
      st.execute("CREATE TABLE books (id SERIAL PRIMARY KEY, author_id INT REFERENCES authors(id))")
      val rs = conn.getMetaData.getImportedKeys(null, null, "books")
      rs.next() shouldBe true
      rs.getString("PKTABLE_NAME")  shouldBe "authors"
      rs.getString("PKCOLUMN_NAME") shouldBe "id"
      rs.getString("FKTABLE_NAME")  shouldBe "books"
      rs.getString("FKCOLUMN_NAME") shouldBe "author_id"
      rs.getInt("KEY_SEQ")          shouldBe 1
      rs.next() shouldBe false
    finally conn.close()
  }

  "getExportedKeys via server" in withServer { port =>
    val conn = serverConn(port)
    try
      val st = conn.createStatement()
      st.execute("CREATE TABLE authors (id SERIAL PRIMARY KEY, name TEXT)")
      st.execute("CREATE TABLE books (id SERIAL PRIMARY KEY, author_id INT REFERENCES authors(id))")
      val rs = conn.getMetaData.getExportedKeys(null, null, "authors")
      rs.next() shouldBe true
      rs.getString("PKTABLE_NAME")  shouldBe "authors"
      rs.getString("PKCOLUMN_NAME") shouldBe "id"
      rs.getString("FKTABLE_NAME")  shouldBe "books"
      rs.getString("FKCOLUMN_NAME") shouldBe "author_id"
      rs.next() shouldBe false
    finally conn.close()
  }

  "getIndexInfo via server" in withServer { port =>
    val conn = serverConn(port)
    try
      val st = conn.createStatement()
      st.execute("CREATE TABLE t (id INT, name TEXT)")
      st.execute("CREATE INDEX idx_name ON t (name)")
      val rs = conn.getMetaData.getIndexInfo(null, null, "t", false, false)
      rs.next() shouldBe true
      rs.getString("INDEX_NAME")    shouldBe "idx_name"
      rs.getString("COLUMN_NAME")   shouldBe "name"
      rs.getBoolean("NON_UNIQUE")   shouldBe true
      rs.getInt("ORDINAL_POSITION") shouldBe 1
      rs.next() shouldBe false
    finally conn.close()
  }
