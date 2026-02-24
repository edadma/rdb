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
