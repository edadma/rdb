package io.github.edadma.petradb.jdbc

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.BeforeAndAfterEach

import java.nio.file.{Files, Paths}

class JdbcFileTests extends AnyFreeSpec with Matchers with BeforeAndAfterEach:

  private val tmpPetra = "/tmp/jdbc_test.petra"
  private val tmpPtxt  = "/tmp/jdbc_test.ptxt"

  override def afterEach(): Unit =
    Files.deleteIfExists(Paths.get(tmpPetra))
    Files.deleteIfExists(Paths.get(tmpPtxt))

  private def memConn()    = new PetraFileConnection(":memory:")
  private def petraConn()  = new PetraFileConnection(tmpPetra)
  private def ptxtConn()   = new PetraFileConnection(tmpPtxt)

  "in-memory: connect and close" in:
    val conn = memConn()
    conn.isClosed shouldBe false
    conn.close()
    conn.isClosed shouldBe true

  "in-memory: executeQuery SELECT 1" in:
    val conn = memConn()
    try
      val rs = conn.createStatement().executeQuery("SELECT 1 AS n")
      rs.next() shouldBe true
      rs.getInt("n") shouldBe 1
      rs.next() shouldBe false
    finally conn.close()

  "in-memory: CREATE TABLE, INSERT, SELECT" in:
    val conn = memConn()
    try
      val st = conn.createStatement()
      st.executeUpdate("CREATE TABLE t (id INT, name TEXT)")
      st.executeUpdate("INSERT INTO t VALUES (1, 'Alice')")
      st.executeUpdate("INSERT INTO t VALUES (2, 'Bob')")
      val rs = st.executeQuery("SELECT * FROM t ORDER BY id")
      rs.next() shouldBe true
      rs.getInt("id")    shouldBe 1
      rs.getString("name") shouldBe "Alice"
      rs.next() shouldBe true
      rs.getInt("id")    shouldBe 2
      rs.getString("name") shouldBe "Bob"
      rs.next() shouldBe false
    finally conn.close()

  "in-memory: ResultSet column metadata" in:
    val conn = memConn()
    try
      val st = conn.createStatement()
      st.executeUpdate("CREATE TABLE t (id INT, name TEXT)")
      val rs   = st.executeQuery("SELECT * FROM t")
      val meta = rs.getMetaData
      meta.getColumnCount shouldBe 2
      meta.getColumnName(1)  shouldBe "id"
      meta.getColumnName(2)  shouldBe "name"
    finally conn.close()

  "in-memory: NULL values set wasNull" in:
    val conn = memConn()
    try
      val st = conn.createStatement()
      st.executeUpdate("CREATE TABLE t (id INT, name TEXT)")
      st.executeUpdate("INSERT INTO t VALUES (1, NULL)")
      val rs = st.executeQuery("SELECT * FROM t")
      rs.next() shouldBe true
      rs.getInt("id")    shouldBe 1
      rs.wasNull         shouldBe false
      rs.getString("name") shouldBe null
      rs.wasNull         shouldBe true
    finally conn.close()

  "in-memory: transaction commit" in:
    val conn = memConn()
    try
      val st = conn.createStatement()
      st.executeUpdate("CREATE TABLE t (id INT)")
      conn.setAutoCommit(false)
      st.executeUpdate("INSERT INTO t VALUES (42)")
      conn.commit()
      conn.setAutoCommit(true)
      val rs = st.executeQuery("SELECT * FROM t")
      rs.next() shouldBe true
      rs.getInt(1) shouldBe 42
    finally conn.close()

  "in-memory: transaction rollback" in:
    val conn = memConn()
    try
      val st = conn.createStatement()
      st.executeUpdate("CREATE TABLE t (id INT)")
      conn.setAutoCommit(false)
      st.executeUpdate("INSERT INTO t VALUES (99)")
      conn.rollback()
      conn.setAutoCommit(true)
      val rs = st.executeQuery("SELECT COUNT(*) AS c FROM t")
      rs.next() shouldBe true
      rs.getInt("c") shouldBe 0
    finally conn.close()

  "in-memory: prepareStatement with ? parameters" in:
    val conn = memConn()
    try
      val st = conn.createStatement()
      st.executeUpdate("CREATE TABLE t (id INT, name TEXT)")
      val ps = conn.prepareStatement("INSERT INTO t VALUES (?, ?)")
      ps.setInt(1, 7)
      ps.setString(2, "Carol")
      ps.executeUpdate()
      val rs = st.executeQuery("SELECT * FROM t")
      rs.next() shouldBe true
      rs.getInt("id")      shouldBe 7
      rs.getString("name") shouldBe "Carol"
    finally conn.close()

  "in-memory: prepareStatement SELECT" in:
    val conn = memConn()
    try
      val st = conn.createStatement()
      st.executeUpdate("CREATE TABLE t (id INT, val DOUBLE)")
      st.executeUpdate("INSERT INTO t VALUES (1, 3.14)")
      val ps = conn.prepareStatement("SELECT * FROM t WHERE id = ?")
      ps.setInt(1, 1)
      val rs = ps.executeQuery()
      rs.next() shouldBe true
      rs.getDouble("val") shouldBe (3.14 +- 0.001)
    finally conn.close()

  "persistent file: data survives reconnect" in:
    val conn1 = petraConn()
    try
      val st = conn1.createStatement()
      st.executeUpdate("CREATE TABLE t (id INT, name TEXT)")
      st.executeUpdate("INSERT INTO t VALUES (1, 'persistent')")
    finally conn1.close()

    val conn2 = petraConn()
    try
      val rs = conn2.createStatement().executeQuery("SELECT * FROM t")
      rs.next() shouldBe true
      rs.getInt("id")      shouldBe 1
      rs.getString("name") shouldBe "persistent"
    finally conn2.close()

  "text file (.ptxt) backend" in:
    val conn = ptxtConn()
    try
      val st = conn.createStatement()
      st.executeUpdate("CREATE TABLE t (id INT, label TEXT)")
      st.executeUpdate("INSERT INTO t VALUES (10, 'text-backed')")
      val rs = st.executeQuery("SELECT * FROM t")
      rs.next() shouldBe true
      rs.getInt("id")      shouldBe 10
      rs.getString("label") shouldBe "text-backed"
    finally conn.close()

  "DatabaseMetaData basics" in:
    val conn = memConn()
    try
      val meta = conn.getMetaData
      meta.getDatabaseProductName() shouldBe "PetraDB"
      meta.getDriverName()          shouldBe "PetraDB JDBC Driver"
      meta.supportsTransactions()   shouldBe true
    finally conn.close()
