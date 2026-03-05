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

  private def memConn()    = new PetraFileConnection(s"memory:${java.util.UUID.randomUUID()}", ":memory:")
  private def petraConn()  = new PetraFileConnection(new java.io.File(tmpPetra).getCanonicalPath, tmpPetra)
  private def ptxtConn()   = new PetraFileConnection(new java.io.File(tmpPtxt).getCanonicalPath, tmpPtxt)

  "PetraDriver.connect file URL" in:
    val conn = new PetraDriver().connect(s"jdbc:petradb:file:$tmpPetra", new java.util.Properties())
    try
      val stmt = conn.createStatement()
      stmt.executeUpdate("CREATE TABLE t (x INT)")
      stmt.executeUpdate("INSERT INTO t VALUES (7)")
      val rs = stmt.executeQuery("SELECT x FROM t")
      rs.next() shouldBe true
      rs.getInt("x") shouldBe 7
    finally conn.close()

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

  "getTables returns all tables" in:
    val conn = memConn()
    try
      val st = conn.createStatement()
      st.executeUpdate("CREATE TABLE authors (id SERIAL, name TEXT)")
      st.executeUpdate("CREATE TABLE books (id SERIAL, title TEXT, author_id INT)")
      val rs = conn.getMetaData.getTables(null, null, null, null)
      val names = collection.mutable.Set[String]()
      while rs.next() do names += rs.getString("TABLE_NAME")
      names shouldBe Set("authors", "books")
    finally conn.close()

  "getTables filters by table name" in:
    val conn = memConn()
    try
      val st = conn.createStatement()
      st.executeUpdate("CREATE TABLE authors (id SERIAL, name TEXT)")
      st.executeUpdate("CREATE TABLE books (id SERIAL, title TEXT)")
      val rs = conn.getMetaData.getTables(null, null, "authors", null)
      rs.next() shouldBe true
      rs.getString("TABLE_NAME") shouldBe "authors"
      rs.next() shouldBe false
    finally conn.close()

  "getTables TABLE_TYPE is TABLE" in:
    val conn = memConn()
    try
      conn.createStatement().executeUpdate("CREATE TABLE t (id INT)")
      val rs = conn.getMetaData.getTables(null, null, null, null)
      rs.next() shouldBe true
      rs.getString("TABLE_TYPE") shouldBe "TABLE"
    finally conn.close()

  "getTables on empty database returns no rows" in:
    val conn = memConn()
    try
      val rs = conn.getMetaData.getTables(null, null, null, null)
      rs.next() shouldBe false
    finally conn.close()

  "getTableTypes returns TABLE" in:
    val conn = memConn()
    try
      val rs = conn.getMetaData.getTableTypes()
      rs.next() shouldBe true
      rs.getString("TABLE_TYPE") shouldBe "TABLE"
      rs.next() shouldBe false
    finally conn.close()

  "getColumns returns all columns for a table" in:
    val conn = memConn()
    try
      conn.createStatement().executeUpdate(
        "CREATE TABLE books (id SERIAL, title TEXT, price NUMERIC(8,2))"
      )
      val rs = conn.getMetaData.getColumns(null, null, "books", null)
      val cols = collection.mutable.ListBuffer[(String, String)]()
      while rs.next() do
        cols += ((rs.getString("COLUMN_NAME"), rs.getString("TYPE_NAME")))
      cols.map(_._1) shouldBe Seq("id", "title", "price")
    finally conn.close()

  "getColumns ORDINAL_POSITION is 1-based" in:
    val conn = memConn()
    try
      conn.createStatement().executeUpdate("CREATE TABLE t (a INT, b TEXT, c BOOLEAN)")
      val rs = conn.getMetaData.getColumns(null, null, "t", null)
      var pos = 1
      while rs.next() do
        rs.getInt("ORDINAL_POSITION") shouldBe pos
        pos += 1
      pos shouldBe 4 // 3 columns checked
    finally conn.close()

  "getColumns filters by column name" in:
    val conn = memConn()
    try
      conn.createStatement().executeUpdate("CREATE TABLE t (id INT, name TEXT, age INT)")
      val rs = conn.getMetaData.getColumns(null, null, "t", "name")
      rs.next() shouldBe true
      rs.getString("COLUMN_NAME") shouldBe "name"
      rs.next() shouldBe false
    finally conn.close()

  "getColumns DATA_TYPE matches expected JDBC type" in:
    val conn = memConn()
    try
      conn.createStatement().executeUpdate(
        "CREATE TABLE t (i INT, t TEXT, b BOOLEAN, d DOUBLE PRECISION)"
      )
      val rs = conn.getMetaData.getColumns(null, null, "t", null)
      val types = collection.mutable.ListBuffer[Int]()
      while rs.next() do types += rs.getInt("DATA_TYPE")
      types(0) shouldBe java.sql.Types.INTEGER
      types(1) shouldBe java.sql.Types.VARCHAR
      types(2) shouldBe java.sql.Types.BOOLEAN
      types(3) shouldBe java.sql.Types.DOUBLE
    finally conn.close()

  "getColumns on unknown table returns no rows" in:
    val conn = memConn()
    try
      val rs = conn.getMetaData.getColumns(null, null, "nonexistent", null)
      rs.next() shouldBe false
    finally conn.close()

  "getColumns NOT NULL is populated" in:
    val conn = memConn()
    try
      conn.createStatement().executeUpdate(
        "CREATE TABLE t (id SERIAL PRIMARY KEY, name TEXT NOT NULL, bio TEXT)"
      )
      val rs = conn.getMetaData.getColumns(null, null, "t", null)
      val cols = collection.mutable.Map[String, (Int, String)]()
      while rs.next() do
        cols(rs.getString("COLUMN_NAME")) = (rs.getInt("NULLABLE"), rs.getString("IS_NULLABLE"))
      cols("id")._2   shouldBe "NO"
      cols("name")._2 shouldBe "NO"
      cols("bio")._2  shouldBe "YES"
    finally conn.close()

  "getColumns IS_AUTOINCREMENT is YES for serial" in:
    val conn = memConn()
    try
      conn.createStatement().executeUpdate(
        "CREATE TABLE t (id SERIAL PRIMARY KEY, name TEXT)"
      )
      val rs = conn.getMetaData.getColumns(null, null, "t", null)
      val auto = collection.mutable.Map[String, String]()
      while rs.next() do auto(rs.getString("COLUMN_NAME")) = rs.getString("IS_AUTOINCREMENT")
      auto("id")   shouldBe "YES"
      auto("name") shouldBe "NO"
    finally conn.close()

  "getColumns COLUMN_DEF shows default value" in:
    val conn = memConn()
    try
      conn.createStatement().executeUpdate(
        "CREATE TABLE t (id INT, status TEXT DEFAULT 'active', score INT DEFAULT 0)"
      )
      val rs = conn.getMetaData.getColumns(null, null, "t", null)
      val defs = collection.mutable.Map[String, String]()
      while rs.next() do defs(rs.getString("COLUMN_NAME")) = rs.getString("COLUMN_DEF")
      defs("status") shouldBe "'active'"
      defs("score")  shouldBe "0"
    finally conn.close()

  "getPrimaryKeys returns pk columns" in:
    val conn = memConn()
    try
      conn.createStatement().executeUpdate(
        "CREATE TABLE t (id SERIAL PRIMARY KEY, name TEXT)"
      )
      val rs = conn.getMetaData.getPrimaryKeys(null, null, "t")
      rs.next() shouldBe true
      rs.getString("COLUMN_NAME") shouldBe "id"
      rs.getInt("KEY_SEQ")        shouldBe 1
      rs.next() shouldBe false
    finally conn.close()

  "getPrimaryKeys composite key" in:
    val conn = memConn()
    try
      conn.createStatement().executeUpdate(
        "CREATE TABLE t (a INT, b INT, c TEXT, PRIMARY KEY (a, b))"
      )
      val rs = conn.getMetaData.getPrimaryKeys(null, null, "t")
      val pks = collection.mutable.ListBuffer[(String, Int)]()
      while rs.next() do pks += ((rs.getString("COLUMN_NAME"), rs.getInt("KEY_SEQ")))
      pks.toSeq shouldBe Seq(("a", 1), ("b", 2))
    finally conn.close()

  "getPrimaryKeys no pk returns empty" in:
    val conn = memConn()
    try
      conn.createStatement().executeUpdate("CREATE TABLE t (id INT, name TEXT)")
      val rs = conn.getMetaData.getPrimaryKeys(null, null, "t")
      rs.next() shouldBe false
    finally conn.close()

  "getSchemas returns one default schema" in:
    val conn = memConn()
    try
      val rs = conn.getMetaData.getSchemas()
      rs.next() shouldBe true
      rs.getString("TABLE_SCHEM") shouldBe ""
      rs.next() shouldBe false
    finally conn.close()

  "getTables TABLE_SCHEM matches getSchemas" in:
    val conn = memConn()
    try
      conn.createStatement().executeUpdate("CREATE TABLE t (id INT)")
      val schemaRs = conn.getMetaData.getSchemas()
      schemaRs.next()
      val schemaName = schemaRs.getString("TABLE_SCHEM")

      val tablesRs = conn.getMetaData.getTables(null, null, null, null)
      tablesRs.next() shouldBe true
      tablesRs.getString("TABLE_SCHEM") shouldBe schemaName
    finally conn.close()

  "shared DB: concurrent connections see each other's changes" in:
    val conn1 = petraConn()
    val conn2 = petraConn()
    try
      conn1.createStatement().executeUpdate("CREATE TABLE t (id INT, name TEXT)")
      conn1.createStatement().executeUpdate("INSERT INTO t VALUES (1, 'shared')")
      val rs = conn2.createStatement().executeQuery("SELECT * FROM t")
      rs.next() shouldBe true
      rs.getInt("id") shouldBe 1
      rs.getString("name") shouldBe "shared"
      rs.next() shouldBe false
    finally
      conn1.close()
      conn2.close()

  "in-memory: getGeneratedKeys after Statement INSERT" in:
    val conn = memConn()
    try
      val st = conn.createStatement()
      st.executeUpdate("CREATE TABLE t (id SERIAL PRIMARY KEY, name TEXT)")
      st.executeUpdate("INSERT INTO t (name) VALUES ('Alice')")
      val rs = st.getGeneratedKeys
      rs.next() shouldBe true
      rs.getInt("id") should be > 0
      rs.next() shouldBe false
    finally conn.close()

  "in-memory: getGeneratedKeys after PreparedStatement INSERT" in:
    val conn = memConn()
    try
      val st = conn.createStatement()
      st.executeUpdate("CREATE TABLE t (id SERIAL PRIMARY KEY, name TEXT)")
      val ps = conn.prepareStatement("INSERT INTO t (name) VALUES (?)")
      ps.setString(1, "Bob")
      ps.executeUpdate()
      val rs = ps.getGeneratedKeys
      rs.next() shouldBe true
      rs.getInt("id") should be > 0
      rs.next() shouldBe false
    finally conn.close()

  "in-memory: getGeneratedKeys via execute()" in:
    val conn = memConn()
    try
      val st = conn.createStatement()
      st.executeUpdate("CREATE TABLE t (id SERIAL PRIMARY KEY, name TEXT)")
      st.execute("INSERT INTO t (name) VALUES ('Carol')")
      val rs = st.getGeneratedKeys
      rs.next() shouldBe true
      rs.getInt("id") should be > 0
      rs.next() shouldBe false
    finally conn.close()

  "in-memory: getGeneratedKeys empty after non-INSERT" in:
    val conn = memConn()
    try
      val st = conn.createStatement()
      st.executeUpdate("CREATE TABLE t (id SERIAL PRIMARY KEY, name TEXT)")
      st.executeUpdate("INSERT INTO t (name) VALUES ('Alice')")
      // Now do a non-INSERT; generated keys should reset to empty
      st.executeUpdate("UPDATE t SET name = 'Updated' WHERE id = 1")
      val rs = st.getGeneratedKeys
      rs.next() shouldBe false
    finally conn.close()

  "in-memory: Statement batch INSERT" in:
    val conn = memConn()
    try
      val st = conn.createStatement()
      st.executeUpdate("CREATE TABLE t (id INT, name TEXT)")
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

  "in-memory: PreparedStatement batch INSERT" in:
    val conn = memConn()
    try
      val st = conn.createStatement()
      st.executeUpdate("CREATE TABLE t (id INT, name TEXT)")
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

  "in-memory: clearBatch discards pending" in:
    val conn = memConn()
    try
      val st = conn.createStatement()
      st.executeUpdate("CREATE TABLE t (id INT)")
      st.addBatch("INSERT INTO t VALUES (1)")
      st.addBatch("INSERT INTO t VALUES (2)")
      st.clearBatch()
      val counts = st.executeBatch()
      counts shouldBe Array.empty[Int]
      val rs = st.executeQuery("SELECT COUNT(*) AS c FROM t")
      rs.next() shouldBe true
      rs.getInt("c") shouldBe 0
    finally conn.close()

  "in-memory: Statement batch UPDATE" in:
    val conn = memConn()
    try
      val st = conn.createStatement()
      st.executeUpdate("CREATE TABLE t (id INT, v INT)")
      st.executeUpdate("INSERT INTO t VALUES (1, 10), (2, 20), (3, 30)")
      st.addBatch("UPDATE t SET v = 0 WHERE id < 3")
      st.addBatch("UPDATE t SET v = 99 WHERE id = 3")
      val counts = st.executeBatch()
      counts shouldBe Array(2, 1)
    finally conn.close()

  "in-memory: getImportedKeys returns FK columns" in:
    val conn = memConn()
    try
      val st = conn.createStatement()
      st.executeUpdate("CREATE TABLE authors (id SERIAL PRIMARY KEY, name TEXT)")
      st.executeUpdate("CREATE TABLE books (id SERIAL PRIMARY KEY, author_id INT REFERENCES authors(id))")
      val rs = conn.getMetaData.getImportedKeys(null, null, "books")
      rs.next() shouldBe true
      rs.getString("PKTABLE_NAME")  shouldBe "authors"
      rs.getString("PKCOLUMN_NAME") shouldBe "id"
      rs.getString("FKTABLE_NAME")  shouldBe "books"
      rs.getString("FKCOLUMN_NAME") shouldBe "author_id"
      rs.getInt("KEY_SEQ")          shouldBe 1
      rs.next() shouldBe false
    finally conn.close()

  "in-memory: getImportedKeys empty when no FKs" in:
    val conn = memConn()
    try
      val st = conn.createStatement()
      st.executeUpdate("CREATE TABLE t (id INT, name TEXT)")
      val rs = conn.getMetaData.getImportedKeys(null, null, "t")
      rs.next() shouldBe false
    finally conn.close()

  "in-memory: getExportedKeys returns child FKs" in:
    val conn = memConn()
    try
      val st = conn.createStatement()
      st.executeUpdate("CREATE TABLE authors (id SERIAL PRIMARY KEY, name TEXT)")
      st.executeUpdate("CREATE TABLE books (id SERIAL PRIMARY KEY, author_id INT REFERENCES authors(id))")
      val rs = conn.getMetaData.getExportedKeys(null, null, "authors")
      rs.next() shouldBe true
      rs.getString("PKTABLE_NAME")  shouldBe "authors"
      rs.getString("PKCOLUMN_NAME") shouldBe "id"
      rs.getString("FKTABLE_NAME")  shouldBe "books"
      rs.getString("FKCOLUMN_NAME") shouldBe "author_id"
      rs.getInt("KEY_SEQ")          shouldBe 1
      rs.next() shouldBe false
    finally conn.close()

  "in-memory: getExportedKeys empty when no children" in:
    val conn = memConn()
    try
      val st = conn.createStatement()
      st.executeUpdate("CREATE TABLE t (id SERIAL PRIMARY KEY)")
      val rs = conn.getMetaData.getExportedKeys(null, null, "t")
      rs.next() shouldBe false
    finally conn.close()

  "in-memory: getImportedKeys DELETE_RULE and UPDATE_RULE" in:
    val conn = memConn()
    try
      val st = conn.createStatement()
      st.executeUpdate("CREATE TABLE parents (id INT PRIMARY KEY)")
      st.executeUpdate("CREATE TABLE children (id INT, pid INT REFERENCES parents(id) ON DELETE CASCADE ON UPDATE RESTRICT)")
      val rs = conn.getMetaData.getImportedKeys(null, null, "children")
      rs.next() shouldBe true
      rs.getInt("DELETE_RULE") shouldBe java.sql.DatabaseMetaData.importedKeyCascade
      rs.getInt("UPDATE_RULE") shouldBe java.sql.DatabaseMetaData.importedKeyRestrict
      rs.next() shouldBe false
    finally conn.close()

  "in-memory: getIndexInfo returns indexes" in:
    val conn = memConn()
    try
      val st = conn.createStatement()
      st.executeUpdate("CREATE TABLE t (id INT, name TEXT)")
      st.executeUpdate("CREATE INDEX idx_name ON t (name)")
      val rs = conn.getMetaData.getIndexInfo(null, null, "t", false, false)
      rs.next() shouldBe true
      rs.getString("TABLE_NAME")   shouldBe "t"
      rs.getString("INDEX_NAME")   shouldBe "idx_name"
      rs.getString("COLUMN_NAME")  shouldBe "name"
      rs.getBoolean("NON_UNIQUE")  shouldBe true
      rs.getInt("ORDINAL_POSITION") shouldBe 1
      rs.next() shouldBe false
    finally conn.close()

  "in-memory: getIndexInfo unique filter" in:
    val conn = memConn()
    try
      val st = conn.createStatement()
      st.executeUpdate("CREATE TABLE t (id INT, name TEXT, code TEXT)")
      st.executeUpdate("CREATE INDEX idx_name ON t (name)")
      st.executeUpdate("CREATE UNIQUE INDEX idx_code ON t (code)")
      val rsAll = conn.getMetaData.getIndexInfo(null, null, "t", false, false)
      var count = 0
      while rsAll.next() do count += 1
      count shouldBe 2
      val rsUniq = conn.getMetaData.getIndexInfo(null, null, "t", true, false)
      rsUniq.next() shouldBe true
      rsUniq.getString("INDEX_NAME") shouldBe "idx_code"
      rsUniq.getBoolean("NON_UNIQUE") shouldBe false
      rsUniq.next() shouldBe false
    finally conn.close()

  "in-memory: getIndexInfo empty when no indexes" in:
    val conn = memConn()
    try
      val st = conn.createStatement()
      st.executeUpdate("CREATE TABLE t (id INT)")
      val rs = conn.getMetaData.getIndexInfo(null, null, "t", false, false)
      rs.next() shouldBe false
    finally conn.close()

  "in-memory: getIndexInfo multi-column index" in:
    val conn = memConn()
    try
      val st = conn.createStatement()
      st.executeUpdate("CREATE TABLE t (a INT, b INT, c TEXT)")
      st.executeUpdate("CREATE INDEX idx_ab ON t (a, b)")
      val rs = conn.getMetaData.getIndexInfo(null, null, "t", false, false)
      rs.next() shouldBe true
      rs.getString("COLUMN_NAME")   shouldBe "a"
      rs.getInt("ORDINAL_POSITION") shouldBe 1
      rs.next() shouldBe true
      rs.getString("COLUMN_NAME")   shouldBe "b"
      rs.getInt("ORDINAL_POSITION") shouldBe 2
      rs.next() shouldBe false
    finally conn.close()

  "shared DB: memory connections are independent" in:
    val conn1 = memConn()
    val conn2 = memConn()
    try
      conn1.createStatement().executeUpdate("CREATE TABLE t (id INT)")
      conn1.createStatement().executeUpdate("INSERT INTO t VALUES (1)")
      assertThrows[Exception]:
        conn2.createStatement().executeQuery("SELECT * FROM t")
    finally
      conn1.close()
      conn2.close()
