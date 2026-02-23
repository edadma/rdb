package io.github.edadma.petradb.integration

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import io.github.edadma.petradb.*
import io.github.edadma.petradb.client.{Session, SessionOptions}
import io.github.edadma.petradb.server.PetraServer
import io.github.edadma.microserve.EventLoop

import scala.concurrent.Await
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global

class IntegrationTests extends AnyFreeSpec with Matchers:

  private def withSession(test: Session => Unit): Unit =
    val loop   = new EventLoop
    val db     = new MemoryDB
    val server = new PetraServer(loop, db, port = 0)
    server.start()
    val port   = server.actualPort
    val thread = new Thread(() => loop.run())
    thread.setDaemon(true)
    thread.start()
    Thread.sleep(100)
    val session = Session(SessionOptions(port = port))
    try
      test(session)
    finally
      Await.ready(session.close(), 5.seconds)
      server.stop(() => loop.stop())
      thread.join(3000)

  private def withConnectedSession(test: (Session, Int) => Unit): Unit =
    val loop   = new EventLoop
    val db     = new MemoryDB
    val server = new PetraServer(loop, db, port = 0)
    server.start()
    val port   = server.actualPort
    val thread = new Thread(() => loop.run())
    thread.setDaemon(true)
    thread.start()
    Thread.sleep(100)
    val session = Session(SessionOptions(port = port))
    Await.result(session.connect(), 5.seconds)
    try
      test(session, port)
    finally
      Await.ready(session.close(), 5.seconds)
      server.stop(() => loop.stop())
      thread.join(3000)

  "stateless execute" - {

    "DDL result" in withSession { session =>
      val results = Await.result(
        session.execute("CREATE TABLE t (id INT, name TEXT)"),
        5.seconds,
      )
      results shouldBe Seq(CreateTableResult("t"))
    }

    "insert and select" in withSession { session =>
      Await.result(session.execute("CREATE TABLE t (id INT, name TEXT)"), 5.seconds)
      Await.result(session.execute("INSERT INTO t VALUES (1, 'Alice')"), 5.seconds)

      val results = Await.result(session.execute("SELECT * FROM t"), 5.seconds)
      val table   = results.head.asInstanceOf[QueryResult].table
      table.data.length shouldBe 1
      table.data(0).getInt("id") shouldBe 1
      table.data(0).getString("name") shouldBe "Alice"
    }

    "multi-statement" in withSession { session =>
      val results = Await.result(
        session.execute("CREATE TABLE t (id INT); INSERT INTO t VALUES (1); SELECT * FROM t"),
        5.seconds,
      )
      results.length shouldBe 3
      results(0) shouldBe CreateTableResult("t")
      results(1) shouldBe a[InsertResult]
      results(2) shouldBe a[QueryResult]
    }

    "SQL error throws RuntimeException" in withSession { session =>
      val ex = intercept[RuntimeException] {
        Await.result(session.execute("SELECT * FROM nonexistent"), 5.seconds)
      }
      ex.getMessage should not be empty
    }
  }

  "connected session" - {

    "state persists across execute calls" in withConnectedSession { (session, _) =>
      Await.result(session.execute("CREATE TABLE t (id INT)"), 5.seconds)
      Await.result(session.execute("INSERT INTO t VALUES (1)"), 5.seconds)
      Await.result(session.execute("INSERT INTO t VALUES (2)"), 5.seconds)

      val results = Await.result(session.execute("SELECT COUNT(*) FROM t"), 5.seconds)
      val count   = results.head.asInstanceOf[QueryResult].table.data(0).data(0)
      count.asInstanceOf[NumberValue].value.intValue shouldBe 2
    }

    "transaction commit" in withConnectedSession { (session, _) =>
      Await.result(session.execute("CREATE TABLE t (id INT)"), 5.seconds)
      Await.result(session.execute("BEGIN"), 5.seconds)
      Await.result(session.execute("INSERT INTO t VALUES (1)"), 5.seconds)
      Await.result(session.execute("COMMIT"), 5.seconds)

      val results = Await.result(session.execute("SELECT * FROM t"), 5.seconds)
      results.head.asInstanceOf[QueryResult].table.data.length shouldBe 1
    }

    "transaction rollback" in withConnectedSession { (session, _) =>
      Await.result(session.execute("CREATE TABLE t (id INT)"), 5.seconds)
      Await.result(session.execute("BEGIN"), 5.seconds)
      Await.result(session.execute("INSERT INTO t VALUES (1)"), 5.seconds)
      Await.result(session.execute("ROLLBACK"), 5.seconds)

      val results = Await.result(session.execute("SELECT * FROM t"), 5.seconds)
      results.head.asInstanceOf[QueryResult].table.data.length shouldBe 0
    }

    "two sessions on same server see each other's committed data" in withConnectedSession { (session1, port) =>
      val session2 = Session(SessionOptions(port = port))
      Await.result(session2.connect(), 5.seconds)
      try
        Await.result(session1.execute("CREATE TABLE t (id INT, name TEXT)"), 5.seconds)
        Await.result(session1.execute("INSERT INTO t VALUES (1, 'from-session-1')"), 5.seconds)

        // session2 should see data committed by session1
        val r1    = Await.result(session2.execute("SELECT * FROM t"), 5.seconds)
        val table = r1.head.asInstanceOf[QueryResult].table
        table.data.length shouldBe 1
        table.data(0).getString("name") shouldBe "from-session-1"

        // session2 inserts its own row
        Await.result(session2.execute("INSERT INTO t VALUES (2, 'from-session-2')"), 5.seconds)

        // session1 sees both rows
        val r2    = Await.result(session1.execute("SELECT COUNT(*) FROM t"), 5.seconds)
        val count = r2.head.asInstanceOf[QueryResult].table.data(0).data(0)
        count.asInstanceOf[NumberValue].value.intValue shouldBe 2
      finally
        Await.ready(session2.close(), 5.seconds)
    }
  }
