package io.github.edadma.petradb.integration

import org.scalatest.Assertion
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers

import io.github.edadma.petradb.*
import io.github.edadma.petradb.client.{Session, SessionOptions}

import scala.concurrent.{ExecutionContext, Future}

trait IntegrationTestBase extends AsyncFreeSpec with Matchers:

  implicit override def executionContext: ExecutionContext = ExecutionContext.global

  protected def startServer(): Future[(AnyRef, Int)]
  protected def stopServer(handle: AnyRef): Future[Unit]

  private def withServer(test: Session => Future[Assertion]): Future[Assertion] =
    startServer().flatMap { case (handle, port) =>
      val session = Session(SessionOptions(port = port))
      test(session)
        .flatMap { result =>
          session.close().flatMap(_ => stopServer(handle).map(_ => result))
        }
        .recoverWith { case ex =>
          session.close().flatMap(_ => stopServer(handle)).flatMap(_ => Future.failed(ex))
        }
    }

  private def withConnectedSession(test: (Session, Int) => Future[Assertion]): Future[Assertion] =
    startServer().flatMap { case (handle, port) =>
      val session = Session(SessionOptions(port = port))
      session.connect().flatMap { _ =>
        test(session, port)
          .flatMap { result =>
            session.close().flatMap(_ => stopServer(handle).map(_ => result))
          }
          .recoverWith { case ex =>
            session.close().flatMap(_ => stopServer(handle)).flatMap(_ => Future.failed(ex))
          }
      }
    }

  "stateless execute" - {

    "DDL result" in withServer { session =>
      session.execute("CREATE TABLE t (id INT, name TEXT)").map { results =>
        results shouldBe Seq(CreateTableResult("t"))
      }
    }

    "insert and select" in withServer { session =>
      for
        _       <- session.execute("CREATE TABLE t (id INT, name TEXT)")
        _       <- session.execute("INSERT INTO t VALUES (1, 'Alice')")
        results <- session.execute("SELECT * FROM t")
      yield
        val table = results.head.asInstanceOf[QueryResult].table
        table.data.length shouldBe 1
        table.data(0).getInt("id") shouldBe 1
        table.data(0).getString("name") shouldBe "Alice"
    }

    "multi-statement" in withServer { session =>
      session.execute("CREATE TABLE t (id INT); INSERT INTO t VALUES (1); SELECT * FROM t").map { results =>
        results.length shouldBe 3
        results(0) shouldBe CreateTableResult("t")
        results(1) shouldBe a[InsertResult]
        results(2) shouldBe a[QueryResult]
      }
    }

    "SQL error throws RuntimeException" in withServer { session =>
      session.execute("SELECT * FROM nonexistent").failed.map { ex =>
        ex shouldBe a[RuntimeException]
        ex.getMessage should not be empty
      }
    }
  }

  "connected session" - {

    "state persists across execute calls" in withConnectedSession { (session, _) =>
      for
        _ <- session.execute("CREATE TABLE t (id INT)")
        _ <- session.execute("INSERT INTO t VALUES (1)")
        _ <- session.execute("INSERT INTO t VALUES (2)")
        results <- session.execute("SELECT COUNT(*) FROM t")
      yield
        val count = results.head.asInstanceOf[QueryResult].table.data(0).data(0)
        count.asInstanceOf[NumberValue].value.intValue shouldBe 2
    }

    "transaction commit" in withConnectedSession { (session, _) =>
      for
        _ <- session.execute("CREATE TABLE t (id INT)")
        _ <- session.execute("BEGIN")
        _ <- session.execute("INSERT INTO t VALUES (1)")
        _ <- session.execute("COMMIT")
        results <- session.execute("SELECT * FROM t")
      yield
        results.head.asInstanceOf[QueryResult].table.data.length shouldBe 1
    }

    "transaction rollback" in withConnectedSession { (session, _) =>
      for
        _ <- session.execute("CREATE TABLE t (id INT)")
        _ <- session.execute("BEGIN")
        _ <- session.execute("INSERT INTO t VALUES (1)")
        _ <- session.execute("ROLLBACK")
        results <- session.execute("SELECT * FROM t")
      yield
        results.head.asInstanceOf[QueryResult].table.data.length shouldBe 0
    }

    "two sessions on same server see each other's committed data" in {
      startServer().flatMap { case (handle, port) =>
        val session1 = Session(SessionOptions(port = port))
        val session2 = Session(SessionOptions(port = port))

        val test = for
          _       <- session1.connect()
          _       <- session2.connect()
          _       <- session1.execute("CREATE TABLE t (id INT, name TEXT)")
          _       <- session1.execute("INSERT INTO t VALUES (1, 'from-session-1')")
          r1      <- session2.execute("SELECT * FROM t")
          table    = r1.head.asInstanceOf[QueryResult].table
          _        = table.data.length shouldBe 1
          _        = table.data(0).getString("name") shouldBe "from-session-1"
          _       <- session2.execute("INSERT INTO t VALUES (2, 'from-session-2')")
          r2      <- session1.execute("SELECT COUNT(*) FROM t")
          count    = r2.head.asInstanceOf[QueryResult].table.data(0).data(0)
        yield count.asInstanceOf[NumberValue].value.intValue shouldBe 2

        test
          .flatMap { result =>
            session1.close()
              .flatMap(_ => session2.close())
              .flatMap(_ => stopServer(handle))
              .map(_ => result)
          }
          .recoverWith { case ex =>
            session1.close()
              .flatMap(_ => session2.close())
              .flatMap(_ => stopServer(handle))
              .flatMap(_ => Future.failed(ex))
          }
      }
    }
  }
