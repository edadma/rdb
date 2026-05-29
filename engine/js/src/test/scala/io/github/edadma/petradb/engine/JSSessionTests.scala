package io.github.edadma.petradb.engine

import scala.scalajs.js
import org.scalatest.Assertion
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.{ExecutionContext, Future}
import scala.scalajs.js.JSConverters._

class JSSessionTests extends AsyncFreeSpec with Matchers {

  implicit override def executionContext: ExecutionContext = ExecutionContext.global

  private def withSession(f: JSSession => Future[Assertion]): Future[Assertion] =
    val session = new JSSession()
    f(session).andThen { case _ => session.close() }

  private def query(session: JSSession, sql: String): Future[js.Dynamic] =
    session.execute(sql).toFuture.map(_.last.asInstanceOf[js.Dynamic])

  "toJS converts DateValue to JS Date" in withSession { session =>
    for
      _ <- session.execute("CREATE TABLE t (d DATE)").toFuture
      _ <- session.execute("INSERT INTO t (d) VALUES ('2024-01-15')").toFuture
      result <- query(session, "SELECT d FROM t")
    yield
      val row = result.rows.asInstanceOf[js.Array[js.Dynamic]](0)
      val d = row.d
      d.isInstanceOf[js.Date] shouldBe true
  }

  "toJS converts TimeValue to string" in withSession { session =>
    for
      _ <- session.execute("CREATE TABLE t (t TIME)").toFuture
      _ <- session.execute("INSERT INTO t (t) VALUES ('10:30:00')").toFuture
      result <- query(session, "SELECT t FROM t")
    yield
      val row = result.rows.asInstanceOf[js.Array[js.Dynamic]](0)
      val t = row.t.asInstanceOf[String]
      t shouldBe "10:30"
  }

  "toJS converts TimestampValue to JS Date" in withSession { session =>
    for
      _ <- session.execute("CREATE TABLE t (ts TIMESTAMP)").toFuture
      _ <- session.execute("INSERT INTO t (ts) VALUES ('2024-01-15 10:30:00')").toFuture
      result <- query(session, "SELECT ts FROM t")
    yield
      val row = result.rows.asInstanceOf[js.Array[js.Dynamic]](0)
      val ts = row.ts
      ts.isInstanceOf[js.Date] shouldBe true
  }

  "toJS converts TimestampTZValue to JS Date" in withSession { session =>
    for
      _ <- session.execute("CREATE TABLE t (ts TIMESTAMP WITH TIME ZONE)").toFuture
      _ <- session.execute("INSERT INTO t (ts) VALUES ('2024-01-15T10:30:00+05:00')").toFuture
      result <- query(session, "SELECT ts FROM t")
    yield
      val row = result.rows.asInstanceOf[js.Array[js.Dynamic]](0)
      val ts = row.ts
      ts.isInstanceOf[js.Date] shouldBe true
  }

  "toJS converts IntervalValue to string" in withSession { session =>
    for
      _ <- session.execute("CREATE TABLE t (i INTERVAL)").toFuture
      _ <- session.execute("INSERT INTO t (i) VALUES ('1 day 2 hours')").toFuture
      result <- query(session, "SELECT i FROM t")
    yield
      val row = result.rows.asInstanceOf[js.Array[js.Dynamic]](0)
      val i = row.i.asInstanceOf[String]
      i should include("day")
  }

  "toJS converts ByteaValue to array" in withSession { session =>
    for
      _ <- session.execute("CREATE TABLE t (b BYTEA)").toFuture
      _ <- session.execute("INSERT INTO t (b) VALUES ('\\x48656c6c6f')").toFuture
      result <- query(session, "SELECT b FROM t")
    yield
      val row = result.rows.asInstanceOf[js.Array[js.Dynamic]](0)
      val b = row.b.asInstanceOf[js.Array[Byte]]
      b.length should be > 0
  }

  "toJS handles all types in one table" in withSession { session =>
    for
      _ <- session.execute(
        """CREATE TABLE t (
          |  d DATE,
          |  ts TIMESTAMP,
          |  txt TEXT,
          |  num INT,
          |  b BOOLEAN
          |)""".stripMargin
      ).toFuture
      _ <- session.execute(
        "INSERT INTO t (d, ts, txt, num, b) VALUES ('2024-01-15', '2024-01-15 10:30:00', 'hello', 42, TRUE)"
      ).toFuture
      result <- query(session, "SELECT d, ts, txt, num, b FROM t")
    yield
      val row = result.rows.asInstanceOf[js.Array[js.Dynamic]](0)
      row.d.isInstanceOf[js.Date] shouldBe true
      row.ts.isInstanceOf[js.Date] shouldBe true
      row.txt.asInstanceOf[String] shouldBe "hello"
      (row.num.asInstanceOf[Int]) shouldBe 42
      row.b.asInstanceOf[Boolean] shouldBe true
  }

  "typeString includes date/time types in fields metadata" in withSession { session =>
    for
      _ <- session.execute("CREATE TABLE t (d DATE, tm TIME, ts TIMESTAMP)").toFuture
      _ <- session.execute("INSERT INTO t (d, tm, ts) VALUES ('2024-01-15', '10:30:00', '2024-01-15 10:30:00')").toFuture
      result <- query(session, "SELECT d, tm, ts FROM t")
    yield
      val fields = result.fields.asInstanceOf[js.Array[js.Dynamic]]
      fields(0).dataType.asInstanceOf[String] shouldBe "date"
      fields(1).dataType.asInstanceOf[String] shouldBe "time"
      fields(2).dataType.asInstanceOf[String] shouldBe "timestamp"
  }

  // A timestamptz carries an explicit offset, so its JS Date must land on the exact instant the
  // offset names — not be reinterpreted in the runtime's local zone. '...+05:00' is 05:30:00Z.
  "toJS preserves the instant of a timestamptz (no timezone shift)" in withSession { session =>
    for
      _ <- session.execute("CREATE TABLE t (ts TIMESTAMP WITH TIME ZONE)").toFuture
      _ <- session.execute("INSERT INTO t (ts) VALUES ('2024-01-15T10:30:00+05:00')").toFuture
      result <- query(session, "SELECT ts FROM t")
    yield
      val row = result.rows.asInstanceOf[js.Array[js.Dynamic]](0)
      val ts = row.ts.asInstanceOf[js.Date]
      ts.getTime() shouldBe js.Date.UTC(2024, 0, 15, 5, 30, 0)
  }

  // The reported bug: CURRENT_TIMESTAMP came back as a zone-naive timestamp, so new Date(str) read
  // the UTC wall clock as local time and shifted the instant by the local offset. As a timestamptz
  // it must round-trip to a JS Date that matches the actual wall clock.
  "CURRENT_TIMESTAMP round-trips to a JS Date matching the wall clock" in withSession { session =>
    val before = js.Date.now()
    for
      result <- query(session, "SELECT CURRENT_TIMESTAMP AS ts")
    yield
      val after = js.Date.now()
      val row = result.rows.asInstanceOf[js.Array[js.Dynamic]](0)
      val ts = row.ts.asInstanceOf[js.Date]
      ts.isInstanceOf[js.Date] shouldBe true
      val t = ts.getTime()
      withClue(s"CURRENT_TIMESTAMP=$t expected within 2ms of [$before, $after]: ") {
        (t >= before - 2 && t <= after + 2) shouldBe true
      }
  }
}
