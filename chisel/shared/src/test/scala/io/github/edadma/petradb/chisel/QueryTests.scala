package io.github.edadma.petradb.chisel

import io.github.edadma.petradb.Session
import io.github.edadma.petradb.engine.MemoryDB

import scala.concurrent.{ExecutionContext, Future}

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

/** End-to-end: the `sql"…"` interpolator + `Read`/`Get` decoding against a real engine session. The
  * engine completes its `Future`s synchronously, so results extract via `.value.get.get` without
  * blocking — keeping these assertions cross-platform.
  */
class QueryTests extends AnyFreeSpec with Matchers:

  given ExecutionContext = ExecutionContext.parasitic

  case class User(id: Long, name: String, age: Int) derives Read, Write

  extension [A](f: Future[A]) private def await: A = f.value.get.get

  private def seeded(): Session =
    given s: Session = new MemoryDB().connect()
    sql"create table users (id integer primary key, name text, age integer)".run.await
    sql"insert into users (id, name, age) values (1, 'alice', 30), (2, 'bob', 25), (3, 'carol', 30)".update.await
    s

  "query decoding" - {
    "toList decodes every row into the case class" in {
      given s: Session = seeded()
      val users        = sql"select id, name, age from users order by id".query[User].toList.await
      users shouldBe List(User(1, "alice", 30), User(2, "bob", 25), User(3, "carol", 30))
    }

    "a bound parameter filters the result" in {
      given s: Session = seeded()
      val age          = 30
      val users        = (sql"select * from users where age = $age" ++ sql" order by id").query[User].toList.await
      users.map(_.name) shouldBe List("alice", "carol")
    }

    "option returns Some for a hit and None for a miss" in {
      given s: Session = seeded()
      sql"select * from users where id = ${2}".query[User].option.await shouldBe Some(User(2, "bob", 25))
      sql"select * from users where id = ${99}".query[User].option.await shouldBe None
    }

    "unique returns the single row" in {
      given s: Session = seeded()
      sql"select * from users where id = ${1}".query[User].unique.await shouldBe User(1, "alice", 30)
    }

    "unique fails when the result is not exactly one row" in {
      given s: Session = seeded()
      a[DecodeException] should be thrownBy sql"select * from users".query[User].unique.await
      a[DecodeException] should be thrownBy sql"select * from users where id = ${99}".query[User].unique.await
    }
  }

  "statement execution" - {
    "update reports the affected row count" in {
      given s: Session = seeded()
      sql"update users set age = ${40} where age = ${30}".update.await shouldBe 2
      sql"delete from users where id = ${2}".update.await shouldBe 1
    }

    "INSERT … RETURNING decodes through query" in {
      given s: Session = seeded()
      val inserted     =
        sql"insert into users (id, name, age) values (${4}, ${"dave"}, ${22}) returning id, name, age"
          .query[User].unique.await
      inserted shouldBe User(4, "dave", 22)
    }
  }
