package io.github.edadma.petradb.chisel

import io.github.edadma.petradb.Session
import io.github.edadma.petradb.engine.MemoryDB

import scala.concurrent.{ExecutionContext, Future}

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class RepoTests extends AnyFreeSpec with Matchers:

  given ExecutionContext = ExecutionContext.parasitic

  extension [A](f: Future[A]) private def await: A = f.value.get.get

  case class User(id: Long, name: String, age: Int) derives Read, Write

  private val users = Repo[User, Long]("users")

  private def withUsers(): Session =
    given s: Session = new MemoryDB().connect()
    sql"create table users (id serial primary key, name text, age integer)".run.await
    s

  "generated-id repository" - {
    "insertReturning assigns the id and reads the row back" in {
      given s: Session = withUsers()
      val saved        = users.insertReturning(User(0, "alice", 30)).await
      saved.id should be > 0L
      saved.name shouldBe "alice"
    }

    "findById / findAll / count" in {
      given s: Session = withUsers()
      val a            = users.insertReturning(User(0, "alice", 30)).await
      val b            = users.insertReturning(User(0, "bob", 25)).await
      users.findById(a.id).await shouldBe Some(a)
      users.findById(999L).await shouldBe None
      users.findAll.await.toSet shouldBe Set(a, b)
      users.count.await shouldBe 2L
    }

    "existsById reflects presence" in {
      given s: Session = withUsers()
      val a            = users.insertReturning(User(0, "alice", 30)).await
      users.existsById(a.id).await shouldBe true
      users.existsById(999L).await shouldBe false
    }

    "update changes non-id columns for the entity's id" in {
      given s: Session = withUsers()
      val a            = users.insertReturning(User(0, "alice", 30)).await
      users.update(a.copy(age = 31)).await shouldBe 1
      users.findById(a.id).await shouldBe Some(a.copy(age = 31))
    }

    "deleteById and deleteAll remove rows" in {
      given s: Session = withUsers()
      val a            = users.insertReturning(User(0, "alice", 30)).await
      val b            = users.insertReturning(User(0, "bob", 25)).await
      users.deleteById(a.id).await shouldBe 1
      users.count.await shouldBe 1L
      users.deleteAll.await shouldBe 1
      users.count.await shouldBe 0L
    }

    "insert omits the generated id column" in {
      given s: Session = withUsers()
      users.insert(User(0, "carol", 40)).await shouldBe 1
      users.findAll.await.map(_.name) shouldBe List("carol")
    }
  }

  "explicit-id repository" - {
    case class Widget(sku: String, label: String) derives Read, Write
    val widgets = Repo[Widget, String]("widgets", idColumn = "sku", generatedId = false)

    "insert writes the caller-supplied id and round-trips" in {
      given s: Session = new MemoryDB().connect()
      sql"create table widgets (sku text primary key, label text)".run.await
      widgets.insert(Widget("w-1", "Sprocket")).await shouldBe 1
      widgets.findById("w-1").await shouldBe Some(Widget("w-1", "Sprocket"))
      widgets.update(Widget("w-1", "Cog")).await shouldBe 1
      widgets.findById("w-1").await shouldBe Some(Widget("w-1", "Cog"))
    }
  }
