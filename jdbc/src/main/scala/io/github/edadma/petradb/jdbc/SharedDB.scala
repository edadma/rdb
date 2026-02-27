package io.github.edadma.petradb.jdbc

import io.github.edadma.petradb.*

import scala.collection.mutable

object SharedDB:
  private case class Entry(db: DB, var refCount: Int)
  private val registry = mutable.Map[String, Entry]()

  def acquire(key: String): DB =
    synchronized:
      registry.get(key) match
        case Some(entry) =>
          entry.refCount += 1
          entry.db
        case None =>
          val db = key match
            case k if k.startsWith("memory:") => new MemoryDB
            case p if p.endsWith(".ptxt")     => TextDB.open(p)
            case p =>
              val f = new java.io.File(p)
              if f.exists() then PersistentDB.open(p) else PersistentDB.create(p, 4096)
          registry(key) = Entry(db, 1)
          db

  def release(key: String): Unit =
    synchronized:
      registry.get(key).foreach { entry =>
        entry.refCount -= 1
        if entry.refCount <= 0 then
          registry.remove(key)
          entry.db.close()
      }
