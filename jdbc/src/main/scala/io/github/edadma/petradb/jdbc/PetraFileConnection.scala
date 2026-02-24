package io.github.edadma.petradb.jdbc

import io.github.edadma.petradb.*

class PetraFileConnection(path: String) extends AbstractConnection:

  val url: String      = s"jdbc:petradb:file:$path"
  val username: String = ""

  private val db: DB = path match
    case ":memory:" => new MemoryDB
    case p if p.endsWith(".ptxt") => TextDB.open(p)
    case p =>
      val f = new java.io.File(p)
      if f.exists() then PersistentDB.open(p)
      else PersistentDB.create(p, 4096)

  private given session: Session = db.connect()

  def execute(sql: String): Seq[Result] = executeSQL(sql)
  def doClose(): Unit                   = db.close()
