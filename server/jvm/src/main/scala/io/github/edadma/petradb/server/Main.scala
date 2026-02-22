package io.github.edadma.petradb.server

import mainargs.{main, arg, Flag, ParserForMethods}
import io.github.edadma.petradb.*
import io.github.edadma.microserve.EventLoop

object Main:
  @main
  def run(
    @arg(short = 'm', doc = "Use in-memory database")
    memory: Flag = Flag(false),
    @arg(short = 'p', doc = "Port number")
    port: Int = 5432,
    @arg(short = 'h', doc = "Host address")
    host: String = "127.0.0.1",
    @arg(doc = "Database file path")
    path: Option[String] = None,
  ): Unit =
    val db: DB =
      if memory.value || path.isEmpty then new MemoryDB
      else
        val p = path.get
        val f = new java.io.File(p)
        if f.exists() then PersistentDB.open(p)
        else PersistentDB.create(p, 4096)

    val mode = if path.isDefined then s"persistent (${path.get})" else "in-memory"
    val loop = new EventLoop
    val server = new PetraServer(loop, db, host, port)

    server.start { () =>
      println(s"PetraDB server ($mode) listening on http://$host:$port")
    }

    loop.run()

  def main(args: Array[String]): Unit =
    ParserForMethods(this).runOrExit(args.toIndexedSeq, allowPositional = true)
