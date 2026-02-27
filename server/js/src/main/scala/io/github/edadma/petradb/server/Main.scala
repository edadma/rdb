package io.github.edadma.petradb.server

import mainargs.{main, arg, Flag, ParserForMethods}
import io.github.edadma.petradb.*
import io.github.edadma.cross_platform.exists

object Main:
  @main
  def run(
    @arg(short = 'm', doc = "Use in-memory database")
    memory: Flag = Flag(false),
    @arg(short = 'p', doc = "Port number")
    port: Int = DefaultPort,
    @arg(short = 'h', doc = "Host address")
    host: String = "127.0.0.1",
    @arg(short = 'c', doc = "Path to config file")
    config: Option[String] = None,
    @arg(doc = "Database file path")
    path: Option[String] = None,
  ): Unit =
    val db: DB =
      if memory.value || path.isEmpty then new MemoryDB
      else
        val p = path.get
        if exists(p) then PersistentDB.open(p)
        else PersistentDB.create(p, 4096)

    val serverConfig = config match
      case Some(p) => ServerConfig.fromFile(p)
      case None    => ServerConfig.unrestricted

    val mode = if path.isDefined then s"persistent (${path.get})" else "in-memory"
    val server = new PetraServer(db, host, port, serverConfig.auth)

    server.start { () =>
      println(s"PetraDB server ($mode) listening on http://$host:$port")
    }

  def main(args: Array[String]): Unit =
    ParserForMethods(this).runOrExit(args.toIndexedSeq, allowPositional = true)
