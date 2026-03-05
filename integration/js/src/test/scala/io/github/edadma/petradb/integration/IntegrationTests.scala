package io.github.edadma.petradb.integration

import io.github.edadma.petradb.engine.MemoryDB
import io.github.edadma.petradb.server.PetraServer

import scala.concurrent.{Future, Promise}

class IntegrationTests extends IntegrationTestBase:

  protected def startServer(): Future[(AnyRef, Int)] =
    val db     = new MemoryDB
    val server = new PetraServer(db, port = 0)
    val p      = Promise[(AnyRef, Int)]()
    server.start { () =>
      p.success((server, server.actualPort))
    }
    p.future

  protected def stopServer(handle: AnyRef): Future[Unit] =
    val server = handle.asInstanceOf[PetraServer]
    val p      = Promise[Unit]()
    server.stop { () =>
      p.success(())
    }
    p.future
