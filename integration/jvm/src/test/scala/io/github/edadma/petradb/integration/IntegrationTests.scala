package io.github.edadma.petradb.integration

import io.github.edadma.petradb.engine.MemoryDB
import io.github.edadma.petradb.server.PetraServer
import io.github.edadma.microserve.EventLoop

import scala.concurrent.Future

class IntegrationTests extends IntegrationTestBase:

  protected def startServer(): Future[(AnyRef, Int)] =
    val loop   = new EventLoop
    val db     = new MemoryDB
    val server = new PetraServer(loop, db, port = 0)
    server.start()
    val port   = server.actualPort
    val thread = new Thread(() => loop.run())
    thread.setDaemon(true)
    thread.start()
    Thread.sleep(100)
    Future.successful(((server, loop, thread), port))

  protected def stopServer(handle: AnyRef): Future[Unit] =
    val (server, loop, thread) = handle.asInstanceOf[(PetraServer, EventLoop, Thread)]
    server.stop(() => loop.stop())
    thread.join(3000)
    Future.unit
