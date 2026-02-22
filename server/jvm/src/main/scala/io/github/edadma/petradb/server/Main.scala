package io.github.edadma.petradb.server

import io.github.edadma.microserve.EventLoop

@main def run(): Unit =
  val loop = new EventLoop
  val server = new PetraServer(loop, port = 5432)

  server.start { () =>
    println("PetraDB server listening on http://127.0.0.1:5432")
  }

  loop.run()
