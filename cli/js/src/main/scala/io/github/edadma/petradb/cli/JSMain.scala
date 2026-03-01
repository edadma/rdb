package io.github.edadma.petradb.cli

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExportTopLevel

object JSMain:
  @JSExportTopLevel("main")
  def jsMain(args: js.Array[String]): Unit =
    Main.main(args.toArray)
