package io.github.edadma.rdb

import scala.scalajs.js
import js.Dynamic.{global => g}

object Platform:
  def randomUUID: String = g.crypto.randomUUID().asInstanceOf[String]
