package io.github.edadma.rdb

import scala.scalajs.js
import js.Dynamic.{global => g}

object Platform:
  private val crypto = g.require("crypto")

  def randomUUID: String = crypto.randomUUID().asInstanceOf[String]
