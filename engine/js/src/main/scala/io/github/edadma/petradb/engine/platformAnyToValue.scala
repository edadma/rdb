package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

import scala.scalajs.js
import js.JSConverters.*

private[engine] def platformAnyToValue(a: Any): Value =
  a match
    case arr: js.Array[?] => ArrayValue(arr.toSeq.map(v => anyToValue(v.asInstanceOf[Any])).toIndexedSeq)
    case _ => throw ExecutionException(null, s"cannot convert ${a.getClass.getName} to a database value")
