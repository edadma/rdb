package io.github.edadma.petradb.engine

import io.github.edadma.petradb.{Session as _, *}

private[engine] def platformAnyToValue(a: Any): Value =
  throw ExecutionException(null, s"cannot convert ${a.getClass.getName} to a database value")
