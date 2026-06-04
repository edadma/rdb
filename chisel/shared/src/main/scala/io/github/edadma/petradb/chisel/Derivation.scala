package io.github.edadma.petradb.chisel

import scala.compiletime.{constValueTuple, erasedValue, summonInline}

/** Compile-time helpers shared by [[Read]] and [[Write]] derivation.
  *
  * All three are `inline` so they expand at the derivation site, where the leaf `Get`/`Put` givens
  * are in scope. No runtime reflection is used, which is what keeps derivation working on JS and
  * Native as well as the JVM.
  */
private[chisel] object Derivation:

  /** The element labels of a product type's `Mirror`, as a `String` array (field/column names). */
  inline def labels[L <: Tuple]: Array[String] =
    constValueTuple[L].toArray.map(_.asInstanceOf[String])

  /** One `Get` per element type, in declaration order. */
  inline def gets[T <: Tuple]: Array[Get[Any]] = summonGets[T].toArray

  /** One `Put` per element type, in declaration order. */
  inline def puts[T <: Tuple]: Array[Put[Any]] = summonPuts[T].toArray

  private inline def summonGets[T <: Tuple]: List[Get[Any]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (h *: t)   => summonInline[Get[h]].asInstanceOf[Get[Any]] :: summonGets[t]

  private inline def summonPuts[T <: Tuple]: List[Put[Any]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (h *: t)   => summonInline[Put[h]].asInstanceOf[Put[Any]] :: summonPuts[t]
