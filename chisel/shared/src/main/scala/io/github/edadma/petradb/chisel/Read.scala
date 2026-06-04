package io.github.edadma.petradb.chisel

import io.github.edadma.petradb.Row

import scala.deriving.Mirror

/** Decodes a whole [[Row]] into a Scala value of type `A`.
  *
  * Derived for any product (case class, named tuple) by composing a [[Get]] per field. The default
  * derivation is **by name**: each field is read from the column whose name matches the field label,
  * so it is robust to column reordering and works directly with `SELECT *`. For projections whose
  * field names don't match the result columns (e.g. a plain `(Long, String)` tuple), use
  * [[Read.positional]].
  */
trait Read[A]:
  def read(row: Row): A

  /** Decode as `A`, then transform the whole decoded value. */
  final def map[B](f: A => B): Read[B] = (row: Row) => f(read(row))

object Read:
  inline def apply[A](using r: Read[A]): Read[A] = r

  /** By-name derivation: field label → column name. Invoked by `derives Read`. */
  inline def derived[A](using m: Mirror.ProductOf[A]): Read[A] =
    fromLabels(Derivation.labels[m.MirroredElemLabels], Derivation.gets[m.MirroredElemTypes], m.fromProduct)

  /** Positional derivation: read fields from the row's columns left-to-right, ignoring names.
    * Use for tuple projections where field labels don't correspond to column names.
    */
  inline def positional[A](using m: Mirror.ProductOf[A]): Read[A] =
    fromPositions(Derivation.gets[m.MirroredElemTypes], m.fromProduct)

  /** Assemble a by-name reader from precomputed labels and decoders. Public so the inline derivation
    * can reference it from the call site; also usable to hand-build a reader.
    */
  def fromLabels[A](labels: Array[String], gets: Array[Get[Any]], build: Product => A): Read[A] =
    new ByName[A](labels, gets, build)

  /** Assemble a positional reader from precomputed decoders. */
  def fromPositions[A](gets: Array[Get[Any]], build: Product => A): Read[A] =
    new Positional[A](gets, build)

  // Concrete implementations keep the per-row loop out of the inline expansion, so `derives Read`
  // sites duplicate only the array construction, not the decoding logic.
  private final class ByName[A](labels: Array[String], gets: Array[Get[Any]], build: Product => A) extends Read[A]:
    def read(row: Row): A =
      val values = new Array[Any](labels.length)
      var i      = 0
      while i < labels.length do
        values(i) = gets(i).get(row(labels(i)))
        i += 1
      build(Tuple.fromArray(values))

  private final class Positional[A](gets: Array[Get[Any]], build: Product => A) extends Read[A]:
    def read(row: Row): A =
      val values = new Array[Any](gets.length)
      var i      = 0
      while i < gets.length do
        values(i) = gets(i).get(row.data(i))
        i += 1
      build(Tuple.fromArray(values))
