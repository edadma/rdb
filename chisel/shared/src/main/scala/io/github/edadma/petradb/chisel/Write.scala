package io.github.edadma.petradb.chisel

import io.github.edadma.petradb.Value

import scala.deriving.Mirror

/** Encodes a Scala value of type `A` into named columns: `Seq[(columnName, Value)]`.
  *
  * Derived for any product by composing a [[Put]] per field, pairing each encoded value with its
  * field label. The result drives INSERT column/value lists and UPDATE `SET` clauses — the column
  * order follows declaration order.
  */
trait Write[A]:
  def writeNamed(a: A): Seq[(String, Value)]

  /** Encode a `B` by first projecting it to an `A`. */
  final def contramap[B](f: B => A): Write[B] = (b: B) => writeNamed(f(b))

object Write:
  inline def apply[A](using w: Write[A]): Write[A] = w

  /** By-name derivation: field label → column name. Invoked by `derives Write`. */
  inline def derived[A](using m: Mirror.ProductOf[A]): Write[A] =
    fromLabels(Derivation.labels[m.MirroredElemLabels], Derivation.puts[m.MirroredElemTypes])

  /** Assemble a by-name writer from precomputed labels and encoders. Public so the inline derivation
    * can reference it from the call site; also usable to hand-build a writer.
    */
  def fromLabels[A](labels: Array[String], puts: Array[Put[Any]]): Write[A] =
    new ByName[A](labels, puts)

  // Concrete implementation keeps the per-row loop out of the inline expansion.
  private final class ByName[A](labels: Array[String], puts: Array[Put[Any]]) extends Write[A]:
    def writeNamed(a: A): Seq[(String, Value)] =
      val product = a.asInstanceOf[Product]
      val out     = Vector.newBuilder[(String, Value)]
      var i       = 0
      while i < labels.length do
        out += labels(i) -> puts(i).put(product.productElement(i))
        i += 1
      out.result()
