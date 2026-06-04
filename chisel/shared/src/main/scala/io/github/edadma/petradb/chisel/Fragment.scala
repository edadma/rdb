package io.github.edadma.petradb.chisel

import io.github.edadma.petradb.Value

/** A SQL fragment: literal text parts interleaved with bound parameter [[Value]]s.
  *
  * Stored unrendered (parts + params) so fragments compose with `++` without rewriting placeholder
  * indices. [[sql]] renders the canonical engine form — `$1`, `$2`, … — at execution time. Because
  * interpolated arguments are encoded to [[Value]]s and bound, they never become SQL text, so the
  * interpolator is injection-safe by construction.
  *
  * Invariant: `parts.length == params.length + 1`.
  */
final class Fragment(val parts: Seq[String], val params: Seq[Value]):

  /** Concatenate two fragments, joining the text at the boundary and appending parameters. */
  def ++(other: Fragment): Fragment =
    val joined = parts.init :+ (parts.last + other.parts.head)
    new Fragment(joined ++ other.parts.tail, params ++ other.params)

  /** Render to engine SQL with positional `$n` placeholders, numbered left-to-right. */
  def sql: String =
    val sb = new StringBuilder(parts.head)
    var i  = 1
    for p <- parts.tail do
      sb.append('$').append(i).append(p)
      i += 1
    sb.toString

  override def toString: String = s"Fragment($sql, $params)"

object Fragment:
  /** The empty fragment — identity for `++`. */
  val empty: Fragment = new Fragment(Seq(""), Nil)

  /** Raw SQL text with no bound parameters (e.g. a dynamically chosen table name or clause). */
  def const(text: String): Fragment = new Fragment(Seq(text), Nil)

  /** A single bound parameter and nothing else — renders to one `$n` placeholder. */
  def param(value: Value): Fragment = new Fragment(Seq("", ""), Seq(value))

/** One interpolated argument, already encoded to a [[Value]] via its [[Put]]. */
final case class Param(value: Value)

object Param:
  /** Any value with a [[Put]] becomes a [[Param]] at an interpolation site. */
  given fromPut[A](using p: Put[A]): Conversion[A, Param] = a => Param(p.put(a))

extension (sc: StringContext)
  /** The `sql"…"` interpolator. Each `$arg` is encoded via its [[Put]] into a bound parameter; the
    * surrounding text becomes the fragment's literal parts. No argument is ever rendered into SQL
    * text, so injection is impossible.
    */
  def sql(args: Param*): Fragment =
    new Fragment(sc.parts.toIndexedSeq, args.map(_.value))
