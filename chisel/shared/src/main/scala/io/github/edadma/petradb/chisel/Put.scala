package io.github.edadma.petradb.chisel

import io.github.edadma.petradb.*
import io.github.edadma.dal

import java.time.{Duration, LocalDate, LocalDateTime, LocalTime, OffsetDateTime, OffsetTime}
import java.util.UUID

/** Encodes a Scala value of type `A` into a PetraDB [[Value]].
  *
  * A `Put[A]` is the write half of the mapping layer — used to build bind parameters and INSERT
  * value lists. Because the engine binds [[Value]]s directly, encoding never produces SQL text and
  * carries no escaping concerns.
  */
trait Put[A]:
  def put(a: A): Value

  /** Encode a `B` by first projecting it to an `A` — e.g. `Put[String].contramap[Color](_.name)`. */
  final def contramap[B](f: B => A): Put[B] = (b: B) => put(f(b))

object Put:
  inline def apply[A](using p: Put[A]): Put[A] = p

  given Put[Int]        = i => NumberValue(dal.IntType, i)
  given Put[Long]       = l => NumberValue(dal.LongType, l)
  given Put[Short]      = s => NumberValue(dal.IntType, s.toInt)
  given Put[Byte]       = b => NumberValue(dal.IntType, b.toInt)
  given Put[Double]     = d => NumberValue(dal.DoubleType, d)
  given Put[Float]      = f => NumberValue(dal.DoubleType, f.toDouble)
  given Put[BigDecimal] = bd => NumberValue(dal.BigDecType, bd.bigDecimal)
  given Put[String]     = TextValue(_)
  given Put[Boolean]    = BooleanValue(_)
  given Put[Value]      = identity(_) // a pre-built Value passes through unchanged

  given Put[LocalDate]      = DateValue(_)
  given Put[LocalTime]      = TimeValue(_)
  given Put[LocalDateTime]  = TimestampValue(_)
  given Put[OffsetDateTime] = TimestampTZValue(_)
  given Put[OffsetTime]     = TimeTZValue(_)
  given Put[Duration]       = IntervalValue(_)
  given Put[Array[Byte]]    = ByteaValue(_)
  given Put[UUID]           = u => UUIDValue(u.toString)

  /** A nullable column: `None` encodes to SQL `NULL`, `Some(a)` to the underlying encoding. */
  given option[A](using p: Put[A]): Put[Option[A]] =
    case Some(a) => p.put(a)
    case None    => NullValue()
