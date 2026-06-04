package io.github.edadma.petradb.chisel

import io.github.edadma.petradb.*

import java.time.{Duration, LocalDate, LocalDateTime, LocalTime, OffsetDateTime, OffsetTime}
import java.util.UUID

/** Decodes a single PetraDB [[Value]] into a Scala value of type `A`.
  *
  * A `Get[A]` is the read half of the mapping layer: it knows how to turn one column's `Value` into
  * an `A`. Leaf instances live here; `Read[A]` composes them across a whole row.
  */
trait Get[A]:
  def get(v: Value): A

  /** Decode as `A`, then transform — e.g. `Get[String].map(Color.valueOf)`. */
  final def map[B](f: A => B): Get[B] = (v: Value) => f(get(v))

object Get:
  inline def apply[A](using g: Get[A]): Get[A] = g

  given Get[Int]        = _.intValue
  given Get[Long]       = _.longValue
  given Get[Short]      = _.shortValue
  given Get[Byte]       = _.byteValue
  given Get[Double]     = _.doubleValue
  given Get[Float]      = _.floatValue
  given Get[BigDecimal] = v => BigDecimal(v.number.toString)
  given Get[String]     = _.string

  given Get[Boolean] =
    case BooleanValue(b) => b
    case v               => decodeFail(v, "boolean")

  given Get[LocalDate] =
    case DateValue(d) => d
    case v            => decodeFail(v, "date")

  given Get[LocalTime] =
    case TimeValue(t) => t
    case v            => decodeFail(v, "time")

  given Get[LocalDateTime] =
    case TimestampValue(t) => t
    case v                 => decodeFail(v, "timestamp")

  given Get[OffsetDateTime] =
    case TimestampTZValue(t) => t
    case v                   => decodeFail(v, "timestamptz")

  given Get[OffsetTime] =
    case TimeTZValue(t) => t
    case v              => decodeFail(v, "timetz")

  given Get[Duration] =
    case IntervalValue(d) => d
    case v                => decodeFail(v, "interval")

  given Get[Array[Byte]] =
    case ByteaValue(d) => d
    case v             => decodeFail(v, "bytea")

  given Get[UUID] =
    case UUIDValue(id) => UUID.fromString(id)
    case TextValue(s)  => UUID.fromString(s)
    case v             => decodeFail(v, "uuid")

  /** A nullable column: SQL `NULL` decodes to `None`, anything else to `Some(get)`. */
  given option[A](using g: Get[A]): Get[Option[A]] =
    v => if v.isNull then None else Some(g.get(v))
