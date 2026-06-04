package io.github.edadma.petradb.chisel

import io.github.edadma.petradb.*

import scala.concurrent.{ExecutionContext, Future}

/** A SQL fragment paired with a row decoder, ready to run against a [[Session]].
  *
  * Rows are decoded with the table's authoritative [[Metadata]] (not each row's own), so by-name
  * [[Read]] decoding works regardless of how the engine populates per-row metadata.
  */
final class Query[A](frag: Fragment, read: Read[A]):

  /** All result rows, decoded. */
  def toList(using Session, ExecutionContext): Future[List[A]] = rows.map(_.toList)

  /** All result rows, decoded. */
  def toVector(using Session, ExecutionContext): Future[Vector[A]] = rows

  /** The first row, or `None` if the result is empty. */
  def option(using Session, ExecutionContext): Future[Option[A]] = rows.map(_.headOption)

  /** Exactly one row; fails with [[DecodeException]] for zero or multiple rows. */
  def unique(using Session, ExecutionContext): Future[A] =
    rows.map { rs =>
      if rs.length == 1 then rs.head
      else throw DecodeException(s"expected exactly one row, got ${rs.length}")
    }

  private def rows(using s: Session, ec: ExecutionContext): Future[Vector[A]] =
    s.execute(frag.sql, frag.params).map { results =>
      Query.tableOf(results) match
        case Some(t) => t.data.iterator.map(r => read.read(Query.withMeta(r, t.meta))).toVector
        case None    => Vector.empty
    }

object Query:
  /** The last table-bearing result — a SELECT's `QueryResult` or an INSERT…RETURNING's table. */
  private[chisel] def tableOf(rs: Seq[Result]): Option[TableValue] =
    rs.reverseIterator.collectFirst {
      case QueryResult(t)     => t
      case InsertResult(_, t) => t
    }

  private[chisel] def withMeta(r: Row, meta: Metadata): Row =
    if r.meta eq meta then r else r.copy(meta = meta)

extension (frag: Fragment)
  /** Attach a row decoder, producing a runnable [[Query]]. */
  def query[A](using read: Read[A]): Query[A] = new Query(frag, read)

  /** Run as a statement, returning the number of affected rows (INSERT/UPDATE/DELETE). */
  def update(using s: Session, ec: ExecutionContext): Future[Int] =
    s.execute(frag.sql, frag.params).map { rs =>
      rs.reverseIterator.collectFirst {
        case UpdateResult(n)    => n
        case DeleteResult(n)    => n
        case InsertResult(_, t) => t.data.length
      }.getOrElse(0)
    }

  /** Run for side effects, returning the raw engine results. */
  def run(using s: Session, ec: ExecutionContext): Future[Seq[Result]] =
    s.execute(frag.sql, frag.params)

  /** Decode the first column of the first row as a scalar — for `count(*)`, `exists`, `max`, etc.
    * Fails with [[DecodeException]] if the result has no rows.
    */
  def queryValue[A](using s: Session, ec: ExecutionContext, g: Get[A]): Future[A] =
    s.execute(frag.sql, frag.params).map { rs =>
      Query.tableOf(rs).filter(_.data.nonEmpty) match
        case Some(t) => g.get(t.data.head.data.head)
        case None    => throw DecodeException("expected at least one row with one column")
    }

  /** Decode the first column of the first row, or `None` if the result is empty. */
  def queryValueOption[A](using s: Session, ec: ExecutionContext, g: Get[A]): Future[Option[A]] =
    s.execute(frag.sql, frag.params).map { rs =>
      Query.tableOf(rs).filter(_.data.nonEmpty).map(t => g.get(t.data.head.data.head))
    }
