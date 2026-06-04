package io.github.edadma.petradb.chisel

import io.github.edadma.petradb.{Session, Value}

import scala.concurrent.{ExecutionContext, Future}

/** A typed repository over a single table: CRUD generated from the entity's [[Read]]/[[Write]]
  * instances, with `sql"…"` as the escape hatch for anything more complex.
  *
  * `A` is the entity type and `Id` the primary-key type. Column names come from the entity's field
  * labels (via [[Write]]); the table and id-column names are supplied here. All identifiers are
  * compile-time/trusted (never user input), so they are interpolated into SQL text directly while
  * every actual *value* is still bound as a parameter.
  *
  * @param generatedId when true (the default), `insert`/`insertReturning` omit the id column so the
  *                     database assigns it (e.g. `SERIAL`); the in-memory id field is ignored.
  */
final class Repo[A, Id](
    val table: String,
    val idColumn: String,
    val generatedId: Boolean,
)(using read: Read[A], write: Write[A], idPut: Put[Id]):

  /** The single row with this id, or `None`. */
  def findById(id: Id)(using Session, ExecutionContext): Future[Option[A]] =
    (Fragment.const(s"select * from $table where $idColumn = ") ++ Fragment.param(idPut.put(id)))
      .query[A].option

  /** Every row in the table. */
  def findAll(using Session, ExecutionContext): Future[List[A]] =
    Fragment.const(s"select * from $table").query[A].toList

  /** The number of rows in the table. */
  def count(using Session, ExecutionContext): Future[Long] =
    Fragment.const(s"select count(*) from $table").queryValue[Long]

  /** Whether a row with this id exists. */
  def existsById(id: Id)(using Session, ExecutionContext): Future[Boolean] =
    (Fragment.const(s"select 1 from $table where $idColumn = ") ++ Fragment.param(idPut.put(id)))
      .queryValueOption[Int].map(_.isDefined)

  /** Insert one entity, returning the number of rows inserted (1). */
  def insert(a: A)(using Session, ExecutionContext): Future[Int] =
    insertFragment(a).update

  /** Insert one entity and read it back (`RETURNING *`) — the way to recover a generated id. */
  def insertReturning(a: A)(using Session, ExecutionContext): Future[A] =
    (insertFragment(a) ++ Fragment.const(" returning *")).query[A].unique

  /** Update the row matching the entity's id, setting all other columns; returns rows affected. */
  def update(a: A)(using Session, ExecutionContext): Future[Int] =
    val (idPairs, setPairs) = write.writeNamed(a).partition(_._1 == idColumn)
    if setPairs.isEmpty || idPairs.isEmpty then Future.successful(0)
    else
      val sets = setPairs
        .map((c, v) => Fragment.const(s"$c = ") ++ Fragment.param(v))
        .reduceLeft(_ ++ Fragment.const(", ") ++ _)
      (Fragment.const(s"update $table set ") ++ sets ++
        Fragment.const(s" where $idColumn = ") ++ Fragment.param(idPairs.head._2)).update

  /** Delete the row with this id; returns rows affected. */
  def deleteById(id: Id)(using Session, ExecutionContext): Future[Int] =
    (Fragment.const(s"delete from $table where $idColumn = ") ++ Fragment.param(idPut.put(id))).update

  /** Delete every row; returns rows affected. */
  def deleteAll(using Session, ExecutionContext): Future[Int] =
    Fragment.const(s"delete from $table").update

  private def insertFragment(a: A): Fragment =
    val all  = write.writeNamed(a)
    val cols = if generatedId then all.filterNot(_._1 == idColumn) else all
    val names = cols.map(_._1).mkString(", ")
    Fragment.const(s"insert into $table ($names) values (") ++ commaParams(cols.map(_._2)) ++ Fragment.const(")")

  private def commaParams(values: Seq[Value]): Fragment =
    values.map(Fragment.param).reduceLeftOption(_ ++ Fragment.const(", ") ++ _).getOrElse(Fragment.empty)

object Repo:
  /** Build a repository. `generatedId` defaults to true (database-assigned `SERIAL`-style keys). */
  def apply[A, Id](table: String, idColumn: String = "id", generatedId: Boolean = true)(using
      Read[A],
      Write[A],
      Put[Id],
  ): Repo[A, Id] = new Repo(table, idColumn, generatedId)
