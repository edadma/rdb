package io.github.edadma.petradb.jdbc

import io.github.edadma.petradb.*

class PetraStatement(val conn: AbstractConnection) extends AbstractStatement:

  private var _closed       = false
  private var _resultSet: java.sql.ResultSet = null
  private var _updateCount  = -1

  override def getConnection(): java.sql.Connection = conn
  override def isClosed(): Boolean = _closed
  override def close(): Unit = { _closed = true }

  override def execute(sql: String): Boolean =
    val results = conn.execute(sql)
    processResults(results)

  override def executeQuery(sql: String): java.sql.ResultSet =
    conn.execute(sql).head match
      case QueryResult(table) => new PetraResultSet(table)
      case _ => throw java.sql.SQLException("query did not return a result set")

  override def executeUpdate(sql: String): Int =
    val results = conn.execute(sql)
    if results.isEmpty then 0
    else results.head match
      case UpdateResult(n)    => n
      case DeleteResult(n)    => n
      case InsertResult(_, _) => 1
      case _                  => 0

  override def getResultSet(): java.sql.ResultSet = _resultSet
  override def getUpdateCount(): Int = _updateCount

  protected def processResults(results: Seq[Result]): Boolean =
    if results.isEmpty then
      _resultSet = null
      _updateCount = -1
      false
    else results.head match
      case QueryResult(table) =>
        _resultSet = new PetraResultSet(table)
        _updateCount = -1
        true
      case InsertResult(_, _) =>
        _resultSet = null
        _updateCount = 1
        false
      case UpdateResult(n) =>
        _resultSet = null
        _updateCount = n
        false
      case DeleteResult(n) =>
        _resultSet = null
        _updateCount = n
        false
      case _ =>
        _resultSet = null
        _updateCount = 0
        false
