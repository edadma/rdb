package io.github.edadma.rdb

import scala.collection.mutable

class Session(val db: DB):
  private var _inTransaction: Boolean = false
  private var _aborted: Boolean = false
  val preparedStatements: mutable.Map[String, PreparedStatement] = mutable.Map.empty

  def inTransaction: Boolean = _inTransaction
  def isTransactionAborted: Boolean = _aborted
  def markTransactionAborted(): Unit = _aborted = true

  def beginTransaction(): Unit =
    if _inTransaction then sys.error("already in a transaction")
    _inTransaction = true
    _aborted = false
    db.snapshot()

  def commitTransaction(): Unit =
    if !_inTransaction then sys.error("no active transaction")
    if _aborted then sys.error("current transaction is aborted, use ROLLBACK")
    db.commitSnapshot()
    _inTransaction = false
    _aborted = false

  def rollbackTransaction(): Unit =
    if !_inTransaction then sys.error("no active transaction")
    db.rollbackSnapshot()
    _inTransaction = false
    _aborted = false

  def prepare(sql: String): PreparedStatement =
    val cmds = SQLParser.parseCommands(sql)
    val name = s"_auto_${preparedStatements.size}"
    val ps = PreparedStatement(name, cmds)
    preparedStatements(name) = ps
    ps
