package io.github.edadma.petradb

import scala.collection.mutable

class Session(val db: DB):
  private var _inTransaction: Boolean = false
  private var _aborted: Boolean = false
  private var txnHandle: Option[TransactionHandle] = None
  val preparedStatements: mutable.Map[String, PreparedStatement] = mutable.Map.empty

  // Temp table support — session-scoped, always in-memory
  private[petradb] val tempTables = new mutable.HashMap[String, Table]
  private[petradb] lazy val tempDB = new MemoryDB

  def getTable(name: String): Option[Table] =
    tempTables.get(name).orElse(db.getTable(name))

  def hasTable(name: String): Boolean =
    tempTables.contains(name) || db.hasTable(name)

  def hasTempTable(name: String): Boolean = tempTables.contains(name)

  def inTransaction: Boolean = _inTransaction
  def isTransactionAborted: Boolean = _aborted
  def markTransactionAborted(): Unit = _aborted = true

  def beginTransaction(): Unit =
    if _inTransaction then sys.error("already in a transaction")
    _inTransaction = true
    _aborted = false
    txnHandle = Some(db.snapshot())

  def commitTransaction(): Unit =
    if !_inTransaction then sys.error("no active transaction")
    if _aborted then sys.error("current transaction is aborted, use ROLLBACK")
    db.commitSnapshot(txnHandle.get)
    txnHandle = None
    _inTransaction = false
    _aborted = false

  def rollbackTransaction(): Unit =
    if !_inTransaction then sys.error("no active transaction")
    db.rollbackSnapshot(txnHandle.get)
    txnHandle = None
    _inTransaction = false
    _aborted = false

  private[petradb] def activateHandle(): Unit =
    txnHandle.foreach(h => db.activateHandle(h))

  private[petradb] def deactivateHandle(): Unit =
    txnHandle.foreach(_ => db.deactivateHandle())

  def prepare(sql: String): PreparedStatement =
    val cmds = SQLParser.parseCommands(sql)
    val name = s"_auto_${preparedStatements.size}"
    val ps = PreparedStatement(name, cmds)
    preparedStatements(name) = ps
    ps
