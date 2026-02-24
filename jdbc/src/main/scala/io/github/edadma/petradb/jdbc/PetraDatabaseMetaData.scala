package io.github.edadma.petradb.jdbc

class PetraDatabaseMetaData(conn: AbstractConnection) extends AbstractDatabaseMetaData:

  override def getDatabaseProductName(): String    = "PetraDB"
  override def getDatabaseProductVersion(): String = "1.1.0"
  override def getDriverName(): String             = "PetraDB JDBC Driver"
  override def getDriverVersion(): String          = "1.1.0"
  override def getDriverMajorVersion(): Int        = 1
  override def getDriverMinorVersion(): Int        = 0
  override def getDatabaseMajorVersion(): Int      = 1
  override def getDatabaseMinorVersion(): Int      = 1

  override def getURL(): String      = conn.url
  override def getUserName(): String = conn.username

  override def supportsTransactions(): Boolean = true
  override def supportsMinimumSQLGrammar(): Boolean = true

  override def getIdentifierQuoteString(): String = "\""
  override def getSearchStringEscape(): String    = "\\"

  override def getConnection(): java.sql.Connection = conn
