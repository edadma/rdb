package io.github.edadma.petradb.jdbc

import io.github.edadma.petradb.{TableValue, Metadata}

import java.sql.{SQLFeatureNotSupportedException, RowIdLifetime}

abstract class AbstractDatabaseMetaData extends java.sql.DatabaseMetaData:

  protected def emptyRS: java.sql.ResultSet =
    new PetraResultSet(TableValue(Vector.empty, Metadata(Vector.empty)))

  // ── Product / Driver info (override in concrete class) ──────────────
  def getDatabaseProductName(): String    = ""
  def getDatabaseProductVersion(): String = ""
  def getDriverName(): String             = ""
  def getDriverVersion(): String          = ""
  def getDriverMajorVersion(): Int        = 0
  def getDriverMinorVersion(): Int        = 0
  def getDatabaseMajorVersion(): Int      = 1
  def getDatabaseMinorVersion(): Int      = 1
  def getJDBCMajorVersion(): Int          = 4
  def getJDBCMinorVersion(): Int          = 0

  // ── User / URL (override in concrete class) ─────────────────────────
  def getURL(): String      = ""
  def getUserName(): String = ""

  // ── Boolean feature flags (all false by default) ────────────────────
  def allProceduresAreCallable(): Boolean = false
  def allTablesAreSelectable(): Boolean   = false
  def isReadOnly(): Boolean               = false
  def nullsAreSortedHigh(): Boolean       = false
  def nullsAreSortedLow(): Boolean        = false
  def nullsAreSortedAtStart(): Boolean    = false
  def nullsAreSortedAtEnd(): Boolean      = false
  def usesLocalFiles(): Boolean           = false
  def usesLocalFilePerTable(): Boolean    = false
  def supportsMixedCaseIdentifiers(): Boolean   = false
  def storesUpperCaseIdentifiers(): Boolean     = false
  def storesLowerCaseIdentifiers(): Boolean     = false
  def storesMixedCaseIdentifiers(): Boolean     = false
  def supportsMixedCaseQuotedIdentifiers(): Boolean = false
  def storesUpperCaseQuotedIdentifiers(): Boolean   = false
  def storesLowerCaseQuotedIdentifiers(): Boolean   = false
  def storesMixedCaseQuotedIdentifiers(): Boolean   = false
  def supportsAlterTableWithAddColumn(): Boolean    = false
  def supportsAlterTableWithDropColumn(): Boolean   = false
  def supportsColumnAliasing(): Boolean             = false
  def nullPlusNonNullIsNull(): Boolean              = false
  def supportsConvert(): Boolean                    = false
  def supportsConvert(fromType: Int, toType: Int): Boolean = false
  def supportsTableCorrelationNames(): Boolean      = false
  def supportsDifferentTableCorrelationNames(): Boolean = false
  def supportsExpressionsInOrderBy(): Boolean       = false
  def supportsOrderByUnrelated(): Boolean           = false
  def supportsGroupBy(): Boolean                    = false
  def supportsGroupByUnrelated(): Boolean           = false
  def supportsGroupByBeyondSelect(): Boolean        = false
  def supportsLikeEscapeClause(): Boolean           = false
  def supportsMultipleResultSets(): Boolean         = false
  def supportsMultipleTransactions(): Boolean       = false
  def supportsNonNullableColumns(): Boolean         = false
  def supportsMinimumSQLGrammar(): Boolean          = true
  def supportsCoreSQLGrammar(): Boolean             = false
  def supportsExtendedSQLGrammar(): Boolean         = false
  def supportsANSI92EntryLevelSQL(): Boolean        = false
  def supportsANSI92IntermediateSQL(): Boolean      = false
  def supportsANSI92FullSQL(): Boolean              = false
  def supportsIntegrityEnhancementFacility(): Boolean = false
  def supportsOuterJoins(): Boolean                 = false
  def supportsFullOuterJoins(): Boolean             = false
  def supportsLimitedOuterJoins(): Boolean          = false
  def isCatalogAtStart(): Boolean                   = false
  def supportsSchemasInDataManipulation(): Boolean  = false
  def supportsSchemasInProcedureCalls(): Boolean    = false
  def supportsSchemasInTableDefinitions(): Boolean  = false
  def supportsSchemasInIndexDefinitions(): Boolean  = false
  def supportsSchemasInPrivilegeDefinitions(): Boolean = false
  def supportsCatalogsInDataManipulation(): Boolean    = false
  def supportsCatalogsInProcedureCalls(): Boolean      = false
  def supportsCatalogsInTableDefinitions(): Boolean    = false
  def supportsCatalogsInIndexDefinitions(): Boolean    = false
  def supportsCatalogsInPrivilegeDefinitions(): Boolean = false
  def supportsPositionedDelete(): Boolean           = false
  def supportsPositionedUpdate(): Boolean           = false
  def supportsSelectForUpdate(): Boolean            = false
  def supportsStoredProcedures(): Boolean           = false
  def supportsSubqueriesInComparisons(): Boolean    = false
  def supportsSubqueriesInExists(): Boolean         = false
  def supportsSubqueriesInIns(): Boolean            = false
  def supportsSubqueriesInQuantifieds(): Boolean    = false
  def supportsCorrelatedSubqueries(): Boolean       = false
  def supportsUnion(): Boolean                      = false
  def supportsUnionAll(): Boolean                   = false
  def supportsOpenCursorsAcrossCommit(): Boolean    = false
  def supportsOpenCursorsAcrossRollback(): Boolean  = false
  def supportsOpenStatementsAcrossCommit(): Boolean = false
  def supportsOpenStatementsAcrossRollback(): Boolean = false
  def doesMaxRowSizeIncludeBlobs(): Boolean         = false
  def supportsTransactions(): Boolean               = false
  def supportsTransactionIsolationLevel(level: Int): Boolean = false
  def supportsDataDefinitionAndDataManipulationTransactions(): Boolean = false
  def supportsDataManipulationTransactionsOnly(): Boolean              = false
  def dataDefinitionCausesTransactionCommit(): Boolean                 = false
  def dataDefinitionIgnoredInTransactions(): Boolean                   = false
  def supportsResultSetType(`type`: Int): Boolean                      = false
  def supportsResultSetConcurrency(`type`: Int, concurrency: Int): Boolean = false
  def ownUpdatesAreVisible(`type`: Int): Boolean   = false
  def ownDeletesAreVisible(`type`: Int): Boolean   = false
  def ownInsertsAreVisible(`type`: Int): Boolean   = false
  def othersUpdatesAreVisible(`type`: Int): Boolean = false
  def othersDeletesAreVisible(`type`: Int): Boolean = false
  def othersInsertsAreVisible(`type`: Int): Boolean = false
  def updatesAreDetected(`type`: Int): Boolean     = false
  def deletesAreDetected(`type`: Int): Boolean     = false
  def insertsAreDetected(`type`: Int): Boolean     = false
  def supportsBatchUpdates(): Boolean              = false
  def supportsSavepoints(): Boolean                = false
  def supportsNamedParameters(): Boolean           = false
  def supportsMultipleOpenResults(): Boolean       = false
  def supportsGetGeneratedKeys(): Boolean          = false
  def supportsResultSetHoldability(holdability: Int): Boolean = false
  def locatorsUpdateCopy(): Boolean                = false
  def supportsStatementPooling(): Boolean          = false
  def supportsStoredFunctionsUsingCallSyntax(): Boolean = false
  def autoCommitFailureClosesAllResultSets(): Boolean   = false
  def generatedKeyAlwaysReturned(): Boolean        = false

  // ── Int limits (all 0 by default) ───────────────────────────────────
  def getMaxBinaryLiteralLength(): Int   = 0
  def getMaxCharLiteralLength(): Int     = 0
  def getMaxColumnNameLength(): Int      = 0
  def getMaxColumnsInGroupBy(): Int      = 0
  def getMaxColumnsInIndex(): Int        = 0
  def getMaxColumnsInOrderBy(): Int      = 0
  def getMaxColumnsInSelect(): Int       = 0
  def getMaxColumnsInTable(): Int        = 0
  def getMaxConnections(): Int           = 0
  def getMaxCursorNameLength(): Int      = 0
  def getMaxIndexLength(): Int           = 0
  def getMaxSchemaNameLength(): Int      = 0
  def getMaxProcedureNameLength(): Int   = 0
  def getMaxCatalogNameLength(): Int     = 0
  def getMaxRowSize(): Int               = 0
  def getMaxStatementLength(): Int       = 0
  def getMaxStatements(): Int            = 0
  def getMaxTableNameLength(): Int       = 0
  def getMaxTablesInSelect(): Int        = 0
  def getMaxUserNameLength(): Int        = 0
  def getDefaultTransactionIsolation(): Int = java.sql.Connection.TRANSACTION_READ_COMMITTED
  def getResultSetHoldability(): Int     = java.sql.ResultSet.CLOSE_CURSORS_AT_COMMIT
  def getSQLStateType(): Int             = java.sql.DatabaseMetaData.sqlStateSQL

  // ── String info (empty by default) ──────────────────────────────────
  def getIdentifierQuoteString(): String = "\""
  def getSQLKeywords(): String           = ""
  def getNumericFunctions(): String      = ""
  def getStringFunctions(): String       = ""
  def getSystemFunctions(): String       = ""
  def getTimeDateFunctions(): String     = ""
  def getSearchStringEscape(): String    = "\\"
  def getExtraNameCharacters(): String   = ""
  def getSchemaTerm(): String            = ""
  def getProcedureTerm(): String         = ""
  def getCatalogTerm(): String           = ""
  def getCatalogSeparator(): String      = ""

  // ── RowId lifetime ──────────────────────────────────────────────────
  def getRowIdLifetime(): RowIdLifetime  = RowIdLifetime.ROWID_UNSUPPORTED

  // ── Schema result sets (all empty by default) ───────────────────────
  def getProcedures(catalog: String, schemaPattern: String, procedureNamePattern: String): java.sql.ResultSet = emptyRS
  def getProcedureColumns(catalog: String, schemaPattern: String, procedureNamePattern: String, columnNamePattern: String): java.sql.ResultSet = emptyRS
  def getTables(catalog: String, schemaPattern: String, tableNamePattern: String, types: Array[String]): java.sql.ResultSet = emptyRS
  def getSchemas(): java.sql.ResultSet = emptyRS
  def getSchemas(catalog: String, schemaPattern: String): java.sql.ResultSet = emptyRS
  def getCatalogs(): java.sql.ResultSet = emptyRS
  def getTableTypes(): java.sql.ResultSet = emptyRS
  def getColumns(catalog: String, schemaPattern: String, tableNamePattern: String, columnNamePattern: String): java.sql.ResultSet = emptyRS
  def getColumnPrivileges(catalog: String, schema: String, table: String, columnNamePattern: String): java.sql.ResultSet = emptyRS
  def getTablePrivileges(catalog: String, schemaPattern: String, tableNamePattern: String): java.sql.ResultSet = emptyRS
  def getBestRowIdentifier(catalog: String, schema: String, table: String, scope: Int, nullable: Boolean): java.sql.ResultSet = emptyRS
  def getVersionColumns(catalog: String, schema: String, table: String): java.sql.ResultSet = emptyRS
  def getPrimaryKeys(catalog: String, schema: String, table: String): java.sql.ResultSet = emptyRS
  def getImportedKeys(catalog: String, schema: String, table: String): java.sql.ResultSet = emptyRS
  def getExportedKeys(catalog: String, schema: String, table: String): java.sql.ResultSet = emptyRS
  def getCrossReference(parentCatalog: String, parentSchema: String, parentTable: String, foreignCatalog: String, foreignSchema: String, foreignTable: String): java.sql.ResultSet = emptyRS
  def getTypeInfo(): java.sql.ResultSet = emptyRS
  def getIndexInfo(catalog: String, schema: String, table: String, unique: Boolean, approximate: Boolean): java.sql.ResultSet = emptyRS
  def getUDTs(catalog: String, schemaPattern: String, typeNamePattern: String, types: Array[Int]): java.sql.ResultSet = emptyRS
  def getSuperTypes(catalog: String, schemaPattern: String, typeNamePattern: String): java.sql.ResultSet = emptyRS
  def getSuperTables(catalog: String, schemaPattern: String, tableNamePattern: String): java.sql.ResultSet = emptyRS
  def getAttributes(catalog: String, schemaPattern: String, typeNamePattern: String, attributeNamePattern: String): java.sql.ResultSet = emptyRS
  def getClientInfoProperties(): java.sql.ResultSet = emptyRS
  def getFunctions(catalog: String, schemaPattern: String, functionNamePattern: String): java.sql.ResultSet = emptyRS
  def getFunctionColumns(catalog: String, schemaPattern: String, functionNamePattern: String, columnNamePattern: String): java.sql.ResultSet = emptyRS
  def getPseudoColumns(catalog: String, schemaPattern: String, tableNamePattern: String, columnNamePattern: String): java.sql.ResultSet = emptyRS

  // ── Connection link ─────────────────────────────────────────────────
  def getConnection(): java.sql.Connection = null

  // ── Wrapper ─────────────────────────────────────────────────────────
  def unwrap[T](iface: Class[T]): T       = throw java.sql.SQLException("not a wrapper")
  def isWrapperFor(iface: Class[?]): Boolean = false
