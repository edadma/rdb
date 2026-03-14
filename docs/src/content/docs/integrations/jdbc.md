---
title: JDBC
description: JDBC driver reference for PetraDB.
---

PetraDB includes a JDBC 4 driver for JVM applications. The driver registers itself automatically via the Java ServiceLoader mechanism. It ships as a single fat jar with no transitive dependencies.

## Installation

**Maven:**
```xml
<dependency>
    <groupId>io.github.edadma</groupId>
    <artifactId>petradb-jdbc</artifactId>
    <version>1.4.3</version>
</dependency>
```

**Gradle:**
```groovy
implementation 'io.github.edadma:petradb-jdbc:1.4.3'
```

**sbt:**
```scala
libraryDependencies += "io.github.edadma" % "petradb-jdbc" % "1.4.3"
```

Or download the jar directly from [Maven Central](https://central.sonatype.com/artifact/io.github.edadma/petradb-jdbc).

## Connection Modes

### In-Memory

```java
Connection conn = DriverManager.getConnection("jdbc:petradb:memory");
```

Each connection creates an isolated in-memory database.

### File

```java
Connection conn = DriverManager.getConnection("jdbc:petradb:file:/path/to/database.petra");
```

Opens an existing database or creates a new one. Supported file extensions:

| Extension | Backend |
|-----------|---------|
| `.petra` | Persistent page store |
| `.ptxt` | Text-based store |
| other | Persistent page store (default) |

### Server

```java
Properties props = new Properties();
props.setProperty("user", "username");
props.setProperty("password", "password");

Connection conn = DriverManager.getConnection("jdbc:petradb://localhost:5480", props);
```

## Statements

### Statement

```java
Statement stmt = conn.createStatement();

// Query
ResultSet rs = stmt.executeQuery("SELECT * FROM users");

// Update
int rowCount = stmt.executeUpdate("INSERT INTO users (name) VALUES ('Alice')");

// General execution
boolean hasResults = stmt.execute("SELECT 1");
```

### PreparedStatement

Uses `?` parameter placeholders:

```java
PreparedStatement ps = conn.prepareStatement(
    "SELECT * FROM users WHERE id = ? AND active = ?");
ps.setInt(1, 42);
ps.setBoolean(2, true);
ResultSet rs = ps.executeQuery();
```

Supported setter methods:

| Method | SQL Type |
|--------|----------|
| `setInt(index, value)` | INT |
| `setLong(index, value)` | BIGINT |
| `setFloat(index, value)` | FLOAT |
| `setDouble(index, value)` | DOUBLE |
| `setBigDecimal(index, value)` | NUMERIC |
| `setString(index, value)` | TEXT |
| `setBoolean(index, value)` | BOOLEAN |
| `setDate(index, value)` | DATE |
| `setTimestamp(index, value)` | TIMESTAMP |
| `setNull(index, sqlType)` | NULL |
| `setObject(index, value)` | auto-detected |

## Batch Operations

### Statement Batch

```java
Statement stmt = conn.createStatement();
stmt.addBatch("INSERT INTO users (name) VALUES ('Alice')");
stmt.addBatch("INSERT INTO users (name) VALUES ('Bob')");
int[] counts = stmt.executeBatch();
```

### PreparedStatement Batch

```java
PreparedStatement ps = conn.prepareStatement(
    "INSERT INTO users (name, email) VALUES (?, ?)");

ps.setString(1, "Alice");
ps.setString(2, "alice@example.com");
ps.addBatch();

ps.setString(1, "Bob");
ps.setString(2, "bob@example.com");
ps.addBatch();

int[] counts = ps.executeBatch();
```

## ResultSet

Forward-only, read-only result sets:

```java
ResultSet rs = stmt.executeQuery("SELECT id, name FROM users");
while (rs.next()) {
    int id = rs.getInt("id");
    String name = rs.getString("name");
    // rs.wasNull() checks if last value read was NULL
}
rs.close();
```

Column access by index (1-based) or name:

```java
rs.getInt(1);          // by position
rs.getString("name");  // by label
```

## Transactions

Auto-commit is enabled by default. Disable it for explicit transaction control:

```java
conn.setAutoCommit(false);
try {
    stmt.executeUpdate("UPDATE accounts SET balance = balance - 100 WHERE id = 1");
    stmt.executeUpdate("UPDATE accounts SET balance = balance + 100 WHERE id = 2");
    conn.commit();
} catch (SQLException e) {
    conn.rollback();
}
```

## DatabaseMetaData

```java
DatabaseMetaData meta = conn.getMetaData();

// List tables
ResultSet tables = meta.getTables(null, null, null, null);
while (tables.next()) {
    String tableName = tables.getString("TABLE_NAME");
}

// List columns
ResultSet cols = meta.getColumns(null, null, "users", null);
while (cols.next()) {
    String name = cols.getString("COLUMN_NAME");
    String type = cols.getString("TYPE_NAME");
    boolean nullable = cols.getInt("NULLABLE") == DatabaseMetaData.columnNullable;
}

// Primary keys
ResultSet pks = meta.getPrimaryKeys(null, null, "users");

// Foreign keys
ResultSet fks = meta.getImportedKeys(null, null, "orders");

// Indexes
ResultSet idxs = meta.getIndexInfo(null, null, "users", false, false);
```

`getColumns()` returns `COLUMN_SIZE`, `DECIMAL_DIGITS`, and `CHAR_OCTET_LENGTH` based on each column's type and precision/scale:

| Column Type | COLUMN_SIZE | DECIMAL_DIGITS | CHAR_OCTET_LENGTH |
|------------|-------------|----------------|-------------------|
| `VARCHAR(n)` / `CHAR(n)` | n | null | n * 4 |
| `TEXT` | 2147483647 | null | 2147483647 |
| `INT` / `SERIAL` | 10 | 0 | null |
| `BIGINT` / `BIGSERIAL` | 19 | 0 | null |
| `NUMERIC(p,s)` | p | s | null |
| `BOOLEAN` | 1 | null | null |

## Type Mapping

| PetraDB Type | JDBC Type | Java Type |
|-------------|-----------|-----------|
| INT / SERIAL | `Types.INTEGER` | `Integer` |
| BIGINT / BIGSERIAL | `Types.BIGINT` | `Long` |
| DOUBLE | `Types.DOUBLE` | `Double` |
| NUMERIC | `Types.NUMERIC` | `BigDecimal` |
| TEXT / VARCHAR / CHAR | `Types.VARCHAR` | `String` |
| BOOLEAN | `Types.BOOLEAN` | `Boolean` |
| DATE | `Types.DATE` | `java.sql.Date` |
| TIMESTAMP | `Types.TIMESTAMP` | `java.sql.Timestamp` |
| UUID / ENUM | `Types.VARCHAR` | `String` |
