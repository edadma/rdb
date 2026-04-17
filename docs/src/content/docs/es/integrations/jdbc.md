---
title: JDBC
description: Referencia del driver JDBC para PetraDB.
---

PetraDB incluye un driver JDBC 4 para aplicaciones JVM. El driver se registra automaticamente mediante el mecanismo Java ServiceLoader. Se distribuye como un unico fat jar sin dependencias transitivas.

## Instalacion

**Maven:**
```xml
<dependency>
    <groupId>io.github.edadma</groupId>
    <artifactId>petradb-jdbc</artifactId>
    <version>1.5.2</version>
</dependency>
```

**Gradle:**
```groovy
implementation 'io.github.edadma:petradb-jdbc:1.5.2'
```

**sbt:**
```scala
libraryDependencies += "io.github.edadma" % "petradb-jdbc" % "1.5.2"
```

O descarga el jar directamente desde [Maven Central](https://central.sonatype.com/artifact/io.github.edadma/petradb-jdbc).

## Modos de conexion

### En memoria

```java
Connection conn = DriverManager.getConnection("jdbc:petradb:memory");
```

Cada conexion crea una base de datos en memoria aislada.

### Archivo

```java
Connection conn = DriverManager.getConnection("jdbc:petradb:file:/path/to/database.petra");
```

Abre una base de datos existente o crea una nueva. Extensiones de archivo soportadas:

| Extension | Backend |
|-----------|---------|
| `.petra` | Almacenamiento persistente de paginas |
| `.ptxt` | Almacenamiento basado en texto |
| otra | Almacenamiento persistente de paginas (por defecto) |

### Servidor

```java
Properties props = new Properties();
props.setProperty("user", "username");
props.setProperty("password", "password");

Connection conn = DriverManager.getConnection("jdbc:petradb://localhost:5480", props);
```

## Sentencias

### Statement

```java
Statement stmt = conn.createStatement();

// Consulta
ResultSet rs = stmt.executeQuery("SELECT * FROM users");

// Actualizacion
int rowCount = stmt.executeUpdate("INSERT INTO users (name) VALUES ('Alice')");

// Ejecucion general
boolean hasResults = stmt.execute("SELECT 1");
```

### PreparedStatement

Usa marcadores de parametro `?`:

```java
PreparedStatement ps = conn.prepareStatement(
    "SELECT * FROM users WHERE id = ? AND active = ?");
ps.setInt(1, 42);
ps.setBoolean(2, true);
ResultSet rs = ps.executeQuery();
```

Metodos setter soportados:

| Metodo | Tipo SQL |
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
| `setObject(index, value)` | auto-detectado |

## Operaciones por lotes

### Lote de Statement

```java
Statement stmt = conn.createStatement();
stmt.addBatch("INSERT INTO users (name) VALUES ('Alice')");
stmt.addBatch("INSERT INTO users (name) VALUES ('Bob')");
int[] counts = stmt.executeBatch();
```

### Lote de PreparedStatement

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

Conjuntos de resultados de solo avance y solo lectura:

```java
ResultSet rs = stmt.executeQuery("SELECT id, name FROM users");
while (rs.next()) {
    int id = rs.getInt("id");
    String name = rs.getString("name");
    // rs.wasNull() verifica si el ultimo valor leido fue NULL
}
rs.close();
```

Acceso a columnas por indice (base 1) o nombre:

```java
rs.getInt(1);          // por posicion
rs.getString("name");  // por etiqueta
```

## Transacciones

Auto-commit esta habilitado por defecto. Desactivalo para control de transacciones explicito:

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

// Listar tablas
ResultSet tables = meta.getTables(null, null, null, null);
while (tables.next()) {
    String tableName = tables.getString("TABLE_NAME");
}

// Listar columnas
ResultSet cols = meta.getColumns(null, null, "users", null);
while (cols.next()) {
    String name = cols.getString("COLUMN_NAME");
    String type = cols.getString("TYPE_NAME");
    boolean nullable = cols.getInt("NULLABLE") == DatabaseMetaData.columnNullable;
}

// Claves primarias
ResultSet pks = meta.getPrimaryKeys(null, null, "users");

// Claves foraneas
ResultSet fks = meta.getImportedKeys(null, null, "orders");

// Indices
ResultSet idxs = meta.getIndexInfo(null, null, "users", false, false);
```

`getColumns()` retorna `COLUMN_SIZE`, `DECIMAL_DIGITS` y `CHAR_OCTET_LENGTH` basados en el tipo y precision/escala de cada columna:

| Tipo de columna | COLUMN_SIZE | DECIMAL_DIGITS | CHAR_OCTET_LENGTH |
|------------|-------------|----------------|-------------------|
| `VARCHAR(n)` / `CHAR(n)` | n | null | n * 4 |
| `TEXT` | 2147483647 | null | 2147483647 |
| `INT` / `SERIAL` | 10 | 0 | null |
| `BIGINT` / `BIGSERIAL` | 19 | 0 | null |
| `NUMERIC(p,s)` | p | s | null |
| `BOOLEAN` | 1 | null | null |

## Mapeo de tipos

| Tipo PetraDB | Tipo JDBC | Tipo Java |
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
