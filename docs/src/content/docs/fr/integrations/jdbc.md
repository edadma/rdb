---
title: JDBC
description: Reference du pilote JDBC pour PetraDB.
---

PetraDB inclut un pilote JDBC 4 pour les applications JVM. Le pilote s'enregistre automatiquement via le mecanisme Java ServiceLoader. Il est distribue sous forme d'un fat jar unique sans dependances transitives.

## Installation

**Maven :**
```xml
<dependency>
    <groupId>io.github.edadma</groupId>
    <artifactId>petradb-jdbc</artifactId>
    <version>1.5.0</version>
</dependency>
```

**Gradle :**
```groovy
implementation 'io.github.edadma:petradb-jdbc:1.5.0'
```

**sbt :**
```scala
libraryDependencies += "io.github.edadma" % "petradb-jdbc" % "1.5.0"
```

Ou telechargez le jar directement depuis [Maven Central](https://central.sonatype.com/artifact/io.github.edadma/petradb-jdbc).

## Modes de connexion

### En memoire

```java
Connection conn = DriverManager.getConnection("jdbc:petradb:memory");
```

Chaque connexion cree une base de donnees en memoire isolee.

### Fichier

```java
Connection conn = DriverManager.getConnection("jdbc:petradb:file:/path/to/database.petra");
```

Ouvre une base de donnees existante ou en cree une nouvelle. Extensions de fichiers supportees :

| Extension | Backend |
|-----------|---------|
| `.petra` | Stockage persistant par pages |
| `.ptxt` | Stockage base sur du texte |
| autre | Stockage persistant par pages (par defaut) |

### Serveur

```java
Properties props = new Properties();
props.setProperty("user", "username");
props.setProperty("password", "password");

Connection conn = DriverManager.getConnection("jdbc:petradb://localhost:5480", props);
```

## Instructions

### Statement

```java
Statement stmt = conn.createStatement();

// Requete
ResultSet rs = stmt.executeQuery("SELECT * FROM users");

// Mise a jour
int rowCount = stmt.executeUpdate("INSERT INTO users (name) VALUES ('Alice')");

// Execution generale
boolean hasResults = stmt.execute("SELECT 1");
```

### PreparedStatement

Utilise les placeholders de parametres `?` :

```java
PreparedStatement ps = conn.prepareStatement(
    "SELECT * FROM users WHERE id = ? AND active = ?");
ps.setInt(1, 42);
ps.setBoolean(2, true);
ResultSet rs = ps.executeQuery();
```

Methodes setter supportees :

| Methode | Type SQL |
|---------|----------|
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
| `setObject(index, value)` | auto-detecte |

## Operations par lots

### Lot de Statement

```java
Statement stmt = conn.createStatement();
stmt.addBatch("INSERT INTO users (name) VALUES ('Alice')");
stmt.addBatch("INSERT INTO users (name) VALUES ('Bob')");
int[] counts = stmt.executeBatch();
```

### Lot de PreparedStatement

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

Jeux de resultats en lecture seule et en avant uniquement :

```java
ResultSet rs = stmt.executeQuery("SELECT id, name FROM users");
while (rs.next()) {
    int id = rs.getInt("id");
    String name = rs.getString("name");
    // rs.wasNull() verifie si la derniere valeur lue etait NULL
}
rs.close();
```

Acces aux colonnes par index (base 1) ou par nom :

```java
rs.getInt(1);          // par position
rs.getString("name");  // par label
```

## Transactions

L'auto-commit est active par defaut. Desactivez-le pour un controle explicite des transactions :

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

// Lister les tables
ResultSet tables = meta.getTables(null, null, null, null);
while (tables.next()) {
    String tableName = tables.getString("TABLE_NAME");
}

// Lister les colonnes
ResultSet cols = meta.getColumns(null, null, "users", null);
while (cols.next()) {
    String name = cols.getString("COLUMN_NAME");
    String type = cols.getString("TYPE_NAME");
    boolean nullable = cols.getInt("NULLABLE") == DatabaseMetaData.columnNullable;
}

// Cles primaires
ResultSet pks = meta.getPrimaryKeys(null, null, "users");

// Cles etrangeres
ResultSet fks = meta.getImportedKeys(null, null, "orders");

// Index
ResultSet idxs = meta.getIndexInfo(null, null, "users", false, false);
```

`getColumns()` retourne `COLUMN_SIZE`, `DECIMAL_DIGITS` et `CHAR_OCTET_LENGTH` bases sur le type et la precision/echelle de chaque colonne :

| Type de colonne | COLUMN_SIZE | DECIMAL_DIGITS | CHAR_OCTET_LENGTH |
|----------------|-------------|----------------|-------------------|
| `VARCHAR(n)` / `CHAR(n)` | n | null | n * 4 |
| `TEXT` | 2147483647 | null | 2147483647 |
| `INT` / `SERIAL` | 10 | 0 | null |
| `BIGINT` / `BIGSERIAL` | 19 | 0 | null |
| `NUMERIC(p,s)` | p | s | null |
| `BOOLEAN` | 1 | null | null |

## Mapping de types

| Type PetraDB | Type JDBC | Type Java |
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
