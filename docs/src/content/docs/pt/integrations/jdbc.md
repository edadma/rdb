---
title: JDBC
description: Referencia do driver JDBC para PetraDB.
---

O PetraDB inclui um driver JDBC 4 para aplicacoes JVM. O driver se registra automaticamente via mecanismo Java ServiceLoader. Ele e distribuido como um unico fat jar sem dependencias transitivas.

## Instalacao

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

Ou baixe o jar diretamente do [Maven Central](https://central.sonatype.com/artifact/io.github.edadma/petradb-jdbc).

## Modos de Conexao

### Em Memoria

```java
Connection conn = DriverManager.getConnection("jdbc:petradb:memory");
```

Cada conexao cria um banco de dados em memoria isolado.

### Arquivo

```java
Connection conn = DriverManager.getConnection("jdbc:petradb:file:/path/to/database.petra");
```

Abre um banco de dados existente ou cria um novo. Extensoes de arquivo suportadas:

| Extensao | Backend |
|-----------|---------|
| `.petra` | Armazenamento persistente de paginas |
| `.ptxt` | Armazenamento baseado em texto |
| outra | Armazenamento persistente de paginas (padrao) |

### Servidor

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

// Consulta
ResultSet rs = stmt.executeQuery("SELECT * FROM users");

// Atualizacao
int rowCount = stmt.executeUpdate("INSERT INTO users (name) VALUES ('Alice')");

// Execucao geral
boolean hasResults = stmt.execute("SELECT 1");
```

### PreparedStatement

Usa placeholders de parametro `?`:

```java
PreparedStatement ps = conn.prepareStatement(
    "SELECT * FROM users WHERE id = ? AND active = ?");
ps.setInt(1, 42);
ps.setBoolean(2, true);
ResultSet rs = ps.executeQuery();
```

Metodos setter suportados:

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

## Operacoes em Lote

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

Conjuntos de resultados somente para frente e somente leitura:

```java
ResultSet rs = stmt.executeQuery("SELECT id, name FROM users");
while (rs.next()) {
    int id = rs.getInt("id");
    String name = rs.getString("name");
    // rs.wasNull() verifica se o ultimo valor lido era NULL
}
rs.close();
```

Acesso a coluna por indice (base 1) ou nome:

```java
rs.getInt(1);          // por posicao
rs.getString("name");  // por rotulo
```

## Transacoes

Auto-commit esta habilitado por padrao. Desabilite-o para controle explicito de transacao:

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

// Listar tabelas
ResultSet tables = meta.getTables(null, null, null, null);
while (tables.next()) {
    String tableName = tables.getString("TABLE_NAME");
}

// Listar colunas
ResultSet cols = meta.getColumns(null, null, "users", null);
while (cols.next()) {
    String name = cols.getString("COLUMN_NAME");
    String type = cols.getString("TYPE_NAME");
    boolean nullable = cols.getInt("NULLABLE") == DatabaseMetaData.columnNullable;
}

// Chaves primarias
ResultSet pks = meta.getPrimaryKeys(null, null, "users");

// Chaves estrangeiras
ResultSet fks = meta.getImportedKeys(null, null, "orders");

// Indices
ResultSet idxs = meta.getIndexInfo(null, null, "users", false, false);
```

`getColumns()` retorna `COLUMN_SIZE`, `DECIMAL_DIGITS` e `CHAR_OCTET_LENGTH` baseados no tipo e precisao/escala de cada coluna:

| Tipo da Coluna | COLUMN_SIZE | DECIMAL_DIGITS | CHAR_OCTET_LENGTH |
|------------|-------------|----------------|-------------------|
| `VARCHAR(n)` / `CHAR(n)` | n | null | n * 4 |
| `TEXT` | 2147483647 | null | 2147483647 |
| `INT` / `SERIAL` | 10 | 0 | null |
| `BIGINT` / `BIGSERIAL` | 19 | 0 | null |
| `NUMERIC(p,s)` | p | s | null |
| `BOOLEAN` | 1 | null | null |

## Mapeamento de Tipos

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
