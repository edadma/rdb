---
title: JDBC
description: PetraDBのJDBCドライバーリファレンスです。
---

PetraDBにはJVMアプリケーション用のJDBC 4ドライバーが含まれています。ドライバーはJava ServiceLoaderメカニズムにより自動登録されます。推移的な依存関係のない単一のファットjarとして配布されます。

## インストール

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

または[Maven Central](https://central.sonatype.com/artifact/io.github.edadma/petradb-jdbc)からjarを直接ダウンロードできます。

## 接続モード

### インメモリ

```java
Connection conn = DriverManager.getConnection("jdbc:petradb:memory");
```

各接続は独立したインメモリデータベースを作成します。

### ファイル

```java
Connection conn = DriverManager.getConnection("jdbc:petradb:file:/path/to/database.petra");
```

既存のデータベースを開くか、新しいデータベースを作成します。サポートされるファイル拡張子：

| 拡張子 | バックエンド |
|-----------|---------|
| `.petra` | 永続ページストア |
| `.ptxt` | テキストベースストア |
| その他 | 永続ページストア（デフォルト） |

### サーバー

```java
Properties props = new Properties();
props.setProperty("user", "username");
props.setProperty("password", "password");

Connection conn = DriverManager.getConnection("jdbc:petradb://localhost:5480", props);
```

## ステートメント

### Statement

```java
Statement stmt = conn.createStatement();

// クエリ
ResultSet rs = stmt.executeQuery("SELECT * FROM users");

// 更新
int rowCount = stmt.executeUpdate("INSERT INTO users (name) VALUES ('Alice')");

// 汎用実行
boolean hasResults = stmt.execute("SELECT 1");
```

### PreparedStatement

`?`パラメータプレースホルダーを使用します。

```java
PreparedStatement ps = conn.prepareStatement(
    "SELECT * FROM users WHERE id = ? AND active = ?");
ps.setInt(1, 42);
ps.setBoolean(2, true);
ResultSet rs = ps.executeQuery();
```

サポートされるsetterメソッド：

| メソッド | SQL型 |
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
| `setObject(index, value)` | 自動検出 |

## バッチ操作

### Statementバッチ

```java
Statement stmt = conn.createStatement();
stmt.addBatch("INSERT INTO users (name) VALUES ('Alice')");
stmt.addBatch("INSERT INTO users (name) VALUES ('Bob')");
int[] counts = stmt.executeBatch();
```

### PreparedStatementバッチ

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

前方のみ、読み取り専用のResultSetです。

```java
ResultSet rs = stmt.executeQuery("SELECT id, name FROM users");
while (rs.next()) {
    int id = rs.getInt("id");
    String name = rs.getString("name");
    // rs.wasNull()で最後に読み取った値がNULLかチェック
}
rs.close();
```

インデックス（1ベース）または名前でカラムにアクセスできます。

```java
rs.getInt(1);          // 位置で
rs.getString("name");  // ラベルで
```

## トランザクション

デフォルトでオートコミットが有効です。明示的なトランザクション制御には無効にします。

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

// テーブル一覧
ResultSet tables = meta.getTables(null, null, null, null);
while (tables.next()) {
    String tableName = tables.getString("TABLE_NAME");
}

// カラム一覧
ResultSet cols = meta.getColumns(null, null, "users", null);
while (cols.next()) {
    String name = cols.getString("COLUMN_NAME");
    String type = cols.getString("TYPE_NAME");
    boolean nullable = cols.getInt("NULLABLE") == DatabaseMetaData.columnNullable;
}

// 主キー
ResultSet pks = meta.getPrimaryKeys(null, null, "users");

// 外部キー
ResultSet fks = meta.getImportedKeys(null, null, "orders");

// インデックス
ResultSet idxs = meta.getIndexInfo(null, null, "users", false, false);
```

`getColumns()`は各カラムの型と精度/スケールに基づいて`COLUMN_SIZE`、`DECIMAL_DIGITS`、`CHAR_OCTET_LENGTH`を返します。

| カラム型 | COLUMN_SIZE | DECIMAL_DIGITS | CHAR_OCTET_LENGTH |
|------------|-------------|----------------|-------------------|
| `VARCHAR(n)` / `CHAR(n)` | n | null | n * 4 |
| `TEXT` | 2147483647 | null | 2147483647 |
| `INT` / `SERIAL` | 10 | 0 | null |
| `BIGINT` / `BIGSERIAL` | 19 | 0 | null |
| `NUMERIC(p,s)` | p | s | null |
| `BOOLEAN` | 1 | null | null |

## 型マッピング

| PetraDB型 | JDBC型 | Java型 |
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
