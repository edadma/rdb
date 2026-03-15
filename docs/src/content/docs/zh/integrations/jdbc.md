---
title: JDBC
description: PetraDB 的 JDBC 驱动参考。
---

PetraDB 包含一个适用于 JVM 应用程序的 JDBC 4 驱动。该驱动通过 Java ServiceLoader 机制自动注册。它以单个 fat jar 发布，没有传递依赖。

## 安装

**Maven：**
```xml
<dependency>
    <groupId>io.github.edadma</groupId>
    <artifactId>petradb-jdbc</artifactId>
    <version>1.5.0</version>
</dependency>
```

**Gradle：**
```groovy
implementation 'io.github.edadma:petradb-jdbc:1.5.0'
```

**sbt：**
```scala
libraryDependencies += "io.github.edadma" % "petradb-jdbc" % "1.5.0"
```

或直接从 [Maven Central](https://central.sonatype.com/artifact/io.github.edadma/petradb-jdbc) 下载 jar 文件。

## 连接模式

### 内存

```java
Connection conn = DriverManager.getConnection("jdbc:petradb:memory");
```

每个连接创建一个隔离的内存数据库。

### 文件

```java
Connection conn = DriverManager.getConnection("jdbc:petradb:file:/path/to/database.petra");
```

打开现有数据库或创建新数据库。支持的文件扩展名：

| 扩展名 | 后端 |
|-----------|---------|
| `.petra` | 持久化页面存储 |
| `.ptxt` | 文本存储 |
| 其他 | 持久化页面存储（默认） |

### 服务器

```java
Properties props = new Properties();
props.setProperty("user", "username");
props.setProperty("password", "password");

Connection conn = DriverManager.getConnection("jdbc:petradb://localhost:5480", props);
```

## 语句

### Statement

```java
Statement stmt = conn.createStatement();

// 查询
ResultSet rs = stmt.executeQuery("SELECT * FROM users");

// 更新
int rowCount = stmt.executeUpdate("INSERT INTO users (name) VALUES ('Alice')");

// 通用执行
boolean hasResults = stmt.execute("SELECT 1");
```

### PreparedStatement

使用 `?` 参数占位符：

```java
PreparedStatement ps = conn.prepareStatement(
    "SELECT * FROM users WHERE id = ? AND active = ?");
ps.setInt(1, 42);
ps.setBoolean(2, true);
ResultSet rs = ps.executeQuery();
```

支持的设置方法：

| 方法 | SQL 类型 |
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
| `setObject(index, value)` | 自动检测 |

## 批量操作

### Statement 批量

```java
Statement stmt = conn.createStatement();
stmt.addBatch("INSERT INTO users (name) VALUES ('Alice')");
stmt.addBatch("INSERT INTO users (name) VALUES ('Bob')");
int[] counts = stmt.executeBatch();
```

### PreparedStatement 批量

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

只向前、只读的结果集：

```java
ResultSet rs = stmt.executeQuery("SELECT id, name FROM users");
while (rs.next()) {
    int id = rs.getInt("id");
    String name = rs.getString("name");
    // rs.wasNull() 检查最后读取的值是否为 NULL
}
rs.close();
```

按索引（从 1 开始）或名称访问列：

```java
rs.getInt(1);          // 按位置
rs.getString("name");  // 按标签
```

## 事务

默认启用自动提交。禁用它以进行显式事务控制：

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

// 列出表
ResultSet tables = meta.getTables(null, null, null, null);
while (tables.next()) {
    String tableName = tables.getString("TABLE_NAME");
}

// 列出列
ResultSet cols = meta.getColumns(null, null, "users", null);
while (cols.next()) {
    String name = cols.getString("COLUMN_NAME");
    String type = cols.getString("TYPE_NAME");
    boolean nullable = cols.getInt("NULLABLE") == DatabaseMetaData.columnNullable;
}

// 主键
ResultSet pks = meta.getPrimaryKeys(null, null, "users");

// 外键
ResultSet fks = meta.getImportedKeys(null, null, "orders");

// 索引
ResultSet idxs = meta.getIndexInfo(null, null, "users", false, false);
```

`getColumns()` 根据每列的类型和精度/标度返回 `COLUMN_SIZE`、`DECIMAL_DIGITS` 和 `CHAR_OCTET_LENGTH`：

| 列类型 | COLUMN_SIZE | DECIMAL_DIGITS | CHAR_OCTET_LENGTH |
|------------|-------------|----------------|-------------------|
| `VARCHAR(n)` / `CHAR(n)` | n | null | n * 4 |
| `TEXT` | 2147483647 | null | 2147483647 |
| `INT` / `SERIAL` | 10 | 0 | null |
| `BIGINT` / `BIGSERIAL` | 19 | 0 | null |
| `NUMERIC(p,s)` | p | s | null |
| `BOOLEAN` | 1 | null | null |

## 类型映射

| PetraDB 类型 | JDBC 类型 | Java 类型 |
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
