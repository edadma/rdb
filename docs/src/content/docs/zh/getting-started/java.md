---
title: Java 入门
description: 通过 JDBC 将 PetraDB 添加到你的 Java 项目并运行第一个 SQL 查询。
---

PetraDB 提供标准的 JDBC 4 驱动，因此你可以使用熟悉的 `java.sql` API — `Connection`、`Statement`、`ResultSet`、`PreparedStatement`。

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

驱动会自动注册 — 无需 `Class.forName()`。

## 运行你的第一个查询

```java
import java.sql.*;

public class Main {
    public static void main(String[] args) throws SQLException {
        Connection conn = DriverManager.getConnection("jdbc:petradb:memory");
        Statement stmt = conn.createStatement();

        stmt.executeUpdate("""
            CREATE TABLE users (
                id SERIAL PRIMARY KEY,
                name TEXT NOT NULL,
                email TEXT
            )
        """);

        stmt.executeUpdate("INSERT INTO users (name, email) VALUES ('Alice', 'alice@example.com')");
        stmt.executeUpdate("INSERT INTO users (name, email) VALUES ('Bob', 'bob@example.com')");

        ResultSet rs = stmt.executeQuery("SELECT id, name, email FROM users ORDER BY id");
        while (rs.next()) {
            System.out.printf("%d: %s <%s>%n",
                rs.getInt("id"),
                rs.getString("name"),
                rs.getString("email"));
        }

        rs.close();
        stmt.close();
        conn.close();
    }
}
```

输出：
```
1: Alice <alice@example.com>
2: Bob <bob@example.com>
```

## 持久化存储

对于需要在重启后保留的数据，使用文件连接 URL：

```java
Connection conn = DriverManager.getConnection("jdbc:petradb:file:mydata.petra");
```

文件在首次使用时创建。支持的扩展名：

| 扩展名 | 存储类型 |
|-----------|-------------|
| `.petra` | 防崩溃持久化存储 |
| `.ptxt` | 人类可读文本格式 |

## 预处理语句

```java
PreparedStatement ps = conn.prepareStatement(
    "SELECT * FROM users WHERE name = ? AND id > ?");
ps.setString(1, "Alice");
ps.setInt(2, 0);
ResultSet rs = ps.executeQuery();
```

## 事务

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

## 下一步

查看 [JDBC 参考](/integrations/jdbc/)了解完整 API，包括批量操作、元数据、类型映射和服务器连接。
