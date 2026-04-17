---
title: Getting Started with Java
description: Add PetraDB to your Java project via JDBC and run your first SQL queries.
---

PetraDB provides a standard JDBC 4 driver, so you use the familiar `java.sql` API — `Connection`, `Statement`, `ResultSet`, `PreparedStatement`.

## Install

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

The driver registers itself automatically — no `Class.forName()` needed.

## Run your first query

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

Output:
```
1: Alice <alice@example.com>
2: Bob <bob@example.com>
```

## Persistent storage

For data that survives restarts, use a file connection URL:

```java
Connection conn = DriverManager.getConnection("jdbc:petradb:file:mydata.petra");
```

The file is created on first use. Supported extensions:

| Extension | Storage type |
|-----------|-------------|
| `.petra` | Crash-safe persistent storage |
| `.ptxt` | Human-readable text format |

## Prepared statements

```java
PreparedStatement ps = conn.prepareStatement(
    "SELECT * FROM users WHERE name = ? AND id > ?");
ps.setString(1, "Alice");
ps.setInt(2, 0);
ResultSet rs = ps.executeQuery();
```

## Transactions

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

## Next steps

See the [JDBC reference](/integrations/jdbc/) for the complete API, including batch operations, metadata, type mapping, and server connections.
