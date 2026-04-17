---
title: Java ではじめる
description: JDBC経由でJavaプロジェクトにPetraDBを追加して、最初のSQLクエリを実行します。
---

PetraDBは標準的なJDBC 4ドライバーを提供しているので、おなじみの`java.sql` API — `Connection`、`Statement`、`ResultSet`、`PreparedStatement` — を使用できます。

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

ドライバーは自動登録されるため、`Class.forName()`は不要です。

## 最初のクエリを実行する

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

出力：
```
1: Alice <alice@example.com>
2: Bob <bob@example.com>
```

## 永続ストレージ

再起動後もデータを保持するには、ファイル接続URLを使用します。

```java
Connection conn = DriverManager.getConnection("jdbc:petradb:file:mydata.petra");
```

ファイルは初回使用時に作成されます。サポートされる拡張子：

| 拡張子 | ストレージタイプ |
|-----------|-------------|
| `.petra` | クラッシュセーフな永続ストレージ |
| `.ptxt` | 人間が読めるテキスト形式 |

## プリペアドステートメント

```java
PreparedStatement ps = conn.prepareStatement(
    "SELECT * FROM users WHERE name = ? AND id > ?");
ps.setString(1, "Alice");
ps.setInt(2, 0);
ResultSet rs = ps.executeQuery();
```

## トランザクション

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

## 次のステップ

バッチ操作、メタデータ、型マッピング、サーバー接続を含む完全なAPIについては、[JDBCリファレンス](/integrations/jdbc/)をご覧ください。
