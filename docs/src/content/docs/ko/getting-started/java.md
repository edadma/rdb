---
title: Java 시작하기
description: JDBC를 통해 Java 프로젝트에 PetraDB를 추가하고 첫 번째 SQL 쿼리를 실행합니다.
---

PetraDB는 표준 JDBC 4 드라이버를 제공하므로, 익숙한 `java.sql` API를 사용합니다 — `Connection`, `Statement`, `ResultSet`, `PreparedStatement`.

## 설치

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

드라이버가 자동으로 등록됩니다 — `Class.forName()`이 필요 없습니다.

## 첫 번째 쿼리 실행

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

출력:
```
1: Alice <alice@example.com>
2: Bob <bob@example.com>
```

## 영구 스토리지

재시작 후에도 데이터를 유지하려면 파일 연결 URL을 사용합니다:

```java
Connection conn = DriverManager.getConnection("jdbc:petradb:file:mydata.petra");
```

파일은 처음 사용 시 생성됩니다. 지원되는 확장자:

| 확장자 | 스토리지 유형 |
|-----------|-------------|
| `.petra` | 충돌 안전 영구 스토리지 |
| `.ptxt` | 사람이 읽을 수 있는 텍스트 형식 |

## 준비된 구문

```java
PreparedStatement ps = conn.prepareStatement(
    "SELECT * FROM users WHERE name = ? AND id > ?");
ps.setString(1, "Alice");
ps.setInt(2, 0);
ResultSet rs = ps.executeQuery();
```

## 트랜잭션

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

## 다음 단계

배치 작업, 메타데이터, 타입 매핑, 서버 연결을 포함한 전체 API는 [JDBC 레퍼런스](/integrations/jdbc/)를 참고하세요.
