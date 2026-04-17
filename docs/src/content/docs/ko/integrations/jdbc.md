---
title: JDBC
description: PetraDB용 JDBC 드라이버 레퍼런스.
---

PetraDB에는 JVM 애플리케이션용 JDBC 4 드라이버가 포함되어 있습니다. 드라이버는 Java ServiceLoader 메커니즘을 통해 자동으로 등록됩니다. 전이적 의존성이 없는 단일 fat jar로 제공됩니다.

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

**sbt:**
```scala
libraryDependencies += "io.github.edadma" % "petradb-jdbc" % "1.5.2"
```

또는 [Maven Central](https://central.sonatype.com/artifact/io.github.edadma/petradb-jdbc)에서 직접 jar를 다운로드합니다.

## 연결 모드

### 인메모리

```java
Connection conn = DriverManager.getConnection("jdbc:petradb:memory");
```

각 연결은 격리된 인메모리 데이터베이스를 생성합니다.

### 파일

```java
Connection conn = DriverManager.getConnection("jdbc:petradb:file:/path/to/database.petra");
```

기존 데이터베이스를 열거나 새로 생성합니다. 지원되는 파일 확장자:

| 확장자 | 백엔드 |
|-----------|---------|
| `.petra` | 영구 페이지 저장소 |
| `.ptxt` | 텍스트 기반 저장소 |
| 기타 | 영구 페이지 저장소 (기본값) |

### 서버

```java
Properties props = new Properties();
props.setProperty("user", "username");
props.setProperty("password", "password");

Connection conn = DriverManager.getConnection("jdbc:petradb://localhost:5480", props);
```

## 구문

### Statement

```java
Statement stmt = conn.createStatement();

// 쿼리
ResultSet rs = stmt.executeQuery("SELECT * FROM users");

// 수정
int rowCount = stmt.executeUpdate("INSERT INTO users (name) VALUES ('Alice')");

// 일반 실행
boolean hasResults = stmt.execute("SELECT 1");
```

### PreparedStatement

`?` 매개변수 플레이스홀더를 사용합니다:

```java
PreparedStatement ps = conn.prepareStatement(
    "SELECT * FROM users WHERE id = ? AND active = ?");
ps.setInt(1, 42);
ps.setBoolean(2, true);
ResultSet rs = ps.executeQuery();
```

지원되는 setter 메서드:

| 메서드 | SQL 타입 |
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
| `setObject(index, value)` | 자동 감지 |

## 배치 작업

### Statement 배치

```java
Statement stmt = conn.createStatement();
stmt.addBatch("INSERT INTO users (name) VALUES ('Alice')");
stmt.addBatch("INSERT INTO users (name) VALUES ('Bob')");
int[] counts = stmt.executeBatch();
```

### PreparedStatement 배치

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

순방향 전용, 읽기 전용 결과 집합:

```java
ResultSet rs = stmt.executeQuery("SELECT id, name FROM users");
while (rs.next()) {
    int id = rs.getInt("id");
    String name = rs.getString("name");
    // rs.wasNull()로 마지막 읽은 값이 NULL인지 확인
}
rs.close();
```

인덱스(1부터 시작) 또는 이름으로 컬럼 접근:

```java
rs.getInt(1);          // 위치로
rs.getString("name");  // 라벨로
```

## 트랜잭션

기본적으로 자동 커밋이 활성화되어 있습니다. 명시적 트랜잭션 제어를 위해 비활성화합니다:

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

// 테이블 목록
ResultSet tables = meta.getTables(null, null, null, null);
while (tables.next()) {
    String tableName = tables.getString("TABLE_NAME");
}

// 컬럼 목록
ResultSet cols = meta.getColumns(null, null, "users", null);
while (cols.next()) {
    String name = cols.getString("COLUMN_NAME");
    String type = cols.getString("TYPE_NAME");
    boolean nullable = cols.getInt("NULLABLE") == DatabaseMetaData.columnNullable;
}

// 기본 키
ResultSet pks = meta.getPrimaryKeys(null, null, "users");

// 외래 키
ResultSet fks = meta.getImportedKeys(null, null, "orders");

// 인덱스
ResultSet idxs = meta.getIndexInfo(null, null, "users", false, false);
```

`getColumns()`는 각 컬럼의 타입과 정밀도/스케일에 기반한 `COLUMN_SIZE`, `DECIMAL_DIGITS`, `CHAR_OCTET_LENGTH`를 반환합니다:

| 컬럼 타입 | COLUMN_SIZE | DECIMAL_DIGITS | CHAR_OCTET_LENGTH |
|------------|-------------|----------------|-------------------|
| `VARCHAR(n)` / `CHAR(n)` | n | null | n * 4 |
| `TEXT` | 2147483647 | null | 2147483647 |
| `INT` / `SERIAL` | 10 | 0 | null |
| `BIGINT` / `BIGSERIAL` | 19 | 0 | null |
| `NUMERIC(p,s)` | p | s | null |
| `BOOLEAN` | 1 | null | null |

## 타입 매핑

| PetraDB 타입 | JDBC 타입 | Java 타입 |
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
