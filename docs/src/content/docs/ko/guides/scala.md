---
title: Scala 사용법
description: JVM, JS, Native에서 Scala로 PetraDB를 사용하는 방법.
---

## 인메모리 데이터베이스

```scala
import io.github.edadma.petradb.*
import io.github.edadma.petradb.engine.*

given Session = new MemoryDB().connect()

val results = executeSQL("""
  CREATE TABLE products (
    id SERIAL,
    name TEXT NOT NULL,
    price NUMERIC(10,2),
    category TEXT
  );

  INSERT INTO products (name, price, category) VALUES
    ('Laptop', 999.99, 'Electronics'),
    ('Coffee', 4.50, 'Food'),
    ('Book', 19.99, 'Education');

  SELECT category, COUNT(*), AVG(price)
  FROM products
  GROUP BY category
  ORDER BY category;
""")

results.foreach(println)
```

각 `MemoryDB` 인스턴스는 격리되고 독립적입니다. 모든 데이터가 메모리에 존재합니다.

## 영구 데이터베이스

PetraDB는 [stow](https://github.com/edadma/stow)를 통해 JVM과 Native에서 충돌 안전 내구성 스토리지를 지원합니다.

### 새 데이터베이스 생성

```scala
import io.github.edadma.petradb.*
import io.github.edadma.petradb.engine.*

val db = PersistentDB.create("mydata.db", 4096)
given Session = db.connect()

executeSQL("""
  CREATE TABLE users (
    id SERIAL,
    name TEXT NOT NULL,
    email TEXT,
    PRIMARY KEY (id)
  );

  INSERT INTO users (name, email) VALUES ('Alice', 'alice@example.com');
""")

db.close()
```

### 기존 데이터베이스 다시 열기

```scala
val db = PersistentDB.open("mydata.db")
given Session = db.connect()

val results = executeSQL("SELECT * FROM users")
results.foreach(println)

db.close()
```

다시 열면 모든 테이블, 데이터, 열거 타입, 자동 증가 상태가 복원됩니다.

영구 데이터베이스는 충돌 안전을 위해 copy-on-write 페이지와 이중 버퍼 헤더를 사용합니다. 모든 DDL과 DML 작업은 내구성이 있습니다.

## 텍스트 데이터베이스

`TextDB`는 데이터베이스를 사람이 편집 가능한 `.ptxt` 파일로 저장합니다. 열 때 메모리에 로드하고 변경 후마다 파일을 다시 작성합니다. 초기 개발, 설정, 버전 관리에 적합합니다.

```scala
import io.github.edadma.petradb.*
import io.github.edadma.petradb.engine.*

val db = TextDB.open("mydata.ptxt")
given Session = db.connect()

executeSQL("""
  CREATE TABLE settings (key TEXT, value TEXT);
  INSERT INTO settings (key, value) VALUES ('theme', 'dark');
""")

db.close()
```

같은 파일을 다시 열면 모든 데이터가 복원됩니다:

```scala
val db = TextDB.open("mydata.ptxt")
given Session = db.connect()

val results = executeSQL("SELECT * FROM settings")
results.foreach(println)
```

JVM과 Native에서 작동합니다. `.ptxt` 형식은 사람이 읽을 수 있고 diff 친화적입니다. 프로덕션 데이터의 충돌 안전 내구성이 필요하면 `PersistentDB`를 사용하세요.

## SQL 실행

`executeSQL(sql)`은 세미콜론으로 구분된 하나 이상의 문을 실행하고 `Seq[Result]`를 반환합니다.

```scala
val results: Seq[Result] = executeSQL("SELECT * FROM users; SELECT * FROM products;")
```

결과 타입, 값 추출, 전체 API에 대해서는 [Scala API 레퍼런스](/reference/api-scala/)를 참고하세요.

## 테스트

```bash
# 모든 플랫폼에서 테스트 실행
sbt test

# JavaScript 테스트만 실행
sbt engineJS/test

# JVM 테스트만 실행
sbt engineJVM/test

# Native 테스트만 실행
sbt engineNative/test
```

## 플랫폼 참고사항

- **JVM** — 스레드 안전, Spring Boot, Play Framework, Akka 등과 통합
- **Scala.js** — Node.js와 브라우저에서 실행
- **Scala Native** — 네이티브 실행 파일로 컴파일; CLI 도구와 임베디드 시스템에 적합
