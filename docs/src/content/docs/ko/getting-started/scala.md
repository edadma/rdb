---
title: Scala 시작하기
description: Scala 프로젝트에 PetraDB를 추가하고 첫 번째 SQL 쿼리를 실행합니다.
---

## 설치

`build.sbt`에 추가합니다:

```scala
libraryDependencies += "io.github.edadma" %%% "petradb-engine" % "1.5.2"
```

`%%%` 연산자는 플랫폼에 맞는 아티팩트를 자동으로 선택합니다 — JVM, Scala.js, 또는 Scala Native.

## 첫 번째 쿼리 실행

```scala
import io.github.edadma.petradb.*
import io.github.edadma.petradb.engine.*

given Session = new MemoryDB().connect()

val results = executeSQL("""
  CREATE TABLE users (
    id SERIAL,
    name TEXT NOT NULL,
    email TEXT
  );

  INSERT INTO users (name, email) VALUES
    ('Alice', 'alice@example.com'),
    ('Bob', 'bob@example.com');

  SELECT * FROM users;
""")

results.foreach(println)
```

각 `MemoryDB` 인스턴스는 완전히 격리된 인메모리 데이터베이스입니다. 모든 데이터는 메모리에 존재하며, 파일시스템에는 아무것도 쓰지 않습니다.

## 영구 스토리지

재시작 후에도 데이터를 유지해야 할 때, PetraDB는 외부 인프라 없이 두 가지 옵션을 제공합니다:

**`PersistentDB`** — [stow](https://github.com/edadma/stow)를 통한 단일 파일의 충돌 안전 내구성 스토리지. copy-on-write 페이지와 이중 버퍼 헤더를 사용합니다. JVM과 Native에서 사용 가능합니다.

**`TextDB`** — 데이터베이스를 사람이 읽을 수 있는 `.ptxt` 파일로 저장합니다. 개발, 설정 데이터, 버전 관리에 적합합니다.

두 가지 모두 [Scala 가이드](/guides/scala/)에서 자세히 다룹니다.

## 브라우저에서 시도하기

프로젝트 설정 없이 PetraDB의 SQL 지원을 바로 실험해 볼 수 있습니다. [플레이그라운드](/playground/)에서 전체 엔진이 브라우저에서 실행됩니다.

## 다음 단계

[Scala 가이드](/guides/scala/)에서 영구 및 텍스트 데이터베이스, SQL 실행, 결과 처리, 전체 API를 다룹니다. PetraDB를 네트워크 서비스로 실행하려면 [서버](/guides/server/)와 [클라이언트](/guides/client/) 가이드를 참고하세요.
