---
title: CLI
description: PetraDB를 위한 대화형 SQL 셸과 커맨드라인 도구.
---

## 설치

```bash
npm install -g @petradb/cli
```

## 사용법

```bash
petradb [OPTIONS] [path]
```

경로가 주어지면, CLI는 해당 위치의 영구 데이터베이스를 열거나 생성합니다. 경로 없이 실행하면 인메모리 데이터베이스를 사용합니다.

### 옵션

| 옵션 | 설명 |
|--------|-------------|
| `-m`, `--memory` | 인메모리 데이터베이스 사용 (경로가 주어져도) |
| `-e`, `--execute` | SQL을 실행하고 종료 (반복 가능) |
| `-f`, `--file` | SQL 파일을 실행하고 종료 (반복 가능) |
| `--stdin` | stdin에서 SQL을 읽어 실행하고 종료 |
| `--host` | 원격 PetraDB 서버에 연결 |
| `--port` | 서버 포트 (기본값: 5480) |
| `--user` | 인증용 사용자 이름 |
| `--password` | 인증용 비밀번호 |

### 예제

```bash
# 대화형 인메모리 세션 시작
petradb

# 영구 데이터베이스 열기 또는 생성
petradb mydata.db

# 비대화형으로 SQL 실행
petradb mydata.db -e "SELECT * FROM users"

# 파일 실행 후 쿼리 실행
petradb mydata.db -f schema.sql -e "SELECT count(*) FROM users"

# stdin에서 SQL 파이프
echo "SELECT 1 + 1" | petradb --stdin

# 원격 서버에 연결
petradb --host myserver.example.com --port 5480

# 인증과 함께 연결
petradb --host localhost --user admin --password secret
```

## 데이터베이스 모드

파일 확장자에 따라 스토리지 모드가 결정됩니다:

| 확장자 | 모드 | 설명 |
|-----------|------|-------------|
| `.petra` (또는 기타) | 영구 | copy-on-write 페이지를 통한 충돌 안전 내구성 스토리지 |
| `.ptxt` | 텍스트 | 변경 후마다 다시 작성되는 사람이 읽을 수 있는 파일 |
| *(경로 없음)* | 인메모리 | 세션 지속 시간 동안만 데이터 존재 |

## 덤프

`dump` 하위 명령은 영구 데이터베이스의 전체 스키마와 데이터를 SQL 문으로 출력합니다.

```bash
petradb dump mydata.db
```

출력에는 데이터베이스를 처음부터 재생성할 수 있는 `CREATE TYPE`, `CREATE TABLE`, `INSERT INTO`, `CREATE VIEW` 문이 포함됩니다.

## 대화형 REPL

`-e`, `-f`, `--stdin` 없이 시작하면, CLI는 readline 히스토리가 있는 대화형 SQL 셸에 진입합니다.

`;`로 끝나는 SQL을 입력하여 실행합니다. 여러 줄 입력이 지원됩니다 — `;`로 끝날 때까지 연속 프롬프트(`  -> `)가 나타납니다.

### 메타 명령

| 명령 | 설명 |
|---------|-------------|
| `\dt` | 모든 테이블 나열 |
| `\dv` | 모든 뷰 나열 |
| `\ds` | 모든 시퀀스 나열 |
| `\di` | 모든 인덱스 나열 |
| `\d <name>` | 테이블 또는 뷰 설명 (컬럼, 타입, NULL 허용 여부) |
| `\i <file>` | 파일에서 SQL 실행 |
| `\dump` | 전체 스키마와 데이터를 SQL로 출력 |
| `\copy <args>` | `COPY` 문 실행 |
| `\timing` | 쿼리 시간 측정 켜기/끄기 |
| `\q` | 종료 |

### 세션 예제

```
$ petradb
PetraDB — interactive SQL shell
Type \q to quit, \dt to list tables, \d <table> to describe a table.

petra> CREATE TABLE cities (name TEXT, population INT);
CREATE TABLE
petra> INSERT INTO cities (name, population) VALUES ('Oslo', 709037);
INSERT 0 1
petra> SELECT * FROM cities;
 name | population
------+------------
 Oslo |     709037
(1 row)
petra> \dt
 Table
--------
 cities
(1 row)
petra> \d cities
Table "cities"
 Column     | Type | Nullable
------------+------+----------
 name       | TEXT | null
 population | INT  | null
petra> \timing
Timing is on.
petra> SELECT count(*) FROM cities;
 count
-------
     1
(1 row)
Time: 0.002 s
petra> \q
```
