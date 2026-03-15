---
title: 데이터 타입
description: PetraDB가 지원하는 SQL 데이터 타입.
---

PetraDB는 SQL 구문, 식별자 처리, 타입 캐스팅에 있어 PostgreSQL 규칙을 따릅니다.

## SQL 호환성

- **대소문자 구분 없는 키워드** — `SELECT`, `select`, `Select`는 동일합니다
- **따옴표 없는 식별자 접기** — 따옴표 없는 식별자는 소문자로 접힘 (`CREATE TABLE Users` → 테이블 이름 `users`)
- **큰따옴표 식별자** — 대소문자 보존 (`"MixedCase"`는 그대로 유지)
- **문자열 이스케이프** — 작은따옴표 중복 (`'it''s'`)과 E-문자열 (`E'it\'s'`)
- **연산자** — 같지 않음에 `!=`와 `<>` 모두 사용 가능

## 지원되는 타입

| 타입 | 설명 |
|------|-------------|
| `SMALLINT` | 16비트 정수 (-32768 ~ 32767) |
| `INT` / `INTEGER` | 32비트 정수 |
| `BIGINT` | 64비트 정수 |
| `SMALLSERIAL` | 자동 증가 16비트 정수 (백업 시퀀스 생성) |
| `SERIAL` | 자동 증가 32비트 정수 (백업 시퀀스 생성) |
| `BIGSERIAL` | 자동 증가 64비트 정수 (백업 시퀀스 생성) |
| `DOUBLE` / `FLOAT` / `REAL` | 배정밀도 부동 소수점 |
| `NUMERIC(p,s)` / `DECIMAL(p,s)` | 고정 정밀도 소수 |
| `TEXT` | 가변 길이 문자열 |
| `CHAR(n)` | 고정 길이 문자열 (오른쪽 공백 패딩) |
| `VARCHAR(n)` | 가변 길이 문자열 (최대 n자, 패딩 없음) |
| `BOOLEAN` | 참/거짓 |
| `DATE` | 날짜 (`yyyy-MM-dd`) |
| `TIME` | 시각 (`HH:mm:ss`) |
| `TIMESTAMP` | 날짜와 시간 |
| `TIMESTAMP WITH TIME ZONE` | 시간대 오프셋이 있는 날짜와 시간 |
| `INTERVAL` | 기간 (ISO 8601 또는 `N days N hours N minutes N seconds`) |
| `UUID` | 범용 고유 식별자 |
| `JSON` / `JSONB` | 구조화된 JSON 객체와 배열 |
| `BYTEA` | 바이너리 데이터 |
| `ENUM` | 커스텀 열거 타입 (`CREATE TYPE ... AS ENUM`으로 생성) |
| `INT[]`, `TEXT[]` 등 | 타입 배열 (`[]` 접미사가 있는 모든 기본 타입) |

## 타입 캐스팅

`::` 연산자 또는 `CAST(expr AS type)`를 사용하여 타입 간 변환합니다:

```sql
SELECT '2024-06-15'::DATE;
SELECT CAST('14:30:00' AS TIME);
SELECT '2 hours 30 minutes'::INTERVAL;
SELECT CAST(val AS TEXT);
SELECT '42'::INT;
SELECT 1::BOOLEAN;
SELECT EXTRACT(year FROM created_at);
```

## 날짜/시간 연산

```sql
SELECT '2024-01-01'::DATE + 10;                        -- 일 수 더하기
SELECT '2024-01-15'::DATE - '2024-01-10'::DATE;        -- 일 수 차이
SELECT now() + '2 hours'::INTERVAL;                     -- 타임스탬프 + 인터벌
SELECT now() - '30 minutes'::INTERVAL;                  -- 타임스탬프 - 인터벌
SELECT '1 hour'::INTERVAL * 3;                          -- 인터벌 스케일
SELECT EXTRACT(year FROM now());                        -- 필드 추출
SELECT date_trunc('month', now());                      -- 절삭
```

## 제약 조건

```sql
PRIMARY KEY (id)
UNIQUE (email)
NOT NULL
DEFAULT value
FOREIGN KEY (col) REFERENCES other_table (col) ON DELETE CASCADE ON UPDATE CASCADE
```
