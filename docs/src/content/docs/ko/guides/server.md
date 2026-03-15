---
title: 서버
description: PetraDB를 HTTP 서버로 실행하기.
---

## 설치

```bash
npm install -g @petradb/server
```

## 사용법

```bash
petradb-server [OPTIONS] [path]
```

경로가 주어지면, 서버는 해당 위치의 영구 데이터베이스를 열거나 생성합니다. 경로 없이 실행하면 인메모리 데이터베이스를 사용합니다.

### 옵션

| 옵션 | 설명 |
|--------|-------------|
| `-m`, `--memory` | 인메모리 데이터베이스 사용 |
| `-p`, `--port` | 포트 번호 (기본값: `5480`) |
| `-h`, `--host` | 호스트 주소 (기본값: `127.0.0.1`) |
| `-c`, `--config` | TOML 설정 파일 경로 |

### 예제

```bash
# 기본 포트에서 인메모리 데이터베이스
petradb-server

# 커스텀 포트에서 영구 데이터베이스
petradb-server -p 8080 mydata.db

# 설정 파일과 함께
petradb-server -c petradb.toml mydata.db
```

## 설정

TOML 파일로 인증, CORS, 세션 제한을 제어합니다.

```toml
auth = "basic"

[[users]]
username = "admin"
password = "$HASHED_PASSWORD"

[[users]]
username = "reader"
password = "$HASHED_PASSWORD"

[cors]
origin = "*"          # "*" (기본값), "none", 또는 특정 오리진

[sessions]
max_sessions = 100    # 0 = 무제한 (기본값)
```

### 인증 모드

| 모드 | 설명 |
|------|-------------|
| `"none"` | 인증 없음 (설정 파일이 없을 때 기본값) |
| `"basic"` | `[[users]]` 목록에 대한 HTTP Basic 인증 |

설정 파일의 비밀번호는 PBKDF2 해시로 저장됩니다.

`auth = "basic"`인 경우, 모든 요청(`GET /health` 제외)에 `Authorization: Basic <credentials>` 헤더가 포함되어야 합니다.

### CORS

| `origin` 값 | 동작 |
|----------------|-----------|
| `"*"` (기본값) | 모든 오리진 허용 |
| `"none"` | CORS 헤더 없음 |
| URL (예: `"https://app.example.com"`) | 해당 오리진만 허용 |

## HTTP API

모든 SQL 요청과 응답은 PetraDB 바이너리 코덱(`application/octet-stream`)을 사용합니다. 이 엔드포인트를 직접 호출하는 대신 [`@petradb/client`](/guides/client/) 라이브러리를 사용하세요.

### SQL 실행

```
POST /sql
Content-Type: application/octet-stream
X-Session-Id: <session-id>   (선택사항)

<sql text>
```

바이너리 인코딩된 `Seq[Result]`를 반환합니다. `X-Session-Id` 헤더 없이는 각 요청이 일회성 임시 세션에서 실행됩니다.

### 세션 생성

```
POST /session
```

`{ "sessionId": "<id>" }`를 반환합니다. 반환된 ID를 이후 `X-Session-Id` 헤더에 사용하여 요청 간에 트랜잭션 상태를 공유합니다.

### 세션 종료

```
DELETE /session/<id>
```

성공 시 `{ "ok": "true" }`를, 세션이 존재하지 않으면 `404`를 반환합니다.

### 상태 확인

```
GET /health
```

`{ "status": "ok" }`를 반환합니다. 인증 대상이 아닙니다.

## 오류 응답

| 상태 | 의미 |
|--------|---------|
| 400 | SQL 파서 오류, 타입 오류, 또는 정의되지 않은 참조 |
| 401 | 누락되었거나 유효하지 않은 자격 증명 |
| 409 | 스키마 또는 제약 조건 위반 |
| 503 | 세션 제한 도달 |
