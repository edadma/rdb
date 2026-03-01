---
title: Server
description: Running PetraDB as an HTTP server.
---

## Installation

```bash
npm install -g @petradb/server
```

## Usage

```bash
petradb-server [OPTIONS] [path]
```

If a path is given, the server opens (or creates) a persistent database at that location. Without a path, an in-memory database is used.

### Options

| Option | Description |
|--------|-------------|
| `-m`, `--memory` | Use an in-memory database |
| `-p`, `--port` | Port number (default: `5480`) |
| `-h`, `--host` | Host address (default: `127.0.0.1`) |
| `-c`, `--config` | Path to a TOML configuration file |

### Examples

```bash
# In-memory database on the default port
petradb-server

# Persistent database on a custom port
petradb-server -p 8080 mydata.db

# With a config file
petradb-server -c petradb.toml mydata.db
```

## Configuration

A TOML file controls authentication, CORS, and session limits.

```toml
auth = "basic"

[[users]]
username = "admin"
password = "$HASHED_PASSWORD"

[[users]]
username = "reader"
password = "$HASHED_PASSWORD"

[cors]
origin = "*"          # "*" (default), "none", or a specific origin

[sessions]
max_sessions = 100    # 0 = unlimited (default)
```

### Authentication Modes

| Mode | Description |
|------|-------------|
| `"none"` | No authentication (default when no config file is given) |
| `"basic"` | HTTP Basic authentication against the `[[users]]` list |

Passwords in the config file are stored as PBKDF2 hashes.

When `auth = "basic"`, every request (except `GET /health`) must include an `Authorization: Basic <credentials>` header.

### CORS

| `origin` value | Behaviour |
|----------------|-----------|
| `"*"` (default) | Allow all origins |
| `"none"` | No CORS headers |
| A URL (e.g. `"https://app.example.com"`) | Allow only that origin |

## HTTP API

All SQL requests and responses use the PetraDB binary codec (`application/octet-stream`). Use the [`@petradb/client`](/guides/client/) library instead of calling these endpoints directly.

### Execute SQL

```
POST /sql
Content-Type: application/octet-stream
X-Session-Id: <session-id>   (optional)

<sql text>
```

Returns a binary-encoded `Seq[Result]`. Without an `X-Session-Id` header, each request runs in a one-off transient session.

### Create Session

```
POST /session
```

Returns `{ "sessionId": "<id>" }`. Use the returned ID in subsequent `X-Session-Id` headers to share transaction state across requests.

### Close Session

```
DELETE /session/<id>
```

Returns `{ "ok": "true" }` on success, or `404` if the session does not exist.

### Health Check

```
GET /health
```

Returns `{ "status": "ok" }`. Not subject to authentication.

## Error Responses

| Status | Meaning |
|--------|---------|
| 400 | SQL parse error, type error, or undefined reference |
| 401 | Missing or invalid credentials |
| 409 | Schema or constraint violation |
| 503 | Session limit reached |
