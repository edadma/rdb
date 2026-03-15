---
title: 服务器
description: 将 PetraDB 作为 HTTP 服务器运行。
---

## 安装

```bash
npm install -g @petradb/server
```

## 使用方法

```bash
petradb-server [OPTIONS] [path]
```

如果提供了路径，服务器会在该位置打开（或创建）持久化数据库。不提供路径时使用内存数据库。

### 选项

| 选项 | 描述 |
|--------|-------------|
| `-m`、`--memory` | 使用内存数据库 |
| `-p`、`--port` | 端口号（默认：`5480`） |
| `-h`、`--host` | 主机地址（默认：`127.0.0.1`） |
| `-c`、`--config` | TOML 配置文件路径 |

### 示例

```bash
# 默认端口上的内存数据库
petradb-server

# 自定义端口上的持久化数据库
petradb-server -p 8080 mydata.db

# 使用配置文件
petradb-server -c petradb.toml mydata.db
```

## 配置

TOML 文件控制认证、CORS 和会话限制。

```toml
auth = "basic"

[[users]]
username = "admin"
password = "$HASHED_PASSWORD"

[[users]]
username = "reader"
password = "$HASHED_PASSWORD"

[cors]
origin = "*"          # "*"（默认）、"none" 或特定源
[sessions]
max_sessions = 100    # 0 = 无限制（默认）
```

### 认证模式

| 模式 | 描述 |
|------|-------------|
| `"none"` | 无认证（未提供配置文件时的默认值） |
| `"basic"` | 针对 `[[users]]` 列表的 HTTP Basic 认证 |

配置文件中的密码以 PBKDF2 哈希存储。

当 `auth = "basic"` 时，每个请求（除 `GET /health` 外）必须包含 `Authorization: Basic <credentials>` 头。

### CORS

| `origin` 值 | 行为 |
|----------------|-----------|
| `"*"`（默认） | 允许所有来源 |
| `"none"` | 不添加 CORS 头 |
| URL（例如 `"https://app.example.com"`） | 仅允许该来源 |

## HTTP API

所有 SQL 请求和响应使用 PetraDB 二进制编解码（`application/octet-stream`）。请使用 [`@petradb/client`](/guides/client/) 库而不是直接调用这些端点。

### 执行 SQL

```
POST /sql
Content-Type: application/octet-stream
X-Session-Id: <session-id>   （可选）

<sql text>
```

返回二进制编码的 `Seq[Result]`。不带 `X-Session-Id` 头时，每个请求在一次性临时会话中运行。

### 创建会话

```
POST /session
```

返回 `{ "sessionId": "<id>" }`。在后续的 `X-Session-Id` 头中使用返回的 ID，以在请求之间共享事务状态。

### 关闭会话

```
DELETE /session/<id>
```

成功时返回 `{ "ok": "true" }`，如果会话不存在则返回 `404`。

### 健康检查

```
GET /health
```

返回 `{ "status": "ok" }`。不受认证限制。

## 错误响应

| 状态码 | 含义 |
|--------|---------|
| 400 | SQL 解析错误、类型错误或未定义的引用 |
| 401 | 缺少或无效的凭证 |
| 409 | Schema 或约束违反 |
| 503 | 会话数达到上限 |
