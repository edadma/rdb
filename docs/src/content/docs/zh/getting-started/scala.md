---
title: Scala 入门
description: 将 PetraDB 添加到你的 Scala 项目并运行第一个 SQL 查询。
---

## 安装

添加到你的 `build.sbt`：

```scala
libraryDependencies += "io.github.edadma" %%% "petradb-engine" % "1.5.6"
```

`%%%` 运算符会根据你的平台自动选择正确的制品 — JVM、Scala.js 或 Scala Native。

## 运行你的第一个查询

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

每个 `MemoryDB` 实例是一个完全隔离的内存数据库。所有数据存储在内存中 — 不会触及文件系统。

## 持久化存储

当你需要数据在重启后保留时，PetraDB 提供两种无需外部基础设施的选项：

**`PersistentDB`** — 单文件防崩溃持久化存储，通过 [stow](https://github.com/edadma/stow) 使用写时复制页面和双缓冲头。适用于 JVM 和 Native。

**`TextDB`** — 将数据库存储为人类可读的 `.ptxt` 文件。适合开发、配置数据和版本控制。

两者在 [Scala 指南](/guides/scala/)中有详细介绍。

## 在浏览器中尝试

你可以立即体验 PetraDB 的 SQL 支持 — 无需设置项目。[Playground](/playground/) 在你的浏览器中运行完整引擎。

## 下一步

[Scala 指南](/guides/scala/)涵盖了持久化和文本数据库、执行 SQL、结果处理和完整 API。关于将 PetraDB 作为网络服务运行，请参阅[服务器](/guides/server/)和[客户端](/guides/client/)指南。
