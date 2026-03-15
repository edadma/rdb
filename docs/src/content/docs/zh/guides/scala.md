---
title: Scala 使用指南
description: 在 JVM、JS 和 Native 上从 Scala 使用 PetraDB。
---

## 内存数据库

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

每个 `MemoryDB` 实例是隔离且独立的。所有数据存储在内存中。

## 持久化数据库

PetraDB 通过 [stow](https://github.com/edadma/stow) 在 JVM 和 Native 上支持防崩溃持久化存储。

### 创建新数据库

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

### 重新打开已有数据库

```scala
val db = PersistentDB.open("mydata.db")
given Session = db.connect()

val results = executeSQL("SELECT * FROM users")
results.foreach(println)

db.close()
```

重新打开时，所有表、数据、枚举类型和自增状态都会恢复。

持久化数据库使用写时复制页面和双缓冲头来保证崩溃安全。所有 DDL 和 DML 操作都是持久的。

## 文本数据库

`TextDB` 将数据库存储为人类可编辑的 `.ptxt` 文件。打开时加载到内存，每次更改后重写文件。适合早期开发、配置和版本控制。

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

重新打开同一文件以恢复所有数据：

```scala
val db = TextDB.open("mydata.ptxt")
given Session = db.connect()

val results = executeSQL("SELECT * FROM settings")
results.foreach(println)
```

适用于 JVM 和 Native。`.ptxt` 格式人类可读且对 diff 友好。对于生产数据需要防崩溃持久化时，请使用 `PersistentDB`。

## 执行 SQL

`executeSQL(sql)` 运行一个或多个以分号分隔的语句，返回 `Seq[Result]`。

```scala
val results: Seq[Result] = executeSQL("SELECT * FROM users; SELECT * FROM products;")
```

完整的结果类型、值提取和完整 API 请参阅 [Scala API 参考](/reference/api-scala/)。

## 测试

```bash
# 在所有平台上运行测试
sbt test

# 仅运行 JavaScript 测试
sbt engineJS/test

# 仅运行 JVM 测试
sbt engineJVM/test

# 仅运行 Native 测试
sbt engineNative/test
```

## 平台说明

- **JVM** — 线程安全，可与 Spring Boot、Play Framework、Akka 等集成
- **Scala.js** — 在 Node.js 和浏览器中运行
- **Scala Native** — 编译为原生可执行文件；适合 CLI 工具和嵌入式系统
