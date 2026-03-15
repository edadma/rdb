---
title: Scala の使い方
description: JVM、JS、NativeでScalaからPetraDBを使用する方法です。
---

## インメモリデータベース

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

各`MemoryDB`インスタンスは独立しています。すべてのデータはメモリ上に存在します。

## 永続データベース

PetraDBは[stow](https://github.com/edadma/stow)を使用した、JVMとNativeでのクラッシュセーフな永続ストレージをサポートしています。

### 新しいデータベースの作成

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

### 既存のデータベースを開く

```scala
val db = PersistentDB.open("mydata.db")
given Session = db.connect()

val results = executeSQL("SELECT * FROM users")
results.foreach(println)

db.close()
```

再度開くと、すべてのテーブル、データ、列挙型、自動インクリメントの状態が復元されます。

永続データベースはクラッシュセーフのためにコピーオンライトページとダブルバッファードヘッダーを使用します。すべてのDDLおよびDML操作は永続的です。

## テキストデータベース

`TextDB`はデータベースを人間が編集可能な`.ptxt`ファイルとして保存します。開くとメモリにロードされ、変更のたびにファイルを書き換えます。初期開発、設定、バージョン管理に最適です。

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

同じファイルを再度開くとすべてのデータが復元されます。

```scala
val db = TextDB.open("mydata.ptxt")
given Session = db.connect()

val results = executeSQL("SELECT * FROM settings")
results.foreach(println)
```

JVMとNativeで動作します。`.ptxt`形式は人間が読めて差分に適しています。本番データのクラッシュセーフな永続性が必要な場合は`PersistentDB`を使用してください。

## SQLの実行

`executeSQL(sql)`は1つ以上のセミコロン区切りの文を実行し、`Seq[Result]`を返します。

```scala
val results: Seq[Result] = executeSQL("SELECT * FROM users; SELECT * FROM products;")
```

結果型、値の抽出、完全なAPIについては[Scala APIリファレンス](/reference/api-scala/)をご覧ください。

## テスト

```bash
# すべてのプラットフォームでテストを実行
sbt test

# JavaScriptテストのみ実行
sbt engineJS/test

# JVMテストのみ実行
sbt engineJVM/test

# Nativeテストのみ実行
sbt engineNative/test
```

## プラットフォームに関する注意事項

- **JVM** — スレッドセーフ、Spring Boot、Play Framework、Akkaなどと統合
- **Scala.js** — Node.jsとブラウザで動作
- **Scala Native** — ネイティブ実行ファイルにコンパイル、CLIツールや組み込みシステムに最適
