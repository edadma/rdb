---
title: Scala ではじめる
description: ScalaプロジェクトにPetraDBを追加して、最初のSQLクエリを実行します。
---

## インストール

`build.sbt`に追加します。

```scala
libraryDependencies += "io.github.edadma" %%% "petradb-engine" % "1.5.6"
```

`%%%`演算子がプラットフォームに応じた正しいアーティファクト（JVM、Scala.js、Scala Native）を選択します。

## 最初のクエリを実行する

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

各`MemoryDB`インスタンスは完全に独立したインメモリデータベースです。すべてのデータはメモリ上に存在し、ファイルシステムには一切アクセスしません。

## 永続ストレージ

再起動後もデータを保持する必要がある場合、PetraDBには外部インフラストラクチャ不要の2つのオプションがあります。

**`PersistentDB`** — [stow](https://github.com/edadma/stow)によるコピーオンライトページとダブルバッファードヘッダーを使用した、単一ファイルのクラッシュセーフな永続ストレージです。JVMとNativeで利用可能です。

**`TextDB`** — データベースを人間が読める`.ptxt`ファイルとして保存します。開発、設定データ、バージョン管理に最適です。

どちらも[Scalaガイド](/guides/scala/)で詳しく説明しています。

## ブラウザで試す

プロジェクトのセットアップなしで、PetraDBのSQLサポートを今すぐ試せます。[プレイグラウンド](/playground/)はブラウザ上でフルエンジンを実行します。

## 次のステップ

[Scalaガイド](/guides/scala/)では、永続データベースとテキストデータベース、SQLの実行、結果の処理、完全なAPIについて説明しています。PetraDBをネットワークサービスとして実行する方法については、[サーバー](/guides/server/)と[クライアント](/guides/client/)ガイドをご覧ください。
