---
title: Scala
description: Référence de l'API Scala pour PetraDB.
---

## Installation

Ajoutez à votre `build.sbt` :

```scala
libraryDependencies += "io.github.edadma" %%% "petradb-engine" % "1.5.2"
```

## Structure des packages

PetraDB est réparti sur deux packages :

- **`io.github.edadma.petradb`** — types partagés (`Result`, `Value`, `Row`, `TableValue`, trait `Session`)
- **`io.github.edadma.petradb.engine`** — le moteur de base de données (`MemoryDB`, `PersistentDB`, `TextDB`, `Session`, `executeSQL`)

Importez les deux pour utiliser le moteur directement :

```scala
import io.github.edadma.petradb.*
import io.github.edadma.petradb.engine.*
```

## Base de données en mémoire

```scala
import io.github.edadma.petradb.*
import io.github.edadma.petradb.engine.*

given Session = new MemoryDB().connect()
```

## Base de données persistante

```scala
import io.github.edadma.petradb.*
import io.github.edadma.petradb.engine.*

// Créer une nouvelle
val db = PersistentDB.create("path/to/db", pageSize = 4096)
given Session = db.connect()

// Rouvrir une existante
val db = PersistentDB.open("path/to/db")
given Session = db.connect()

// Fermer lorsque terminé
db.close()
```

## Base de données texte

```scala
import io.github.edadma.petradb.*
import io.github.edadma.petradb.engine.*

val db = TextDB.open("path/to/data.ptxt")
given Session = db.connect()

db.close()
```

Fichier `.ptxt` lisible par l'homme. Charge en mémoire à l'ouverture, réécrit après chaque modification. Fonctionne sur JVM et Native.

## Exécuter du SQL

### `executeSQL(sql: String)(using Session): Seq[Result]`

Exécute une ou plusieurs instructions SQL séparées par des points-virgules et retourne une séquence de résultats.

```scala
val results: Seq[Result] = executeSQL("SELECT * FROM users")
```

## Types de résultats

```scala
sealed trait Result
case class QueryResult(table: TableValue)                           extends Result
case class InsertResult(obj: Map[String, Value], table: TableValue) extends Result
case class CreateTableResult(table: String)                         extends Result
case class DropTableResult(table: String)                           extends Result
case class CreateIndexResult(name: String)                          extends Result
case class DropIndexResult(name: String)                            extends Result
case class CreateTypeResult(typ: String)                            extends Result
case class DropTypeResult(name: String)                             extends Result
case class CreateViewResult(name: String)                           extends Result
case class DropViewResult(name: String)                             extends Result
case class CreateSequenceResult(name: String)                       extends Result
case class DropSequenceResult(name: String)                         extends Result
case class UpdateResult(rows: Int)                                  extends Result
case class DeleteResult(rows: Int)                                  extends Result
case class TruncateResult(table: String)                            extends Result
case class AlterTableResult()                                       extends Result
case class ExplainResult(plan: String)                              extends Result
case class PrepareResult(name: String)                              extends Result
case class DeallocateResult(name: String)                           extends Result
case class CopyResult(rows: Int)                                    extends Result
case class CreateSchemaResult(name: String)                         extends Result
case object BeginResult                                             extends Result
case object CommitResult                                            extends Result
case object RollbackResult                                          extends Result
case object DoBlockResult                                           extends Result
case class CreateFunctionResult(name: String)                       extends Result
case class DropFunctionResult(name: String)                         extends Result
case class CreateProcedureResult(name: String)                      extends Result
case class DropProcedureResult(name: String)                        extends Result
case class CreateTriggerResult(name: String)                        extends Result
case class DropTriggerResult(name: String)                          extends Result
case object CallResult                                              extends Result
```

## Accéder aux données de requête

```scala
val QueryResult(table) = executeQuery("SELECT * FROM users")

// Accéder aux lignes
val rows: IndexedSeq[Row] = table.data

for (row <- table.data) {
  val id: Int = row.getInt("id")
  val name: String = row.getString("name")
  val email: Option[String] = row.getStringOption("email")
}
```

## Fonctions définies par l'utilisateur

Enregistrez des fonctions Scala natives appelables depuis SQL, les déclencheurs et les procédures stockées :

```scala
db.registerScalarFunction("my_double", {
  case Seq(v) => NumberValue(v.intValue * 2)
}, NumberType)

// Maintenant utilisable en SQL :
// SELECT my_double(age) FROM users;
```

Les fonctions enregistrées de cette manière fonctionnent partout : `SELECT`, `WHERE`, blocs `DO`, fonctions stockées et déclencheurs.

## Extraction de valeurs

```scala
val row: Row = table.data.head

// Extraction type-safe
val id: Int = row.getInt("id")
val name: String = row.getString("name")
val email: Option[String] = row.getStringOption("email")
val isActive: Boolean = row.getBoolean("is_active")

// Accès direct
val value: Value = row("column_name")
```
