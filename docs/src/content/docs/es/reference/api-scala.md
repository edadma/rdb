---
title: Scala
description: Referencia de la API Scala para PetraDB.
---

## Instalacion

Agrega a tu `build.sbt`:

```scala
libraryDependencies += "io.github.edadma" %%% "petradb-engine" % "1.5.6"
```

## Estructura de paquetes

PetraDB se divide en dos paquetes:

- **`io.github.edadma.petradb`** — tipos compartidos (`Result`, `Value`, `Row`, `TableValue`, trait `Session`)
- **`io.github.edadma.petradb.engine`** — el motor de base de datos (`MemoryDB`, `PersistentDB`, `TextDB`, `Session`, `executeSQL`)

Importa ambos para usar el motor directamente:

```scala
import io.github.edadma.petradb.*
import io.github.edadma.petradb.engine.*
```

## Base de datos en memoria

```scala
import io.github.edadma.petradb.*
import io.github.edadma.petradb.engine.*

given Session = new MemoryDB().connect()
```

## Base de datos persistente

```scala
import io.github.edadma.petradb.*
import io.github.edadma.petradb.engine.*

// Crear nueva
val db = PersistentDB.create("path/to/db", pageSize = 4096)
given Session = db.connect()

// Reabrir existente
val db = PersistentDB.open("path/to/db")
given Session = db.connect()

// Cerrar cuando termines
db.close()
```

## Base de datos de texto

```scala
import io.github.edadma.petradb.*
import io.github.edadma.petradb.engine.*

val db = TextDB.open("path/to/data.ptxt")
given Session = db.connect()

db.close()
```

Archivo `.ptxt` legible por humanos. Carga en memoria al abrir, reescribe despues de cada cambio. Funciona en JVM y Native.

## Ejecutar SQL

### `executeSQL(sql: String)(using Session): Seq[Result]`

Ejecuta una o mas sentencias SQL separadas por punto y coma y retorna una secuencia de resultados.

```scala
val results: Seq[Result] = executeSQL("SELECT * FROM users")
```

## Tipos de resultado

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

## Acceder a datos de consulta

```scala
val QueryResult(table) = executeQuery("SELECT * FROM users")

// Acceder a filas
val rows: IndexedSeq[Row] = table.data

for (row <- table.data) {
  val id: Int = row.getInt("id")
  val name: String = row.getString("name")
  val email: Option[String] = row.getStringOption("email")
}
```

## Funciones definidas por el usuario

Registra funciones Scala nativas invocables desde SQL, triggers y procedimientos almacenados:

```scala
db.registerScalarFunction("my_double", {
  case Seq(v) => NumberValue(v.intValue * 2)
}, NumberType)

// Ahora utilizable en SQL:
// SELECT my_double(age) FROM users;
```

Las funciones registradas de esta manera funcionan en todas partes: `SELECT`, `WHERE`, bloques `DO`, funciones almacenadas y triggers.

## Extraccion de valores

```scala
val row: Row = table.data.head

// Extraccion con seguridad de tipos
val id: Int = row.getInt("id")
val name: String = row.getString("name")
val email: Option[String] = row.getStringOption("email")
val isActive: Boolean = row.getBoolean("is_active")

// Acceso directo
val value: Value = row("column_name")
```
