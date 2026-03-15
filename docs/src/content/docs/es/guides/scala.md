---
title: Uso con Scala
description: Uso de PetraDB desde Scala en JVM, JS y Native.
---

## Base de datos en memoria

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

Cada instancia de `MemoryDB` es aislada e independiente. Todos los datos residen en memoria.

## Base de datos persistente

PetraDB soporta almacenamiento durable a prueba de fallos en JVM y Native via [stow](https://github.com/edadma/stow).

### Crear una nueva base de datos

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

### Reabrir una base de datos existente

```scala
val db = PersistentDB.open("mydata.db")
given Session = db.connect()

val results = executeSQL("SELECT * FROM users")
results.foreach(println)

db.close()
```

Todas las tablas, datos, tipos enum y estado de auto-incremento se restauran al reabrir.

Las bases de datos persistentes usan paginas copy-on-write y encabezados con doble buffer para seguridad ante fallos. Todas las operaciones DDL y DML son durables.

## Base de datos de texto

`TextDB` almacena la base de datos como un archivo `.ptxt` editable por humanos. Carga en memoria al abrir y reescribe el archivo despues de cada cambio. Ideal para desarrollo temprano, configuracion y control de versiones.

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

Reabre el mismo archivo para restaurar todos los datos:

```scala
val db = TextDB.open("mydata.ptxt")
given Session = db.connect()

val results = executeSQL("SELECT * FROM settings")
results.foreach(println)
```

Funciona en JVM y Native. El formato `.ptxt` es legible por humanos y amigable para diffs. Usa `PersistentDB` cuando necesites durabilidad a prueba de fallos para datos de produccion.

## Ejecutar SQL

`executeSQL(sql)` ejecuta una o mas sentencias separadas por punto y coma y retorna un `Seq[Result]`.

```scala
val results: Seq[Result] = executeSQL("SELECT * FROM users; SELECT * FROM products;")
```

Consulta la [referencia de la API Scala](/reference/api-scala/) para los tipos de resultado, extraccion de valores y la API completa.

## Pruebas

```bash
# Ejecutar pruebas para todas las plataformas
sbt test

# Ejecutar solo pruebas de JavaScript
sbt engineJS/test

# Ejecutar solo pruebas de JVM
sbt engineJVM/test

# Ejecutar solo pruebas de Native
sbt engineNative/test
```

## Notas de plataforma

- **JVM** — thread-safe, se integra con Spring Boot, Play Framework, Akka, etc.
- **Scala.js** — se ejecuta en Node.js y navegadores
- **Scala Native** — compila a ejecutables nativos; ideal para herramientas CLI y sistemas embebidos
