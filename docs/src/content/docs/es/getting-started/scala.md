---
title: Primeros pasos con Scala
description: Agrega PetraDB a tu proyecto Scala y ejecuta tus primeras consultas SQL.
---

## Instalacion

Agrega a tu `build.sbt`:

```scala
libraryDependencies += "io.github.edadma" %%% "petradb-engine" % "1.5.2"
```

El operador `%%%` selecciona el artefacto correcto para tu plataforma — JVM, Scala.js o Scala Native.

## Ejecuta tu primera consulta

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

Cada instancia de `MemoryDB` es una base de datos en memoria completamente aislada. Todos los datos residen en memoria — nada toca el sistema de archivos.

## Almacenamiento persistente

Cuando necesitas que los datos sobrevivan a los reinicios, PetraDB tiene dos opciones que no requieren infraestructura externa:

**`PersistentDB`** — almacenamiento durable a prueba de fallos en un solo archivo, usando paginas copy-on-write y encabezados con doble buffer via [stow](https://github.com/edadma/stow). Disponible en JVM y Native.

**`TextDB`** — almacena la base de datos como un archivo `.ptxt` legible por humanos. Ideal para desarrollo, datos de configuracion y control de versiones.

Ambos se cubren en detalle en la [guia de Scala](/guides/scala/).

## Pruebalo en el navegador

Puedes experimentar con el soporte SQL de PetraDB ahora mismo — sin necesidad de configurar un proyecto. El [playground](/playground/) ejecuta el motor completo en tu navegador.

## Siguientes pasos

La [guia de Scala](/guides/scala/) cubre bases de datos persistentes y de texto, ejecucion de SQL, manejo de resultados y la API completa. Para ejecutar PetraDB como servicio de red, consulta las guias de [Servidor](/guides/server/) y [Cliente](/guides/client/).
