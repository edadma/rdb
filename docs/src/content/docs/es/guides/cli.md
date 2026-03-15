---
title: CLI
description: Shell SQL interactivo y herramientas de linea de comandos para PetraDB.
---

## Instalacion

```bash
npm install -g @petradb/cli
```

## Uso

```bash
petradb [OPCIONES] [ruta]
```

Si se proporciona una ruta, el CLI abre (o crea) una base de datos persistente en esa ubicacion. Sin una ruta, se usa una base de datos en memoria.

### Opciones

| Opcion | Descripcion |
|--------|-------------|
| `-m`, `--memory` | Usar una base de datos en memoria (incluso si se proporciona una ruta) |
| `-e`, `--execute` | Ejecutar SQL y salir (repetible) |
| `-f`, `--file` | Ejecutar un archivo SQL y salir (repetible) |
| `--stdin` | Leer SQL desde stdin y salir |
| `--host` | Conectar a un servidor PetraDB remoto |
| `--port` | Puerto del servidor (por defecto: 5480) |
| `--user` | Nombre de usuario para autenticacion |
| `--password` | Contrasena para autenticacion |

### Ejemplos

```bash
# Iniciar una sesion interactiva en memoria
petradb

# Abrir o crear una base de datos persistente
petradb mydata.db

# Ejecutar SQL de forma no interactiva
petradb mydata.db -e "SELECT * FROM users"

# Ejecutar un archivo y luego una consulta
petradb mydata.db -f schema.sql -e "SELECT count(*) FROM users"

# Enviar SQL desde stdin
echo "SELECT 1 + 1" | petradb --stdin

# Conectar a un servidor remoto
petradb --host myserver.example.com --port 5480

# Conectar con autenticacion
petradb --host localhost --user admin --password secret
```

## Modos de base de datos

La extension del archivo determina el modo de almacenamiento:

| Extension | Modo | Descripcion |
|-----------|------|-------------|
| `.petra` (o cualquier otra) | Persistente | Almacenamiento durable a prueba de fallos via paginas copy-on-write |
| `.ptxt` | Texto | Archivo legible por humanos, reescrito despues de cada cambio |
| *(sin ruta)* | En memoria | Los datos existen solo durante la duracion de la sesion |

## Volcado

El subcomando `dump` imprime el esquema completo y los datos de una base de datos persistente como sentencias SQL.

```bash
petradb dump mydata.db
```

La salida incluye sentencias `CREATE TYPE`, `CREATE TABLE`, `INSERT INTO` y `CREATE VIEW` que pueden recrear la base de datos desde cero.

## REPL interactivo

Cuando se inicia sin `-e`, `-f` o `--stdin`, el CLI entra en un shell SQL interactivo con historial readline.

Escribe SQL terminado con `;` para ejecutar. Se soporta entrada multilinea — aparece un prompt de continuacion (`  -> `) hasta que termines con `;`.

### Meta-comandos

| Comando | Descripcion |
|---------|-------------|
| `\dt` | Listar todas las tablas |
| `\dv` | Listar todas las vistas |
| `\ds` | Listar todas las secuencias |
| `\di` | Listar todos los indices |
| `\d <nombre>` | Describir una tabla o vista (columnas, tipos, nulabilidad) |
| `\i <archivo>` | Ejecutar SQL desde un archivo |
| `\dump` | Imprimir el esquema completo y los datos como SQL |
| `\copy <args>` | Ejecutar una sentencia `COPY` |
| `\timing` | Alternar medicion de tiempo de consulta |
| `\q` | Salir |

### Sesion de ejemplo

```
$ petradb
PetraDB — interactive SQL shell
Type \q to quit, \dt to list tables, \d <table> to describe a table.

petra> CREATE TABLE cities (name TEXT, population INT);
CREATE TABLE
petra> INSERT INTO cities (name, population) VALUES ('Oslo', 709037);
INSERT 0 1
petra> SELECT * FROM cities;
 name | population
------+------------
 Oslo |     709037
(1 row)
petra> \dt
 Table
--------
 cities
(1 row)
petra> \d cities
Table "cities"
 Column     | Type | Nullable
------------+------+----------
 name       | TEXT | null
 population | INT  | null
petra> \timing
Timing is on.
petra> SELECT count(*) FROM cities;
 count
-------
     1
(1 row)
Time: 0.002 s
petra> \q
```
