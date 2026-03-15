---
title: Registro de cambios
---

## v1.5-20260315

### PL/pgSQL — Procedimientos almacenados, funciones y triggers

Soporte completo de lenguaje procedural en bloques DO, funciones almacenadas y procedimientos almacenados:

- **Bloques DO** — bloques PL/pgSQL anonimos con DECLARE, BEGIN...END
- **Funciones almacenadas** — `CREATE FUNCTION name(params) RETURNS type AS $$ ... $$ LANGUAGE plpgsql`, invocables en cualquier expresion SQL
- **Procedimientos almacenados** — `CREATE PROCEDURE name(params) AS $$ ... $$ LANGUAGE plpgsql`, invocados con `CALL`
- **Triggers** — `CREATE TRIGGER name BEFORE|AFTER INSERT|UPDATE|DELETE ON table FOR EACH ROW EXECUTE FUNCTION func()`
  - Los triggers BEFORE pueden cancelar operaciones retornando NULL
  - Los triggers AFTER se ejecutan despues de la operacion
  - Los triggers se disparan para INSERT, UPDATE, DELETE y COPY FROM
  - Variables TG_OP, TG_TABLE_NAME, OLD, NEW disponibles
- **Flujo de control** — IF/ELSIF/ELSE, WHILE LOOP, FOR range/query LOOP, RETURN, RAISE NOTICE/EXCEPTION, PERFORM, EXCEPTION WHEN
- **Persistencia** — funciones, procedimientos y triggers sobreviven al cierre y reapertura en PersistentDB y TextDB
- **OR REPLACE** — sobreescribir funciones y procedimientos existentes

### Funciones nativas definidas por el usuario

Registrar callbacks del lenguaje anfitrion como funciones SQL, invocables desde consultas, triggers y procedimientos:

- **Scala** — `db.registerScalarFunction("name", { args => result })`
- **JavaScript** — `session.registerFunction("name", (args) => result)`
- **C** — `petradb_create_function(db, "name", nargs, user_data, callback)` con API de valores/contexto tipados al estilo SQLite

### Biblioteca C y API de cursores

- **Biblioteca compartida nativa** — `libpetradb-engine.so` via Scala Native con funciones `@exported` invocables desde C
- **API C al estilo SQLite** — `petradb_open`, `petradb_exec`, `petradb_prepare/step/finalize`, accesores de columna tipados
- **Funciones definidas por el usuario** — `petradb_value_int/double/text`, `petradb_result_int/double/text/null/error`, `petradb_user_data`
- **API de cursores** — `session.openCursor(sql)` para iteracion fila por fila con `step()`, accesores de columna tipados, `fetch(n)`, `move(n)`, consultas parametrizadas
- **Encabezado C** — `petradb.h` con documentacion completa de la API
- **Suite de pruebas C** — 67 pruebas
- **Prueba FFI de Rust** — 38 pruebas demostrando interoperabilidad entre lenguajes

### Tablas virtuales

- **Framework extensible** — `CREATE VIRTUAL TABLE name USING module(args)`, solo lectura, aparece en SHOW TABLES
- **Modulo CSV integrado** — `CREATE VIRTUAL TABLE t USING csv('file.csv')` con opciones de encabezado/delimitador
- **Modulos personalizados** — `db.registerVirtualTableModule("name", module)` en la API de Scala

### Funcion de tabla csv_file()

Consultar archivos CSV directamente sin importar:

```sql
SELECT * FROM csv_file('data.csv');
SELECT e.name, d.dept FROM csv_file('employees.csv') e
  JOIN csv_file('departments.csv') d ON e.dept_id = d.id;
```

### Indices avanzados

- **Indices parciales** — `CREATE INDEX ... WHERE condition` — solo indexar filas que coincidan con el predicado
- **Indices de expresion** — `CREATE INDEX ... ON table ((expr))` — indexar valores calculados como `lower(email)`
- **Combinados** — los indices parciales y de expresion funcionan juntos

### Funciones de ventana

- **FIRST_VALUE(expr)** — valor en la primera fila del marco de ventana
- **LAST_VALUE(expr)** — valor en la ultima fila del marco de ventana
- **NTH_VALUE(expr, n)** — valor en la fila n del marco

### DELETE ... USING

Eliminaciones multi-tabla con sintaxis PostgreSQL:

```sql
DELETE FROM orders USING customers
WHERE orders.customer_id = customers.id AND customers.status = 'inactive';
```

### Quarry — Constructor de consultas tipado basado en AST

Nuevo paquete `@petradb/quarry`: constructor de consultas con seguridad de tipos que genera objetos AST (no cadenas SQL):

- Definicion de esquema con 21 tipos de columna
- CRUD completo: select, insert, update, delete con referencias de columna tipadas
- Joins: inner, left, right, full outer, cross con resultados tipados
- Expresiones: mas de 50 operadores, agregados, CASE/CAST/EXISTS, subconsultas
- Upsert: `onConflictDoNothing()`, `onConflictDoUpdate()`
- Alias de tablas para auto-joins
- Transacciones, RETURNING, DISTINCT ON
- Pruebas de tipos en tiempo de compilacion para todas las funcionalidades
- Operaciones de conjuntos: UNION, INTERSECT, EXCEPT
- Funciones de ventana, CTEs, helpers escalares con nombre

### Correcciones de errores

- **ByteaValue** — `ARRAY[...]` en columnas BYTEA ahora produce correctamente `ByteaValue` en lugar de `ArrayValue`
- **Tipos de resultado JS/Client** — se agregaron los manejadores de tipos de resultado PL/pgSQL faltantes (DoBlockResult, CreateFunctionResult, etc.) para prevenir fallos por coincidencia no exhaustiva
- **Codecs** — se agrego serializacion para todos los nuevos tipos de resultado para comunicacion cliente/servidor
- **llms.txt** — se corrigio el campo `type` a campo `command`, se actualizaron todos los tipos de resultado y funcionalidades

### Cambio de nombre de modulo

- **shared → common** — se renombro el modulo de tipos compartidos de `petradb-shared` a `petradb-common`

### Documentacion

- Nueva pagina de referencia de **PL/pgSQL** (triggers, funciones, procedimientos, flujo de control)
- Nueva pagina de referencia de la **API de C** (interfaz completa al estilo SQLite)
- Nuevas guias de **Primeros pasos** para Java (JDBC) y C
- Documentacion DDL actualizada: indices parciales/de expresion, restricciones CHECK, triggers, rutinas almacenadas
- Documentacion DML actualizada: DELETE...USING, csv_file(), tablas virtuales
- Documentacion de la API JS/Scala actualizada: registerFunction, tipos de resultado
- Pagina de inicio: cuatro botones de primeros pasos (JS, Java, Scala, C)
- llms.txt reescrito con todas las funcionalidades actuales

### Actualizaciones de version

| Componente | Maven Central | npm |
|-----------|---------------|-----|
| common | 1.5.0 | — |
| engine | 1.5.0 | @petradb/engine 1.5.0 |
| client | 1.5.0 | @petradb/client 1.5.0 |
| server | 1.5.0 | @petradb/server 1.5.0 |
| cli | 1.5.0 | @petradb/cli 1.5.0 |
| jdbc | 1.5.0 | — |
| drizzle | — | @petradb/drizzle 1.5.0 |
| knex | — | @petradb/knex 1.5.0 |
| lucid | — | @petradb/lucid 1.5.0 |
| quarry | — | @petradb/quarry 1.5.0 |

## v1.4-20260314

### Correcciones de errores y mejoras

- **Correccion de subconsulta IN correlacionada** — `IN (SELECT ...)` correlacionado con tablas indexadas ahora funciona correctamente
- **Resolucion de columnas calificadas** — correcciones para referencias de columna ambiguas en joins complejos
- **Manejo de IN/ANY vacios** — casos limite de `IN ()` y `= ANY('{}')` resueltos
- **Limpieza de CASCADE en claves foraneas** — `DROP TABLE ... CASCADE` ahora elimina correctamente las restricciones FK en tablas hijas
- **DROP TABLE IF EXISTS ... CASCADE** — combinar `IF EXISTS` con `CASCADE` ya no causa un error de analisis
- **Conversion de literales de array** — soporte para la sintaxis de literales de array PostgreSQL `'{1,2,3}'::integer[]`
- **Correcciones de consultas parametrizadas** — mejor vinculacion de parametros para subconsultas y rutas correlacionadas

### Actualizaciones de version

| Componente | Maven Central | npm |
|-----------|---------------|-----|
| shared | 1.4.2 | — |
| engine | 1.4.9 | @petradb/engine 1.4.3 |
| client | 1.4.2 | @petradb/client 1.4.2 |
| server | — | @petradb/server 1.4.3 |
| cli | — | @petradb/cli 1.4.3 |
| jdbc | 1.4.3 | — |
| drizzle | — | @petradb/drizzle 1.4.3 |

## v1.4-20260312

### Expresiones de tabla comunes (CTEs)

Soporte completo de CTE con `WITH` y `WITH RECURSIVE`.

**CTEs no recursivas** — subconsultas nombradas para legibilidad y reutilizacion:

```sql
WITH active_orders AS (
  SELECT * FROM orders WHERE status = 'active'
)
SELECT customer_id, SUM(amount)
FROM active_orders
GROUP BY customer_id;
```

Se pueden definir multiples CTEs en una sola consulta, y las CTEs posteriores pueden referenciar a las anteriores. Se soportan alias de columna: `WITH t(x, y) AS (...)`. Las CTEs ocultan nombres de tabla si comparten el mismo nombre.

**CTEs recursivas** — consultas iterativas para datos jerarquicos y de grafos:

```sql
WITH RECURSIVE descendants(id, name, depth) AS (
  SELECT id, name, 0 FROM employees WHERE manager_id IS NULL
  UNION ALL
  SELECT e.id, e.name, d.depth + 1
  FROM employees e INNER JOIN descendants d ON e.manager_id = d.id
)
SELECT name, depth FROM descendants ORDER BY depth, name;
```

Se soportan tanto `UNION ALL` (mantener duplicados) como `UNION` (deduplicados). Maximo 1000 iteraciones como limite de seguridad.

### Funciones de ventana

Soporte completo de funciones de ventana con tres categorias:

**Funciones de clasificacion** — `ROW_NUMBER()`, `RANK()`, `DENSE_RANK()` con `PARTITION BY` y `ORDER BY`:

```sql
SELECT name, department, salary,
  RANK() OVER (PARTITION BY department ORDER BY salary DESC) AS dept_rank
FROM employees;
```

**Funciones de valor** — `LAG()`, `LEAD()`, `NTILE()` con desplazamiento y valores por defecto configurables:

```sql
SELECT name, salary,
  LAG(salary, 1, 0) OVER (ORDER BY salary) AS prev_salary,
  NTILE(4) OVER (ORDER BY salary) AS quartile
FROM employees;
```

**Funciones de ventana con agregados** — cualquier agregado (`SUM`, `COUNT`, `AVG`, `MIN`, `MAX`, etc.) con `OVER()`, incluyendo especificaciones de marco:

```sql
SELECT name, salary,
  SUM(salary) OVER (ORDER BY salary ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS running_total,
  AVG(salary) OVER (PARTITION BY department) AS dept_avg
FROM employees;
```

Limites de marco: `UNBOUNDED PRECEDING`, `UNBOUNDED FOLLOWING`, `CURRENT ROW`, `N PRECEDING`, `N FOLLOWING`. Sin clausula de marco, las funciones de ventana con agregados calculan sobre toda la particion.

### Clausula FILTER en agregados

`FILTER (WHERE ...)` en funciones de agregado, tanto en consultas agrupadas como en funciones de ventana:

```sql
SELECT
  COUNT(*) AS total,
  COUNT(*) FILTER (WHERE status = 'active') AS active_count,
  SUM(amount) FILTER (WHERE amount > 100) OVER (ORDER BY created_at
    ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW) AS running_high_value
FROM orders;
```

### Optimizacion de hash join

Los equijoins sin indice ahora usan una estrategia de hash join en lugar de un producto cartesiano, reduciendo la complejidad del join de O(n*m) a O(n+m). Se aplica a joins INNER, LEFT, RIGHT y FULL. Los index nested loop joins siguen siendo preferidos cuando hay un indice disponible. Las condiciones de join que no son equijoins recurren al producto cartesiano. Visible en la salida de `EXPLAIN` como `Hash Join`, `Hash Left Join`, `Hash Right Join`, `Hash Full Join`.

### Columnas generadas

Columnas calculadas con `GENERATED ALWAYS AS (expr) STORED`:

```sql
CREATE TABLE products (
  price NUMERIC,
  tax_rate NUMERIC DEFAULT 0.08,
  total NUMERIC GENERATED ALWAYS AS (price * (1 + tax_rate)) STORED
);
```

Las columnas generadas se recalculan en INSERT y UPDATE. No se pueden establecer directamente.

### Ordenamiento de nulos en ORDER BY

`ORDER BY` ahora usa por defecto el ordenamiento de nulos del estandar SQL: `ASC` → `NULLS LAST`, `DESC` → `NULLS FIRST`. Se soportan las sobreescrituras explicitas `NULLS FIRST` / `NULLS LAST`.

### Soporte de secuencias

Soporte completo de secuencias compatible con PostgreSQL. `CREATE SEQUENCE` y `DROP SEQUENCE` con opciones (`INCREMENT BY`, `START WITH`, `MINVALUE`, `MAXVALUE`, `CYCLE`, `IF NOT EXISTS` / `IF EXISTS`). Funciones de secuencia: `nextval()`, `currval()`, `setval()`, `lastval()`.

Las columnas `SERIAL`, `SMALLSERIAL` y `BIGSERIAL` ahora crean secuencias de respaldo (nombradas `<tabla>_<columna>_seq`), coincidiendo con el comportamiento de PostgreSQL. `DROP TABLE` propaga la eliminacion a las secuencias asociadas. `TRUNCATE` reinicia las secuencias de respaldo. El estado de las secuencias es completamente transaccional — `ROLLBACK` restaura los contadores de secuencia. Las bases de datos persistentes serializan el estado de las secuencias en el catalogo.

Nuevos comandos SQL: `SHOW SEQUENCES`, `SHOW INDEXES` (todos los indices de todas las tablas).

CLI: nuevos meta-comandos `\ds` (listar secuencias) y `\di` (listar indices).

### Clausula USING en CREATE INDEX

La sintaxis `CREATE INDEX ... USING btree` ahora es aceptada (btree es el unico metodo soportado). Esto mejora la compatibilidad con DDL generado por PostgreSQL y ORMs.

### Correcciones de errores

- `ORDER BY` con valores NULL: los comparadores ahora retornan 0 cuando ambos valores son NULL, corrigiendo resultados de ordenamiento no deterministas con multiples claves de ordenamiento
- Resolucion de alias en `ORDER BY`: los alias de SELECT (ej. `SELECT x AS y ... ORDER BY y`) ahora se resuelven correctamente en consultas no agrupadas, tanto con como sin funciones de ventana
- Manejo de nulos en `Type.convert()`: los valores NULL pasados a traves de conversion de tipo (ej. via parametros de sentencias preparadas en UPDATE SET) ahora se preservan como NULL en lugar de convertirse a la representacion de texto del tipo. Corregido en 13 tipos: TEXT, VARCHAR, CHAR, UUID, TIMESTAMP, DATE, TIME, TIMETZ, INTERVAL, TIMESTAMPTZ, BYTEA, JSON, ENUM
- Advertencia de coincidencia exhaustiva en el parser de ORDER BY para clausula de nulos
- Errores silenciosos en la terminal del playground para excepciones sincronas

### Actualizaciones de version

Todos los componentes actualizados a 1.4.1:

| Componente | Maven Central | npm |
|-----------|---------------|-----|
| shared | 1.4.1 | — |
| engine | 1.4.1 | @petradb/engine 1.4.1 |
| client | 1.4.1 | @petradb/client 1.4.1 |
| server | 1.4.1 | @petradb/server 1.4.1 |
| cli | 1.4.1 | @petradb/cli 1.4.1 |
| jdbc | 1.4.1 | — |
| knex | — | @petradb/knex 1.4.0 |
| lucid | — | @petradb/lucid 1.4.0 |
| drizzle | — | @petradb/drizzle 1.4.1 |

## v1.3-20260309

### DDL transaccional

Las sentencias DDL (CREATE TABLE, CREATE INDEX, DROP TABLE, etc.) ahora son completamente soportadas dentro de transacciones y se revierten atomicamente con DML. DDL y DML pueden intercalarse libremente dentro de un solo bloque BEGIN/COMMIT. Tanto MemoryDB como PersistentDB capturan una instantanea completa del catalogo en el momento del BEGIN y la restauran en ROLLBACK.

### Consultas relacionales de Drizzle

Soporte completo para consultas relacionales de Drizzle ORM (`db.query.*.findMany()`, `db.query.*.findFirst()`). Se agregaron las funciones escalares `json_build_array` y `json_build_object`, y se corrigio la sustitucion de parametros en subconsultas LATERAL.

### Actualizaciones de version

| Componente | Maven Central | npm |
|-----------|---------------|-----|
| engine | 1.3.1 | @petradb/engine 1.3.3 |
| server | 1.3.1 | @petradb/server 1.3.1 |
| cli | 1.3.1 | @petradb/cli 1.3.1 |
| jdbc | 1.3.1 | — |
| drizzle | — | @petradb/drizzle 1.3.1 |

## v1.3-20260308

### Soporte de esquemas

Espacios de nombres de esquema al estilo PostgreSQL. Cada base de datos tiene un esquema `public` por defecto; los nombres de tabla no calificados se resuelven a `public`. Los nombres calificados con esquema (`schema.tabla`) funcionan en todas las sentencias DDL y DML — CREATE TABLE, INSERT, UPDATE, DELETE, SELECT, ALTER TABLE, DROP TABLE, TRUNCATE, CREATE INDEX y COPY.

```sql
CREATE SCHEMA inventory;
CREATE TABLE inventory.products (id SERIAL PRIMARY KEY, name TEXT);
INSERT INTO inventory.products (name) VALUES ('Widget');
SELECT * FROM inventory.products;
```

### Tablas virtuales information_schema

`information_schema.schemata`, `information_schema.tables` e `information_schema.columns` ahora son consultables. Las tablas calificadas con esquema reportan su `table_schema` correcto. Estas vistas se generan dinamicamente a partir de los metadatos de la base de datos.

### Migraciones de Drizzle ORM

Nueva funcion `migrate()` en `@petradb/drizzle` que aplica archivos de migracion de Drizzle Kit. Lee el `meta/_journal.json` y ejecuta los archivos de migracion SQL en orden, rastreando las migraciones aplicadas en `drizzle.__drizzle_migrations`.

```typescript
import { migrate } from "@petradb/drizzle";
await migrate(db, { migrationsFolder: "./drizzle" });
```

### Mejoras de metadatos JDBC

`DatabaseMetaData.getColumns()` ahora retorna valores precisos de `COLUMN_SIZE`, `DECIMAL_DIGITS` y `CHAR_OCTET_LENGTH` basados en el tipo de columna y las declaraciones de precision/escala.

### Actualizaciones de version

Todos los componentes actualizados a 1.3.0:

| Componente | Maven Central | npm |
|-----------|---------------|-----|
| shared | 1.3.0 | — |
| engine | 1.3.0 | @petradb/engine 1.3.0 |
| client | 1.3.0 | @petradb/client 1.3.0 |
| server | 1.3.0 | @petradb/server 1.3.0 |
| cli | 1.3.0 | @petradb/cli 1.3.0 |
| jdbc | 1.3.0 | — |
| knex | — | @petradb/knex 1.3.0 |
| lucid | — | @petradb/lucid 1.3.0 |
| drizzle | — | @petradb/drizzle 1.3.0 |

## v1.2-20260308

### Reescritura del driver Drizzle ORM

`@petradb/drizzle` reescrito de un wrapper `drizzle-orm/pg-proxy` a un driver de dialecto PostgreSQL personalizado que extiende `PgSession`/`PgPreparedQuery`/`PgTransaction` directamente. Esto proporciona paridad completa de funcionalidades con `drizzle-orm/node-postgres`:

- `db.transaction()` con commit/rollback automatico
- `tx.rollback()` para rollback explicito
- `returning()` en insert/update/delete (incluyendo seleccion parcial de columnas)
- Soporte de consultas relacionales (pendiente soporte del motor para `json_build_array`/`json_agg`)

### Coercion de tipos: parametros de texto a columnas NUMERIC

`NumericType.convert` ahora acepta `TextValue` y lo analiza como `BigDecimal`, coincidiendo con el comportamiento de coercion existente de `IntegerType`, `BigintType`, `SmallintType` y `DoubleType`. Esto corrige INSERT/UPDATE parametrizados via ORMs que envian valores numericos como texto (comportamiento estandar del protocolo de cable PostgreSQL).

### Logica de tres valores para NULL

Logica completa de tres valores SQL para el manejo de NULL:

- Los operadores de comparacion (`=`, `!=`, `<`, `>`, `<=`, `>=`) retornan NULL cuando cualquier operando es NULL
- `AND`/`OR` implementan tablas de verdad de tres valores apropiadas (ej. `FALSE AND NULL` → `FALSE`, `TRUE OR NULL` → `TRUE`)
- `IN`/`NOT IN` propagan NULL correctamente (ej. `3 NOT IN (1, 2, NULL)` → desconocido)
- Las operaciones aritmeticas (`+`, `-`, `*`, `/`, `%`) y la concatenacion de cadenas (`||`) propagan NULL
- `LIKE` maneja operandos NULL

### Gramatica de expresiones unificada

Las jerarquias separadas `expression` y `booleanExpression` del parser SQL se han fusionado en una sola sintaxis de expresiones. Los operadores booleanos (`AND`, `OR`, `NOT`) ahora son operadores regulares en la cadena de precedencia. Esto permite expresiones booleanas en cualquier lugar donde una expresion sea valida (ej. `SELECT a > 5 AND b < 10`).

### Validacion anticipada de referencias de columna

Las referencias de columna en `WHERE`, `GROUP BY`, `HAVING` y `ORDER BY` ahora se validan anticipadamente en el momento de construccion del plan de consulta, detectando columnas inexistentes incluso en tablas vacias u ordenamientos de una sola fila. Anteriormente, las referencias incorrectas solo se detectaban en tiempo de evaluacion por fila, por lo que las consultas contra tablas vacias tenian exito silenciosamente.

### Correcciones de errores

- Restriccion NOT NULL no aplicada en UPDATE
- Restriccion UNIQUE rechazaba multiples NULLs (estandar SQL: los NULLs son distintos)
- Columnas duplicadas en la lista de columnas de INSERT no detectadas
- `SUM`/`AVG`/`MIN`/`MAX` en tabla vacia retornaba 0 en lugar de NULL
- `LIKE '_'` coincidia con cadena vacia
- `LIMIT 0` lanzaba un error

### Actualizaciones de version

| Componente | Maven Central | npm |
|-----------|---------------|-----|
| shared | 1.2.3 | — |
| engine | 1.2.9 | @petradb/engine 1.2.16 |
| client | 1.2.5 | @petradb/client 1.2.5 |
| server | 1.2.6 | @petradb/server 1.2.9 |
| cli | 1.2.8 | @petradb/cli 1.2.9 |
| jdbc | 1.2.13 | — |
| knex | — | @petradb/knex 1.2.2 |
| lucid | — | @petradb/lucid 1.2.1 |
| drizzle | — | @petradb/drizzle 1.2.2 |

## v1.2-20260307

### SQL: Palabra clave `DEFAULT` en INSERT VALUES

`INSERT INTO t (id, name) VALUES (DEFAULT, 'Alice')` ahora funciona. La palabra clave `DEFAULT` del estandar SQL era previamente rechazada por el parser, rompiendo las sentencias INSERT generadas por ORMs que pasan explicitamente `DEFAULT` para columnas serial o con valores por defecto.

### Integracion Drizzle ORM

Nuevo paquete `@petradb/drizzle` que proporciona un driver de [Drizzle ORM](https://orm.drizzle.team) con una implementacion de dialecto PostgreSQL personalizada. Soporta definiciones de esquema con `pgTable`, insert/select/update/delete, clausulas returning, `db.transaction()` con commit/rollback automatico, y consultas con seguridad de tipos. Paridad completa de funcionalidades con `drizzle-orm/node-postgres`.

### Actualizaciones de version para paquetes dependientes

Engine, server, cli y jdbc actualizados para incluir la correccion de la palabra clave DEFAULT.

| Componente | Maven Central | npm |
|-----------|---------------|-----|
| shared | 1.2.3 | — |
| engine | 1.2.7 | @petradb/engine 1.2.14 |
| client | 1.2.5 | @petradb/client 1.2.5 |
| server | — | @petradb/server 1.2.7 |
| cli | — | @petradb/cli 1.2.7 |
| jdbc | 1.2.11 | — |
| knex | — | @petradb/knex 1.2.2 |
| lucid | — | @petradb/lucid 1.2.1 |
| drizzle | — | @petradb/drizzle 1.2.0 |

## v1.2-20260306

### API JS: `close()` retorna `Promise<void>`
`Session.close()` ahora retorna `Promise<void>` en lugar de `void`, coincidiendo con la API del modulo cliente para intercambiabilidad.

### Analisis de timestamps
`parseTimestamp` ahora maneja el sufijo `Z`, desplazamientos `+/-HH:MM`, milisegundos y timestamps separados por espacio con informacion de zona horaria. Elimina la zona horaria a `LocalDateTime` para columnas `TIMESTAMP`.

### Completitud de la fachada JS
`toJS` y `typeString` ahora manejan `DateValue`, `TimeValue`, `TimestampTZValue`, `TimeTZValue`, `IntervalValue` y `ByteaValue`.

### SQL: estrella calificada (`table.*`)
La sintaxis `SELECT t.*` ahora funciona en consultas, incluyendo joins y expresiones mixtas.

### Coercion de tipos en comparaciones
- `NumberValue` y `TextValue` ahora pueden compararse entre tipos (parametros de texto vs columnas numericas y viceversa)
- `TimestampValue` ahora puede compararse contra `TextValue` analizando el texto como timestamp

### Driver Knex: vinculacion de Date
`_sanitizeBindings` convierte objetos JS `Date` a cadenas ISO antes de pasarlos al motor, previniendo `DateTimeParseException` en el formato de `Date.toString()`.

| Componente | Maven Central | npm |
|-----------|---------------|-----|
| shared | 1.2.3 | — |
| engine | 1.2.6 | @petradb/engine 1.2.13 |
| client | 1.2.5 | @petradb/client 1.2.5 |
| server | — | @petradb/server 1.2.5 |
| cli | — | @petradb/cli 1.2.5 |
| jdbc | 1.2.10 | — |
| knex | — | @petradb/knex 1.2.2 |
| lucid | — | @petradb/lucid 1.2.1 |

## v1.2-20260305

### Driver JDBC
- Publicacion de fat jar — `io.github.edadma:petradb-jdbc` ahora es un unico jar autocontenido en Maven Central
- URLs de conexion limpias — `jdbc:petradb:memory`, `jdbc:petradb:file:/path`, `jdbc:petradb://host:port`
- Auto-descubrimiento ServiceLoader — `DriverManager.getConnection()` funciona sin `Class.forName`
- Cadenas de version de metadatos fijas corregidas

### Motor JS/TS (`@petradb/engine`)
- Se agregaron `CreateViewResult`, `DropViewResult`, `ExplainResult`, `CopyResult` a la fachada JS
- Se agregaron `ExplainResult` y `CopyResult` a las definiciones de tipos TypeScript

### Documentacion
- Nueva guia de Knex.js con ejemplos completos
- Documentacion JDBC: se agregaron fragmentos de instalacion Maven/Gradle/sbt, se corrigio el numero de puerto

### Infraestructura
- `petradb-shared` ahora publicable en Maven Central
- Script de prueba de humo post-publicacion cubriendo artefactos npm, Scala y JDBC

| Componente | Maven Central | npm |
|-----------|---------------|-----|
| shared | 1.2.1 | — |
| engine | 1.2.2 | @petradb/engine 1.2.5 |
| client | 1.2.3 | @petradb/client 1.2.3 |
| server | — | @petradb/server 1.2.3 |
| cli | — | @petradb/cli 1.2.3 |
| jdbc | 1.2.6 | — |
| knex | — | @petradb/knex 1.2.0 |

## v1.2.2

### Reestructuracion de subpaquete del motor
- Motor movido a subpaquete `io.github.edadma.petradb.engine`
- Nuevo trait compartido `Session` extendido tanto por engine como por client

### Soporte de cliente CLI
- Conectar a un servidor PetraDB remoto: `petradb --host localhost --port 5480`
- Flags `--user` y `--password` para autenticacion
- Los meta-comandos funcionan a traves de la red via SQL

### SQL
- El comando `SHOW VIEWS` retorna nombres y definiciones de vistas

### Dialecto Knex
- Adaptador de dialecto `@petradb/knex` para usar el constructor de consultas Knex.js con PetraDB

### Correcciones
- Correccion de publicacion npm del cliente
- Correccion de publicacion npm del CLI
- Correccion de cadenas de version de metadatos JDBC fijas
- Mas de 1013 pruebas pasando en JVM, JS y Native

## v1.2

### Driver JDBC
- Publicado en Maven Central como `petradb-jdbc`
- `getGeneratedKeys()`, `addBatch()`/`executeBatch()`, metadatos de FK/indices para DBeaver
- Modo archivo (embebido) y modo servidor (red) para conexiones

### Motor SQL
- `COPY FROM/TO` para importacion/exportacion CSV
- `CREATE TEMP TABLE`, `CREATE/DROP VIEW`
- Introspeccion con `SHOW FOREIGN KEYS`/`SHOW INDEXES`
- Optimizacion de index nested loop join para equijoins
- Parser migrado a fastparse

### Servidor
- Soporte CORS con configuracion TOML
- `max_sessions` configurable, puerto por defecto 5480
- Plataforma de servidor JS con backend HTTP Node.js

### Cliente
- Nuevo paquete npm `@petradb/client` con fachada JS
- Clase `Session` con `connect()`/`execute()`/`close()` retornando Promises

### CLI
- Comandos `\timing`, `\copy`
- Historial persistente en Native

### Compilacion
- Scala 3.8.2, sbt 1.12.4
- 1000 pruebas pasando en JVM, JS y Native

## v1.1.0

### TextDB — persistencia en archivo de texto editable
Un nuevo backend de almacenamiento que persiste la base de datos como un archivo de texto `.ptxt`. Carga en memoria al abrir y reescribe el archivo despues de cada cambio.

### Upsert — `ON CONFLICT DO UPDATE`
Semantica de insertar-o-actualizar con la pseudo-tabla `EXCLUDED`.

### Jerarquia de excepciones mejorada
Clases de excepcion tipadas reemplazan las llamadas genericas a `problem()`.

### ALTER TABLE centralizado
`DB.alterTable()` ahora centraliza todo el despacho de ALTER TABLE.

## v1.0.1

- Renombrar `ConnectSQL` a `Session` en `@petradb/engine`
- API `execute()` asincrona retornando `Promise<ExecuteResult[]>`
- Nuevo paquete `@petradb/client` para uso en red
- Formatos de respuesta alineados entre engine y server

## v1.0.0

Primera version estable.

- Motor SQL multiplataforma (JVM, JavaScript, Native)
- Sintaxis compatible con PostgreSQL
- Almacenamiento en memoria y persistente (a prueba de fallos)
- DDL, DML, joins, subconsultas, agregaciones, transacciones
- Operadores JSONB, tipos de array, restricciones CHECK
- 879 pruebas pasando
