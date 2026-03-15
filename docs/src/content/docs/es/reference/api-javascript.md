---
title: JavaScript / TypeScript
description: Referencia de la API JavaScript y TypeScript para PetraDB.
---

## Instalacion

```bash
npm install @petradb/engine
```

## Session

### `new Session(options?)`

Crea una nueva instancia de base de datos.

| Opcion | Tipo | Por defecto | Descripcion |
|--------|------|---------|-------------|
| `rowMode` | `'object' \| 'array'` | `'object'` | Formato de fila por defecto para resultados SELECT |
| `storage` | `'memory' \| 'persistent' \| 'text'` | `'memory'` | Backend de almacenamiento |
| `path` | `string` | — | Ruta de archivo (requerida para persistent y text) |
| `pageSize` | `number` | `4096` | Tamano de pagina en bytes (solo persistent) |

```javascript
// En memoria (por defecto)
const db = new Session();

// Almacenamiento persistente a prueba de fallos (Node.js)
const db = new Session({ storage: 'persistent', path: './mydb' });

// Archivo de texto legible por humanos (Node.js)
const db = new Session({ storage: 'text', path: './data.ptxt' });
```

Para almacenamiento persistente, PetraDB detecta automaticamente si crear un nuevo archivo o abrir uno existente.

### `db.close()`

Libera descriptores de archivo. Retorna `Promise<void>`. Requerido para almacenamiento persistente y de texto. Sin efecto (pero aun asincrono) para bases de datos en memoria.

```javascript
await db.close();
```

### `db.execute(sql, options?)`

Ejecuta una o mas sentencias SQL separadas por `;`. Retorna una promesa que se resuelve en un array de resultados.

| Opcion | Tipo | Por defecto | Descripcion |
|--------|------|---------|-------------|
| `rowMode` | `'object' \| 'array'` | por defecto del constructor | Formato de fila para esta llamada |

```javascript
const [{ rows, fields }] = await db.execute('SELECT * FROM users');
```

### `db.prepare(sql)`

Crea una sentencia preparada con marcadores de parametro `$1`, `$2`, .... Retorna un objeto de sentencia con un metodo `execute(params, options?)`.

```javascript
const stmt = db.prepare('SELECT * FROM users WHERE id = $1');
const [{ rows }] = await stmt.execute([42]);

// Con opciones
const [{ rows }] = await stmt.execute([42], { rowMode: 'array' });
```

### `db.registerFunction(name, callback)`

Registra una funcion JavaScript nativa invocable desde SQL, triggers y procedimientos almacenados:

```javascript
db.registerFunction('my_double', (args) => args[0] * 2);

// Ahora utilizable en SQL:
const [{ rows }] = await db.execute('SELECT my_double(21) AS val');
// rows[0].val === 42
```

El callback recibe un array de valores JavaScript (numeros, cadenas, booleanos o `null`) y debe retornar un valor JavaScript.

## Interfaces TypeScript

```typescript
interface SessionOptions {
  rowMode?: 'object' | 'array';
  storage?: 'memory' | 'persistent' | 'text';
  path?: string;
  pageSize?: number;
}

interface ExecuteOptions {
  rowMode?: 'object' | 'array';
}

interface PreparedStatement {
  execute(params?: any[], options?: ExecuteOptions): Promise<ExecuteResult[]>
}

class Session {
  constructor(options?: SessionOptions)
  execute(sql: string, options?: ExecuteOptions): Promise<ExecuteResult[]>
  prepare(sql: string): PreparedStatement
  close(): Promise<void>
}
```

## Tipos de resultado

Cada resultado tiene un campo `command` para discriminacion:

```typescript
// DDL
{ command: 'create table', table: string }
{ command: 'drop table', table: string }
{ command: 'create type', type: string }
{ command: 'drop type', type: string }
{ command: 'create index', index: string }
{ command: 'drop index', index: string }
{ command: 'truncate table', table: string }
{ command: 'alter table' }
{ command: 'create view', view: string }
{ command: 'drop view', view: string }
{ command: 'create sequence', sequence: string }
{ command: 'drop sequence', sequence: string }
{ command: 'create schema', schema: string }
{ command: 'create trigger', trigger: string }
{ command: 'drop trigger', trigger: string }

// PL/pgSQL
{ command: 'do' }
{ command: 'create function', function: string }
{ command: 'drop function', function: string }
{ command: 'create procedure', procedure: string }
{ command: 'drop procedure', procedure: string }
{ command: 'call' }

// Plan de consulta
{ command: 'explain', plan: string }

// DML
{ command: 'insert', result: Record<string, any>, rows: T[], fields: FieldInfo[] }
{ command: 'select', rows: T[], fields: { name: string, dataType: string }[] }
{ command: 'update', rowCount: number }
{ command: 'delete', rowCount: number }
{ command: 'copy', rowCount: number }

// Transacciones
{ command: 'begin' }
{ command: 'commit' }
{ command: 'rollback' }

// Sentencias preparadas
{ command: 'prepare', name: string }
{ command: 'deallocate', name: string }
```

## Mapeo de valores

| Tipo SQL | Tipo JavaScript |
|----------|----------------|
| INT, BIGINT, DOUBLE, NUMERIC | `number` |
| TEXT, CHAR, VARCHAR | `string` |
| BOOLEAN | `boolean` |
| UUID | `string` |
| TIMESTAMP | `Date` |
| ENUM | `string` (etiqueta) |
| JSON array | `Array` |
| JSON object | `Object` |
| NULL | `null` |
