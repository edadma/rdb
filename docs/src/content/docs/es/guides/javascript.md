---
title: Uso con JavaScript / TypeScript
description: Uso de PetraDB desde JavaScript y TypeScript.
---

## Crear una base de datos

Cada instancia de `Session` es una base de datos completamente aislada. Por defecto se ejecuta en memoria, pero puedes elegir almacenamiento persistente o de texto.

```javascript
import { Session } from '@petradb/engine';

// En memoria (por defecto)
const db = new Session();
```

## Modos de almacenamiento

### Memoria (por defecto)

Los datos residen en memoria y se pierden cuando el proceso termina. Funciona en todas partes: Node.js, Deno, Bun y navegadores.

```javascript
const db = new Session();
// o explicitamente:
const db = new Session({ storage: 'memory' });
```

### Persistente (Node.js)

Almacenamiento durable a prueba de fallos en un solo archivo binario, usando paginas copy-on-write y encabezados con doble buffer. Si el archivo existe se abre; de lo contrario se crea una nueva base de datos.

```javascript
const db = new Session({ storage: 'persistent', path: './mydb' });

// Opcional: establecer tamano de pagina (por defecto 4096)
const db = new Session({ storage: 'persistent', path: './mydb', pageSize: 8192 });
```

### Texto (Node.js)

Almacena datos en un archivo de texto legible por humanos (`.ptxt`). Util para depuracion, control de versiones o edicion manual de datos.

```javascript
const db = new Session({ storage: 'text', path: './data.ptxt' });
```

### Cierre

Llama a `await db.close()` para liberar descriptores de archivo cuando uses almacenamiento persistente o de texto. Para bases de datos en memoria, `close()` es una operacion sin efecto.

```javascript
const db = new Session({ storage: 'persistent', path: './mydb' });
// ... usar la base de datos ...
await db.close();
```

## Ejecutar SQL

Usa `db.execute(sql)` para ejecutar una o mas sentencias SQL separadas por punto y coma. Retorna una promesa que se resuelve en un array de objetos de resultado.

```javascript
await db.execute(`
  CREATE TABLE users (
    id SERIAL,
    name TEXT NOT NULL,
    email TEXT,
    PRIMARY KEY (id)
  )
`);

await db.execute("INSERT INTO users (name, email) VALUES ('Alice', 'alice@example.com')");
await db.execute("INSERT INTO users (name, email) VALUES ('Bob', 'bob@example.com')");

const [{ rows, fields }] = await db.execute('SELECT * FROM users');
// rows:   [{ id: 1, name: 'Alice', email: 'alice@example.com' }, ...]
// fields: [{ name: 'id', dataType: 'serial' }, { name: 'name', dataType: 'text' }, ...]
```

## Modos de fila

Por defecto, las filas SELECT se retornan como objetos con claves por nombre de columna. Usa `rowMode: 'array'` para arrays posicionales en su lugar.

```javascript
// Establecer por defecto para todas las consultas
const db = new Session({ rowMode: 'array' });

// O sobreescribir por llamada
const [{ rows }] = await db.execute('SELECT id, name FROM users', { rowMode: 'array' });
// rows: [[1, 'Alice'], [2, 'Bob']]
```

## Sentencias preparadas

Usa `db.prepare(sql)` con marcadores de parametro `$1`, `$2`, .... Retorna un objeto de sentencia con un metodo `execute(params, options?)`.

```javascript
const stmt = db.prepare('SELECT * FROM users WHERE id = $1');
const [{ rows }] = await stmt.execute([42]);

// Con opciones
const [{ rows }] = await stmt.execute([42], { rowMode: 'array' });
```

El `PREPARE` / `EXECUTE` / `DEALLOCATE` a nivel SQL tambien es soportado — consulta la [referencia de Transacciones](/reference/transactions/) para mas detalles.

Consulta la [referencia de la API JavaScript](/reference/api-javascript/) para los tipos de resultado, mapeo de valores y las interfaces TypeScript completas.

## TypeScript

Se incluyen definiciones de tipos completas. Usa uniones discriminadas para estrechar los tipos de resultado:

```typescript
import { Session, ExecuteResult } from '@petradb/engine';

const db = new Session();
const results: ExecuteResult[] = await db.execute('SELECT * FROM users');

for (const result of results) {
  if (result.command === 'select') {
    // result.rows y result.fields estan tipados aqui
  }
}
```

## Ejemplo completo

```javascript
import { Session } from '@petradb/engine';

const db = new Session();

await db.execute(`
  CREATE TYPE status AS ENUM ('active', 'inactive');
  CREATE TABLE products (
    id SERIAL,
    name TEXT NOT NULL,
    price NUMERIC(10,2),
    status status DEFAULT 'active',
    tags JSON,
    created_at TIMESTAMP,
    PRIMARY KEY (id)
  )
`);

await db.execute(`
  INSERT INTO products (name, price, tags, created_at) VALUES
    ('Laptop', 999.99, '["electronics", "computers"]', '2025-01-15 10:30:00');
  INSERT INTO products (name, price, tags, created_at) VALUES
    ('Coffee', 4.50, '["food", "organic"]', '2025-01-16 08:00:00')
`);

const [{ rows }] = await db.execute(`
  SELECT name, price FROM products
  WHERE price > 10
  ORDER BY price DESC
`);

console.log(rows); // [{ name: 'Laptop', price: 999.99 }]
```

## Manejo de errores

```javascript
async function safeExecute(db, sql) {
  try {
    return await db.execute(sql);
  } catch (error) {
    console.error('SQL Error:', error.message);
    return null;
  }
}
```

## Notas de plataforma

- El modo en memoria funciona en todas partes: Node.js, Deno, Bun y navegadores (con bundlers)
- El almacenamiento persistente y de texto requiere Node.js (usan el sistema de archivos)
- No se requieren dependencias externas ni modulos nativos
- Definiciones TypeScript incluidas
- Considera Web Workers para conjuntos de datos grandes en navegadores
