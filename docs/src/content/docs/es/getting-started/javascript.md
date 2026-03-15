---
title: Primeros pasos con JavaScript / TypeScript
description: Instala PetraDB y ejecuta tus primeras consultas SQL desde JavaScript o TypeScript.
---

## Instalacion

```bash
npm install @petradb/engine
```

Sin modulos nativos, sin scripts post-instalacion — solo JavaScript.

## Ejecuta tu primera consulta

```javascript
import { Session } from '@petradb/engine';

const db = new Session();

await db.execute(`
  CREATE TABLE users (
    id SERIAL,
    name TEXT NOT NULL,
    email TEXT
  );

  INSERT INTO users (name, email) VALUES
    ('Alice', 'alice@example.com'),
    ('Bob', 'bob@example.com');
`);

const [{ rows }] = await db.execute('SELECT * FROM users');
console.log(rows);
// [
//   { id: 1, name: 'Alice', email: 'alice@example.com' },
//   { id: 2, name: 'Bob', email: 'bob@example.com' }
// ]
```

Eso es todo — una base de datos SQL completa compatible con PostgreSQL, en memoria, sin configuracion. Cada `Session` es una instancia de base de datos aislada.

## Donde se ejecuta

El motor es JavaScript puro sin dependencias nativas, por lo que funciona en cualquier lugar donde se ejecute JavaScript: Node.js, Deno, Bun y directamente en el navegador. Puedes [probarlo ahora mismo en el playground](/playground/).

## Almacenamiento persistente

Cuando necesitas que los datos sobrevivan a los reinicios, pasa una opcion `storage` al constructor:

```javascript
// Almacenamiento durable a prueba de fallos en un solo archivo (Node.js)
const db = new Session({ storage: 'persistent', path: './mydb' });

// Archivo de texto legible por humanos (Node.js)
const db = new Session({ storage: 'text', path: './data.ptxt' });
```

Para bases de datos persistentes, PetraDB detecta automaticamente si debe crear un nuevo archivo o abrir uno existente. Llama a `await db.close()` cuando termines para liberar el descriptor de archivo.

Para acceso multi-proceso o en red, ejecuta PetraDB como un [servidor](/guides/server/) y conecta con la biblioteca [cliente](/guides/client/).

## Siguientes pasos

La [guia de JavaScript / TypeScript](/guides/javascript/) cubre los modos de almacenamiento, modos de fila, sentencias preparadas y la API completa. Consulta tambien las guias de [CLI](/guides/cli/), [Servidor](/guides/server/) y [Cliente](/guides/client/).
