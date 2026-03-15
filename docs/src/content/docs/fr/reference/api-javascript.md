---
title: JavaScript / TypeScript
description: Reference de l'API JavaScript et TypeScript pour PetraDB.
---

## Installation

```bash
npm install @petradb/engine
```

## Session

### `new Session(options?)`

Cree une nouvelle instance de base de donnees.

| Option | Type | Par defaut | Description |
|--------|------|-----------|-------------|
| `rowMode` | `'object' \| 'array'` | `'object'` | Format de ligne par defaut pour les resultats SELECT |
| `storage` | `'memory' \| 'persistent' \| 'text'` | `'memory'` | Backend de stockage |
| `path` | `string` | -- | Chemin du fichier (requis pour persistent et text) |
| `pageSize` | `number` | `4096` | Taille de page en octets (persistent uniquement) |

```javascript
// En memoire (par defaut)
const db = new Session();

// Stockage persistant resistant aux pannes (Node.js)
const db = new Session({ storage: 'persistent', path: './mydb' });

// Fichier texte lisible par l'homme (Node.js)
const db = new Session({ storage: 'text', path: './data.ptxt' });
```

Pour le stockage persistant, PetraDB detecte automatiquement s'il faut creer un nouveau fichier ou ouvrir un fichier existant.

### `db.close()`

Libere les descripteurs de fichiers. Retourne une `Promise<void>`. Requis pour le stockage persistant et texte. No-op (mais toujours asynchrone) pour les bases de donnees en memoire.

```javascript
await db.close();
```

### `db.execute(sql, options?)`

Execute une ou plusieurs instructions SQL separees par `;`. Retourne une promesse qui se resout en un tableau de resultats.

| Option | Type | Par defaut | Description |
|--------|------|-----------|-------------|
| `rowMode` | `'object' \| 'array'` | valeur du constructeur | Format de ligne pour cet appel |

```javascript
const [{ rows, fields }] = await db.execute('SELECT * FROM users');
```

### `db.prepare(sql)`

Cree un prepared statement avec les placeholders de parametres `$1`, `$2`, ... Retourne un objet statement avec une methode `execute(params, options?)`.

```javascript
const stmt = db.prepare('SELECT * FROM users WHERE id = $1');
const [{ rows }] = await stmt.execute([42]);

// Avec options
const [{ rows }] = await stmt.execute([42], { rowMode: 'array' });
```

### `db.registerFunction(name, callback)`

Enregistre une fonction JavaScript native appelable depuis SQL, les declencheurs et les procedures stockees :

```javascript
db.registerFunction('my_double', (args) => args[0] * 2);

// Maintenant utilisable en SQL :
const [{ rows }] = await db.execute('SELECT my_double(21) AS val');
// rows[0].val === 42
```

Le callback recoit un tableau de valeurs JavaScript (nombres, chaines, booleens ou `null`) et doit retourner une valeur JavaScript.

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

## Types de resultats

Chaque resultat possede un champ `command` pour la discrimination :

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

// Plan de requete
{ command: 'explain', plan: string }

// DML
{ command: 'insert', result: Record<string, any>, rows: T[], fields: FieldInfo[] }
{ command: 'select', rows: T[], fields: { name: string, dataType: string }[] }
{ command: 'update', rowCount: number }
{ command: 'delete', rowCount: number }
{ command: 'copy', rowCount: number }

// Transactions
{ command: 'begin' }
{ command: 'commit' }
{ command: 'rollback' }

// Prepared statements
{ command: 'prepare', name: string }
{ command: 'deallocate', name: string }
```

## Mapping de valeurs

| Type SQL | Type JavaScript |
|----------|----------------|
| INT, BIGINT, DOUBLE, NUMERIC | `number` |
| TEXT, CHAR, VARCHAR | `string` |
| BOOLEAN | `boolean` |
| UUID | `string` |
| TIMESTAMP | `Date` |
| ENUM | `string` (label) |
| Tableau JSON | `Array` |
| Objet JSON | `Object` |
| NULL | `null` |
