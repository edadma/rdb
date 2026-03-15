---
title: Utilisation JavaScript / TypeScript
description: Utiliser PetraDB depuis JavaScript et TypeScript.
---

## Creer une base de donnees

Chaque instance `Session` est une base de donnees entierement isolee. Par defaut, elle fonctionne en memoire, mais vous pouvez choisir un stockage persistant ou texte.

```javascript
import { Session } from '@petradb/engine';

// En memoire (par defaut)
const db = new Session();
```

## Modes de stockage

### Memoire (par defaut)

Les donnees restent en memoire et sont perdues a la fin du processus. Fonctionne partout : Node.js, Deno, Bun et navigateurs.

```javascript
const db = new Session();
// ou explicitement :
const db = new Session({ storage: 'memory' });
```

### Persistant (Node.js)

Stockage durable resistant aux pannes dans un seul fichier binaire, utilisant des pages en copie-sur-ecriture et des en-tetes a double tampon. Si le fichier existe, il est ouvert ; sinon, une nouvelle base de donnees est creee.

```javascript
const db = new Session({ storage: 'persistent', path: './mydb' });

// Optionnel : definir la taille de page (par defaut 4096)
const db = new Session({ storage: 'persistent', path: './mydb', pageSize: 8192 });
```

### Texte (Node.js)

Stocke les donnees dans un fichier texte lisible par l'homme (`.ptxt`). Utile pour le debogage, le controle de version ou l'edition manuelle des donnees.

```javascript
const db = new Session({ storage: 'text', path: './data.ptxt' });
```

### Fermeture

Appelez `await db.close()` pour liberer les descripteurs de fichiers lors de l'utilisation du stockage persistant ou texte. Pour les bases de donnees en memoire, `close()` est un no-op.

```javascript
const db = new Session({ storage: 'persistent', path: './mydb' });
// ... utiliser la base de donnees ...
await db.close();
```

## Executer du SQL

Utilisez `db.execute(sql)` pour executer une ou plusieurs instructions SQL separees par des points-virgules. Elle retourne une promesse qui se resout en un tableau d'objets de resultats.

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

## Modes de lignes

Par defaut, les lignes SELECT sont retournees sous forme d'objets indexes par nom de colonne. Utilisez `rowMode: 'array'` pour obtenir des tableaux positionnels a la place.

```javascript
// Definir la valeur par defaut pour toutes les requetes
const db = new Session({ rowMode: 'array' });

// Ou surcharger par appel
const [{ rows }] = await db.execute('SELECT id, name FROM users', { rowMode: 'array' });
// rows: [[1, 'Alice'], [2, 'Bob']]
```

## Prepared statements

Utilisez `db.prepare(sql)` avec les placeholders de parametres `$1`, `$2`, ... Retourne un objet statement avec une methode `execute(params, options?)`.

```javascript
const stmt = db.prepare('SELECT * FROM users WHERE id = $1');
const [{ rows }] = await stmt.execute([42]);

// Avec options
const [{ rows }] = await stmt.execute([42], { rowMode: 'array' });
```

Le SQL `PREPARE` / `EXECUTE` / `DEALLOCATE` est egalement supporte -- consultez la [reference Transactions](/reference/transactions/) pour les details.

Consultez la [reference API JavaScript](/reference/api-javascript/) pour les types de resultats, le mapping de valeurs et les interfaces TypeScript completes.

## TypeScript

Les definitions de types completes sont incluses. Utilisez les unions discriminees pour affiner les types de resultats :

```typescript
import { Session, ExecuteResult } from '@petradb/engine';

const db = new Session();
const results: ExecuteResult[] = await db.execute('SELECT * FROM users');

for (const result of results) {
  if (result.command === 'select') {
    // result.rows et result.fields sont types ici
  }
}
```

## Exemple complet

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

## Gestion des erreurs

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

## Notes sur les plateformes

- Le mode en memoire fonctionne partout : Node.js, Deno, Bun et navigateurs (avec bundlers)
- Le stockage persistant et texte necessite Node.js (ils utilisent le systeme de fichiers)
- Aucune dependance externe ou module natif requis
- Definitions TypeScript incluses
- Envisagez les Web Workers pour les grands jeux de donnees dans les navigateurs
