---
title: Utilisation JavaScript / TypeScript
description: Utiliser PetraDB depuis JavaScript et TypeScript.
---

## Créer une base de données

Chaque instance `Session` est une base de données entièrement isolée. Par défaut, elle fonctionne en mémoire, mais vous pouvez choisir un stockage persistant ou texte.

```javascript
import { Session } from '@petradb/engine';

// En mémoire (par défaut)
const db = new Session();
```

## Modes de stockage

### Mémoire (par défaut)

Les données restent en mémoire et sont perdues à la fin du processus. Fonctionne partout : Node.js, Deno, Bun et navigateurs.

```javascript
const db = new Session();
// ou explicitement :
const db = new Session({ storage: 'memory' });
```

### Persistant (Node.js)

Stockage durable résistant aux pannes dans un seul fichier binaire, utilisant des pages en copie-sur-écriture et des en-têtes à double tampon. Si le fichier existe, il est ouvert ; sinon, une nouvelle base de données est créée.

```javascript
const db = new Session({ storage: 'persistent', path: './mydb' });

// Optionnel : définir la taille de page (par défaut 4096)
const db = new Session({ storage: 'persistent', path: './mydb', pageSize: 8192 });
```

### Texte (Node.js)

Stocke les données dans un fichier texte lisible par l'homme (`.ptxt`). Utile pour le débogage, le contrôle de version ou l'édition manuelle des données.

```javascript
const db = new Session({ storage: 'text', path: './data.ptxt' });
```

### Fermeture

Appelez `await db.close()` pour libérer les descripteurs de fichiers lors de l'utilisation du stockage persistant ou texte. Pour les bases de données en mémoire, `close()` est un no-op.

```javascript
const db = new Session({ storage: 'persistent', path: './mydb' });
// ... utiliser la base de données ...
await db.close();
```

## Exécuter du SQL

Utilisez `db.execute(sql)` pour exécuter une ou plusieurs instructions SQL séparées par des points-virgules. Elle retourne une promesse qui se résout en un tableau d'objets de résultats.

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

Par défaut, les lignes SELECT sont retournées sous forme d'objets indexés par nom de colonne. Utilisez `rowMode: 'array'` pour obtenir des tableaux positionnels à la place.

```javascript
// Définir la valeur par défaut pour toutes les requêtes
const db = new Session({ rowMode: 'array' });

// Ou surcharger par appel
const [{ rows }] = await db.execute('SELECT id, name FROM users', { rowMode: 'array' });
// rows: [[1, 'Alice'], [2, 'Bob']]
```

## Prepared statements

Utilisez `db.prepare(sql)` avec les placeholders de paramètres `$1`, `$2`, ... Retourne un objet statement avec une méthode `execute(params, options?)`.

```javascript
const stmt = db.prepare('SELECT * FROM users WHERE id = $1');
const [{ rows }] = await stmt.execute([42]);

// Avec options
const [{ rows }] = await stmt.execute([42], { rowMode: 'array' });
```

Le SQL `PREPARE` / `EXECUTE` / `DEALLOCATE` est également supporté — consultez la [référence Transactions](/reference/transactions/) pour les détails.

Consultez la [référence API JavaScript](/reference/api-javascript/) pour les types de résultats, le mapping de valeurs et les interfaces TypeScript complètes.

## TypeScript

Les définitions de types complètes sont incluses. Utilisez les unions discriminées pour affiner les types de résultats :

```typescript
import { Session, ExecuteResult } from '@petradb/engine';

const db = new Session();
const results: ExecuteResult[] = await db.execute('SELECT * FROM users');

for (const result of results) {
  if (result.command === 'select') {
    // result.rows et result.fields sont typés ici
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

- Le mode en mémoire fonctionne partout : Node.js, Deno, Bun et navigateurs (avec bundlers)
- Le stockage persistant et texte nécessite Node.js (ils utilisent le système de fichiers)
- Aucune dépendance externe ou module natif requis
- Définitions TypeScript incluses
- Envisagez les Web Workers pour les grands jeux de données dans les navigateurs
