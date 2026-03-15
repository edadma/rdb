---
title: Premiers pas avec JavaScript / TypeScript
description: Installez PetraDB et executez vos premieres requetes SQL depuis JavaScript ou TypeScript.
---

## Installation

```bash
npm install @petradb/engine
```

Pas de modules natifs, pas de scripts post-installation -- juste du JavaScript.

## Executez votre premiere requete

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

C'est tout -- une base de donnees SQL complete compatible PostgreSQL, en memoire, sans aucune configuration. Chaque `Session` est une instance de base de donnees isolee.

## Ou elle fonctionne

Le moteur est du JavaScript pur sans dependance native, il fonctionne donc partout ou JavaScript s'execute : Node.js, Deno, Bun et directement dans le navigateur. Vous pouvez [l'essayer maintenant dans le playground](/playground/).

## Stockage persistant

Lorsque vous avez besoin que les donnees survivent aux redemarrages, passez une option `storage` au constructeur :

```javascript
// Stockage durable resistant aux pannes dans un seul fichier (Node.js)
const db = new Session({ storage: 'persistent', path: './mydb' });

// Fichier texte lisible par l'homme (Node.js)
const db = new Session({ storage: 'text', path: './data.ptxt' });
```

Pour les bases de donnees persistantes, PetraDB detecte automatiquement s'il faut creer un nouveau fichier ou ouvrir un fichier existant. Appelez `await db.close()` lorsque vous avez termine pour liberer le descripteur de fichier.

Pour l'acces multi-processus ou en reseau, executez PetraDB en tant que [serveur](/guides/server/) et connectez-vous avec la bibliotheque [client](/guides/client/).

## Etapes suivantes

Le [guide JavaScript / TypeScript](/guides/javascript/) couvre les modes de stockage, les modes de lignes, les prepared statements et l'API complete. Consultez egalement les guides [CLI](/guides/cli/), [Serveur](/guides/server/) et [Client](/guides/client/).
