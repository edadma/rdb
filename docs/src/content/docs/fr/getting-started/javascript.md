---
title: Premiers pas avec JavaScript / TypeScript
description: Installez PetraDB et exécutez vos premières requêtes SQL depuis JavaScript ou TypeScript.
---

## Installation

```bash
npm install @petradb/engine
```

Pas de modules natifs, pas de scripts post-installation — juste du JavaScript.

## Exécutez votre première requête

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

C'est tout — une base de données SQL complète compatible PostgreSQL, en mémoire, sans aucune configuration. Chaque `Session` est une instance de base de données isolée.

## Où elle fonctionne

Le moteur est du JavaScript pur sans dépendance native, il fonctionne donc partout où JavaScript s'exécute : Node.js, Deno, Bun et directement dans le navigateur. Vous pouvez [l'essayer maintenant dans le playground](/playground/).

## Stockage persistant

Lorsque vous avez besoin que les données survivent aux redémarrages, passez une option `storage` au constructeur :

```javascript
// Stockage durable résistant aux pannes dans un seul fichier (Node.js)
const db = new Session({ storage: 'persistent', path: './mydb' });

// Fichier texte lisible par l'homme (Node.js)
const db = new Session({ storage: 'text', path: './data.ptxt' });
```

Pour les bases de données persistantes, PetraDB détecte automatiquement s'il faut créer un nouveau fichier ou ouvrir un fichier existant. Appelez `await db.close()` lorsque vous avez terminé pour libérer le descripteur de fichier.

Pour l'accès multi-processus ou en réseau, exécutez PetraDB en tant que [serveur](/guides/server/) et connectez-vous avec la bibliothèque [client](/guides/client/).

## Étapes suivantes

Le [guide JavaScript / TypeScript](/guides/javascript/) couvre les modes de stockage, les modes de lignes, les prepared statements et l'API complète. Consultez également les guides [CLI](/guides/cli/), [Serveur](/guides/server/) et [Client](/guides/client/).
