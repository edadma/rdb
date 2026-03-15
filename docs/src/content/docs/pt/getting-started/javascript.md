---
title: Primeiros Passos com JavaScript / TypeScript
description: Instale o PetraDB e execute suas primeiras consultas SQL a partir de JavaScript ou TypeScript.
---

## Instalacao

```bash
npm install @petradb/engine
```

Sem modulos nativos, sem scripts pos-instalacao — apenas JavaScript.

## Execute sua primeira consulta

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

Pronto — um banco de dados SQL completo compativel com PostgreSQL, em memoria, sem configuracao. Cada `Session` e uma instancia de banco de dados isolada.

## Onde funciona

O engine e JavaScript puro sem dependencias nativas, entao funciona em qualquer lugar que JavaScript rode: Node.js, Deno, Bun e diretamente no navegador. Voce pode [experimenta-lo agora no playground](/playground/).

## Armazenamento persistente

Quando voce precisa que os dados sobrevivam a reinicializacoes, passe uma opcao `storage` ao construtor:

```javascript
// Armazenamento duravel seguro contra falhas em um unico arquivo (Node.js)
const db = new Session({ storage: 'persistent', path: './mydb' });

// Arquivo de texto legivel por humanos (Node.js)
const db = new Session({ storage: 'text', path: './data.ptxt' });
```

Para bancos de dados persistentes, o PetraDB detecta automaticamente se deve criar um novo arquivo ou abrir um existente. Chame `await db.close()` quando terminar para liberar o handle do arquivo.

Para acesso multi-processo ou em rede, execute o PetraDB como um [servidor](/guides/server/) e conecte com a biblioteca [client](/guides/client/).

## Proximos passos

O [guia JavaScript / TypeScript](/guides/javascript/) cobre modos de armazenamento, modos de linha, prepared statements e a API completa. Veja tambem os guias [CLI](/guides/cli/), [Servidor](/guides/server/) e [Cliente](/guides/client/).
