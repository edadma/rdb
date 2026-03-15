---
title: Uso com JavaScript / TypeScript
description: Usando o PetraDB a partir de JavaScript e TypeScript.
---

## Criando um Banco de Dados

Cada instancia `Session` e um banco de dados totalmente isolado. Por padrao funciona em memoria, mas voce pode escolher armazenamento persistente ou de texto.

```javascript
import { Session } from '@petradb/engine';

// Em memoria (padrao)
const db = new Session();
```

## Modos de Armazenamento

### Memoria (padrao)

Os dados ficam na memoria e sao perdidos quando o processo encerra. Funciona em todos os lugares: Node.js, Deno, Bun e navegadores.

```javascript
const db = new Session();
// ou explicitamente:
const db = new Session({ storage: 'memory' });
```

### Persistente (Node.js)

Armazenamento duravel seguro contra falhas em um unico arquivo binario, usando paginas copy-on-write e cabecalhos com buffer duplo. Se o arquivo existe, ele e aberto; caso contrario, um novo banco de dados e criado.

```javascript
const db = new Session({ storage: 'persistent', path: './mydb' });

// Opcional: definir tamanho da pagina (padrao 4096)
const db = new Session({ storage: 'persistent', path: './mydb', pageSize: 8192 });
```

### Texto (Node.js)

Armazena dados em um arquivo de texto legivel por humanos (`.ptxt`). Util para depuracao, controle de versao ou edicao manual de dados.

```javascript
const db = new Session({ storage: 'text', path: './data.ptxt' });
```

### Fechamento

Chame `await db.close()` para liberar handles de arquivo ao usar armazenamento persistente ou de texto. Para bancos em memoria, `close()` e uma operacao sem efeito.

```javascript
const db = new Session({ storage: 'persistent', path: './mydb' });
// ... use o banco de dados ...
await db.close();
```

## Executando SQL

Use `db.execute(sql)` para executar um ou mais comandos SQL separados por ponto e virgula. Retorna uma promise que resolve para um array de objetos de resultado.

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

## Modos de Linha

Por padrao, linhas de SELECT sao retornadas como objetos indexados por nome de coluna. Use `rowMode: 'array'` para arrays posicionais.

```javascript
// Definir padrao para todas as consultas
const db = new Session({ rowMode: 'array' });

// Ou sobrescrever por chamada
const [{ rows }] = await db.execute('SELECT id, name FROM users', { rowMode: 'array' });
// rows: [[1, 'Alice'], [2, 'Bob']]
```

## Prepared Statements

Use `db.prepare(sql)` com placeholders de parametro `$1`, `$2`, .... Retorna um objeto statement com um metodo `execute(params, options?)`.

```javascript
const stmt = db.prepare('SELECT * FROM users WHERE id = $1');
const [{ rows }] = await stmt.execute([42]);

// Com opcoes
const [{ rows }] = await stmt.execute([42], { rowMode: 'array' });
```

`PREPARE` / `EXECUTE` / `DEALLOCATE` em nivel SQL tambem e suportado — veja a [referencia de Transacoes](/reference/transactions/) para detalhes.

Veja a [referencia da API JavaScript](/reference/api-javascript/) para tipos de resultado, mapeamento de valores e interfaces TypeScript completas.

## TypeScript

Definicoes de tipo completas estao incluidas. Use unions discriminadas para restringir tipos de resultado:

```typescript
import { Session, ExecuteResult } from '@petradb/engine';

const db = new Session();
const results: ExecuteResult[] = await db.execute('SELECT * FROM users');

for (const result of results) {
  if (result.command === 'select') {
    // result.rows e result.fields sao tipados aqui
  }
}
```

## Exemplo Completo

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

## Tratamento de Erros

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

## Notas sobre Plataformas

- O modo em memoria funciona em todos os lugares: Node.js, Deno, Bun e navegadores (com bundlers)
- Armazenamento persistente e de texto requerem Node.js (usam o sistema de arquivos)
- Sem dependencias externas ou modulos nativos necessarios
- Definicoes TypeScript incluidas
- Considere Web Workers para grandes conjuntos de dados em navegadores
