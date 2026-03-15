---
title: JavaScript / TypeScript
description: Referencia da API JavaScript e TypeScript para PetraDB.
---

## Instalacao

```bash
npm install @petradb/engine
```

## Session

### `new Session(options?)`

Cria uma nova instancia de banco de dados.

| Opcao | Tipo | Padrao | Descricao |
|--------|------|---------|-------------|
| `rowMode` | `'object' \| 'array'` | `'object'` | Formato padrao de linha para resultados SELECT |
| `storage` | `'memory' \| 'persistent' \| 'text'` | `'memory'` | Backend de armazenamento |
| `path` | `string` | — | Caminho do arquivo (obrigatorio para persistente e texto) |
| `pageSize` | `number` | `4096` | Tamanho da pagina em bytes (apenas persistente) |

```javascript
// Em memoria (padrao)
const db = new Session();

// Armazenamento persistente seguro contra falhas (Node.js)
const db = new Session({ storage: 'persistent', path: './mydb' });

// Arquivo de texto legivel por humanos (Node.js)
const db = new Session({ storage: 'text', path: './data.ptxt' });
```

Para armazenamento persistente, o PetraDB detecta automaticamente se deve criar um novo arquivo ou abrir um existente.

### `db.close()`

Libera handles de arquivo. Retorna `Promise<void>`. Obrigatorio para armazenamento persistente e de texto. Sem efeito (mas ainda assincrono) para bancos em memoria.

```javascript
await db.close();
```

### `db.execute(sql, options?)`

Executa um ou mais comandos SQL separados por `;`. Retorna uma promise que resolve para um array de resultados.

| Opcao | Tipo | Padrao | Descricao |
|--------|------|---------|-------------|
| `rowMode` | `'object' \| 'array'` | padrao do construtor | Formato de linha para esta chamada |

```javascript
const [{ rows, fields }] = await db.execute('SELECT * FROM users');
```

### `db.prepare(sql)`

Cria um prepared statement com placeholders de parametro `$1`, `$2`, .... Retorna um objeto statement com um metodo `execute(params, options?)`.

```javascript
const stmt = db.prepare('SELECT * FROM users WHERE id = $1');
const [{ rows }] = await stmt.execute([42]);

// Com opcoes
const [{ rows }] = await stmt.execute([42], { rowMode: 'array' });
```

### `db.registerFunction(name, callback)`

Registra uma funcao JavaScript nativa chamavel a partir de SQL, triggers e stored procedures:

```javascript
db.registerFunction('my_double', (args) => args[0] * 2);

// Agora utilizavel em SQL:
const [{ rows }] = await db.execute('SELECT my_double(21) AS val');
// rows[0].val === 42
```

O callback recebe um array de valores JavaScript (numeros, strings, booleanos ou `null`) e deve retornar um valor JavaScript.

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

## Tipos de Resultado

Todo resultado tem um campo `command` para discriminacao:

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

// Plano de consulta
{ command: 'explain', plan: string }

// DML
{ command: 'insert', result: Record<string, any>, rows: T[], fields: FieldInfo[] }
{ command: 'select', rows: T[], fields: { name: string, dataType: string }[] }
{ command: 'update', rowCount: number }
{ command: 'delete', rowCount: number }
{ command: 'copy', rowCount: number }

// Transacoes
{ command: 'begin' }
{ command: 'commit' }
{ command: 'rollback' }

// Prepared statements
{ command: 'prepare', name: string }
{ command: 'deallocate', name: string }
```

## Mapeamento de Valores

| Tipo SQL | Tipo JavaScript |
|----------|----------------|
| INT, BIGINT, DOUBLE, NUMERIC | `number` |
| TEXT, CHAR, VARCHAR | `string` |
| BOOLEAN | `boolean` |
| UUID | `string` |
| TIMESTAMP | `Date` |
| ENUM | `string` (rotulo) |
| JSON array | `Array` |
| JSON object | `Object` |
| NULL | `null` |
