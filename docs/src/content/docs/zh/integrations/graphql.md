---
title: GraphQL
description: 使用 PetraDB 和 GraphQL Yoga 构建 GraphQL API。
---

PetraDB 可以与任何在 Node.js 上运行的 GraphQL 服务器配合使用。本指南使用 [GraphQL Yoga](https://the-guild.dev/graphql/yoga-server) — 一个轻量级、符合规范的服务器 — 但同样的方法适用于 Apollo Server、Mercurius 或任何其他框架。PetraDB 在进程内运行，因此解析器直接调用引擎，无需网络往返。

## 安装

```bash
npm install @petradb/engine graphql-yoga graphql
```

## 设置

```javascript
import { Session } from '@petradb/engine';
import { createYoga, createSchema } from 'graphql-yoga';
import { createServer } from 'http';

const db = new Session();

await db.execute(`
  CREATE TABLE users (
    id SERIAL PRIMARY KEY,
    name TEXT NOT NULL,
    email TEXT
  )
`);
```

## Schema 和解析器

定义映射到 PetraDB 表的 GraphQL schema：

```javascript
const yoga = createYoga({
  schema: createSchema({
    typeDefs: `
      type User {
        id: Int!
        name: String!
        email: String
      }

      type Query {
        users: [User!]!
        user(id: Int!): User
      }

      type Mutation {
        createUser(name: String!, email: String): User!
        updateUser(id: Int!, name: String, email: String): User
        deleteUser(id: Int!): Boolean!
      }
    `,
    resolvers: {
      Query: {
        users: async () => {
          const [{ rows }] = await db.execute('SELECT * FROM users');
          return rows;
        },
        user: async (_, { id }) => {
          const [{ rows }] = await db.prepare('SELECT * FROM users WHERE id = $1')
            .execute([id]);
          return rows[0] || null;
        },
      },
      Mutation: {
        createUser: async (_, { name, email }) => {
          const [{ rows }] = await db.prepare(
            'INSERT INTO users (name, email) VALUES ($1, $2) RETURNING *'
          ).execute([name, email ?? null]);
          return rows[0];
        },
        updateUser: async (_, { id, name, email }) => {
          const sets = [];
          const params = [];
          let i = 1;
          if (name !== undefined) { sets.push(`name = $${i++}`); params.push(name); }
          if (email !== undefined) { sets.push(`email = $${i++}`); params.push(email ?? null); }
          if (sets.length === 0) return null;
          params.push(id);
          const [{ rows }] = await db.prepare(
            `UPDATE users SET ${sets.join(', ')} WHERE id = $${i} RETURNING *`
          ).execute(params);
          return rows[0] || null;
        },
        deleteUser: async (_, { id }) => {
          const [result] = await db.prepare('DELETE FROM users WHERE id = $1').execute([id]);
          return result.rowCount > 0;
        },
      },
    },
  }),
});

const server = createServer(yoga);
server.listen(4000, () => console.log('GraphQL API running on http://localhost:4000/graphql'));
```

## 参数化查询

始终使用 `db.prepare()` 配合位置参数（`$1`、`$2`...）处理用户提供的值。这可以防止 SQL 注入并自动处理类型转换。

```javascript
const [{ rows }] = await db.prepare(
  'SELECT * FROM users WHERE name = $1 AND email = $2'
).execute([name, email]);
```

## 嵌套解析器

对于关联数据，添加运行额外查询的字段解析器：

```javascript
await db.execute(`
  CREATE TABLE posts (
    id SERIAL PRIMARY KEY,
    author_id INTEGER NOT NULL REFERENCES users(id),
    title TEXT NOT NULL,
    body TEXT
  )
`);

const schema = createSchema({
  typeDefs: `
    type User {
      id: Int!
      name: String!
      posts: [Post!]!
    }

    type Post {
      id: Int!
      title: String!
      body: String
      author: User!
    }

    type Query {
      users: [User!]!
      posts: [Post!]!
    }
  `,
  resolvers: {
    Query: {
      users: async () => {
        const [{ rows }] = await db.execute('SELECT * FROM users');
        return rows;
      },
      posts: async () => {
        const [{ rows }] = await db.execute('SELECT * FROM posts');
        return rows;
      },
    },
    User: {
      posts: async (user) => {
        const [{ rows }] = await db.prepare(
          'SELECT * FROM posts WHERE author_id = $1'
        ).execute([user.id]);
        return rows;
      },
    },
    Post: {
      author: async (post) => {
        const [{ rows }] = await db.prepare(
          'SELECT * FROM users WHERE id = $1'
        ).execute([post.author_id]);
        return rows[0];
      },
    },
  },
});
```

## 分页

基于偏移量的分页直接映射到 SQL 的 `LIMIT` 和 `OFFSET`：

```javascript
const resolvers = {
  Query: {
    users: async (_, { limit = 10, offset = 0 }) => {
      const [{ rows }] = await db.prepare(
        'SELECT * FROM users ORDER BY id LIMIT $1 OFFSET $2'
      ).execute([limit, offset]);
      return rows;
    },
  },
};
```

对于基于游标的分页，使用行 ID 作为游标：

```javascript
const resolvers = {
  Query: {
    users: async (_, { first = 10, after }) => {
      let sql = 'SELECT * FROM users';
      const params = [];
      if (after) {
        sql += ' WHERE id > $1';
        params.push(after);
      }
      sql += ` ORDER BY id LIMIT $${params.length + 1}`;
      params.push(first + 1); // 多取一条以检查是否有下一页

      const [{ rows }] = await db.prepare(sql).execute(params);
      const hasNextPage = rows.length > first;
      const edges = rows.slice(0, first);

      return {
        edges: edges.map((node) => ({ node, cursor: node.id })),
        pageInfo: {
          hasNextPage,
          endCursor: edges.length ? edges[edges.length - 1].id : null,
        },
      };
    },
  },
};
```

## 事务

使用 `BEGIN` / `COMMIT` / `ROLLBACK` 将多步变更包装在事务中：

```javascript
const resolvers = {
  Mutation: {
    transferCredits: async (_, { fromId, toId, amount }) => {
      await db.execute('BEGIN');
      try {
        await db.prepare(
          'UPDATE accounts SET balance = balance - $1 WHERE id = $2'
        ).execute([amount, fromId]);
        await db.prepare(
          'UPDATE accounts SET balance = balance + $1 WHERE id = $2'
        ).execute([amount, toId]);
        await db.execute('COMMIT');
        return true;
      } catch (e) {
        await db.execute('ROLLBACK');
        throw e;
      }
    },
  },
};
```

## 聚合

使用 SQL 聚合并返回计算字段：

```javascript
const resolvers = {
  Query: {
    userStats: async () => {
      const [{ rows }] = await db.execute(`
        SELECT
          COUNT(*) AS total,
          COUNT(email) AS with_email,
          MIN(id) AS first_id,
          MAX(id) AS last_id
        FROM users
      `);
      return rows[0];
    },
  },
};
```

## 类型映射

PetraDB 返回原生 JavaScript 类型，因此 GraphQL 标量无需手动转换即可使用：

| PetraDB 类型 | JS 类型 | GraphQL 标量 |
|---|---|---|
| `SERIAL` / `INTEGER` | `number` | `Int` |
| `BIGINT` | `number` | `Int` |
| `DOUBLE` / `REAL` | `number` | `Float` |
| `NUMERIC` | `number` | `Float` |
| `TEXT` / `VARCHAR` | `string` | `String` |
| `BOOLEAN` | `boolean` | `Boolean` |
| `DATE` / `TIMESTAMP` | `Date` | `String`（或自定义标量） |
| `JSON` | `object` | 自定义标量或 `String` |
| `NULL` | `null` | 可空字段 |
