---
title: GraphQL
description: Building a GraphQL API with PetraDB and GraphQL Yoga.
---

PetraDB works with any GraphQL server that runs on Node.js. This guide uses [GraphQL Yoga](https://the-guild.dev/graphql/yoga-server) — a lightweight, spec-compliant server — but the same approach applies to Apollo Server, Mercurius, or any other framework. PetraDB runs in-process, so resolvers call the engine directly with no network round-trips.

## Install

```bash
npm install @petradb/engine graphql-yoga graphql
```

## Setup

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

## Schema and resolvers

Define a GraphQL schema that maps to your PetraDB tables:

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

## Parameterized queries

Always use `db.prepare()` with positional parameters (`$1`, `$2`, …) for user-supplied values. This prevents SQL injection and handles type conversion automatically.

```javascript
const [{ rows }] = await db.prepare(
  'SELECT * FROM users WHERE name = $1 AND email = $2'
).execute([name, email]);
```

## Nested resolvers

For related data, add field resolvers that run additional queries:

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

## Pagination

Offset-based pagination maps directly to SQL `LIMIT` and `OFFSET`:

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

For cursor-based pagination, use the row ID as the cursor:

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
      params.push(first + 1); // fetch one extra to check hasNextPage

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

## Transactions

Wrap multi-step mutations in a transaction using `BEGIN` / `COMMIT` / `ROLLBACK`:

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

## Aggregations

Use SQL aggregates and return computed fields:

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

## Type mapping

PetraDB returns native JavaScript types, so GraphQL scalars work without manual coercion:

| PetraDB type | JS type | GraphQL scalar |
|---|---|---|
| `SERIAL` / `INTEGER` | `number` | `Int` |
| `BIGINT` | `number` | `Int` |
| `DOUBLE` / `REAL` | `number` | `Float` |
| `NUMERIC` | `number` | `Float` |
| `TEXT` / `VARCHAR` | `string` | `String` |
| `BOOLEAN` | `boolean` | `Boolean` |
| `DATE` / `TIMESTAMP` | `Date` | `String` (or custom scalar) |
| `JSON` | `object` | custom scalar or `String` |
| `NULL` | `null` | nullable field |
