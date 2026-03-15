---
title: GraphQL
description: PetraDB와 GraphQL Yoga로 GraphQL API 구축하기.
---

PetraDB는 Node.js에서 실행되는 모든 GraphQL 서버와 함께 작동합니다. 이 가이드는 경량의 사양 준수 서버인 [GraphQL Yoga](https://the-guild.dev/graphql/yoga-server)를 사용하지만, Apollo Server, Mercurius 또는 다른 프레임워크에도 동일한 접근 방식이 적용됩니다. PetraDB는 프로세스 내에서 실행되므로, 리졸버가 네트워크 왕복 없이 엔진을 직접 호출합니다.

## 설치

```bash
npm install @petradb/engine graphql-yoga graphql
```

## 설정

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

## 스키마와 리졸버

PetraDB 테이블에 매핑되는 GraphQL 스키마를 정의합니다:

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

## 매개변수화된 쿼리

사용자 제공 값에는 항상 `db.prepare()`와 위치 매개변수(`$1`, `$2`, ...)를 사용합니다. 이렇게 하면 SQL 인젝션을 방지하고 타입 변환을 자동으로 처리합니다.

```javascript
const [{ rows }] = await db.prepare(
  'SELECT * FROM users WHERE name = $1 AND email = $2'
).execute([name, email]);
```

## 중첩 리졸버

관련 데이터의 경우, 추가 쿼리를 실행하는 필드 리졸버를 추가합니다:

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

## 페이지네이션

오프셋 기반 페이지네이션은 SQL의 `LIMIT`과 `OFFSET`에 직접 매핑됩니다:

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

커서 기반 페이지네이션의 경우, 행 ID를 커서로 사용합니다:

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
      params.push(first + 1); // hasNextPage 확인을 위해 하나 더 가져옴

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

## 트랜잭션

다중 단계 뮤테이션을 `BEGIN` / `COMMIT` / `ROLLBACK`으로 트랜잭션에 래핑합니다:

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

## 집계

SQL 집계를 사용하여 계산된 필드를 반환합니다:

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

## 타입 매핑

PetraDB는 네이티브 JavaScript 타입을 반환하므로, GraphQL 스칼라가 수동 강제 변환 없이 작동합니다:

| PetraDB 타입 | JS 타입 | GraphQL 스칼라 |
|---|---|---|
| `SERIAL` / `INTEGER` | `number` | `Int` |
| `BIGINT` | `number` | `Int` |
| `DOUBLE` / `REAL` | `number` | `Float` |
| `NUMERIC` | `number` | `Float` |
| `TEXT` / `VARCHAR` | `string` | `String` |
| `BOOLEAN` | `boolean` | `Boolean` |
| `DATE` / `TIMESTAMP` | `Date` | `String` (또는 커스텀 스칼라) |
| `JSON` | `object` | 커스텀 스칼라 또는 `String` |
| `NULL` | `null` | NULL 허용 필드 |
