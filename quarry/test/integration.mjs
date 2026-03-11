import { describe, it, before, after } from 'node:test'
import assert from 'node:assert/strict'
import { Session } from '@petradb/engine'
import {
  quarry,
  table,
  serial,
  text,
  integer,
  boolean,
  col,
  eq,
  ne,
  gt,
  gte,
  lt,
  lte,
  like,
  and,
  or,
  not,
  isNull,
  isNotNull,
  inList,
  notInList,
  between,
  count,
  sum,
  avg,
  min,
  max,
  fn,
  alias,
  literal,
  asc,
  desc,
  add,
  sub,
} from '../dist/index.js'

// ── Schema definitions ──

const users = table('users', {
  id: serial('id').primaryKey(),
  name: text('name').notNull(),
  email: text('email').notNull().unique(),
  age: integer('age'),
  active: boolean('active').notNull().default(true),
})

const posts = table('posts', {
  id: serial('id').primaryKey(),
  userId: integer('user_id').notNull(),
  title: text('title').notNull(),
  body: text('body'),
})

// ── Tests ──

describe('quarry', () => {
  let session
  let db

  before(async () => {
    session = new Session()
    db = quarry(session)
    await db.createTable(users)
    await db.createTable(posts)
  })

  after(async () => {
    await session.close()
  })

  // ── DDL ──

  describe('createTable', () => {
    it('creates a table via AST', async () => {
      const [res] = await session.execute('SHOW TABLES')
      const names = res.rows.map((r) => r.table_name).sort()
      assert.deepStrictEqual(names, ['posts', 'users'])
    })

    it('creates columns with correct types', async () => {
      const [res] = await session.execute('SHOW COLUMNS users')
      const cols = res.rows
      const id = cols.find((c) => c.name === 'id')
      assert.equal(id.type, 'serial')
      assert.equal(id.required, true)

      const age = cols.find((c) => c.name === 'age')
      assert.equal(age.type, 'integer')
      assert.equal(age.required, false)

      const active = cols.find((c) => c.name === 'active')
      assert.equal(active.type, 'boolean')
      assert.equal(active.required, true)
    })

    it('toCreateAST produces correct structure', () => {
      const ast = users.toCreateAST()
      assert.equal(ast.kind, 'createTable')
      assert.equal(ast.table, 'users')
      assert.equal(ast.columns.length, 5)
      assert.equal(ast.columns[0].name, 'id')
      assert.equal(ast.columns[0].type, 'serial')
      assert.equal(ast.columns[0].primaryKey, true)
      assert.equal(ast.columns[1].name, 'name')
      assert.equal(ast.columns[1].notNull, true)
      assert.equal(ast.columns[2].unique, true) // email
    })
  })

  // ── INSERT ──

  describe('insert', () => {
    it('inserts a single row', async () => {
      const result = await db.insert(users).values({ name: 'Alice', email: 'alice@test.com', age: 30 }).execute()
      assert.equal(result.name, 'Alice')
      assert.equal(result.email, 'alice@test.com')
      assert.equal(result.age, 30)
      assert.equal(result.active, true) // default
    })

    it('inserts with explicit boolean false', async () => {
      const result = await db
        .insert(users)
        .values({ name: 'Bob', email: 'bob@test.com', active: false })
        .execute()
      assert.equal(result.active, false)
    })

    it('inserts with null optional field', async () => {
      const result = await db
        .insert(users)
        .values({ name: 'Charlie', email: 'charlie@test.com', age: null })
        .execute()
      assert.equal(result.age, null)
    })

    it('inserts multiple rows', async () => {
      await db
        .insert(users)
        .values(
          { name: 'Dave', email: 'dave@test.com', age: 25 },
          { name: 'Eve', email: 'eve@test.com', age: 35 },
        )
        .execute()
      const rows = await db
        .select(users)
        .where(or(eq(col(users, 'name'), 'Dave'), eq(col(users, 'name'), 'Eve')))
        .execute()
      assert.equal(rows.length, 2)
    })

    it('toAST produces correct insert structure', () => {
      const ast = db.insert(users).values({ name: 'Test', email: 'test@test.com' }).toAST()
      assert.equal(ast.kind, 'insert')
      assert.equal(ast.table, 'users')
      assert.ok(ast.columns.includes('name'))
      assert.ok(ast.columns.includes('email'))
      assert.equal(ast.rows.length, 1)
    })

    it('throws on unknown column', () => {
      assert.throws(
        () => db.insert(users).values({ name: 'X', email: 'x@test.com', nonexistent: 42 }).toAST(),
        /Unknown column 'nonexistent'/,
      )
    })

    it('throws on empty values', () => {
      assert.throws(() => db.insert(users).toAST(), /insert requires at least one row/)
    })
  })

  // ── SELECT ──

  describe('select', () => {
    it('selects all rows', async () => {
      const rows = await db.select(users).execute()
      assert.ok(rows.length >= 5)
      assert.ok('id' in rows[0])
      assert.ok('name' in rows[0])
      assert.ok('email' in rows[0])
    })

    it('selects with where clause', async () => {
      const rows = await db.select(users).where(eq(col(users, 'name'), 'Alice')).execute()
      assert.equal(rows.length, 1)
      assert.equal(rows[0].name, 'Alice')
    })

    it('selects specific columns', async () => {
      const rows = await db.select(users).columns(col(users, 'name'), col(users, 'email')).execute()
      assert.ok(rows.length > 0)
      assert.ok('name' in rows[0])
      assert.ok('email' in rows[0])
    })

    it('selects with orderBy asc', async () => {
      const rows = await db.select(users).orderBy(asc(col(users, 'name'))).execute()
      for (let i = 1; i < rows.length; i++) {
        assert.ok(rows[i].name >= rows[i - 1].name)
      }
    })

    it('selects with orderBy desc', async () => {
      const rows = await db.select(users).orderBy(desc(col(users, 'name'))).execute()
      for (let i = 1; i < rows.length; i++) {
        assert.ok(rows[i].name <= rows[i - 1].name)
      }
    })

    it('selects with limit', async () => {
      const rows = await db.select(users).limit(2).execute()
      assert.equal(rows.length, 2)
    })

    it('selects with offset and limit', async () => {
      const all = await db.select(users).orderBy(asc(col(users, 'id'))).execute()
      const offset = await db.select(users).orderBy(asc(col(users, 'id'))).offset(2).limit(2).execute()
      assert.equal(offset.length, 2)
      assert.equal(offset[0].id, all[2].id)
    })

    it('selects with distinct', async () => {
      const rows = await db.select(users).columns(col(users, 'active')).distinct().execute()
      const values = rows.map((r) => r.active)
      assert.equal(new Set(values).size, values.length)
    })

    it('selects with greater than', async () => {
      const rows = await db.select(users).where(gt(col(users, 'age'), 30)).execute()
      for (const row of rows) {
        assert.ok(row.age > 30)
      }
    })

    it('selects with AND condition', async () => {
      const rows = await db
        .select(users)
        .where(and(eq(col(users, 'active'), true), gt(col(users, 'age'), 25)))
        .execute()
      for (const row of rows) {
        assert.equal(row.active, true)
        assert.ok(row.age > 25)
      }
    })

    it('selects with OR condition', async () => {
      const rows = await db
        .select(users)
        .where(or(eq(col(users, 'name'), 'Alice'), eq(col(users, 'name'), 'Bob')))
        .execute()
      assert.equal(rows.length, 2)
    })

    it('selects with NOT', async () => {
      const rows = await db.select(users).where(not(eq(col(users, 'name'), 'Alice'))).execute()
      for (const row of rows) {
        assert.notEqual(row.name, 'Alice')
      }
    })

    it('selects with IS NULL', async () => {
      const rows = await db.select(users).where(isNull(col(users, 'age'))).execute()
      for (const row of rows) {
        assert.equal(row.age, null)
      }
    })

    it('selects with IS NOT NULL', async () => {
      const rows = await db.select(users).where(isNotNull(col(users, 'age'))).execute()
      for (const row of rows) {
        assert.notEqual(row.age, null)
      }
    })

    it('selects with IN list', async () => {
      const rows = await db.select(users).where(inList(col(users, 'name'), ['Alice', 'Bob'])).execute()
      assert.equal(rows.length, 2)
    })

    it('selects with NOT IN list', async () => {
      const rows = await db.select(users).where(notInList(col(users, 'name'), ['Alice', 'Bob'])).execute()
      for (const row of rows) {
        assert.ok(row.name !== 'Alice' && row.name !== 'Bob')
      }
    })

    it('selects with BETWEEN', async () => {
      const rows = await db.select(users).where(between(col(users, 'age'), 25, 35)).execute()
      for (const row of rows) {
        assert.ok(row.age >= 25 && row.age <= 35)
      }
    })

    it('selects with LIKE', async () => {
      const rows = await db.select(users).where(like(col(users, 'name'), 'A%')).execute()
      for (const row of rows) {
        assert.ok(row.name.startsWith('A'))
      }
    })

    it('selects with comparison operators', async () => {
      const r1 = await db.select(users).where(gte(col(users, 'age'), 30)).execute()
      for (const row of r1) assert.ok(row.age >= 30)

      const r2 = await db.select(users).where(lt(col(users, 'age'), 30)).execute()
      for (const row of r2) assert.ok(row.age < 30)

      const r3 = await db.select(users).where(lte(col(users, 'age'), 30)).execute()
      for (const row of r3) assert.ok(row.age <= 30)

      const r4 = await db.select(users).where(ne(col(users, 'name'), 'Alice')).execute()
      for (const row of r4) assert.notEqual(row.name, 'Alice')
    })

    it('selects with aggregate count', async () => {
      const rows = await db.select(users).columns(alias(count(), 'total')).execute()
      assert.ok(rows[0].total >= 5)
    })

    it('selects with groupBy and aggregate', async () => {
      const rows = await db
        .select(users)
        .columns(col(users, 'active'), alias(count(), 'cnt'))
        .groupBy(col(users, 'active'))
        .execute()
      assert.ok(rows.length > 0)
      for (const row of rows) {
        assert.ok('active' in row)
        assert.ok('cnt' in row)
      }
    })

    it('selects with arithmetic expressions', async () => {
      const rows = await db
        .select(users)
        .columns(col(users, 'name'), alias(add(col(users, 'age'), 10), 'age_plus_10'))
        .where(eq(col(users, 'name'), 'Alice'))
        .execute()
      assert.equal(rows[0].age_plus_10, 40) // Alice is 30
    })

    it('selects with function call', async () => {
      const rows = await db
        .select(users)
        .columns(alias(fn('upper', col(users, 'name')), 'upper_name'))
        .where(eq(col(users, 'name'), 'Alice'))
        .execute()
      assert.equal(rows[0].upper_name, 'ALICE')
    })

    it('toAST produces correct select structure', () => {
      const ast = db
        .select(users)
        .where(eq(col(users, 'name'), 'Alice'))
        .orderBy(asc(col(users, 'id')))
        .limit(10)
        .toAST()

      assert.equal(ast.kind, 'query')
      assert.equal(ast.query.kind, 'select')
      assert.ok(ast.query.where)
      assert.equal(ast.query.where.kind, 'binary')
      assert.equal(ast.query.where.op, '=')
      assert.equal(ast.query.limit, 10)
      assert.equal(ast.query.orderBy.length, 1)
    })
  })

  // ── JOIN ──

  describe('joins', () => {
    before(async () => {
      await db.insert(posts).values({ userId: 1, title: 'Hello World', body: 'First post' }).execute()
      await db.insert(posts).values({ userId: 1, title: 'Second Post', body: 'Another' }).execute()
      await db.insert(posts).values({ userId: 2, title: 'Bob Post', body: null }).execute()
    })

    it('inner join', async () => {
      const rows = await db
        .select(posts)
        .columns(col(users, 'name'), col(posts, 'title'))
        .innerJoin(users, eq(col(posts, 'userId'), col(users, 'id')))
        .where(eq(col(users, 'name'), 'Alice'))
        .execute()
      assert.equal(rows.length, 2)
      for (const row of rows) {
        assert.equal(row.name, 'Alice')
      }
    })

    it('left join', async () => {
      const rows = await db
        .select(users)
        .columns(col(users, 'name'), col(posts, 'title'))
        .leftJoin(posts, eq(col(users, 'id'), col(posts, 'userId')))
        .where(eq(col(users, 'name'), 'Charlie'))
        .execute()
      assert.equal(rows.length, 1)
      assert.equal(rows[0].name, 'Charlie')
      assert.equal(rows[0].title, null) // no posts for Charlie
    })
  })

  // ── UPDATE ──

  describe('update', () => {
    it('updates rows with where clause', async () => {
      const result = await db
        .update(users)
        .set({ age: 31 })
        .where(eq(col(users, 'name'), 'Alice'))
        .execute()
      assert.equal(result.rowCount, 1)

      const rows = await db.select(users).where(eq(col(users, 'name'), 'Alice')).execute()
      assert.equal(rows[0].age, 31)
    })

    it('updates multiple fields', async () => {
      await db
        .update(users)
        .set({ name: 'Alice Smith', age: 32 })
        .where(eq(col(users, 'name'), 'Alice'))
        .execute()

      const rows = await db.select(users).where(eq(col(users, 'name'), 'Alice Smith')).execute()
      assert.equal(rows.length, 1)
      assert.equal(rows[0].age, 32)

      // Restore original name for other tests
      await db.update(users).set({ name: 'Alice' }).where(eq(col(users, 'name'), 'Alice Smith')).execute()
    })

    it('updates with null', async () => {
      await db.update(users).set({ age: null }).where(eq(col(users, 'name'), 'Dave')).execute()
      const rows = await db.select(users).where(eq(col(users, 'name'), 'Dave')).execute()
      assert.equal(rows[0].age, null)
    })

    it('throws on empty set', () => {
      assert.throws(() => db.update(users).where(eq(col(users, 'name'), 'Alice')).toAST(), /update requires at least one set clause/)
    })

    it('throws on unknown column', () => {
      assert.throws(() => db.update(users).set({ nonexistent: 42 }).toAST(), /Unknown column 'nonexistent'/)
    })

    it('toAST produces correct update structure', () => {
      const ast = db.update(users).set({ name: 'Test' }).where(eq(col(users, 'id'), 1)).toAST()
      assert.equal(ast.kind, 'update')
      assert.equal(ast.table, 'users')
      assert.equal(ast.sets.length, 1)
      assert.equal(ast.sets[0].col, 'name')
      assert.ok(ast.where)
    })
  })

  // ── DELETE ──

  describe('delete', () => {
    it('deletes rows with where clause', async () => {
      await db.insert(users).values({ name: 'Temp', email: 'temp@test.com' }).execute()
      const result = await db.delete(users).where(eq(col(users, 'name'), 'Temp')).execute()
      assert.equal(result.rowCount, 1)

      const rows = await db.select(users).where(eq(col(users, 'name'), 'Temp')).execute()
      assert.equal(rows.length, 0)
    })

    it('toAST produces correct delete structure', () => {
      const ast = db.delete(users).where(eq(col(users, 'id'), 1)).toAST()
      assert.equal(ast.kind, 'delete')
      assert.equal(ast.table, 'users')
      assert.ok(ast.where)
      assert.equal(ast.where.kind, 'binary')
    })
  })

  // ── Expression helpers ──

  describe('expressions', () => {
    it('literal creates correct AST nodes', () => {
      assert.deepStrictEqual(literal('hello'), { kind: 'string', value: 'hello' })
      assert.deepStrictEqual(literal(42), { kind: 'number', value: 42 })
      assert.deepStrictEqual(literal(true), { kind: 'boolean', value: true })
      assert.deepStrictEqual(literal(null), { kind: 'null' })
    })

    it('and with single expression returns it directly', () => {
      const expr = col(users, 'name')
      assert.strictEqual(and(expr), expr)
    })

    it('or with single expression returns it directly', () => {
      const expr = col(users, 'name')
      assert.strictEqual(or(expr), expr)
    })

    it('and throws on zero expressions', () => {
      assert.throws(() => and(), /at least one/)
    })

    it('or throws on zero expressions', () => {
      assert.throws(() => or(), /at least one/)
    })

    it('col produces correct column reference', () => {
      const c = col(users, 'name')
      assert.deepStrictEqual(c, { kind: 'column', table: 'users', name: 'name' })
    })

    it('nested and/or builds correct tree', () => {
      const expr = and(
        or(eq(col(users, 'name'), 'A'), eq(col(users, 'name'), 'B')),
        gt(col(users, 'age'), 20),
      )
      assert.equal(expr.kind, 'binary')
      assert.equal(expr.op, 'AND')
      assert.equal(expr.left.kind, 'binary')
      assert.equal(expr.left.op, 'OR')
    })
  })

  // ── AST inspection (no execution) ──

  describe('AST structure', () => {
    it('select with all clauses', () => {
      const ast = db
        .select(users)
        .columns(col(users, 'name'), alias(count(), 'cnt'))
        .where(gt(col(users, 'age'), 20))
        .groupBy(col(users, 'name'))
        .having(gt(alias(count(), 'cnt'), 1))
        .orderBy(desc(col(users, 'name')))
        .limit(10)
        .offset(5)
        .distinct()
        .toAST()

      const q = ast.query
      assert.equal(q.kind, 'select')
      assert.equal(q.exprs.length, 2)
      assert.ok(q.where)
      assert.ok(q.groupBy)
      assert.equal(q.groupBy.length, 1)
      assert.ok(q.having)
      assert.ok(q.orderBy)
      assert.equal(q.orderBy.length, 1)
      assert.equal(q.orderBy[0].direction, 'desc')
      assert.equal(q.limit, 10)
      assert.equal(q.offset, 5)
      assert.equal(q.distinct, true)
    })

    it('insert with multiple rows', () => {
      const ast = db
        .insert(users)
        .values({ name: 'A', email: 'a@test.com' }, { name: 'B', email: 'b@test.com' })
        .toAST()

      assert.equal(ast.kind, 'insert')
      assert.equal(ast.rows.length, 2)
      assert.ok(ast.columns.length >= 2)
    })

    it('join builds nested from clause', () => {
      const ast = db
        .select(users)
        .innerJoin(posts, eq(col(users, 'id'), col(posts, 'userId')))
        .toAST()

      const from = ast.query.from[0]
      assert.equal(from.kind, 'joinInner')
      assert.equal(from.left.kind, 'table')
      assert.equal(from.left.name, 'users')
      assert.equal(from.right.kind, 'table')
      assert.equal(from.right.name, 'posts')
    })
  })
})
