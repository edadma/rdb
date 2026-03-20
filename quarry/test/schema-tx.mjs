import { describe, it, before, after } from 'node:test'
import assert from 'node:assert/strict'
import { Session } from '@petradb/engine'
import {
  quarry,
  table,
  serial,
  bigserial,
  text,
  varchar,
  char,
  integer,
  smallint,
  bigint,
  doublePrecision,
  real,
  numeric,
  boolean,
  uuid,
  timestamp,
  timestamptz,
  date,
  time,
  timetz,
  interval,
  json,
  bytea,
  eq,
  alias,
  fn,
  asc,
} from '../dist/index.js'

// ── Schema type tests ──

describe('schema column types', () => {
  let session
  let db

  before(async () => {
    session = new Session()
    db = quarry(session)
  })

  after(async () => {
    await session.close()
  })

  it('serial creates auto-incrementing integer', async () => {
    const t = table('t_serial', { id: serial('id').primaryKey(), name: text('name').notNull() })
    await db.createTable(t)
    const [r1] = await db.insert(t).values({ name: 'a' }).execute()
    const [r2] = await db.insert(t).values({ name: 'b' }).execute()
    assert.equal(r1.id, 1)
    assert.equal(r2.id, 2)
  })

  it('bigserial creates auto-incrementing bigint', async () => {
    const t = table('t_bigserial', { id: bigserial('id').primaryKey(), name: text('name').notNull() })
    await db.createTable(t)
    const [r] = await db.insert(t).values({ name: 'a' }).execute()
    assert.equal(typeof r.id, 'number')
    assert.equal(r.id, 1)
  })

  it('varchar creates variable-length text column', async () => {
    const t = table('t_varchar', { id: serial('id').primaryKey(), v: varchar('v', 50).notNull() })
    await db.createTable(t)
    const [r] = await db.insert(t).values({ v: 'hello' }).execute()
    assert.equal(r.v, 'hello')
  })

  it('char creates fixed-length text column', async () => {
    const t = table('t_char', { id: serial('id').primaryKey(), c: char('c', 5).notNull() })
    await db.createTable(t)
    const [r] = await db.insert(t).values({ c: 'hi' }).execute()
    assert.equal(typeof r.c, 'string')
  })

  it('smallint creates 16-bit integer column', async () => {
    const t = table('t_smallint', { id: serial('id').primaryKey(), s: smallint('s').notNull() })
    await db.createTable(t)
    const [r] = await db.insert(t).values({ s: 42 }).execute()
    assert.equal(r.s, 42)
  })

  it('bigint creates 64-bit integer column', async () => {
    const t = table('t_bigint', { id: serial('id').primaryKey(), b: bigint('b').notNull() })
    await db.createTable(t)
    const [r] = await db.insert(t).values({ b: 9999999 }).execute()
    assert.equal(r.b, 9999999)
  })

  it('doublePrecision creates floating-point column', async () => {
    const t = table('t_double', { id: serial('id').primaryKey(), d: doublePrecision('d').notNull() })
    await db.createTable(t)
    const [r] = await db.insert(t).values({ d: 3.14 }).execute()
    assert.ok(Math.abs(r.d - 3.14) < 0.001)
  })

  it('real creates single-precision float column', async () => {
    const t = table('t_real', { id: serial('id').primaryKey(), r: real('r').notNull() })
    await db.createTable(t)
    const [r] = await db.insert(t).values({ r: 2.5 }).execute()
    assert.ok(Math.abs(r.r - 2.5) < 0.001)
  })

  it('numeric creates arbitrary-precision column', async () => {
    const t = table('t_numeric', { id: serial('id').primaryKey(), n: numeric('n', 10, 2).notNull() })
    await db.createTable(t)
    const [r] = await db.insert(t).values({ n: 123.45 }).execute()
    assert.ok(Math.abs(r.n - 123.45) < 0.01)
  })

  it('uuid creates UUID column', async () => {
    const t = table('t_uuid', { id: serial('id').primaryKey(), u: uuid('u').notNull() })
    await db.createTable(t)
    const [r] = await db.insert(t).values({ u: '550e8400-e29b-41d4-a716-446655440000' }).execute()
    assert.equal(r.u, '550e8400-e29b-41d4-a716-446655440000')
  })

  it('timestamp creates timestamp column', async () => {
    const t = table('t_ts', { id: serial('id').primaryKey(), ts: timestamp('ts').notNull() })
    await db.createTable(t)
    const [r] = await db.insert(t).values({ ts: '2026-03-12 10:30:00' }).execute()
    assert.ok(r.ts)
  })

  it('date creates date column', async () => {
    const t = table('t_date', { id: serial('id').primaryKey(), d: date('d').notNull() })
    await db.createTable(t)
    const [r] = await db.insert(t).values({ d: '2026-03-12' }).execute()
    assert.ok(r.d)
  })

  it('time creates time column', async () => {
    const t = table('t_time', { id: serial('id').primaryKey(), t: time('t').notNull() })
    await db.createTable(t)
    const [r] = await db.insert(t).values({ t: '10:30:00' }).execute()
    assert.ok(r.t.startsWith('10:30'))
  })

  it('json creates JSON column', async () => {
    const t = table('t_json', { id: serial('id').primaryKey(), j: json('j').notNull() })
    await db.createTable(t)
    const [r] = await db.insert(t).values({ j: '{"key": "value"}' }).execute()
    assert.equal(r.j.key, 'value')
  })

  it('boolean creates boolean column', async () => {
    const t = table('t_bool', { id: serial('id').primaryKey(), b: boolean('b').notNull() })
    await db.createTable(t)
    const [r] = await db.insert(t).values({ b: true }).execute()
    assert.equal(r.b, true)
  })

  it('interval creates interval column', async () => {
    const t = table('t_interval', { id: serial('id').primaryKey(), i: interval('i').notNull() })
    await db.createTable(t)
    const [r] = await db.insert(t).values({ i: '1 day' }).execute()
    assert.ok(r.i)
  })

  it('column type names are correct after createTable', async () => {
    const t = table('t_all', {
      a: serial('a').primaryKey(),
      b: bigserial('b'),
      c: text('c'),
      d: varchar('d', 100),
      e: char('e', 10),
      f: integer('f'),
      g: smallint('g'),
      h: bigint('h'),
      i: doublePrecision('i'),
      j: real('j'),
      k: numeric('k', 10, 2),
      l: boolean('l'),
      m: uuid('m'),
      n: timestamp('n'),
      o: date('o'),
      p: time('p'),
      q: json('q'),
      r: bytea('r'),
      s: interval('s'),
      t: timestamptz('t'),
      u: timetz('u'),
    })

    await db.createTable(t)
    const [res] = await session.execute('SHOW COLUMNS t_all')
    const types = Object.fromEntries(res.rows.map((c) => [c.name, c.type]))
    assert.equal(types.a, 'serial')
    assert.equal(types.b, 'bigserial')
    assert.equal(types.c, 'text')
    assert.equal(types.d, 'varchar:100')
    assert.equal(types.e, 'char:10')
    assert.equal(types.f, 'integer')
    assert.equal(types.g, 'smallint')
    assert.equal(types.h, 'bigint')
    assert.equal(types.i, 'double')
    assert.equal(types.j, 'double') // engine normalizes real → double
    assert.equal(types.k, 'numeric:10:2')
    assert.equal(types.l, 'boolean')
    assert.equal(types.m, 'uuid')
    assert.equal(types.n, 'timestamp')
    assert.equal(types.o, 'date')
    assert.equal(types.p, 'time')
    assert.equal(types.q, 'json')
    assert.equal(types.r, 'bytea')
    assert.equal(types.s, 'interval')
    assert.equal(types.t, 'timestamptz')
    assert.equal(types.u, 'timetz')
  })
})

// ── Transaction tests ──

describe('transactions', () => {
  let session
  let db

  const accounts = table('accounts', {
    id: serial('id').primaryKey(),
    name: text('name').notNull(),
    balance: integer('balance').notNull(),
  })

  before(async () => {
    session = new Session()
    db = quarry(session)
    await db.createTable(accounts)
    await db.insert(accounts).values(
      { name: 'Alice', balance: 1000 },
      { name: 'Bob', balance: 500 },
    ).execute()
  })

  after(async () => {
    await session.close()
  })

  it('commits on success', async () => {
    await db.transaction(async (tx) => {
      await tx.update(accounts).set({ balance: 900 }).where(eq(accounts.name, 'Alice')).execute()
      await tx.update(accounts).set({ balance: 600 }).where(eq(accounts.name, 'Bob')).execute()
    })

    const rows = await db.select(accounts).orderBy(asc(accounts.name)).execute()
    assert.equal(rows[0].name, 'Alice')
    assert.equal(rows[0].balance, 900)
    assert.equal(rows[1].name, 'Bob')
    assert.equal(rows[1].balance, 600)
  })

  it('rolls back on error', async () => {
    const aliceBefore = (await db.select(accounts).where(eq(accounts.name, 'Alice')).execute())[0].balance

    await assert.rejects(async () => {
      await db.transaction(async (tx) => {
        await tx.update(accounts).set({ balance: 0 }).where(eq(accounts.name, 'Alice')).execute()
        throw new Error('simulated failure')
      })
    }, /simulated failure/)

    const aliceAfter = (await db.select(accounts).where(eq(accounts.name, 'Alice')).execute())[0].balance
    assert.equal(aliceAfter, aliceBefore)
  })

  it('returns the value from the callback', async () => {
    const result = await db.transaction(async (tx) => {
      const rows = await tx.select(accounts).where(eq(accounts.name, 'Alice')).execute()
      return rows[0].balance
    })
    assert.equal(typeof result, 'number')
  })

  it('nested operations within transaction', async () => {
    await db.transaction(async (tx) => {
      const [inserted] = await tx.insert(accounts).values({ name: 'Charlie', balance: 200 }).execute()
      assert.equal(inserted.name, 'Charlie')

      await tx.update(accounts).set({ balance: 300 }).where(eq(accounts.name, 'Charlie')).execute()

      const rows = await tx.select(accounts).where(eq(accounts.name, 'Charlie')).execute()
      assert.equal(rows[0].balance, 300)
    })

    // Verify committed
    const rows = await db.select(accounts).where(eq(accounts.name, 'Charlie')).execute()
    assert.equal(rows.length, 1)
    assert.equal(rows[0].balance, 300)
  })
})

// ── Returning tests ──

describe('returning', () => {
  let session
  let db

  const items = table('ret_items', {
    id: serial('id').primaryKey(),
    name: text('name').notNull(),
    price: integer('price').notNull(),
  })

  before(async () => {
    session = new Session()
    db = quarry(session)
    await db.createTable(items)
    await db.insert(items).values(
      { name: 'A', price: 10 },
      { name: 'B', price: 20 },
      { name: 'C', price: 30 },
    ).execute()
  })

  after(async () => {
    await session.close()
  })

  it('insert returns inserted row', async () => {
    const rows = await db.insert(items).values({ name: 'D', price: 40 }).execute()
    assert.equal(rows.length, 1)
    assert.equal(rows[0].name, 'D')
    assert.equal(rows[0].price, 40)
  })

  it('update returns rows in result', async () => {
    const result = await db
      .update(items)
      .set({ price: 99 })
      .where(eq(items.name, 'A'))
      .returning(items.id, items.name, items.price)
      .execute()
    assert.equal(result.rowCount, 1)
    assert.equal(result.rows.length, 1)
    assert.equal(result.rows[0].name, 'A')
    assert.equal(result.rows[0].price, 99)
  })

  it('delete returns rows in result', async () => {
    const result = await db
      .delete(items)
      .where(eq(items.name, 'C'))
      .returning(items.id, items.name, items.price)
      .execute()
    assert.equal(result.rowCount, 1)
    assert.equal(result.rows.length, 1)
    assert.equal(result.rows[0].name, 'C')
    assert.equal(result.rows[0].price, 30)
  })
})
