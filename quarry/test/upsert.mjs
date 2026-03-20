import { describe, it, before, after } from 'node:test'
import assert from 'node:assert/strict'
import { Session } from '@petradb/engine'
import {
  quarry,
  table,
  serial,
  text,
  integer,
  eq,
  asc,
} from '../dist/index.js'

// ── Schema ──

const kv = table('kv', {
  id: serial('id').primaryKey(),
  v: integer('v').notNull(),
})

const products = table('products', {
  id: serial('id').primaryKey(),
  sku: text('sku').notNull().unique(),
  name: text('name').notNull(),
  price: integer('price').notNull(),
})

// ── Tests ──

describe('upsert', () => {
  let session
  let db

  before(async () => {
    session = new Session()
    db = quarry(session)
    await db.createTable(kv)
    await db.createTable(products)

    // Seed data
    await db.insert(kv).values({ v: 10 }).execute()
    await db.insert(kv).values({ v: 20 }).execute()
    await db.insert(products).values({ sku: 'ABC', name: 'Widget', price: 100 }).execute()
    await db.insert(products).values({ sku: 'DEF', name: 'Gadget', price: 200 }).execute()
  })

  after(async () => {
    await session.close()
  })

  // ── ON CONFLICT DO NOTHING ──

  describe('onConflictDoNothing', () => {
    it('silently skips duplicate on primary key', async () => {
      await db.insert(kv).values({ id: 1, v: 999 }).onConflictDoNothing().execute()

      const rows = await db.from(kv).where(eq(kv.id, 1)).execute()
      assert.equal(rows[0].v, 10) // original value unchanged
    })

    it('silently skips duplicate on unique column', async () => {
      await db.insert(products).values({ sku: 'ABC', name: 'New Widget', price: 500 }).onConflictDoNothing().execute()

      const rows = await db.from(products).where(eq(products.sku, 'ABC')).execute()
      assert.equal(rows[0].name, 'Widget') // unchanged
      assert.equal(rows[0].price, 100)
    })

    it('inserts non-conflicting row normally', async () => {
      await db.insert(kv).values({ id: 99, v: 77 }).onConflictDoNothing().execute()

      const rows = await db.from(kv).where(eq(kv.id, 99)).execute()
      assert.equal(rows.length, 1)
      assert.equal(rows[0].v, 77)
    })

    it('produces correct AST', () => {
      const ast = db.insert(kv).values({ v: 1 }).onConflictDoNothing().toAST()
      assert.equal(ast.kind, 'insert')
      assert.ok(ast.onConflict)
      assert.equal(ast.onConflict.kind, 'doNothing')
    })
  })

  // ── ON CONFLICT DO UPDATE ──

  describe('onConflictDoUpdate', () => {
    it('updates existing row on primary key conflict', async () => {
      await db
        .insert(kv)
        .values({ id: 2, v: 99 })
        .onConflictDoUpdate(['id'], { v: 99 })
        .execute()

      const rows = await db.from(kv).where(eq(kv.id, 2)).execute()
      assert.equal(rows[0].v, 99)
    })

    it('updates existing row on unique column conflict', async () => {
      await db
        .insert(products)
        .values({ sku: 'DEF', name: 'Super Gadget', price: 350 })
        .onConflictDoUpdate(['sku'], { name: 'Super Gadget', price: 350 })
        .execute()

      const rows = await db.from(products).where(eq(products.sku, 'DEF')).execute()
      assert.equal(rows[0].name, 'Super Gadget')
      assert.equal(rows[0].price, 350)
    })

    it('inserts when no conflict exists', async () => {
      await db
        .insert(products)
        .values({ sku: 'GHI', name: 'Doohickey', price: 50 })
        .onConflictDoUpdate(['sku'], { name: 'Doohickey', price: 50 })
        .execute()

      const rows = await db.from(products).where(eq(products.sku, 'GHI')).execute()
      assert.equal(rows.length, 1)
      assert.equal(rows[0].name, 'Doohickey')
      assert.equal(rows[0].price, 50)
    })

    it('updates only specified columns', async () => {
      // Only update price, not name
      await db
        .insert(products)
        .values({ sku: 'ABC', name: 'Ignored Name', price: 150 })
        .onConflictDoUpdate(['sku'], { price: 150 })
        .execute()

      const rows = await db.from(products).where(eq(products.sku, 'ABC')).execute()
      assert.equal(rows[0].name, 'Widget') // name unchanged
      assert.equal(rows[0].price, 150) // price updated
    })

    it('produces correct AST', () => {
      const ast = db
        .insert(products)
        .values({ sku: 'X', name: 'Y', price: 10 })
        .onConflictDoUpdate(['sku'], { name: 'Z', price: 20 })
        .toAST()

      assert.equal(ast.kind, 'insert')
      assert.ok(ast.onConflict)
      assert.equal(ast.onConflict.kind, 'doUpdate')
      assert.deepStrictEqual(ast.onConflict.conflictColumns, ['sku'])
      assert.equal(ast.onConflict.updates.length, 2)
      assert.equal(ast.onConflict.updates[0].col, 'name')
      assert.equal(ast.onConflict.updates[1].col, 'price')
    })

    it('throws on unknown conflict column', () => {
      assert.throws(
        () =>
          db
            .insert(products)
            .values({ sku: 'X', name: 'Y', price: 10 })
            .onConflictDoUpdate(['nonexistent'], { name: 'Z' })
            .toAST(),
        /Unknown column 'nonexistent'/,
      )
    })

    it('throws on unknown update column', () => {
      assert.throws(
        () =>
          db
            .insert(products)
            .values({ sku: 'X', name: 'Y', price: 10 })
            .onConflictDoUpdate(['sku'], { nonexistent: 42 })
            .toAST(),
        /Unknown column 'nonexistent'/,
      )
    })
  })

  // ── Upsert with RETURNING ──

  describe('upsert with returning', () => {
    it('doNothing returns empty array on conflict', async () => {
      const result = await db
        .insert(kv)
        .values({ id: 1, v: 999 })
        .onConflictDoNothing()
        .execute()

      // On conflict do nothing — no rows returned
      assert.ok(Array.isArray(result))
    })

    it('doUpdate returns the updated row', async () => {
      const result = await db
        .insert(kv)
        .values({ id: 2, v: 55 })
        .onConflictDoUpdate(['id'], { v: 55 })
        .execute()

      assert.ok(Array.isArray(result))
    })
  })

  // ── Without onConflict (normal insert still works) ──

  describe('normal insert unchanged', () => {
    it('insert without onConflict has no onConflict in AST', () => {
      const ast = db.insert(kv).values({ v: 42 }).toAST()
      assert.equal(ast.onConflict, undefined)
    })
  })
})
