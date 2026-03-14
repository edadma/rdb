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
  eq,
  gt,
  and,
  isNotNull,
  count,
  alias,
  asc,
  desc,
} from '../dist/index.js'

// ── Schema ──

const products = table('products', {
  id: serial('id').primaryKey(),
  name: text('name').notNull(),
  price: integer('price').notNull(),
  category: text('category').notNull(),
  active: boolean('active').notNull().default(true),
})

const archive = table('archive', {
  id: serial('id').primaryKey(),
  name: text('name').notNull(),
  price: integer('price').notNull(),
  category: text('category').notNull(),
})

const priceUpdates = table('price_updates', {
  id: serial('id').primaryKey(),
  productName: text('product_name').notNull(),
  newPrice: integer('new_price').notNull(),
})

const deleteList = table('delete_list', {
  id: serial('id').primaryKey(),
  productName: text('product_name').notNull(),
})

describe('INSERT...SELECT, UPDATE...FROM, DELETE...USING, DISTINCT ON', () => {
  let session
  let db

  before(async () => {
    session = new Session()
    db = quarry(session)
    await db.createTable(products)
    await db.createTable(archive)
    await db.createTable(priceUpdates)
    await db.createTable(deleteList)

    await db.insert(products).values(
      { name: 'Widget', price: 100, category: 'tools', active: true },
      { name: 'Gadget', price: 250, category: 'electronics', active: true },
      { name: 'Gizmo', price: 50, category: 'tools', active: false },
      { name: 'Doohickey', price: 75, category: 'electronics', active: true },
      { name: 'Thingamajig', price: 200, category: 'tools', active: true },
    ).execute()

    await db.insert(priceUpdates).values(
      { productName: 'Widget', newPrice: 120 },
      { productName: 'Gadget', newPrice: 225 },
    ).execute()

    await db.insert(deleteList).values(
      { productName: 'Gizmo' },
    ).execute()
  })

  after(async () => {
    await session.close()
  })

  // ── INSERT...SELECT ──

  describe('insertFrom', () => {
    it('inserts rows from a select query', async () => {
      // Archive all active products
      const selectExpr = db
        .select(products)
        .columns(products.name, products.price, products.category)
        .where(eq(products.active, true))
        .toExpr()

      await db
        .insertFrom(archive, selectExpr, ['name', 'price', 'category'])
        .execute()

      // Verify rows were inserted by querying the archive
      const archived = await db.select(archive).orderBy(asc(archive.name)).execute()
      assert.equal(archived.length, 4) // Widget, Gadget, Doohickey, Thingamajig
      const names = archived.map((r) => r.name).sort()
      assert.deepStrictEqual(names, ['Doohickey', 'Gadget', 'Thingamajig', 'Widget'])
    })

    it('inserted rows are queryable', async () => {
      const rows = await db.select(archive).orderBy(asc(archive.name)).execute()
      assert.equal(rows.length, 4)
      assert.equal(rows[0].name, 'Doohickey')
    })

    it('AST has query field instead of rows', () => {
      const selectExpr = db
        .select(products)
        .columns(products.name, products.price, products.category)
        .toExpr()

      const ast = db.insertFrom(archive, selectExpr, ['name', 'price', 'category']).toAST()
      assert.equal(ast.kind, 'insert')
      assert.ok(ast.query, 'AST should have query field')
      assert.equal(ast.query.kind, 'select')
      assert.equal(ast.rows, undefined)
      assert.deepStrictEqual(ast.columns, ['name', 'price', 'category'])
    })

    it('works without explicit columns when query matches all columns', async () => {
      // Clean archive first
      await db.delete(archive).execute()

      // Without columns, engine expects SELECT to produce values for all columns (id, name, price, category)
      // Use a subquery that produces all 4 columns
      const selectExpr = db
        .select(products)
        .columns(products.id, products.name, products.price, products.category)
        .where(eq(products.name, 'Gadget'))
        .toExpr()

      const rows = await db.insertFrom(archive, selectExpr).execute()
      assert.equal(rows.length, 1)
      assert.equal(rows[0].name, 'Gadget')
    })

    it('supports onConflictDoNothing', () => {
      const selectExpr = db.select(products).columns(products.name, products.price, products.category).toExpr()
      const ast = db.insertFrom(archive, selectExpr, ['name', 'price', 'category']).onConflictDoNothing().toAST()
      assert.ok(ast.onConflict)
      assert.equal(ast.onConflict.kind, 'doNothing')
    })
  })

  // ── UPDATE...FROM ──

  describe('update from', () => {
    it('updates rows using data from another table', async () => {
      await db
        .update(products)
        .set({ price: 999 }) // placeholder, actual update uses col reference
        .from(priceUpdates)
        .where(eq(products.name, priceUpdates.productName))
        .execute()

      // Verify Widget was updated (but to 999 since we used .set() with a literal)
      // For real FROM updates we'd need expression-based set values,
      // but this verifies the FROM clause is wired correctly
      const widget = await db.select(products).where(eq(products.name, 'Widget')).execute()
      assert.equal(widget[0].price, 999)
    })

    it('AST includes from field', () => {
      const ast = db
        .update(products)
        .set({ price: 100 })
        .from(priceUpdates)
        .where(eq(products.name, priceUpdates.productName))
        .toAST()

      assert.equal(ast.kind, 'update')
      assert.ok(ast.from, 'AST should have from field')
      assert.equal(ast.from.length, 1)
      assert.equal(ast.from[0].kind, 'table')
      assert.equal(ast.from[0].name, 'price_updates')
    })

    it('from accepts multiple tables', () => {
      const ast = db
        .update(products)
        .set({ price: 100 })
        .from(priceUpdates, archive)
        .where(eq(products.name, 'test'))
        .toAST()

      assert.equal(ast.from.length, 2)
    })
  })

  // ── DELETE...USING ──

  describe('delete using', () => {
    it('deletes rows based on another table', async () => {
      const result = await db
        .delete(products)
        .using(deleteList)
        .where(eq(products.name, deleteList.productName))
        .execute()

      assert.equal(result.rowCount, 1)

      // Verify Gizmo was deleted
      const remaining = await db.select(products).where(eq(products.name, 'Gizmo')).execute()
      assert.equal(remaining.length, 0)
    })

    it('AST includes using field', () => {
      const ast = db
        .delete(products)
        .using(deleteList)
        .where(eq(products.name, deleteList.productName))
        .toAST()

      assert.equal(ast.kind, 'delete')
      assert.ok(ast.using, 'AST should have using field')
      assert.equal(ast.using.length, 1)
      assert.equal(ast.using[0].kind, 'table')
      assert.equal(ast.using[0].name, 'delete_list')
    })
  })

  // ── DISTINCT ON ──

  describe('distinctOn', () => {
    it('returns one row per distinct value', async () => {
      // Current state: Widget(999,tools), Gadget(250,electronics), Doohickey(75,electronics), Thingamajig(200,tools)
      // Gizmo was deleted by delete using test
      const rows = await db
        .select(products)
        .distinctOn(products.category)
        .orderBy(asc(products.category), asc(products.name))
        .execute()

      const categories = rows.map((r) => r.category).sort()
      // Should have exactly one row per category
      assert.equal(new Set(categories).size, categories.length)
      assert.equal(rows.length, 2) // electronics, tools
    })

    it('picks the first row according to ORDER BY', async () => {
      // For each category, get the cheapest product
      const rows = await db
        .select(products)
        .distinctOn(products.category)
        .orderBy(asc(products.category), asc(products.price))
        .execute()

      const electronics = rows.find((r) => r.category === 'electronics')
      assert.equal(electronics.name, 'Doohickey') // price 75, cheapest in electronics

      const tools = rows.find((r) => r.category === 'tools')
      assert.equal(tools.name, 'Thingamajig') // price 200, cheaper than Widget at 999
    })

    it('AST has distinctOn field', () => {
      const ast = db
        .select(products)
        .distinctOn(products.category)
        .orderBy(asc(products.category))
        .toAST()

      assert.ok(ast.query.distinctOn, 'AST should have distinctOn field')
      assert.equal(ast.query.distinct, undefined) // distinctOn uses its own path, not plain distinct
      assert.equal(ast.query.distinctOn.length, 1)
      assert.equal(ast.query.distinctOn[0].kind, 'column')
    })

    it('supports multiple distinctOn columns', () => {
      const ast = db
        .select(products)
        .distinctOn(products.category, products.active)
        .toAST()

      assert.equal(ast.query.distinctOn.length, 2)
    })
  })
})
