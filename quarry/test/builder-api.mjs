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
  avg,
  count,
  sum,
  max,
  alias,
  asc,
  desc,
  inSubquery,
  notInSubquery,
  subquery,
  exists,
  withCTE,
} from '../dist/index.js'

// ── Schema ──

const departments = table('departments', {
  id: serial('id').primaryKey(),
  name: text('name').notNull(),
})

const employees = table('employees', {
  id: serial('id').primaryKey(),
  name: text('name').notNull(),
  departmentId: integer('department_id').notNull(),
  salary: integer('salary').notNull(),
})

const projects = table('projects', {
  id: serial('id').primaryKey(),
  name: text('name').notNull(),
  leadId: integer('lead_id'),
})

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

// ── Tests ──

describe('builder-based API (no raw AST)', () => {
  let session
  let db

  before(async () => {
    session = new Session()
    db = quarry(session)
    await db.createTable(departments)
    await db.createTable(employees)
    await db.createTable(projects)
    await db.createTable(products)
    await db.createTable(archive)

    await db.insert(departments).values(
      { name: 'Engineering' },
      { name: 'Marketing' },
      { name: 'Sales' },
    ).execute()

    await db.insert(employees).values(
      { name: 'Alice', departmentId: 1, salary: 120000 },
      { name: 'Bob', departmentId: 1, salary: 100000 },
      { name: 'Carol', departmentId: 2, salary: 90000 },
      { name: 'Dave', departmentId: 2, salary: 85000 },
      { name: 'Eve', departmentId: 3, salary: 95000 },
    ).execute()

    await db.insert(projects).values(
      { name: 'Alpha', leadId: 1 },
      { name: 'Beta', leadId: 3 },
      { name: 'Gamma', leadId: null },
    ).execute()

    await db.insert(products).values(
      { name: 'Widget', price: 100, category: 'tools', active: true },
      { name: 'Gadget', price: 250, category: 'electronics', active: true },
      { name: 'Gizmo', price: 50, category: 'tools', active: false },
    ).execute()
  })

  after(async () => {
    await session.close()
  })

  // ── inSubquery accepts SelectBuilder ──

  describe('inSubquery', () => {
    it('accepts a SelectBuilder directly', async () => {
      const engDepts = db.select(departments)
        .columns(departments.id)
        .where(eq(departments.name, 'Engineering'))

      const rows = await db.select(employees)
        .where(inSubquery(employees.departmentId, engDepts))
        .orderBy(asc(employees.name))
        .execute()

      assert.equal(rows.length, 2)
      assert.equal(rows[0].name, 'Alice')
      assert.equal(rows[1].name, 'Bob')
    })

    it('works with multi-value subquery builder', async () => {
      const deptQuery = db.select(departments)
        .columns(departments.id)
        .where(gt(departments.id, 1))

      const rows = await db.select(employees)
        .where(inSubquery(employees.departmentId, deptQuery))
        .orderBy(asc(employees.name))
        .execute()

      assert.equal(rows.length, 3)
    })
  })

  // ── notInSubquery accepts SelectBuilder ──

  describe('notInSubquery', () => {
    it('accepts a SelectBuilder directly', async () => {
      const engDepts = db.select(departments)
        .columns(departments.id)
        .where(eq(departments.name, 'Engineering'))

      const rows = await db.select(employees)
        .where(notInSubquery(employees.departmentId, engDepts))
        .orderBy(asc(employees.name))
        .execute()

      assert.equal(rows.length, 3)
      const names = rows.map((r) => r.name)
      assert.ok(!names.includes('Alice'))
      assert.ok(!names.includes('Bob'))
    })
  })

  // ── subquery accepts SelectBuilder ──

  describe('subquery', () => {
    it('accepts a SelectBuilder for scalar comparison', async () => {
      const avgSalary = db.select(employees).columns(avg(employees.salary))

      const rows = await db.select(employees)
        .where(gt(employees.salary, subquery(avgSalary)))
        .orderBy(asc(employees.name))
        .execute()

      // avg = 98000; above: Alice (120000), Bob (100000)
      assert.equal(rows.length, 2)
      assert.equal(rows[0].name, 'Alice')
      assert.equal(rows[1].name, 'Bob')
    })

    it('accepts a SelectBuilder as selected column', async () => {
      const maxSal = db.select(employees)
        .columns(max(employees.salary))
        .where(eq(employees.departmentId, departments.id))

      const rows = await db.select(departments)
        .columns(departments.name, alias(subquery(maxSal), 'max_salary'))
        .orderBy(asc(departments.name))
        .execute()

      assert.equal(rows.length, 3)
      const eng = rows.find((r) => r.name === 'Engineering')
      assert.equal(eng.max_salary, 120000)
    })
  })

  // ── exists accepts SelectBuilder ──

  describe('exists', () => {
    it('accepts a SelectBuilder directly', async () => {
      const leadsProject = db.select(projects)
        .columns(projects.id)
        .where(eq(projects.leadId, employees.id))

      const rows = await db.select(employees)
        .where(exists(leadsProject))
        .orderBy(asc(employees.name))
        .execute()

      assert.equal(rows.length, 2)
      assert.equal(rows[0].name, 'Alice')
      assert.equal(rows[1].name, 'Carol')
    })
  })

  // ── insertFrom accepts SelectBuilder ──

  describe('insertFrom', () => {
    it('accepts a SelectBuilder directly', async () => {
      const activeProducts = db.select(products)
        .columns(products.name, products.price, products.category)
        .where(eq(products.active, true))

      await db.insertFrom(archive, activeProducts, ['name', 'price', 'category']).execute()

      const archived = await db.select(archive).orderBy(asc(archive.name)).execute()
      assert.equal(archived.length, 2)
      const names = archived.map((r) => r.name).sort()
      assert.deepStrictEqual(names, ['Gadget', 'Widget'])
    })
  })

  // ── executeQuery accepts SelectBuilder ──

  describe('executeQuery', () => {
    it('accepts a SelectBuilder directly', async () => {
      const query = db.select(employees)
        .columns(employees.name)
        .orderBy(asc(employees.name))
        .limit(2)

      const rows = await db.executeQuery(query)
      assert.equal(rows.length, 2)
      assert.equal(rows[0].name, 'Alice')
      assert.equal(rows[1].name, 'Bob')
    })

    it('accepts a SetOperationBuilder directly', async () => {
      const eng = db.select(employees)
        .columns(employees.name)
        .where(eq(employees.departmentId, 1))
      const sales = db.select(employees)
        .columns(employees.name)
        .where(eq(employees.departmentId, 3))

      const rows = await db.executeQuery(eng.union(sales))
      assert.equal(rows.length, 3) // Alice, Bob, Eve
    })
  })

  // ── withCTE accepts SelectBuilder ──

  describe('withCTE', () => {
    it('accepts SelectBuilder for CTE queries and main query', async () => {
      const cteQuery = db.select(employees)
        .columns(employees.departmentId, alias(sum(employees.salary), 'total'))
        .groupBy(employees.departmentId)

      const deptTotals = table('dept_totals', {
        departmentId: integer('department_id'),
        total: integer('total'),
      })

      const mainQuery = db.select(deptTotals).orderBy(desc(deptTotals.total))

      const rows = await db.executeQuery(
        withCTE([{ name: 'dept_totals', query: cteQuery }], mainQuery),
      )
      assert.equal(rows.length, 3)
      assert.equal(rows[0].total, 220000) // eng: 120k + 100k
    })

    it('CTE with column aliases and builder queries', async () => {
      const cteQuery = db.select(employees)
        .columns(employees.departmentId, alias(count(), 'c'))
        .groupBy(employees.departmentId)

      const deptCounts = table('dept_counts', {
        department: integer('department'),
        headcount: integer('headcount'),
      })

      const mainQuery = db.select(deptCounts)

      const rows = await db.executeQuery(
        withCTE(
          [{ name: 'dept_counts', columns: ['department', 'headcount'], query: cteQuery }],
          mainQuery,
        ),
      )
      assert.equal(rows.length, 3)
      assert.ok('department' in rows[0])
      assert.ok('headcount' in rows[0])
    })
  })

  // ── ToCreateAST not exported ──

  describe('public API', () => {
    it('ToCreateAST is not exported', async () => {
      const mod = await import('../dist/index.js')
      assert.equal(mod.ToCreateAST, undefined)
    })
  })
})
