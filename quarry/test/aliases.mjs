import { describe, it, before, after } from 'node:test'
import assert from 'node:assert/strict'
import { Session } from '@petradb/engine'
import {
  quarry,
  table,
  serial,
  text,
  integer,
  col,
  eq,
  gt,
  isNull,
  alias,
  count,
  asc,
  desc,
} from '../dist/index.js'

// ── Schema ──

const employees = table('employees', {
  id: serial('id').primaryKey(),
  name: text('name').notNull(),
  managerId: integer('manager_id'),
  departmentId: integer('department_id').notNull(),
  salary: integer('salary').notNull(),
})

const departments = table('departments', {
  id: serial('id').primaryKey(),
  name: text('name').notNull(),
})

// ── Tests ──

describe('table aliases', () => {
  let session
  let db

  before(async () => {
    session = new Session()
    db = quarry(session)
    await db.createTable(departments)
    await db.createTable(employees)

    // Seed departments
    await db.insert(departments).values({ name: 'Engineering' }).execute()
    await db.insert(departments).values({ name: 'Marketing' }).execute()

    // Seed employees with manager hierarchy
    // Alice (id=1) - no manager (CEO)
    await db.insert(employees).values({ name: 'Alice', managerId: null, departmentId: 1, salary: 150000 }).execute()
    // Bob (id=2) - reports to Alice
    await db.insert(employees).values({ name: 'Bob', managerId: 1, departmentId: 1, salary: 120000 }).execute()
    // Carol (id=3) - reports to Alice
    await db.insert(employees).values({ name: 'Carol', managerId: 1, departmentId: 2, salary: 110000 }).execute()
    // Dave (id=4) - reports to Bob
    await db.insert(employees).values({ name: 'Dave', managerId: 2, departmentId: 1, salary: 90000 }).execute()
    // Eve (id=5) - reports to Carol
    await db.insert(employees).values({ name: 'Eve', managerId: 3, departmentId: 2, salary: 85000 }).execute()
  })

  after(async () => {
    await session.close()
  })

  // ── as() basics ──

  describe('as() method', () => {
    it('creates an aliased table with the alias as _name', () => {
      const e = employees.as('e')
      assert.equal(e._name, 'e')
      assert.equal(e._originalName, 'employees')
    })

    it('preserves columns from original table', () => {
      const e = employees.as('e')
      assert.ok(e._columns.id)
      assert.ok(e._columns.name)
      assert.ok(e._columns.managerId)
    })

    it('two aliases of same table have different names', () => {
      const e1 = employees.as('e1')
      const e2 = employees.as('e2')
      assert.equal(e1._name, 'e1')
      assert.equal(e2._name, 'e2')
      assert.equal(e1._originalName, 'employees')
      assert.equal(e2._originalName, 'employees')
    })

    it('col() uses alias name for table reference', () => {
      const e = employees.as('e')
      const c = col(e, 'name')
      assert.equal(c.kind, 'column')
      assert.equal(c.table, 'e')
      assert.equal(c.name, 'name')
    })

    it('throws on toCreateAST for aliased table', () => {
      const e = employees.as('e')
      assert.throws(() => e.toCreateAST(), /Cannot create table from an alias/)
    })

    it('re-aliasing works correctly', () => {
      const e1 = employees.as('e1')
      const e2 = e1.as('e2')
      assert.equal(e2._name, 'e2')
      assert.equal(e2._originalName, 'employees')
    })
  })

  // ── AST structure ──

  describe('AST structure', () => {
    it('aliased table in FROM generates aliasRelation node', () => {
      const e = employees.as('e')
      const ast = db.select(e).toAST()
      const from = ast.query.from[0]
      assert.equal(from.kind, 'aliasRelation')
      assert.equal(from.relation.kind, 'table')
      assert.equal(from.relation.name, 'employees')
      assert.equal(from.alias, 'e')
    })

    it('non-aliased table in FROM generates plain table node', () => {
      const ast = db.select(employees).toAST()
      const from = ast.query.from[0]
      assert.equal(from.kind, 'table')
      assert.equal(from.name, 'employees')
    })

    it('aliased table in JOIN generates aliasRelation node', () => {
      const e1 = employees.as('e1')
      const e2 = employees.as('e2')
      const ast = db
        .select(e1)
        .innerJoin(e2, eq(col(e1, 'managerId'), col(e2, 'id')))
        .toAST()

      const from = ast.query.from[0]
      assert.equal(from.kind, 'joinInner')
      assert.equal(from.left.kind, 'aliasRelation')
      assert.equal(from.left.alias, 'e1')
      assert.equal(from.right.kind, 'aliasRelation')
      assert.equal(from.right.alias, 'e2')
    })
  })

  // ── Self-join queries ──

  describe('self-join', () => {
    it('finds employees with their manager names', async () => {
      const e = employees.as('e')
      const m = employees.as('m')

      const rows = await db
        .select(e)
        .columns(
          alias(col(e, 'name'), 'employee_name'),
          alias(col(m, 'name'), 'manager_name'),
        )
        .innerJoin(m, eq(col(e, 'managerId'), col(m, 'id')))
        .orderBy(asc(col(e, 'name')))
        .execute()

      // Alice has no manager (excluded by inner join)
      assert.equal(rows.length, 4)
      assert.equal(rows[0].employee_name, 'Bob')
      assert.equal(rows[0].manager_name, 'Alice')
      assert.equal(rows[1].employee_name, 'Carol')
      assert.equal(rows[1].manager_name, 'Alice')
      assert.equal(rows[2].employee_name, 'Dave')
      assert.equal(rows[2].manager_name, 'Bob')
      assert.equal(rows[3].employee_name, 'Eve')
      assert.equal(rows[3].manager_name, 'Carol')
    })

    it('left join includes employees without managers', async () => {
      const e = employees.as('e')
      const m = employees.as('m')

      const rows = await db
        .select(e)
        .columns(
          alias(col(e, 'name'), 'employee_name'),
          alias(col(m, 'name'), 'manager_name'),
        )
        .leftJoin(m, eq(col(e, 'managerId'), col(m, 'id')))
        .orderBy(asc(col(e, 'name')))
        .execute()

      assert.equal(rows.length, 5)
      // Alice has no manager
      assert.equal(rows[0].employee_name, 'Alice')
      assert.equal(rows[0].manager_name, null)
    })

    it('filters on aliased columns', async () => {
      const e = employees.as('e')
      const m = employees.as('m')

      // Find employees whose manager earns more than 130000
      const rows = await db
        .select(e)
        .columns(alias(col(e, 'name'), 'employee_name'))
        .innerJoin(m, eq(col(e, 'managerId'), col(m, 'id')))
        .where(gt(col(m, 'salary'), 130000))
        .execute()

      // Only Alice earns > 130000, her reports are Bob and Carol
      assert.equal(rows.length, 2)
      const names = rows.map((r) => r.employee_name).sort()
      assert.deepStrictEqual(names, ['Bob', 'Carol'])
    })

    it('counts direct reports per manager', async () => {
      const e = employees.as('e')
      const m = employees.as('m')

      const rows = await db
        .select(m)
        .columns(
          alias(col(m, 'name'), 'manager_name'),
          alias(count(), 'report_count'),
        )
        .innerJoin(e, eq(col(e, 'managerId'), col(m, 'id')))
        .groupBy(col(m, 'name'))
        .orderBy(desc(alias(count(), 'report_count')))
        .execute()

      assert.ok(rows.length > 0)
      const alice = rows.find((r) => r.manager_name === 'Alice')
      assert.equal(alice.report_count, 2) // Bob, Carol
      const bob = rows.find((r) => r.manager_name === 'Bob')
      assert.equal(bob.report_count, 1) // Dave
    })
  })

  // ── Alias with other tables ──

  describe('alias with different tables', () => {
    it('join aliased table with non-aliased table', async () => {
      const e = employees.as('e')

      const rows = await db
        .select(e)
        .columns(
          alias(col(e, 'name'), 'employee_name'),
          alias(col(departments, 'name'), 'dept_name'),
        )
        .innerJoin(departments, eq(col(e, 'departmentId'), col(departments, 'id')))
        .where(eq(col(e, 'name'), 'Alice'))
        .execute()

      assert.equal(rows.length, 1)
      assert.equal(rows[0].employee_name, 'Alice')
      assert.equal(rows[0].dept_name, 'Engineering')
    })

    it('aliased + non-aliased + self-join', async () => {
      const e = employees.as('e')
      const m = employees.as('m')

      // Employee name, manager name, department name
      const rows = await db
        .select(e)
        .columns(
          alias(col(e, 'name'), 'employee_name'),
          alias(col(m, 'name'), 'manager_name'),
          alias(col(departments, 'name'), 'dept_name'),
        )
        .innerJoin(m, eq(col(e, 'managerId'), col(m, 'id')))
        .innerJoin(departments, eq(col(e, 'departmentId'), col(departments, 'id')))
        .where(eq(col(e, 'name'), 'Dave'))
        .execute()

      assert.equal(rows.length, 1)
      assert.equal(rows[0].employee_name, 'Dave')
      assert.equal(rows[0].manager_name, 'Bob')
      assert.equal(rows[0].dept_name, 'Engineering')
    })
  })

  // ── Select from aliased table (no join) ──

  describe('aliased table without joins', () => {
    it('selects from aliased table', async () => {
      const e = employees.as('e')
      const rows = await db
        .select(e)
        .where(eq(col(e, 'name'), 'Alice'))
        .execute()

      assert.equal(rows.length, 1)
      assert.equal(rows[0].name, 'Alice')
    })

    it('orderBy on aliased columns works', async () => {
      const e = employees.as('e')
      const rows = await db
        .select(e)
        .orderBy(desc(col(e, 'salary')))
        .limit(3)
        .execute()

      assert.equal(rows.length, 3)
      assert.equal(rows[0].name, 'Alice') // 150000
      assert.equal(rows[1].name, 'Bob')   // 120000
    })
  })
})
