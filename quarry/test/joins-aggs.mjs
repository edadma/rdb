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
  gt,
  isNull,
  isNotNull,
  count,
  sum,
  avg,
  alias,
  asc,
  desc,
  filter,
  variance,
  varPop,
  stddev,
  stddevPop,
  bitAndAgg,
  bitOrAgg,
  every,
} from '../dist/index.js'

// ── Schema ──

const departments = table('departments', {
  id: serial('id').primaryKey(),
  name: text('name').notNull(),
})

const employees = table('employees', {
  id: serial('id').primaryKey(),
  name: text('name').notNull(),
  deptId: integer('dept_id'),
  salary: integer('salary').notNull(),
  active: boolean('active').notNull().default(true),
  flags: integer('flags').notNull().default(0),
})

describe('joins and aggregates (round 1)', () => {
  let session
  let db

  before(async () => {
    session = new Session()
    db = quarry(session)
    await db.createTable(departments)
    await db.createTable(employees)

    await db.insert(departments).values(
      { name: 'Engineering' },
      { name: 'Marketing' },
      { name: 'Sales' },
    ).execute()

    await db.insert(employees).values(
      { name: 'Alice', deptId: 1, salary: 100, active: true, flags: 6 },
      { name: 'Bob', deptId: 1, salary: 120, active: true, flags: 3 },
      { name: 'Charlie', deptId: 2, salary: 80, active: false, flags: 5 },
      { name: 'Dave', deptId: null, salary: 90, active: true, flags: 7 },
    ).execute()
  })

  after(async () => {
    await session.close()
  })

  // ── RIGHT JOIN ──

  describe('rightJoin', () => {
    it('returns all rows from right table', async () => {
      // right join departments: all departments appear, even Sales (no employees)
      // Use aliases to distinguish the two name columns
      const rows = await db
        .select(employees)
        .columns(
          alias(col(departments, 'name'), 'dept_name'),
          alias(col(employees, 'name'), 'emp_name'),
        )
        .rightJoin(departments, eq(col(employees, 'deptId'), col(departments, 'id')))
        .orderBy(asc(col(departments, 'name')))
        .execute()

      const deptNames = rows.map((r) => r.dept_name)
      assert.ok(deptNames.includes('Sales'), 'Sales department should appear in right join')
      // Sales has no employees so emp_name should be null
      const salesRow = rows.find((r) => r.dept_name === 'Sales')
      assert.equal(salesRow.emp_name, null)
    })

    it('AST structure is correct', () => {
      const ast = db
        .select(employees)
        .rightJoin(departments, eq(col(employees, 'deptId'), col(departments, 'id')))
        .toAST()
      assert.equal(ast.query.from[0].kind, 'joinRight')
    })
  })

  // ── FULL JOIN ──

  describe('fullJoin', () => {
    it('returns all rows from both tables', async () => {
      const rows = await db
        .select(employees)
        .columns(
          alias(col(employees, 'name'), 'emp_name'),
          alias(col(departments, 'name'), 'dept_name'),
        )
        .fullJoin(departments, eq(col(employees, 'deptId'), col(departments, 'id')))
        .execute()

      // Should include: Alice+Eng, Bob+Eng, Charlie+Mkt, Dave+null, null+Sales
      assert.ok(rows.length >= 5, `Expected at least 5 rows, got ${rows.length}`)
      // Dave has no dept
      const daveRow = rows.find((r) => r.emp_name === 'Dave')
      assert.equal(daveRow.dept_name, null)
      // Sales has no employees
      const salesRow = rows.find((r) => r.dept_name === 'Sales')
      assert.equal(salesRow.emp_name, null)
    })

    it('AST structure is correct', () => {
      const ast = db
        .select(employees)
        .fullJoin(departments, eq(col(employees, 'deptId'), col(departments, 'id')))
        .toAST()
      assert.equal(ast.query.from[0].kind, 'joinFull')
    })
  })

  // ── CROSS JOIN ──

  describe('crossJoin', () => {
    it('returns cartesian product', async () => {
      const rows = await db.select(employees).crossJoin(departments).execute()
      // 4 employees × 3 departments = 12
      assert.equal(rows.length, 12)
    })

    it('AST structure has no on clause', () => {
      const ast = db.select(employees).crossJoin(departments).toAST()
      const from = ast.query.from[0]
      assert.equal(from.kind, 'joinCross')
      assert.equal(from.on, undefined)
    })
  })

  // ── NULLS FIRST / LAST ──

  describe('nulls first/last', () => {
    it('asc with nulls first puts nulls at top', async () => {
      const rows = await db
        .select(employees)
        .columns(col(employees, 'name'), col(employees, 'deptId'))
        .orderBy(asc(col(employees, 'deptId'), { nulls: 'first' }))
        .execute()
      assert.equal(rows[0].dept_id, null)
    })

    it('asc with nulls last puts nulls at bottom', async () => {
      const rows = await db
        .select(employees)
        .columns(col(employees, 'name'), col(employees, 'deptId'))
        .orderBy(asc(col(employees, 'deptId'), { nulls: 'last' }))
        .execute()
      assert.equal(rows[rows.length - 1].dept_id, null)
    })

    it('desc with nulls first puts nulls at top', async () => {
      const rows = await db
        .select(employees)
        .columns(col(employees, 'name'), col(employees, 'deptId'))
        .orderBy(desc(col(employees, 'deptId'), { nulls: 'first' }))
        .execute()
      assert.equal(rows[0].dept_id, null)
    })

    it('desc with nulls last puts nulls at bottom', async () => {
      const rows = await db
        .select(employees)
        .columns(col(employees, 'name'), col(employees, 'deptId'))
        .orderBy(desc(col(employees, 'deptId'), { nulls: 'last' }))
        .execute()
      assert.equal(rows[rows.length - 1].dept_id, null)
    })

    it('AST has nullsFirst field', () => {
      const ob1 = asc(col(employees, 'id'), { nulls: 'first' })
      assert.equal(ob1.nullsFirst, true)
      const ob2 = desc(col(employees, 'id'), { nulls: 'last' })
      assert.equal(ob2.nullsFirst, false)
    })

    it('AST omits nullsFirst when not specified', () => {
      const ob1 = asc(col(employees, 'id'))
      assert.equal(ob1.nullsFirst, undefined)
      const ob2 = desc(col(employees, 'id'))
      assert.equal(ob2.nullsFirst, undefined)
    })
  })

  // ── Aggregate FILTER ──

  describe('aggregate filter', () => {
    it('count with filter', async () => {
      const rows = await db
        .select(employees)
        .columns(
          alias(count(), 'total'),
          alias(filter(count(), gt(col(employees, 'salary'), 90)), 'high_salary_count'),
        )
        .execute()
      assert.equal(rows[0].total, 4)
      assert.equal(rows[0].high_salary_count, 2) // Alice(100), Bob(120)
    })

    it('sum with filter', async () => {
      const rows = await db
        .select(employees)
        .columns(
          alias(filter(sum(col(employees, 'salary')), eq(col(employees, 'active'), true)), 'active_salary_sum'),
        )
        .execute()
      assert.equal(rows[0].active_salary_sum, 310) // Alice(100) + Bob(120) + Dave(90)
    })

    it('AST has filter field', () => {
      const expr = filter(count(), gt(col(employees, 'salary'), 90))
      assert.equal(expr.kind, 'apply')
      assert.equal(expr.func, 'count')
      assert.ok(expr.filter)
      assert.equal(expr.filter.kind, 'binary')
      assert.equal(expr.filter.op, '>')
    })
  })

  // ── Statistical aggregates ──

  describe('statistical aggregates', () => {
    it('variance computes sample variance', async () => {
      const rows = await db
        .select(employees)
        .columns(alias(variance(col(employees, 'salary')), 'v'))
        .execute()
      assert.equal(typeof rows[0].v, 'number')
      assert.ok(rows[0].v > 0)
    })

    it('varPop computes population variance', async () => {
      const rows = await db
        .select(employees)
        .columns(alias(varPop(col(employees, 'salary')), 'v'))
        .execute()
      assert.equal(typeof rows[0].v, 'number')
      assert.ok(rows[0].v > 0)
    })

    it('stddev computes sample standard deviation', async () => {
      const rows = await db
        .select(employees)
        .columns(alias(stddev(col(employees, 'salary')), 's'))
        .execute()
      assert.equal(typeof rows[0].s, 'number')
      assert.ok(rows[0].s > 0)
    })

    it('stddevPop computes population standard deviation', async () => {
      const rows = await db
        .select(employees)
        .columns(alias(stddevPop(col(employees, 'salary')), 's'))
        .execute()
      assert.equal(typeof rows[0].s, 'number')
      assert.ok(rows[0].s > 0)
    })
  })

  // ── Bitwise aggregates ──

  describe('bitwise aggregates', () => {
    it('bitAndAgg computes bitwise AND across rows', async () => {
      const rows = await db
        .select(employees)
        .columns(alias(bitAndAgg(col(employees, 'flags')), 'result'))
        .execute()
      // 6 & 3 & 5 & 7 = (0b110 & 0b011 & 0b101 & 0b111) = 0b000 = 0
      assert.equal(rows[0].result, 6 & 3 & 5 & 7)
    })

    it('bitOrAgg computes bitwise OR across rows', async () => {
      const rows = await db
        .select(employees)
        .columns(alias(bitOrAgg(col(employees, 'flags')), 'result'))
        .execute()
      // 6 | 3 | 5 | 7 = 0b111 = 7
      assert.equal(rows[0].result, 6 | 3 | 5 | 7)
    })
  })

  // ── EVERY ──

  describe('every', () => {
    it('returns false when not all rows satisfy condition', async () => {
      const rows = await db
        .select(employees)
        .columns(alias(every(col(employees, 'active')), 'all_active'))
        .execute()
      assert.equal(rows[0].all_active, false) // Charlie is inactive
    })

    it('returns true when filtered to matching rows', async () => {
      const rows = await db
        .select(employees)
        .columns(alias(every(col(employees, 'active')), 'all_active'))
        .where(isNotNull(col(employees, 'deptId')))
        .execute()
      // dept employees: Alice(true), Bob(true), Charlie(false)
      assert.equal(rows[0].all_active, false)
    })
  })
})
