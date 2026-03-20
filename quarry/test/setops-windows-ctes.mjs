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
  alias,
  asc,
  desc,
  count,
  sum,
  // Set operations are on SelectBuilder
  // Window functions
  over,
  rowNumber,
  rank,
  denseRank,
  ntile,
  lag,
  lead,
  firstValue,
  lastValue,
  nthValue,
  unboundedPreceding,
  unboundedFollowing,
  currentRow,
  preceding,
  following,
  // CTEs
  withCTE,
  // Named scalar functions
  lower,
  upper,
  length,
  trim,
  substring,
  replace,
  concatWs,
  reverse,
  repeat,
  lpad,
  rpad,
  abs,
  ceil,
  floor,
  round,
  trunc,
  sqrt,
  sign,
  greatest,
  least,
  coalesce,
  nullif,
  now,
  genRandomUuid,
  datePart,
  toChar,
  literal,
} from '../dist/index.js'

// ── Schema ──

const employees = table('employees', {
  id: serial('id').primaryKey(),
  name: text('name').notNull(),
  dept: text('dept').notNull(),
  salary: integer('salary').notNull(),
  active: boolean('active').notNull().default(true),
})

const contractors = table('contractors', {
  id: serial('id').primaryKey(),
  name: text('name').notNull(),
  dept: text('dept').notNull(),
  salary: integer('salary').notNull(),
  active: boolean('active').notNull().default(true),
})

describe('set operations, window functions, CTEs, named functions', () => {
  let session
  let db

  before(async () => {
    session = new Session()
    db = quarry(session)
    await db.createTable(employees)
    await db.createTable(contractors)

    await db.insert(employees).values(
      { name: 'Alice', dept: 'eng', salary: 120 },
      { name: 'Bob', dept: 'eng', salary: 100 },
      { name: 'Carol', dept: 'sales', salary: 90 },
      { name: 'Dave', dept: 'sales', salary: 110 },
      { name: 'Eve', dept: 'eng', salary: 95 },
    ).execute()

    await db.insert(contractors).values(
      { name: 'Frank', dept: 'eng', salary: 80 },
      { name: 'Bob', dept: 'eng', salary: 100 },
    ).execute()
  })

  after(async () => {
    await session.close()
  })

  // ── SET OPERATIONS ──

  describe('set operations', () => {
    it('UNION removes duplicates', async () => {
      const empNames = db.select(employees.name).from(employees)
      const conNames = db.select(contractors.name).from(contractors)
      const rows = await empNames.union(conNames).execute()
      const names = rows.map((r) => r.name).sort()
      // Bob appears in both but UNION deduplicates
      assert.ok(!names.includes(undefined))
      assert.equal(new Set(names).size, names.length)
      assert.ok(names.includes('Frank'))
      assert.ok(names.includes('Alice'))
    })

    it('UNION ALL keeps duplicates', async () => {
      const empNames = db.select(employees.name).from(employees)
      const conNames = db.select(contractors.name).from(contractors)
      const rows = await empNames.unionAll(conNames).execute()
      // 5 employees + 2 contractors = 7
      assert.equal(rows.length, 7)
    })

    it('INTERSECT returns common rows', async () => {
      const empNameDept = db.select(employees.name, employees.dept).from(employees)
      const conNameDept = db.select(contractors.name, contractors.dept).from(contractors)
      const rows = await empNameDept.intersect(conNameDept).execute()
      // Only Bob in eng appears in both
      assert.equal(rows.length, 1)
      assert.equal(rows[0].name, 'Bob')
    })

    it('EXCEPT returns rows in first but not second', async () => {
      const empNameDept = db.select(employees.name, employees.dept).from(employees)
      const conNameDept = db.select(contractors.name, contractors.dept).from(contractors)
      const rows = await empNameDept.except(conNameDept).execute()
      // All employees except Bob
      assert.equal(rows.length, 4)
      const names = rows.map((r) => r.name).sort()
      assert.ok(!names.includes('Bob'))
    })

    it('AST structure is correct', () => {
      const q1 = db.select(employees.name).from(employees)
      const q2 = db.select(contractors.name).from(contractors)
      const ast = q1.union(q2).toAST()
      assert.equal(ast.query.kind, 'setOperation')
      assert.equal(ast.query.op, 'UNION')
      assert.equal(ast.query.left.kind, 'select')
      assert.equal(ast.query.right.kind, 'select')
    })
  })

  // ── WINDOW FUNCTIONS ──

  describe('window functions', () => {
    it('rowNumber assigns sequential numbers', async () => {
      const rows = await db
        .select(
          employees.name,
          alias(rowNumber({ orderBy: [asc(employees.salary)] }), 'rn'),
        )
        .from(employees)
        .orderBy(asc(employees.salary))
        .execute()
      assert.equal(rows[0].rn, 1)
      assert.equal(rows[4].rn, 5)
    })

    it('rank with ties', async () => {
      // Add a tie scenario via the existing data
      const rows = await db
        .select(
          employees.name,
          employees.salary,
          alias(rank({ orderBy: [desc(employees.salary)] }), 'r'),
        )
        .from(employees)
        .orderBy(desc(employees.salary))
        .execute()
      assert.equal(rows[0].r, 1) // Alice 120
      assert.equal(rows[0].name, 'Alice')
    })

    it('denseRank', async () => {
      const rows = await db
        .select(
          employees.name,
          alias(denseRank({ orderBy: [desc(employees.salary)] }), 'dr'),
        )
        .from(employees)
        .orderBy(desc(employees.salary))
        .execute()
      assert.equal(rows[0].dr, 1)
    })

    it('rowNumber with partitionBy', async () => {
      const rows = await db
        .select(
          employees.name,
          employees.dept,
          alias(rowNumber({ partitionBy: [employees.dept], orderBy: [asc(employees.salary)] }), 'rn'),
        )
        .from(employees)
        .orderBy(asc(employees.dept), asc(employees.salary))
        .execute()

      // Within eng partition: Eve(95)=1, Bob(100)=2, Alice(120)=3
      const eng = rows.filter((r) => r.dept === 'eng')
      assert.equal(eng[0].rn, 1)
      assert.equal(eng[2].rn, 3)

      // Within sales: Carol(90)=1, Dave(110)=2
      const sales = rows.filter((r) => r.dept === 'sales')
      assert.equal(sales[0].rn, 1)
      assert.equal(sales[1].rn, 2)
    })

    it('lag returns previous row value', async () => {
      const rows = await db
        .select(
          employees.name,
          employees.salary,
          alias(lag(employees.salary, 1, 0, { orderBy: [asc(employees.salary)] }), 'prev_salary'),
        )
        .from(employees)
        .orderBy(asc(employees.salary))
        .execute()
      assert.equal(rows[0].prev_salary, 0) // first row, no previous → default 0
      assert.equal(rows[1].prev_salary, rows[0].salary) // second row = first row's salary
    })

    it('lead returns next row value', async () => {
      const rows = await db
        .select(
          employees.name,
          employees.salary,
          alias(lead(employees.salary, 1, 0, { orderBy: [asc(employees.salary)] }), 'next_salary'),
        )
        .from(employees)
        .orderBy(asc(employees.salary))
        .execute()
      assert.equal(rows[rows.length - 1].next_salary, 0) // last row → default 0
      assert.equal(rows[0].next_salary, rows[1].salary)
    })

    it('ntile distributes into buckets', async () => {
      const rows = await db
        .select(
          employees.name,
          alias(ntile(2, { orderBy: [asc(employees.salary)] }), 'bucket'),
        )
        .from(employees)
        .orderBy(asc(employees.salary))
        .execute()
      // 5 rows into 2 buckets: 3 in bucket 1, 2 in bucket 2
      const b1 = rows.filter((r) => r.bucket === 1)
      const b2 = rows.filter((r) => r.bucket === 2)
      assert.equal(b1.length, 3)
      assert.equal(b2.length, 2)
    })

    it('firstValue returns first in window', async () => {
      const rows = await db
        .select(
          employees.name,
          alias(firstValue(employees.name, { partitionBy: [employees.dept], orderBy: [asc(employees.salary)] }), 'cheapest'),
        )
        .from(employees)
        .orderBy(asc(employees.dept), asc(employees.salary))
        .execute()
      const eng = rows.filter((r) => r.dept === 'eng')
      for (const r of eng) {
        assert.equal(r.cheapest, 'Eve') // cheapest in eng
      }
    })

    it('over wraps any aggregate as window function', async () => {
      const rows = await db
        .select(
          employees.name,
          employees.salary,
          alias(over(sum(employees.salary), { partitionBy: [employees.dept] }), 'dept_total'),
        )
        .from(employees)
        .orderBy(asc(employees.name))
        .execute()
      const alice = rows.find((r) => r.name === 'Alice')
      const carol = rows.find((r) => r.name === 'Carol')
      assert.equal(alice.dept_total, 120 + 100 + 95) // eng total
      assert.equal(carol.dept_total, 90 + 110) // sales total
    })

    it('AST structure is correct', () => {
      const expr = rowNumber({ partitionBy: [employees.dept], orderBy: [asc(employees.salary)] })
      assert.equal(expr.kind, 'window')
      assert.equal(expr.func.kind, 'apply')
      assert.equal(expr.func.func, 'row_number')
      assert.equal(expr.partitionBy.length, 1)
      assert.equal(expr.orderBy.length, 1)
    })
  })

  // ── CTEs ──

  describe('CTEs', () => {
    it('basic CTE', async () => {
      const cteQuery = db
        .select(employees.dept, alias(sum(employees.salary), 'total'))
        .from(employees)
        .groupBy(employees.dept)

      const deptTotals = table('dept_totals', {
        dept: text('dept'),
        total: integer('total'),
      })

      const mainQuery = db.from(deptTotals).orderBy(desc(deptTotals.total))

      const rows = await db.executeQuery(
        withCTE([{ name: 'dept_totals', query: cteQuery }], mainQuery),
      )
      assert.equal(rows.length, 2)
      assert.equal(rows[0].dept, 'eng')
      assert.equal(rows[0].total, 315)
    })

    it('CTE with column aliases', async () => {
      const cteQuery = db
        .select(employees.dept, alias(count(), 'c'))
        .from(employees)
        .groupBy(employees.dept)

      const deptCounts = table('dept_counts', {
        department: text('department'),
        headcount: integer('headcount'),
      })

      const mainQuery = db.from(deptCounts)

      const rows = await db.executeQuery(
        withCTE([{ name: 'dept_counts', columns: ['department', 'headcount'], query: cteQuery }], mainQuery),
      )
      assert.equal(rows.length, 2)
      assert.ok('department' in rows[0])
      assert.ok('headcount' in rows[0])
    })
  })

  // ── NAMED SCALAR FUNCTIONS ──

  describe('named scalar functions', () => {
    describe('string', () => {
      it('lower/upper', async () => {
        const rows = await db
          .select(alias(lower(employees.name), 'lo'), alias(upper(employees.name), 'up'))
          .from(employees)
          .where(eq(employees.name, 'Alice'))
          .execute()
        assert.equal(rows[0].lo, 'alice')
        assert.equal(rows[0].up, 'ALICE')
      })

      it('length', async () => {
        const rows = await db
          .select(employees.name, alias(length(employees.name), 'len'))
          .from(employees)
          .where(eq(employees.name, 'Alice'))
          .execute()
        assert.equal(rows[0].len, 5)
      })

      it('trim', async () => {
        const rows = await db
          .select(alias(trim(literal('  hi  ')), 'trimmed'))
          .from(employees)
          .limit(1)
          .execute()
        assert.equal(rows[0].trimmed, 'hi')
      })

      it('substring', async () => {
        const rows = await db
          .select(alias(substring(employees.name, 1, 3), 'sub'))
          .from(employees)
          .where(eq(employees.name, 'Alice'))
          .execute()
        assert.equal(rows[0].sub, 'Ali')
      })

      it('replace', async () => {
        const rows = await db
          .select(alias(replace(employees.name, 'Ali', 'Mal'), 'replaced'))
          .from(employees)
          .where(eq(employees.name, 'Alice'))
          .execute()
        assert.equal(rows[0].replaced, 'Malce')
      })

      it('concatWs', async () => {
        const rows = await db
          .select(alias(concatWs('-', employees.name, employees.dept), 'combined'))
          .from(employees)
          .where(eq(employees.name, 'Alice'))
          .execute()
        assert.equal(rows[0].combined, 'Alice-eng')
      })

      it('reverse', async () => {
        const rows = await db
          .select(alias(reverse(employees.name), 'rev'))
          .from(employees)
          .where(eq(employees.name, 'Bob'))
          .execute()
        assert.equal(rows[0].rev, 'boB')
      })

      it('repeat', async () => {
        const rows = await db
          .select(alias(repeat(literal('ab'), 3), 'r'))
          .from(employees)
          .limit(1)
          .execute()
        assert.equal(rows[0].r, 'ababab')
      })

      it('lpad/rpad', async () => {
        const rows = await db
          .select(
            alias(lpad(literal('hi'), 5, '*'), 'l'),
            alias(rpad(literal('hi'), 5, '*'), 'r'),
          )
          .from(employees)
          .limit(1)
          .execute()
        assert.equal(rows[0].l, '***hi')
        assert.equal(rows[0].r, 'hi***')
      })
    })

    describe('math', () => {
      it('abs', async () => {
        const rows = await db
          .select(alias(abs(literal(-42)), 'v'))
          .from(employees)
          .limit(1)
          .execute()
        assert.equal(rows[0].v, 42)
      })

      it('ceil/floor', async () => {
        const rows = await db
          .select(
            alias(ceil(literal(3.2)), 'c'),
            alias(floor(literal(3.8)), 'f'),
          )
          .from(employees)
          .limit(1)
          .execute()
        assert.equal(rows[0].c, 4)
        assert.equal(rows[0].f, 3)
      })

      it('round/trunc', async () => {
        const rows = await db
          .select(
            alias(round(literal(3.456), 2), 'r'),
            alias(trunc(literal(3.456), 1), 't'),
          )
          .from(employees)
          .limit(1)
          .execute()
        assert.equal(rows[0].r, 3.46)
        assert.equal(rows[0].t, 3.4)
      })

      it('sqrt/sign', async () => {
        const rows = await db
          .select(
            alias(sqrt(literal(16)), 's'),
            alias(sign(literal(-5)), 'sg'),
          )
          .from(employees)
          .limit(1)
          .execute()
        assert.equal(rows[0].s, 4)
        assert.equal(rows[0].sg, -1)
      })

      it('greatest/least', async () => {
        const rows = await db
          .select(
            alias(greatest(1, 5, 3), 'g'),
            alias(least(1, 5, 3), 'l'),
          )
          .from(employees)
          .limit(1)
          .execute()
        assert.equal(rows[0].g, 5)
        assert.equal(rows[0].l, 1)
      })
    })

    describe('null handling', () => {
      it('coalesce returns first non-null', async () => {
        const rows = await db
          .select(alias(coalesce(literal(null), literal(null), 42), 'v'))
          .from(employees)
          .limit(1)
          .execute()
        assert.equal(rows[0].v, 42)
      })

      it('nullif returns null when equal', async () => {
        const rows = await db
          .select(
            alias(nullif(literal(1), 1), 'a'),
            alias(nullif(literal(1), 2), 'b'),
          )
          .from(employees)
          .limit(1)
          .execute()
        assert.equal(rows[0].a, null)
        assert.equal(rows[0].b, 1)
      })
    })

    describe('date/time', () => {
      it('now returns a timestamp', async () => {
        const rows = await db
          .select(alias(now(), 'ts'))
          .from(employees)
          .limit(1)
          .execute()
        assert.ok(rows[0].ts != null)
      })
    })

    describe('uuid', () => {
      it('genRandomUuid returns a UUID string', async () => {
        const rows = await db
          .select(alias(genRandomUuid(), 'u'))
          .from(employees)
          .limit(1)
          .execute()
        assert.match(rows[0].u, /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/)
      })
    })
  })
})
