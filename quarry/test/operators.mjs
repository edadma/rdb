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
  ne,
  gt,
  gte,
  lt,
  lte,
  and,
  or,
  not,
  isNull,
  isNotNull,
  // New operators
  op,
  unaryOp,
  isDistinctFrom,
  isNotDistinctFrom,
  notLike,
  notIlike,
  ilike,
  isTrue,
  isNotTrue,
  isFalse,
  isNotFalse,
  isUnknown,
  isNotUnknown,
  notBetween,
  betweenSymmetric,
  notBetweenSymmetric,
  mod,
  pow,
  neg,
  concat,
  bitAnd,
  bitOr,
  bitXor,
  bitNot,
  leftShift,
  rightShift,
  jsonGet,
  jsonGetText,
  jsonContains,
  jsonHasKey,
  arrayOverlap,
  // Special expressions
  caseWhen,
  cast,
  exists,
  // Aggregates
  stringAgg,
  arrayAgg,
  boolAnd,
  boolOr,
  jsonAgg,
  jsonObjectAgg,
  // Existing
  add,
  sub,
  mul,
  div,
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
  inList,
  between,
  like,
} from '../dist/index.js'

// ── Schema ──

const items = table('items', {
  id: serial('id').primaryKey(),
  name: text('name').notNull(),
  price: integer('price'),
  active: boolean('active').notNull().default(true),
  category: text('category'),
})

describe('new operators and expressions', () => {
  let session
  let db

  before(async () => {
    session = new Session()
    db = quarry(session)
    await db.createTable(items)

    // Seed test data
    await db.insert(items).values(
      { name: 'Widget', price: 100, active: true, category: 'tools' },
      { name: 'Gadget', price: 250, active: true, category: 'electronics' },
      { name: 'Gizmo', price: 50, active: false, category: 'tools' },
      { name: 'Doohickey', price: null, active: true, category: null },
    ).execute()
  })

  after(async () => {
    await session.close()
  })

  // ── Generic op() and unaryOp() ──

  describe('generic op() and unaryOp()', () => {
    it('op() creates a binary expression with any operator', () => {
      const expr = op(items.price, '>', 100)
      assert.equal(expr.kind, 'binary')
      assert.equal(expr.op, '>')
    })

    it('op() executes with a custom operator', async () => {
      const rows = await db.from(items).where(op(items.price, '>=', 100)).execute()
      assert.ok(rows.length >= 2) // Widget(100), Gadget(250)
    })

    it('unaryOp() creates a unary expression', () => {
      const expr = unaryOp('NOT', eq(items.active, true))
      assert.equal(expr.kind, 'unary')
      assert.equal(expr.op, 'NOT')
    })
  })

  // ── Pattern matching ──

  describe('pattern matching', () => {
    it('notLike excludes matching patterns', async () => {
      const rows = await db.from(items).where(notLike(items.name, 'G%')).execute()
      for (const row of rows) {
        assert.ok(!row.name.startsWith('G'))
      }
    })

    it('ilike matches case-insensitively', async () => {
      const rows = await db.from(items).where(ilike(items.name, 'widget')).execute()
      assert.equal(rows.length, 1)
      assert.equal(rows[0].name, 'Widget')
    })

    it('notIlike excludes case-insensitive matches', async () => {
      const rows = await db.from(items).where(notIlike(items.name, 'widget')).execute()
      for (const row of rows) {
        assert.notEqual(row.name.toLowerCase(), 'widget')
      }
    })
  })

  // ── IS DISTINCT FROM ──

  describe('IS DISTINCT FROM', () => {
    it('isDistinctFrom treats null as a regular value', async () => {
      // null IS DISTINCT FROM null => false, null IS DISTINCT FROM 100 => true
      const rows = await db.from(items).where(isDistinctFrom(items.price, null)).execute()
      for (const row of rows) {
        assert.notEqual(row.price, null)
      }
    })

    it('isNotDistinctFrom matches null with null', async () => {
      const rows = await db.from(items).where(isNotDistinctFrom(items.price, null)).execute()
      assert.equal(rows.length, 1)
      assert.equal(rows[0].name, 'Doohickey')
    })
  })

  // ── Boolean tests ──

  describe('boolean tests', () => {
    it('isTrue filters true values', async () => {
      const rows = await db.from(items).where(isTrue(items.active)).execute()
      for (const row of rows) {
        assert.equal(row.active, true)
      }
    })

    it('isFalse filters false values', async () => {
      const rows = await db.from(items).where(isFalse(items.active)).execute()
      for (const row of rows) {
        assert.equal(row.active, false)
      }
    })

    it('isNotTrue includes false and null', async () => {
      const rows = await db.from(items).where(isNotTrue(items.active)).execute()
      for (const row of rows) {
        assert.notEqual(row.active, true)
      }
    })

    it('isNotFalse includes true and null', async () => {
      const rows = await db.from(items).where(isNotFalse(items.active)).execute()
      for (const row of rows) {
        assert.notEqual(row.active, false)
      }
    })
  })

  // ── BETWEEN variants ──

  describe('BETWEEN variants', () => {
    it('notBetween excludes range', async () => {
      const rows = await db.from(items).where(notBetween(items.price, 50, 100)).execute()
      for (const row of rows) {
        assert.ok(row.price < 50 || row.price > 100)
      }
    })

    it('betweenSymmetric works regardless of order', async () => {
      // betweenSymmetric(price, 200, 50) should match 50-200 range
      const rows = await db.from(items).where(betweenSymmetric(items.price, 200, 50)).execute()
      for (const row of rows) {
        assert.ok(row.price >= 50 && row.price <= 200)
      }
    })

    it('notBetweenSymmetric excludes range regardless of order', async () => {
      const rows = await db.from(items).where(notBetweenSymmetric(items.price, 200, 50)).execute()
      for (const row of rows) {
        assert.ok(row.price < 50 || row.price > 200)
      }
    })
  })

  // ── Arithmetic ──

  describe('arithmetic operators', () => {
    it('mod calculates remainder', async () => {
      const rows = await db
        .select(items.name, alias(mod(items.price, 100), 'remainder'))
        .from(items)
        .where(eq(items.name, 'Gadget'))
        .execute()
      assert.equal(rows[0].remainder, 50) // 250 % 100 = 50
    })

    it('pow calculates exponentiation', async () => {
      const rows = await db
        .select(items.name, alias(pow(items.price, 2), 'squared'))
        .from(items)
        .where(eq(items.name, 'Gizmo'))
        .execute()
      assert.equal(rows[0].squared, 2500) // 50^2
    })

    it('neg negates a value', async () => {
      const rows = await db
        .select(items.name, alias(neg(items.price), 'neg_price'))
        .from(items)
        .where(eq(items.name, 'Widget'))
        .execute()
      assert.equal(rows[0].neg_price, -100)
    })
  })

  // ── String concat ──

  describe('string concat', () => {
    it('concat joins strings with ||', async () => {
      const rows = await db
        .select(alias(concat(items.name, ' item'), 'label'))
        .from(items)
        .where(eq(items.name, 'Widget'))
        .execute()
      assert.equal(rows[0].label, 'Widget item')
    })
  })

  // ── Bitwise operators ──

  describe('bitwise operators', () => {
    it('bitAnd computes bitwise AND', async () => {
      const rows = await db
        .select(alias(bitAnd(items.price, 0xFF), 'masked'))
        .from(items)
        .where(eq(items.name, 'Gadget'))
        .execute()
      assert.equal(rows[0].masked, 250 & 0xFF)
    })

    it('bitOr computes bitwise OR', async () => {
      const rows = await db
        .select(alias(bitOr(items.price, 1), 'result'))
        .from(items)
        .where(eq(items.name, 'Widget'))
        .execute()
      assert.equal(rows[0].result, 100 | 1) // 101
    })

    it('bitXor computes bitwise XOR', async () => {
      const rows = await db
        .select(alias(bitXor(items.price, 0xFF), 'result'))
        .from(items)
        .where(eq(items.name, 'Gizmo'))
        .execute()
      assert.equal(rows[0].result, 50 ^ 0xFF)
    })

    it('bitNot computes bitwise NOT', async () => {
      const rows = await db
        .select(alias(bitNot(items.price), 'result'))
        .from(items)
        .where(eq(items.name, 'Gizmo'))
        .execute()
      assert.equal(rows[0].result, ~50)
    })

    it('leftShift shifts bits left', async () => {
      const rows = await db
        .select(alias(leftShift(items.price, 2), 'result'))
        .from(items)
        .where(eq(items.name, 'Gizmo'))
        .execute()
      assert.equal(rows[0].result, 50 << 2) // 200
    })

    it('rightShift shifts bits right', async () => {
      const rows = await db
        .select(alias(rightShift(items.price, 1), 'result'))
        .from(items)
        .where(eq(items.name, 'Widget'))
        .execute()
      assert.equal(rows[0].result, 100 >> 1) // 50
    })
  })

  // ── JSON operators ──

  describe('JSON operators', () => {
    before(async () => {
      await session.execute(`
        CREATE TABLE json_test (
          id SERIAL PRIMARY KEY,
          data JSON NOT NULL
        )
      `)
      await session.execute(`INSERT INTO json_test (data) VALUES ('{"name": "Alice", "age": 30, "tags": ["a", "b"]}')`)
    })

    it('jsonGet retrieves a JSON field', async () => {
      const [{ rows }] = await session.executeAST({
        kind: 'query',
        query: {
          kind: 'select',
          exprs: [{ kind: 'alias', expr: { kind: 'binary', left: { kind: 'column', table: 'json_test', name: 'data' }, op: '->', right: { kind: 'string', value: 'name' } }, alias: 'val' }],
          from: [{ kind: 'table', name: 'json_test' }],
        },
      })
      assert.equal(rows[0].val, 'Alice')
    })

    it('jsonGetText retrieves as text', async () => {
      const [{ rows }] = await session.executeAST({
        kind: 'query',
        query: {
          kind: 'select',
          exprs: [{ kind: 'alias', expr: jsonGetText({ kind: 'column', table: 'json_test', name: 'data' }, 'name'), alias: 'val' }],
          from: [{ kind: 'table', name: 'json_test' }],
        },
      })
      assert.equal(rows[0].val, 'Alice')
    })
  })

  // ── CASE expression ──

  describe('CASE expression', () => {
    it('caseWhen produces correct AST', () => {
      const expr = caseWhen(
        [
          { when: gt(items.price, 200), then: literal('expensive') },
          { when: gt(items.price, 75), then: literal('moderate') },
        ],
        'cheap',
      )
      assert.equal(expr.kind, 'case')
      assert.equal(expr.whens.length, 2)
      assert.equal(expr.whens[0].expr.kind, 'string')
      assert.equal(expr.whens[0].expr.value, 'expensive')
      assert.equal(expr.els.kind, 'string')
      assert.equal(expr.els.value, 'cheap')
    })

    it('caseWhen executes correctly', async () => {
      const rows = await db
        .select(
          items.name,
          alias(
            caseWhen(
              [
                { when: gt(items.price, 200), then: literal('expensive') },
                { when: gt(items.price, 75), then: literal('moderate') },
              ],
              'cheap',
            ),
            'tier',
          ),
        )
        .from(items)
        .where(isNotNull(items.price))
        .orderBy(desc(items.price))
        .execute()

      assert.equal(rows[0].name, 'Gadget')
      assert.equal(rows[0].tier, 'expensive')
      assert.equal(rows[1].name, 'Widget')
      assert.equal(rows[1].tier, 'moderate')
      assert.equal(rows[2].name, 'Gizmo')
      assert.equal(rows[2].tier, 'cheap')
    })

    it('caseWhen without else', () => {
      const expr = caseWhen([{ when: gt(items.price, 100), then: literal('high') }])
      assert.equal(expr.kind, 'case')
      assert.equal(expr.els, undefined)
    })
  })

  // ── CAST expression ──

  describe('CAST expression', () => {
    it('cast produces correct AST', () => {
      const expr = cast(items.price, 'text')
      assert.equal(expr.kind, 'cast')
      assert.equal(expr.targetType, 'text')
    })

    it('cast executes correctly', async () => {
      const rows = await db
        .select(items.name, alias(cast(items.price, 'text'), 'price_text'))
        .from(items)
        .where(eq(items.name, 'Widget'))
        .execute()
      assert.equal(typeof rows[0].price_text, 'string')
      assert.equal(rows[0].price_text, '100')
    })
  })

  // ── EXISTS expression ──

  describe('EXISTS expression', () => {
    it('exists accepts a builder and produces correct AST', () => {
      const subq = db.from(items).where(eq(items.name, 'Widget'))
      const expr = exists(subq)
      assert.equal(expr.kind, 'exists')
      assert.equal(expr.subquery.kind, 'select')
    })
  })

  // ── Additional aggregate functions ──

  describe('aggregate functions', () => {
    it('stringAgg concatenates values', async () => {
      const rows = await db
        .select(alias(stringAgg(items.name, ', '), 'names'))
        .from(items)
        .where(isNotNull(items.price))
        .execute()
      const names = rows[0].names.split(', ').sort()
      assert.deepStrictEqual(names, ['Gadget', 'Gizmo', 'Widget'])
    })

    it('arrayAgg collects values into array', async () => {
      const rows = await db
        .select(alias(arrayAgg(items.price), 'prices'))
        .from(items)
        .where(isNotNull(items.price))
        .execute()
      const prices = rows[0].prices.sort((a, b) => a - b)
      assert.deepStrictEqual(prices, [50, 100, 250])
    })

    it('boolAnd returns AND of all values', async () => {
      const rows = await db.select(alias(boolAnd(items.active), 'all_active')).from(items).execute()
      assert.equal(rows[0].all_active, false) // one is false
    })

    it('boolOr returns OR of all values', async () => {
      const rows = await db.select(alias(boolOr(items.active), 'any_active')).from(items).execute()
      assert.equal(rows[0].any_active, true)
    })

    it('jsonAgg collects values into JSON array', async () => {
      const rows = await db
        .select(alias(jsonAgg(items.name), 'names'))
        .from(items)
        .where(inList(items.name, ['Widget', 'Gizmo']))
        .execute()
      const names = rows[0].names.sort()
      assert.deepStrictEqual(names, ['Gizmo', 'Widget'])
    })

    it('jsonObjectAgg builds key-value object', async () => {
      const rows = await db
        .select(alias(jsonObjectAgg(items.name, items.price), 'price_map'))
        .from(items)
        .where(isNotNull(items.price))
        .execute()
      const map = rows[0].price_map
      assert.equal(map.Widget, 100)
      assert.equal(map.Gadget, 250)
      assert.equal(map.Gizmo, 50)
    })
  })

  // ── AST structure tests for new operators ──

  describe('AST structure', () => {
    it('isDistinctFrom creates correct binary node', () => {
      const expr = isDistinctFrom(items.price, null)
      assert.equal(expr.kind, 'binary')
      assert.equal(expr.op, 'IS DISTINCT FROM')
      assert.equal(expr.right.kind, 'null')
    })

    it('notLike creates correct binary node', () => {
      const expr = notLike(items.name, 'A%')
      assert.equal(expr.kind, 'binary')
      assert.equal(expr.op, 'NOT LIKE')
    })

    it('mod creates % operator', () => {
      const expr = mod(items.price, 10)
      assert.equal(expr.op, '%')
    })

    it('pow creates ^ operator', () => {
      const expr = pow(items.price, 2)
      assert.equal(expr.op, '^')
    })

    it('neg creates unary -', () => {
      const expr = neg(items.price)
      assert.equal(expr.kind, 'unary')
      assert.equal(expr.op, '-')
    })

    it('concat creates || operator', () => {
      const expr = concat(items.name, ' suffix')
      assert.equal(expr.op, '||')
    })

    it('bitwise operators create correct nodes', () => {
      assert.equal(bitAnd(items.price, 0xFF).op, '&')
      assert.equal(bitOr(items.price, 1).op, '|')
      assert.equal(bitXor(items.price, 1).op, '#')
      assert.equal(leftShift(items.price, 2).op, '<<')
      assert.equal(rightShift(items.price, 2).op, '>>')
      assert.equal(bitNot(items.price).op, '~')
    })

    it('JSON operators create correct nodes', () => {
      const data = items.name // placeholder
      assert.equal(jsonGet(data, 'key').op, '->')
      assert.equal(jsonGetText(data, 'key').op, '->>')
      assert.equal(jsonContains(data, data).op, '@>')
      assert.equal(jsonHasKey(data, 'key').op, '?')
    })

    it('betweenSymmetric creates correct AST', () => {
      const expr = betweenSymmetric(items.price, 200, 50)
      assert.equal(expr.kind, 'between')
      assert.equal(expr.op, 'BETWEEN SYMMETRIC')
    })

    it('notBetween creates correct AST', () => {
      const expr = notBetween(items.price, 50, 100)
      assert.equal(expr.kind, 'between')
      assert.equal(expr.op, 'NOT BETWEEN')
    })

    it('boolean test operators create correct nodes', () => {
      const expr = items.active
      assert.equal(isTrue(expr).op, 'IS TRUE')
      assert.equal(isNotTrue(expr).op, 'IS NOT TRUE')
      assert.equal(isFalse(expr).op, 'IS FALSE')
      assert.equal(isNotFalse(expr).op, 'IS NOT FALSE')
      assert.equal(isUnknown(expr).op, 'IS UNKNOWN')
      assert.equal(isNotUnknown(expr).op, 'IS NOT UNKNOWN')
    })

    it('cast creates correct AST', () => {
      const expr = cast(items.price, 'text')
      assert.equal(expr.kind, 'cast')
      assert.equal(expr.targetType, 'text')
    })

    it('caseWhen creates correct AST', () => {
      const expr = caseWhen(
        [{ when: gt(items.price, 100), then: literal('high') }],
        'low',
      )
      assert.equal(expr.kind, 'case')
      assert.equal(expr.whens.length, 1)
      assert.equal(expr.els.kind, 'string')
    })

    it('exists creates correct AST', () => {
      const expr = exists(db.from(items))
      assert.equal(expr.kind, 'exists')
    })
  })
})
