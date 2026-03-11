import type {
  ASTExpr,
  ASTCommand,
  ASTQueryCommand,
  ASTInsertCommand,
  ASTUpdateCommand,
  ASTDeleteCommand,
  ASTOrderBy,
  ASTUpdateSet,
} from './ast.js'
import type { TableDef, ColumnsConfig, InferSelect, InferInsert } from './schema.js'

// ── Session interface ──

export interface QuarrySession {
  executeAST(ast: any, options?: { rowMode?: string }): Promise<any[]>
}

// ── Select builder ──

export class SelectBuilder<T extends TableDef<any, any>, TResult = InferSelect<T>> {
  private _table: T
  private _session: QuarrySession
  private _columns: ASTExpr[] = [{ kind: 'star' }]
  private _where?: ASTExpr
  private _orderBy?: ASTOrderBy[]
  private _limit?: number
  private _offset?: number
  private _groupBy?: ASTExpr[]
  private _having?: ASTExpr
  private _distinct = false
  private _joins: ASTExpr[] = []

  constructor(session: QuarrySession, table: T) {
    this._session = session
    this._table = table
  }

  columns(...cols: ASTExpr[]): this {
    this._columns = cols
    return this
  }

  where(condition: ASTExpr): this {
    this._where = condition
    return this
  }

  orderBy(...orders: ASTOrderBy[]): this {
    this._orderBy = orders
    return this
  }

  limit(n: number): this {
    this._limit = n
    return this
  }

  offset(n: number): this {
    this._offset = n
    return this
  }

  groupBy(...exprs: ASTExpr[]): this {
    this._groupBy = exprs
    return this
  }

  having(condition: ASTExpr): this {
    this._having = condition
    return this
  }

  distinct(): this {
    this._distinct = true
    return this
  }

  innerJoin<U extends TableDef<any, any>>(table: U, on: ASTExpr): this {
    this._joins.push({
      kind: 'joinInner',
      left: { kind: 'table', name: '' }, // placeholder, resolved in toAST
      right: { kind: 'table', name: table._name },
      on,
    })
    return this
  }

  leftJoin<U extends TableDef<any, any>>(table: U, on: ASTExpr): this {
    this._joins.push({
      kind: 'joinLeft',
      left: { kind: 'table', name: '' },
      right: { kind: 'table', name: table._name },
      on,
    })
    return this
  }

  toAST(): ASTQueryCommand {
    let from: ASTExpr = { kind: 'table', name: this._table._name }

    for (const join of this._joins) {
      if (join.kind === 'joinInner') {
        from = { kind: 'joinInner', left: from, right: join.right, on: join.on }
      } else if (join.kind === 'joinLeft') {
        from = { kind: 'joinLeft', left: from, right: join.right, on: join.on }
      }
    }

    const select: ASTExpr = {
      kind: 'select',
      exprs: this._columns,
      from: [from],
      where: this._where,
      orderBy: this._orderBy,
      offset: this._offset,
      limit: this._limit,
      groupBy: this._groupBy,
      having: this._having,
      distinct: this._distinct || undefined,
    }

    return { kind: 'query', query: select }
  }

  async execute(): Promise<TResult[]> {
    const ast = this.toAST()
    const results = await this._session.executeAST(ast)
    const result = results[0] as any
    return result.rows as TResult[]
  }
}

// ── Insert builder ──

export class InsertBuilder<T extends TableDef<any, any>> {
  private _table: T
  private _session: QuarrySession
  private _rows: Record<string, unknown>[] = []
  private _returning?: ASTExpr[]

  constructor(session: QuarrySession, table: T) {
    this._session = session
    this._table = table
  }

  values(...rows: InferInsert<T>[]): this {
    this._rows.push(...(rows as Record<string, unknown>[]))
    return this
  }

  returning(...exprs: ASTExpr[]): this {
    this._returning = exprs
    return this
  }

  toAST(): ASTInsertCommand {
    if (this._rows.length === 0) throw new Error('insert requires at least one row')

    const columns = this._table._columns as ColumnsConfig
    const allKeys = new Set<string>()
    for (const row of this._rows) {
      for (const key of Object.keys(row)) allKeys.add(key)
    }

    const colNames: string[] = []
    const dbColNames: string[] = []
    for (const key of allKeys) {
      const colDef = columns[key]
      if (!colDef) throw new Error(`Unknown column '${key}' in table '${this._table._name}'`)
      colNames.push(key)
      dbColNames.push(colDef._columnName)
    }

    const astRows: ASTExpr[][] = this._rows.map((row) =>
      colNames.map((key) => {
        const value = row[key]
        if (value === undefined || value === null) return { kind: 'null' as const }
        if (typeof value === 'string') return { kind: 'string' as const, value }
        if (typeof value === 'number') return { kind: 'number' as const, value }
        if (typeof value === 'boolean') return { kind: 'boolean' as const, value }
        return { kind: 'string' as const, value: String(value) }
      }),
    )

    return {
      kind: 'insert',
      table: this._table._name,
      columns: dbColNames,
      rows: astRows,
      returning: this._returning ?? [{ kind: 'star' }],
    }
  }

  async execute(): Promise<InferSelect<T>> {
    const ast = this.toAST()
    const results = await this._session.executeAST(ast)
    const result = results[0] as any
    return result.rows[0] as InferSelect<T>
  }
}

// ── Update builder ──

export class UpdateBuilder<T extends TableDef<any, any>> {
  private _table: T
  private _session: QuarrySession
  private _sets: ASTUpdateSet[] = []
  private _where?: ASTExpr
  private _returning?: ASTExpr[]

  constructor(session: QuarrySession, table: T) {
    this._session = session
    this._table = table
  }

  set(values: Partial<InferSelect<T>>): this {
    const columns = this._table._columns as ColumnsConfig
    for (const [key, value] of Object.entries(values as Record<string, unknown>)) {
      const colDef = columns[key]
      if (!colDef) throw new Error(`Unknown column '${key}' in table '${this._table._name}'`)
      let astValue: ASTExpr
      if (value === null || value === undefined) astValue = { kind: 'null' }
      else if (typeof value === 'string') astValue = { kind: 'string', value }
      else if (typeof value === 'number') astValue = { kind: 'number', value }
      else if (typeof value === 'boolean') astValue = { kind: 'boolean', value }
      else astValue = { kind: 'string', value: String(value) }
      this._sets.push({ col: colDef._columnName, value: astValue })
    }
    return this
  }

  where(condition: ASTExpr): this {
    this._where = condition
    return this
  }

  returning(...exprs: ASTExpr[]): this {
    this._returning = exprs
    return this
  }

  toAST(): ASTUpdateCommand {
    if (this._sets.length === 0) throw new Error('update requires at least one set clause')
    return {
      kind: 'update',
      table: this._table._name,
      sets: this._sets,
      where: this._where,
      returning: this._returning,
    }
  }

  async execute(): Promise<{ rowCount: number }> {
    const ast = this.toAST()
    const results = await this._session.executeAST(ast)
    const result = results[0] as any
    return { rowCount: result.rowCount ?? 0 }
  }
}

// ── Delete builder ──

export class DeleteBuilder<T extends TableDef<any, any>> {
  private _table: T
  private _session: QuarrySession
  private _where?: ASTExpr
  private _returning?: ASTExpr[]

  constructor(session: QuarrySession, table: T) {
    this._session = session
    this._table = table
  }

  where(condition: ASTExpr): this {
    this._where = condition
    return this
  }

  returning(...exprs: ASTExpr[]): this {
    this._returning = exprs
    return this
  }

  toAST(): ASTDeleteCommand {
    return {
      kind: 'delete',
      table: this._table._name,
      where: this._where,
      returning: this._returning,
    }
  }

  async execute(): Promise<{ rowCount: number }> {
    const ast = this.toAST()
    const results = await this._session.executeAST(ast)
    const result = results[0] as any
    return { rowCount: result.rowCount ?? 0 }
  }
}

// ── Quarry DB entry point ──

export class QuarryDB {
  private _session: QuarrySession

  constructor(session: QuarrySession) {
    this._session = session
  }

  select<T extends TableDef<any, any>>(table: T): SelectBuilder<T> {
    return new SelectBuilder(this._session, table)
  }

  insert<T extends TableDef<any, any>>(table: T): InsertBuilder<T> {
    return new InsertBuilder(this._session, table)
  }

  update<T extends TableDef<any, any>>(table: T): UpdateBuilder<T> {
    return new UpdateBuilder(this._session, table)
  }

  delete<T extends TableDef<any, any>>(table: T): DeleteBuilder<T> {
    return new DeleteBuilder(this._session, table)
  }

  async createTable<T extends TableDef<any, any>>(table: T): Promise<void> {
    await this._session.executeAST(table.toCreateAST())
  }
}

export function quarry(session: QuarrySession): QuarryDB {
  return new QuarryDB(session)
}

// ── OrderBy helpers ──

export function asc(expr: ASTExpr): ASTOrderBy {
  return { expr, direction: 'asc' }
}

export function desc(expr: ASTExpr): ASTOrderBy {
  return { expr, direction: 'desc' }
}
