import type {
  ASTExpr,
  ASTCommand,
  ASTQueryCommand,
  ASTInsertCommand,
  ASTUpdateCommand,
  ASTDeleteCommand,
  ASTOrderBy,
  ASTUpdateSet,
  ASTOnConflict,
} from './ast.js'
import type { TableDef, ColumnsConfig, InferSelect, InferInsert, Nullable } from './schema.js'

// ── Helpers ──

function tableToExpr(table: TableDef<any, any>): ASTExpr {
  if (table._originalName) {
    return { kind: 'aliasRelation', relation: { kind: 'table', name: table._originalName }, alias: table._name }
  }
  return { kind: 'table', name: table._name }
}

// ── Session interface ──

export interface QuarrySession {
  executeAST(ast: any, options?: { rowMode?: string }): Promise<any[]>
  execute(sql: string, options?: { rowMode?: string }): Promise<any[]>
}

// ── Select builder ──

interface SelectState {
  tableExpr: ASTExpr
  columns: ASTExpr[]
  where?: ASTExpr
  orderBy?: ASTOrderBy[]
  limit?: number
  offset?: number
  groupBy?: ASTExpr[]
  having?: ASTExpr
  distinct: boolean
  joins: { kind: 'joinInner' | 'joinLeft'; right: ASTExpr; on: ASTExpr }[]
}

export class SelectBuilder<TResult> {
  private _session: QuarrySession
  private _state: SelectState

  /** @internal */
  constructor(session: QuarrySession, state: SelectState) {
    this._session = session
    this._state = state
  }

  columns(...cols: ASTExpr[]): SelectBuilder<TResult> {
    return new SelectBuilder(this._session, { ...this._state, columns: cols })
  }

  where(condition: ASTExpr): SelectBuilder<TResult> {
    return new SelectBuilder(this._session, { ...this._state, where: condition })
  }

  orderBy(...orders: ASTOrderBy[]): SelectBuilder<TResult> {
    return new SelectBuilder(this._session, { ...this._state, orderBy: orders })
  }

  limit(n: number): SelectBuilder<TResult> {
    return new SelectBuilder(this._session, { ...this._state, limit: n })
  }

  offset(n: number): SelectBuilder<TResult> {
    return new SelectBuilder(this._session, { ...this._state, offset: n })
  }

  groupBy(...exprs: ASTExpr[]): SelectBuilder<TResult> {
    return new SelectBuilder(this._session, { ...this._state, groupBy: exprs })
  }

  having(condition: ASTExpr): SelectBuilder<TResult> {
    return new SelectBuilder(this._session, { ...this._state, having: condition })
  }

  distinct(): SelectBuilder<TResult> {
    return new SelectBuilder(this._session, { ...this._state, distinct: true })
  }

  innerJoin<U extends TableDef<any, any>>(
    table: U,
    on: ASTExpr,
  ): SelectBuilder<TResult & InferSelect<U>> {
    return new SelectBuilder(this._session, {
      ...this._state,
      joins: [
        ...this._state.joins,
        { kind: 'joinInner', right: tableToExpr(table), on },
      ],
    })
  }

  leftJoin<U extends TableDef<any, any>>(
    table: U,
    on: ASTExpr,
  ): SelectBuilder<TResult & Nullable<InferSelect<U>>> {
    return new SelectBuilder(this._session, {
      ...this._state,
      joins: [
        ...this._state.joins,
        { kind: 'joinLeft', right: tableToExpr(table), on },
      ],
    })
  }

  toExpr(): ASTExpr {
    let from: ASTExpr = this._state.tableExpr

    for (const join of this._state.joins) {
      from = { kind: join.kind, left: from, right: join.right, on: join.on }
    }

    return {
      kind: 'select',
      exprs: this._state.columns,
      from: [from],
      where: this._state.where,
      orderBy: this._state.orderBy,
      offset: this._state.offset,
      limit: this._state.limit,
      groupBy: this._state.groupBy,
      having: this._state.having,
      distinct: this._state.distinct || undefined,
    }
  }

  toAST(): ASTQueryCommand {
    return { kind: 'query', query: this.toExpr() }
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
  private _onConflict?: ASTOnConflict

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

  onConflictDoNothing(): this {
    this._onConflict = { kind: 'doNothing' }
    return this
  }

  onConflictDoUpdate(
    conflictColumns: (keyof T['_columns'] & string)[],
    updates: Partial<InferSelect<T>>,
  ): this {
    const columns = this._table._columns as ColumnsConfig
    const dbConflictCols = conflictColumns.map((key) => {
      const colDef = columns[key]
      if (!colDef) throw new Error(`Unknown column '${key}' in table '${this._table._name}'`)
      return colDef._columnName
    })

    const sets: ASTUpdateSet[] = []
    for (const [key, value] of Object.entries(updates as Record<string, unknown>)) {
      const colDef = columns[key]
      if (!colDef) throw new Error(`Unknown column '${key}' in table '${this._table._name}'`)
      let astValue: ASTExpr
      if (value === null || value === undefined) astValue = { kind: 'null' }
      else if (typeof value === 'string') astValue = { kind: 'string', value }
      else if (typeof value === 'number') astValue = { kind: 'number', value }
      else if (typeof value === 'boolean') astValue = { kind: 'boolean', value }
      else astValue = { kind: 'string', value: String(value) }
      sets.push({ col: colDef._columnName, value: astValue })
    }

    this._onConflict = { kind: 'doUpdate', conflictColumns: dbConflictCols, updates: sets }
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
      onConflict: this._onConflict,
    }
  }

  async execute(): Promise<InferSelect<T>[]> {
    const ast = this.toAST()
    const results = await this._session.executeAST(ast)
    const result = results[0] as any
    return result.rows as InferSelect<T>[]
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

  async execute(): Promise<{ rowCount: number; rows: InferSelect<T>[] }> {
    const ast = this.toAST()
    const results = await this._session.executeAST(ast)
    const result = results[0] as any
    // With RETURNING, engine returns a select-style result
    if (result.command === 'select') {
      return { rowCount: result.rows?.length ?? 0, rows: result.rows ?? [] }
    }
    return { rowCount: result.rowCount ?? 0, rows: result.rows ?? [] }
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

  async execute(): Promise<{ rowCount: number; rows: InferSelect<T>[] }> {
    const ast = this.toAST()
    const results = await this._session.executeAST(ast)
    const result = results[0] as any
    // With RETURNING, engine returns a select-style result
    if (result.command === 'select') {
      return { rowCount: result.rows?.length ?? 0, rows: result.rows ?? [] }
    }
    return { rowCount: result.rowCount ?? 0, rows: result.rows ?? [] }
  }
}

// ── Quarry DB entry point ──

export class QuarryDB {
  private _session: QuarrySession

  constructor(session: QuarrySession) {
    this._session = session
  }

  select<T extends TableDef<any, any>>(table: T): SelectBuilder<InferSelect<T>> {
    return new SelectBuilder<InferSelect<T>>(this._session, {
      tableExpr: tableToExpr(table),
      columns: [{ kind: 'star' }],
      distinct: false,
      joins: [],
    })
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

  async transaction<R>(fn: (tx: QuarryDB) => Promise<R>): Promise<R> {
    await this._session.execute('BEGIN')
    try {
      const result = await fn(this)
      await this._session.execute('COMMIT')
      return result
    } catch (e) {
      await this._session.execute('ROLLBACK')
      throw e
    }
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
