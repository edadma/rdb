// AST node types — plain discriminated unions that get sent to the engine's fromJS converter

// ── Expressions ──

export type ASTExpr =
  | ASTColumn
  | ASTString
  | ASTNumber
  | ASTBoolean
  | ASTNull
  | ASTStar
  | ASTTableStar
  | ASTBinary
  | ASTUnary
  | ASTAlias
  | ASTApply
  | ASTIn
  | ASTInQuery
  | ASTBetween
  | ASTCase
  | ASTSubquery
  | ASTExists
  | ASTCast
  | ASTParameter
  | ASTSelect
  | ASTTable
  | ASTAliasRelation
  | ASTJoinInner
  | ASTJoinLeft
  | ASTJoinRight
  | ASTJoinFull
  | ASTJoinCross

export interface ASTColumn {
  kind: 'column'
  table?: string
  name: string
}

export interface ASTString {
  kind: 'string'
  value: string
}

export interface ASTNumber {
  kind: 'number'
  value: number
}

export interface ASTBoolean {
  kind: 'boolean'
  value: boolean
}

export interface ASTNull {
  kind: 'null'
}

export interface ASTStar {
  kind: 'star'
}

export interface ASTTableStar {
  kind: 'tableStar'
  table: string
}

export interface ASTBinary {
  kind: 'binary'
  left: ASTExpr
  op: string
  right: ASTExpr
}

export interface ASTUnary {
  kind: 'unary'
  op: string
  expr: ASTExpr
}

export interface ASTAlias {
  kind: 'alias'
  expr: ASTExpr
  alias: string
}

export interface ASTApply {
  kind: 'apply'
  func: string
  args: ASTExpr[]
  filter?: ASTExpr
}

export interface ASTIn {
  kind: 'in'
  value: ASTExpr
  op: string // 'IN' or 'NOT IN'
  exprs: ASTExpr[]
}

export interface ASTInQuery {
  kind: 'inQuery'
  value: ASTExpr
  op: string // 'IN' or 'NOT IN'
  query: ASTExpr
}

export interface ASTBetween {
  kind: 'between'
  value: ASTExpr
  op: string // 'BETWEEN' or 'NOT BETWEEN'
  lower: ASTExpr
  upper: ASTExpr
}

export interface ASTWhen {
  when: ASTExpr
  expr: ASTExpr
}

export interface ASTCase {
  kind: 'case'
  whens: ASTWhen[]
  els?: ASTExpr
}

export interface ASTSubquery {
  kind: 'subquery'
  query: ASTExpr
}

export interface ASTExists {
  kind: 'exists'
  subquery: ASTExpr
}

export interface ASTCast {
  kind: 'cast'
  expr: ASTExpr
  targetType: string
}

export interface ASTParameter {
  kind: 'parameter'
  index: number
}

export interface ASTOrderBy {
  expr: ASTExpr
  direction: 'asc' | 'desc'
  nullsFirst?: boolean
}

export interface ASTSelect {
  kind: 'select'
  exprs: ASTExpr[]
  from?: ASTExpr[]
  where?: ASTExpr
  groupBy?: ASTExpr[]
  having?: ASTExpr
  orderBy?: ASTOrderBy[]
  offset?: number
  limit?: number
  distinct?: boolean
  distinctOn?: ASTExpr[]
}

export interface ASTTable {
  kind: 'table'
  name: string
}

export interface ASTAliasRelation {
  kind: 'aliasRelation'
  relation: ASTExpr
  alias: string
}

export interface ASTJoinInner {
  kind: 'joinInner'
  left: ASTExpr
  right: ASTExpr
  on: ASTExpr
}

export interface ASTJoinLeft {
  kind: 'joinLeft'
  left: ASTExpr
  right: ASTExpr
  on: ASTExpr
}

export interface ASTJoinRight {
  kind: 'joinRight'
  left: ASTExpr
  right: ASTExpr
  on: ASTExpr
}

export interface ASTJoinFull {
  kind: 'joinFull'
  left: ASTExpr
  right: ASTExpr
  on: ASTExpr
}

export interface ASTJoinCross {
  kind: 'joinCross'
  left: ASTExpr
  right: ASTExpr
}

// ── Commands ──

export type ASTCommand =
  | ASTQueryCommand
  | ASTInsertCommand
  | ASTUpdateCommand
  | ASTDeleteCommand
  | ASTCreateTableCommand

export interface ASTQueryCommand {
  kind: 'query'
  query: ASTExpr
}

export interface ASTUpdateSet {
  col: string
  value: ASTExpr
}

export interface ASTOnConflictDoNothing {
  kind: 'doNothing'
}

export interface ASTOnConflictDoUpdate {
  kind: 'doUpdate'
  conflictColumns: string[]
  updates: ASTUpdateSet[]
}

export type ASTOnConflict = ASTOnConflictDoNothing | ASTOnConflictDoUpdate

export interface ASTInsertCommand {
  kind: 'insert'
  table: string
  columns?: string[]
  rows?: ASTExpr[][]
  query?: ASTExpr
  returning?: ASTExpr[]
  onConflict?: ASTOnConflict
}

export interface ASTUpdateCommand {
  kind: 'update'
  table: string
  sets: ASTUpdateSet[]
  from?: ASTExpr[]
  where?: ASTExpr
  returning?: ASTExpr[]
}

export interface ASTDeleteCommand {
  kind: 'delete'
  table: string
  using?: ASTExpr[]
  where?: ASTExpr
  returning?: ASTExpr[]
}

export interface ASTColumnDef {
  name: string
  type: string
  notNull: boolean
  unique: boolean
  primaryKey: boolean
  default?: ASTExpr
  references?: { table: string; column: string }
}

export interface ASTCreateTableCommand {
  kind: 'createTable'
  table: string
  columns: ASTColumnDef[]
  ifNotExists?: boolean
}
