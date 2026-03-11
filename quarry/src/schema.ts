import type { ASTExpr, ASTColumnDef, ASTCreateTableCommand } from './ast.js'

// ── Column type mapping ──

type ColumnTypeMap = {
  serial: number
  text: string
  integer: number
  boolean: boolean
}

type ColumnTypeName = keyof ColumnTypeMap

// ── Column definition ──

export interface ColumnDef<
  TName extends string,
  TType,
  TNotNull extends boolean,
  THasDefault extends boolean,
> {
  readonly _name: TName
  readonly _type: TType
  readonly _notNull: TNotNull
  readonly _hasDefault: THasDefault
  readonly _columnName: string
  readonly _typeName: string
  readonly _unique: boolean
  readonly _primaryKey: boolean
  readonly _default: unknown | undefined
  readonly _references: { table: string; column: string } | undefined
  notNull(): ColumnDef<TName, TType, true, THasDefault>
  unique(): ColumnDef<TName, TType, TNotNull, THasDefault>
  primaryKey(): ColumnDef<TName, TType, TNotNull, true>
  default(value: TType): ColumnDef<TName, TType, TNotNull, true>
  references(table: string, column: string): ColumnDef<TName, TType, TNotNull, THasDefault>
}

function createColumn<TName extends string, TType, THasDefault extends boolean>(
  columnName: TName,
  typeName: string,
  hasDefault: THasDefault,
): ColumnDef<TName, TType, false, THasDefault> {
  const col = {
    _name: columnName as TName,
    _type: undefined as unknown as TType,
    _notNull: false as const,
    _hasDefault: hasDefault,
    _columnName: columnName,
    _typeName: typeName,
    _unique: false,
    _primaryKey: false,
    _default: undefined as unknown | undefined,
    _references: undefined as { table: string; column: string } | undefined,
    notNull() {
      return { ...this, _notNull: true as const } as any
    },
    unique() {
      return { ...this, _unique: true } as any
    },
    primaryKey() {
      return { ...this, _primaryKey: true, _hasDefault: true as const } as any
    },
    default(value: TType) {
      return { ...this, _default: value, _hasDefault: true as const } as any
    },
    references(table: string, column: string) {
      return { ...this, _references: { table, column } } as any
    },
  }
  return col as any
}

// ── Column constructors ──

export function serial<TName extends string>(name: TName): ColumnDef<TName, number, true, true> {
  return createColumn<TName, number, true>(name, 'serial', true).notNull() as any
}

export function text<TName extends string>(name: TName): ColumnDef<TName, string, false, false> {
  return createColumn<TName, string, false>(name, 'text', false)
}

export function integer<TName extends string>(name: TName): ColumnDef<TName, number, false, false> {
  return createColumn<TName, number, false>(name, 'integer', false)
}

export function boolean<TName extends string>(name: TName): ColumnDef<TName, boolean, false, false> {
  return createColumn<TName, boolean, false>(name, 'boolean', false)
}

// ── Table definition ──

export type ColumnsConfig = Record<string, ColumnDef<any, any, any, any>>

export interface TableDef<TName extends string, TColumns extends ColumnsConfig> {
  readonly _name: TName
  readonly _columns: TColumns
  toCreateAST(): ASTCreateTableCommand
}

export type InferSelect<T extends TableDef<any, any>> = {
  [K in keyof T['_columns']]: T['_columns'][K] extends ColumnDef<any, infer TType, infer TNotNull, any>
    ? TNotNull extends true
      ? TType
      : TType | null
    : never
}

type RequiredInsertKeys<T extends ColumnsConfig> = {
  [K in keyof T]: T[K] extends ColumnDef<any, any, infer TNotNull, infer THasDefault>
    ? TNotNull extends true
      ? THasDefault extends true
        ? never
        : K
      : never
    : never
}[keyof T]

type OptionalInsertKeys<T extends ColumnsConfig> = Exclude<keyof T, RequiredInsertKeys<T>>

export type InferInsert<T extends TableDef<any, any>> = {
  [K in RequiredInsertKeys<T['_columns']>]: T['_columns'][K] extends ColumnDef<any, infer TType, any, any>
    ? TType
    : never
} & {
  [K in OptionalInsertKeys<T['_columns']>]?: T['_columns'][K] extends ColumnDef<any, infer TType, infer TNotNull, any>
    ? TNotNull extends true
      ? TType
      : TType | null
    : never
}

function valueToAST(value: unknown): ASTExpr {
  if (value === null || value === undefined) return { kind: 'null' }
  if (typeof value === 'string') return { kind: 'string', value }
  if (typeof value === 'number') return { kind: 'number', value }
  if (typeof value === 'boolean') return { kind: 'boolean', value }
  return { kind: 'string', value: String(value) }
}

function columnDefToAST(key: string, col: ColumnDef<any, any, any, any>): ASTColumnDef {
  const def_: ASTColumnDef = {
    name: col._columnName,
    type: col._typeName,
    notNull: col._notNull,
    unique: col._unique,
    primaryKey: col._primaryKey,
  }
  if (col._default !== undefined) {
    def_.default = valueToAST(col._default)
  }
  if (col._references !== undefined) {
    def_.references = col._references
  }
  return def_
}

export type Nullable<T> = { [K in keyof T]: T[K] | null }

export function table<TName extends string, TColumns extends ColumnsConfig>(
  name: TName,
  columns: TColumns,
): TableDef<TName, TColumns> {
  return {
    _name: name,
    _columns: columns,
    toCreateAST(): ASTCreateTableCommand {
      return {
        kind: 'createTable',
        table: name,
        columns: Object.keys(columns).map((key) => columnDefToAST(key, columns[key])),
      }
    },
  }
}
