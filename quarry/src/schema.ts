import type { ASTExpr, ASTColumnDef, ASTCreateTableCommand } from './ast.js'

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

// Auto-incrementing integer primary key
export function serial<TName extends string>(name: TName): ColumnDef<TName, number, true, true> {
  return createColumn<TName, number, true>(name, 'serial', true).notNull() as any
}

// Auto-incrementing bigint primary key
export function bigserial<TName extends string>(name: TName): ColumnDef<TName, number, true, true> {
  return createColumn<TName, number, true>(name, 'bigserial', true).notNull() as any
}

// Variable-length text
export function text<TName extends string>(name: TName): ColumnDef<TName, string, false, false> {
  return createColumn<TName, string, false>(name, 'text', false)
}

// Variable-length text with max length
export function varchar<TName extends string>(name: TName, length?: number): ColumnDef<TName, string, false, false> {
  const typeName = length !== undefined ? `varchar(${length})` : 'varchar'
  return createColumn<TName, string, false>(name, typeName, false)
}

// Fixed-length text
export function char<TName extends string>(name: TName, length?: number): ColumnDef<TName, string, false, false> {
  const typeName = length !== undefined ? `char(${length})` : 'char'
  return createColumn<TName, string, false>(name, typeName, false)
}

// 32-bit integer
export function integer<TName extends string>(name: TName): ColumnDef<TName, number, false, false> {
  return createColumn<TName, number, false>(name, 'integer', false)
}

// 16-bit integer
export function smallint<TName extends string>(name: TName): ColumnDef<TName, number, false, false> {
  return createColumn<TName, number, false>(name, 'smallint', false)
}

// 64-bit integer
export function bigint<TName extends string>(name: TName): ColumnDef<TName, number, false, false> {
  return createColumn<TName, number, false>(name, 'bigint', false)
}

// Double-precision floating point
export function doublePrecision<TName extends string>(name: TName): ColumnDef<TName, number, false, false> {
  return createColumn<TName, number, false>(name, 'double', false)
}

// Single-precision floating point
export function real<TName extends string>(name: TName): ColumnDef<TName, number, false, false> {
  return createColumn<TName, number, false>(name, 'real', false)
}

// Arbitrary precision numeric
export function numeric<TName extends string>(name: TName, precision?: number, scale?: number): ColumnDef<TName, number, false, false> {
  let typeName = 'numeric'
  if (precision !== undefined) {
    typeName = scale !== undefined ? `numeric(${precision},${scale})` : `numeric(${precision})`
  }
  return createColumn<TName, number, false>(name, typeName, false)
}

// Boolean
export function boolean<TName extends string>(name: TName): ColumnDef<TName, boolean, false, false> {
  return createColumn<TName, boolean, false>(name, 'boolean', false)
}

// UUID
export function uuid<TName extends string>(name: TName): ColumnDef<TName, string, false, false> {
  return createColumn<TName, string, false>(name, 'uuid', false)
}

// Timestamp without timezone
export function timestamp<TName extends string>(name: TName): ColumnDef<TName, string, false, false> {
  return createColumn<TName, string, false>(name, 'timestamp', false)
}

// Timestamp with timezone
export function timestamptz<TName extends string>(name: TName): ColumnDef<TName, string, false, false> {
  return createColumn<TName, string, false>(name, 'timestamptz', false)
}

// Date
export function date<TName extends string>(name: TName): ColumnDef<TName, string, false, false> {
  return createColumn<TName, string, false>(name, 'date', false)
}

// Time without timezone
export function time<TName extends string>(name: TName): ColumnDef<TName, string, false, false> {
  return createColumn<TName, string, false>(name, 'time', false)
}

// Time with timezone
export function timetz<TName extends string>(name: TName): ColumnDef<TName, string, false, false> {
  return createColumn<TName, string, false>(name, 'timetz', false)
}

// Interval
export function interval<TName extends string>(name: TName): ColumnDef<TName, string, false, false> {
  return createColumn<TName, string, false>(name, 'interval', false)
}

// JSON
export function json<TName extends string>(name: TName): ColumnDef<TName, unknown, false, false> {
  return createColumn<TName, unknown, false>(name, 'json', false)
}

// Binary data
export function bytea<TName extends string>(name: TName): ColumnDef<TName, number[], false, false> {
  return createColumn<TName, number[], false>(name, 'bytea', false)
}

// ── Table definition ──

export type ColumnsConfig = Record<string, ColumnDef<any, any, any, any>>

export interface TableDef<TName extends string, TColumns extends ColumnsConfig> {
  readonly _name: TName
  readonly _columns: TColumns
  readonly _originalName?: string
  toCreateAST(): ASTCreateTableCommand
  as<TAlias extends string>(alias: TAlias): TableDef<TAlias, TColumns>
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
    as<TAlias extends string>(alias: TAlias): TableDef<TAlias, TColumns> {
      return {
        _name: alias,
        _columns: columns,
        _originalName: name,
        toCreateAST() {
          throw new Error('Cannot create table from an alias')
        },
        as<TAlias2 extends string>(alias2: TAlias2): TableDef<TAlias2, TColumns> {
          return table(name, columns).as(alias2)
        },
      }
    },
  }
}
