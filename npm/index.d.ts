export interface ConnectSQLOptions {
    rowMode?: 'object' | 'array';
}

export interface ExecuteOptions {
    rowMode?: 'object' | 'array';
}

export interface FieldInfo {
    name: string;
    dataType: string;
}

export interface CreateTableResult {
    command: 'create table';
    table: string;
}

export interface DropTableResult {
    command: 'drop table';
    table: string;
}

export interface CreateTypeResult {
    command: 'create type';
    type: string;
}

export interface DropTypeResult {
    command: 'drop type';
    type: string;
}

export interface CreateIndexResult {
    command: 'create index';
    index: string;
}

export interface DropIndexResult {
    command: 'drop index';
    index: string;
}

export interface TruncateTableResult {
    command: 'truncate table';
    table: string;
}

export interface AlterTableResult {
    command: 'alter table';
}

export interface PrepareResult {
    command: 'prepare';
    name: string;
}

export interface DeallocateResult {
    command: 'deallocate';
    name: string;
}

export interface BeginResult {
    command: 'begin';
}

export interface CommitResult {
    command: 'commit';
}

export interface RollbackResult {
    command: 'rollback';
}

export interface InsertResult {
    command: 'insert';
    result: Record<string, any>;
}

export interface SelectResult<T = Record<string, any>> {
    command: 'select';
    rows: T[];
    fields: FieldInfo[];
}

export interface UpdateResult {
    command: 'update';
    rows: number;
}

export interface DeleteResult {
    command: 'delete';
    rows: number;
}

export type ExecuteResult =
    | CreateTableResult
    | DropTableResult
    | CreateTypeResult
    | DropTypeResult
    | CreateIndexResult
    | DropIndexResult
    | TruncateTableResult
    | AlterTableResult
    | InsertResult
    | SelectResult
    | UpdateResult
    | DeleteResult
    | PrepareResult
    | DeallocateResult
    | BeginResult
    | CommitResult
    | RollbackResult;

export class ConnectSQL {
    constructor(options?: ConnectSQLOptions);
    execute(sql: string, options?: ExecuteOptions): ExecuteResult[];
}
