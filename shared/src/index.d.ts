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

export interface DropIndexResult {
    command: 'drop index';
    index: string;
}

export interface AlterTableResult {
    command: 'alter table';
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
    | DropIndexResult
    | AlterTableResult
    | InsertResult
    | SelectResult
    | UpdateResult
    | DeleteResult;

export class ConnectSQL {
    constructor(options?: ConnectSQLOptions);
    execute(sql: string, options?: ExecuteOptions): ExecuteResult[];
}
