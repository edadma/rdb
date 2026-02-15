interface ConnectSQLOptions {
    rowMode?: 'object' | 'array';
}

interface ExecuteOptions {
    rowMode?: 'object' | 'array';
}

interface FieldInfo {
    name: string;
    dataType: string;
}

interface CreateTableResult {
    command: 'create table';
    table: string;
}

interface DropTableResult {
    command: 'drop table';
    table: string;
}

interface CreateTypeResult {
    command: 'create type';
    type: string;
}

interface DropTypeResult {
    command: 'drop type';
    type: string;
}

interface DropIndexResult {
    command: 'drop index';
    index: string;
}

interface AlterTableResult {
    command: 'alter table';
}

interface InsertResult {
    command: 'insert';
    result: Record<string, any>;
}

interface SelectResult<T = Record<string, any>> {
    command: 'select';
    rows: T[];
    fields: FieldInfo[];
}

interface UpdateResult {
    command: 'update';
    rows: number;
}

interface DeleteResult {
    command: 'delete';
    rows: number;
}

type ExecuteResult =
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
