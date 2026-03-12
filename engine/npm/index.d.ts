export interface SessionOptions {
    rowMode?: 'object' | 'array';
    storage?: 'memory' | 'persistent' | 'text';
    path?: string;
    pageSize?: number;
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

export interface CreateViewResult {
    command: 'create view';
    view: string;
}

export interface DropViewResult {
    command: 'drop view';
    view: string;
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

export interface ExplainResult {
    command: 'explain';
    plan: string;
}

export interface CopyResult {
    command: 'copy';
    rowCount: number;
}

export interface CreateSchemaResult {
    command: 'create schema';
    schema: string;
}

export interface CreateSequenceResult {
    command: 'create sequence';
    sequence: string;
}

export interface DropSequenceResult {
    command: 'drop sequence';
    sequence: string;
}

export interface InsertResult {
    command: 'insert';
    result: Record<string, any>;
    rows: Record<string, any>[];
    fields: FieldInfo[];
}

export interface SelectResult<T = Record<string, any>> {
    command: 'select';
    rows: T[];
    fields: FieldInfo[];
}

export interface UpdateResult {
    command: 'update';
    rowCount: number;
}

export interface DeleteResult {
    command: 'delete';
    rowCount: number;
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
    | CreateViewResult
    | DropViewResult
    | InsertResult
    | SelectResult
    | UpdateResult
    | DeleteResult
    | PrepareResult
    | DeallocateResult
    | BeginResult
    | CommitResult
    | RollbackResult
    | ExplainResult
    | CopyResult
    | CreateSchemaResult
    | CreateSequenceResult
    | DropSequenceResult;

export interface PreparedStatement {
    execute(params?: any[], options?: ExecuteOptions): Promise<ExecuteResult[]>;
}

export class Session {
    constructor(options?: SessionOptions);
    execute(sql: string, options?: ExecuteOptions): Promise<ExecuteResult[]>;
    executeAST(ast: any, options?: ExecuteOptions): Promise<ExecuteResult[]>;
    prepare(sql: string): PreparedStatement;
    close(): Promise<void>;
}
