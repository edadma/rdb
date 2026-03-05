declare const ClientBase: any;
declare const QueryCompilerBase: any;
declare const SchemaCompilerBase: any;
declare const TableCompilerBase: any;
declare const ColumnCompilerBase: any;
declare class ColumnCompiler_PetraDB extends ColumnCompilerBase {
    constructor(client: any, tableCompiler: any, columnBuilder: any);
    increments(options?: any): string;
    bigincrements(options?: any): string;
    integer(): string;
    text(): string;
    tinyint(): string;
    smallint(): string;
    mediumint(): string;
    bigint(): string;
    varchar(length: number): string;
    floating(): string;
    double(): string;
    decimal(precision: number | null, scale: number): string;
    boolean(): string;
    date(): string;
    time(): string;
    datetime(): string;
    timestamp(): string;
    uuid(options?: any): string;
    json(): string;
    jsonb(): string;
    binary(): string;
    enu(allowed: string[], options?: any): string;
}
declare class TableCompiler_PetraDB extends TableCompilerBase {
    constructor(client: any, tableBuilder: any);
    createQuery(columns: any, ifNot: boolean, like: any): void;
    addColumns(columns: any, prefix: string, colCompilers: any): void;
    primaryKeys(): string | undefined;
    index(columns: any, indexName: any): void;
    unique(columns: any, indexName: any): void;
    dropIndex(columns: any, indexName: any): void;
    dropUnique(columns: any, indexName: any): void;
    dropForeign(columns: any, indexName: any): void;
}
declare class SchemaCompiler_PetraDB extends SchemaCompilerBase {
    constructor(client: any, builder: any);
    hasTable(tableName: string): void;
    hasColumn(tableName: string, columnName: string): void;
    renameTable(from: string, to: string): void;
}
declare class QueryCompiler_PetraDB extends QueryCompilerBase {
    constructor(client: any, builder: any, formatter: any);
    truncate(): string;
    insert(): any;
    update(): {
        sql: string;
        returning: any;
    };
    del(): {
        sql: string;
        returning: any;
    };
    _returning(value: any): string;
}
declare class PetraDBClient extends ClientBase {
    constructor(config: any);
    _driver(): any;
    queryCompiler(builder: any, formatter: any): QueryCompiler_PetraDB;
    schemaCompiler(): SchemaCompiler_PetraDB;
    tableCompiler(): TableCompiler_PetraDB;
    columnCompiler(): ColumnCompiler_PetraDB;
    wrapIdentifierImpl(value: string): string;
    acquireRawConnection(): Promise<any>;
    destroyRawConnection(connection: any): Promise<void>;
    validateConnection(connection: any): boolean;
    positionBindings(sql: string): string;
    _query(connection: any, obj: any): Promise<any>;
    processResponse(obj: any, runner: any): any;
    poolDefaults(): any;
}
export default PetraDBClient;
