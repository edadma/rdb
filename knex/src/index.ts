// @ts-nocheck — Knex internals are untyped CJS; we extend them at runtime.

import { createRequire } from "node:module";
import { fileURLToPath } from "node:url";
import path from "node:path";

const _require = createRequire(import.meta.url);

const ClientBase = _require("knex/lib/client.js");
const QueryCompilerBase = _require("knex/lib/query/querycompiler.js");
const SchemaCompilerBase = _require("knex/lib/schema/compiler.js");
const TableCompilerBase = _require("knex/lib/schema/tablecompiler.js");
const ColumnCompilerBase = _require("knex/lib/schema/columncompiler.js");

// ---------------------------------------------------------------------------
// Column Compiler
// ---------------------------------------------------------------------------

class ColumnCompiler_PetraDB extends ColumnCompilerBase {
  constructor(client: any, tableCompiler: any, columnBuilder: any) {
    super(client, tableCompiler, columnBuilder);
    this.modifiers = ["nullable", "defaultTo"];
  }

  increments(options: any = { primaryKey: true }) {
    return (
      "serial" +
      (this.tableCompiler._canBeAddPrimaryKey(options) ? " primary key" : "")
    );
  }

  bigincrements(options: any = { primaryKey: true }) {
    return (
      "bigserial" +
      (this.tableCompiler._canBeAddPrimaryKey(options) ? " primary key" : "")
    );
  }

  integer() {
    return "integer";
  }

  text() {
    return "text";
  }

  tinyint() {
    return "smallint";
  }

  smallint() {
    return "smallint";
  }

  mediumint() {
    return "integer";
  }

  bigint() {
    return "bigint";
  }

  varchar(length: number) {
    return `varchar(${length || 255})`;
  }

  floating() {
    return "double";
  }

  double() {
    return "double";
  }

  decimal(precision: number | null, scale: number) {
    if (precision === null) return "numeric";
    return `numeric(${precision ?? 8}, ${scale ?? 2})`;
  }

  boolean() {
    return "boolean";
  }

  date() {
    return "date";
  }

  time() {
    return "time";
  }

  datetime() {
    return "timestamp";
  }

  timestamp() {
    return "timestamp";
  }

  uuid(options: any = { primaryKey: false }) {
    return (
      "uuid" +
      (this.tableCompiler._canBeAddPrimaryKey(options) ? " primary key" : "")
    );
  }

  json() {
    return "json";
  }

  jsonb() {
    return "jsonb";
  }

  binary() {
    return "bytea";
  }

  enu(allowed: string[], options: any = {}) {
    if (options.useNative) {
      const enumName = options.enumName
        ? `"${options.enumName}"`
        : `"${this.args[0]}_enum"`;
      if (!options.existingType) {
        const values = allowed.map((v: string) => `'${v}'`).join(", ");
        this.tableCompiler.unshiftQuery(
          `create type ${enumName} as enum (${values})`
        );
      }
      return enumName;
    }
    const values = allowed.map((v: string) => `'${v}'`).join(", ");
    return `text check (${this.formatter.wrap(this.args[0])} in (${values}))`;
  }
}

// ---------------------------------------------------------------------------
// Table Compiler
// ---------------------------------------------------------------------------

class TableCompiler_PetraDB extends TableCompilerBase {
  constructor(client: any, tableBuilder: any) {
    super(client, tableBuilder);
  }

  createQuery(columns: any, ifNot: boolean, like: any) {
    const createStatement = ifNot
      ? "create table if not exists "
      : "create table ";
    const columnsSql = ` (${columns.sql.join(", ")}${
      this.primaryKeys() || ""
    }${this._addChecks()})`;

    const sql =
      createStatement +
      this.tableName() +
      (like && this.tableNameLike()
        ? " (like " +
          this.tableNameLike() +
          " including all" +
          (columns.sql.length ? ", " + columns.sql.join(", ") : "") +
          ")"
        : columnsSql);

    this.pushQuery({
      sql,
      bindings: columns.bindings,
    });
  }

  // PetraDB supports ALTER TABLE ADD COLUMN
  addColumns(columns: any, prefix: string, colCompilers: any) {
    if (prefix === this.alterColumnsPrefix) {
      for (const col of colCompilers) {
        const quotedTableName = this.tableName();
        const type = col.getColumnType();
        const colName = this.client.wrapIdentifier(
          col.getColumnName(),
          col.columnBuilder.queryContext()
        );
        this.pushQuery({
          sql: `alter table ${quotedTableName} alter column ${colName} type ${type}`,
          bindings: [],
        });
      }
    } else {
      super.addColumns(columns, prefix);
    }
  }

  primaryKeys() {
    const pks = (this.grouped.alterTable || []).filter(
      (k: any) => k.method === "primary"
    );
    if (pks.length > 0 && pks[0].args.length > 0) {
      const columns = pks[0].args[0];
      let constraintName = pks[0].args[1] || "";
      if (typeof constraintName === "object") {
        constraintName = constraintName.constraintName || "";
      }
      constraintName = constraintName
        ? this.formatter.wrap(constraintName)
        : this.formatter.wrap(`${this.tableNameRaw}_pkey`);
      return `, constraint ${constraintName} primary key (${this.formatter.columnize(columns)})`;
    }
  }

  index(columns: any, indexName: any) {
    indexName = indexName
      ? this.formatter.wrap(indexName)
      : this._indexCommand("index", this.tableNameRaw, columns);
    this.pushQuery(
      `create index ${indexName} on ${this.tableName()} (${this.formatter.columnize(columns)})`
    );
  }

  unique(columns: any, indexName: any) {
    indexName = indexName
      ? this.formatter.wrap(indexName)
      : this._indexCommand("unique", this.tableNameRaw, columns);
    this.pushQuery(
      `alter table ${this.tableName()} add constraint ${indexName} unique (${this.formatter.columnize(columns)})`
    );
  }

  dropIndex(columns: any, indexName: any) {
    indexName = indexName
      ? this.formatter.wrap(indexName)
      : this._indexCommand("index", this.tableNameRaw, columns);
    this.pushQuery(`drop index ${indexName}`);
  }

  dropUnique(columns: any, indexName: any) {
    indexName = indexName
      ? this.formatter.wrap(indexName)
      : this._indexCommand("unique", this.tableNameRaw, columns);
    this.pushQuery(
      `alter table ${this.tableName()} drop constraint ${indexName}`
    );
  }

  dropForeign(columns: any, indexName: any) {
    indexName = indexName
      ? this.formatter.wrap(indexName)
      : this._indexCommand("foreign", this.tableNameRaw, columns);
    this.pushQuery(
      `alter table ${this.tableName()} drop constraint ${indexName}`
    );
  }
}

// ---------------------------------------------------------------------------
// Schema Compiler
// ---------------------------------------------------------------------------

class SchemaCompiler_PetraDB extends SchemaCompilerBase {
  constructor(client: any, builder: any) {
    super(client, builder);
  }

  hasTable(tableName: string) {
    this.pushQuery({
      sql: "SHOW TABLES",
      output(resp: any) {
        const rows = resp.rows || resp;
        return rows.some(
          (r: any) =>
            (r.table_name || r.TABLE_NAME || "").toLowerCase() ===
            tableName.toLowerCase()
        );
      },
    });
  }

  hasColumn(tableName: string, columnName: string) {
    const wrapped = this.formatter.wrap(tableName);
    this.pushQuery({
      sql: `SHOW COLUMNS FROM ${wrapped}`,
      output(resp: any) {
        const rows = resp.rows || resp;
        return rows.some(
          (r: any) =>
            (r.name || r.column_name || r.NAME || "").toLowerCase() ===
            columnName.toLowerCase()
        );
      },
    });
  }

  renameTable(from: string, to: string) {
    this.pushQuery(
      `alter table ${this.formatter.wrap(from)} rename to ${this.formatter.wrap(to)}`
    );
  }
}

// ---------------------------------------------------------------------------
// Query Compiler
// ---------------------------------------------------------------------------

class QueryCompiler_PetraDB extends QueryCompilerBase {
  constructor(client: any, builder: any, formatter: any) {
    super(client, builder, formatter);
    this._defaultInsertValue = "default";
  }

  truncate() {
    return `truncate table ${this.tableName}`;
  }

  insert() {
    let sql = super.insert();
    if (sql === "") return sql;

    const { returning } = this.single;
    if (returning) sql += this._returning(returning);
    return { sql, returning };
  }

  update() {
    const withSQL = this.with();
    const updateData = this._prepUpdate(this.single.update);
    const wheres = this.where();
    const { returning } = this.single;
    return {
      sql:
        withSQL +
        `update ${this.tableName} set ${updateData.join(", ")}` +
        (wheres ? ` ${wheres}` : "") +
        this._returning(returning),
      returning,
    };
  }

  del() {
    const { tableName } = this;
    const withSQL = this.with();
    const wheres = this.where() || "";
    const { returning } = this.single;
    return {
      sql:
        withSQL +
        `delete from ${tableName}` +
        (wheres ? ` ${wheres}` : "") +
        this._returning(returning),
      returning,
    };
  }

  _returning(value: any) {
    return value ? ` returning ${this.formatter.columnize(value)}` : "";
  }
}

// ---------------------------------------------------------------------------
// Client
// ---------------------------------------------------------------------------

class PetraDBClient extends ClientBase {
  constructor(config: any) {
    super({ ...config, client: "petradb" });
  }

  _driver() {
    return _require("@petradb/engine");
  }

  queryCompiler(builder: any, formatter: any) {
    return new QueryCompiler_PetraDB(this, builder, formatter);
  }

  schemaCompiler() {
    return new SchemaCompiler_PetraDB(this, ...arguments);
  }

  tableCompiler() {
    return new TableCompiler_PetraDB(this, ...arguments);
  }

  columnCompiler() {
    return new ColumnCompiler_PetraDB(this, ...arguments);
  }

  wrapIdentifierImpl(value: string) {
    if (value === "*") return value;
    return `"${value.replace(/"/g, '""')}"`;
  }

  async acquireRawConnection() {
    const { Session } = this.driver;
    const settings = this.connectionSettings || {};
    return new Session({
      storage: settings.storage || "memory",
      ...(settings.path ? { path: settings.path } : {}),
      ...(settings.pageSize ? { pageSize: settings.pageSize } : {}),
    });
  }

  async destroyRawConnection(connection: any) {
    connection.close();
  }

  validateConnection(connection: any) {
    return true;
  }

  // Convert ? placeholders → $1, $2, ...
  positionBindings(sql: string) {
    let questionCount = 0;
    return sql.replace(/(\\*)(\?)/g, function (_match: string, escapes: string) {
      if (escapes.length % 2) {
        return "?";
      } else {
        questionCount++;
        return `$${questionCount}`;
      }
    });
  }

  async _query(connection: any, obj: any) {
    if (!obj.sql) throw new Error("The query is empty");

    const sql = obj.sql;
    const bindings = obj.bindings || [];
    let results: any[];

    if (bindings.length > 0) {
      const stmt = connection.prepare(sql);
      results = await stmt.execute(bindings);
    } else {
      results = await connection.execute(sql);
    }

    // Take the first result (one statement per knex query)
    obj.response = results[0];
    return obj;
  }

  processResponse(obj: any, runner: any) {
    const resp = obj.response;
    if (obj.output) return obj.output.call(runner, resp);
    if (obj.method === "raw") return resp;

    const { returning } = obj;

    // PetraDB returns { command, rows, fields } for selects,
    // { command, result, rows, fields } for inserts,
    // { command, rowCount } for update/delete.
    const command = resp && resp.command;

    switch (obj.method) {
      case "select":
        return resp.rows || [];
      case "first":
        return (resp.rows || [])[0];
      case "pluck":
        return (resp.rows || []).map((r: any) => r[obj.pluck]);
      case "insert":
        if (returning) {
          return resp.rows || [];
        }
        // Return the auto-generated values (e.g. serial id)
        return [resp.result];
      case "update":
      case "del":
      case "counter":
        if (returning) {
          return resp.rows || [];
        }
        return resp.rowCount ?? 0;
      default:
        return resp;
    }
  }

  poolDefaults() {
    const defaults = super.poolDefaults();
    // In-process engine — one connection is fine
    return { ...defaults, min: 1, max: 1 };
  }
}

Object.assign(PetraDBClient.prototype, {
  dialect: "petradb",
  driverName: "petradb",
});

export default PetraDBClient;
