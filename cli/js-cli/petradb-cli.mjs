#!/usr/bin/env node

import { createInterface } from "node:readline/promises";
import { readFileSync } from "node:fs";
import { stdin, stdout, argv, exit } from "node:process";

// Try fastLinkJS output first, fall back to fullLinkJS output
let Session, TextTable;
try {
  ({ Session, TextTable } = await import(
    "../../engine/js/target/scala-3.8.1/petradb-engine-fastopt/main.js"
  ));
} catch {
  ({ Session, TextTable } = await import(
    "../../engine/js/target/scala-3.8.1/petradb-engine-opt/main.js"
  ));
}

// ── Arg parsing ────────────────────────────────────────────────────

function parseArgs(args) {
  const opts = { memory: false, files: [], execSql: [], stdin: false };
  let i = 0;
  while (i < args.length) {
    const a = args[i];
    if (a === "-m" || a === "--memory") {
      opts.memory = true;
    } else if (a === "-f" || a === "--file") {
      if (++i >= args.length) {
        console.error("Error: -f requires a file path");
        exit(1);
      }
      opts.files.push(args[i]);
    } else if (a === "-e" || a === "--execute") {
      if (++i >= args.length) {
        console.error("Error: -e requires a SQL string");
        exit(1);
      }
      opts.execSql.push(args[i]);
    } else if (a === "--stdin") {
      opts.stdin = true;
    } else if (a === "-h" || a === "--help") {
      console.log(`Usage: petradb-cli.mjs [options]

Options:
  -m, --memory        Use in-memory database (default)
  -f, --file <path>   Execute SQL file and exit
  -e, --execute <sql> Execute SQL string and exit
  --stdin             Read SQL from stdin and exit
  -h, --help          Show this help`);
      exit(0);
    } else {
      console.error(`Unknown option: ${a}`);
      exit(1);
    }
    i++;
  }
  return opts;
}

// ── Output formatting ──────────────────────────────────────────────

function formatResult(result) {
  switch (result.command) {
    case "select": {
      if (!result.fields || result.fields.length === 0) {
        console.log("(0)");
        return;
      }
      const t = new TextTable();
      t.header(result.fields.map((f) => f.name));
      for (const [i, f] of result.fields.entries()) {
        if (isNumericType(f.dataType)) t.rightAlignment(i + 1);
      }
      for (const row of result.rows) {
        t.row(result.fields.map((f) => renderValue(row[f.name])));
      }
      console.log(t.render());
      console.log(`(${result.rows.length})\n`);
      return;
    }
    case "insert":
      if (result.fields && result.fields.length > 0) {
        const t = new TextTable();
        t.header(result.fields.map((f) => f.name));
        for (const row of result.rows) {
          t.row(result.fields.map((f) => renderValue(row[f.name])));
        }
        console.log(t.render());
      } else {
        console.log("INSERT 0 1");
      }
      return;
    case "update":
      console.log(`UPDATE ${result.rowCount}`);
      return;
    case "delete":
      console.log(`DELETE ${result.rowCount}`);
      return;
    case "create table":
      console.log("CREATE TABLE");
      return;
    case "create index":
      console.log("CREATE INDEX");
      return;
    case "drop table":
      console.log("DROP TABLE");
      return;
    case "drop index":
      console.log("DROP INDEX");
      return;
    case "create type":
      console.log("CREATE TYPE");
      return;
    case "drop type":
      console.log("DROP TYPE");
      return;
    case "alter table":
      console.log("ALTER TABLE");
      return;
    case "truncate table":
      console.log("TRUNCATE TABLE");
      return;
    case "begin":
      console.log("BEGIN");
      return;
    case "commit":
      console.log("COMMIT");
      return;
    case "rollback":
      console.log("ROLLBACK");
      return;
    case "prepare":
      console.log("PREPARE");
      return;
    case "deallocate":
      console.log("DEALLOCATE");
      return;
    default:
      console.log(result.command.toUpperCase());
  }
}

function isNumericType(dt) {
  return /^(int|integer|smallint|bigint|serial|smallserial|bigserial|double|float|real|numeric|decimal)/i.test(
    dt,
  );
}

function renderValue(v) {
  if (v === null || v === undefined) return "";
  if (v instanceof Date) return v.toISOString().replace("T", " ").replace("Z", "");
  if (Array.isArray(v)) return JSON.stringify(v);
  if (typeof v === "object") return JSON.stringify(v);
  return String(v);
}

// ── SQL execution ──────────────────────────────────────────────────

async function executeSql(db, sql) {
  try {
    const results = await db.execute(sql);
    for (const r of results) formatResult(r);
  } catch (e) {
    console.error(e.message || String(e));
  }
}

async function executeFile(db, path) {
  try {
    const sql = readFileSync(path, "utf-8");
    await executeSql(db, sql);
  } catch (e) {
    if (e.code === "ENOENT") console.error(`File not found: ${path}`);
    else console.error(e.message || String(e));
  }
}

// ── REPL ───────────────────────────────────────────────────────────

async function repl(db) {
  const rl = createInterface({ input: stdin, output: stdout });

  console.log("PetraDB — interactive SQL shell (JavaScript)");
  console.log("Type \\q to quit, \\dt to list tables, \\d <table> to describe a table.");
  console.log();

  try {
    while (true) {
      let line;
      try {
        line = await rl.question("petra> ");
      } catch {
        break; // EOF
      }
      const trimmed = line.trim();
      if (!trimmed) continue;

      if (trimmed.startsWith("\\")) {
        const shouldContinue = await handleMeta(db, trimmed);
        if (!shouldContinue) break;
        continue;
      }

      // Collect multiline input until semicolon
      let buf = line;
      if (!trimmed.endsWith(";")) {
        while (true) {
          let cont;
          try {
            cont = await rl.question("  -> ");
          } catch {
            break; // EOF
          }
          buf += "\n" + cont;
          if (cont.trim().endsWith(";")) break;
        }
      }

      await executeSql(db, buf);
    }
  } finally {
    rl.close();
  }
}

async function handleMeta(db, input) {
  const parts = input.split(/\s+/);
  const cmd = parts[0];

  switch (cmd) {
    case "\\q":
      return false;
    case "\\dt": {
      // List tables — use a query to discover them
      try {
        const [res] = await db.execute(
          "SELECT table_name FROM information_schema.tables ORDER BY table_name",
        );
        if (res.rows.length === 0) {
          console.log("No tables.");
        } else {
          const t = new TextTable();
          t.header(["Table"]);
          for (const row of res.rows) t.row([row.table_name]);
          console.log(t.render());
        }
      } catch {
        // Fall back: try to get table list by creating and querying a temp workaround
        console.log("\\dt is not supported in this build.");
      }
      return true;
    }
    case "\\d": {
      if (parts.length < 2) {
        console.log("Usage: \\d <table>");
        return true;
      }
      const tableName = parts[1];
      try {
        // Use a SELECT to discover columns
        const [res] = await db.execute(
          `SELECT column_name, data_type, is_nullable FROM information_schema.columns WHERE table_name = '${tableName}' ORDER BY ordinal_position`,
        );
        if (res.rows.length === 0) {
          console.log(`Table '${tableName}' not found.`);
        } else {
          const t = new TextTable();
          t.header(["Column", "Type", "Nullable"]);
          for (const row of res.rows)
            t.row([row.column_name, row.data_type, row.is_nullable]);
          console.log(`Table "${tableName}"`);
          console.log(t.render());
        }
      } catch {
        console.log(`Table '${tableName}' not found.`);
      }
      return true;
    }
    case "\\i": {
      if (parts.length < 2) {
        console.log("Usage: \\i <file>");
        return true;
      }
      await executeFile(db, parts[1]);
      return true;
    }
    case "\\timing": {
      console.log("Use: time node cli/js-cli/petradb-cli.mjs -f <file>");
      return true;
    }
    default:
      console.log(`Unknown command: ${cmd}`);
      console.log("Available: \\dt  \\d <table>  \\i <file>  \\q");
      return true;
  }
}

// ── Main ───────────────────────────────────────────────────────────

const opts = parseArgs(argv.slice(2));
const db = new Session();
const batch = opts.files.length > 0 || opts.execSql.length > 0 || opts.stdin;

if (batch) {
  for (const f of opts.files) await executeFile(db, f);
  if (opts.stdin) {
    const chunks = [];
    for await (const chunk of stdin) chunks.push(chunk);
    const sql = Buffer.concat(chunks).toString("utf-8");
    if (sql.trim()) await executeSql(db, sql);
  }
  for (const sql of opts.execSql) await executeSql(db, sql);
} else {
  await repl(db);
}
