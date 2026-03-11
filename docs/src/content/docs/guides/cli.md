---
title: CLI
description: Interactive SQL shell and command-line tools for PetraDB.
---

## Installation

```bash
npm install -g @petradb/cli
```

## Usage

```bash
petradb [OPTIONS] [path]
```

If a path is given, the CLI opens (or creates) a persistent database at that location. Without a path, an in-memory database is used.

### Options

| Option | Description |
|--------|-------------|
| `-m`, `--memory` | Use an in-memory database (even if a path is given) |
| `-e`, `--execute` | Execute SQL and exit (repeatable) |
| `-f`, `--file` | Execute a SQL file and exit (repeatable) |
| `--stdin` | Read SQL from stdin and exit |
| `--host` | Connect to a remote PetraDB server |
| `--port` | Server port (default: 5480) |
| `--user` | Username for authentication |
| `--password` | Password for authentication |

### Examples

```bash
# Start an interactive in-memory session
petradb

# Open or create a persistent database
petradb mydata.db

# Run SQL non-interactively
petradb mydata.db -e "SELECT * FROM users"

# Execute a file then a query
petradb mydata.db -f schema.sql -e "SELECT count(*) FROM users"

# Pipe SQL from stdin
echo "SELECT 1 + 1" | petradb --stdin

# Connect to a remote server
petradb --host myserver.example.com --port 5480

# Connect with authentication
petradb --host localhost --user admin --password secret
```

## Database Modes

The file extension determines the storage mode:

| Extension | Mode | Description |
|-----------|------|-------------|
| `.petra` (or any other) | Persistent | Crash-safe durable storage via copy-on-write pages |
| `.ptxt` | Text | Human-readable file, rewritten after every change |
| *(no path)* | In-memory | Data lives only for the duration of the session |

## Dump

The `dump` subcommand prints the full schema and data of a persistent database as SQL statements.

```bash
petradb dump mydata.db
```

Output includes `CREATE TYPE`, `CREATE TABLE`, `INSERT INTO`, and `CREATE VIEW` statements that can recreate the database from scratch.

## Interactive REPL

When started without `-e`, `-f`, or `--stdin`, the CLI enters an interactive SQL shell with readline history.

Type SQL terminated by `;` to execute. Multi-line input is supported — a continuation prompt (`  -> `) appears until you end with `;`.

### Meta-Commands

| Command | Description |
|---------|-------------|
| `\dt` | List all tables |
| `\dv` | List all views |
| `\ds` | List all sequences |
| `\di` | List all indexes |
| `\d <name>` | Describe a table or view (columns, types, nullability) |
| `\i <file>` | Execute SQL from a file |
| `\dump` | Print the full schema and data as SQL |
| `\copy <args>` | Run a `COPY` statement |
| `\timing` | Toggle query timing on/off |
| `\q` | Quit |

### Example Session

```
$ petradb
PetraDB — interactive SQL shell
Type \q to quit, \dt to list tables, \d <table> to describe a table.

petra> CREATE TABLE cities (name TEXT, population INT);
CREATE TABLE
petra> INSERT INTO cities (name, population) VALUES ('Oslo', 709037);
INSERT 0 1
petra> SELECT * FROM cities;
 name | population
------+------------
 Oslo |     709037
(1 row)
petra> \dt
 Table
--------
 cities
(1 row)
petra> \d cities
Table "cities"
 Column     | Type | Nullable
------------+------+----------
 name       | TEXT | null
 population | INT  | null
petra> \timing
Timing is on.
petra> SELECT count(*) FROM cities;
 count
-------
     1
(1 row)
Time: 0.002 s
petra> \q
```
