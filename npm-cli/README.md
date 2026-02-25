# @petradb/cli

PetraDB — a lightweight PostgreSQL-compatible SQL database engine. This package provides the interactive CLI (REPL) and batch execution tool.

## Installation

```bash
npm install -g @petradb/cli
```

## Usage

### Interactive REPL

```bash
# In-memory database
petradb -m

# Persistent database (creates or opens)
petradb mydb.petra
```

### Batch execution

```bash
# Execute a SQL file
petradb -m -f schema.sql

# Execute inline SQL
petradb -m -e "CREATE TABLE users (id SERIAL PRIMARY KEY, name TEXT);"

# Multiple operations
petradb -m -f schema.sql -f seed.sql -e "SELECT * FROM users;"

# Read SQL from stdin
echo "SELECT 1 + 1;" | petradb -m --stdin
```

## Flags

| Flag | Short | Description |
|------|-------|-------------|
| `--memory` | `-m` | Use in-memory database |
| `--file <path>` | `-f` | Execute SQL file (repeatable) |
| `--execute <sql>` | `-e` | Execute SQL string (repeatable) |
| `--stdin` | | Read SQL from stdin |
| `--path <path>` | | Database file path |

## Meta-commands

Inside the interactive REPL:

| Command | Description |
|---------|-------------|
| `\dt` | List all tables |
| `\d <table>` | Describe a table's columns |
| `\i <file>` | Execute a SQL file |
| `\dump` | Dump the database schema |
| `\q` | Quit |

## Documentation

Full documentation at [petradb.dev](https://petradb.dev).

## License

ISC
