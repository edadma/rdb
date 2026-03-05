# @petradb/cli

Interactive SQL shell and batch execution tool for PetraDB. Works with local databases or connects to a remote PetraDB server.

## Installation

```bash
npm install -g @petradb/cli
```

## Usage

```bash
# In-memory database
petradb -m

# Persistent database (creates or opens)
petradb mydb.petra

# Execute a SQL file
petradb -m -f schema.sql

# Execute inline SQL
petradb -m -e "SELECT 1 + 1;"

# Connect to a remote server
petradb --host localhost --port 5480

# Connect with authentication
petradb --host myserver.example.com --user admin --password secret
```

## Options

| Option | Description |
|--------|-------------|
| `-m` | In-memory database |
| `-e <sql>` | Execute SQL and exit (repeatable) |
| `-f <file>` | Execute SQL file and exit (repeatable) |
| `--stdin` | Read SQL from stdin |
| `--host` | Connect to a remote PetraDB server |
| `--port` | Server port (default: 5480) |
| `--user` | Username for authentication |
| `--password` | Password for authentication |

## Documentation

Full documentation at **[petradb.dev](https://petradb.dev)**.

## License

[ISC](https://opensource.org/licenses/ISC)
