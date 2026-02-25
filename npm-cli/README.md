# @petradb/cli

Interactive SQL shell and batch execution tool for PetraDB.

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
```

## Documentation

Full documentation at **[petradb.dev](https://petradb.dev)**.

## License

[ISC](https://opensource.org/licenses/ISC)
