# @petradb/server

Lightweight SQL database server for PetraDB.

## Installation

```bash
npm install -g @petradb/server
```

## Usage

```bash
# In-memory database on default port (5480)
petradb-server -m

# Persistent database
petradb-server mydb.petra

# Custom host and port
petradb-server -m -h 0.0.0.0 -p 8080

# With config file
petradb-server -m -c config.toml
```

## Documentation

Full documentation at **[petradb.dev](https://petradb.dev)**.

## License

[ISC](https://opensource.org/licenses/ISC)
