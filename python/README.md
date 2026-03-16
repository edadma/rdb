# petradb

Embeddable SQL database for Python — PostgreSQL-compatible, in-memory or persistent.

Wraps the PetraDB native shared library via ctypes. No compilation needed — just install the library and import.

## Install

```bash
pip install petradb
```

The native library is bundled in the wheel. Currently supports **Linux x86_64** only. macOS and ARM support coming soon.

## Quick Start

```python
from petradb import Database

db = Database()  # in-memory

db.execute("""
    CREATE TABLE users (id SERIAL PRIMARY KEY, name TEXT NOT NULL, age INT);
    INSERT INTO users (name, age) VALUES ('Alice', 30), ('Bob', 25);
""")

for row in db.query("SELECT * FROM users ORDER BY name"):
    print(f"{row.id}: {row.name}, age {row.age}")

db.close()
```

## Persistent Storage

```python
db = Database('mydata.db')  # creates or opens
```

## Context Manager

```python
with Database() as db:
    db.execute("CREATE TABLE t (id INT)")
    rows = db.query("SELECT * FROM t")
```

## Row Access

```python
row = db.query_one("SELECT id, name FROM users WHERE id = 1")
row.name       # attribute access
row['name']    # dict-style access
row[0]         # index access
row.to_dict()  # {'id': 1, 'name': 'Alice'}
```

## Cursor (Lazy Iteration)

```python
with db.cursor("SELECT * FROM large_table") as cur:
    for row in cur:
        process(row)
```

## User-Defined Functions

```python
db.create_function('double', 1, lambda args: args[0] * 2)
db.query("SELECT double(age) FROM users")
```

## Links

- [Documentation](https://petradb.dev)
- [C API Reference](https://petradb.dev/reference/api-c/)
- [GitHub](https://github.com/edadma/petradb)
