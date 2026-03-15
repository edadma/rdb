"""PetraDB — Embeddable SQL database for Python.

A PostgreSQL-compatible SQL database that runs in-memory or on disk.
Uses the PetraDB native shared library via ctypes.

Usage:
    from petradb import Database

    db = Database()
    db.execute("CREATE TABLE users (id SERIAL PRIMARY KEY, name TEXT)")
    db.execute("INSERT INTO users (name) VALUES ('Alice')")

    for row in db.query("SELECT * FROM users"):
        print(row.id, row.name)

    db.close()
"""

from petradb._binding import _lib, _ensure_loaded
from petradb._database import Database, Cursor, Row

__all__ = ['Database', 'Cursor', 'Row']
__version__ = '1.5.1'
