#!/usr/bin/env python3
"""Tests for the petradb Python wrapper package."""

import sys
import os

# Add package to path for testing without install
sys.path.insert(0, os.path.dirname(__file__))

from petradb import Database

tests = 0
passed = 0

def check(condition, msg):
    global tests, passed
    tests += 1
    if condition:
        passed += 1
    else:
        print(f"  FAIL: {msg}")

print("PetraDB Python Package Tests")
print("=============================\n")

# ── Basic lifecycle ─────────────────────────────────────────────────

print("test_lifecycle")
db = Database()
check(db._db > 0, "Database opens")
db.close()

# ── Context manager ─────────────────────────────────────────────────

print("test_context_manager")
with Database() as db:
    db.execute("CREATE TABLE t (id INT)")
    check(True, "context manager works")

# ── DDL + DML ───────────────────────────────────────────────────────

print("test_ddl_dml")
with Database() as db:
    db.execute("CREATE TABLE users (id SERIAL PRIMARY KEY, name TEXT NOT NULL, age INT)")
    db.execute("INSERT INTO users (name, age) VALUES ('Alice', 30)")
    db.execute("INSERT INTO users (name, age) VALUES ('Bob', 25)")
    db.execute("INSERT INTO users (name, age) VALUES ('Carol', 35)")

    rows = db.query("SELECT name, age FROM users ORDER BY name")
    check(len(rows) == 3, "3 rows returned")
    check(rows[0].name == 'Alice', "first row name")
    check(rows[0].age == 30, "first row age")
    check(rows[2].name == 'Carol', "last row name")

# ── Row access ──────────────────────────────────────────────────────

print("test_row_access")
with Database() as db:
    db.execute("CREATE TABLE t (id INT, name TEXT)")
    db.execute("INSERT INTO t VALUES (1, 'hello')")
    row = db.query_one("SELECT id, name FROM t")

    check(row.id == 1, "attribute access")
    check(row['name'] == 'hello', "dict-style access")
    check(row[0] == 1, "index access")
    check(row.to_dict() == {'id': 1, 'name': 'hello'}, "to_dict")
    check(list(row.keys()) == ['id', 'name'], "keys()")
    check(len(row) == 2, "len()")

# ── NULL handling ───────────────────────────────────────────────────

print("test_null")
with Database() as db:
    db.execute("CREATE TABLE t (val INT)")
    db.execute("INSERT INTO t VALUES (NULL)")
    row = db.query_one("SELECT val FROM t")
    check(row.val is None, "NULL returns None")

# ── Cursor iteration ───────────────────────────────────────────────

print("test_cursor")
with Database() as db:
    db.execute("CREATE TABLE t (n INT)")
    for i in range(5):
        db.execute(f"INSERT INTO t VALUES ({i})")

    names = []
    with db.cursor("SELECT n FROM t ORDER BY n") as cur:
        check(cur.column_count == 1, "column count")
        check(cur.columns == ['n'], "column names")
        for row in cur:
            names.append(row.n)
    check(names == [0, 1, 2, 3, 4], "cursor iteration")

# ── Aggregates ──────────────────────────────────────────────────────

print("test_aggregates")
with Database() as db:
    db.execute("CREATE TABLE t (val INT)")
    db.execute("INSERT INTO t VALUES (10), (20), (30)")
    row = db.query_one("SELECT COUNT(*) AS cnt, SUM(val) AS total, AVG(val) AS average FROM t")
    check(row.cnt == 3, "count")
    check(row.total == 60, "sum")

# ── Update + Delete ─────────────────────────────────────────────────

print("test_update_delete")
with Database() as db:
    db.execute("CREATE TABLE t (id INT, name TEXT)")
    db.execute("INSERT INTO t VALUES (1, 'Alice'), (2, 'Bob'), (3, 'Carol')")

    rc = db.execute("UPDATE t SET name = 'ALICE' WHERE id = 1")
    check(rc == 1, "update returns 1")

    rc = db.execute("DELETE FROM t WHERE id = 2")
    check(rc == 1, "delete returns 1")

    rows = db.query("SELECT name FROM t ORDER BY id")
    check(len(rows) == 2, "2 rows remain")
    check(rows[0].name == 'ALICE', "updated value")

# ── Joins ───────────────────────────────────────────────────────────

print("test_joins")
with Database() as db:
    db.execute("CREATE TABLE depts (id SERIAL PRIMARY KEY, name TEXT)")
    db.execute("CREATE TABLE emps (id SERIAL PRIMARY KEY, name TEXT, dept_id INT)")
    db.execute("INSERT INTO depts (name) VALUES ('Engineering'), ('Marketing')")
    db.execute("INSERT INTO emps (name, dept_id) VALUES ('Alice', 1), ('Bob', 2)")

    rows = db.query(
        "SELECT e.name, d.name AS dept FROM emps e "
        "INNER JOIN depts d ON e.dept_id = d.id ORDER BY e.name"
    )
    check(rows[0].name == 'Alice', "join name")
    check(rows[0].dept == 'Engineering', "join dept")

# ── Error handling ──────────────────────────────────────────────────

print("test_errors")
with Database() as db:
    try:
        db.execute("INVALID SQL")
        check(False, "should have raised")
    except Exception:
        check(True, "invalid SQL raises exception")

    try:
        db.query("SELECT * FROM nonexistent")
        check(False, "should have raised")
    except Exception:
        check(True, "nonexistent table raises exception")

# ── User-defined functions ──────────────────────────────────────────

print("test_create_function")
with Database() as db:
    db.create_function('py_double', 1, lambda args: args[0] * 2)
    row = db.query_one("SELECT py_double(21) AS val")
    check(row.val == 42, "user function returns 42")

    db.create_function('py_greet', 1, lambda args: f"Hello, {args[0]}!")
    row = db.query_one("SELECT py_greet('World') AS val")
    check(row.val == "Hello, World!", "string function")

    db.create_function('py_null_safe', 1, lambda args: args[0] * 2 if args[0] is not None else None)
    row = db.query_one("SELECT py_null_safe(NULL) AS val")
    check(row.val is None, "null-safe function")

# ── Empty results ───────────────────────────────────────────────────

print("test_empty")
with Database() as db:
    db.execute("CREATE TABLE t (id INT)")
    rows = db.query("SELECT * FROM t")
    check(len(rows) == 0, "empty table returns empty list")
    row = db.query_one("SELECT * FROM t")
    check(row is None, "query_one on empty returns None")

# ── Results ─────────────────────────────────────────────────────────

print(f"\n=============================")
print(f"{passed}/{tests} tests passed")
sys.exit(0 if passed == tests else 1)
