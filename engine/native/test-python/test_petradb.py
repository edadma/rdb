#!/usr/bin/env python3
"""PetraDB Python FFI test using ctypes."""

import ctypes
import os
import sys

# Find the shared library
lib_dir = os.path.join(os.path.dirname(__file__), '..', 'target', 'scala-3.8.2')
lib_path = os.path.join(lib_dir, 'libpetradb-engine.so')

if not os.path.exists(lib_path):
    # Try .dylib for macOS
    lib_path = os.path.join(lib_dir, 'libpetradb-engine.dylib')

if not os.path.exists(lib_path):
    print(f"ERROR: Library not found. Run 'sbt engineNative/nativeLink' first.")
    sys.exit(1)

lib = ctypes.CDLL(lib_path)

# ── Define C function signatures ────────────────────────────────────

lib.petradb_open.restype = ctypes.c_int
lib.petradb_close.argtypes = [ctypes.c_int]
lib.petradb_close.restype = ctypes.c_int
lib.petradb_connect.argtypes = [ctypes.c_int]
lib.petradb_connect.restype = ctypes.c_int
lib.petradb_exec.argtypes = [ctypes.c_int, ctypes.c_char_p]
lib.petradb_exec.restype = ctypes.c_int
lib.petradb_prepare.argtypes = [ctypes.c_int, ctypes.c_char_p]
lib.petradb_prepare.restype = ctypes.c_int
lib.petradb_step.argtypes = [ctypes.c_int]
lib.petradb_step.restype = ctypes.c_int
lib.petradb_finalize.argtypes = [ctypes.c_int]
lib.petradb_finalize.restype = ctypes.c_int
lib.petradb_column_count.argtypes = [ctypes.c_int]
lib.petradb_column_count.restype = ctypes.c_int
lib.petradb_column_name.argtypes = [ctypes.c_int, ctypes.c_int]
lib.petradb_column_name.restype = ctypes.c_char_p
lib.petradb_column_int.argtypes = [ctypes.c_int, ctypes.c_int]
lib.petradb_column_int.restype = ctypes.c_int
lib.petradb_column_double.argtypes = [ctypes.c_int, ctypes.c_int]
lib.petradb_column_double.restype = ctypes.c_double
lib.petradb_column_text.argtypes = [ctypes.c_int, ctypes.c_int]
lib.petradb_column_text.restype = ctypes.c_char_p
lib.petradb_column_is_null.argtypes = [ctypes.c_int, ctypes.c_int]
lib.petradb_column_is_null.restype = ctypes.c_int
lib.petradb_errmsg.restype = ctypes.c_char_p

# ── Test helpers ────────────────────────────────────────────────────

tests = 0
passed = 0

def check(condition, msg):
    global tests, passed
    tests += 1
    if condition:
        passed += 1
    else:
        print(f"  FAIL: {msg}")

def sql(s):
    return s.encode('utf-8')

# ── Tests ───────────────────────────────────────────────────────────

print("PetraDB Python FFI Tests")
print("========================\n")

# Lifecycle
print("test_lifecycle")
db = lib.petradb_open()
check(db > 0, "open returns valid handle")
conn = lib.petradb_connect(db)
check(conn > 0, "connect returns valid handle")

# DDL + DML
print("test_ddl_dml")
rc = lib.petradb_exec(conn, sql("CREATE TABLE users (id SERIAL PRIMARY KEY, name TEXT NOT NULL, age INT)"))
check(rc >= 0, "CREATE TABLE")
lib.petradb_exec(conn, sql("INSERT INTO users (name, age) VALUES ('Alice', 30)"))
lib.petradb_exec(conn, sql("INSERT INTO users (name, age) VALUES ('Bob', 25)"))
lib.petradb_exec(conn, sql("INSERT INTO users (name, age) VALUES ('Carol', 35)"))

# Query
print("test_query")
cur = lib.petradb_prepare(conn, sql("SELECT name, age FROM users ORDER BY name"))
check(cur > 0, "prepare returns valid handle")
check(lib.petradb_column_count(cur) == 2, "column count is 2")
check(lib.petradb_column_name(cur, 0) == b"name", "column 0 is 'name'")
check(lib.petradb_column_name(cur, 1) == b"age", "column 1 is 'age'")

rc = lib.petradb_step(cur)
check(rc == 1, "step returns ROW")
check(lib.petradb_column_text(cur, 0) == b"Alice", "row 1 name")
check(lib.petradb_column_int(cur, 1) == 30, "row 1 age")

rc = lib.petradb_step(cur)
check(rc == 1, "step returns ROW")
check(lib.petradb_column_text(cur, 0) == b"Bob", "row 2 name")
check(lib.petradb_column_int(cur, 1) == 25, "row 2 age")

rc = lib.petradb_step(cur)
check(rc == 1, "step returns ROW")
check(lib.petradb_column_text(cur, 0) == b"Carol", "row 3 name")

rc = lib.petradb_step(cur)
check(rc == 0, "step returns DONE after last row")
lib.petradb_finalize(cur)

# NULL handling
print("test_null")
lib.petradb_exec(conn, sql("CREATE TABLE t (val INT)"))
lib.petradb_exec(conn, sql("INSERT INTO t VALUES (NULL)"))
cur = lib.petradb_prepare(conn, sql("SELECT val FROM t"))
lib.petradb_step(cur)
check(lib.petradb_column_is_null(cur, 0) == 1, "NULL detected")
check(lib.petradb_column_int(cur, 0) == 0, "NULL int is 0")
check(lib.petradb_column_text(cur, 0) is None, "NULL text is None")
lib.petradb_finalize(cur)

# Aggregates
print("test_aggregates")
cur = lib.petradb_prepare(conn, sql("SELECT COUNT(*) AS cnt, SUM(age) AS total FROM users"))
lib.petradb_step(cur)
check(lib.petradb_column_int(cur, 0) == 3, "count is 3")
check(lib.petradb_column_int(cur, 1) == 90, "sum is 90")
lib.petradb_finalize(cur)

# Update + Delete
print("test_update_delete")
rc = lib.petradb_exec(conn, sql("UPDATE users SET age = 31 WHERE name = 'Alice'"))
check(rc == 1, "update affects 1 row")
rc = lib.petradb_exec(conn, sql("DELETE FROM users WHERE name = 'Bob'"))
check(rc == 1, "delete affects 1 row")

cur = lib.petradb_prepare(conn, sql("SELECT name, age FROM users ORDER BY name"))
lib.petradb_step(cur)
check(lib.petradb_column_text(cur, 0) == b"Alice", "Alice remains")
check(lib.petradb_column_int(cur, 1) == 31, "Alice age updated")
lib.petradb_step(cur)
check(lib.petradb_column_text(cur, 0) == b"Carol", "Carol remains")
rc = lib.petradb_step(cur)
check(rc == 0, "only 2 rows remain")
lib.petradb_finalize(cur)

# Error handling
print("test_errors")
rc = lib.petradb_exec(conn, sql("INVALID SQL"))
check(rc == -1, "invalid SQL returns -1")
errmsg = lib.petradb_errmsg()
check(len(errmsg) > 0, "error message is non-empty")
lib.petradb_exec(conn, sql("SELECT 1"))
errmsg = lib.petradb_errmsg()
check(errmsg == b"", "error cleared after success")

# Join
print("test_join")
lib.petradb_exec(conn, sql("CREATE TABLE depts (id SERIAL PRIMARY KEY, name TEXT NOT NULL)"))
lib.petradb_exec(conn, sql("CREATE TABLE emps (id SERIAL PRIMARY KEY, name TEXT NOT NULL, dept_id INT)"))
lib.petradb_exec(conn, sql("INSERT INTO depts (name) VALUES ('Engineering')"))
lib.petradb_exec(conn, sql("INSERT INTO depts (name) VALUES ('Marketing')"))
lib.petradb_exec(conn, sql("INSERT INTO emps (name, dept_id) VALUES ('Dave', 1)"))
lib.petradb_exec(conn, sql("INSERT INTO emps (name, dept_id) VALUES ('Eve', 2)"))

cur = lib.petradb_prepare(conn, sql(
    "SELECT e.name, d.name AS dept FROM emps e "
    "INNER JOIN depts d ON e.dept_id = d.id ORDER BY e.name"
))
lib.petradb_step(cur)
check(lib.petradb_column_text(cur, 0) == b"Dave", "join row 1 name")
check(lib.petradb_column_text(cur, 1) == b"Engineering", "join row 1 dept")
lib.petradb_step(cur)
check(lib.petradb_column_text(cur, 0) == b"Eve", "join row 2 name")
check(lib.petradb_column_text(cur, 1) == b"Marketing", "join row 2 dept")
lib.petradb_finalize(cur)

# Cleanup
lib.petradb_close(db)

# Results
print(f"\n========================")
print(f"{passed}/{tests} tests passed")
sys.exit(0 if passed == tests else 1)
