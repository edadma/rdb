package main

// #cgo LDFLAGS: -L${SRCDIR}/../target/scala-3.8.2 -lpetradb-engine -Wl,-rpath,${SRCDIR}/../target/scala-3.8.2
// #include "../petradb.h"
import "C"
import (
	"fmt"
	"os"
)

var tests, passed int

func check(condition bool, msg string) {
	tests++
	if condition {
		passed++
	} else {
		fmt.Printf("  FAIL: %s\n", msg)
	}
}

func main() {
	fmt.Println("PetraDB Go FFI Tests")
	fmt.Println("====================\n")

	testLifecycle()
	testCreateInsertQuery()
	testNullHandling()
	testAggregates()
	testJoin()
	testUpdateDelete()
	testPersistent()
	testErrors()

	fmt.Printf("\n====================\n")
	fmt.Printf("%d/%d tests passed\n", passed, tests)
	if passed != tests {
		panic("some tests failed")
	}
}

func testLifecycle() {
	fmt.Println("test_lifecycle")
	db := C.petradb_open()
	check(db > 0, "open returns valid handle")

	conn := C.petradb_connect(db)
	check(conn > 0, "connect returns valid handle")

	rc := C.petradb_close(db)
	check(rc == 0, "close returns 0")
}

func testCreateInsertQuery() {
	fmt.Println("test_create_insert_query")
	db := C.petradb_open()
	conn := C.petradb_connect(db)

	C.petradb_exec(conn, C.CString("CREATE TABLE users (id SERIAL PRIMARY KEY, name TEXT NOT NULL, age INT)"))
	C.petradb_exec(conn, C.CString("INSERT INTO users (name, age) VALUES ('Alice', 30)"))
	C.petradb_exec(conn, C.CString("INSERT INTO users (name, age) VALUES ('Bob', 25)"))
	C.petradb_exec(conn, C.CString("INSERT INTO users (name, age) VALUES ('Carol', 35)"))

	cur := C.petradb_prepare(conn, C.CString("SELECT name, age FROM users ORDER BY name"))
	check(cur > 0, "prepare returns valid handle")
	check(C.petradb_column_count(cur) == 2, "column count is 2")
	check(C.GoString(C.petradb_column_name(cur, 0)) == "name", "column 0 name")

	rc := C.petradb_step(cur)
	check(rc == C.PETRADB_ROW, "step returns ROW")
	check(C.GoString(C.petradb_column_text(cur, 0)) == "Alice", "row 1 name")
	check(C.petradb_column_int(cur, 1) == 30, "row 1 age")

	C.petradb_step(cur)
	check(C.GoString(C.petradb_column_text(cur, 0)) == "Bob", "row 2 name")

	C.petradb_step(cur)
	check(C.GoString(C.petradb_column_text(cur, 0)) == "Carol", "row 3 name")

	rc = C.petradb_step(cur)
	check(rc == C.PETRADB_DONE, "step returns DONE")

	C.petradb_finalize(cur)
	C.petradb_close(db)
}

func testNullHandling() {
	fmt.Println("test_null")
	db := C.petradb_open()
	conn := C.petradb_connect(db)

	C.petradb_exec(conn, C.CString("CREATE TABLE t (val INT)"))
	C.petradb_exec(conn, C.CString("INSERT INTO t VALUES (NULL)"))

	cur := C.petradb_prepare(conn, C.CString("SELECT val FROM t"))
	C.petradb_step(cur)
	check(C.petradb_column_is_null(cur, 0) == 1, "NULL detected")
	check(C.petradb_column_int(cur, 0) == 0, "NULL int is 0")
	check(C.petradb_column_text(cur, 0) == nil, "NULL text is nil")

	C.petradb_finalize(cur)
	C.petradb_close(db)
}

func testAggregates() {
	fmt.Println("test_aggregates")
	db := C.petradb_open()
	conn := C.petradb_connect(db)

	C.petradb_exec(conn, C.CString("CREATE TABLE scores (name TEXT, score INT)"))
	C.petradb_exec(conn, C.CString("INSERT INTO scores VALUES ('Alice', 90), ('Bob', 80), ('Carol', 95)"))

	cur := C.petradb_prepare(conn, C.CString("SELECT COUNT(*) FROM scores"))
	C.petradb_step(cur)
	check(C.petradb_column_int(cur, 0) == 3, "count is 3")
	C.petradb_finalize(cur)

	cur = C.petradb_prepare(conn, C.CString("SELECT SUM(score) FROM scores"))
	C.petradb_step(cur)
	check(C.petradb_column_int(cur, 0) == 265, "sum is 265")
	C.petradb_finalize(cur)

	C.petradb_close(db)
}

func testJoin() {
	fmt.Println("test_join")
	db := C.petradb_open()
	conn := C.petradb_connect(db)

	C.petradb_exec(conn, C.CString("CREATE TABLE depts (id SERIAL PRIMARY KEY, name TEXT NOT NULL)"))
	C.petradb_exec(conn, C.CString("CREATE TABLE emps (id SERIAL PRIMARY KEY, name TEXT NOT NULL, dept_id INT)"))
	C.petradb_exec(conn, C.CString("INSERT INTO depts (name) VALUES ('Engineering')"))
	C.petradb_exec(conn, C.CString("INSERT INTO depts (name) VALUES ('Marketing')"))
	C.petradb_exec(conn, C.CString("INSERT INTO emps (name, dept_id) VALUES ('Alice', 1)"))
	C.petradb_exec(conn, C.CString("INSERT INTO emps (name, dept_id) VALUES ('Bob', 2)"))

	cur := C.petradb_prepare(conn, C.CString("SELECT e.name, d.name AS dept FROM emps e INNER JOIN depts d ON e.dept_id = d.id ORDER BY e.name"))
	C.petradb_step(cur)
	check(C.GoString(C.petradb_column_text(cur, 0)) == "Alice", "join row 1 name")
	check(C.GoString(C.petradb_column_text(cur, 1)) == "Engineering", "join row 1 dept")
	C.petradb_step(cur)
	check(C.GoString(C.petradb_column_text(cur, 0)) == "Bob", "join row 2 name")
	check(C.GoString(C.petradb_column_text(cur, 1)) == "Marketing", "join row 2 dept")

	C.petradb_finalize(cur)
	C.petradb_close(db)
}

func testUpdateDelete() {
	fmt.Println("test_update_delete")
	db := C.petradb_open()
	conn := C.petradb_connect(db)

	C.petradb_exec(conn, C.CString("CREATE TABLE t (id INT, val TEXT)"))
	C.petradb_exec(conn, C.CString("INSERT INTO t VALUES (1, 'a'), (2, 'b'), (3, 'c')"))

	rc := C.petradb_exec(conn, C.CString("UPDATE t SET val = 'x' WHERE id = 2"))
	check(rc == 1, "update affects 1 row")

	rc = C.petradb_exec(conn, C.CString("DELETE FROM t WHERE id = 3"))
	check(rc == 1, "delete affects 1 row")

	cur := C.petradb_prepare(conn, C.CString("SELECT id, val FROM t ORDER BY id"))
	C.petradb_step(cur)
	check(C.petradb_column_int(cur, 0) == 1, "row 1 id")
	C.petradb_step(cur)
	check(C.GoString(C.petradb_column_text(cur, 1)) == "x", "row 2 updated")
	rc2 := C.petradb_step(cur)
	check(rc2 == C.PETRADB_DONE, "only 2 rows remain")

	C.petradb_finalize(cur)
	C.petradb_close(db)
}

func testPersistent() {
	fmt.Println("test_persistent")
	path := C.CString("/tmp/petradb_go_test.db")

	db := C.petradb_open_persistent(path)
	check(db > 0, "persistent open")
	conn := C.petradb_connect(db)
	C.petradb_exec(conn, C.CString("CREATE TABLE t (name TEXT)"))
	C.petradb_exec(conn, C.CString("INSERT INTO t VALUES ('hello')"))
	C.petradb_close(db)

	db2 := C.petradb_open_persistent(path)
	check(db2 > 0, "persistent reopen")
	conn2 := C.petradb_connect(db2)
	cur := C.petradb_prepare(conn2, C.CString("SELECT name FROM t"))
	C.petradb_step(cur)
	check(C.GoString(C.petradb_column_text(cur, 0)) == "hello", "data survived reopen")
	C.petradb_finalize(cur)
	C.petradb_close(db2)

	// cleanup
	os.Remove("/tmp/petradb_go_test.db")
}

func testErrors() {
	fmt.Println("test_errors")
	db := C.petradb_open()
	conn := C.petradb_connect(db)

	rc := C.petradb_exec(conn, C.CString("INVALID SQL"))
	check(rc == -1, "invalid SQL returns -1")
	check(C.GoString(C.petradb_errmsg()) != "", "error message non-empty")

	C.petradb_exec(conn, C.CString("CREATE TABLE t (id INT)"))
	check(C.GoString(C.petradb_errmsg()) == "", "error cleared after success")

	C.petradb_close(db)
}
