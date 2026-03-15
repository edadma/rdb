use std::ffi::{CStr, CString};
use std::os::raw::{c_char, c_int, c_double, c_longlong};

unsafe extern "C" {
    fn petradb_open() -> c_int;
    fn petradb_close(db: c_int) -> c_int;
    fn petradb_connect(db: c_int) -> c_int;
    fn petradb_exec(conn: c_int, sql: *const c_char) -> c_int;
    fn petradb_prepare(conn: c_int, sql: *const c_char) -> c_int;
    fn petradb_step(cursor: c_int) -> c_int;
    fn petradb_finalize(cursor: c_int) -> c_int;
    fn petradb_column_count(cursor: c_int) -> c_int;
    fn petradb_column_name(cursor: c_int, index: c_int) -> *const c_char;
    fn petradb_column_int(cursor: c_int, index: c_int) -> c_int;
    fn petradb_column_int64(cursor: c_int, index: c_int) -> c_longlong;
    fn petradb_column_double(cursor: c_int, index: c_int) -> c_double;
    fn petradb_column_text(cursor: c_int, index: c_int) -> *const c_char;
    fn petradb_column_is_null(cursor: c_int, index: c_int) -> c_int;
    fn petradb_column_bytes(cursor: c_int, index: c_int) -> c_int;
    fn petradb_errmsg() -> *const c_char;
}

fn sql(s: &str) -> CString {
    CString::new(s).unwrap()
}

fn get_text(cursor: c_int, col: c_int) -> String {
    unsafe {
        let ptr = petradb_column_text(cursor, col);
        if ptr.is_null() {
            String::from("NULL")
        } else {
            CStr::from_ptr(ptr).to_string_lossy().into_owned()
        }
    }
}

fn get_col_name(cursor: c_int, col: c_int) -> String {
    unsafe {
        CStr::from_ptr(petradb_column_name(cursor, col))
            .to_str().unwrap().to_owned()
    }
}

fn get_errmsg() -> String {
    unsafe {
        CStr::from_ptr(petradb_errmsg()).to_string_lossy().into_owned()
    }
}

use std::sync::atomic::{AtomicU32, Ordering};

static TESTS: AtomicU32 = AtomicU32::new(0);
static FAILURES: AtomicU32 = AtomicU32::new(0);

macro_rules! check {
    ($actual:expr, $expected:expr, $msg:expr) => {{
        let a = $actual;
        let e = $expected;
        TESTS.fetch_add(1, Ordering::SeqCst);
        if a != e {
            eprintln!("  FAIL: {} — expected {:?}, got {:?}", $msg, e, a);
            FAILURES.fetch_add(1, Ordering::SeqCst);
        }
    }};
}

fn main() {
    println!("PetraDB Rust FFI Tests");
    println!("======================\n");

    test_lifecycle();
    test_create_insert_query();
    test_types_and_null();
    test_aggregates();
    test_join();
    test_error_handling();

    let total = TESTS.load(Ordering::SeqCst);
    let failed = FAILURES.load(Ordering::SeqCst);
    let passed = total - failed;
    println!("\n======================");
    println!("{}/{} tests passed", passed, total);
    if failed > 0 {
        std::process::exit(1);
    }
}

fn test_lifecycle() {
    println!("test_lifecycle");
    unsafe {
        let db = petradb_open();
        check!(db > 0, true, "open returns valid handle");

        let conn = petradb_connect(db);
        check!(conn > 0, true, "connect returns valid handle");

        let rc = petradb_close(db);
        check!(rc, 0, "close returns 0");
    }
}

fn test_create_insert_query() {
    println!("test_create_insert_query");
    unsafe {
        let db = petradb_open();
        let conn = petradb_connect(db);

        let rc = petradb_exec(conn, sql("CREATE TABLE products (id SERIAL PRIMARY KEY, name TEXT NOT NULL, price NUMERIC, in_stock BOOLEAN DEFAULT true)").as_ptr());
        check!(rc >= 0, true, "CREATE TABLE succeeds");

        petradb_exec(conn, sql("INSERT INTO products (name, price) VALUES ('Widget', 9.99)").as_ptr());
        petradb_exec(conn, sql("INSERT INTO products (name, price) VALUES ('Gadget', 24.95)").as_ptr());
        petradb_exec(conn, sql("INSERT INTO products (name, price, in_stock) VALUES ('Doohickey', 4.50, false)").as_ptr());

        let cur = petradb_prepare(conn, sql("SELECT id, name, price, in_stock FROM products ORDER BY id").as_ptr());
        check!(petradb_column_count(cur), 4, "column count is 4");
        check!(get_col_name(cur, 0), "id", "column 0 name");
        check!(get_col_name(cur, 1), "name", "column 1 name");

        // Row 1: Widget
        check!(petradb_step(cur), 1, "step row 1");
        check!(petradb_column_int(cur, 0), 1, "row 1 id");
        check!(get_text(cur, 1), "Widget", "row 1 name");
        check!(petradb_column_int(cur, 3), 1, "row 1 in_stock is true");

        // Row 2: Gadget
        check!(petradb_step(cur), 1, "step row 2");
        check!(get_text(cur, 1), "Gadget", "row 2 name");

        // Row 3: Doohickey
        check!(petradb_step(cur), 1, "step row 3");
        check!(get_text(cur, 1), "Doohickey", "row 3 name");
        check!(petradb_column_int(cur, 3), 0, "row 3 in_stock is false");

        // Done
        check!(petradb_step(cur), 0, "step returns DONE");

        petradb_finalize(cur);
        petradb_close(db);
    }
}

fn test_types_and_null() {
    println!("test_types_and_null");
    unsafe {
        let db = petradb_open();
        let conn = petradb_connect(db);

        petradb_exec(conn, sql("CREATE TABLE t (i INT, d DOUBLE, s TEXT, n INT)").as_ptr());
        petradb_exec(conn, sql("INSERT INTO t VALUES (42, 3.14, 'hello', NULL)").as_ptr());

        let cur = petradb_prepare(conn, sql("SELECT i, d, s, n FROM t").as_ptr());
        petradb_step(cur);

        check!(petradb_column_int(cur, 0), 42, "int value");
        check!(petradb_column_int64(cur, 0), 42, "int64 value");

        let d = petradb_column_double(cur, 1);
        check!((d - 3.14).abs() < 0.001, true, "double value ~3.14");

        check!(get_text(cur, 2), "hello", "text value");
        check!(petradb_column_bytes(cur, 2), 5, "text bytes");

        check!(petradb_column_is_null(cur, 3), 1, "column is null");
        check!(petradb_column_int(cur, 3), 0, "null int is 0");
        check!(petradb_column_text(cur, 3).is_null(), true, "null text is null ptr");

        petradb_finalize(cur);
        petradb_close(db);
    }
}

fn test_aggregates() {
    println!("test_aggregates");
    unsafe {
        let db = petradb_open();
        let conn = petradb_connect(db);

        petradb_exec(conn, sql("CREATE TABLE scores (name TEXT, score INT)").as_ptr());
        petradb_exec(conn, sql("INSERT INTO scores VALUES ('Alice', 90)").as_ptr());
        petradb_exec(conn, sql("INSERT INTO scores VALUES ('Bob', 80)").as_ptr());
        petradb_exec(conn, sql("INSERT INTO scores VALUES ('Carol', 95)").as_ptr());

        let cur = petradb_prepare(conn, sql("SELECT COUNT(*) FROM scores").as_ptr());
        petradb_step(cur);
        check!(petradb_column_int(cur, 0), 3, "count is 3");
        petradb_finalize(cur);

        let cur = petradb_prepare(conn, sql("SELECT SUM(score) FROM scores").as_ptr());
        petradb_step(cur);
        check!(petradb_column_int(cur, 0), 265, "sum is 265");
        petradb_finalize(cur);

        let cur = petradb_prepare(conn, sql("SELECT name FROM scores ORDER BY score DESC LIMIT 1").as_ptr());
        petradb_step(cur);
        check!(get_text(cur, 0), "Carol", "highest scorer is Carol");
        petradb_finalize(cur);

        petradb_close(db);
    }
}

fn test_join() {
    println!("test_join");
    unsafe {
        let db = petradb_open();
        let conn = petradb_connect(db);

        petradb_exec(conn, sql("CREATE TABLE departments (id SERIAL PRIMARY KEY, name TEXT NOT NULL)").as_ptr());
        petradb_exec(conn, sql("CREATE TABLE employees (id SERIAL PRIMARY KEY, name TEXT NOT NULL, dept_id INT NOT NULL)").as_ptr());
        petradb_exec(conn, sql("INSERT INTO departments (name) VALUES ('Engineering')").as_ptr());
        petradb_exec(conn, sql("INSERT INTO departments (name) VALUES ('Marketing')").as_ptr());
        petradb_exec(conn, sql("INSERT INTO employees (name, dept_id) VALUES ('Alice', 1)").as_ptr());
        petradb_exec(conn, sql("INSERT INTO employees (name, dept_id) VALUES ('Bob', 2)").as_ptr());
        petradb_exec(conn, sql("INSERT INTO employees (name, dept_id) VALUES ('Carol', 1)").as_ptr());

        let cur = petradb_prepare(conn, sql(
            "SELECT e.name, d.name AS dept FROM employees e INNER JOIN departments d ON e.dept_id = d.id ORDER BY e.name"
        ).as_ptr());

        petradb_step(cur);
        check!(get_text(cur, 0), "Alice", "join row 1 name");
        check!(get_text(cur, 1), "Engineering", "join row 1 dept");

        petradb_step(cur);
        check!(get_text(cur, 0), "Bob", "join row 2 name");
        check!(get_text(cur, 1), "Marketing", "join row 2 dept");

        petradb_step(cur);
        check!(get_text(cur, 0), "Carol", "join row 3 name");
        check!(get_text(cur, 1), "Engineering", "join row 3 dept");

        check!(petradb_step(cur), 0, "no more rows");

        petradb_finalize(cur);
        petradb_close(db);
    }
}

fn test_error_handling() {
    println!("test_error_handling");
    unsafe {
        let db = petradb_open();
        let conn = petradb_connect(db);

        let rc = petradb_exec(conn, sql("INVALID SQL").as_ptr());
        check!(rc, -1, "invalid SQL returns -1");
        check!(get_errmsg().is_empty(), false, "error message set");

        petradb_exec(conn, sql("CREATE TABLE t (id INT)").as_ptr());
        check!(get_errmsg().is_empty(), true, "error cleared after success");

        petradb_close(db);
    }
}
