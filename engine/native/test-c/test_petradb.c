#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../petradb.h"

static int tests_run = 0;
static int tests_passed = 0;

#define ASSERT(cond, msg) do { \
    tests_run++; \
    if (!(cond)) { \
        printf("  FAIL: %s (line %d)\n", msg, __LINE__); \
        printf("        errmsg: %s\n", petradb_errmsg()); \
    } else { \
        tests_passed++; \
    } \
} while(0)

#define ASSERT_EQ_INT(actual, expected, msg) do { \
    tests_run++; \
    if ((actual) != (expected)) { \
        printf("  FAIL: %s — expected %d, got %d (line %d)\n", msg, (expected), (actual), __LINE__); \
    } else { \
        tests_passed++; \
    } \
} while(0)

#define ASSERT_EQ_STR(actual, expected, msg) do { \
    tests_run++; \
    if ((actual) == NULL || strcmp((actual), (expected)) != 0) { \
        printf("  FAIL: %s — expected \"%s\", got \"%s\" (line %d)\n", \
               msg, (expected), (actual) ? (actual) : "NULL", __LINE__); \
    } else { \
        tests_passed++; \
    } \
} while(0)

void test_open_close(void) {
    printf("test_open_close\n");
    int db = petradb_open();
    ASSERT(db > 0, "petradb_open returns valid handle");

    int result = petradb_close(db);
    ASSERT_EQ_INT(result, 0, "petradb_close returns 0");

    /* Close invalid handle */
    result = petradb_close(9999);
    ASSERT_EQ_INT(result, -1, "close invalid handle returns -1");
}

void test_connect(void) {
    printf("test_connect\n");
    int db = petradb_open();
    int conn = petradb_connect(db);
    ASSERT(conn > 0, "petradb_connect returns valid handle");

    /* Connect to invalid db */
    int bad = petradb_connect(9999);
    ASSERT_EQ_INT(bad, 0, "connect to invalid db returns 0");

    petradb_close(db);
}

void test_exec_ddl(void) {
    printf("test_exec_ddl\n");
    int db = petradb_open();
    int conn = petradb_connect(db);

    int rc = petradb_exec(conn, "CREATE TABLE users (id SERIAL PRIMARY KEY, name TEXT NOT NULL, age INT);");
    ASSERT(rc >= 0, "CREATE TABLE succeeds");

    rc = petradb_exec(conn, "INSERT INTO users (name, age) VALUES ('Alice', 30);");
    ASSERT(rc >= 0, "INSERT succeeds");

    rc = petradb_exec(conn, "INSERT INTO users (name, age) VALUES ('Bob', 25);");
    ASSERT(rc >= 0, "second INSERT succeeds");

    petradb_close(db);
}

void test_cursor_basic(void) {
    printf("test_cursor_basic\n");
    int db = petradb_open();
    int conn = petradb_connect(db);

    petradb_exec(conn, "CREATE TABLE t (id SERIAL PRIMARY KEY, name TEXT NOT NULL, age INT);");
    petradb_exec(conn, "INSERT INTO t (name, age) VALUES ('Alice', 30);");
    petradb_exec(conn, "INSERT INTO t (name, age) VALUES ('Bob', 25);");
    petradb_exec(conn, "INSERT INTO t (name, age) VALUES ('Carol', 35);");

    int cur = petradb_prepare(conn, "SELECT name, age FROM t ORDER BY name");
    ASSERT(cur > 0, "petradb_prepare returns valid handle");

    /* Column metadata */
    ASSERT_EQ_INT(petradb_column_count(cur), 2, "column count is 2");
    ASSERT_EQ_STR(petradb_column_name(cur, 0), "name", "column 0 is 'name'");
    ASSERT_EQ_STR(petradb_column_name(cur, 1), "age", "column 1 is 'age'");

    /* Row 1: Alice, 30 */
    int rc = petradb_step(cur);
    ASSERT_EQ_INT(rc, PETRADB_ROW, "step returns ROW for first row");
    ASSERT_EQ_STR(petradb_column_text(cur, 0), "Alice", "row 1 name is Alice");
    ASSERT_EQ_INT(petradb_column_int(cur, 1), 30, "row 1 age is 30");

    /* Row 2: Bob, 25 */
    rc = petradb_step(cur);
    ASSERT_EQ_INT(rc, PETRADB_ROW, "step returns ROW for second row");
    ASSERT_EQ_STR(petradb_column_text(cur, 0), "Bob", "row 2 name is Bob");
    ASSERT_EQ_INT(petradb_column_int(cur, 1), 25, "row 2 age is 25");

    /* Row 3: Carol, 35 */
    rc = petradb_step(cur);
    ASSERT_EQ_INT(rc, PETRADB_ROW, "step returns ROW for third row");
    ASSERT_EQ_STR(petradb_column_text(cur, 0), "Carol", "row 3 name is Carol");
    ASSERT_EQ_INT(petradb_column_int(cur, 1), 35, "row 3 age is 35");

    /* No more rows */
    rc = petradb_step(cur);
    ASSERT_EQ_INT(rc, PETRADB_DONE, "step returns DONE after last row");

    ASSERT_EQ_INT(petradb_finalize(cur), 0, "finalize returns 0");
    petradb_close(db);
}

void test_null_handling(void) {
    printf("test_null_handling\n");
    int db = petradb_open();
    int conn = petradb_connect(db);

    petradb_exec(conn, "CREATE TABLE t (id INT, val INT);");
    petradb_exec(conn, "INSERT INTO t (id, val) VALUES (1, NULL);");

    int cur = petradb_prepare(conn, "SELECT val FROM t");
    petradb_step(cur);

    ASSERT_EQ_INT(petradb_column_is_null(cur, 0), 1, "NULL column detected");
    ASSERT_EQ_INT(petradb_column_type(cur, 0), PETRADB_NULL, "type is NULL");
    ASSERT_EQ_INT(petradb_column_int(cur, 0), 0, "int value of NULL is 0");
    ASSERT(petradb_column_text(cur, 0) == NULL, "text value of NULL is NULL pointer");
    ASSERT_EQ_INT(petradb_column_bytes(cur, 0), 0, "bytes of NULL is 0");

    petradb_finalize(cur);
    petradb_close(db);
}

void test_column_types(void) {
    printf("test_column_types\n");
    int db = petradb_open();
    int conn = petradb_connect(db);

    petradb_exec(conn, "CREATE TABLE t (i INT, f DOUBLE, s TEXT, b BOOLEAN);");
    petradb_exec(conn, "INSERT INTO t VALUES (42, 3.14, 'hello', true);");

    int cur = petradb_prepare(conn, "SELECT i, f, s, b FROM t");
    petradb_step(cur);

    ASSERT_EQ_INT(petradb_column_type(cur, 0), PETRADB_INTEGER, "int column type");
    ASSERT_EQ_INT(petradb_column_int(cur, 0), 42, "int value");

    ASSERT_EQ_INT(petradb_column_type(cur, 2), PETRADB_TEXT, "text column type");
    ASSERT_EQ_STR(petradb_column_text(cur, 2), "hello", "text value");
    ASSERT_EQ_INT(petradb_column_bytes(cur, 2), 5, "bytes of 'hello' is 5");

    ASSERT_EQ_INT(petradb_column_type(cur, 3), PETRADB_INTEGER, "boolean maps to integer type");
    ASSERT_EQ_INT(petradb_column_int(cur, 3), 1, "true is 1");

    petradb_finalize(cur);
    petradb_close(db);
}

void test_empty_result(void) {
    printf("test_empty_result\n");
    int db = petradb_open();
    int conn = petradb_connect(db);

    petradb_exec(conn, "CREATE TABLE t (id INT);");

    int cur = petradb_prepare(conn, "SELECT * FROM t");
    ASSERT_EQ_INT(petradb_column_count(cur), 1, "column count on empty table");

    int rc = petradb_step(cur);
    ASSERT_EQ_INT(rc, PETRADB_DONE, "step returns DONE immediately on empty result");

    petradb_finalize(cur);
    petradb_close(db);
}

void test_error_reporting(void) {
    printf("test_error_reporting\n");
    int db = petradb_open();
    int conn = petradb_connect(db);

    /* Execute invalid SQL */
    int rc = petradb_exec(conn, "INVALID SQL STATEMENT;");
    ASSERT_EQ_INT(rc, -1, "invalid SQL returns -1");
    ASSERT(strlen(petradb_errmsg()) > 0, "error message is non-empty after failure");

    /* Successful operation clears error */
    petradb_exec(conn, "CREATE TABLE t (id INT);");
    ASSERT_EQ_INT((int)strlen(petradb_errmsg()), 0, "error cleared after success");

    petradb_close(db);
}

void test_multiple_cursors(void) {
    printf("test_multiple_cursors\n");
    int db = petradb_open();
    int conn = petradb_connect(db);

    petradb_exec(conn, "CREATE TABLE t1 (x INT); INSERT INTO t1 VALUES (1); INSERT INTO t1 VALUES (2);");
    petradb_exec(conn, "CREATE TABLE t2 (y INT); INSERT INTO t2 VALUES (10); INSERT INTO t2 VALUES (20);");

    int cur1 = petradb_prepare(conn, "SELECT x FROM t1 ORDER BY x");
    int cur2 = petradb_prepare(conn, "SELECT y FROM t2 ORDER BY y");

    petradb_step(cur1);
    ASSERT_EQ_INT(petradb_column_int(cur1, 0), 1, "cursor 1 first row");

    petradb_step(cur2);
    ASSERT_EQ_INT(petradb_column_int(cur2, 0), 10, "cursor 2 first row");

    petradb_step(cur1);
    ASSERT_EQ_INT(petradb_column_int(cur1, 0), 2, "cursor 1 second row");

    petradb_step(cur2);
    ASSERT_EQ_INT(petradb_column_int(cur2, 0), 20, "cursor 2 second row");

    petradb_finalize(cur1);
    petradb_finalize(cur2);
    petradb_close(db);
}

void test_update_delete(void) {
    printf("test_update_delete\n");
    int db = petradb_open();
    int conn = petradb_connect(db);

    petradb_exec(conn, "CREATE TABLE t (id INT, val TEXT);");
    petradb_exec(conn, "INSERT INTO t VALUES (1, 'a');");
    petradb_exec(conn, "INSERT INTO t VALUES (2, 'b');");
    petradb_exec(conn, "INSERT INTO t VALUES (3, 'c');");

    int rc = petradb_exec(conn, "UPDATE t SET val = 'x' WHERE id = 2;");
    ASSERT_EQ_INT(rc, 1, "update affects 1 row");

    rc = petradb_exec(conn, "DELETE FROM t WHERE id = 3;");
    ASSERT_EQ_INT(rc, 1, "delete affects 1 row");

    /* Verify */
    int cur = petradb_prepare(conn, "SELECT id, val FROM t ORDER BY id");
    petradb_step(cur);
    ASSERT_EQ_INT(petradb_column_int(cur, 0), 1, "row 1 id");
    ASSERT_EQ_STR(petradb_column_text(cur, 1), "a", "row 1 val unchanged");

    petradb_step(cur);
    ASSERT_EQ_INT(petradb_column_int(cur, 0), 2, "row 2 id");
    ASSERT_EQ_STR(petradb_column_text(cur, 1), "x", "row 2 val updated");

    int done = petradb_step(cur);
    ASSERT_EQ_INT(done, PETRADB_DONE, "only 2 rows remain");

    petradb_finalize(cur);
    petradb_close(db);
}

void test_persistent(void) {
    printf("test_persistent\n");
    const char *path = "/tmp/petradb_c_test.db";

    /* Create and populate */
    int db = petradb_open_persistent(path);
    ASSERT(db > 0, "persistent open returns valid handle");
    if (db == 0) {
        printf("  errmsg: %s\n", petradb_errmsg());
        return;
    }

    int conn = petradb_connect(db);
    ASSERT(conn > 0, "persistent connect");

    petradb_exec(conn, "CREATE TABLE t (id SERIAL PRIMARY KEY, name TEXT NOT NULL)");
    petradb_exec(conn, "INSERT INTO t (name) VALUES ('Alice')");
    petradb_exec(conn, "INSERT INTO t (name) VALUES ('Bob')");

    int cur = petradb_prepare(conn, "SELECT name FROM t ORDER BY name");
    petradb_step(cur);
    ASSERT_EQ_STR(petradb_column_text(cur, 0), "Alice", "persistent row 1");
    petradb_step(cur);
    ASSERT_EQ_STR(petradb_column_text(cur, 0), "Bob", "persistent row 2");
    petradb_finalize(cur);
    petradb_close(db);

    /* Reopen and verify */
    int db2 = petradb_open_persistent(path);
    ASSERT(db2 > 0, "persistent reopen");
    int conn2 = petradb_connect(db2);

    cur = petradb_prepare(conn2, "SELECT name FROM t ORDER BY name");
    petradb_step(cur);
    ASSERT_EQ_STR(petradb_column_text(cur, 0), "Alice", "persistent survived reopen row 1");
    petradb_step(cur);
    ASSERT_EQ_STR(petradb_column_text(cur, 0), "Bob", "persistent survived reopen row 2");
    petradb_finalize(cur);
    petradb_close(db2);

    remove(path);
}

void my_double_func(int ctx, int argc, const int* argv) {
    if (argc != 1 || petradb_value_is_null(argv[0])) {
        petradb_result_null(ctx);
        return;
    }
    int val = petradb_value_int(argv[0]);
    petradb_result_int(ctx, val * 2);
}

void my_concat_func(int ctx, int argc, const int* argv) {
    if (argc != 2) {
        petradb_result_error(ctx, "concat requires 2 args");
        return;
    }
    const char *a = petradb_value_text(argv[0]);
    const char *b = petradb_value_text(argv[1]);
    if (a == NULL || b == NULL) { petradb_result_null(ctx); return; }
    char buf[256];
    snprintf(buf, sizeof(buf), "%s%s", a, b);
    petradb_result_text(ctx, buf);
}

static int counter = 0;

void my_counter_func(int ctx, int argc, const int* argv) {
    int *p = (int*)petradb_user_data(ctx);
    (*p)++;
    petradb_result_int(ctx, *p);
}

void test_create_function(void) {
    printf("test_create_function\n");
    int db = petradb_open();

    /* Basic integer function */
    int rc = petradb_create_function(db, "my_double", 1, NULL, my_double_func);
    ASSERT_EQ_INT(rc, 0, "create_function returns 0");

    int conn = petradb_connect(db);
    int cur = petradb_prepare(conn, "SELECT my_double(21) AS val");
    petradb_step(cur);
    ASSERT_EQ_INT(petradb_column_int(cur, 0), 42, "native function returns 42");
    petradb_finalize(cur);

    /* Use in WHERE */
    petradb_exec(conn, "CREATE TABLE t (val INT); INSERT INTO t VALUES (5); INSERT INTO t VALUES (10);");
    cur = petradb_prepare(conn, "SELECT my_double(val) AS d FROM t ORDER BY val");
    petradb_step(cur);
    ASSERT_EQ_INT(petradb_column_int(cur, 0), 10, "native func on col val=5");
    petradb_step(cur);
    ASSERT_EQ_INT(petradb_column_int(cur, 0), 20, "native func on col val=10");
    petradb_finalize(cur);

    /* NULL handling */
    cur = petradb_prepare(conn, "SELECT my_double(NULL) AS val");
    petradb_step(cur);
    ASSERT_EQ_INT(petradb_column_is_null(cur, 0), 1, "native func NULL returns NULL");
    petradb_finalize(cur);

    /* Text function */
    petradb_create_function(db, "my_concat", 2, NULL, my_concat_func);
    cur = petradb_prepare(conn, "SELECT my_concat('Hello, ', 'World!') AS val");
    petradb_step(cur);
    ASSERT_EQ_STR(petradb_column_text(cur, 0), "Hello, World!", "text function result");
    petradb_finalize(cur);

    /* User data */
    counter = 0;
    petradb_create_function(db, "my_counter", 0, &counter, my_counter_func);
    cur = petradb_prepare(conn, "SELECT my_counter() AS c1, my_counter() AS c2, my_counter() AS c3");
    petradb_step(cur);
    ASSERT(counter >= 1, "user_data counter incremented");
    petradb_finalize(cur);

    /* Value type checking */
    petradb_exec(conn, "CREATE TABLE t2 (i INT, s TEXT); INSERT INTO t2 VALUES (42, 'hello');");
    cur = petradb_prepare(conn, "SELECT my_double(i) AS di FROM t2");
    petradb_step(cur);
    ASSERT_EQ_INT(petradb_column_int(cur, 0), 84, "func on int column");
    petradb_finalize(cur);

    petradb_close(db);
}

void test_blob(void) {
    printf("test_blob\n");
    int db = petradb_open();
    int conn = petradb_connect(db);

    petradb_exec(conn, "CREATE TABLE t (id INT, data BYTEA);");
    petradb_exec(conn, "INSERT INTO t VALUES (1, ARRAY[72, 101, 108, 108, 111]);");

    int cur = petradb_prepare(conn, "SELECT data FROM t");
    petradb_step(cur);

    int len = petradb_column_bytes(cur, 0);
    ASSERT_EQ_INT(len, 5, "blob length is 5");

    ASSERT_EQ_INT(petradb_column_type(cur, 0), PETRADB_BLOB, "blob column type");

    const unsigned char *blob = (const unsigned char *)petradb_column_blob(cur, 0);
    ASSERT(blob != NULL, "blob pointer is not NULL");
    ASSERT_EQ_INT(blob[0], 72, "blob byte 0 is 'H'");
    ASSERT_EQ_INT(blob[1], 101, "blob byte 1 is 'e'");
    ASSERT_EQ_INT(blob[4], 111, "blob byte 4 is 'o'");

    /* NULL blob */
    petradb_exec(conn, "INSERT INTO t VALUES (2, NULL);");
    int cur2 = petradb_prepare(conn, "SELECT data FROM t WHERE id = 2");
    petradb_step(cur2);
    ASSERT(petradb_column_blob(cur2, 0) == NULL, "NULL blob returns NULL pointer");
    ASSERT_EQ_INT(petradb_column_bytes(cur2, 0), 0, "NULL blob bytes is 0");

    petradb_finalize(cur);
    petradb_finalize(cur2);
    petradb_close(db);
}

int main(void) {
    printf("PetraDB C API Tests\n");
    printf("====================\n\n");

    test_open_close();
    test_connect();
    test_exec_ddl();
    test_cursor_basic();
    test_null_handling();
    test_column_types();
    test_empty_result();
    test_error_reporting();
    test_multiple_cursors();
    test_update_delete();
    test_blob();
    test_create_function();
    test_persistent();

    printf("\n====================\n");
    printf("%d/%d tests passed\n", tests_passed, tests_run);

    return tests_passed == tests_run ? 0 : 1;
}
