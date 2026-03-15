/**
 * PetraDB C API
 *
 * SQLite-style interface for the PetraDB embedded SQL database engine.
 *
 * Usage:
 *   int db = petradb_open();
 *   int conn = petradb_connect(db);
 *   petradb_exec(conn, "CREATE TABLE users (id SERIAL PRIMARY KEY, name TEXT)");
 *   petradb_exec(conn, "INSERT INTO users (name) VALUES ('Alice')");
 *
 *   int cur = petradb_prepare(conn, "SELECT id, name FROM users");
 *   while (petradb_step(cur) == 1) {
 *       int id = petradb_column_int(cur, 0);
 *       const char *name = petradb_column_text(cur, 1);
 *       printf("id=%d name=%s\n", id, name);
 *   }
 *   petradb_finalize(cur);
 *   petradb_close(db);
 */

#ifndef PETRADB_H
#define PETRADB_H

#ifdef __cplusplus
extern "C" {
#endif

/* Column type constants (matches SQLite) */
#define PETRADB_INTEGER  1
#define PETRADB_FLOAT    2
#define PETRADB_TEXT     3
#define PETRADB_BLOB     4
#define PETRADB_NULL     5

/* Step return values */
#define PETRADB_ROW      1
#define PETRADB_DONE     0
#define PETRADB_ERROR   -1

/* ── Database lifecycle ─────────────────────────────────────────── */

/** Open a new in-memory database. Returns handle, or 0 on error. */
int petradb_open(void);

/** Open a persistent database at path. Returns handle, or 0 on error. */
int petradb_open_persistent(const char *path);

/** Close a database. Returns 0 on success, -1 on error. */
int petradb_close(int db);

/* ── User-defined functions ──────────────────────────────────────── */

/** Callback type for user-defined SQL functions.
  * argc: number of arguments
  * argv: array of null-terminated strings (NULL values are null pointers)
  * result: buffer to write the result string into (null-terminated)
  * result_size: size of result buffer
  * Return 0 for success, -1 for error.
  */
typedef int (*petradb_func_callback)(int argc, const char** argv, char* result, int result_size);

/** Register a native function callable from SQL, triggers, and procedures. */
int petradb_create_function(int db, const char* name, petradb_func_callback fn);

/* ── Connection ─────────────────────────────────────────────────── */

/** Create a session (connection) to a database. Returns handle, or 0 on error. */
int petradb_connect(int db);

/* ── Execute ────────────────────────────────────────────────────── */

/** Execute SQL that doesn't return rows. Returns affected row count, or -1 on error. */
int petradb_exec(int conn, const char *sql);

/* ── Cursor (query) ─────────────────────────────────────────────── */

/** Prepare a SELECT query. Returns cursor handle, or 0 on error. */
int petradb_prepare(int conn, const char *sql);

/** Advance to next row. Returns PETRADB_ROW (1), PETRADB_DONE (0), or PETRADB_ERROR (-1). */
int petradb_step(int cursor);

/** Close a cursor. Returns 0 on success, -1 on error. */
int petradb_finalize(int cursor);

/* ── Column metadata ────────────────────────────────────────────── */

/** Number of columns in result set. Returns -1 on error. */
int petradb_column_count(int cursor);

/** Column name at index. Do NOT free. Returns NULL on error. */
const char *petradb_column_name(int cursor, int index);

/* ── Column values ──────────────────────────────────────────────── */

/** Column type at index. Returns PETRADB_INTEGER/FLOAT/TEXT/BLOB/NULL. */
int petradb_column_type(int cursor, int index);

/** Column value as int. Returns 0 for NULL. */
int petradb_column_int(int cursor, int index);

/** Column value as int64. Returns 0 for NULL. */
long long petradb_column_int64(int cursor, int index);

/** Column value as double. Returns 0.0 for NULL. */
double petradb_column_double(int cursor, int index);

/** Column value as text. Do NOT free. Returns NULL for SQL NULL. */
const char *petradb_column_text(int cursor, int index);

/** Column value as blob. Do NOT free. Returns NULL for SQL NULL.
  * Use petradb_column_bytes() for the length. */
const void *petradb_column_blob(int cursor, int index);

/** Byte length of column value as text/blob. Returns 0 for NULL. */
int petradb_column_bytes(int cursor, int index);

/** Returns 1 if column is NULL, 0 if not, -1 on error. */
int petradb_column_is_null(int cursor, int index);

/* ── Error reporting ────────────────────────────────────────────── */

/** Last error message. Do NOT free. Empty string if no error. */
const char *petradb_errmsg(void);

#ifdef __cplusplus
}
#endif

#endif /* PETRADB_H */
