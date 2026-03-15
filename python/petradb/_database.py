"""High-level Pythonic API for PetraDB."""

import ctypes
from petradb._binding import (
    _ensure_loaded, PETRADB_ROW, PETRADB_DONE,
    PETRADB_INTEGER, PETRADB_FLOAT, PETRADB_TEXT, PETRADB_BLOB, PETRADB_NULL,
)


class PetraDBError(Exception):
    """Raised when PetraDB returns an error."""
    pass


class Row:
    """A single result row with attribute and index access.

    >>> row.name        # attribute access
    >>> row['name']     # dict-style access
    >>> row[0]          # index access
    """

    __slots__ = ('_values', '_columns')

    def __init__(self, values, columns):
        self._values = values
        self._columns = columns

    def __getattr__(self, name):
        try:
            idx = self._columns.index(name)
            return self._values[idx]
        except ValueError:
            raise AttributeError(f"Row has no column '{name}'")

    def __getitem__(self, key):
        if isinstance(key, int):
            return self._values[key]
        return getattr(self, key)

    def __repr__(self):
        pairs = ', '.join(f'{c}={v!r}' for c, v in zip(self._columns, self._values))
        return f'Row({pairs})'

    def __iter__(self):
        return iter(self._values)

    def __len__(self):
        return len(self._values)

    def keys(self):
        return list(self._columns)

    def values(self):
        return list(self._values)

    def items(self):
        return list(zip(self._columns, self._values))

    def to_dict(self):
        return dict(zip(self._columns, self._values))


class Cursor:
    """Lazy row-by-row iterator over query results.

    >>> cur = db.cursor("SELECT * FROM users")
    >>> for row in cur:
    ...     print(row.name)
    >>> cur.close()
    """

    def __init__(self, lib, handle):
        self._lib = lib
        self._handle = handle
        self._closed = False

        self._col_count = lib.petradb_column_count(handle)
        self._columns = [
            lib.petradb_column_name(handle, i).decode('utf-8')
            for i in range(self._col_count)
        ]

    @property
    def columns(self):
        return list(self._columns)

    @property
    def column_count(self):
        return self._col_count

    def _read_value(self, col):
        if self._lib.petradb_column_is_null(self._handle, col):
            return None
        t = self._lib.petradb_column_type(self._handle, col)
        if t == PETRADB_INTEGER:
            return self._lib.petradb_column_int(self._handle, col)
        elif t == PETRADB_FLOAT:
            return self._lib.petradb_column_double(self._handle, col)
        elif t == PETRADB_TEXT:
            raw = self._lib.petradb_column_text(self._handle, col)
            return raw.decode('utf-8') if raw else None
        elif t == PETRADB_BLOB:
            size = self._lib.petradb_column_bytes(self._handle, col)
            ptr = self._lib.petradb_column_blob(self._handle, col)
            return ctypes.string_at(ptr, size) if ptr else None
        elif t == PETRADB_NULL:
            return None
        else:
            raw = self._lib.petradb_column_text(self._handle, col)
            return raw.decode('utf-8') if raw else None

    def fetchone(self):
        if self._closed:
            raise PetraDBError("Cursor is closed")
        rc = self._lib.petradb_step(self._handle)
        if rc == PETRADB_ROW:
            values = [self._read_value(i) for i in range(self._col_count)]
            return Row(values, self._columns)
        elif rc == PETRADB_DONE:
            return None
        else:
            raise PetraDBError(self._lib.petradb_errmsg().decode('utf-8'))

    def fetchall(self):
        rows = []
        while True:
            row = self.fetchone()
            if row is None:
                break
            rows.append(row)
        return rows

    def close(self):
        if not self._closed:
            self._lib.petradb_finalize(self._handle)
            self._closed = True

    def __iter__(self):
        return self

    def __next__(self):
        row = self.fetchone()
        if row is None:
            raise StopIteration
        return row

    def __enter__(self):
        return self

    def __exit__(self, *args):
        self.close()

    def __del__(self):
        self.close()


class Database:
    """PetraDB database connection.

    >>> db = Database()                          # in-memory
    >>> db = Database('mydata.db')               # persistent
    >>> db.execute("CREATE TABLE t (id INT)")
    >>> rows = db.query("SELECT * FROM t")
    >>> db.close()
    """

    def __init__(self, path=None):
        self._lib = _ensure_loaded()
        self._callbacks = []  # prevent GC of ctypes callbacks

        if path is not None:
            self._db = self._lib.petradb_open_persistent(path.encode('utf-8'))
        else:
            self._db = self._lib.petradb_open()

        if self._db == 0:
            raise PetraDBError(self._lib.petradb_errmsg().decode('utf-8'))

        self._conn = self._lib.petradb_connect(self._db)
        if self._conn == 0:
            self._lib.petradb_close(self._db)
            raise PetraDBError(self._lib.petradb_errmsg().decode('utf-8'))

    def execute(self, sql):
        """Execute SQL that doesn't return rows. Returns affected row count."""
        rc = self._lib.petradb_exec(self._conn, sql.encode('utf-8'))
        if rc < 0:
            raise PetraDBError(self._lib.petradb_errmsg().decode('utf-8'))
        return rc

    def query(self, sql):
        """Execute a SELECT query and return all rows as a list of Row objects."""
        with self.cursor(sql) as cur:
            return cur.fetchall()

    def query_one(self, sql):
        """Execute a SELECT query and return the first row, or None."""
        with self.cursor(sql) as cur:
            return cur.fetchone()

    def cursor(self, sql):
        """Open a cursor for row-by-row iteration."""
        handle = self._lib.petradb_prepare(self._conn, sql.encode('utf-8'))
        if handle == 0:
            raise PetraDBError(self._lib.petradb_errmsg().decode('utf-8'))
        return Cursor(self._lib, handle)

    def create_function(self, name, nargs, func):
        """Register a Python function callable from SQL.

        >>> def my_double(args):
        ...     return args[0] * 2
        >>> db.create_function('my_double', 1, my_double)
        >>> db.query("SELECT my_double(21)")  # [Row(my_double(21)=42)]
        """
        lib = self._lib

        def _callback(ctx, argc, argv):
            try:
                args = []
                for i in range(argc):
                    if lib.petradb_value_is_null(argv[i]):
                        args.append(None)
                    else:
                        t = lib.petradb_value_type(argv[i])
                        if t == PETRADB_INTEGER:
                            args.append(lib.petradb_value_int(argv[i]))
                        elif t == PETRADB_FLOAT:
                            args.append(lib.petradb_value_double(argv[i]))
                        elif t == PETRADB_TEXT:
                            raw = lib.petradb_value_text(argv[i])
                            args.append(raw.decode('utf-8') if raw else None)
                        else:
                            raw = lib.petradb_value_text(argv[i])
                            args.append(raw.decode('utf-8') if raw else None)

                result = func(args)

                if result is None:
                    lib.petradb_result_null(ctx)
                elif isinstance(result, int):
                    lib.petradb_result_int(ctx, result)
                elif isinstance(result, float):
                    lib.petradb_result_double(ctx, result)
                elif isinstance(result, str):
                    lib.petradb_result_text(ctx, result.encode('utf-8'))
                else:
                    lib.petradb_result_text(ctx, str(result).encode('utf-8'))
            except Exception as e:
                lib.petradb_result_error(ctx, str(e).encode('utf-8'))

        cb = lib._FUNC_CALLBACK(_callback)
        self._callbacks.append(cb)  # prevent GC

        rc = lib.petradb_create_function(
            self._db, name.encode('utf-8'), nargs, None, cb
        )
        if rc != 0:
            raise PetraDBError(lib.petradb_errmsg().decode('utf-8'))

    def close(self):
        """Close the database connection."""
        if hasattr(self, '_db') and self._db:
            self._lib.petradb_close(self._db)
            self._db = 0

    def __enter__(self):
        return self

    def __exit__(self, *args):
        self.close()

    def __del__(self):
        self.close()
