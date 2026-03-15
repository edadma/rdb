"""Low-level ctypes bindings to libpetradb-engine."""

import ctypes
import ctypes.util
import os
import sys

# Type constants (match petradb.h)
PETRADB_INTEGER = 1
PETRADB_FLOAT = 2
PETRADB_TEXT = 3
PETRADB_BLOB = 4
PETRADB_NULL = 5

PETRADB_ROW = 1
PETRADB_DONE = 0
PETRADB_ERROR = -1

_lib = None

def _find_library():
    """Find libpetradb-engine.so/.dylib in standard locations."""
    # 1. PETRADB_LIB_PATH environment variable
    env_path = os.environ.get('PETRADB_LIB_PATH')
    if env_path and os.path.exists(env_path):
        return env_path

    # 2. Next to this package
    pkg_dir = os.path.dirname(os.path.abspath(__file__))
    for name in ('libpetradb-engine.so', 'libpetradb-engine.dylib'):
        p = os.path.join(pkg_dir, name)
        if os.path.exists(p):
            return p

    # 3. Common install locations
    for d in ('/usr/local/lib', '/usr/lib'):
        for name in ('libpetradb-engine.so', 'libpetradb-engine.dylib'):
            p = os.path.join(d, name)
            if os.path.exists(p):
                return p

    # 4. ctypes.util.find_library
    found = ctypes.util.find_library('petradb-engine')
    if found:
        return found

    # 5. Development build location (relative to repo root)
    dev_path = os.path.join(pkg_dir, '..', '..', 'engine', 'native', 'target', 'scala-3.8.2')
    for name in ('libpetradb-engine.so', 'libpetradb-engine.dylib'):
        p = os.path.join(dev_path, name)
        if os.path.exists(p):
            return os.path.abspath(p)

    return None


def _ensure_loaded():
    """Load the shared library and set up all function signatures."""
    global _lib
    if _lib is not None:
        return _lib

    path = _find_library()
    if path is None:
        raise RuntimeError(
            "Could not find libpetradb-engine. Set PETRADB_LIB_PATH or install the library.\n"
            "Build from source: cd petradb && sbt engineNative/nativeLink\n"
            "Or download from: https://github.com/edadma/petradb/releases"
        )

    lib = ctypes.CDLL(path)

    # ── Database lifecycle ──────────────────────────────────────────
    lib.petradb_open.argtypes = []
    lib.petradb_open.restype = ctypes.c_int

    lib.petradb_open_persistent.argtypes = [ctypes.c_char_p]
    lib.petradb_open_persistent.restype = ctypes.c_int

    lib.petradb_close.argtypes = [ctypes.c_int]
    lib.petradb_close.restype = ctypes.c_int

    # ── Connection ──────────────────────────────────────────────────
    lib.petradb_connect.argtypes = [ctypes.c_int]
    lib.petradb_connect.restype = ctypes.c_int

    # ── Execute ─────────────────────────────────────────────────────
    lib.petradb_exec.argtypes = [ctypes.c_int, ctypes.c_char_p]
    lib.petradb_exec.restype = ctypes.c_int

    # ── Cursor ──────────────────────────────────────────────────────
    lib.petradb_prepare.argtypes = [ctypes.c_int, ctypes.c_char_p]
    lib.petradb_prepare.restype = ctypes.c_int

    lib.petradb_step.argtypes = [ctypes.c_int]
    lib.petradb_step.restype = ctypes.c_int

    lib.petradb_finalize.argtypes = [ctypes.c_int]
    lib.petradb_finalize.restype = ctypes.c_int

    # ── Column metadata ─────────────────────────────────────────────
    lib.petradb_column_count.argtypes = [ctypes.c_int]
    lib.petradb_column_count.restype = ctypes.c_int

    lib.petradb_column_name.argtypes = [ctypes.c_int, ctypes.c_int]
    lib.petradb_column_name.restype = ctypes.c_char_p

    # ── Column values ───────────────────────────────────────────────
    lib.petradb_column_type.argtypes = [ctypes.c_int, ctypes.c_int]
    lib.petradb_column_type.restype = ctypes.c_int

    lib.petradb_column_int.argtypes = [ctypes.c_int, ctypes.c_int]
    lib.petradb_column_int.restype = ctypes.c_int

    lib.petradb_column_int64.argtypes = [ctypes.c_int, ctypes.c_int]
    lib.petradb_column_int64.restype = ctypes.c_longlong

    lib.petradb_column_double.argtypes = [ctypes.c_int, ctypes.c_int]
    lib.petradb_column_double.restype = ctypes.c_double

    lib.petradb_column_text.argtypes = [ctypes.c_int, ctypes.c_int]
    lib.petradb_column_text.restype = ctypes.c_char_p

    lib.petradb_column_blob.argtypes = [ctypes.c_int, ctypes.c_int]
    lib.petradb_column_blob.restype = ctypes.c_void_p

    lib.petradb_column_bytes.argtypes = [ctypes.c_int, ctypes.c_int]
    lib.petradb_column_bytes.restype = ctypes.c_int

    lib.petradb_column_is_null.argtypes = [ctypes.c_int, ctypes.c_int]
    lib.petradb_column_is_null.restype = ctypes.c_int

    # ── User-defined functions ──────────────────────────────────────
    FUNC_CALLBACK = ctypes.CFUNCTYPE(None, ctypes.c_int, ctypes.c_int, ctypes.POINTER(ctypes.c_int))

    lib.petradb_create_function.argtypes = [ctypes.c_int, ctypes.c_char_p, ctypes.c_int, ctypes.c_void_p, FUNC_CALLBACK]
    lib.petradb_create_function.restype = ctypes.c_int

    lib.petradb_value_int.argtypes = [ctypes.c_int]
    lib.petradb_value_int.restype = ctypes.c_int

    lib.petradb_value_int64.argtypes = [ctypes.c_int]
    lib.petradb_value_int64.restype = ctypes.c_longlong

    lib.petradb_value_double.argtypes = [ctypes.c_int]
    lib.petradb_value_double.restype = ctypes.c_double

    lib.petradb_value_text.argtypes = [ctypes.c_int]
    lib.petradb_value_text.restype = ctypes.c_char_p

    lib.petradb_value_type.argtypes = [ctypes.c_int]
    lib.petradb_value_type.restype = ctypes.c_int

    lib.petradb_value_is_null.argtypes = [ctypes.c_int]
    lib.petradb_value_is_null.restype = ctypes.c_int

    lib.petradb_result_int.argtypes = [ctypes.c_int, ctypes.c_int]
    lib.petradb_result_int.restype = None

    lib.petradb_result_int64.argtypes = [ctypes.c_int, ctypes.c_longlong]
    lib.petradb_result_int64.restype = None

    lib.petradb_result_double.argtypes = [ctypes.c_int, ctypes.c_double]
    lib.petradb_result_double.restype = None

    lib.petradb_result_text.argtypes = [ctypes.c_int, ctypes.c_char_p]
    lib.petradb_result_text.restype = None

    lib.petradb_result_null.argtypes = [ctypes.c_int]
    lib.petradb_result_null.restype = None

    lib.petradb_result_error.argtypes = [ctypes.c_int, ctypes.c_char_p]
    lib.petradb_result_error.restype = None

    lib.petradb_user_data.argtypes = [ctypes.c_int]
    lib.petradb_user_data.restype = ctypes.c_void_p

    # ── Error reporting ─────────────────────────────────────────────
    lib.petradb_errmsg.argtypes = []
    lib.petradb_errmsg.restype = ctypes.c_char_p

    # Store callback type for use by Database.create_function
    lib._FUNC_CALLBACK = FUNC_CALLBACK

    _lib = lib
    return lib
