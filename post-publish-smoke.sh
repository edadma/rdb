#!/bin/bash
# post-publish-smoke.sh
# Smoke-tests the latest published PetraDB packages from npm and Maven Central.
# Run this after a publish to verify the released artifacts actually work.
#
# Requires: node, npm, sbt, curl
# Usage: ./post-publish-smoke.sh [--port PORT]

set -uo pipefail   # NOT -e: test failures must not abort the script

# ── Configuration ──────────────────────────────────────────────────────────────

PORT=15480
SERVER_PID=""

while [[ $# -gt 0 ]]; do
  case "$1" in
    --port) PORT="$2"; shift 2 ;;
    *) echo "Unknown option: $1"; exit 1 ;;
  esac
done

# Resolve published versions from local package.json files
ENGINE_NPM_VERSION=$(node -p "require('./engine/npm/package.json').version")
CLIENT_NPM_VERSION=$(node -p "require('./client/npm/package.json').version")
SERVER_NPM_VERSION=$(node -p "require('./server/npm/package.json').version")
CLI_NPM_VERSION=$(node -p "require('./cli/npm/package.json').version")

# Parse build.sbt for Scala versions
ENGINE_SCALA_VERSION=$(awk '/name.*:=.*"petradb-engine"/{found=1} found && /version.*:=/{split($0, a, "\""); print a[2]; exit}' build.sbt)
CLIENT_SCALA_VERSION=$(awk '/name.*:=.*"petradb-client"/{found=1} found && /version.*:=/{split($0, a, "\""); print a[2]; exit}' build.sbt)
JDBC_SCALA_VERSION=$(awk '/name.*:=.*"petradb-jdbc"/{found=1} found && /version.*:=/{split($0, a, "\""); print a[2]; exit}' build.sbt)
SCALA_FULL_VERSION=$(awk '/ThisBuild.*scalaVersion/{split($0, a, "\""); print a[2]; exit}' build.sbt)

# ── Helpers ─────────────────────────────────────────────────────────────────────

RED='\033[0;31m'; GREEN='\033[0;32m'; YELLOW='\033[1;33m'; CYAN='\033[0;36m'; RESET='\033[0m'
PASS=0; FAIL=0

header() { echo -e "\n${CYAN}══ $* ══${RESET}"; }
pass()   { echo -e "  ${GREEN}✓ PASS${RESET}  $*"; ((PASS++)) || true; }
fail()   { echo -e "  ${RED}✗ FAIL${RESET}  $*"; ((FAIL++)) || true; }
info()   { echo -e "  ${YELLOW}·${RESET} $*"; }

check_output() {
  local label="$1"
  local expected="$2"
  local actual="$3"
  if echo "$actual" | grep -qF "$expected"; then
    pass "$label"
  else
    fail "$label"
    echo -e "    ${YELLOW}expected:${RESET} $expected"
    echo -e "    ${YELLOW}got:${RESET}"
    echo "$actual" | sed 's/^/      /'
  fi
}

run_node() {
  # Run node; capture stdout+stderr; never abort the parent script on failure
  node "$@" 2>&1 || true
}

kill_server() {
  if [[ -n "$SERVER_PID" ]]; then
    kill "$SERVER_PID" 2>/dev/null || true
    SERVER_PID=""
  fi
}

WORK=$(mktemp -d)
trap "kill_server; rm -rf '$WORK'" EXIT

echo -e "${CYAN}"
echo "╔══════════════════════════════════════════════════╗"
echo "║       PetraDB Post-Publish Smoke Tests           ║"
echo "╚══════════════════════════════════════════════════╝"
echo -e "${RESET}"
info "npm engine  : @petradb/engine@${ENGINE_NPM_VERSION}"
info "npm client  : @petradb/client@${CLIENT_NPM_VERSION}"
info "npm server  : @petradb/server@${SERVER_NPM_VERSION}"
info "npm cli     : @petradb/cli@${CLI_NPM_VERSION}"
info "npm quarry  : @petradb/quarry@$(node -p "require('./quarry/package.json').version")"
info "scala engine: io.github.edadma:petradb-engine:${ENGINE_SCALA_VERSION}"
info "scala client: io.github.edadma:petradb-client:${CLIENT_SCALA_VERSION}"
info "scala jdbc  : io.github.edadma:petradb-jdbc:${JDBC_SCALA_VERSION}"
info "scala ver   : ${SCALA_FULL_VERSION}"
info "server port : ${PORT}"
echo ""
info "⚠  Allow a few minutes after publishing for packages to propagate."

# ── 1. npm: Embedded engine ─────────────────────────────────────────────────────

header "JS/TS: Embedded engine (@petradb/engine@${ENGINE_NPM_VERSION})"

ENGINE_DIR="$WORK/js-engine"
mkdir -p "$ENGINE_DIR"

cat > "$ENGINE_DIR/package.json" <<JSON
{
  "name": "petra-smoke-engine",
  "version": "0.0.0",
  "private": true,
  "type": "module",
  "dependencies": {
    "@petradb/engine": "${ENGINE_NPM_VERSION}"
  }
}
JSON

info "Installing @petradb/engine@${ENGINE_NPM_VERSION} from npm..."
(cd "$ENGINE_DIR" && npm install --prefer-online --no-audit --quiet) || { fail "npm install @petradb/engine"; }

cat > "$ENGINE_DIR/smoke.mjs" <<'JS'
import { Session } from '@petradb/engine';

const db = new Session();

await db.execute(`
  CREATE TABLE users (id SERIAL, name TEXT NOT NULL, score INT);
  INSERT INTO users (name, score) VALUES ('Alice', 90), ('Bob', 75), ('Carol', 85);
`);

// COUNT
const [r1] = await db.execute('SELECT COUNT(*) AS n FROM users');
if (Number(r1.rows[0].n) !== 3) throw new Error(`Expected 3 rows, got ${r1.rows[0].n}`);

// View
await db.execute("CREATE VIEW top_users AS SELECT name FROM users WHERE score >= 85 ORDER BY name");
const [r2] = await db.execute('SELECT * FROM top_users');
if (r2.rows.length !== 2) throw new Error(`Expected 2 rows from view, got ${r2.rows.length}`);
if (r2.rows[0].name !== 'Alice') throw new Error(`Expected Alice first, got ${r2.rows[0].name}`);

// Upsert DO NOTHING
await db.execute(`INSERT INTO users (name, score) VALUES ('Alice', 100) ON CONFLICT DO NOTHING`);
const [r3] = await db.execute("SELECT score FROM users WHERE name = 'Alice'");
if (Number(r3.rows[0].score) !== 90) throw new Error('ON CONFLICT DO NOTHING should have preserved original score');

// Temp table
await db.execute(`
  CREATE TEMP TABLE staging (val INT);
  INSERT INTO staging VALUES (1), (2), (3);
`);
const [r4] = await db.execute('SELECT SUM(val) AS total FROM staging');
if (Number(r4.rows[0].total) !== 6) throw new Error(`Expected sum 6, got ${r4.rows[0].total}`);

console.log('OK');
JS

OUT=$(cd "$ENGINE_DIR" && run_node smoke.mjs)
check_output "DDL + DML + view + upsert + temp table" "OK" "$OUT"

cat > "$ENGINE_DIR/smoke2.mjs" <<'JS'
import { Session } from '@petradb/engine';
const db = new Session();
await db.execute("CREATE TABLE t (id SERIAL, v TEXT)");
await db.execute("INSERT INTO t (v) VALUES ('hello')");
const [r] = await db.execute("SELECT * FROM t", { rowMode: "array" });
if (r.rows[0][1] !== 'hello') throw new Error(`rowMode:array failed, got: ${JSON.stringify(r.rows[0])}`);
console.log('OK');
JS

check_output "rowMode: array" "OK" "$(cd "$ENGINE_DIR" && run_node smoke2.mjs)"


# ── 2. npm: Server ──────────────────────────────────────────────────────────────

header "Server (@petradb/server@${SERVER_NPM_VERSION})"

SERVER_DIR="$WORK/server"
mkdir -p "$SERVER_DIR"

cat > "$SERVER_DIR/package.json" <<JSON
{
  "name": "petra-smoke-server",
  "version": "0.0.0",
  "private": true,
  "dependencies": {
    "@petradb/server": "${SERVER_NPM_VERSION}"
  }
}
JSON

info "Installing @petradb/server@${SERVER_NPM_VERSION} from npm..."
(cd "$SERVER_DIR" && npm install --prefer-online --no-audit --quiet) || { fail "npm install @petradb/server"; }

SERVER_BIN="$SERVER_DIR/node_modules/.bin/petradb-server"

if [[ ! -x "$SERVER_BIN" ]]; then
  fail "petradb-server binary not found at $SERVER_BIN"
else
  info "Starting server on port ${PORT}..."
  "$SERVER_BIN" -m -p "$PORT" &
  SERVER_PID=$!
  sleep 2

  if kill -0 "$SERVER_PID" 2>/dev/null; then
    pass "Server started (PID $SERVER_PID)"
  else
    fail "Server failed to start"
    SERVER_PID=""
  fi

  if [[ -n "$SERVER_PID" ]]; then
    HTTP_BODY=$(curl -s "http://localhost:${PORT}/sql" \
      -X POST -H 'Content-Type: application/json' \
      -d '{"sql":"SELECT 1 AS n"}' 2>&1 || true)
    check_output "Server responds to HTTP query" "1" "$HTTP_BODY"
  fi
fi


# ── 3. npm: Client against server ──────────────────────────────────────────────

header "JS/TS: Network client (@petradb/client@${CLIENT_NPM_VERSION})"

if [[ -z "$SERVER_PID" ]]; then
  info "Skipping client tests — server is not running"
else
  CLIENT_DIR="$WORK/js-client"
  mkdir -p "$CLIENT_DIR"

  cat > "$CLIENT_DIR/package.json" <<JSON
{
  "name": "petra-smoke-client",
  "version": "0.0.0",
  "private": true,
  "type": "module",
  "dependencies": {
    "@petradb/client": "${CLIENT_NPM_VERSION}"
  }
}
JSON

  info "Installing @petradb/client@${CLIENT_NPM_VERSION} from npm..."
  (cd "$CLIENT_DIR" && npm install --prefer-online --no-audit --quiet) || { fail "npm install @petradb/client"; }

  cat > "$CLIENT_DIR/smoke.mjs" <<JS
import { Session } from '@petradb/client';

const db = new Session({ port: ${PORT} });
await db.connect();

await db.execute('CREATE TABLE items (id SERIAL, label TEXT)');
await db.execute("INSERT INTO items (label) VALUES ('widget'), ('gadget')");

const [r1] = await db.execute('SELECT COUNT(*) AS n FROM items');
if (Number(r1.rows[0].n) !== 2) throw new Error(\`Expected 2 rows, got \${r1.rows[0].n}\`);

await db.execute("UPDATE items SET label = 'WIDGET' WHERE label = 'widget'");
const [r2] = await db.execute("SELECT label FROM items WHERE id = 1");
if (r2.rows[0].label !== 'WIDGET') throw new Error(\`UPDATE did not apply, got \${r2.rows[0].label}\`);

await db.close();
console.log('OK');
JS

  OUT=$(cd "$CLIENT_DIR" && run_node smoke.mjs)
  check_output "Connect, DDL, INSERT, SELECT, UPDATE" "OK" "$OUT"
fi


# ── 4. CLI ─────────────────────────────────────────────────────────────────────

header "CLI (@petradb/cli@${CLI_NPM_VERSION})"

CLI_DIR="$WORK/cli"
mkdir -p "$CLI_DIR"

cat > "$CLI_DIR/package.json" <<JSON
{
  "name": "petra-smoke-cli",
  "version": "0.0.0",
  "private": true,
  "dependencies": {
    "@petradb/cli": "${CLI_NPM_VERSION}"
  }
}
JSON

info "Installing @petradb/cli@${CLI_NPM_VERSION} from npm..."
(cd "$CLI_DIR" && npm install --prefer-online --no-audit --quiet) || { fail "npm install @petradb/cli"; }

CLI_BIN="$CLI_DIR/node_modules/.bin/petradb"

if [[ ! -x "$CLI_BIN" ]]; then
  fail "petradb CLI binary not found at $CLI_BIN"
else
  OUT=$("$CLI_BIN" -e "CREATE TABLE t (id SERIAL, v TEXT); INSERT INTO t (v) VALUES ('hello'); SELECT * FROM t" 2>&1 || true)
  check_output "CLI -e: inline SQL"  "hello" "$OUT"

  OUT=$("$CLI_BIN" -e "SELECT 1 + 1 AS result" 2>&1 || true)
  check_output "CLI -e: arithmetic"  "2" "$OUT"

  if [[ -n "$SERVER_PID" ]]; then
    OUT=$("$CLI_BIN" --port "$PORT" -e "CREATE TABLE cli_test (n INT); INSERT INTO cli_test VALUES (42); SELECT n FROM cli_test" 2>&1 || true)
    check_output "CLI -e: against server" "42" "$OUT"
  else
    info "Skipping CLI-against-server test — server is not running"
  fi
fi


# ── 5. npm: Quarry ────────────────────────────────────────────────────────────

QUARRY_NPM_VERSION=$(node -p "require('./quarry/package.json').version")

header "JS/TS: Quarry (@petradb/quarry@${QUARRY_NPM_VERSION})"

QUARRY_DIR="$WORK/js-quarry"
mkdir -p "$QUARRY_DIR"

cat > "$QUARRY_DIR/package.json" <<JSON
{
  "name": "petra-smoke-quarry",
  "version": "0.0.0",
  "private": true,
  "type": "module",
  "dependencies": {
    "@petradb/quarry": "${QUARRY_NPM_VERSION}"
  }
}
JSON

info "Installing @petradb/quarry@${QUARRY_NPM_VERSION} from npm..."
(cd "$QUARRY_DIR" && npm install --prefer-online --no-audit --quiet) || { fail "npm install @petradb/quarry"; }

cat > "$QUARRY_DIR/smoke.mjs" <<'JS'
import { Session } from '@petradb/engine';
import { quarry, table, serial, text, integer, boolean, eq, gt, asc, count, alias } from '@petradb/quarry';

const users = table('users', {
  id: serial('id').primaryKey(),
  name: text('name').notNull(),
  email: text('email').notNull().unique(),
  age: integer('age'),
  active: boolean('active').notNull().default(true),
});

const session = new Session();
const db = quarry(session);

await db.createTable(users);

// Insert
const [alice] = await db.insert(users).values({ name: 'Alice', email: 'alice@test.com', age: 30 }).execute();
if (alice.name !== 'Alice') throw new Error(`Expected Alice, got ${alice.name}`);

await db.insert(users).values({ name: 'Bob', email: 'bob@test.com', age: 25, active: false }).execute();
await db.insert(users).values({ name: 'Carol', email: 'carol@test.com', age: 35 }).execute();

// Select with WHERE
const active = await db.select(users).where(eq(users.active, true)).orderBy(asc(users.name)).execute();
if (active.length !== 2) throw new Error(`Expected 2 active users, got ${active.length}`);
if (active[0].name !== 'Alice') throw new Error(`Expected Alice first, got ${active[0].name}`);

// Select with aggregate
const rows = await db.select(users).columns(alias(count(), 'total')).execute();
if (rows[0].total !== 3) throw new Error(`Expected count 3, got ${rows[0].total}`);

// Update
const updated = await db.update(users).set({ age: 31 }).where(eq(users.name, 'Alice')).execute();
if (updated.rowCount !== 1) throw new Error(`Expected 1 updated, got ${updated.rowCount}`);

// Delete
const deleted = await db.delete(users).where(eq(users.name, 'Bob')).execute();
if (deleted.rowCount !== 1) throw new Error(`Expected 1 deleted, got ${deleted.rowCount}`);

// Verify final state
const remaining = await db.select(users).orderBy(asc(users.name)).execute();
if (remaining.length !== 2) throw new Error(`Expected 2 remaining, got ${remaining.length}`);
if (remaining[0].age !== 31) throw new Error(`Expected Alice age 31, got ${remaining[0].age}`);

console.log('OK');
JS

OUT=$(cd "$QUARRY_DIR" && run_node smoke.mjs)
check_output "Schema, insert, select, where, aggregate, update, delete" "OK" "$OUT"


# ── 6. Python: pip install petradb ────────────────────────────────────────────

PYTHON_NPM_VERSION=$(node -p "require('./python/pyproject.toml', 'utf8')" 2>/dev/null || true)
# Parse version from pyproject.toml
PYTHON_PKG_VERSION=$(grep '^version' python/pyproject.toml | head -1 | sed 's/.*"\(.*\)"/\1/')

header "Python: petradb@${PYTHON_PKG_VERSION}"

PYTHON_DIR="$WORK/python"
mkdir -p "$PYTHON_DIR"

info "Creating venv and installing petradb@${PYTHON_PKG_VERSION} from PyPI..."
python3 -m venv "$PYTHON_DIR/venv"
"$PYTHON_DIR/venv/bin/pip" install --quiet "petradb==${PYTHON_PKG_VERSION}" 2>&1 || { fail "pip install petradb"; }

cat > "$PYTHON_DIR/smoke.py" <<'PYTHON'
import os
from petradb import Database

DB_PATH = "mydata.db"

# Clean up from any previous run
if os.path.exists(DB_PATH):
    os.unlink(DB_PATH)

# Create and populate
db = Database(DB_PATH)
db.execute("CREATE TABLE users (id SERIAL PRIMARY KEY, name TEXT NOT NULL, age INT)")
db.execute("INSERT INTO users (name, age) VALUES ('Alice', 30)")
db.execute("INSERT INTO users (name, age) VALUES ('Bob', 25)")
db.execute("INSERT INTO users (name, age) VALUES ('Carol', 35)")

rows = db.query("SELECT name, age FROM users ORDER BY name")
assert len(rows) == 3, f"Expected 3 rows, got {len(rows)}"
assert rows[0].name == 'Alice', f"Expected Alice, got {rows[0].name}"
assert rows[0].age == 30, f"Expected 30, got {rows[0].age}"

# Aggregate
row = db.query_one("SELECT COUNT(*) AS cnt, SUM(age) AS total FROM users")
assert row.cnt == 3, f"Expected count 3, got {row.cnt}"
assert row.total == 90, f"Expected sum 90, got {row.total}"

# Update
rc = db.execute("UPDATE users SET age = 31 WHERE name = 'Alice'")
assert rc == 1, f"Expected 1 updated, got {rc}"

# Delete
rc = db.execute("DELETE FROM users WHERE name = 'Bob'")
assert rc == 1, f"Expected 1 deleted, got {rc}"

db.close()

# Reopen persistent database and verify data survived
db2 = Database(DB_PATH)
rows = db2.query("SELECT name, age FROM users ORDER BY name")
assert len(rows) == 2, f"Expected 2 rows after reopen, got {len(rows)}"
assert rows[0].name == 'Alice', f"Expected Alice, got {rows[0].name}"
assert rows[0].age == 31, f"Expected age 31, got {rows[0].age}"
assert rows[1].name == 'Carol', f"Expected Carol, got {rows[1].name}"
db2.close()

# Clean up
os.unlink(DB_PATH)
print("OK")
PYTHON

OUT=$(cd "$PYTHON_DIR" && "$PYTHON_DIR/venv/bin/python3" smoke.py 2>&1 || true)
check_output "In-memory + persistent (mydata.db) + reopen" "OK" "$OUT"


# ── 7. Scala: Embedded engine ───────────────────────────────────────────────────

header "Scala: Embedded engine (petradb-engine:${ENGINE_SCALA_VERSION})"

SCALA_ENGINE_DIR="$WORK/scala-engine"
mkdir -p "$SCALA_ENGINE_DIR/src/main/scala"
mkdir -p "$SCALA_ENGINE_DIR/project"

echo "sbt.version=1.10.6" > "$SCALA_ENGINE_DIR/project/build.properties"

cat > "$SCALA_ENGINE_DIR/build.sbt" <<SBT
scalaVersion := "${SCALA_FULL_VERSION}"
resolvers += Resolver.mavenCentral
libraryDependencies += "io.github.edadma" %% "petradb-engine" % "${ENGINE_SCALA_VERSION}"
SBT

cat > "$SCALA_ENGINE_DIR/src/main/scala/Smoke.scala" <<'SCALA'
import io.github.edadma.petradb.{Session as _, *}
import io.github.edadma.petradb.engine.*

@main def smoke(): Unit =
  given Session = new MemoryDB().connect()

  executeSQL("""
    CREATE TABLE products (id SERIAL, name TEXT, price NUMERIC(10,2));
    INSERT INTO products (name, price) VALUES ('Widget', 9.99), ('Gadget', 24.99), ('Doohickey', 4.99);
  """)

  val count = executeSQL("SELECT COUNT(*) AS n FROM products")
    .collect { case QueryResult(t) => t }.head.data(0).data(0).string.toInt
  assert(count == 3, s"Expected 3, got $count")

  executeSQL("CREATE VIEW cheap AS SELECT name FROM products WHERE price < 10 ORDER BY name")
  val cheapRows = executeSQL("SELECT * FROM cheap").collect { case QueryResult(t) => t }.head
  assert(cheapRows.data.length == 2, s"Expected 2 cheap products, got ${cheapRows.data.length}")

  executeSQL("INSERT INTO products (name, price) VALUES ('Widget', 99.99) ON CONFLICT DO NOTHING")
  val price = executeSQL("SELECT price FROM products WHERE name = 'Widget'")
    .collect { case QueryResult(t) => t }.head.data(0).data(0).string
  assert(price == "9.99", s"Expected 9.99, got $price")

  executeSQL("CREATE TEMP TABLE tmp (x INT); INSERT INTO tmp VALUES (10), (20), (30)")
  val sum = executeSQL("SELECT SUM(x) AS s FROM tmp")
    .collect { case QueryResult(t) => t }.head.data(0).data(0).string
  assert(sum == "60", s"Expected 60, got $sum")

  println("OK")
SCALA

info "Running Scala engine smoke test (this will take a minute on first run)..."
SCALA_OUT=$(cd "$SCALA_ENGINE_DIR" && sbt --no-colors "run" 2>&1 || true)
check_output "Embedded engine: DDL, views, upsert, temp tables" "OK" "$SCALA_OUT"


# ── 6. Scala: Client against server ────────────────────────────────────────────

header "Scala: Network client (petradb-client:${CLIENT_SCALA_VERSION})"

if [[ -z "$SERVER_PID" ]]; then
  info "Skipping Scala client test — server is not running"
else
  SCALA_CLIENT_DIR="$WORK/scala-client"
  mkdir -p "$SCALA_CLIENT_DIR/src/main/scala"
  mkdir -p "$SCALA_CLIENT_DIR/project"

  echo "sbt.version=1.10.6" > "$SCALA_CLIENT_DIR/project/build.properties"

  cat > "$SCALA_CLIENT_DIR/build.sbt" <<SBT
scalaVersion := "${SCALA_FULL_VERSION}"
resolvers += Resolver.mavenCentral
libraryDependencies += "io.github.edadma" %% "petradb-client" % "${CLIENT_SCALA_VERSION}"
SBT

  cat > "$SCALA_CLIENT_DIR/src/main/scala/Smoke.scala" <<SCALA
import io.github.edadma.petradb.{Session as _, *}
import io.github.edadma.petradb.client.*
import scala.concurrent.Await
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global

@main def smoke(): Unit =
  val session = Session(SessionOptions(host = "localhost", port = ${PORT}))
  Await.result(session.connect(), 30.seconds)

  Await.result(session.execute("CREATE TABLE orders (id SERIAL, item TEXT, qty INT)"), 10.seconds)
  Await.result(session.execute("INSERT INTO orders (item, qty) VALUES ('apple', 5), ('banana', 3)"), 10.seconds)

  val results = Await.result(session.execute("SELECT SUM(qty) AS total FROM orders"), 10.seconds)
  val total = results.collect { case QueryResult(t) => t }.head.data(0).data(0).string.toInt
  assert(total == 8, s"Expected 8, got \$total")

  Await.result(session.close(), 10.seconds)
  println("OK")
SCALA

  info "Running Scala client smoke test..."
  SCALA_CLIENT_OUT=$(cd "$SCALA_CLIENT_DIR" && sbt --no-colors "run" 2>&1 || true)
  check_output "Network client: connect, DDL, INSERT, SELECT" "OK" "$SCALA_CLIENT_OUT"
fi


# ── 7. JDBC: fat jar from Maven Central ───────────────────────────────────────

header "JDBC: fat jar (petradb-jdbc:${JDBC_SCALA_VERSION})"

JDBC_JAVA_DIR="$WORK/jdbc-java"
mkdir -p "$JDBC_JAVA_DIR"

JDBC_JAR_URL="https://repo1.maven.org/maven2/io/github/edadma/petradb-jdbc/${JDBC_SCALA_VERSION}/petradb-jdbc-${JDBC_SCALA_VERSION}.jar"
FAT_JAR="$JDBC_JAVA_DIR/petradb-jdbc.jar"

info "Downloading petradb-jdbc-${JDBC_SCALA_VERSION}.jar from Maven Central..."
HTTP_CODE=$(curl -s -o "$FAT_JAR" -w "%{http_code}" "$JDBC_JAR_URL" 2>&1 || true)

if [[ "$HTTP_CODE" != "200" || ! -s "$FAT_JAR" ]]; then
  fail "Failed to download fat jar (HTTP $HTTP_CODE)"
  info "URL: $JDBC_JAR_URL"
else
  cat > "$JDBC_JAVA_DIR/JdbcSmoke.java" <<'JAVA'
import java.sql.*;

public class JdbcSmoke {
    public static void main(String[] args) throws Exception {
        // No Class.forName — rely on ServiceLoader auto-discovery
        Connection conn = DriverManager.getConnection("jdbc:petradb:memory");
        Statement stmt = conn.createStatement();
        stmt.executeUpdate("CREATE TABLE t (id SERIAL, v TEXT)");
        stmt.executeUpdate("INSERT INTO t (v) VALUES ('hello')");
        ResultSet rs = stmt.executeQuery("SELECT v FROM t");
        rs.next();
        String val_ = rs.getString("v");
        if (!"hello".equals(val_)) throw new RuntimeException("Expected hello, got " + val_);
        stmt.close();
        conn.close();
        System.out.println("OK");
    }
}
JAVA

  info "Compiling and running with java -cp (no Class.forName)..."
  JAVA_OUT=$(cd "$JDBC_JAVA_DIR" && javac -cp "$FAT_JAR" JdbcSmoke.java && java -cp ".:$FAT_JAR" JdbcSmoke 2>&1 || true)
  check_output "DriverManager auto-discovers PetraDriver via ServiceLoader" "OK" "$JAVA_OUT"
fi


# ── Summary ─────────────────────────────────────────────────────────────────────

kill_server

echo ""
echo -e "${CYAN}══ Results ══${RESET}"
echo -e "  ${GREEN}Passed: ${PASS}${RESET}"
if [[ $FAIL -gt 0 ]]; then
  echo -e "  ${RED}Failed: ${FAIL}${RESET}"
  echo ""
  echo -e "${RED}Some tests failed. Check output above for details.${RESET}"
  exit 1
else
  echo -e "  ${RED}Failed: 0${RESET}"
  echo ""
  echo -e "${GREEN}All smoke tests passed! ✓${RESET}"
fi
