#!/bin/bash
set -e

./publish.sh server
./publish.sh client

node server/npm/bin/petradb-server -m -p 15480 &
SERVER_PID=$!
trap "kill $SERVER_PID 2>/dev/null" EXIT
sleep 1

node --input-type=module -e "
import { Session } from './client/npm/main.js';
const db = new Session({ port: 15480 });
await db.connect();
await db.execute('CREATE TABLE t (id SERIAL, name TEXT)');
await db.execute(\"INSERT INTO t (name) VALUES ('Alice')\");
const [{ rows }] = await db.execute('SELECT * FROM t');
await db.close();
console.log(rows);
process.exit(rows[0].name === 'Alice' ? 0 : 1);
" && echo "PASS" || echo "FAIL"
