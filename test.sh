#!/bin/bash
set -e

sbt rdbJS/fastOptJS
node --experimental-strip-types --test ts-test/rdb.test.ts
