#!/bin/bash
set -e

sbt rdbJS/fullOptJS
node --experimental-strip-types --test ts-test/rdb.test.ts
