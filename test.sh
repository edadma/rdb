#!/bin/bash
set -e

SCALA_VERSION="3.8.2"

sbt engineJS/fullLinkJS
cp engine/js/target/scala-${SCALA_VERSION}/petradb-engine-opt/main.js engine/npm/main.js
node --experimental-strip-types --test engine/ts-test/petradb.test.ts
