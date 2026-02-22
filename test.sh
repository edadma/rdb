#!/bin/bash
set -e

sbt engineJS/fullLinkJS
node --experimental-strip-types --test ts-test/petradb.test.ts
