#!/bin/bash
set -e

sbt rdbJS/fullOptJS
cp js/target/scala-3.8.1/rdb-opt/main.js npm/main.js
cp shared/src/index.d.ts npm/index.d.ts
cd npm && npm publish --access public
