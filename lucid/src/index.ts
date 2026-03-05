// @ts-nocheck — Monkey-patches Lucid internals which are untyped.

import { createRequire } from "node:module";
import path from "node:path";
import { PetraDBDialect } from "./dialect.js";

const _require = createRequire(import.meta.url);

/**
 * Resolve the actual filesystem path of @adonisjs/lucid's build directory.
 * We use absolute paths to bypass the package's exports map, which doesn't
 * expose internal modules we need to patch.
 */
function lucidBuildDir(): string {
  // require.resolve('@adonisjs/lucid') returns .../build/index.js
  return path.dirname(_require.resolve("@adonisjs/lucid"));
}

/**
 * Patch Lucid's client allowlist to accept 'petradb' as a valid client name.
 *
 * This must run before any Lucid connections are created. Import this module
 * as a side-effect import in your AdonisJS app:
 *
 *   import '@petradb/lucid'
 */
function patch() {
  const buildDir = lucidBuildDir();

  // 1. Add PetraDBDialect to the dialect mapping
  const dialects = _require(path.join(buildDir, "src/dialects/index.js"));
  dialects.clientsToDialectsMapping["petradb"] = PetraDBDialect;

  // 2. Update the clientsNames array (used for validation in Connection constructor)
  if (!dialects.clientsNames.includes("petradb")) {
    dialects.clientsNames.push("petradb");
  }

  // 3. Patch Connection.prototype.getWriteConfig to swap the 'petradb' string
  //    for the actual PetraDBClient class constructor (Knex escape hatch)
  const connectionMod = _require(path.join(buildDir, "src/connection/index.js"));
  const Connection = connectionMod.Connection;
  const originalGetWriteConfig = Connection.prototype.getWriteConfig;

  Connection.prototype.getWriteConfig = function () {
    const config = originalGetWriteConfig.call(this);
    if (config.client === "petradb") {
      const PetraDBClient = _require("@petradb/knex").default;
      return { ...config, client: PetraDBClient };
    }
    return config;
  };
}

// Run the patch immediately on import
patch();

export { PetraDBDialect };
