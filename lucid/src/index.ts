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

  // 3. Patch Connection to swap 'petradb' string for PetraDBClient class
  //    and skip knex-dynamic-connection patching (PetraDB is in-process like SQLite)
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

  // PetraDB is an in-process engine — no read/write replicas, no dynamic
  // connection patching needed. Skip patchKnex for petradb connections.
  // 4. Default disableTransactions for migrations (PetraDB doesn't support DDL
  //    inside transactions, same as SQLite)
  const originalConnect = Connection.prototype.connect;
  Connection.prototype.connect = function () {
    if (this.clientName === "petradb") {
      this.config.migrations = Object.assign(
        { disableTransactions: true },
        this.config.migrations
      );
    }
    return originalConnect.call(this);
  };

  const originalSetupWriteConnection = Connection.prototype.setupWriteConnection;
  Connection.prototype.setupWriteConnection = function () {
    if (this.clientName === "petradb") {
      const knexMod = _require("knex");
      const { Logger: ConnectionLogger } = _require(
        path.join(buildDir, "src/connection/logger.js")
      );
      this.client = knexMod.knex(
        Object.assign(
          { log: new ConnectionLogger(this.name, this.logger) },
          this.getWriteConfig(),
          { debug: false }
        )
      );
      // Skip patchKnex — not needed for in-process engines
      return;
    }
    originalSetupWriteConnection.call(this);
  };

  const originalSetupReadConnection = Connection.prototype.setupReadConnection;
  Connection.prototype.setupReadConnection = function () {
    if (this.clientName === "petradb") {
      // In-process engine — read client is same as write client
      this.readClient = this.client;
      return;
    }
    originalSetupReadConnection.call(this);
  };
}

// Run the patch immediately on import
patch();

export { PetraDBDialect };
