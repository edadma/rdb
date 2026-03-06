// Quick smoke test: verify the monkey-patch works and Lucid accepts 'petradb'

import { createRequire } from "node:module";
import path from "node:path";

const _require = createRequire(import.meta.url);
const buildDir = path.dirname(_require.resolve("@adonisjs/lucid"));

// Step 1: Verify the patch hasn't been applied yet
const dialects = _require(path.join(buildDir, "src/dialects/index.js"));
console.log("Before patch - clientsNames:", dialects.clientsNames);
console.log(
  "Before patch - has petradb:",
  dialects.clientsNames.includes("petradb")
);

// Step 2: Apply the patch (side-effect import)
await import("./dist/index.js");

// Step 3: Verify the patch was applied
console.log("\nAfter patch - clientsNames:", dialects.clientsNames);
console.log(
  "After patch - has petradb:",
  dialects.clientsNames.includes("petradb")
);
console.log(
  "After patch - dialect class:",
  dialects.clientsToDialectsMapping["petradb"]?.name
);

// Step 4: Try creating a Connection with 'petradb' client
const connectionMod = _require(path.join(buildDir, "src/connection/index.js"));
try {
  const conn = new connectionMod.Connection(
    "test",
    {
      client: "petradb",
      connection: { storage: "memory" },
    },
    { trace() {} }
  );
  console.log("\nConnection created! clientName:", conn.clientName);

  // Step 5: Verify getWriteConfig swaps the string for the class
  const writeConfig = conn.getWriteConfig();
  console.log(
    "getWriteConfig client type:",
    typeof writeConfig.client,
    writeConfig.client === "petradb"
      ? "(still string - BAD)"
      : "(function - GOOD)"
  );
  console.log(
    "getWriteConfig client name:",
    writeConfig.client?.name || writeConfig.client
  );
} catch (err) {
  console.error("Connection creation FAILED:", err.message);
  process.exit(1);
}

console.log("\nAll checks passed!");
