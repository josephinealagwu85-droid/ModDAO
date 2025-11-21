
import { describe, expect, it } from "vitest";

// Import all test suites
import "./security.test.ts";
import "./platform.test.ts";
import "./voting.test.ts";
import "./appeals.test.ts";

const accounts = simnet.getAccounts();
const address1 = accounts.get("wallet_1")!;

/*
  Main test runner for ModDAO Contract
  Individual test suites are imported above:
  - security.test.ts: Security features, admin controls, emergency mode
  - platform.test.ts: Platform registration and case submission
  - voting.test.ts: Voting system and case resolution
  - appeals.test.ts: Appeals system

  Run with: npm test
  Run with coverage: npm run test:report
*/

describe("ModDAO Contract - Integration Tests", () => {
  it("ensures simnet is well initialised", () => {
    expect(simnet.blockHeight).toBeDefined();
  });

  // Integration tests can be added here if needed
  // Most tests are in the imported test suites above
});
