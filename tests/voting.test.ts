import { describe, expect, it, beforeEach } from "vitest";
import { Cl } from "@stacks/transactions";

const accounts = simnet.getAccounts();
const deployer = accounts.get("deployer")!;
const wallet1 = accounts.get("wallet_1")!;
const wallet2 = accounts.get("wallet_2")!;
const wallet3 = accounts.get("wallet_3")!;
const wallet4 = accounts.get("wallet_4")!;

const contractName = "ModDAOcontract";

describe("ModDAO Contract - Voting System", () => {

  describe("Voting on Cases", () => {
    beforeEach(() => {
      // Setup: Register platform and submit case
      simnet.callPublicFn(
        contractName,
        "register-platform",
        [Cl.stringAscii("TestPlatform")],
        wallet1
      );

      // Mint tokens to voters
      simnet.callPublicFn(
        contractName,
        "mint-tokens",
        [Cl.principal(wallet2), Cl.uint(10000)],
        deployer
      );
      simnet.callPublicFn(
        contractName,
        "mint-tokens",
        [Cl.principal(wallet3), Cl.uint(10000)],
        deployer
      );

      const contentHash = new Uint8Array(32).fill(1);
      simnet.callPublicFn(
        contractName,
        "submit-moderation-case",
        [
          Cl.uint(1),
          Cl.buffer(contentHash),
          Cl.stringAscii("US")
        ],
        wallet1
      );
    });

    it("allows moderator to vote on case", () => {
      const response = simnet.callPublicFn(
        contractName,
        "vote-on-case",
        [
          Cl.uint(1),
          Cl.uint(1), // VIOLATION-HARASSMENT
          Cl.uint(1000),
          Cl.uint(50)
        ],
        wallet2
      );
      expect(response.result).toBeOk(Cl.bool(true));
    });

    it("transfers stake from voter to contract", () => {
      const balanceBefore = simnet.callReadOnlyFn(
        contractName,
        "get-token-balance",
        [Cl.principal(wallet2)],
        wallet2
      );

      simnet.callPublicFn(
        contractName,
        "vote-on-case",
        [
          Cl.uint(1),
          Cl.uint(1),
          Cl.uint(1000),
          Cl.uint(50)
        ],
        wallet2
      );

      const balanceAfter = simnet.callReadOnlyFn(
        contractName,
        "get-token-balance",
        [Cl.principal(wallet2)],
        wallet2
      );

      expect(balanceAfter.result).toBeUint(9000);
    });

    it("prevents double voting", () => {
      simnet.callPublicFn(
        contractName,
        "vote-on-case",
        [
          Cl.uint(1),
          Cl.uint(1),
          Cl.uint(1000),
          Cl.uint(50)
        ],
        wallet2
      );

      const response = simnet.callPublicFn(
        contractName,
        "vote-on-case",
        [
          Cl.uint(1),
          Cl.uint(2),
          Cl.uint(1000),
          Cl.uint(50)
        ],
        wallet2
      );
      expect(response.result).toBeErr(Cl.uint(105)); // ERR-ALREADY-VOTED
    });

    it("rejects vote with insufficient stake", () => {
      const response = simnet.callPublicFn(
        contractName,
        "vote-on-case",
        [
          Cl.uint(1),
          Cl.uint(1),
          Cl.uint(500), // Below MIN-STAKE
          Cl.uint(50)
        ],
        wallet2
      );
      expect(response.result).toBeErr(Cl.uint(109)); // ERR-INSUFFICIENT-STAKE
    });

    it("rejects vote with excessive stake", () => {
      const response = simnet.callPublicFn(
        contractName,
        "vote-on-case",
        [
          Cl.uint(1),
          Cl.uint(1),
          Cl.uint(200000), // Above max-stake-per-vote
          Cl.uint(50)
        ],
        wallet2
      );
      expect(response.result).toBeErr(Cl.uint(103)); // ERR-INVALID-AMOUNT
    });

    it("rejects invalid violation type", () => {
      const response = simnet.callPublicFn(
        contractName,
        "vote-on-case",
        [
          Cl.uint(1),
          Cl.uint(10), // Invalid violation type
          Cl.uint(1000),
          Cl.uint(50)
        ],
        wallet2
      );
      expect(response.result).toBeErr(Cl.uint(103)); // ERR-INVALID-AMOUNT
    });

    it("rejects invalid cultural weight", () => {
      const response = simnet.callPublicFn(
        contractName,
        "vote-on-case",
        [
          Cl.uint(1),
          Cl.uint(1),
          Cl.uint(1000),
          Cl.uint(150) // Above 100
        ],
        wallet2
      );
      expect(response.result).toBeErr(Cl.uint(103)); // ERR-INVALID-AMOUNT
    });

    it("increments moderator count for case", () => {
      simnet.callPublicFn(
        contractName,
        "vote-on-case",
        [Cl.uint(1), Cl.uint(1), Cl.uint(1000), Cl.uint(50)],
        wallet2
      );

      const count = simnet.callReadOnlyFn(
        contractName,
        "get-case-moderator-count",
        [Cl.uint(1)],
        wallet2
      );
      expect(count.result).toBeUint(1);
    });

    it("allows multiple moderators to vote", () => {
      simnet.callPublicFn(
        contractName,
        "vote-on-case",
        [Cl.uint(1), Cl.uint(1), Cl.uint(1000), Cl.uint(50)],
        wallet2
      );

      const response = simnet.callPublicFn(
        contractName,
        "vote-on-case",
        [Cl.uint(1), Cl.uint(1), Cl.uint(1000), Cl.uint(50)],
        wallet3
      );
      expect(response.result).toBeOk(Cl.bool(true));

      const count = simnet.callReadOnlyFn(
        contractName,
        "get-case-moderator-count",
        [Cl.uint(1)],
        wallet2
      );
      expect(count.result).toBeUint(2);
    });

    it("updates violation votes correctly", () => {
      simnet.callPublicFn(
        contractName,
        "vote-on-case",
        [Cl.uint(1), Cl.uint(1), Cl.uint(1000), Cl.uint(50)],
        wallet2
      );

      simnet.callPublicFn(
        contractName,
        "vote-on-case",
        [Cl.uint(1), Cl.uint(1), Cl.uint(1500), Cl.uint(75)],
        wallet3
      );

      const caseInfo = simnet.callReadOnlyFn(
        contractName,
        "get-case-info",
        [Cl.uint(1)],
        wallet2
      );

      const info = caseInfo.result as any;
      expect(info.value.data["violation-votes"]).toEqual(Cl.list([
        Cl.uint(0), Cl.uint(2), Cl.uint(0), Cl.uint(0), Cl.uint(0), Cl.uint(0), Cl.uint(0)
      ]));
      expect(info.value.data["violation-stakes"]).toEqual(Cl.list([
        Cl.uint(0), Cl.uint(2500), Cl.uint(0), Cl.uint(0), Cl.uint(0), Cl.uint(0), Cl.uint(0)
      ]));
      expect(info.value.data["total-stake"]).toBeUint(2500);
    });

    it("updates moderator reputation", () => {
      const initialRep = simnet.callReadOnlyFn(
        contractName,
        "get-moderator-reputation",
        [Cl.principal(wallet2)],
        wallet2
      );

      simnet.callPublicFn(
        contractName,
        "vote-on-case",
        [Cl.uint(1), Cl.uint(1), Cl.uint(1000), Cl.uint(50)],
        wallet2
      );

      const afterRep = simnet.callReadOnlyFn(
        contractName,
        "get-moderator-reputation",
        [Cl.principal(wallet2)],
        wallet2
      );

      expect(initialRep.result).toBeNone();
      expect(afterRep.result).toBeSome(
        Cl.tuple({
          "total-cases": Cl.uint(1),
          "correct-votes": Cl.uint(0),
          "total-stake": Cl.uint(1000),
          "earned-rewards": Cl.uint(0),
          "cultural-regions": Cl.list([Cl.stringAscii("")])
        })
      );
    });

    it("prevents voting on resolved case", () => {
      // Add minimum votes and resolve
      simnet.callPublicFn(
        contractName,
        "vote-on-case",
        [Cl.uint(1), Cl.uint(1), Cl.uint(1000), Cl.uint(50)],
        wallet2
      );
      simnet.callPublicFn(
        contractName,
        "vote-on-case",
        [Cl.uint(1), Cl.uint(1), Cl.uint(1000), Cl.uint(50)],
        wallet3
      );
      simnet.callPublicFn(
        contractName,
        "vote-on-case",
        [Cl.uint(1), Cl.uint(1), Cl.uint(1000), Cl.uint(50)],
        wallet4
      );

      simnet.callPublicFn(
        contractName,
        "resolve-case",
        [Cl.uint(1)],
        wallet1
      );

      // Try to vote again
      const response = simnet.callPublicFn(
        contractName,
        "vote-on-case",
        [Cl.uint(1), Cl.uint(2), Cl.uint(1000), Cl.uint(50)],
        deployer
      );
      expect(response.result).toBeErr(Cl.uint(106)); // ERR-CASE-CLOSED
    });
  });

  describe("Case Resolution", () => {
    beforeEach(() => {
      // Setup: Register, submit, vote, and resolve case
      simnet.callPublicFn(
        contractName,
        "register-platform",
        [Cl.stringAscii("TestPlatform")],
        wallet1
      );

      // Mint tokens to voters
      simnet.callPublicFn(
        contractName,
        "mint-tokens",
        [Cl.principal(wallet2), Cl.uint(20000)],
        deployer
      );
      simnet.callPublicFn(
        contractName,
        "mint-tokens",
        [Cl.principal(wallet3), Cl.uint(20000)],
        deployer
      );
      simnet.callPublicFn(
        contractName,
        "mint-tokens",
        [Cl.principal(wallet4), Cl.uint(20000)],
        deployer
      );

      const contentHash = new Uint8Array(32).fill(1);
      simnet.callPublicFn(
        contractName,
        "submit-moderation-case",
        [Cl.uint(1), Cl.buffer(contentHash), Cl.stringAscii("US")],
        wallet1
      );
    });

    it("requires minimum moderators before resolution", () => {
      // Only 1 vote (need 3)
      simnet.callPublicFn(
        contractName,
        "vote-on-case",
        [Cl.uint(1), Cl.uint(1), Cl.uint(1000), Cl.uint(50)],
        wallet2
      );

      const response = simnet.callPublicFn(
        contractName,
        "resolve-case",
        [Cl.uint(1)],
        wallet1
      );
      expect(response.result).toBeErr(Cl.uint(109)); // ERR-INSUFFICIENT-STAKE
    });

    it("allows resolution with sufficient moderators", () => {
      // Add 3 votes
      simnet.callPublicFn(
        contractName,
        "vote-on-case",
        [Cl.uint(1), Cl.uint(1), Cl.uint(1000), Cl.uint(50)],
        wallet2
      );
      simnet.callPublicFn(
        contractName,
        "vote-on-case",
        [Cl.uint(1), Cl.uint(1), Cl.uint(1000), Cl.uint(50)],
        wallet3
      );
      simnet.callPublicFn(
        contractName,
        "vote-on-case",
        [Cl.uint(1), Cl.uint(1), Cl.uint(1000), Cl.uint(50)],
        wallet4
      );

      const response = simnet.callPublicFn(
        contractName,
        "resolve-case",
        [Cl.uint(1)],
        wallet1
      );
      expect(response.result).toBeOk(Cl.uint(1)); // Returns winning violation
    });

    it("correctly determines winning violation", () => {
      // Vote for different violations
      simnet.callPublicFn(
        contractName,
        "vote-on-case",
        [Cl.uint(1), Cl.uint(1), Cl.uint(1000), Cl.uint(50)], // Harassment
        wallet2
      );
      simnet.callPublicFn(
        contractName,
        "vote-on-case",
        [Cl.uint(1), Cl.uint(2), Cl.uint(2000), Cl.uint(50)], // Spam with more stake
        wallet3
      );
      simnet.callPublicFn(
        contractName,
        "vote-on-case",
        [Cl.uint(1), Cl.uint(1), Cl.uint(500), Cl.uint(50)], // Harassment
        wallet4
      );

      const response = simnet.callPublicFn(
        contractName,
        "resolve-case",
        [Cl.uint(1)],
        wallet1
      );
      expect(response.result).toBeOk(Cl.uint(1)); // Harassment wins (1500 stake vs 2000 spam)
    });

    it("updates case status to resolved", () => {
      // Add votes and resolve
      simnet.callPublicFn(
        contractName,
        "vote-on-case",
        [Cl.uint(1), Cl.uint(1), Cl.uint(1000), Cl.uint(50)],
        wallet2
      );
      simnet.callPublicFn(
        contractName,
        "vote-on-case",
        [Cl.uint(1), Cl.uint(1), Cl.uint(1000), Cl.uint(50)],
        wallet3
      );
      simnet.callPublicFn(
        contractName,
        "vote-on-case",
        [Cl.uint(1), Cl.uint(1), Cl.uint(1000), Cl.uint(50)],
        wallet4
      );
      simnet.callPublicFn(
        contractName,
        "resolve-case",
        [Cl.uint(1)],
        wallet1
      );

      const caseInfo = simnet.callReadOnlyFn(
        contractName,
        "get-case-info",
        [Cl.uint(1)],
        wallet1
      );

      const info = caseInfo.result as any;
      expect(info.value.data.status).toBeUint(1); // STATUS-RESOLVED
      expect(info.value.data["resolved-at"]).toBeUint(simnet.blockHeight);
      expect(info.value.data["final-decision"]).toBeUint(1);
    });

    it("prevents resolution of already resolved case", () => {
      // Add votes and resolve
      simnet.callPublicFn(
        contractName,
        "vote-on-case",
        [Cl.uint(1), Cl.uint(1), Cl.uint(1000), Cl.uint(50)],
        wallet2
      );
      simnet.callPublicFn(
        contractName,
        "vote-on-case",
        [Cl.uint(1), Cl.uint(1), Cl.uint(1000), Cl.uint(50)],
        wallet3
      );
      simnet.callPublicFn(
        contractName,
        "vote-on-case",
        [Cl.uint(1), Cl.uint(1), Cl.uint(1000), Cl.uint(50)],
        wallet4
      );
      simnet.callPublicFn(
        contractName,
        "resolve-case",
        [Cl.uint(1)],
        wallet1
      );

      // Try to resolve again
      const response = simnet.callPublicFn(
        contractName,
        "resolve-case",
        [Cl.uint(1)],
        wallet1
      );
      expect(response.result).toBeErr(Cl.uint(106)); // ERR-CASE-CLOSED
    });
  });
});
