import { describe, expect, it, beforeEach } from "vitest";
import { Cl } from "@stacks/transactions";

const accounts = simnet.getAccounts();
const deployer = accounts.get("deployer")!;
const wallet1 = accounts.get("wallet_1")!;
const wallet2 = accounts.get("wallet_2")!;

const contractName = "ModDAOcontract";

describe("ModDAO Contract - Platform & Case Management", () => {

  describe("Platform Registration", () => {
    it("allows users to register a platform", () => {
      const response = simnet.callPublicFn(
        contractName,
        "register-platform",
        [Cl.stringAscii("TestPlatform")],
        wallet1
      );
      expect(response.result).toBeOk(Cl.uint(1));
    });

    it("increments platform ID for each registration", () => {
      simnet.callPublicFn(
        contractName,
        "register-platform",
        [Cl.stringAscii("Platform1")],
        wallet1
      );

      const response = simnet.callPublicFn(
        contractName,
        "register-platform",
        [Cl.stringAscii("Platform2")],
        wallet2
      );
      expect(response.result).toBeOk(Cl.uint(2));
    });

    it("stores platform information correctly", () => {
      simnet.callPublicFn(
        contractName,
        "register-platform",
        [Cl.stringAscii("MyPlatform")],
        wallet1
      );

      const platformInfo = simnet.callReadOnlyFn(
        contractName,
        "get-platform-info",
        [Cl.uint(1)],
        wallet1
      );

      expect(platformInfo.result).toBeSome(
        Cl.tuple({
          name: Cl.stringAscii("MyPlatform"),
          owner: Cl.principal(wallet1),
          active: Cl.bool(true),
          "total-cases": Cl.uint(0),
          "reputation-score": Cl.uint(1000)
        })
      );
    });

    it("rejects empty platform name", () => {
      const response = simnet.callPublicFn(
        contractName,
        "register-platform",
        [Cl.stringAscii("")],
        wallet1
      );
      expect(response.result).toBeErr(Cl.uint(103)); // ERR-INVALID-AMOUNT
    });

    it("respects rate limiting", () => {
      // Register multiple platforms quickly
      for (let i = 0; i < 5; i++) {
        simnet.callPublicFn(
          contractName,
          "register-platform",
          [Cl.stringAscii(`Platform${i}`)],
          wallet1
        );
      }

      // 6th attempt should fail due to rate limit
      const response = simnet.callPublicFn(
        contractName,
        "register-platform",
        [Cl.stringAscii("Platform6")],
        wallet1
      );
      expect(response.result).toBeErr(Cl.uint(112)); // ERR-RATE-LIMIT-EXCEEDED
    });
  });

  describe("Moderation Case Submission", () => {
    beforeEach(() => {
      simnet.callPublicFn(
        contractName,
        "register-platform",
        [Cl.stringAscii("TestPlatform")],
        wallet1
      );
    });

    it("allows platform owner to submit a case", () => {
      const contentHash = new Uint8Array(32).fill(1);
      const response = simnet.callPublicFn(
        contractName,
        "submit-moderation-case",
        [
          Cl.uint(1),
          Cl.buffer(contentHash),
          Cl.stringAscii("US")
        ],
        wallet1
      );
      expect(response.result).toBeOk(Cl.uint(1));
    });

    it("rejects case submission from non-owner", () => {
      const contentHash = new Uint8Array(32).fill(1);
      const response = simnet.callPublicFn(
        contractName,
        "submit-moderation-case",
        [
          Cl.uint(1),
          Cl.buffer(contentHash),
          Cl.stringAscii("US")
        ],
        wallet2
      );
      expect(response.result).toBeErr(Cl.uint(101)); // ERR-NOT-TOKEN-OWNER
    });

    it("rejects invalid content hash", () => {
      const invalidHash = new Uint8Array(16).fill(1); // Wrong size
      const response = simnet.callPublicFn(
        contractName,
        "submit-moderation-case",
        [
          Cl.uint(1),
          Cl.buffer(invalidHash),
          Cl.stringAscii("US")
        ],
        wallet1
      );
      expect(response.result).toBeErr(Cl.uint(116)); // ERR-INVALID-CONTENT-HASH
    });

    it("rejects invalid cultural region", () => {
      const contentHash = new Uint8Array(32).fill(1);
      const response = simnet.callPublicFn(
        contractName,
        "submit-moderation-case",
        [
          Cl.uint(1),
          Cl.buffer(contentHash),
          Cl.stringAscii("") // Empty region
        ],
        wallet1
      );
      expect(response.result).toBeErr(Cl.uint(117)); // ERR-INVALID-CULTURAL-REGION
    });

    it("increments platform case count", () => {
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

      const platformInfo = simnet.callReadOnlyFn(
        contractName,
        "get-platform-info",
        [Cl.uint(1)],
        wallet1
      );

      const info = platformInfo.result as any;
      expect(info.value.data["total-cases"]).toBeUint(1);
    });

    it("initializes case moderator count", () => {
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

      const count = simnet.callReadOnlyFn(
        contractName,
        "get-case-moderator-count",
        [Cl.uint(1)],
        wallet1
      );
      expect(count.result).toBeUint(0);
    });

    it("stores case information correctly", () => {
      const contentHash = new Uint8Array(32).fill(42);
      simnet.callPublicFn(
        contractName,
        "submit-moderation-case",
        [
          Cl.uint(1),
          Cl.buffer(contentHash),
          Cl.stringAscii("EU")
        ],
        wallet1
      );

      const caseInfo = simnet.callReadOnlyFn(
        contractName,
        "get-case-info",
        [Cl.uint(1)],
        wallet1
      );

      expect(caseInfo.result).toBeSome(
        Cl.tuple({
          "platform-id": Cl.uint(1),
          "content-hash": Cl.buffer(contentHash),
          submitter: Cl.principal(wallet1),
          "total-stake": Cl.uint(0),
          "violation-votes": Cl.list([Cl.uint(0), Cl.uint(0), Cl.uint(0), Cl.uint(0), Cl.uint(0), Cl.uint(0), Cl.uint(0)]),
          "violation-stakes": Cl.list([Cl.uint(0), Cl.uint(0), Cl.uint(0), Cl.uint(0), Cl.uint(0), Cl.uint(0), Cl.uint(0)]),
          status: Cl.uint(0), // STATUS-PENDING
          "created-at": Cl.uint(simnet.blockHeight),
          "resolved-at": Cl.uint(0),
          "final-decision": Cl.uint(0), // VIOLATION-NONE
          "cultural-region": Cl.stringAscii("EU")
        })
      );
    });
  });
});
