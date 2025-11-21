import { describe, expect, it, beforeEach } from "vitest";
import { Cl } from "@stacks/transactions";

const accounts = simnet.getAccounts();
const deployer = accounts.get("deployer")!;
const wallet1 = accounts.get("wallet_1")!;
const wallet2 = accounts.get("wallet_2")!;
const wallet3 = accounts.get("wallet_3")!;

const contractName = "ModDAOcontract";

describe("ModDAO Contract - Appeals System", () => {

  describe("Case Appeals", () => {
    beforeEach(() => {
      // Setup: Register, submit, vote, and resolve case
      simnet.callPublicFn(
        contractName,
        "register-platform",
        [Cl.stringAscii("TestPlatform")],
        wallet1
      );

      // Mint tokens to voters and appellant
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
        [Cl.principal(deployer), Cl.uint(20000)],
        deployer
      );

      const contentHash = new Uint8Array(32).fill(1);
      simnet.callPublicFn(
        contractName,
        "submit-moderation-case",
        [Cl.uint(1), Cl.buffer(contentHash), Cl.stringAscii("US")],
        wallet1
      );

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
        deployer
      );

      simnet.callPublicFn(
        contractName,
        "resolve-case",
        [Cl.uint(1)],
        wallet1
      );
    });

    it("allows appeal of resolved case", () => {
      const response = simnet.callPublicFn(
        contractName,
        "appeal-case",
        [
          Cl.uint(1),
          Cl.stringUtf8("I believe this decision is incorrect")
        ],
        wallet2
      );
      expect(response.result).toBeOk(Cl.bool(true));
    });

    it("requires 2x stake for appeal", () => {
      // Total stake was 3000, so appeal needs 6000
      // wallet2 has 19000 left (20000 - 1000 from voting)
      const response = simnet.callPublicFn(
        contractName,
        "appeal-case",
        [Cl.uint(1), Cl.stringUtf8("Appeal reason")],
        wallet2
      );
      expect(response.result).toBeOk(Cl.bool(true));

      const balance = simnet.callReadOnlyFn(
        contractName,
        "get-token-balance",
        [Cl.principal(wallet2)],
        wallet2
      );
      expect(balance.result).toBeUint(13000); // 19000 - 6000
    });

    it("stores appeal information correctly", () => {
      const appealReason = "This decision violates community guidelines";
      simnet.callPublicFn(
        contractName,
        "appeal-case",
        [Cl.uint(1), Cl.stringUtf8(appealReason)],
        wallet2
      );

      const appealInfo = simnet.callReadOnlyFn(
        contractName,
        "get-appeal-info",
        [Cl.uint(1)],
        wallet2
      );

      expect(appealInfo.result).toBeSome(
        Cl.tuple({
          appellant: Cl.principal(wallet2),
          "appeal-stake": Cl.uint(6000), // 2x total stake (3000)
          "appeal-reason": Cl.stringUtf8(appealReason),
          "appeal-timestamp": Cl.uint(simnet.blockHeight),
          "appeal-resolved": Cl.bool(false)
        })
      );
    });

    it("updates case status to appealed", () => {
      simnet.callPublicFn(
        contractName,
        "appeal-case",
        [Cl.uint(1), Cl.stringUtf8("Appeal")],
        wallet2
      );

      const caseInfo = simnet.callReadOnlyFn(
        contractName,
        "get-case-info",
        [Cl.uint(1)],
        wallet2
      );

      const info = caseInfo.result as any;
      expect(info.value.data.status).toBeUint(2); // STATUS-APPEALED
    });

    it("prevents double appeals", () => {
      simnet.callPublicFn(
        contractName,
        "appeal-case",
        [Cl.uint(1), Cl.stringUtf8("First appeal")],
        wallet2
      );

      const response = simnet.callPublicFn(
        contractName,
        "appeal-case",
        [Cl.uint(1), Cl.stringUtf8("Second appeal")],
        wallet3
      );
      expect(response.result).toBeErr(Cl.uint(110)); // ERR-ALREADY-APPEALED
    });

    it("prevents appeal of non-resolved case", () => {
      // Submit new case
      const contentHash = new Uint8Array(32).fill(2);
      simnet.callPublicFn(
        contractName,
        "submit-moderation-case",
        [Cl.uint(1), Cl.buffer(contentHash), Cl.stringAscii("US")],
        wallet1
      );

      const response = simnet.callPublicFn(
        contractName,
        "appeal-case",
        [Cl.uint(2), Cl.stringUtf8("Appeal")],
        wallet2
      );
      expect(response.result).toBeErr(Cl.uint(106)); // ERR-CASE-CLOSED
    });

    it("prevents appeal after window closes", () => {
      // Advance blocks beyond appeal window (144 blocks)
      for (let i = 0; i < 150; i++) {
        simnet.mineBlock([]);
      }

      const response = simnet.callPublicFn(
        contractName,
        "appeal-case",
        [Cl.uint(1), Cl.stringUtf8("Late appeal")],
        wallet2
      );
      expect(response.result).toBeErr(Cl.uint(108)); // ERR-APPEAL-WINDOW-CLOSED
    });

    it("rejects appeal with insufficient balance", () => {
      // Remove tokens from potential appellant
      const poorWallet = accounts.get("wallet_4")!;
      simnet.callPublicFn(
        contractName,
        "mint-tokens",
        [Cl.principal(poorWallet), Cl.uint(1000)], // Less than needed 6000
        deployer
      );

      const response = simnet.callPublicFn(
        contractName,
        "appeal-case",
        [Cl.uint(1), Cl.stringUtf8("Appeal")],
        poorWallet
      );
      expect(response.result).toBeErr(Cl.uint(109)); // ERR-INSUFFICIENT-STAKE
    });
  });

  describe("Appeal Resolution", () => {
    it("should handle appeal resolution logic", () => {
      // Setup case with appeal
      simnet.callPublicFn(
        contractName,
        "register-platform",
        [Cl.stringAscii("TestPlatform")],
        wallet1
      );

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
        [Cl.principal(deployer), Cl.uint(20000)],
        deployer
      );

      const contentHash = new Uint8Array(32).fill(1);
      simnet.callPublicFn(
        contractName,
        "submit-moderation-case",
        [Cl.uint(1), Cl.buffer(contentHash), Cl.stringAscii("US")],
        wallet1
      );

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
        deployer
      );

      simnet.callPublicFn(
        contractName,
        "resolve-case",
        [Cl.uint(1)],
        wallet1
      );

      simnet.callPublicFn(
        contractName,
        "appeal-case",
        [Cl.uint(1), Cl.stringUtf8("Appeal")],
        wallet2
      );

      // Verify appeal was recorded
      const appealInfo = simnet.callReadOnlyFn(
        contractName,
        "get-appeal-info",
        [Cl.uint(1)],
        wallet2
      );

      expect(appealInfo.result).toBeSome();
    });
  });
});
