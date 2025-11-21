import { describe, expect, it, beforeEach } from "vitest";
import { Cl } from "@stacks/transactions";

const accounts = simnet.getAccounts();
const deployer = accounts.get("deployer")!;
const wallet1 = accounts.get("wallet_1")!;
const wallet2 = accounts.get("wallet_2")!;

const contractName = "ModDAOcontract";

describe("ModDAO Contract - Initialization & Basic Security", () => {

  describe("Contract Initialization", () => {
    it("ensures contract is initialized correctly", () => {
      expect(simnet.blockHeight).toBeDefined();

      const supply = simnet.callReadOnlyFn(
        contractName,
        "get-total-supply",
        [],
        deployer
      );
      expect(supply.result).toBeUint(1000000);
    });

    it("mints initial tokens to deployer", () => {
      const balance = simnet.callReadOnlyFn(
        contractName,
        "get-token-balance",
        [Cl.principal(deployer)],
        deployer
      );
      expect(balance.result).toBeUint(1000000);
    });

    it("contract is not paused initially", () => {
      const paused = simnet.callReadOnlyFn(
        contractName,
        "is-contract-paused",
        [],
        deployer
      );
      expect(paused.result).toBeBool(false);
    });

    it("emergency mode is disabled initially", () => {
      const emergency = simnet.callReadOnlyFn(
        contractName,
        "is-emergency-mode",
        [],
        deployer
      );
      expect(emergency.result).toBeBool(false);
    });
  });

  describe("Contract Pause Security", () => {
    it("allows owner to pause contract", () => {
      const response = simnet.callPublicFn(
        contractName,
        "pause-contract",
        [],
        deployer
      );
      expect(response.result).toBeOk(Cl.bool(true));

      const paused = simnet.callReadOnlyFn(
        contractName,
        "is-contract-paused",
        [],
        deployer
      );
      expect(paused.result).toBeBool(true);
    });

    it("prevents non-owner from pausing", () => {
      const response = simnet.callPublicFn(
        contractName,
        "pause-contract",
        [],
        wallet1
      );
      expect(response.result).toBeErr(Cl.uint(100)); // ERR-OWNER-ONLY
    });

    it("blocks operations when paused", () => {
      simnet.callPublicFn(contractName, "pause-contract", [], deployer);

      const response = simnet.callPublicFn(
        contractName,
        "register-platform",
        [Cl.stringAscii("TestPlatform")],
        wallet1
      );
      expect(response.result).toBeErr(Cl.uint(111)); // ERR-CONTRACT-PAUSED
    });

    it("allows owner to unpause contract", () => {
      simnet.callPublicFn(contractName, "pause-contract", [], deployer);

      const response = simnet.callPublicFn(
        contractName,
        "unpause-contract",
        [],
        deployer
      );
      expect(response.result).toBeOk(Cl.bool(true));

      const paused = simnet.callReadOnlyFn(
        contractName,
        "is-contract-paused",
        [],
        deployer
      );
      expect(paused.result).toBeBool(false);
    });
  });

  describe("Emergency Mode Security", () => {
    it("allows admin to enable emergency mode", () => {
      const response = simnet.callPublicFn(
        contractName,
        "enable-emergency-mode",
        [],
        deployer
      );
      expect(response.result).toBeOk(Cl.bool(true));

      const emergency = simnet.callReadOnlyFn(
        contractName,
        "is-emergency-mode",
        [],
        deployer
      );
      expect(emergency.result).toBeBool(true);
    });

    it("automatically pauses contract in emergency mode", () => {
      simnet.callPublicFn(
        contractName,
        "enable-emergency-mode",
        [],
        deployer
      );

      const paused = simnet.callReadOnlyFn(
        contractName,
        "is-contract-paused",
        [],
        deployer
      );
      expect(paused.result).toBeBool(true);
    });

    it("allows emergency withdrawal only in emergency mode", () => {
      const response = simnet.callPublicFn(
        contractName,
        "emergency-withdraw",
        [Cl.uint(1000), Cl.principal(wallet1)],
        deployer
      );
      expect(response.result).toBeErr(Cl.uint(118)); // ERR-EMERGENCY-WITHDRAWAL
    });

    it("allows emergency withdrawal when in emergency mode", () => {
      simnet.callPublicFn(
        contractName,
        "enable-emergency-mode",
        [],
        deployer
      );

      const response = simnet.callPublicFn(
        contractName,
        "emergency-withdraw",
        [Cl.uint(1000), Cl.principal(wallet1)],
        deployer
      );
      expect(response.result).toBeOk(Cl.bool(true));
    });
  });

  describe("Admin Management", () => {
    it("allows owner to add admin", () => {
      const response = simnet.callPublicFn(
        contractName,
        "add-admin",
        [Cl.principal(wallet1)],
        deployer
      );
      expect(response.result).toBeOk(Cl.bool(true));

      const isAdmin = simnet.callReadOnlyFn(
        contractName,
        "is-admin",
        [Cl.principal(wallet1)],
        deployer
      );
      expect(isAdmin.result).toBeBool(true);
    });

    it("allows owner to remove admin", () => {
      simnet.callPublicFn(
        contractName,
        "add-admin",
        [Cl.principal(wallet1)],
        deployer
      );

      const response = simnet.callPublicFn(
        contractName,
        "remove-admin",
        [Cl.principal(wallet1)],
        deployer
      );
      expect(response.result).toBeOk(Cl.bool(true));

      const isAdmin = simnet.callReadOnlyFn(
        contractName,
        "is-admin",
        [Cl.principal(wallet1)],
        deployer
      );
      expect(isAdmin.result).toBeBool(false);
    });

    it("prevents non-owner from adding admin", () => {
      const response = simnet.callPublicFn(
        contractName,
        "add-admin",
        [Cl.principal(wallet2)],
        wallet1
      );
      expect(response.result).toBeErr(Cl.uint(100)); // ERR-OWNER-ONLY
    });
  });

  describe("User Blacklist", () => {
    it("allows admin to blacklist user", () => {
      const response = simnet.callPublicFn(
        contractName,
        "blacklist-user",
        [Cl.principal(wallet1)],
        deployer
      );
      expect(response.result).toBeOk(Cl.bool(true));

      const isBlacklisted = simnet.callReadOnlyFn(
        contractName,
        "is-user-blacklisted",
        [Cl.principal(wallet1)],
        deployer
      );
      expect(isBlacklisted.result).toBeBool(true);
    });

    it("prevents blacklisted user from registering platform", () => {
      simnet.callPublicFn(
        contractName,
        "blacklist-user",
        [Cl.principal(wallet1)],
        deployer
      );

      const response = simnet.callPublicFn(
        contractName,
        "register-platform",
        [Cl.stringAscii("TestPlatform")],
        wallet1
      );
      expect(response.result).toBeErr(Cl.uint(115)); // ERR-UNAUTHORIZED
    });

    it("allows admin to remove user from blacklist", () => {
      simnet.callPublicFn(
        contractName,
        "blacklist-user",
        [Cl.principal(wallet1)],
        deployer
      );

      const response = simnet.callPublicFn(
        contractName,
        "remove-from-blacklist",
        [Cl.principal(wallet1)],
        deployer
      );
      expect(response.result).toBeOk(Cl.bool(true));

      const isBlacklisted = simnet.callReadOnlyFn(
        contractName,
        "is-user-blacklisted",
        [Cl.principal(wallet1)],
        deployer
      );
      expect(isBlacklisted.result).toBeBool(false);
    });
  });

  describe("Configuration Updates", () => {
    it("allows admin to update minimum moderators", () => {
      const response = simnet.callPublicFn(
        contractName,
        "update-min-moderators",
        [Cl.uint(5)],
        deployer
      );
      expect(response.result).toBeOk(Cl.bool(true));

      const minMods = simnet.callReadOnlyFn(
        contractName,
        "get-min-moderators-required",
        [],
        deployer
      );
      expect(minMods.result).toBeUint(5);
    });

    it("allows admin to update max stake per vote", () => {
      const response = simnet.callPublicFn(
        contractName,
        "update-max-stake",
        [Cl.uint(50000)],
        deployer
      );
      expect(response.result).toBeOk(Cl.bool(true));

      const maxStake = simnet.callReadOnlyFn(
        contractName,
        "get-max-stake-per-vote",
        [],
        deployer
      );
      expect(maxStake.result).toBeUint(50000);
    });

    it("rejects invalid minimum moderator values", () => {
      const response = simnet.callPublicFn(
        contractName,
        "update-min-moderators",
        [Cl.uint(15)], // Above max
        deployer
      );
      expect(response.result).toBeErr(Cl.uint(103)); // ERR-INVALID-AMOUNT
    });

    it("rejects invalid max stake values", () => {
      const response = simnet.callPublicFn(
        contractName,
        "update-max-stake",
        [Cl.uint(500)], // Below min stake
        deployer
      );
      expect(response.result).toBeErr(Cl.uint(103)); // ERR-INVALID-AMOUNT
    });
  });
});
