import React, { useState, useEffect } from 'react';
import { UserSession } from '@stacks/connect';
import { callReadOnlyFunction, cvToJSON } from '@stacks/transactions';
import { StacksTestnet } from '@stacks/network';

interface ContractStatusProps {
  userSession: UserSession;
}

export function ContractStatus({ userSession }: ContractStatusProps) {
  const [contractStatus, setContractStatus] = useState<any>(null);
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    loadContractStatus();
  }, []);

  const loadContractStatus = async () => {
    try {
      const network = new StacksTestnet();

      // Get contract paused status
      const pausedResult = await callReadOnlyFunction({
        network,
        contractAddress: 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM',
        contractName: 'ModDAOcontract',
        functionName: 'is-contract-paused',
        functionArgs: [],
        senderAddress: userSession.loadUserData().profile.stxAddress.testnet,
      });

      // Get emergency mode status
      const emergencyResult = await callReadOnlyFunction({
        network,
        contractAddress: 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM',
        contractName: 'ModDAOcontract',
        functionName: 'is-emergency-mode',
        functionArgs: [],
        senderAddress: userSession.loadUserData().profile.stxAddress.testnet,
      });

      // Get total supply
      const supplyResult = await callReadOnlyFunction({
        network,
        contractAddress: 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM',
        contractName: 'ModDAOcontract',
        functionName: 'get-total-supply',
        functionArgs: [],
        senderAddress: userSession.loadUserData().profile.stxAddress.testnet,
      });

      setContractStatus({
        paused: cvToJSON(pausedResult).value,
        emergency: cvToJSON(emergencyResult).value,
        totalSupply: cvToJSON(supplyResult).value,
      });
    } catch (error) {
      console.error('Error loading contract status:', error);
    } finally {
      setLoading(false);
    }
  };

  if (loading) {
    return (
      <div className="card">
        <div className="animate-pulse">
          <div className="h-4 bg-gray-200 rounded w-1/4 mb-4"></div>
          <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
            <div className="h-16 bg-gray-200 rounded"></div>
            <div className="h-16 bg-gray-200 rounded"></div>
            <div className="h-16 bg-gray-200 rounded"></div>
          </div>
        </div>
      </div>
    );
  }

  return (
    <div className="card">
      <h2 className="text-lg font-semibold mb-4 flex items-center">
        <span className="mr-2">📊</span>
        Contract Status
      </h2>

      <div className="grid grid-cols-1 md:grid-cols-3 gap-4">
        <div className="text-center">
          <div className={`inline-flex items-center px-3 py-1 rounded-full text-sm font-medium ${
            contractStatus?.paused ? 'bg-red-100 text-red-800' : 'bg-green-100 text-green-800'
          }`}>
            {contractStatus?.paused ? '⏸️ Paused' : '▶️ Active'}
          </div>
          <p className="text-sm text-gray-600 mt-1">Contract Status</p>
        </div>

        <div className="text-center">
          <div className={`inline-flex items-center px-3 py-1 rounded-full text-sm font-medium ${
            contractStatus?.emergency ? 'bg-red-100 text-red-800' : 'bg-green-100 text-green-800'
          }`}>
            {contractStatus?.emergency ? '🚨 Emergency' : '✅ Normal'}
          </div>
          <p className="text-sm text-gray-600 mt-1">Emergency Mode</p>
        </div>

        <div className="text-center">
          <div className="text-2xl font-bold text-primary-600">
            {contractStatus?.totalSupply ? (contractStatus.totalSupply / 1000000).toLocaleString() : '0'}
          </div>
          <p className="text-sm text-gray-600">Total MOD Tokens</p>
        </div>
      </div>

      {contractStatus?.paused && (
        <div className="mt-4 p-3 bg-yellow-50 border border-yellow-200 rounded-md">
          <p className="text-sm text-yellow-800">
            ⚠️ Contract is currently paused. Some operations may be unavailable.
          </p>
        </div>
      )}

      {contractStatus?.emergency && (
        <div className="mt-4 p-3 bg-red-50 border border-red-200 rounded-md">
          <p className="text-sm text-red-800">
            🚨 Emergency mode is active. Only emergency functions are available.
          </p>
        </div>
      )}
    </div>
  );
}
