import React from 'react';

interface ConnectWalletProps {
  onConnect: () => void;
}

export function ConnectWallet({ onConnect }: ConnectWalletProps) {
  return (
    <div className="min-h-screen bg-gradient-to-br from-primary-50 to-primary-100 flex items-center justify-center">
      <div className="max-w-md w-full bg-white rounded-lg shadow-lg p-8 text-center">
        <div className="mb-8">
          <h1 className="text-3xl font-bold text-gray-900 mb-2">ModDAO</h1>
          <p className="text-gray-600">Decentralized Content Moderation Platform</p>
        </div>

        <div className="mb-8">
          <div className="bg-blue-50 rounded-lg p-4 mb-6">
            <h3 className="font-semibold text-blue-900 mb-2">Features</h3>
            <ul className="text-sm text-blue-800 space-y-1 text-left">
              <li>• Stake-based content moderation</li>
              <li>• Transparent appeal system</li>
              <li>• Cultural region considerations</li>
              <li>• Reputation-based governance</li>
            </ul>
          </div>

          <div className="grid grid-cols-2 gap-4 text-center">
            <div className="bg-gray-50 rounded-lg p-4">
              <div className="text-2xl font-bold text-primary-600">1000</div>
              <div className="text-sm text-gray-600">Min Stake</div>
            </div>
            <div className="bg-gray-50 rounded-lg p-4">
              <div className="text-2xl font-bold text-success">7</div>
              <div className="text-sm text-gray-600">Violation Types</div>
            </div>
          </div>
        </div>

        <button
          onClick={onConnect}
          className="w-full btn btn-primary text-lg py-3"
        >
          Connect Wallet
        </button>

        <p className="text-xs text-gray-500 mt-4">
          Connect your Stacks wallet to participate in decentralized content moderation
        </p>
      </div>
    </div>
  );
}
