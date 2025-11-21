import React, { useState, useEffect } from 'react';
import { UserSession } from '@stacks/connect';
import { CaseList } from './CaseList';
import { PlatformManagement } from './PlatformManagement';
import { ModeratorStats } from './ModeratorStats';
import { ContractStatus } from './ContractStatus';

interface DashboardProps {
  userSession: UserSession;
  userData: any;
}

export function Dashboard({ userSession, userData }: DashboardProps) {
  const [activeTab, setActiveTab] = useState<'cases' | 'platforms' | 'stats' | 'admin'>('cases');

  const tabs = [
    { id: 'cases' as const, label: 'Moderation Cases', icon: '📋' },
    { id: 'platforms' as const, label: 'Platforms', icon: '🏢' },
    { id: 'stats' as const, label: 'My Stats', icon: '📊' },
    { id: 'admin' as const, label: 'Admin Panel', icon: '⚙️' },
  ];

  return (
    <div className="space-y-6">
      {/* Navigation Tabs */}
      <div className="bg-white rounded-lg shadow-sm border border-gray-200">
        <div className="border-b border-gray-200">
          <nav className="flex space-x-8 px-6">
            {tabs.map((tab) => (
              <button
                key={tab.id}
                onClick={() => setActiveTab(tab.id)}
                className={`py-4 px-1 border-b-2 font-medium text-sm ${
                  activeTab === tab.id
                    ? 'border-primary-500 text-primary-600'
                    : 'border-transparent text-gray-500 hover:text-gray-700 hover:border-gray-300'
                }`}
              >
                <span className="mr-2">{tab.icon}</span>
                {tab.label}
              </button>
            ))}
          </nav>
        </div>
      </div>

      {/* Contract Status */}
      <ContractStatus userSession={userSession} />

      {/* Tab Content */}
      <div className="space-y-6">
        {activeTab === 'cases' && (
          <CaseList userSession={userSession} userData={userData} />
        )}
        {activeTab === 'platforms' && (
          <PlatformManagement userSession={userSession} userData={userData} />
        )}
        {activeTab === 'stats' && (
          <ModeratorStats userSession={userSession} userData={userData} />
        )}
        {activeTab === 'admin' && (
          <AdminPanel userSession={userSession} userData={userData} />
        )}
      </div>
    </div>
  );
}

// Placeholder for Admin Panel
function AdminPanel({ userSession, userData }: DashboardProps) {
  return (
    <div className="card">
      <h2 className="text-xl font-semibold mb-4">Admin Panel</h2>
      <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
        <div>
          <h3 className="font-medium mb-3">Contract Management</h3>
          <div className="space-y-3">
            <button className="btn btn-warning w-full">Enable Emergency Mode</button>
            <button className="btn btn-danger w-full">Pause Contract</button>
            <button className="btn btn-success w-full">Mint Tokens</button>
          </div>
        </div>
        <div>
          <h3 className="font-medium mb-3">Configuration</h3>
          <div className="space-y-3">
            <div>
              <label className="block text-sm font-medium text-gray-700 mb-1">
                Min Moderators Required
              </label>
              <input type="number" className="input" defaultValue="3" />
            </div>
            <div>
              <label className="block text-sm font-medium text-gray-700 mb-1">
                Max Stake per Vote
              </label>
              <input type="number" className="input" defaultValue="100000" />
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
