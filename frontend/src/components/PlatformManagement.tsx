import React from 'react';
import { UserSession } from '@stacks/connect';

interface PlatformManagementProps {
  userSession: UserSession;
  userData: any;
}

export function PlatformManagement({ userSession, userData }: PlatformManagementProps) {
  return (
    <div className="space-y-6">
      <div className="flex justify-between items-center">
        <h2 className="text-xl font-semibold">Platform Management</h2>
        <button className="btn btn-primary">Register New Platform</button>
      </div>

      <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
        <div className="card">
          <h3 className="text-lg font-medium mb-4">My Platforms</h3>
          <div className="space-y-3">
            <div className="border rounded-lg p-4">
              <div className="flex justify-between items-start">
                <div>
                  <h4 className="font-medium">SocialMedia Platform</h4>
                  <p className="text-sm text-gray-600">ID: #1 • Active</p>
                  <p className="text-sm text-gray-500">Cases: 15 • Reputation: 1,250</p>
                </div>
                <span className="badge badge-resolved">Active</span>
              </div>
            </div>
          </div>
        </div>

        <div className="card">
          <h3 className="text-lg font-medium mb-4">Platform Statistics</h3>
          <div className="space-y-4">
            <div className="flex justify-between">
              <span>Total Platforms</span>
              <span className="font-medium">1</span>
            </div>
            <div className="flex justify-between">
              <span>Total Cases</span>
              <span className="font-medium">15</span>
            </div>
            <div className="flex justify-between">
              <span>Average Reputation</span>
              <span className="font-medium">1,250</span>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
