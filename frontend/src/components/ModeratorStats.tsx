import React from 'react';
import { UserSession } from '@stacks/connect';

interface ModeratorStatsProps {
  userSession: UserSession;
  userData: any;
}

export function ModeratorStats({ userSession, userData }: ModeratorStatsProps) {
  return (
    <div className="space-y-6">
      <h2 className="text-xl font-semibold">My Moderator Statistics</h2>

      <div className="grid grid-cols-1 md:grid-cols-3 gap-6">
        <div className="card text-center">
          <div className="text-3xl font-bold text-primary-600 mb-2">47</div>
          <div className="text-gray-600">Cases Moderated</div>
        </div>

        <div className="card text-center">
          <div className="text-3xl font-bold text-success mb-2">92%</div>
          <div className="text-gray-600">Accuracy Rate</div>
        </div>

        <div className="card text-center">
          <div className="text-3xl font-bold text-warning mb-2">15,750</div>
          <div className="text-gray-600">MOD Tokens Earned</div>
        </div>
      </div>

      <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
        <div className="card">
          <h3 className="text-lg font-medium mb-4">Recent Activity</h3>
          <div className="space-y-3">
            <div className="flex justify-between items-center py-2 border-b">
              <div>
                <div className="font-medium">Voted on Case #42</div>
                <div className="text-sm text-gray-600">Decision: Harassment • Stake: 2,500 MOD</div>
              </div>
              <span className="text-sm text-green-600">+150 MOD</span>
            </div>
            <div className="flex justify-between items-center py-2 border-b">
              <div>
                <div className="font-medium">Voted on Case #41</div>
                <div className="text-sm text-gray-600">Decision: Spam • Stake: 1,800 MOD</div>
              </div>
              <span className="text-sm text-green-600">+120 MOD</span>
            </div>
            <div className="flex justify-between items-center py-2">
              <div>
                <div className="font-medium">Appealed Case #38</div>
                <div className="text-sm text-gray-600">Stake: 5,400 MOD</div>
              </div>
              <span className="text-sm text-red-600">-5,400 MOD</span>
            </div>
          </div>
        </div>

        <div className="card">
          <h3 className="text-lg font-medium mb-4">Reputation by Category</h3>
          <div className="space-y-3">
            <div className="flex justify-between items-center">
              <span>Harassment</span>
              <div className="flex items-center">
                <div className="w-24 bg-gray-200 rounded-full h-2 mr-2">
                  <div className="bg-red-500 h-2 rounded-full" style={{width: '95%'}}></div>
                </div>
                <span className="text-sm font-medium">95%</span>
              </div>
            </div>
            <div className="flex justify-between items-center">
              <span>Spam</span>
              <div className="flex items-center">
                <div className="w-24 bg-gray-200 rounded-full h-2 mr-2">
                  <div className="bg-yellow-500 h-2 rounded-full" style={{width: '88%'}}></div>
                </div>
                <span className="text-sm font-medium">88%</span>
              </div>
            </div>
            <div className="flex justify-between items-center">
              <span>Hate Speech</span>
              <div className="flex items-center">
                <div className="w-24 bg-gray-200 rounded-full h-2 mr-2">
                  <div className="bg-orange-500 h-2 rounded-full" style={{width: '92%'}}></div>
                </div>
                <span className="text-sm font-medium">92%</span>
              </div>
            </div>
            <div className="flex justify-between items-center">
              <span>Misinformation</span>
              <div className="flex items-center">
                <div className="w-24 bg-gray-200 rounded-full h-2 mr-2">
                  <div className="bg-blue-500 h-2 rounded-full" style={{width: '85%'}}></div>
                </div>
                <span className="text-sm font-medium">85%</span>
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
