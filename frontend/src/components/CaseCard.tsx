import React, { useState } from 'react';
import { UserSession } from '@stacks/connect';
import { VoteModal } from './VoteModal';

interface CaseCardProps {
  caseData: any;
  userSession: UserSession;
  userData: any;
}

const VIOLATION_TYPES = [
  'None',
  'Harassment',
  'Spam',
  'Hate Speech',
  'Misinformation',
  'Adult Content',
  'Violence'
];

const STATUS_LABELS = {
  0: { label: 'Pending', className: 'badge-pending' },
  1: { label: 'Resolved', className: 'badge-resolved' },
  2: { label: 'Appealed', className: 'badge-appealed' },
};

export function CaseCard({ caseData, userSession, userData }: CaseCardProps) {
  const [showVoteModal, setShowVoteModal] = useState(false);

  const statusInfo = STATUS_LABELS[caseData.status as keyof typeof STATUS_LABELS];

  return (
    <div className="card">
      <div className="flex justify-between items-start mb-4">
        <div>
          <div className="flex items-center space-x-2 mb-2">
            <h3 className="text-lg font-medium">Case #{caseData.id}</h3>
            <span className={`badge ${statusInfo.className}`}>
              {statusInfo.label}
            </span>
          </div>
          <div className="text-sm text-gray-600 space-y-1">
            <p>Platform: #{caseData.platformId}</p>
            <p>Region: {caseData.culturalRegion}</p>
            <p>Moderators: {caseData.moderatorCount}/3</p>
          </div>
        </div>
        <div className="text-right">
          <div className="text-lg font-semibold text-primary-600">
            {caseData.totalStake.toLocaleString()} MOD
          </div>
          <div className="text-sm text-gray-600">
            Total Stake
          </div>
        </div>
      </div>

      <div className="mb-4">
        <div className="text-sm text-gray-600 mb-1">Content Hash:</div>
        <code className="text-xs bg-gray-100 px-2 py-1 rounded">
          {caseData.contentHash}
        </code>
      </div>

      <div className="mb-4">
        <div className="text-sm text-gray-600 mb-1">Submitted:</div>
        <div className="text-sm">
          {new Date(caseData.createdAt).toLocaleDateString()}
        </div>
      </div>

      {caseData.finalDecision !== undefined && (
        <div className="mb-4">
          <div className="text-sm text-gray-600 mb-1">Final Decision:</div>
          <div className="text-sm font-medium text-red-600">
            {VIOLATION_TYPES[caseData.finalDecision]}
          </div>
        </div>
      )}

      <div className="flex space-x-3">
        {caseData.status === 0 && (
          <button
            onClick={() => setShowVoteModal(true)}
            className="btn btn-primary"
          >
            Vote on Case
          </button>
        )}

        {caseData.status === 0 && caseData.moderatorCount >= 3 && (
          <button className="btn btn-success">
            Resolve Case
          </button>
        )}

        {caseData.status === 1 && (
          <button className="btn btn-warning">
            Appeal Decision
          </button>
        )}

        <button className="btn btn-secondary">
          View Details
        </button>
      </div>

      {showVoteModal && (
        <VoteModal
          caseData={caseData}
          userSession={userSession}
          userData={userData}
          onClose={() => setShowVoteModal(false)}
        />
      )}
    </div>
  );
}
