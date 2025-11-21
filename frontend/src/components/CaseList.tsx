import React, { useState, useEffect } from 'react';
import { UserSession } from '@stacks/connect';
import { CaseCard } from './CaseCard';
import { SubmitCaseModal } from './SubmitCaseModal';

interface CaseListProps {
  userSession: UserSession;
  userData: any;
}

export function CaseList({ userSession, userData }: CaseListProps) {
  const [cases, setCases] = useState<any[]>([]);
  const [loading, setLoading] = useState(true);
  const [showSubmitModal, setShowSubmitModal] = useState(false);

  useEffect(() => {
    loadCases();
  }, []);

  const loadCases = async () => {
    // Mock data for demonstration - in real app would fetch from contract
    setCases([
      {
        id: 1,
        platformId: 1,
        contentHash: '0x1234567890abcdef',
        submitter: 'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM',
        totalStake: 3000,
        status: 0, // PENDING
        createdAt: Date.now() - 86400000, // 1 day ago
        culturalRegion: 'US',
        moderatorCount: 2,
      },
      {
        id: 2,
        platformId: 1,
        contentHash: '0xabcdef1234567890',
        submitter: 'ST2CY5V39NHDPWSXMW9QDT3HC3GD6Q6XX4CFRK9AG',
        totalStake: 4500,
        status: 1, // RESOLVED
        createdAt: Date.now() - 172800000, // 2 days ago
        culturalRegion: 'EU',
        moderatorCount: 3,
        finalDecision: 1, // HARASSMENT
      },
    ]);
    setLoading(false);
  };

  const handleCaseSubmitted = () => {
    setShowSubmitModal(false);
    loadCases(); // Refresh cases
  };

  if (loading) {
    return (
      <div className="card">
        <div className="animate-pulse space-y-4">
          {[...Array(3)].map((_, i) => (
            <div key={i} className="h-32 bg-gray-200 rounded"></div>
          ))}
        </div>
      </div>
    );
  }

  return (
    <div className="space-y-6">
      {/* Header */}
      <div className="flex justify-between items-center">
        <h2 className="text-xl font-semibold">Moderation Cases</h2>
        <button
          onClick={() => setShowSubmitModal(true)}
          className="btn btn-primary"
        >
          Submit New Case
        </button>
      </div>

      {/* Cases List */}
      <div className="space-y-4">
        {cases.length === 0 ? (
          <div className="card text-center py-12">
            <p className="text-gray-500">No moderation cases found.</p>
            <p className="text-sm text-gray-400 mt-2">
              Be the first to submit a case for moderation!
            </p>
          </div>
        ) : (
          cases.map((caseData) => (
            <CaseCard
              key={caseData.id}
              caseData={caseData}
              userSession={userSession}
              userData={userData}
            />
          ))
        )}
      </div>

      {/* Submit Case Modal */}
      {showSubmitModal && (
        <SubmitCaseModal
          userSession={userSession}
          userData={userData}
          onClose={() => setShowSubmitModal(false)}
          onSubmitted={handleCaseSubmitted}
        />
      )}
    </div>
  );
}
