import React, { useState } from 'react';
import { UserSession } from '@stacks/connect';

interface VoteModalProps {
  caseData: any;
  userSession: UserSession;
  userData: any;
  onClose: () => void;
}

const VIOLATION_TYPES = [
  { value: 0, label: 'None - Content is appropriate' },
  { value: 1, label: 'Harassment - Targeted abuse or threats' },
  { value: 2, label: 'Spam - Unsolicited promotional content' },
  { value: 3, label: 'Hate Speech - Discriminatory content' },
  { value: 4, label: 'Misinformation - False or misleading claims' },
  { value: 5, label: 'Adult Content - Inappropriate sexual content' },
  { value: 6, label: 'Violence - Promotion of violence or harm' },
];

export function VoteModal({ caseData, userSession, userData, onClose }: VoteModalProps) {
  const [violationType, setViolationType] = useState(0);
  const [stakeAmount, setStakeAmount] = useState(1000);
  const [culturalWeight, setCulturalWeight] = useState(50);
  const [loading, setLoading] = useState(false);

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    setLoading(true);

    try {
      // TODO: Implement contract call for voting
      console.log('Voting on case:', {
        caseId: caseData.id,
        violationType,
        stakeAmount,
        culturalWeight
      });

      // Simulate API call
      await new Promise(resolve => setTimeout(resolve, 2000));

      onClose();
    } catch (error) {
      console.error('Error voting:', error);
    } finally {
      setLoading(false);
    }
  };

  return (
    <div className="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center z-50">
      <div className="bg-white rounded-lg max-w-md w-full mx-4 max-h-[90vh] overflow-y-auto">
        <div className="p-6">
          <div className="flex justify-between items-center mb-6">
            <h2 className="text-xl font-semibold">Vote on Case #{caseData.id}</h2>
            <button
              onClick={onClose}
              className="text-gray-400 hover:text-gray-600"
            >
              ✕
            </button>
          </div>

          <form onSubmit={handleSubmit} className="space-y-6">
            <div>
              <label className="block text-sm font-medium text-gray-700 mb-2">
                Violation Type
              </label>
              <select
                value={violationType}
                onChange={(e) => setViolationType(Number(e.target.value))}
                className="input"
                required
              >
                {VIOLATION_TYPES.map((type) => (
                  <option key={type.value} value={type.value}>
                    {type.label}
                  </option>
                ))}
              </select>
            </div>

            <div>
              <label className="block text-sm font-medium text-gray-700 mb-2">
                Stake Amount (MOD tokens)
              </label>
              <input
                type="number"
                min="1000"
                max="100000"
                value={stakeAmount}
                onChange={(e) => setStakeAmount(Number(e.target.value))}
                className="input"
                required
              />
              <p className="text-xs text-gray-500 mt-1">
                Minimum: 1000 MOD • Maximum: 100,000 MOD
              </p>
            </div>

            <div>
              <label className="block text-sm font-medium text-gray-700 mb-2">
                Cultural Weight (0-100)
              </label>
              <input
                type="range"
                min="0"
                max="100"
                value={culturalWeight}
                onChange={(e) => setCulturalWeight(Number(e.target.value))}
                className="w-full h-2 bg-gray-200 rounded-lg appearance-none cursor-pointer"
              />
              <div className="flex justify-between text-xs text-gray-500 mt-1">
                <span>0</span>
                <span className="font-medium">{culturalWeight}</span>
                <span>100</span>
              </div>
              <p className="text-xs text-gray-500 mt-1">
                Higher weight for culturally sensitive content
              </p>
            </div>

            <div className="bg-blue-50 p-4 rounded-lg">
              <h4 className="font-medium text-blue-900 mb-2">Vote Summary</h4>
              <div className="text-sm text-blue-800 space-y-1">
                <p>Case: #{caseData.id}</p>
                <p>Decision: {VIOLATION_TYPES.find(t => t.value === violationType)?.label}</p>
                <p>Stake: {stakeAmount.toLocaleString()} MOD</p>
                <p>Cultural Weight: {culturalWeight}/100</p>
              </div>
            </div>

            <div className="flex space-x-3">
              <button
                type="button"
                onClick={onClose}
                className="flex-1 btn btn-secondary"
                disabled={loading}
              >
                Cancel
              </button>
              <button
                type="submit"
                className="flex-1 btn btn-primary"
                disabled={loading}
              >
                {loading ? 'Voting...' : 'Submit Vote'}
              </button>
            </div>
          </form>
        </div>
      </div>
    </div>
  );
}
