import React, { useState } from 'react';
import { UserSession } from '@stacks/connect';

interface SubmitCaseModalProps {
  userSession: UserSession;
  userData: any;
  onClose: () => void;
  onSubmitted: () => void;
}

const CULTURAL_REGIONS = [
  'US', 'EU', 'AS', 'AF', 'SA', 'OC', 'NA'
];

export function SubmitCaseModal({ userSession, userData, onClose, onSubmitted }: SubmitCaseModalProps) {
  const [platformId, setPlatformId] = useState(1);
  const [contentHash, setContentHash] = useState('');
  const [culturalRegion, setCulturalRegion] = useState('US');
  const [loading, setLoading] = useState(false);

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    setLoading(true);

    try {
      // TODO: Implement contract call for case submission
      console.log('Submitting case:', {
        platformId,
        contentHash,
        culturalRegion
      });

      // Simulate API call
      await new Promise(resolve => setTimeout(resolve, 2000));

      onSubmitted();
    } catch (error) {
      console.error('Error submitting case:', error);
    } finally {
      setLoading(false);
    }
  };

  const generateContentHash = () => {
    // Simple hash generation for demo
    const randomBytes = new Uint8Array(32);
    for (let i = 0; i < 32; i++) {
      randomBytes[i] = Math.floor(Math.random() * 256);
    }
    const hash = Array.from(randomBytes)
      .map(b => b.toString(16).padStart(2, '0'))
      .join('');
    setContentHash('0x' + hash);
  };

  return (
    <div className="fixed inset-0 bg-black bg-opacity-50 flex items-center justify-center z-50">
      <div className="bg-white rounded-lg max-w-md w-full mx-4 max-h-[90vh] overflow-y-auto">
        <div className="p-6">
          <div className="flex justify-between items-center mb-6">
            <h2 className="text-xl font-semibold">Submit Moderation Case</h2>
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
                Platform ID
              </label>
              <input
                type="number"
                min="1"
                value={platformId}
                onChange={(e) => setPlatformId(Number(e.target.value))}
                className="input"
                required
              />
              <p className="text-xs text-gray-500 mt-1">
                The platform where this content was found
              </p>
            </div>

            <div>
              <label className="block text-sm font-medium text-gray-700 mb-2">
                Content Hash
              </label>
              <div className="flex space-x-2">
                <input
                  type="text"
                  value={contentHash}
                  onChange={(e) => setContentHash(e.target.value)}
                  placeholder="0x..."
                  className="input flex-1"
                  required
                />
                <button
                  type="button"
                  onClick={generateContentHash}
                  className="btn btn-secondary whitespace-nowrap"
                >
                  Generate
                </button>
              </div>
              <p className="text-xs text-gray-500 mt-1">
                SHA-256 hash of the content to be moderated
              </p>
            </div>

            <div>
              <label className="block text-sm font-medium text-gray-700 mb-2">
                Cultural Region
              </label>
              <select
                value={culturalRegion}
                onChange={(e) => setCulturalRegion(e.target.value)}
                className="input"
                required
              >
                {CULTURAL_REGIONS.map((region) => (
                  <option key={region} value={region}>
                    {region}
                  </option>
                ))}
              </select>
              <p className="text-xs text-gray-500 mt-1">
                Geographic region for cultural context consideration
              </p>
            </div>

            <div className="bg-yellow-50 p-4 rounded-lg">
              <h4 className="font-medium text-yellow-900 mb-2">⚠️ Important Notes</h4>
              <ul className="text-sm text-yellow-800 space-y-1">
                <li>• Ensure the content hash is accurate and verifiable</li>
                <li>• Choose the appropriate cultural region for context</li>
                <li>• Cases require moderator participation to be resolved</li>
                <li>• False submissions may result in reputation penalties</li>
              </ul>
            </div>

            <div className="bg-blue-50 p-4 rounded-lg">
              <h4 className="font-medium text-blue-900 mb-2">Case Summary</h4>
              <div className="text-sm text-blue-800 space-y-1">
                <p>Platform: #{platformId}</p>
                <p>Content Hash: {contentHash || 'Not set'}</p>
                <p>Cultural Region: {culturalRegion}</p>
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
                disabled={loading || !contentHash}
              >
                {loading ? 'Submitting...' : 'Submit Case'}
              </button>
            </div>
          </form>
        </div>
      </div>
    </div>
  );
}
