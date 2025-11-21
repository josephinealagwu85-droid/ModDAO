# ModDAO - Decentralized Content Moderation Platform

A comprehensive Stacks blockchain-based decentralized content moderation system built with Clarity smart contracts and a modern React frontend.

## 🚀 Features

### Smart Contract Features
- **Stake-weighted Voting**: Moderators stake MOD tokens on violation classifications
- **Cultural Context Awareness**: Region-specific moderation with cultural weight considerations
- **Transparent Appeals**: Multi-level appeal system with stake requirements
- **Reputation System**: Track moderator accuracy and reliability
- **Emergency Controls**: Admin emergency pause and withdrawal functions
- **Batch Operations**: Gas-optimized batch case submission and voting
- **Rate Limiting**: Prevent spam and abuse with configurable rate limits
- **Blacklist System**: Admin controls for user management

### Security Enhancements
- ✅ Comprehensive input validation
- ✅ Reentrancy protection
- ✅ Safe math operations (overflow/underflow protection)
- ✅ Access control with admin roles
- ✅ Emergency pause functionality
- ✅ Blacklist/whitelist user management
- ✅ Rate limiting and spam prevention

### Frontend Dashboard
- 🔗 Wallet connection with Stacks Connect
- 📊 Real-time contract status monitoring
- 🎯 Case submission and voting interface
- 📈 Moderator statistics and reputation tracking
- 🏢 Platform management tools
- ⚙️ Admin panel for contract management
- 🎨 Modern UI with Tailwind CSS

## 🏗️ Architecture

```
ModDAO/
├── contracts/           # Clarity smart contracts
│   └── ModDAOcontract.clar
├── tests/              # Test suites
│   ├── security.test.ts
│   ├── platform.test.ts
│   ├── voting.test.ts
│   └── appeals.test.ts
├── frontend/           # React dashboard
│   ├── src/
│   │   ├── components/
│   │   │   ├── Dashboard.tsx
│   │   │   ├── CaseList.tsx
│   │   │   ├── VoteModal.tsx
│   │   │   └── ...
│   │   ├── App.tsx
│   │   └── main.tsx
│   ├── index.html
│   ├── vite.config.js
│   └── tailwind.config.js
└── package.json
```

## 🛠️ Development Setup

### Prerequisites
- Node.js 18+
- Clarinet (Stacks development environment)
- A Stacks wallet (for testing)

### Installation

1. **Clone and install dependencies:**
```bash
cd ModDAO
npm install
```

2. **Run smart contract tests:**
```bash
npm test
```

3. **Start the frontend development server:**
```bash
npm run dev
```

4. **Build for production:**
```bash
npm run build
```

### Contract Deployment

1. **Test the contract:**
```bash
clarinet test
```

2. **Deploy to testnet:**
```bash
clarinet deployments generate --devnet
clarinet deployments apply --devnet
```

## 📋 Contract Functions

### Core Functions
- `register-platform(name)` - Register a new moderation platform
- `submit-moderation-case(platform-id, content-hash, cultural-region)` - Submit content for moderation
- `vote-on-case(case-id, violation-type, stake-amount, cultural-weight)` - Vote on case violation type
- `resolve-case(case-id)` - Resolve case with stake-weighted decision
- `appeal-case(case-id, appeal-reason)` - Appeal a resolved case decision

### Batch Operations
- `batch-submit-cases(platform-id, content-hashes, cultural-regions)` - Submit multiple cases efficiently
- `batch-vote-on-cases(votes)` - Vote on multiple cases in one transaction

### Security & Admin
- `pause-contract()` - Emergency pause all operations
- `enable-emergency-mode()` - Activate emergency controls
- `add-admin(admin)` - Grant admin privileges
- `blacklist-user(user)` - Prevent user from participating
- `update-min-moderators(count)` - Configure minimum moderators per case

## 🎯 Violation Categories

1. **None** - Content is appropriate
2. **Harassment** - Targeted abuse or threats
3. **Spam** - Unsolicited promotional content
4. **Hate Speech** - Discriminatory content
5. **Misinformation** - False or misleading claims
6. **Adult Content** - Inappropriate sexual content
7. **Violence** - Promotion of violence or harm

## 🔐 Security Features

- **Rate Limiting**: Configurable operation limits per block
- **Stake Requirements**: Minimum/maximum stake amounts
- **Cultural Weights**: Context-aware moderation (0-100 scale)
- **Reentrancy Guards**: Prevent reentrancy attacks
- **Safe Math**: Overflow/underflow protection
- **Input Validation**: Comprehensive parameter validation
- **Emergency Controls**: Circuit breakers for critical situations

## 📊 Token Economics

- **MOD Token**: Native governance and staking token
- **Initial Supply**: 1,000,000 MOD
- **Minimum Stake**: 1,000 MOD per vote
- **Appeal Multiplier**: 2x total case stake required for appeals
- **Rewards**: Stake-weighted reward distribution

## 🌐 Frontend Features

- **Responsive Design**: Works on desktop and mobile
- **Real-time Updates**: Live contract status monitoring
- **Interactive Modals**: Seamless case submission and voting
- **Statistics Dashboard**: Comprehensive moderator analytics
- **Admin Interface**: Contract management tools
- **Wallet Integration**: Secure Stacks wallet connection

## 🧪 Testing

Comprehensive test suite covering:
- ✅ Security features and edge cases
- ✅ Platform registration and management
- ✅ Case submission and voting workflows
- ✅ Appeal system functionality
- ✅ Batch operations
- ✅ Admin controls and emergency functions

Run tests with:
```bash
npm test          # Run all tests
npm run test:report  # With coverage report
```

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch
3. Add tests for new functionality
4. Ensure all tests pass
5. Submit a pull request

## 📄 License

ISC License - see LICENSE file for details.

## 🔗 Links

- [Stacks Documentation](https://docs.stacks.co/)
- [Clarity Language Reference](https://docs.stacks.co/docs/write-smart-contracts/clarity-language)
- [Stacks Connect](https://docs.stacks.co/docs/connect/)
- [Clarinet](https://docs.hiro.so/stacks/clarinet/overview)
