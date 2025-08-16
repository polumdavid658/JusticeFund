# JusticeFund

# 🤝 JusticeFund Smart Contract

## 🎯 Overview
JusticeFund is a decentralized crowdfunding platform for legal battles, built on Stacks blockchain. It enables anyone to create and contribute to legal fundraising campaigns transparently and securely.

## ⚡ Features
- 🔐 Create legal fundraising campaigns
- 💰 Accept STX token donations
- ⏱️ Time-bound campaigns
- 🎯 Target-based funding
- 💸 Automatic fund distribution
- 🔄 Refund mechanism for failed campaigns

## 📋 Usage

### Creating a Campaign
```clarity
(contract-call? .justice-fund create-campaign "Save Forest Park" "Legal battle against illegal logging" u1000000000 u10080)
```

### Making a Donation
```clarity
(contract-call? .justice-fund donate u1)
```

### Finalizing a Campaign
```clarity
(contract-call? .justice-fund finalize-campaign u1)
```

## 🔍 Query Functions
- `get-campaign-details`: View campaign information
- `get-donation-amount`: Check specific donation
- `get-donor-total-contribution`: View total contributions
- `get-total-funds-raised`: Check platform total

## ⚙️ Constants
- Minimum donation: 0.1 STX
- Minimum campaign duration: 1440 blocks (~10 days)
- Maximum campaign duration: 43200 blocks (~300 days)

## 🚀 Getting Started
1. Install Clarinet
2. Clone repository
3. Run `clarinet console`
4. Deploy contract
5. Interact using provided functions

