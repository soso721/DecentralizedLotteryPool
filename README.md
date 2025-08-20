# LotteryPool Smart Contract

A decentralized lottery system built on Stacks blockchain implementing a commit-reveal scheme with rollover functionality and configurable fees.

## Features

- **Commit-Reveal Security**: Players commit to secret values when buying tickets and reveal them later, preventing manipulation
- **Rollover System**: Unclaimed pots automatically roll over to the next round
- **Configurable Fees**: Admin can set fee percentages that go to a treasury
- **Transparent Randomness**: Uses mixed entropy from player reveals and block hashes
- **Event Logging**: Comprehensive event emission for off-chain tracking

## How It Works

1. **Round Creation**: Admin opens a new lottery round with configurable parameters
2. **Ticket Sales**: Players buy tickets by submitting commitment hashes during the sale phase
3. **Reveal Phase**: Players reveal their secrets, which are validated against their commitments
4. **Drawing**: After the reveal period ends, anyone can trigger the draw using mixed entropy
5. **Payout**: Winner receives the pot minus fees, or the pot rolls over if no valid reveals

## Contract Architecture

### State Variables
- `admin`: Contract administrator
- `treasury`: Fee recipient address
- `round-id`: Current round counter
- `rollover`: Amount carried over from previous rounds

### Data Maps
- `rounds`: Round configuration and state
- `tickets`: Ticket ownership mapping
- `commits`: Player commitments and reveal status

## Usage Examples

### Admin Functions

#### Open a New Round
```clarity
;; Open round with 1 STX ticket price, max 100 tickets, 144 blocks sale, 72 blocks reveal, 2.5% fee
(contract-call? .lottery-pool open-round u1000000 u100 u144 u72 u250)
