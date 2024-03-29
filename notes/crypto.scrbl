#lang notes

@block{@block-name{Various}

  EOS Costa Rica Developers Guide
  https://guide.eoscostarica.io/
  Web3 developers portal. Learn about blockchain technology and all the stuff
  that makes it possible.

  Hyperledger
  open-source project developed by the Linux foundation. It uses a modular
  approach to building blockchains collective of open-source blockchains and tools
  that anyone can use to create their own distributed ledgers.

  https://developers.ripple.com/xrp-ledger-overview.html
  Ripple XRP: real-time gross settlement system (RTGS)
  Remittance - transaction of money by foreigners; see SWIFT
  xrp
  SWIFT - cross border payments
  cryptopia
  lightning network and nodes - scalable off-chain instant payments
  second layer
  sharding
  tangle
  token vs. crypto currency ~ value representation
  tokenomics - e.g. collectables; game currency
  SC - Smart Contracts
  DLT supply chain

  Ricardian Contracts
  Digital documents that define the terms and conditions among those involved in
  the contract, these are signed and verified cryptographically and are readable
  by both humans and computer systems.

  Byzantine Fault Tolerances / Consensus methods:
  https://link.medium.com/V2dglrC5UU
  - distributed systems with imperfect information if a component failed
  | Proof-of-Work           | Bitcoin                                                                 |
  | Proof-of-Stake          | Cardano                                                                 |
  | Proof-of-Activity       | mix of PoW & PoS                                                        |
  | Proof-of-Burn           | proof of coins "burned". (Sent to unspendable addresses)                |
  | Proof-of-Space          | i.e. Proof-of-Capacity. Allocate memory or (plots offering) disk space. |
  |                         | Similar to PoW. Use storage instead of computation                      |
  | Proof-of-Time           |                                                                         |
  | Proof-of-Space-and-Time |                                                                         |

  BGP - The Byzantine Generals Problem:
  A groupf generals have to come to a common agreement on whether to attack or
  retreat, but can communicate only by sending messengers who might never arrive.
  Some of whom may be traitors, who have to reach a common decision.

  PoW (Proof of Work)
  - Consensus algorithm used by Bitcoin i.e. a solution of the BGP
  - Cost of registration, investing a scarce resource - electricity used to solve a math (arbitrary) puzzle creates trust
  - election of a leader - randomized, one CPU = one vote
  - mining pools - decrease randomisation, unwanted centralisation

  A generalised solution to distributed consensus:
  https://blog.acolyer.org/2019/03/08/a-generalised-solution-to-distributed-consensus/
  https://en.wikipedia.org/wiki/Consensus_(computer_science)
  https://news.ycombinator.com/item?id=19343398

  IOHK | Philipp Kant, Director of Formal Methods.
  https://www.youtube.com/watch?v=12nQ4oMhIpQ

  Cardano - uses Ouroboros algorith for PoS (Proof of Stake)
  The currency itself is the scare resource
  each time slot, randomly pick one coin, it's owner produces a block
  different leader selection - weighted by stake, the more coins you have the higher chance of getting elected for the next block
  holder of a large amount of coins has a better chance of attacking the system - if people notice the currency gets devaluated -> this forces you to play honest?
  SRSLY??? Quod licet Iovi, non licet bovi
  Explicit decentralized & unpredictable randomness generator everybody agrees on
  Ouroboros protocol
  - Split time into slots, each slot will have a block, a stakeholder is elected for a slot leader and has a right to create a new block.
    Slots grouped into an epoch, before the start of an epoch all stakeholder have to agree on some seed for the next random generator
  - Private dice roll, the send a proof of "the dice is cast" to the bchain, then the rand numers and combined and the seed is created
  - has a Proof of Security (written in Haskell)- for participants having less than a half of the stake (all ADA coins)
  Ouroboros Praos - Ouroboros extention: What happens if messages are delayed
  Process Calculi - model of distributed systems modeled in terms of runnable processes comunicating via channels
  measurement of process similarity or interchangeability, observational equivalence, equational reasoning, bisimilarity
  compositional language
  Process Calculi: examples: CCS, CSP, ACP, π - calculus
  Psi Calculus
  Cardano foundation - ?

  PoI (Proof of Intention)
  OS (Open Source) vs. OE (Open Execution): OS until it's run as a service
  BIP - BTC Improvement Proposas
  SegWit - Segregation Witness
  RGB - BTC based non-BTC assets
  RSK - BTC + SC (Smart Contrats)
  Hyperledger
  ICO - initial coin offering

  ;; BTC
  Split ledger into blocks
  Miners + Devs + Finance Guys
  PoW
  permission-less: Sybil attack "You can register 10000 times"

  twitter.com/polemitis
  Corda - Bank consortium
  Corda Blockchain
  ABCP Asset Backed Commercial Paper
  authenticated network
  certs
  notary notes
  network map nodes - like DNS


  IoV - Internet of value
  cointracking.info - Track portfolio & tax report
  ETF.com - exchange-traded funds
  IOTA
  SolarCoin - WTF?

  ;; Siraj Raval: The Problem with Blockchain
  https://www.youtube.com/watch?v=U2EYT5P4LvM
  humaniq - identifiable bank account for everyone
  bloom - risk assesement and credit scoring, credit worthiness
  bibliograph BABB - save / transfer / invest wealth creating cycle of wealth generation
  MintHealth - medical data in the blockchain (DLT)
  SUKU - blockchain for supplychain: where / when a product has been delivered
  Voatz - voting in federal election in Virginia
  ICO vs ETO: Initial Coin Offering vs Equity Token Offering

  Sharding - spliting network state into partitions (shards) containing independent piece of state and tx-history - higher through put
  Quadratic Sharding - ???

  Side chaing pegging into BTC blockchain - 2nd layer of operability
  Proof of Stake side chanel into BTC blockchain to create another DAPP

  PC Payment channel(s) - lightning network
  SC Side channel
  AS Atomic Swap
  Coin Join
  Nible Wimble - only unspent Tx UTXO

  Lightning Network
  1st layer is interchangeable (austauschbar)
  - coins on different channels
  gRPC
  Macaroon authentication key
  LND - lightning network daemon
  CND -
}

@block{@block-name{Monero XMR}
  privacy-enhancing technologies that obfuscate transactions to achieve
  anonymity and fungibility. Observers cannot decipher addresses trading monero,
  transaction amounts, address balances, or transaction histories.

  Blue Wallet

  PC on Ethereum - Payment Channel on Ethereum
  General State Channel

  Raid Network
  Payment Routing

  Conterfactual
  Celer Network - Layer 2; for Games
  Rained Network - for ???
  Multihop Payments
  Landmark M - Routing
  Stellar Network
  Stellar Consensus Protocol
  Stellar -> XRP (Ripple); supports arbitrage
  Lumen Currency - remittance
  Federated Byzantine Agreement
  Quorum - subgroup sufficient for consensus
  Starlight
  Cross assets Dollar -> Eur -> ...
  Virtual State Channels
  Pederson commitment r*G + v*H
  Quantun Resistance
  NTRU - latice bases cryptography ; not elliptic curves
  Post Quantum Commitment Schemes
  Schnorr signature
  EdDSA - Edwards-Curve Digital Signature Algorithm
}

@block{@block-name{Ethereum}
  @hyperlink["https://youtu.be/jxLkbJozKbY"]{YouTube: What is Ethereum? A Beginner's Explanation in Plain English}
  Ether - Ethereum currency
  DAO Decentralized Autonomous Organization "reverted" the money - see below
  Ethereum protocol
  @block{@block-name{Smart Contract SC}
    The contract "knows" if the money has been sent
     Immutable;
     "Code is Law"
     - SC Deployment to Ethereum Platform - must be payed with Ether
     @block{@block-name{Aspects}
       enforcement
       management
       performance
       payment
     }
  }
}

@block{@block-name{Ethereum Classic}
  ETC; The "original" Ethereum
  Created when the original ETH community decided that "code is no longer the law" and "reverted" the money
}
