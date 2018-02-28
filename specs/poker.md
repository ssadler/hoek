# Protocol for smart contracts using Crypto-Conditions

To facilitate participation in off-chain smart contracts on the [Komodo Platform](https://komodoplatform.com/en), providing a mechanism for dispute resolution.

1. [Introduction](#introduction)
1. [Off chain contract security](#off-chain-contract-security)
1. [Blockchain Poker](#blockchain-poker)
1. [Transactions](#transactions)

## Introduction

This section will introduce the relevant parts of the protocol, and the properties we want them to have.

There are two blockchains, a blockchain providing a value token (**KMD**) and a blockchain providing an evaluation function (**PANGEA**). KMD and PANGEA may be used to refer to the ledger or the currency unit on the ledger, as is the case with Bitcoin.

There are two or more **Players**, who own some KMD.

There is a **Dealer**, who owns some PANGEA.

The Players and the Dealer can be seen, more generally, as parties to a contract, who have different roles and interests.

### Properties

These are the properties that we want the solution to provide.

* Players only need to hold a single value token i.e. units of KMD.
* Only the Dealer needs to hold PANGEA. They may obtain this token via [atomic swap exchange](https://komodoplatform.com/en/barterdex).
* No new domain specific evaluation functions in value chain KMD (but we may introduce general purpose functions).
* Minimal data overhead in value chain KMD.
* On-chain dispute resolution protocol for domain specific evaluation.

## Smart Contract Workflow

In an ideal world, in order to facilitate off chain smart contracts we could simply have the players fund an escrow transaction, do their off-chain processing, and submit an escrow release with an updated payout vector.

However, in reality, for certain kinds of problems, we require a dispute resolution protocol. This is because players may timeout, or a player may refuse to sign a payout transaction if they lose a bet.

In practice, there isn't alot of distinction between a timeout and a dispute; a player that refuses to sign a payout and a player that has timed out are indistinguishable from a cryptographic perspective. In either case, if an n/2+1 majority remains, they get to decide what happens.

If, after a single player has timed out, there does not remain an n/2+1 majority, then we have to fall back to the dispute resolution protocol.

The below table illustrates:

| # players | # sigs for payout | # timeouts for dispute |
|-----------|-------------------|------------------------|
| 2         | 2                 | 1                      |
| 3         | 2                 | 2                      |
| 4         | 3                 | 2                      |

### Blockchain Poker

Let there be 3 actors: **Dealer**, **Player1**, **Player2**. Each has a public and a private key.

#### Game Opening

1. Player1 and Player2 join Dealer's game. The game has parameters **Params**, for example, the dealer's commission.
1. Player1 and Player2 jointly sign a transaction (**Stake**) on the KMD blockchain. The transaction includes a data output with **Params**. The transaction is reviewed by the dealer, and broadcast to the KMD network.
1. Dealer creates a transaction (**StartGame**) on the PANGEA network. The transaction has dedicated dispute outputs for each of the players, and the dealer. It also has an output with a timelock, which triggers a review. The transaction is reviewed by the players and broadcast to the PANGEA network.
1. Game is played privately between players using **PVM** (Poker Virtual Machine).

#### Game Closing - Common case

The quorum required to close the game consists of **n/2+1 players + dealer**. In a 2 player scenario that means both players plus the dealer.

1. Player1, Player2, and Dealer all agree to sign transaction **Payout**, which spends the **Stake** according to the payout vector output of the **PVM**. The transaction is broadcast to the KMD network and no further action is required.

#### Game Closing - Timeout / Dispute

In a 2 player scenario, any single actor may dispute the game and the network will evaluate the posted game states.

1. A single actor, lets say Player1 decides it is neccesary to invoke an external judiciary entity, in this case the application blockchain. They create a transaction **GameState**, spending their dedicated output of **StartGame**. In a data output of this transaction, they attach the compressed output of the PVM, with signatures from all players.
1. Player2 and Dealer notice that Player1 has posted evidence. If they wish, they can also post **GameState** transactions. The evidence is simply a game state, which is the output of the **PVM**, signed by all players.
1. Any player may create a transaction **TriggerReview**, which starts a countdown of a number of blocks, after which the game states will be evaluated. Participants have until this timeout to post their game states.
1. When **TriggerReview** is accepted into the app chain, the game states will be evaluated on-chain using a call to **PVM**. A notary will take the payout vector is taken from the longest valid gamestate, and use it to compile transaction **Payout**. This transaction will then be broadcast to the KMD chain.
