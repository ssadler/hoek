# Protocol for smart contracts using Crypto-Conditions

To facilitate participation in off-chain smart contracts on the [Komodo Platform](https://komodoplatform.com/en), providing a mechanism for dispute resolution.

1. [Introduction](#introduction)
1. [Off chain contract security](#off-chain-contract-security)
1. [Blockchain Poker](#blockchain-poker)
1. [Transactions](#transactions)
    1. [Stake](#transaction-stake)
    1. [StartGame](#transaction-startgame)
    1. [PlayerPayout](#transaction-playerpayout)
1. [Weaknesses](#weaknesses)

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

1. Player1, Player2, and Dealer all agree to sign transaction **PlayerPayout**, which spends the **Stake** according to the payout vector output of the **PVM**. The transaction is broadcast to the KMD network and no further action is required.

#### Game Closing - Timeout / Dispute

In a 2 player scenario, any single actor may dispute the game and the network will evaluate the posted game states.

1. A single actor, lets say Player1 decides it is neccesary to invoke an external judiciary entity, in this case the application blockchain. They create a transaction **GameState**, spending their dedicated output of **StartGame**. In a data output of this transaction, they attach the compressed output of the PVM, with signatures from all players.
1. Player2 and Dealer notice that Player1 has posted evidence. If they wish, they can also post **GameState** transactions. The evidence is simply a game state, which is the output of the **PVM**, signed by all players.
1. Any player may create a transaction **TriggerReview**, which starts a countdown of a number of blocks, after which the game states will be evaluated. Participants have until this timeout to post their game states.
1. When **TriggerReview** is accepted into the app chain, the game states will be evaluated on-chain using a call to **PVM**. A notary will take the payout vector is taken from the longest valid gamestate, and use it to compile transaction **NotaryPayout**. This transaction will then be broadcast to the KMD chain.


## Transactions

If you havn't already, this might be a good time to refer to the [transaction basics](./basics.md) document.

Transactions in this section are using a format suitable for passing to Hoek to sign and encode.

```shell
PLAYER1="03c8a965089173d746144cd667c8cedf985460ecc155811bd729e461f0079222f7"
PLAYER1_SK="Up3VgThhFXFXG7QN5ykym2hkiBrfB76GNhehyUXNG7AJMdLoVPU7"
PLAYER2="03d6de78061ca1695ba068d15ecf4a5431de9dccce7b45a73bb996e7e596acdba7"
PLAYER2_SK="UpyycopsYkknBsPd5Y2BLzKrTYVnhKoFL59H49JWn6TqXmJKxER4"
DEALER="025af7eed280ca8d1ebb294e9388378a2abf5455072c17bdf22506b6aa18dc8a24"
DEALER_SK="UrsT8pXPH1WvfTkkRzP2JsLTB6ebqhH3n6p31UcXpgyoESB9wvPp"

QUORUM=2  # 2/2+1 = 2

PARAMS="[extra params to pass to **PVM**]"
```

### Transaction: Stake

The **Stake** transaction is made on the KMD chain, and uses inputs from each player, and creates a single CryptoCondition output. The output may either be spent by a quorum of the participants (n/2+1 players + dealer), or by a subset of notaries.

```bash
PLAYER1_INPUT='[an input from player 1]'
PLAYER1_CHANGE='[change for player 1]'
PLAYER2_INPUT='[an input from player 2]'
PLAYER2_CHANGE='[change for player 2]'
STAKE_AMOUNT='[stake amount minus fees]'

PAYOUT_SCRIPT='{
    "condition": {
        {
            "type": "threshold-sha-256",
            "threshold": 1,
            "subfulfillments": [
                {
                    "type": "threshold-sha-256",
                    "threshold": 2,
                    "subfulfillments": [
                        {
                            "type": "secp256k1-sha-256",
                            "publicKey": "'$DEALER'"
                        },
                        {
                            "type": "threhsold-sha-256",
                            "threshold": '$QUORUM',
                            "subfulfillments": [
                                {"type": "secp256k1-sha-256", "publicKey": "'$PLAYER1'"},
                                {"type": "secp256k1-sha-256", "publicKey": "'$PLAYER2'"}
                            ]
                        }
                    ]
                },
                {"type": "eval-sha-256", "method": "subsetNotarySigs"}
            ]
        }
    }
}'


STAKE_TX='{
    "inputs": [
        '"$PLAYER1_INPUT"',
        '"$PLAYER2_INPUT"'
    ],
    "outputs": [
        {
            "amount": '$STAKE_AMOUNT',
            "script": '$PAYOUT_SCRIPT'
        },
        '"$PLAYER1_CHANGE"',
        '"$PLAYER2_CHANGE"'
    ]
}';
```

In Haskell:

```haskell
    -- payout is either made by notaries, or dealer + quorum of players
let payoutCond = Threshold 1 [ EvalNode "subsetNotarySigs" ""
                             , Threshold 2 [ Secp256k1 dealer
                                           , Threshold 2 [ Secp256k1 player1
                                                         , Secp256k1 player2
                                                         ]
                                           ]
                             ]
    stakeTx = KTx
        -- players fund game
        [ player1Input
        , player2Input
        ]
        [ TXOutput stakeAmount $ CCOutput payoutCond
        -- players send themselves change
        , player1Change
        , player2Change
        ]
```


### Transaction: StartGame

The **StartGame** transaction is made on the PANGEA chain, and contains the ID of the **Stake** transaction as a data output. The dealer is expected to hold the PANGEA units neccesary to make this transaction. The dealer also provides outputs that are sufficient for the players to post gamestates in the event of a dispute. An exec output is provided that will trigger an on-chain evaluation a subsequent payout; it includes a delay of a number of blocks before it can be triggered.

Note: Currently, this transaction may or may not be used; in the case that it is not used, it would be good to provide the dealer with a way to recollect the outputs, even though they maybe just amount to dust.

```bash
DEALER_INPUT='[an input from dealer]'
DEALER_CHANGE='[change for dealer]'
STAKE_TXID="[txid of STAKE_TX]"

DATAFEE=[a fee suficient to post a data output]
EXECFEE=[a fee suficient to post an eval]

STARTGAME_TX='{
    "inputs": ['$DEALER_INPUT'],
    "outputs": [
        {"script": {"condition": {"type": "secp256k1-sha-256", "publicKey": "'$DEALER'"}},
         "amount": $DATAFEE},
        {"script": {"condition": {"type": "secp256k1-sha-256", "publicKey": "'$PLAYER1'"}},
         "amount": $DATAFEE},
        {"script": {"condition": {"type": "secp256k1-sha-256", "publicKey": "'$PLAYER2'"}},
         "amount": $DATAFEE},
        {
            "amount": $EXECFEE,
            "script": {
                "type": "threshold-sha-256",
                "threshold": 2,
                "subfulfillments": [
                    {"type": "nLockTime", "blocks": $EVAL_DELAY_BLOCKS},
                    {
                        "type": "threshold-sha-256",
                        "threshold": 1,
                        "subfulfillments": [
                            {"type": "secp256k1-sha-256", "publicKey": "'$DEALER'"},
                            {"type": "secp256k1-sha-256", "publicKey": "'$PLAYER1'"},
                            {"type": "secp256k1-sha-256", "publicKey": "'$PLAYER2'"}
                        ]
                    }
                ]
            }
        },
        {"script": {"return": "'$STAKE_TXID'"}, "amount": $DATAFEE},
    ]
}'
```

In Haskell:

```haskell
let addrOutput n pk = TxOutput n $ AddressOutput $ pubKeyAddr pk
    dataFee = 4
    evalFee = 10
    delayBlocks = undefined
    startGameTx = KTx
        -- Dealer provides units of PANGEA
        [ dealerInput ]
        -- Output for each player to post game state binary
        [ addrOutput dataFee dealer
        , addrOutput dataFee player1
        , addrOutput dataFee player2
        -- lock time a certain number of blocks so players can post evidence,
        -- and require a sig from any participant to initiate Exec
        Threshold 2 [ ExecNode "nLockTime" delayBlocks
                    , Threshold 1 [ Secp256k1 dealer
                                  , Secp256k1 player1
                                  , Secp256k1 player2
                                  ]
                    ]
        -- StartGame references the Stake txid
        , CarrierOutput (txHash stakeTx)
        ]
```

### Transaction: PlayerPayout

The **PlayerPayout** transaction is made on the KMD chain. It is independent of the **StartGame** transaction. It distributes the stake according to a payout vector that is agreed upon by a majority of the players + the dealer.

```bash
PLAYERPAYOUT_TX='{
    "inputs": [{
        "txid": "'$STAKE_TXID'",
        "idx": 0,
        "script": '$PAYOUT_SCRIPT'
    }],
    "outputs": [
        {"script": {"condition": {"type": "secp256k1-sha-256", "publicKey": "'$DEALER'"}},
         "amount": '$DEALER_PAYOUT'},
        {"script": {"condition": {"type": "secp256k1-sha-256", "publicKey": "'$PLAYER1'"}},
         "amount": '$PLAYER1_PAYOUT'},
        {"script": {"condition": {"type": "secp256k1-sha-256", "publicKey": "'$PLAYER2'"}},
         "amount": '$PLAYER2_PAYOUT'}
    ]
}'
```

In Haskell:

```haskell
let playerPayoutTx = KTx
    -- Spending the stake
    [ TxInput (OutPoint stakeTxid 0) payoutCondition ]
    -- Payout each participant
    [ mkPayout dealerPayout
    , mkPayout player1Payout
    , mkPayout player2Payout
    ]
```

### Questions

* What happens if the players broadcast Stake but the dealer never broadcasts StartGame?
* How does the notary know that the dispute pertains to the correct transaction on KMD? Put another way,
  what's to stop a group from trying to steal another group's Stake?
