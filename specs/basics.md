# Transaction Basics for Smart Contracts on Komodo

This document has a basic overview of transaction data structures and how to use the [Hoek](https://github.com/libscott/hoek) command line tool.

1. [Data Structures](#data-structures)
1. [Hoek command line tool](#hoek-command-line-tool)


## Data structures

This document assumes some familiarity with the Bitcoin data structure, and uses Haskell to illustrate data structures. It should be considered pseudocode rather than API reference.

### Transaction Basics

To recap, the Bitcoin application is a transcation graph that manipulates a list of transaction outputs that are available to spend ([UTXO](https://bitcoin.org/en/glossary/unspent-transaction-output)s). Each transaction has inputs which spend outputs of other transcations. There are some special transaction inputs that do not spend outputs, but we are not concerned with those for the moment. 

```haskell
data Transaction = Tx
  { inputs  :: [Input]            -- List of transaction inputs
  , outputs :: [Output]           -- List of transaction outputs
  }
```

An **output** contains an amount and an **output script** which locks that amount:

```haskell
data Output = Output
  { amount       :: Int           -- A non zero positive number of units
  , outputScript :: OutputScript  -- A script which locks the output
  }
```

An **input** contains a pointer to a transaction outputs, and an **input script**:

```haskell
data Input = Input
  { outPoint    :: (Txid, Int)    -- A tuple of a transaction ID and the
                                  -- index of an output that is being spent
  , inputScript :: InputScript    -- A script which unlocks the output
  }
```

Those are the absolute basics of the transaction graph at a high level. This remains fixed. However, we do need some new functionality in order to have more flexibility in the scripting language. Let's recap the basics of the scripting language.

### Script Basics

The spending of an ouput involves the output script, and an input script. A very simple example is a hashlock, where the output script contains a hash, and the input must provide a corresponding pre-image:

```haskell
> outputScript = [OP_HASH256, OP_PUSHDATA "2a47601373e62b1f5d58da7f78342b250f7a6635237609fd6c2b2d712d07c2af", OP_EQUAL]
> inputSript = [OP_PUSHDATA "i am a secret"]
> stack = [] -- initialise an empty stack
> evalScript inputScript stack
> print stack
["i am a secret"]
> result = evalScript inputScript stack
```

In terms of the execution, what just happened was:

```
> OP_PUSHDATA "i am a secret"
["i am a secret"]
> OP_HASH256
["2a47601373e62b1f5d58da7f78342b250f7a6635237609fd6c2b2d712d07c2af"]
> OP_PUSHDATA "2a47601373e62b1f5d58da7f78342b250f7a6635237609fd6c2b2d712d07c2af"]
["2a47601373e62b1f5d58da7f78342b250f7a6635237609fd6c2b2d712d07c2af", "2a47601373e62b1f5d58da7f78342b250f7a6635237609fd6c2b2d712d07c2af"]
> OP_EQUAL
True
```

That above demonstrates how bitcoin scripting works, but it only demonstrates one kind of security. There are actually a few different ones. This is a good time to note that Bitcoin has a concept of a "standard" script, that is, it does not allow arbitrary use of it's scripting language. There are some templates which are accepted and others which are not.

### Script Datatypes

Here are our script datatypes. Each datatype can encode to a script, which the network will accept as standard. We aren't detailing all standard types of script here, just the ones we are concerned with.

```haskell
-- Note that the below datatypes are polymorphic, so when you see the | operator,
-- that means there are many possible constructors, ie, either an A or a B.

data OutputScript =
    AddressOutput Address 
    -- Address is the hash of an secp256k1 public key, like: RXSwmXKtDURwXP7sdqNfsJ6Ga8RaxTchxE
    -- Input must provide corresponding public key, plus a signature of the transaction ID
    -- Script: [DUP HASH160 PUSHDATA(RXSwmXKtDURwXP7sdqNfsJ6Ga8RaxTchxE) EQUALVERIFY CHECKSIG]
  
  | CCOutput Condition
    -- Crypto-Condition output; Condition is the hash of a Crypto-Condition, which specifies a structure.
    -- Input must provide a corresponding Fulfillment which corresponds to the condition
    -- Script: [PUSHDATA(ni:///sha-256;uxrFJgwBQbflSybsIzBjfFWXv4EZUawJ50StIP934oc?fpt=preimage-sha-256&cost=1024),
    --          OP_CHECKCRYPTOCONDITION]

data InputScript =
    AddressInput PubKey Signature
    -- Fulfills an AddressOutput
    -- Script: Pushes a PublicKey and a Signature
    
  | CCInput Condition
    -- Fulfills a CCOutput
    -- Script: Pushes a Crypto-Condition fulfillment
```

### Crypto-Conditions

Experienced readers will notice that we have already introduced a new security mechanism into the scripting language. That is an [Interledger Crypto-Condition](https://tools.ietf.org/html/draft-thomas-crypto-conditions-04). It is a multi-level, multi-algorithm data structure which allows arbitrary n-of-m condition trees.

There are a few different reasons why it is desirable to implement a new op-code for Crypto-Conditions rather than getting creative with bitcoin scripting. 

* It's a standardised protocol, already being used by several other networks.
* The crypto-condition makes it easier to encode useful security conditions, and is easier to inspect at runtime as well.

The data structure we will be working with is a `Condition`. This condition has some different types from the reference implementation, it uses Secp256k1 for public key cryptography (as in komodo), and it has an EvalNode

```haskell
data CryptoCondition =
    Secp256k1Condition PublicKey (Maybe Signature)
    -- Condition contains the hash of a secp256k1 public key. Fulfillment provides the signature.
  
  | PreimageCondition Preimage
    -- Condition contains the hash of the pre-image. Fulfillment provides the preimage.
  
  | ThresholdCondition N [CryptoCondition]
    -- N of M structure; requires exactly N of the subcondititons to be fulfilled.
  
  | EvalCondition Method Params Entropy
    -- Passes the transaction to a custom on-chain evaluation function specified by Method.
```

The condition may be in several different states:

* **Binary condition**: Just a script hash, doesn't tell you how to fulfill it. This is what you get in the script output.
* **Unfulfilled condition**: This is what you'll specify when creating conditions, ie, before they are signed and encoded. It is the equivalent of a [redeem script](https://bitcoin.org/en/glossary/redeem-script).
* **Fulfilled condition**: Contains any neccesary signatures. Can be encoded to a **fulfillment binary**. This is what you get in the script input.

Practically speaking, the available morphisms are:

```
* binary fulfillment -> binary condition   (done when verifying a condition)
* redeem script      -> binary condition   (done when creating a script output)
* redeem script      -> binary fulfillment (requires signatures; done when creating a script input)
```

And that covers the overview of the data structures. The next section will introduce the Hoek command line tool to create transactions for use in the contract protocol.

# Hoek command line tool

[Hoek](https://github.com/libscott/hoek) is a command line tool and library for creating bitcoin transactions that use Crypto-Conditions (and ones that don't). Behind the scenes, it depends on [Haskoin](https://github.com/haskoin/haskoin), with some minor changes.

Hoek uses a JSON interface to create transactions:

```shell
UNSIGNED='{
    "inputs": [
        {
            "txid": "c60e318a314c1ab653bf1c18e945e3d1e53b84e71a455446465f71e3180b2b20",
            "idx": 1,
            "script": {"address": "RLiE7UB8L3sEY4aCVEKn5w4STwKc7LzyLG"}
        }
    ],
    "outputs": [
        {
            "amount": 10,
            "script": {
                "condition": {
                    "type": "secp256k1-sha-256",
                    "publicKey": "025ee711359543487640d2d16a555833edf3c6e147f30a5b6ec813606a984e731a"
                }
            }
        }
    ]
}'
hoek encodeTx "$UNSIGNED"
Err (Object (fromList [("msg",String "Can't encode unsigned input: PubKeyHashInput 7d657af495ef143e8255511b2b98931656b99733"),("class",String "OtherError")]))
```

Oh no! What happened? Well, the `encodeTx` method can't encode an input that is not signed. Let's sign it:

```shell
SIGNED=`hoek signTxBitcoin '{
    "tx": '"$UNSIGNED"',
    "privateKeys": ["Uw8X3xAdYphLyA36TeSbofHNF54LYYhZU6TbN1n9r1e7EtnBrYdD"]
}'`
echo $SIGNED | jq
{
  "inputs": [
    {
      "script": "483045022100fb748569a7684f27ce620394122aa69a9e8c0c3d7ddc91cdb764124b58fa802d02203b9ee0109e5e55f2c0628c508e2cbb3ce5d7e34a3b0a2b97fba2a721222fcd100121025ee711359543487640d2d16a555833edf3c6e147f30a5b6ec813606a984e731a",
      "idx": 1,
      "txid": "c60e318a314c1ab653bf1c18e945e3d1e53b84e71a455446465f71e3180b2b20"
    }
  ],
  "outputs": [
    {
      "amount": 10,
      "script": {
        "condition": {
          "publicKey": "025ee711359543487640d2d16a555833edf3c6e147f30a5b6ec813606a984e731a",
          "type": "secp256k1-sha-256"
        }
      }
    }
  ]
}
```

Ok! Now we see that the script of the input has been replaced by a bunch of digits, that is an encoded script. We can go ahead and encode the transaction:

```shell
ENCODED=`hoek encodeTx "$SIGNED"`
echo $ENCODED | jq
{
  "hex": "0100000001202b0b18e3715f464654451ae7843be5d1e345e9181cbf53b61a4c318a310ec6010000006b483045022100fb748569a7684f27ce620394122aa69a9e8c0c3d7ddc91cdb764124b58fa802d02203b9ee0109e5e55f2c0628c508e2cbb3ce5d7e34a3b0a2b97fba2a721222fcd100121025ee711359543487640d2d16a555833edf3c6e147f30a5b6ec813606a984e731a00000000010a000000000000002b29a527802066c8e31c1a77c8e7c1814a17e013afbfca422cd3768ad641a97abab636aeb6088103020000cc00000000",
  "txid": "3eb5b07568516b3c58a6d981f50e452d23d9583eaf72ea5e79a21d01b9c44c9f"
}
```

Alright! There we have a bitcoin rawtransaction, hex encoded, and the transaction ID. What do we get if we decode it?

```shell
hoek decodeTx $ENCODED | jq
{
  "inputs": [
    {
      "script": "483045022100fb748569a7684f27ce620394122aa69a9e8c0c3d7ddc91cdb764124b58fa802d02203b9ee0109e5e55f2c0628c508e2cbb3ce5d7e34a3b0a2b97fba2a721222fcd100121025ee711359543487640d2d16a555833edf3c6e147f30a5b6ec813606a984e731a",
      "idx": 1,
      "txid": "c60e318a314c1ab653bf1c18e945e3d1e53b84e71a455446465f71e3180b2b20"
    }
  ],
  "outputs": [
    {
      "amount": 10,
      "script": {
        "condition": {
          "uri": "ni:///sha-256;ZsjjHBp3yOfBgUoX4BOvv8pCLNN2itZBqXq6tjautgg?fpt=secp256k1-sha-256&cost=131072",
          "type": "condition"
        }
      }
    }
  ]
}
```

We get our transaction back out, with two differences: The output script we specified has been replaced with a condition binary, shown here in uri format. It doesn't reveal the public key, just the hash of the public key. The other difference, is that the input script has been encoded; it contains a signature now. If you want to see what that  decodes to, you can use `hoek decodeScript`:

```shell
hoek decodeScript '{"hex": "483045022100fb748569a7684f27ce620394122aa69a9e8c0c3d7ddc91cdb764124b58fa802d02203b9ee0109e5e55f2c0628c508e2cbb3ce5d7e34a3b0a2b97fba2a721222fcd100121025ee711359543487640d2d16a555833edf3c6e147f30a5b6ec813606a984e731a"}' | jq
{
  "ops": [
    "OP_PUSHDATA \"0E\\STX!\\NUL\\251t\\133i\\167hO'\\206b\\ETX\\148\\DC2*\\166\\154\\158\\140\\f=}\\220\\145\\205\\183d\\DC2KX\\250\\128-\\STX ;\\158\\224\\DLE\\158^U\\242\\192b\\140P\\142,\\187<\\229\\215\\227J;\\n+\\151\\251\\162\\167!\\\"/\\205\\DLE\\SOH\" OPCODE",
    "OP_PUSHDATA \"\\STX^\\231\\DC15\\149CHv@\\210\\209jUX3\\237\\243\\198\\225G\\243\\n[n\\200\\DC3`j\\152Ns\\SUB\" OPCODE"
  ],
  "isPushOnly": true
}
```

Now to create a transaction that spends the above. We copy the redeem script from before into the input. The output in this case is a data carrier output, using the OP_RETURN opcode.

```shell
INPUT_TXID=`echo $ENCODED | jq .txid`
SPEND='{
    "inputs": [{
        "txid": '$INPUT_TXID',
        "idx": 0,
        "script": {
            "fulfillment": {
                "publicKey": "025ee711359543487640d2d16a555833edf3c6e147f30a5b6ec813606a984e731a",
                "type": "secp256k1-sha-256"
            }
        }
    }],
    "outputs": [{
        "amount": 9,
        "script": {
            "return": "AQ=="
        }
    }]
}'
SPEND_SIGNED=`hoek signTxSecp256k1 '{
    "tx": '"$SPEND"',
    "privateKeys": ["Uw8X3xAdYphLyA36TeSbofHNF54LYYhZU6TbN1n9r1e7EtnBrYdD"]
}'`
hoek encodeTx "$SPEND_SIGNED" | jq
{
  "hex": "01000000019f4cc4b9011da2795eea72af3e58d9232d450ef581d9a6583c6b516875b0b53e00000000694c67a5658021025ee711359543487640d2d16a555833edf3c6e147f30a5b6ec813606a984e731a81405329c8215c101c63e1ca635f0ec00c03902b56a86f2e30e9b5bbfcac4cf78e8e77eb1fc3df1b336a44ba3fcec4f93d37aa83da599ff0d7c7b86a4499ff829bbd00000000010900000000000000036a010100000000",
  "txid": "31fe54e4881b1d3b0ce735c2b69aa1ba99a87770b1c6878c879f4d3a57b0b97d"
}
```


