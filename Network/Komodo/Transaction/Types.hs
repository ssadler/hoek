{-# LANGUAGE OverloadedStrings #-}

module Network.Komodo.Transaction.Types where

import Data.Word

import Network.CryptoConditions
import Network.Komodo.Data.Aeson


import Network.Haskoin.Transaction (OutPoint(..))


data KTx = KTx
  { txInputs :: [TxInput]
  , txOutputs :: [TxOutput]
  }


data InputScript = CCInput Condition

data TxInput = TxInput OutPoint InputScript


instance FromJSON TxInput where
  parseJSON = withStrictObject "input" $ \o -> do
    op <- OutPoint <$> o .:- "txid" <*> o .:- "idx"
    TxInput op <$> o .:- "fulfillment"


instance FromJSON InputScript where
  parseJSON o = CCInput <$> parseJSON o


data TxOutput = TxOutput Word64 OutputScript

instance FromJSON TxOutput where
  parseJSON = withStrictObject "output" $ \o -> do
    amount <- o .:- "amount"
    cond <- o .:- "condition"
    pure $ TxOutput amount $ CCOutput cond

data OutputScript = CCOutput Condition


