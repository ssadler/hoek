{-# LANGUAGE OverloadedStrings #-}

module Network.Komodo.Transaction.Types 
  ( KTx(..)
  , OutPoint(..)
  , TxInput(..)
  , InputScript(..)
  , TxOutput(..)
  , OutputScript(..)
  ) where

import Data.Word

import Network.CryptoConditions
import Network.Komodo.Data.Aeson


import Network.Haskoin.Transaction (OutPoint(..))


-- | Komoto Tx
--
data KTx = KTx
  { txInputs :: [TxInput]
  , txOutputs :: [TxOutput]
  } deriving (Eq, Show)


instance ToJSON KTx where
  toJSON (KTx ins outs) =
    object ["inputs" .= ins, "outputs" .= outs]


instance FromJSON KTx where
  parseJSON = withStrictObject "tx" $ \o ->
    KTx <$> o .:- "inputs" <*> o .:- "outputs"


-- | Transaction Input
--
data TxInput = TxInput OutPoint InputScript
  deriving (Eq, Show)


instance ToJSON TxInput where
  toJSON (TxInput (OutPoint txid idx) ins) =
    object ["txid" .= txid, "idx" .= idx, "fulfillment" .= ins]


instance FromJSON TxInput where
  parseJSON = withStrictObject "input" $ \o -> do
    op <- OutPoint <$> o .:- "txid" <*> o .:- "idx"
    TxInput op <$> o .:- "fulfillment"


-- | Input Script
--
data InputScript = CCInput Condition
  deriving (Eq, Show)


instance FromJSON InputScript where
  parseJSON o = CCInput <$> parseJSON o


instance ToJSON InputScript where
  toJSON (CCInput cond) = toJSON cond


-- | TxOutput
--
data TxOutput = TxOutput Word64 OutputScript
  deriving (Eq, Show)


instance ToJSON TxOutput where
  toJSON (TxOutput amount (CCOutput outs)) =
    object ["amount" .= amount, "condition" .= outs]


instance FromJSON TxOutput where
  parseJSON = withStrictObject "output" $ \o -> do
    amount <- o .:- "amount"
    cond <- o .:- "condition"
    pure $ TxOutput amount $ CCOutput cond


data OutputScript = CCOutput Condition
  deriving (Eq, Show)


