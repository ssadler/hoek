{-# LANGUAGE OverloadedStrings #-}

module Network.Komodo.Transaction.Parser where


import Data.Word

import Network.Komodo.CryptoConditions
import Network.Komodo.Data.Aeson

import Network.Haskoin.Transaction (OutPoint(..))


data TxInput = TxInput OutPoint InputScript

instance FromJSON TxInput where
  parseJSON = withStrictObject "input" $ \o -> do
    op <- OutPoint <$> o .:- "txid" <*> o .:- "idx"
    TxInput op <$> o .:- "fulfillment"


data InputScript = CCInput CryptoCondition

instance FromJSON InputScript where
  parseJSON o = CCInput <$> parseJSON o


data TxOutput = TxOutput Word64 OutputScript

instance FromJSON TxOutput where
  parseJSON = withStrictObject "output" $ \o -> do
    amount <- o .:- "amount"
    cond <- o .:- "condition"
    pure $ TxOutput amount $ CCOutput cond

data OutputScript = CCOutput CryptoCondition
