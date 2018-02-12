{-# LANGUAGE OverloadedStrings #-}

module Network.Komodo.Transaction.Types 
  ( KTx(..)
  , Haskoin.OutPoint(..)
  , TxInput(..)
  , InputScript(..)
  , TxOutput(..)
  , OutputScript(..)
  ) where

import           Control.Applicative
import           Data.Word

import           Network.Komodo.CryptoConditions
import           Network.Komodo.Data.Aeson

import qualified Network.Haskoin.Crypto as Haskoin
import qualified Network.Haskoin.Transaction as Haskoin
import qualified Network.Haskoin.Script as Haskoin


-- | Komodo Tx
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
data TxInput = TxInput Haskoin.OutPoint InputScript
  deriving (Eq, Show)


instance ToJSON TxInput where
  toJSON (TxInput (Haskoin.OutPoint txid idx) ins) =
    object ["txid" .= txid, "idx" .= idx, "fulfillment" .= ins]


instance FromJSON TxInput where
  parseJSON = withStrictObject "input" $ \o -> do
    op <- Haskoin.OutPoint <$> o .:- "txid" <*> o .:- "idx"
    TxInput op <$> parseInputScript o


-- | Input Script
--
data InputScript = CCInput Condition
                 | AddressInput Haskoin.Address (Maybe Haskoin.ScriptInput)
  deriving (Eq, Show)


instance FromJSON InputScript where
  parseJSON = withStrictObject "inputScript" parseInputScript


parseInputScript :: StrictObject -> Parser InputScript
parseInputScript o = 
  getCC <|> getAddress <|> fail "Input must contain fulfillment or address"
  where
    getCC = do B58Condition cond <- o .:- "fulfillment"; pure (CCInput cond)
    getAddress = do AddressInput <$> o .:- "address" <*> pure Nothing


instance ToJSON InputScript where
  toJSON (CCInput cond) = toJSON $ B58Condition cond


-- | TxOutput
--
data TxOutput = TxOutput Word64 OutputScript
  deriving (Eq, Show)


instance ToJSON TxOutput where
  toJSON (TxOutput amount (CCOutput cond)) =
    object ["amount" .= amount, "condition" .= B58Condition cond]


instance FromJSON TxOutput where
  parseJSON = withStrictObject "output" $ \o -> do
    amount <- o .:- "amount"
    B58Condition cond <- o .:- "condition"
    pure $ TxOutput amount $ CCOutput cond


data OutputScript = CCOutput Condition
  deriving (Eq, Show)


