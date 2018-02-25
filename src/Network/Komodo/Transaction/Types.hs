{-# LANGUAGE OverloadedStrings #-}

module Network.Komodo.Transaction.Types 
  ( KTx(..)
  , InputScript(..)
  , Haskoin.OutPoint(..)
  , Haskoin.TxHash(..)
  , OutputScript(..)
  , TxInput(..)
  , TxOutput(..)
  ) where

import           Control.Applicative
import           Data.ByteString.Base58
import           Data.Word

import           Network.Komodo.CryptoConditions
import           Network.Komodo.Data.Aeson
import           Network.Komodo.Prelude

import qualified Network.Haskoin.Crypto as Haskoin
import qualified Network.Haskoin.Transaction as Haskoin
import qualified Network.Haskoin.Script as Haskoin
import qualified Network.Haskoin.Util as Haskoin


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
     object $ ["txid" .= txid, "idx" .= idx, "script" .= ins]


instance FromJSON TxInput where
  parseJSON = withStrictObject "input" $ \o -> do
    op <- Haskoin.OutPoint <$> o .:- "txid" <*> o .:- "idx"
    TxInput op <$> o .:- "script"


-- | Input Script
--
data InputScript = ConditionInput Condition
                 | PubKeyHashInput Haskoin.Hash160
                 | ScriptHashInput Haskoin.Hash160
                 | PubKeyInput Haskoin.PubKey
                 | ScriptInput ByteString
  deriving (Eq, Show)


instance FromJSON InputScript where
  parseJSON (String s) =
    case Haskoin.decodeHex (encodeUtf8 s) of
         Just bs -> pure $ ScriptInput bs
         Nothing -> fail "Invalid hex script"
  parseJSON val = switch val
    where
      switch = withStrictObject "InputScript" $ \o -> do
        act <- (getCC <$> o .:- "fulfillment")
           <|> (getAddress <$> o .:- "address")
           <|> (getPubKey <$> o .:- "pubkey")
           <|> fail "inputscript must contain fulfillment, address or pubkey"
        act
      getCC val = ConditionInput <$> parseJSON val
      getAddress val = do 
          addr <- parseJSON val
          pure $ case addr of
                      Haskoin.PubKeyAddress h160 -> PubKeyHashInput h160
                      Haskoin.ScriptAddress h160 -> ScriptHashInput h160
      getPubKey val = do PubKeyInput <$> parseJSON val


instance ToJSON InputScript where
  toJSON (ConditionInput cond) = object ["fulfillment" .= cond]
  toJSON (PubKeyHashInput addr) = object ["address" .= Haskoin.PubKeyAddress addr]
  toJSON (ScriptHashInput addr) = object ["address" .= Haskoin.ScriptAddress addr]
  toJSON (PubKeyInput pk) = object ["pubkey" .= pk]
  toJSON (ScriptInput script) = toJSON $ decodeUtf8 $ Haskoin.encodeHex script


-- | TxOutput
--
data TxOutput = TxOutput Word64 OutputScript
  deriving (Eq, Show)


instance ToJSON TxOutput where
  toJSON (TxOutput amount script) =
    object ["amount" .= amount, "script" .= script]


instance FromJSON TxOutput where
  parseJSON = withStrictObject "output" $ \o ->
    TxOutput <$> o .:- "amount" <*> o .:- "script"


data OutputScript = CCOutput Condition
                  | AddressOutput Haskoin.Address
                  | PubKeyOutput Haskoin.PubKey
                  | ScriptOutput ByteString
  deriving (Eq, Show)


instance FromJSON OutputScript where
  parseJSON (String s) =
    case Haskoin.decodeHex (encodeUtf8 s) of
         Just bs -> pure $ ScriptOutput bs
         Nothing -> fail "Invalid hex script"
  parseJSON val = switch val
    where
      switch = withStrictObject "OutputScript" $ \o -> do
      -- the switch is so that we get the correct parse error
        act <- (getCC <$> o .:- "condition")
           <|> (getAddr <$> o .:- "address")
           <|> fail "outputscript must contain condition or address"
        act
      getCC val = CCOutput <$> parseJSON val
      getAddr val = AddressOutput <$> parseJSON val


instance ToJSON OutputScript where
  toJSON (CCOutput cond) = object ["condition" .= cond]
  toJSON (AddressOutput addr) = object ["address" .= addr]
  toJSON (PubKeyOutput pk) = object ["pubkey" .= pk]
  toJSON (ScriptOutput script) = toJSON $ decodeUtf8 $ Haskoin.encodeHex script
