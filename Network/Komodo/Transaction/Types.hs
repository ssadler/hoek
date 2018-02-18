{-# LANGUAGE OverloadedStrings #-}

module Network.Komodo.Transaction.Types 
  ( KTx(..)
  , Haskoin.OutPoint(..)
  , TxInput(..)
  , InputScript(..)
  , TxOutput(..)
  , OutputScript(..)
  , Haskoin.TxHash(..)
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
data InputScript = CCInput Condition
                 | AddressInput Haskoin.Address
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
           <|> fail "InputScript must contain fulfillment, address or pubkey"
        act
      getCC val = do B58Condition cond <- parseJSON val; pure (CCInput cond)
      getAddress val = do AddressInput <$> parseJSON val
      getPubKey val = do PubKeyInput <$> parseJSON val


instance ToJSON InputScript where
  toJSON (CCInput cond) = object ["fulfillment" .= cond]
  toJSON (AddressInput addr) = object ["address" .= addr]
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
  deriving (Eq, Show)


instance FromJSON OutputScript where
  parseJSON = withStrictObject "OutputScript" $ join . switch
    where
      -- the switch is so that we get the correct parse error
      switch o = getCC <$> o .:- "condition"
                <|> getAddr <$> o .:- "address"
      getCC val = do
        B58Condition cond <- parseJSON val
        pure $ CCOutput cond
      getAddr val = AddressOutput <$> parseJSON val


instance ToJSON OutputScript where
  toJSON (CCOutput cond) = object ["condition" .= B58Condition cond]
  toJSON (AddressOutput addr) = object ["address" .= addr]
  toJSON (PubKeyOutput pk) = object ["pubkey" .= pk]
