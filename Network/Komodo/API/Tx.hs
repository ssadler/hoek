{-# LANGUAGE OverloadedStrings #-}

module Network.Komodo.API.Tx where

import           Data.Aeson hiding (encode)
import           Data.Aeson.Types (Parser, parseEither)
import qualified Data.Map as Map
import           Data.Serialize

import           Network.Haskoin.Util (encodeHex)
import           Network.Haskoin.Transaction
import           Network.Komodo.API.Utils
import           Network.Komodo.Crypto
import           Network.Komodo.Prelude
import qualified Network.Komodo.Transaction.Builder as TX
import           Network.Komodo.Transaction.Types

import           Lens.Micro

import Debug.Trace


encodeTx :: JsonMethod
encodeTx = pureMethod $ \obj -> do
  ktx <- KTx <$> obj .: "inputs" <*> obj .: "outputs"
  pure $ do
    tx <- TX.encodeTx ktx
    pure $ object ["tx" .= tx, "txid" .= txHash tx]


signTx :: JsonMethod
signTx = pureMethod $ \obj -> do
  act <- TX.signTx <$> obj .: "tx" <*> obj .: "privateKeys"
  pure $ do
    tx <- act
    pure $ object ["tx" .= tx, "txid" .= txHash tx]

