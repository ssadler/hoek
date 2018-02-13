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
  ktx <- obj .: "tx"
  pure $ do
    tx <- TX.encodeTx ktx
    pure $ object ["tx" .= tx, "txid" .= txHash tx]


signTxEd25519 :: JsonMethod
signTxEd25519 = pureMethod $ \obj -> do
  ktx <- obj .: "tx"
  keys <- obj .: "privateKeys" >>= mapM parseSecretKey
  pure $ do
    signed <- TX.signTxEd25519 ktx keys
    pure $ object ["tx" .= signed]


signTxBitcoin :: JsonMethod
signTxBitcoin = pureMethod $ \obj -> do
  ktx <- obj .: "tx"
  keys <- obj .: "privateKeys" >>= mapM parseSecretKey
  pure $ do
    signed <- TX.signTxBitcoin ktx keys
    pure $ object ["tx" .= signed]

