{-# LANGUAGE OverloadedStrings #-}

module Network.Komodo.API.Tx where

import           Data.Aeson hiding (encode)
import           Data.Aeson.Types (Parser, parseEither)
import qualified Data.Map as Map
import           Data.Serialize

import           Network.Haskoin.Util (encodeHex, decodeHex)
import           Network.Haskoin.Transaction
import qualified Network.Haskoin.Crypto as Haskoin
import qualified Network.Haskoin.Script as Haskoin
import           Network.Komodo.API.Utils
import           Network.Komodo.Crypto
import           Network.Komodo.Prelude
import qualified Network.Komodo.Transaction.Builder as TX
import qualified Network.Komodo.Transaction.Decode as TX
import           Network.Komodo.Transaction.Types

import           Lens.Micro

import Debug.Trace


encodeTx :: JsonMethod
encodeTx = pureMethod $ \obj -> do
  ktx <- obj .: "tx"
  pure $ do
    tx <- TX.encodeTx ktx
    pure $ object ["tx" .= tx, "txid" .= txHash tx]


decodeTx :: JsonMethod
decodeTx = pureMethod $ \obj -> do
  tx <- obj .: "hex"
  pure $ do
    ktx <- TX.decodeTx tx
    pure $ object ["tx" .= ktx]


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
  keys <- obj .: "privateKeys" >>= mapM parseBitcoinWif
  pure $ do
    signed <- TX.signTxBitcoin ktx keys
    pure $ object ["tx" .= signed]
  where
    parseBitcoinWif txt =
      let mkey = Haskoin.fromWif $ encodeUtf8 txt
       in maybe (fail "invalid (non-WIF) private key") pure mkey


decodeScript :: JsonMethod
decodeScript = pureMethod $ \obj -> do
  scriptHex <- encodeUtf8 <$> obj .: "hex"
  scriptBs <- case decodeHex scriptHex of
                   Just bs -> pure bs
                   Nothing -> fail "Invalid hex"
  (Haskoin.Script ops) <- case Data.Serialize.decode scriptBs of
                            Left e -> fail e
                            Right s -> pure s
  pure $
    pure $ object ["ops" .= show ops, "isPushOnly" .= all Haskoin.isPushOp ops]
