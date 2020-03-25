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
import qualified Network.Komodo.Transaction as TX


encodeTx :: JsonMethod
encodeTx = pureMethod $ \ktx ->
  pure $ do
    tx <- TX.encodeTx ktx
    pure $ object ["hex" .= tx, "txid" .= txHash tx]


decodeTx :: JsonMethod
decodeTx = pureMethod $ \obj -> do
  tx <- obj .: "hex"
  pure $ toJSON <$> TX.decodeTx tx


getTxid :: JsonMethod
getTxid = pureMethod $ \obj -> do
  tx <- obj .: "hex"
  pure $ pure $ toJSON $ txHash tx


signTxEd25519 :: JsonMethod
signTxEd25519 = pureMethod $ \obj -> do
  ktx <- obj .: "tx"
  keys <- obj .: "privateKeys" >>= mapM parseSecretKey
  pure $ toJSON <$> TX.signTxEd25519 keys ktx


signTxSecp256k1 :: JsonMethod
signTxSecp256k1 = pureMethod $ \obj -> do
  ktx <- obj .: "tx"
  keys <- obj .: "privateKeys" >>= mapM parseBitcoinWif
  pure $ toJSON <$> TX.signTxSecp256k1 keys ktx

signTxBitcoin :: JsonMethod
signTxBitcoin = pureMethod $ \obj -> do
  ktx <- obj .: "tx"
  keys <- obj .: "privateKeys" >>= mapM parseBitcoinWif
  pure $ toJSON <$> TX.signTxBitcoin keys ktx

signTx :: JsonMethod
signTx = pureMethod $ \obj -> do
  ktx <- obj .: "tx"
  keys <- obj .: "privateKeys" >>= mapM parseBitcoinWif
  let act = TX.signTxSecp256k1 keys ktx >>= TX.signTxBitcoin keys
  pure $ toJSON <$> act


parseBitcoinWif :: Text -> Parser Haskoin.PrvKey
parseBitcoinWif txt =
  let mkey = Haskoin.fromWif $ encodeUtf8 txt
   in maybe (fail "invalid (non-WIF) private key") pure mkey


decodeScript :: JsonMethod
decodeScript = pureMethod $ \obj -> do
  scriptHex <- encodeUtf8 <$> obj .: "hex"
  scriptBs <- case decodeHex scriptHex of
                   Just bs -> pure bs
                   Nothing -> fail "Invalid hex"
  (Haskoin.Script ops) <- either fail pure $ Data.Serialize.decode scriptBs
  pure $
    pure $ object ["ops" .= (show <$> ops), "isPushOnly" .= all Haskoin.isPushOp ops]
