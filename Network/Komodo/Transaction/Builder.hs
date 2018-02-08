{-# LANGUAGE OverloadedStrings #-}

module Network.Komodo.Transaction.Builder where

import qualified Data.Aeson as Ae
import           Data.Word
import qualified Data.ByteString               as BS
import           Data.Serialize                (encode)

import           Crypto.Error
import           Crypto.PubKey.Ed25519         as Ed2


import           Network.Komodo.Prelude
import           Network.Komodo.CryptoConditions
import           Network.Komodo.Transaction.Parser
import           Network.Haskoin.Script
import           Network.Haskoin.Transaction (Tx(..), TxHash, TxIn(..), TxOut(..), OutPoint(..))
import qualified Network.Haskoin.Transaction as HTx
import           Network.Haskoin.Util



createTx :: [TxInput] -> [TxOutput] -> Tx
createTx ins outs = HTx.createTx 1 (toTxIn <$> ins) [] 0
  where
    toTxIn (TxInput outpoint (CCInput cond)) = 
      let ffillBin = maybe ffillJson id $ getFulfillment cond
          ffillJson = toStrict $ Ae.encode cond
          script = Script [opPushData ffillBin]
       in TxIn outpoint (encode script) 0


--exampleTx :: Tx
--exampleTx =
--  let ccOut = ccTxOut 100 ccInCond
--      ccIn = ccTxIn ("9b907ef1e3c26fc71fe4a4b3580bc75264112f95050014157059c736f0202e71", 0)
--                    ccInCond
--      sig = Ed2.sign skBob pkBob ("" :: ByteString)
--      ccInCond = fulfillEd25519 pkBob sig $ ed25519Condition pkBob
--  in HTx.createTx 1 [ccIn] [ccOut] 0


--ccTxIn :: (TxHash, Word32) -> Condition -> TxIn
--ccTxIn (txid, n) cond = 
--  let outPoint = OutPoint txid n
--      ffillBin = maybe (error "woops") id $ getFulfillment cond
--      script = Script [opPushData ffillBin]
--   in TxIn outPoint (encode script) 0
--
--
--ccTxOut :: Word64 -> Condition -> TxOut
--ccTxOut amount cond =
--  let script = Script [opPushData condBin, OP_CHECKCRYPTOCONDITIONVERIFY]
--      condBin = encodeCondition cond
--   in TxOut amount $ encode script


skBob, skEve :: SecretKey
skBob = toSecret "C\SOH\NAK 6P\151\165|\156\144of-B\174\245h\166\188\135\158\SO\195\b)\253\168\f\221\205\RS"
skEve = toSecret "D\SOH\NAK 6P\151\165|\156\144of-B\174\245h\166\188\135\158\SO\195\b)\253\168\f\221\205\RS"

toSecret :: ByteString -> SecretKey
toSecret = throwCryptoError . secretKey

pkBob :: PublicKey
pkBob = toPublic skBob
