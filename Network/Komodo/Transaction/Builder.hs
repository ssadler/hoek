{-# LANGUAGE OverloadedStrings #-}

module Network.Komodo.Transaction.Builder where

import qualified Data.Aeson as Ae
import           Data.Word
import qualified Data.ByteString               as BS
import           Data.Serialize                (encode)

import           Crypto.PubKey.Ed25519 as Ed2
import           Crypto.Error

import           Network.CryptoConditions
import           Network.Komodo.Crypto
import           Network.Komodo.Prelude
import           Network.Komodo.Transaction.Types
import           Network.Haskoin.Script
import           Network.Haskoin.Transaction (Tx(..), TxHash, TxIn(..), TxOut(..), OutPoint(..))
import qualified Network.Haskoin.Transaction as HTx
import           Network.Haskoin.Util


import Debug.Trace


encodeTx :: KTx -> Except Err Tx
encodeTx (KTx ins outs) = do
  txIns <- mapM toTxIn ins
  pure $ HTx.createTx 1 txIns (toTxOut <$> outs) 0
  where
    toTxIn :: TxInput -> Except Err TxIn
    toTxIn (TxInput outpoint (CCInput cond)) =
      case getFulfillment cond of
           Just ffillBin ->
             let script = Script [opPushData ffillBin]
              in pure $ TxIn outpoint (encode script) 0
           Nothing -> throwE $ errStr TxInvalidFulfillment "Unfulfillable condition"

    toTxOut :: TxOutput -> TxOut
    toTxOut (TxOutput amount (CCOutput cond)) =
      let condBin = encodeCondition cond
          script = Script [opPushData condBin, OP_CHECKCRYPTOCONDITIONVERIFY]
       in TxOut amount $ encode script


signTx :: KTx -> [SecretKey] -> Except Err KTx
signTx (KTx ins outs) keys = pure $
  KTx (signInput keys "" <$> ins) outs


signInput :: [SecretKey] -> ByteString -> TxInput -> TxInput
signInput keys message (TxInput op (CCInput cond)) = 
  let sign c sk = fulfillEd25519 (toPublic sk) sk message c
      cond' = foldl sign cond keys
   in TxInput op (CCInput cond')
