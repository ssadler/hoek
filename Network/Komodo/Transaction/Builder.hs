{-# LANGUAGE OverloadedStrings #-}

module Network.Komodo.Transaction.Builder where

import qualified Data.Aeson as Ae
import           Data.Word
import qualified Data.ByteString               as BS
import           Data.Serialize                (encode)

import           Crypto.Error


import           Network.Komodo.Crypto
import           Network.Komodo.CryptoConditions
import           Network.Komodo.Prelude
import           Network.Komodo.Transaction.Types
import           Network.Haskoin.Script
import           Network.Haskoin.Transaction (Tx(..), TxHash, TxIn(..), TxOut(..), OutPoint(..))
import qualified Network.Haskoin.Transaction as HTx
import           Network.Haskoin.Util


import Debug.Trace


encodeTx :: KTx -> Except Err Tx
encodeTx (KTx ins outs) =
  pure $ HTx.createTx 1 (toTxIn <$> ins) (toTxOut <$> outs) 0


toTxIn :: TxInput -> TxIn
toTxIn (TxInput outpoint (CCInput cond)) =
  let Just ffillBin = getFulfillment $ fakeFulfillCondition cond
      script = Script [opPushData ffillBin]
   in TxIn outpoint (encode script) 0


toTxOut :: TxOutput -> TxOut
toTxOut (TxOutput amount (CCOutput cond)) =
  let condBin = encodeCondition cond
      script = Script [opPushData condBin, OP_CHECKCRYPTOCONDITIONVERIFY]
   in TxOut amount $ encode script


signTx :: Tx -> [SecretKey] -> Except Err Tx
signTx tx keys = traceShow (txIn tx) undefined
