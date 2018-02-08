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
createTx ins outs =
  HTx.createTx 1 (toTxIn <$> ins) (toTxOut <$> outs) 0
  where
    toTxIn (TxInput outpoint (CCInput cond)) = 
      let ffillBin = maybe ffillJson id $ getFulfillment cond
          ffillJson = toStrict $ Ae.encode cond
          script = Script [opPushData ffillBin]
       in TxIn outpoint (encode script) 0
    toTxOut (TxOutput amount (CCOutput cond)) =
      let condBin = encodeCondition cond
          script = Script [opPushData condBin, OP_CHECKCRYPTOCONDITIONVERIFY]
       in TxOut amount $ encode script

