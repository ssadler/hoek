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
import qualified Network.Haskoin.Crypto as Haskoin
import qualified Network.Haskoin.Transaction as Haskoin
import qualified Network.Haskoin.Script as Haskoin
import           Network.Haskoin.Util


encodeTx :: KTx -> Except Err Tx
encodeTx (KTx ins outs) = do
  txIns <- mapM toTxIn ins
  pure $ Haskoin.createTx 1 txIns (toTxOut <$> outs) 0
  where
    toTxIn :: TxInput -> Except Err TxIn
    toTxIn (TxInput outpoint (CCInput cond)) =
      -- TODO: Check fulfilled
      case getFulfillment cond of
           Just ffillBin ->
             let script = Script [opPushData ffillBin]
              in pure $ TxIn outpoint (encode script) 0
           Nothing -> throwE $ errStr TxInvalidFulfillment "Unfulfillable condition"
    toTxIn (TxInput op (ScriptInput s)) = pure $ TxIn op s 0
    toTxIn (TxInput _ inp) =
      throwE $ otherErr $ "Can't encode unsigned input: " ++ show inp


toTxOut :: TxOutput -> TxOut
toTxOut (TxOutput amount script) = TxOut amount $ encode $
  case script of
    (CCOutput cond) ->
      Script [opPushData $ encodeCondition cond, OP_CHECKCRYPTOCONDITION]
    (AddressOutput addr) ->
      Haskoin.encodeOutput $ Haskoin.addressToOutput addr


signTxEd25519 :: KTx -> [SecretKey] -> Except Err KTx
signTxEd25519 (KTx ins outs) keys = pure $
  KTx (signInputEd25519 keys "" <$> ins) outs


signInputEd25519 :: [SecretKey] -> ByteString -> TxInput -> TxInput
signInputEd25519 keys message (TxInput op (CCInput cond)) =
  let sign c sk = fulfillEd25519 (toPublic sk) sk message c
      cond' = foldl sign cond keys
   in TxInput op (CCInput cond')
signInputEd25519 _ _ i = i


signTxBitcoin :: KTx -> [Haskoin.PrvKey] -> Except Err KTx
signTxBitcoin (KTx ins outs) keys = 
  let sigInputs = mapMaybe getSigInput ins
      txIns = [TxIn op "" 0 | TxInput op _ <- ins]
      tx = Haskoin.createTx 1 txIns (toTxOut <$> outs) 0
      eSigned@(Right signed) = Haskoin.signTx tx sigInputs keys
      newIns = zipWith mergeScript ins (Haskoin.txIn signed)
  in pure (KTx newIns outs)
  where
    mergeScript :: TxInput -> TxIn -> TxInput
    mergeScript t@(TxInput op _) (Haskoin.TxIn _ bs _) =
      if bs == mempty then t else TxInput op (ScriptInput bs)
    mergeScript t _ = t

    getSigInput (TxInput op script) =
      let toSigInput inp = Haskoin.SigInput inp op sigType Nothing
          sigType = Haskoin.SigAll False
       in toSigInput <$> case script of
               AddressInput (Haskoin.PubKeyAddress h160) -> Just (Haskoin.PayPKHash h160)
               PubKeyInput pk                            -> Just (Haskoin.PayPK pk)
               _                                         -> Nothing


-- for the purposes of signing an input, it's neccesary to encode the KTx
-- to a TX. Only the signature script of the input in question is important
-- to consider, the others will be nullified anyway. In order to generate it,
-- derive the output script from the private key.
