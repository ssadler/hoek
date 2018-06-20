{-# LANGUAGE OverloadedStrings #-}

module Network.Komodo.Transaction 
  ( module Types
  , H.txHash
  , encodeTx
  , decodeTx
  , signTxEd25519
  , signTxSecp256k1
  , signTxBitcoin
  , fromHaskoinInput
  , fromHaskoinOutput
  , toHaskoinInput
  , toHaskoinOutput
  ) where

import qualified Data.Aeson as Ae
import           Data.Word
import qualified Data.ByteString               as BS
import           Data.Serialize                (encode)

import           Crypto.Error
import qualified Crypto.PubKey.Ed25519 as Ed2
import qualified Crypto.Secp256k1 as EC

import           Network.Komodo.Crypto
import           Network.Komodo.CryptoConditions
import           Network.Komodo.Prelude
import           Network.Komodo.Transaction.Types as Types
import qualified Network.Haskoin.Crypto as H
import qualified Network.Haskoin.Transaction as H
import           Network.Haskoin.Script as H
import           Network.Haskoin.Util


-- | Encoding | --------------------------------------------------------------

encodeTx :: KTx -> Except Err H.Tx
encodeTx (KTx ins outs) = do
  txIns <- mapM toHaskoinInput ins
  pure $ H.createTx 1 txIns (toHaskoinOutput <$> outs) 0


toHaskoinInput :: TxInput -> Except Err H.TxIn
toHaskoinInput (TxInput op (ScriptInput s)) = pure $ H.TxIn op s 0
toHaskoinInput (TxInput outpoint (ConditionInput cond)) =
  case encodeFulfillment cond of
       Just ffillBin ->
         let script = Script [opPushData ffillBin]
          in pure $ H.TxIn outpoint (encode script) 0
       Nothing -> throwE $ otherErr "Can't encode unfulfilled condition"
toHaskoinInput (TxInput _ inp) =
  throwE $ otherErr $ "Can't encode unsigned input: " ++ show inp


toHaskoinOutput :: TxOutput -> H.TxOut
toHaskoinOutput (TxOutput amount script) = H.TxOut amount $
  case script of
    (CCOutput cond) ->
      encode $ Script [opPushData $ encodeCondition cond, OP_CHECKCRYPTOCONDITION]
    (AddressOutput addr) ->
      encode $ H.encodeOutput $ H.addressToOutput addr
    (CarrierOutput bs) ->
      encode $ H.encodeOutput $ H.DataCarrier bs
    (PubKeyOutput pk) ->
      encode $ H.encodeOutput $ H.PayPK pk
    (ScriptOutput s) ->
      s


-- | Decoding | --------------------------------------------------------------

decodeTx :: H.Tx -> Except Err KTx
decodeTx tx = do
  ins <- mapM fromHaskoinInput $ H.txIn tx
  outs <- mapM fromHaskoinOutput $ H.txOut tx
  pure $ KTx ins outs


fromHaskoinInput :: H.TxIn -> Except Err TxInput
fromHaskoinInput (H.TxIn op script _) = pure $ TxInput op $ ScriptInput script


fromHaskoinOutput :: H.TxOut -> Except Err TxOutput
fromHaskoinOutput (H.TxOut amount script) = TxOutput amount <$>
  case H.decodeOutputBS script of
       Left err -> throwE $ otherErr err
       Right so -> convertScriptOutput so


convertScriptOutput :: H.ScriptOutput -> Except Err OutputScript
convertScriptOutput (H.PayPKHash hash) = pure $ AddressOutput $ H.PubKeyAddress hash
convertScriptOutput (H.PayCondition cond) = pure $ CCOutput cond
convertScriptOutput (H.PayPK pk) = pure $ PubKeyOutput pk
convertScriptOutput (H.DataCarrier bs) = pure $ CarrierOutput bs
convertScriptOutput so = throwE $ otherErr $ "Cannot decode output: " ++ show so


-- | Signing | ---------------------------------------------------------------

signTxEd25519 :: [Ed2.SecretKey] -> KTx -> Except Err KTx
signTxEd25519 keys ktx@(KTx ins outs) = 
  let tx = encodeTxEmptyInputs ktx
      signedIns = signInputEd25519 tx keys <$> zip [0..] ins
   in pure $ KTx signedIns outs


signInputEd25519 :: H.Tx -> [Ed2.SecretKey] -> (Int, TxInput) -> TxInput
signInputEd25519 tx keys (i, inp@(TxInput op (ConditionInput cond))) =
  let ms@(Just sigInput) = getSigInput inp
      message = getMessageToSign tx i sigInput
      sign c sk = fulfillEd25519 (Ed2.toPublic sk) sk message c
      cond' = foldl sign cond keys
      newInput = TxInput op (ConditionInput cond')
   in maybe inp (\_ -> newInput) ms
signInputEd25519 _ _ (_, inp) = inp


signTxSecp256k1 :: [H.PrvKey] -> KTx -> Except Err KTx
signTxSecp256k1 keys ktx@(KTx ins outs) =
  let tx = encodeTxEmptyInputs ktx
      ecKeys = H.prvKeySecKey <$> keys
      signedIns = signInputSecp256k1 tx ecKeys <$> zip [0..] ins
   in pure $ KTx signedIns outs


signInputSecp256k1 :: H.Tx -> [EC.SecKey] -> (Int, TxInput) -> TxInput
signInputSecp256k1 tx keys (i, inp@(TxInput op (ConditionInput cond))) =
  let ms@(Just sigInput) = getSigInput inp
      Just message = EC.msg $ getMessageToSign tx i sigInput
      sign c sk = fulfillSecp256k1 (EC.derivePubKey sk) sk message c
      cond' = foldl sign cond keys
      newInput = TxInput op (ConditionInput cond')
   in maybe inp (\_ -> newInput) ms
signInputSecp256k1 _ _ (_, inp) = inp


getMessageToSign :: H.Tx -> Int -> H.SigInput -> ByteString
getMessageToSign tx i (H.SigInput so _ sh rdmM) =
    encode $ txSigHash tx (encodeOutput $ fromMaybe so rdmM) i sh


encodeTxEmptyInputs :: KTx -> H.Tx
encodeTxEmptyInputs (KTx ins outs) = 
  let emptyIns = [H.TxIn op "" 0 | TxInput op _ <- ins]
   in H.createTx 1 emptyIns (toHaskoinOutput <$> outs) 0


signTxBitcoin :: [H.PrvKey] -> KTx -> Except Err KTx
signTxBitcoin keys ktx@(KTx ins outs) =
  let tx = encodeTxEmptyInputs ktx
      sigInputs = mapMaybe getSigInput ins
      esigned = H.signTx tx sigInputs keys
      Right signed = esigned
      newIns = zipWith mergeScript ins (H.txIn signed)
  in case esigned of Left err -> throwE $ otherErr err
                     _        -> pure $ KTx newIns outs
  where
    mergeScript :: TxInput -> H.TxIn -> TxInput
    mergeScript t@(TxInput op _) (H.TxIn _ bs _) =
      if bs == mempty then t else TxInput op (ScriptInput bs)


getSigInput :: TxInput -> Maybe H.SigInput
getSigInput (TxInput op script) =
  let toSigInput inp = H.SigInput inp op sigType Nothing
      sigType = H.SigAll False
      mInput = case script of
           PubKeyInput pk                   -> Just (H.PayPK pk)
           ConditionInput cond              -> Just (H.PayCondition cond)
           AddressInput (H.PubKeyAddress h) -> Just (H.PayPKHash h)
           AddressInput (H.ScriptAddress h) -> Just (H.PayScriptHash h)
           _                                -> Nothing
      in toSigInput <$> mInput
