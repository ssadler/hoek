{-# LANGUAGE OverloadedStrings #-}

module Network.Komodo.Transaction where

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
import qualified Network.Haskoin.Crypto as Haskoin
import qualified Network.Haskoin.Transaction as Haskoin
import           Network.Haskoin.Script as Haskoin
import           Network.Haskoin.Util


-- | Encoding | --------------------------------------------------------------

encodeTx :: KTx -> Except Err Haskoin.Tx
encodeTx (KTx ins outs) = do
  txIns <- mapM toHaskoinInput ins
  pure $ Haskoin.createTx 1 txIns (toHaskoinOutput <$> outs) 0


toHaskoinInput :: TxInput -> Except Err Haskoin.TxIn
toHaskoinInput (TxInput op (ScriptInput s)) = pure $ Haskoin.TxIn op s 0
toHaskoinInput (TxInput outpoint (CCInput cond)) =
  -- TODO: Check fulfilled
  case getFulfillment cond of
       Just ffillBin ->
         let script = Script [opPushData ffillBin]
          in pure $ Haskoin.TxIn outpoint (encode script) 0
       Nothing -> throwE $ errStr TxInvalidFulfillment "Unfulfillable condition"
toHaskoinInput (TxInput _ inp) =
  throwE $ otherErr $ "Can't encode unsigned input: " ++ show inp


toHaskoinOutput :: TxOutput -> Haskoin.TxOut
toHaskoinOutput (TxOutput amount script) = Haskoin.TxOut amount $ encode $
  case script of
    (CCOutput cond) ->
      Script [opPushData $ encodeCondition cond, OP_CHECKCRYPTOCONDITION]
    (AddressOutput addr) ->
      Haskoin.encodeOutput $ Haskoin.addressToOutput addr


-- | Decoding | --------------------------------------------------------------

decodeTx :: Haskoin.Tx -> Except Err KTx
decodeTx tx = do
  ins <- mapM fromHaskoinInput $ Haskoin.txIn tx
  outs <- mapM fromHaskoinOutput $ Haskoin.txOut tx
  pure $ KTx ins outs


fromHaskoinInput :: Haskoin.TxIn -> Except Err TxInput
fromHaskoinInput (Haskoin.TxIn op script _) = pure $ TxInput op $ ScriptInput script


fromHaskoinOutput :: Haskoin.TxOut -> Except Err TxOutput
fromHaskoinOutput (Haskoin.TxOut amount script) = TxOutput amount <$>
  case Haskoin.decodeOutputBS script of
       Left err -> throwE $ otherErr err
       Right so -> convertScriptOutput so


convertScriptOutput :: Haskoin.ScriptOutput -> Except Err OutputScript
convertScriptOutput (Haskoin.PayPKHash hash) = pure $ AddressOutput $ Haskoin.PubKeyAddress hash
convertScriptOutput (Haskoin.PayCondition cond) = pure $ CCOutput cond
convertScriptOutput (Haskoin.PayPK pk) = pure $ PubKeyOutput pk
convertScriptOutput so = throwE $ otherErr $ "Cannot decode output: " ++ show so


-- | Signing | ---------------------------------------------------------------

signTxEd25519 :: KTx -> [SecretKey] -> Except Err KTx
signTxEd25519 (KTx ins outs) keys = 
  let emptyIns = [Haskoin.TxIn op "" 0 | TxInput op _ <- ins]
      tx = Haskoin.createTx 1 emptyIns (toHaskoinOutput <$> outs) 0
      signedIns = signInputEd25519 tx keys <$> zip [0..] ins
   in pure $ KTx signedIns outs


signInputEd25519 :: Haskoin.Tx -> [SecretKey] -> (Int, TxInput) -> TxInput
signInputEd25519 tx keys (i, inp@(TxInput op (CCInput cond))) =
  let ms@(Just sigInput) = getSigInput inp
      message = getMessageToSign tx i sigInput
      sign c sk = fulfillEd25519 (toPublic sk) sk message c
      cond' = foldl sign cond keys
      newInput = TxInput op (CCInput cond')
   in maybe inp (\_ -> newInput) ms
signInputEd25519 _ _ (_, inp) = inp


getMessageToSign :: Haskoin.Tx -> Int -> Haskoin.SigInput -> ByteString
getMessageToSign tx i (Haskoin.SigInput so _ sh rdmM) =
    encode $ txSigHash tx (encodeOutput $ fromMaybe so rdmM) i sh


signTxBitcoin :: KTx -> [Haskoin.PrvKey] -> Except Err KTx
signTxBitcoin (KTx ins outs) keys =
  let sigInputs = mapMaybe getSigInput ins
      emptyIns = [Haskoin.TxIn op "" 0 | TxInput op _ <- ins]
      tx = Haskoin.createTx 1 emptyIns (toHaskoinOutput <$> outs) 0
      esigned@(Right signed) = Haskoin.signTx tx sigInputs keys
      newIns = zipWith mergeScript ins (Haskoin.txIn signed)
  in case esigned of Left err -> throwE $ otherErr err
                     _        -> pure $ KTx newIns outs
  where
    mergeScript :: TxInput -> Haskoin.TxIn -> TxInput
    mergeScript t@(TxInput op _) (Haskoin.TxIn _ bs _) =
      if bs == mempty then t else TxInput op (ScriptInput bs)


getSigInput :: TxInput -> Maybe Haskoin.SigInput
getSigInput (TxInput op script) =
  let toSigInput inp = Haskoin.SigInput inp op sigType Nothing
      sigType = Haskoin.SigAll False
   in toSigInput <$> case script of
           AddressInput (Haskoin.PubKeyAddress h160) -> Just (Haskoin.PayPKHash h160)
           PubKeyInput pk                            -> Just (Haskoin.PayPK pk)
           CCInput cond                              -> Just (Haskoin.PayCondition cond)
           _                                         -> Nothing
