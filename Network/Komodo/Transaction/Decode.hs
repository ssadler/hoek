module Network.Komodo.Transaction.Decode where

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
import qualified Network.Haskoin.Script as Haskoin


decodeTx :: Haskoin.Tx -> Except Err KTx
decodeTx tx = do
  ins <- mapM toTxInput $ Haskoin.txIn tx
  outs <- mapM toTxOutput $ Haskoin.txOut tx
  pure $ KTx ins outs


toTxInput :: Haskoin.TxIn -> Except Err TxInput
toTxInput (Haskoin.TxIn op script _) = pure $ TxInput op $ ScriptInput script


toTxOutput :: Haskoin.TxOut -> Except Err TxOutput
toTxOutput (Haskoin.TxOut amount script) = TxOutput amount <$> 
  case Haskoin.decodeOutputBS script of
       Left err -> throwE $ otherErr err
       Right so -> convertScriptOutput so


convertScriptOutput :: Haskoin.ScriptOutput -> Except Err OutputScript
convertScriptOutput (Haskoin.PayPKHash hash) = pure $ AddressOutput $ Haskoin.PubKeyAddress hash
convertScriptOutput (Haskoin.PayCondition cond) = pure $ CCOutput cond
convertScriptOutput (Haskoin.PayPK pk) = pure $ PubKeyOutput pk
convertScriptOutput so = throwE $ otherErr $ "Cannot decode output: " ++ show so
