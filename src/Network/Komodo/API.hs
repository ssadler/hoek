{-# LANGUAGE OverloadedStrings #-}

module Network.Komodo.API
  ( module API
  , methods
  , runJsonRpc
  , runMethod
  , wrapJson
  ) where

import           Data.Aeson.Types
import qualified Data.Map as Map

import           Lens.Micro

import           Network.Komodo.API.Utils as API
import           Network.Komodo.API.Tx as API
import           Network.Komodo.Crypto as C
import           Network.Komodo.Prelude
import           Network.Haskoin.Internals as H


methods :: Map.Map String (JsonMethod, String)
methods = Map.fromList
  [ ("ed25519KeyPair",   (ed25519KeyPair, "Generate an Ed25519 key pair"))
  , ("secp256k1KeyPair", (secp256k1KeyPair, "Generate an Secp256k1 key pair"))
  , ("encodeTx",         (encodeTx, "Encode a transaction to hex"))
  , ("decodeTx",         (decodeTx, "Decode a transaction from hex"))
  , ("getTxid",          (getTxid, "Get a transaction ID"))
  , ("signTxBitcoin",    (signTxBitcoin, "Sign Secp256k1 script inputs"))
  , ("signTxEd25519",    (signTxEd25519, "Sign Ed25519 condition nodes"))
  , ("signTxSecp256k1",  (signTxSecp256k1, "Sign Secp256k1 condition nodes"))
  , ("signTx",           (API.signTx, "Sign tx secp256k1, cc and script input"))
  , ("decodeScript",     (decodeScript, "Decode a script"))
  , ("encodeCondition",  (encodeConditionJSON, "Encode a Crypto-Conditions condition"))
  ]


runJsonRpc :: Value -> ExceptT Err IO Value
runJsonRpc val = do
  let res = parseEither parseRequest val
  (name, params) <- ExceptT $ pure $ over _Left invalidRequest res
  runMethod name params
  where
    invalidRequest = errStr InvalidProtocol
    parseRequest = withObject "request" $ \obj ->
      (,) <$> obj .: "method" <*> obj .: "params"


runMethod :: String -> Value -> ExceptT Err IO Value
runMethod name params = do
  let throw = throwE $ errStr InvalidMethod name
  (method,_) <- maybe throw pure $ Map.lookup name methods
  method params


wrapJson :: Either Err Value -> Value
wrapJson = either wrapJsonError wrapSuccess
  where
    wrapSuccess val = object ["result" .= val]
    wrapJsonError val = object ["error" .= val]


ed25519KeyPair :: JsonMethod
ed25519KeyPair _ = do
  (pk, sk) <- lift genEd25519KeyPair
  return $ object ["public_key" .= pk, "secret_key" .= sk]


secp256k1KeyPair :: JsonMethod
secp256k1KeyPair _ = do
  sk <- lift $ withSource getEntropy genPrvKey
  let wif = decodeUtf8 $ toWif sk
      pk = derivePubKey sk
  return $ object [ "pubKey" .= pk
                  , "wif" .= wif
                  , "addr" .= pubKeyAddr pk
                  ]



showErrorClasses :: JsonMethod
showErrorClasses _ = return $ object ["errors" .= allErrorClasses]
