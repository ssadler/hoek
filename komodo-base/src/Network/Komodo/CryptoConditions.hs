{-# LANGUAGE OverloadedStrings #-}

module Network.Komodo.CryptoConditions
  ( module DSL
  , module Types
  , conditionIsSigned
  , getConditionPubkeys
  , parsePolyFulfillment
  , fulfillEd25519
  , fulfillSecp256k1
  , ed25519Condition
  , preimageCondition
  ) where

import           Control.Monad.Trans.Except

import           Crypto.Error
import qualified Crypto.PubKey.Ed25519 as Ed2
import qualified Crypto.Secp256k1 as EC

import           Data.Aeson.Types
import           Data.ByteString (ByteString)
import           Data.Monoid
import           Data.Text.Encoding (encodeUtf8)

import           Network.Komodo.Crypto
import           Network.Komodo.Crypto.B58Keys
import           Network.Komodo.CryptoConditions.DSL as DSL
import           Network.Komodo.CryptoConditions.Types as Types



getConditionPubkeys :: Condition -> [PublicKey]
getConditionPubkeys (Ed25519 pk _) = [PK pk]
getConditionPubkeys (Threshold _ cs) = cs >>= getConditionPubkeys
getConditionPubkeys _ = []


parsePolyFulfillment :: Value -> Parser Condition
parsePolyFulfillment val =
  case val of (Object _) -> parseJSON val
              (String t) -> let econd = decodeFulfillmentBase64 (encodeUtf8 t)
                            in either fail pure econd
              _          -> typeMismatch "object or string" val


conditionIsSigned :: Condition -> Bool
conditionIsSigned (Threshold _ cs) = all conditionIsSigned cs
conditionIsSigned (Ed25519 _ (Just _)) = True
conditionIsSigned _ = False


preimageCondition :: ByteString -> Condition
preimageCondition = Preimage


ed25519Condition :: Ed2.PublicKey -> Condition
ed25519Condition pk = Ed25519 pk Nothing


fulfillEd25519 :: Ed2.PublicKey -> Ed2.SecretKey
               -> Message -> Condition -> Condition
fulfillEd25519 pk sk msg c@(Ed25519 pk' _) =
  if pk == pk' then Ed25519 pk (Just $ Ed2.sign sk pk msg) else c
fulfillEd25519 pk sk msg (Threshold t subs) =
  Threshold t $ fulfillEd25519 pk sk msg <$> subs
fulfillEd25519 _ _ _ c = c


fulfillSecp256k1 :: EC.PubKey -> EC.SecKey -> EC.Msg -> Condition -> Condition
fulfillSecp256k1 pk sk msg c@(Secp256k1 pk' _)
  | pk == pk' = Secp256k1 pk $ Just $ EC.signMsg sk msg
  | otherwise = c
fulfillSecp256k1 pk sk msg (Threshold t subs) =
  Threshold t $ fulfillSecp256k1 pk sk msg <$> subs
fulfillSecp256k1 _ _ _ c = c
