{-# LANGUAGE OverloadedStrings #-}

module Network.Komodo.CryptoConditions
  ( module DSL
  , module Types
  , conditionIsSigned
  , getConditionPubkeys
  , parsePolyFulfillment
  ) where


import qualified Crypto.PubKey.Ed25519 as Ed2

import           Data.Aeson.Types
import           Data.Text.Encoding (encodeUtf8)

import           Network.Komodo.Crypto
import           Network.Komodo.CryptoConditions.DSL as DSL
import           Network.Komodo.CryptoConditions.Types as Types
import           Network.Komodo.Prelude



getConditionPubkeys :: CryptoCondition -> [PublicKey]
getConditionPubkeys (Ed25519 pk _) = [PK pk]
getConditionPubkeys (Threshold _ cs) = cs >>= getConditionPubkeys
getConditionPubkeys _ = []


parsePolyFulfillment :: Value -> Parser CryptoCondition
parsePolyFulfillment val =
  case val of (Object _) -> parseJSON val
              (String t) -> let econd = readFulfillmentBase64 (encodeUtf8 t)
                            in either fail pure econd
              _          -> typeMismatch "object or string" val


conditionIsSigned :: CryptoCondition -> Bool
conditionIsSigned (Threshold _ cs) = all conditionIsSigned cs
conditionIsSigned (Ed25519 _ (Just _)) = True
conditionIsSigned _ = False


fulfillEd25519 :: Ed2.PublicKey -> Ed2.Signature
               -> CryptoCondition -> CryptoCondition
fulfillEd25519 pk sig (Threshold t subs) =
  Threshold t $ fulfillEd25519 pk sig <$> subs
fulfillEd25519 pk sig e@(Ed25519 pk' Nothing) =
  if pk == pk' then Ed25519 pk (Just sig) else e
fulfillEd25519 _ _ c = c


readStandardFulfillmentBase64 :: ByteString -> Except Err CryptoCondition
readStandardFulfillmentBase64 = either throw pure . readFulfillmentBase64
  where throw = throwE . errStr TxInvalidFulfillment
