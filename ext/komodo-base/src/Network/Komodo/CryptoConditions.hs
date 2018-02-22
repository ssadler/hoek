{-# LANGUAGE OverloadedStrings #-}

module Network.Komodo.CryptoConditions
  ( module DSL
  , module Types
  , conditionIsSigned
  , getConditionPubkeys
  , parsePolyFulfillment
  ) where

import           Control.Monad.Trans.Except

import           Crypto.Error
import qualified Crypto.PubKey.Ed25519 as Ed2

import           Data.Aeson.Types
import           Data.ByteString (ByteString)
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
              (String t) -> let econd = readFulfillmentBase64 (encodeUtf8 t)
                            in either fail pure econd
              _          -> typeMismatch "object or string" val


conditionIsSigned :: Condition -> Bool
conditionIsSigned (Threshold _ cs) = all conditionIsSigned cs
conditionIsSigned (Ed25519 _ (Just _)) = True
conditionIsSigned _ = False


readStandardFulfillmentBase64 :: ByteString -> Except String Condition
readStandardFulfillmentBase64 = ExceptT . pure . readFulfillmentBase64
