{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Network.Komodo.CryptoConditions.Types (
    module IE
  , module CC
  , B58Condition(..)
  ) where

import qualified Crypto.PubKey.Ed25519 as Ed2

import           Data.Aeson
import qualified Data.Set as Set
import           Data.Word

import           Network.CryptoConditions as CC
import qualified Network.CryptoConditions.Impl as IE

import           Network.Komodo.Crypto
import           Network.Komodo.Crypto.B58Keys
import           Network.Komodo.Data.Aeson
import           Network.Komodo.Prelude


newtype B58Condition = B58Condition Condition

instance ToJSON B58Condition where
  toJSON (B58Condition cond) = case cond of
    Ed25519 pk msig ->
      let sig = maybe [] (\s -> ["signature" .= Sig s]) msig
       in object ([ "type" .= ("ed25519-sha-256" :: String)
                  , "publicKey" .= PK pk
                  ] ++ sig)
    Threshold n subs ->
      object [ "type" .= String "threshold-sha-256"
             , "threshold" .= n
             , "subfulfillments" .= (toJSON <$> subs)
             ]
    Anon _ _ _ _ ->
      object [ "type" .= ("condition" :: String)
             , "uri" .= getConditionURI cond
             ]


instance FromJSON B58Condition where
  parseJSON = withStrictObject "condition" $ \obj ->
    B58Condition <$> do
      condType <- obj .:- "type"
      case condType of
           "ed25519-sha-256" -> do
                PK pk <- obj .:- "publicKey"
                msig <- obj .:-? "signature"
                pure $ Ed25519 pk $ 
                  case msig of (Just (Sig s)) -> Just s
                               Nothing -> Nothing
           "threshold-sha-256" ->
                Threshold <$> obj .:- "threshold" <*> obj .:- "subfulfillments"
           _ -> fail ("Unsupported condition type: " ++ condType)

