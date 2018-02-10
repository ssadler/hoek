{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Network.Komodo.CryptoConditions.Types (
    module IE
  , CryptoCondition(..)
  , validate
  ) where

import qualified Crypto.PubKey.Ed25519 as Ed2

import           Data.Aeson
import qualified Data.Set as Set
import           Data.Word

import           Network.CryptoConditions.Impl
import qualified Network.CryptoConditions.Impl as IE

import           Network.Komodo.Crypto
import           Network.Komodo.Crypto.B58Keys
import           Network.Komodo.Data.Aeson
import           Network.Komodo.Prelude


data CryptoCondition =
    Threshold Word16 [CryptoCondition]
  | Ed25519 Ed2.PublicKey (Maybe Ed2.Signature)
  | Anon Int Fingerprint Int (Set.Set ConditionType)
  deriving (Show, Eq)


instance IsCondition CryptoCondition where
  getType (Anon 2 _ _ _) = thresholdType
  getType (Anon 4 _ _ _) = ed25519Type
  getType (Threshold _ _) = thresholdType
  getType (Ed25519 _ _) = ed25519Type

  getCost (Threshold t subs) = thresholdCost t subs
  getCost (Ed25519 _ _) = ed25519Cost
  getCost (Anon _ _ c _) = c

  getFingerprint (Threshold t subs) = thresholdFingerprint t subs
  getFingerprint (Ed25519 pk _) = ed25519Fingerprint pk
  getFingerprint (Anon _ fp _ _) = fp

  getFulfillment (Threshold t subs) = thresholdFulfillment t subs
  getFulfillment (Ed25519 pk msig) = ed25519Fulfillment pk msig
  getFulfillment (Anon _ _ _ _) = Nothing

  getSubtypes (Threshold _ sts) = thresholdSubtypes sts
  getSubtypes (Anon _ _ _ sts) = sts
  getSubtypes _                = mempty

  parseFulfillment 2 = parseThreshold Threshold
  parseFulfillment 4 = parseEd25519 Ed25519

  verifyMessage (Threshold m subs) = verifyThreshold m subs      
  verifyMessage (Ed25519 pk (Just sig)) = verifyEd25519 pk sig   
  verifyMessage _ = const False                                  

  anon t f c = Anon t f c . toConditionTypes


toConditionTypes :: Set.Set Int -> Set.Set ConditionType
toConditionTypes = Set.map $
  let u = undefined in (\tid -> getType $ Anon tid u u u)


instance ToJSON CryptoCondition where
  toJSON (Ed25519 pk _) =
    object [ "type" .= ("ed25519-sha-256" :: String)
           , "public_key" .= PK pk
           ]
  toJSON (Threshold n subs) = 
    object [ "type" .= String "threshold-sha-256"
           , "threshold" .= n
           , "subconditions" .= (toJSON <$> subs)
           ]
  toJSON _ = undefined


instance FromJSON CryptoCondition where
  parseJSON = withStrictObject "a" $ \obj -> do
    condType <- obj .:- "type"
    case condType of
         "ed25519-sha-256" -> do
           (PK pk) <- obj .:- "public_key"
           pure $ Ed25519 pk Nothing
         "threshold-sha-256" -> do
           subconds <- obj .:- "subconditions" >>= mapM parseJSON
           Threshold <$> obj .:- "threshold" <*> pure subconds
         _ -> fail ("Unsupported condition type: " ++ condType)

