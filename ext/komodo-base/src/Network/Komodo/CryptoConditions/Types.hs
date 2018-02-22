{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Network.Komodo.CryptoConditions.Types (
    module IE
  , Condition(..)
  ) where

import qualified Crypto.PubKey.Ed25519 as Ed2

import           Data.Aeson
import qualified Data.Set as Set
import           Data.Word

import qualified Network.CryptoConditions as CC
import           Network.CryptoConditions.Impl as IE

import           Network.Komodo.Crypto
import           Network.Komodo.Crypto.B58Keys
import           Network.Komodo.Data.Aeson


data Condition =
    Preimage Preimage
  | Prefix Prefix Int Condition
  | Threshold Word16 [Condition]
  | Ed25519 Ed2.PublicKey (Maybe Ed2.Signature)
  | Anon Int Fingerprint Int (Set.Set ConditionType)
  deriving (Show, Eq)


instance IsCondition Condition where
  getType (Anon 0 _ _ _) = preimageType
  getType (Anon 1 _ _ _) = prefixType
  getType (Anon 2 _ _ _) = thresholdType
  getType (Anon 4 _ _ _) = ed25519Type
  getType (Threshold _ _) = thresholdType
  getType (Ed25519 _ _) = ed25519Type
  getType (Preimage _) = preimageType
  getType (Prefix _ _ _) = prefixType

  getCost (Threshold t subs) = thresholdCost t subs
  getCost (Ed25519 _ _) = ed25519Cost
  getCost (Preimage pre) = preimageCost pre
  getCost (Prefix pre mml c) = prefixCost pre mml c
  getCost (Anon _ _ c _) = c

  getFingerprint (Threshold t subs) = thresholdFingerprint t subs
  getFingerprint (Ed25519 pk _) = ed25519Fingerprint pk
  getFingerprint (Preimage pre) = preimageFingerprint pre
  getFingerprint (Prefix pre mml c) = prefixFingerprint pre mml c
  getFingerprint (Anon _ fp _ _) = fp

  getFulfillment (Threshold t subs) = thresholdFulfillment t subs
  getFulfillment (Ed25519 pk msig) = ed25519Fulfillment pk msig
  getFulfillment (Preimage pre) = Just $ preimageFulfillment pre
  getFulfillment (Prefix pre mml c) =  prefixFulfillment pre mml c
  getFulfillment (Anon _ _ _ _) = Nothing

  getSubtypes (Threshold _ sts) = thresholdSubtypes sts
  getSubtypes (Anon _ _ _ sts)  = sts
  getSubtypes (Prefix _ _ c)    = prefixSubtypes c
  getSubtypes _                 = mempty

  parseFulfillment 0 = parsePreimage Preimage
  parseFulfillment 1 = parsePrefix Prefix
  parseFulfillment 2 = parseThreshold Threshold
  parseFulfillment 4 = parseEd25519 Ed25519

  verifyMessage (Preimage image) = verifyPreimage image
  verifyMessage (Prefix pre mml cond) = verifyPrefix pre mml cond
  verifyMessage (Threshold m subs) = verifyThreshold m subs
  verifyMessage (Ed25519 pk (Just sig)) = verifyEd25519 pk sig
  verifyMessage _ = const False

  anon t f c = Anon t f c . toConditionTypes


toConditionTypes :: Set.Set Int -> Set.Set ConditionType
toConditionTypes = Set.map $
  let u = undefined in (\tid -> getType $ Anon tid u u u)


instance ToJSON Condition where
  toJSON (Ed25519 pk msig) =
    let sig = maybe [] (\s -> ["signature" .= Sig s]) msig
     in object ([ "type" .= ("ed25519-sha-256" :: String)
                , "publicKey" .= PK pk
                ] ++ sig)
  toJSON (Threshold n subs) =
    object [ "type" .= String "threshold-sha-256"
           , "threshold" .= n
           , "subfulfillments" .= (toJSON <$> subs)
           ]
  toJSON cond@(Anon _ _ _ _) =
    object [ "type" .= ("condition" :: String)
           , "uri" .= getConditionURI cond
           ]


instance FromJSON Condition where
  parseJSON = withStrictObject "condition" $ \obj -> do
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

