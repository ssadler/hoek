{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Network.Komodo.CryptoConditions.Types (
    module IE
  , Condition(..)
  ) where

import qualified Crypto.PubKey.Ed25519 as Ed2
import qualified Crypto.Secp256k1 as EC

import           Data.Aeson
import           Data.Serialize
import qualified Data.Set as Set
import           Data.Word

import qualified Network.CryptoConditions as CC
import           Network.CryptoConditions.Impl as IE
import           Network.CryptoConditions.Json

import           Network.Komodo.Crypto
import           Network.Komodo.Crypto.B58Keys
import           Network.Komodo.Crypto.B16Keys
import           Network.Komodo.CryptoConditions.Eval
import           Network.Komodo.CryptoConditions.Secp256k1
import           Network.Komodo.Data.Aeson


data Condition =
    Preimage Preimage
  | Threshold Word16 [Condition]
  | Ed25519 Ed2.PublicKey (Maybe Ed2.Signature)
  | Secp256k1 EC.PubKey (Maybe EC.Sig)
  | Eval Method Params
  | Anon Int Fingerprint Int (Set.Set ConditionType)
  deriving (Show, Eq)


instance IsCondition Condition where
  getType (Anon 0 _ _ _) = preimageType
  getType (Anon 2 _ _ _) = thresholdType
  getType (Anon 4 _ _ _) = ed25519Type
  getType (Anon 5 _ _ _) = secp256k1Type
  getType (Anon 15 _ _ _) = evalType
  getType (Threshold _ _) = thresholdType
  getType (Ed25519 _ _) = ed25519Type
  getType (Preimage _) = preimageType
  getType (Secp256k1 _ _) = secp256k1Type
  getType (Eval _ _) = evalType

  getCost (Threshold t subs) = thresholdCost t subs
  getCost (Ed25519 _ _) = ed25519Cost
  getCost (Preimage pre) = preimageCost pre
  getCost (Eval _ _) = evalCost
  getCost (Secp256k1 _ _) = secp256k1Cost
  getCost (Anon _ _ c _) = c

  getFingerprint (Threshold t subs) = thresholdFingerprint t subs
  getFingerprint (Ed25519 pk _) = ed25519Fingerprint pk
  getFingerprint (Preimage pre) = preimageFingerprint pre
  getFingerprint (Eval m pre) = evalFingerprint m pre
  getFingerprint (Secp256k1 pk _) = secp256k1Fingerprint pk
  getFingerprint (Anon _ fp _ _) = fp

  getFulfillmentASN (Threshold t subs) = thresholdFulfillmentASN t subs
  getFulfillmentASN (Ed25519 pk msig) = ed25519FulfillmentASN pk <$> msig
  getFulfillmentASN (Preimage pre) = Just $ preimageFulfillmentASN pre
  getFulfillmentASN (Eval m pre) =  Just $ evalFulfillmentASN m pre
  getFulfillmentASN (Secp256k1 pk msig) = secp256k1FulfillmentASN pk <$> msig
  getFulfillmentASN (Anon _ _ _ _) = Nothing

  getSubtypes (Threshold _ sts) = thresholdSubtypes sts
  getSubtypes (Anon _ _ _ sts)  = sts
  getSubtypes _                 = mempty

  parseFulfillment 0 = parsePreimage Preimage
  parseFulfillment 2 = parseThreshold Threshold
  parseFulfillment 4 = parseEd25519 (\a b -> Ed25519 a $ Just b)
  parseFulfillment 5 = parseSecp256k1 Secp256k1
  parseFulfillment 15 = parseEval Eval

  verifyMessage (Preimage image) = verifyPreimage image
  verifyMessage (Threshold m subs) = verifyThreshold m subs
  verifyMessage (Ed25519 pk (Just sig)) = verifyEd25519 pk sig
  verifyMessage (Secp256k1 pk (Just sig)) = verifySecp256k1 pk sig
  verifyMessage (Eval m pre) = verifyEval m pre
  verifyMessage _ = const False

  anon t f c = Anon t f c . toConditionTypes


toConditionTypes :: Set.Set Int -> Set.Set ConditionType
toConditionTypes = Set.map $
  let u = undefined in (\tid -> getType $ Anon tid u u u)


instance ToJSON Condition where
  toJSON (Ed25519 pk msig) =
    let sig = maybe [] (\s -> ["signature" .= Sig s]) msig
     in object $ [ "type" .= ("ed25519-sha-256" :: String)
                 , "publicKey" .= PK pk
                 ] ++ sig
  toJSON (Threshold n subs) =
    object [ "type" .= String "threshold-sha-256"
           , "threshold" .= n
           , "subfulfillments" .= (toJSON <$> subs)
           ]
  toJSON (Eval m pre) =
    object $ [ "type" .= String "eval-sha-256"
             , "method" .= m
             , "params" .= toB64 pre
             ]
  toJSON (Secp256k1 pk msig) =
    let encodeSig = toB64 . Data.Serialize.encode . EC.exportCompactSig
        sig = maybe [] (\s -> ["signature" .= encodeSig s]) msig
    in object $ [ "type" .= (String "secp256k1-sha-256")
                , "publicKey" .= toB16 (EC.exportPubKey True pk)
                ] ++ sig
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
         "eval-sha-256" -> do
              let params = obj .:- "params" >>= fromB64
              Eval <$> obj .:- "method" <*> params
         "secp256k1-sha-256" -> do
              pkData <- obj .:- "publicKey" >>= parseB16
              sigData <- obj .:-? "signature" >>= mapM fromB64
              makeSecp256k1 Secp256k1 pkData sigData
         _ -> fail ("Unsupported condition type: " ++ condType)
