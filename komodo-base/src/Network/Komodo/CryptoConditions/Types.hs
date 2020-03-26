{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Network.Komodo.CryptoConditions.Types (
    module IE
  , Condition(..)
  , toJsonAnon
  ) where

import           Control.Applicative ((<|>))
import qualified Crypto.PubKey.Ed25519 as Ed2
import qualified Crypto.Secp256k1 as EC

import           Data.Aeson
import qualified Data.Attoparsec.Text as A
import qualified Data.ByteString.Base16 as B16
import           Data.Serialize
import qualified Data.Set as Set
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)
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
  | Eval Code
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
  getType (Eval _) = evalType

  getCost (Threshold t subs) = thresholdCost t subs
  getCost (Ed25519 _ _) = ed25519Cost
  getCost (Preimage pre) = preimageCost pre
  getCost (Eval _) = evalCost
  getCost (Secp256k1 _ _) = secp256k1Cost
  getCost (Anon _ _ c _) = c

  getFingerprint (Threshold t subs) = thresholdFingerprint t subs
  getFingerprint (Ed25519 pk _) = ed25519Fingerprint pk
  getFingerprint (Preimage pre) = preimageFingerprint pre
  getFingerprint (Eval code) = evalFingerprint code
  getFingerprint (Secp256k1 pk _) = secp256k1Fingerprint pk
  getFingerprint (Anon _ fp _ _) = fp

  getFulfillmentASN (Threshold t subs) = thresholdFulfillmentASN t subs
  getFulfillmentASN (Ed25519 pk msig) = ed25519FulfillmentASN pk <$> msig
  getFulfillmentASN (Preimage pre) = Just $ preimageFulfillmentASN pre
  getFulfillmentASN (Eval code) =  Just $ evalFulfillmentASN code
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
  verifyMessage (Eval code) = verifyEval code
  verifyMessage _ = const False

  anon t f c = Anon t f c . toConditionTypes


toConditionTypes :: Set.Set Int -> Set.Set ConditionType
toConditionTypes = Set.map $
  let u = undefined in (\tid -> getType $ Anon tid u u u)


instance ToJSON Condition where
  toJSON cond =
    object $ ( "type" .= typeName (getType cond) ) : f cond
    where
    f (Ed25519 pk msig) =
      let sig = maybe [] (\s -> ["signature" .= Sig s]) msig
       in [ "publicKey" .= PK pk ] ++ sig
    f (Threshold n subs) =
      [ "threshold" .= n
      , "subfulfillments" .= (toJSON <$> subs)
      ]
    f (Eval code) = [ "code" .= toB16 code ]
    f (Secp256k1 pk msig) =
      let encodeSig = toB64 . Data.Serialize.encode . EC.exportCompactSig
          sig = maybe [] (\s -> ["signature" .= encodeSig s]) msig
      in [ "publicKey" .= toB16 (EC.exportPubKey True pk)
         ] ++ sig
    f cond@(Anon _ _ _ _) = [ "uri" .= getConditionURI cond ]


instance FromJSON Condition where
  parseJSON = withStrictObject "condition" $ \obj -> do
    condType <- obj .:- "type"
    muri <- obj .:-? "uri"

    case muri of
      Just uri -> do
        case parseConditionUri uri of
          Nothing -> fail "Could not parse condition URI"
          Just cond -> pure cond

      Nothing ->
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
                  let code = obj .:- "code" >>= fromB16
                  Eval <$> code
             "secp256k1-sha-256" -> do
                  pkData <- obj .:- "publicKey" >>= parseB16
                  sigData <- obj .:-? "signature" >>= mapM fromB64
                  makeSecp256k1 Secp256k1 pkData sigData
             _ -> fail ("Unsupported condition type: " ++ condType)

    where
      fromB16 s = case B16.decode (encodeUtf8 s) of (r,"") -> pure r
                                                    _      -> fail "Invalid b16"


lookupTypeByName :: Text -> Maybe ConditionType
lookupTypeByName "ed25519-sha-256"   = Just ed25519Type
lookupTypeByName "threshold-sha-256" = Just thresholdType
lookupTypeByName "eval-sha-256"      = Just evalType
lookupTypeByName "secp256k1-sha-256" = Just secp256k1Type
lookupTypeByName                   _ = Nothing



parseConditionUri :: Text -> Maybe Condition
parseConditionUri = do
  r <- A.parseOnly parser
  pure $ either (const Nothing) Just r
  where
  parser = do
    "ni:///sha-256;"
    hash <- A.takeWhile (/='?') >>= fromB64
    "?fpt="
    name <- lookupTypeByName <$> A.takeWhile (/='&')
    ct <- case name of Nothing -> fail "Unknown condition type"
                       Just c -> pure c
    "&cost="
    cost <- A.decimal
    subtypes <- (A.endOfInput *> mempty) <|> parseSubtypes
    pure $ anon (typeId ct) hash cost subtypes
  parseSubtypes = do
    "&subtypes="
    names <- A.sepBy1 (A.takeWhile (/=',')) ","
    case mapM lookupTypeByName names of
      Nothing -> fail "Invalid subtypes"
      Just ids -> pure $ Set.fromList $ typeId <$> ids


