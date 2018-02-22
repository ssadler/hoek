{-# LANGUAGE OverloadedStrings #-}

module Network.Komodo.CryptoConditions.Aux where


import           Data.Aeson
import           Data.ASN1.BinaryEncoding
import           Data.ASN1.Encoding
import           Data.ASN1.Parse
import           Data.ASN1.Types
import           Data.ByteString (ByteString)
import           Data.Text (Text)
import           Data.Text.Encoding

import           Network.CryptoConditions.Impl
import           Network.CryptoConditions.Encoding

import           Network.Komodo.Crypto
import           Network.Komodo.Crypto.B58Keys
import Debug.Trace


type Method = Text
type ConditionAux = ByteString
type FulfillmentAux = ByteString


--------------------------------------------------------------------------------
-- | (15) Aux-SHA256 Condition
--


auxType :: ConditionType
auxType = CT 15 "aux-sha-256" False "sha-256"


auxCost :: Int
auxCost = 131072


auxFingerprint :: Method -> ConditionAux -> Fingerprint
auxFingerprint method condAux = sha256 body
  where body = encodeASN1' DER asn
        asn = [ Start Sequence
              , Other Context 0 $ encodeUtf8 method
              , Other Context 1 condAux
              ]


-- TODO: Do we really need the bogus fulfillments?
auxFulfillment :: Method -> ConditionAux -> Maybe FulfillmentAux -> Maybe Fulfillment
auxFulfillment method condAux mFfillAux = Just $ encodeASN1' DER body
  where body = fiveBellsContainer (typeId auxType) [encodeUtf8 method, condAux, ffillAux]
        ffillAux = maybe "" id mFfillAux


parseAux :: (Method -> ConditionAux -> Maybe FulfillmentAux -> c) -> ParseASN1 c
parseAux construct = do
  (method, condAux, ffillAux) <- (,,) <$> parseOther 0 <*> parseOther 1 <*> parseOther 2
  pure $ construct (decodeUtf8 method) condAux $
            case ffillAux of "" -> Nothing
                             bs -> Just bs


verifyAux :: Method -> ConditionAux -> Maybe FulfillmentAux -> Message -> Bool
verifyAux _ _ _ _ = False -- can't really do that right now


