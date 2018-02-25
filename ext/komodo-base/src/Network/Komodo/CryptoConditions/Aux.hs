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
auxFingerprint method condAux =
  hashASN $ asnSequence Sequence $ asnData [encodeUtf8 method, condAux]


auxFulfillmentASN :: Method -> ConditionAux -> FulfillmentAux -> [ASN1]
auxFulfillmentASN method condAux ffillAux =
  asnChoice 15 $ asnData [encodeUtf8 method, condAux, ffillAux]


parseAux :: (Method -> ConditionAux -> Maybe FulfillmentAux -> c) -> ParseASN1 c
parseAux construct = do
  (method, condAux, ffillAux) <- (,,) <$> parseOther 0 <*> parseOther 1 <*> parseOther 2
  pure $ construct (decodeUtf8 method) condAux $
            case ffillAux of "" -> Nothing
                             bs -> Just bs


verifyAux :: Method -> ConditionAux -> Maybe FulfillmentAux -> Message -> Bool
verifyAux _ _ _ _ = False -- can't really do that right now


