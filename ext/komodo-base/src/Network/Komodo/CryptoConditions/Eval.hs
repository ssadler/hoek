{-# LANGUAGE OverloadedStrings #-}

module Network.Komodo.CryptoConditions.Eval where

import           Data.ASN1.Parse
import           Data.ASN1.Types
import           Data.ByteString (ByteString)
import           Data.Text (Text)
import           Data.Text.Encoding

import           Network.CryptoConditions.Impl
import           Network.CryptoConditions.Encoding


type Method = Text
type Params = ByteString
type Entropy = ByteString


--------------------------------------------------------------------------------
-- | (15) Eval-SHA256 Condition
--


evalType :: ConditionType
evalType = CT 15 "eval-sha-256" False "sha-256"


evalCost :: Int
evalCost = 131072


evalFingerprint :: Method -> Params -> Fingerprint
evalFingerprint method condEval =
  hashASN $ asnSequence Sequence $ asnData [encodeUtf8 method, condEval]


evalFulfillmentASN :: Method -> Params -> [ASN1]
evalFulfillmentASN method params =
  asnChoice 15 $ asnData [encodeUtf8 method, params]


parseEval :: (Method -> Params -> c) -> ParseASN1 c
parseEval construct = do
  construct <$> (decodeUtf8 <$> parseOther 0) <*> parseOther 1


verifyEval :: Method -> Params -> Message -> Bool
verifyEval _ _ _ = True

