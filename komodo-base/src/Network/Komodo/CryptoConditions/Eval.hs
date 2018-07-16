{-# LANGUAGE OverloadedStrings #-}

module Network.Komodo.CryptoConditions.Eval where

import           Data.ASN1.Parse
import           Data.ASN1.Types
import           Data.ByteString (ByteString)
import           Data.Text (Text)
import           Data.Text.Encoding

import           Network.CryptoConditions.Impl
import           Network.CryptoConditions.Encoding


type Code = ByteString
type Entropy = ByteString


--------------------------------------------------------------------------------
-- | (15) Eval-SHA256 Condition
--


evalType :: ConditionType
evalType = CT 15 "eval-sha-256" False "sha-256"


evalCost :: Int
evalCost = 1048576


evalFingerprint :: Code -> Fingerprint
evalFingerprint condEval =
  hashASN $ asnSequence Sequence $ asnData [condEval]


evalFulfillmentASN :: Code -> [ASN1]
evalFulfillmentASN params = asnChoice 15 $ asnData [params]


parseEval :: (Code -> c) -> ParseASN1 c
parseEval construct = do
  construct <$> parseOther 0


verifyEval :: Code -> Message -> Bool
verifyEval _ _ = True

