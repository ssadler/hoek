{-# LANGUAGE OverloadedStrings #-}

module Network.Komodo.CryptoConditions.Secp256k1 where

import           Control.Monad (join)

import           Crypto.Secp256k1

import           Data.Aeson
import           Data.ASN1.BinaryEncoding
import           Data.ASN1.Encoding
import           Data.ASN1.Parse
import           Data.ASN1.Types
import           Data.ByteString (ByteString)
import           Data.Serialize

import           Network.CryptoConditions.Impl
import           Network.CryptoConditions.Encoding

--import           Network.Komodo.Crypto


--------------------------------------------------------------------------------
-- | (5) Secp256k1-SHA256 Condition
--

type Construct c = PubKey -> Maybe Sig -> c


secp256k1Type :: ConditionType
secp256k1Type = CT 5 "secp256k1-sha-256" False "sha-256"


secp256k1Cost :: Int
secp256k1Cost = 131072


secp256k1Fingerprint :: PubKey -> Fingerprint
secp256k1Fingerprint pk =
  sha256 $ encodeASN1' DER
        [ Start Sequence
        , Other Context 0 $ exportPubKey True pk
        ]


-- TODO: Do we really need the bogus fulfillments?
secp256k1Fulfillment :: PubKey -> Sig -> Fulfillment
secp256k1Fulfillment pk sig = encodeASN1' DER $
  let pkData = exportPubKey True pk
      sigData = Data.Serialize.encode $ exportCompactSig sig
   in fiveBellsContainer (typeId secp256k1Type) [pkData, sigData]


parseSecp256k1 :: Construct c -> ParseASN1 c
parseSecp256k1 construct = join $
  makeSecp256k1 construct <$> parseOther 0 <*> (Just <$> parseOther 1)


makeSecp256k1 :: Monad m => Construct c -> ByteString -> Maybe ByteString -> m c
makeSecp256k1 construct pkData mSigData = do
  let toPk = maybe (fail "invalid pubkey") pure . importPubKey
      toSig sigData = do
        sig <- either fail pure $ Data.Serialize.decode sigData
        maybe (fail "couldn't read compact sig") pure $ importCompactSig sig
   in construct <$> toPk pkData <*> mapM toSig mSigData


verifySecp256k1 :: PubKey -> Sig -> Message -> Bool
verifySecp256k1 pk sig msgData =
  let Just message = msg $ sha256 msgData
   in verifySig pk sig message
