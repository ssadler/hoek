{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.Komodo.Crypto
  ( genEd25519KeyPair
  , sha3
  , Ed2.sign
  , Ed2.toPublic
  , Ed2.verify
  , parseSecretKey
  ) where

import Crypto.Error (CryptoFailable(..), throwCryptoError)
import Crypto.Hash
import Crypto.Random
import qualified Crypto.PubKey.Ed25519 as Ed2
import Data.ByteString (ByteString)

import Network.Komodo.Crypto.B58Keys as BK


sha3 :: ByteString -> String
sha3 bs = show (hash bs :: Digest SHA3_256)


genEd25519KeyPair :: IO (PublicKey, SecretKey)
genEd25519KeyPair = do
  drg <- getSystemDRG
  -- TODO: scrubbed bytes array?
  let (bs,_) = randomBytesGenerate 32 drg
      (CryptoPassed sk) = Ed2.secretKey (bs::ByteString)
  pure (PK $ Ed2.toPublic sk, SK sk)
