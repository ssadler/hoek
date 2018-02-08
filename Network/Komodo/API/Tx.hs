{-# LANGUAGE OverloadedStrings #-}

module Network.Komodo.API.Tx where

import           Data.Aeson hiding (encode)
import           Data.Aeson.Types (Parser, parseEither)
import qualified Data.Map as Map
import           Data.Serialize

import           Network.Haskoin.Util (encodeHex)
import           Network.Haskoin.Transaction
import           Network.Komodo.API.Utils
import           Network.Komodo.Crypto
import           Network.Komodo.Prelude
import qualified Network.Komodo.Transaction.Builder as TX

import           Lens.Micro

import Debug.Trace

createTx :: JsonMethod
createTx = pureMethod $ \obj -> do
  tx <- TX.createTx <$> obj .: "inputs" <*> obj .: "outputs"
  pure $ pure $ object ["tx" .= tx]

