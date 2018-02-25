module Network.Komodo.API.Utils where

import Control.Monad ((>=>))

import Data.Aeson.Types

import Network.Komodo.Prelude


type JsonMethod = Value -> ExceptT Err IO Value


ioMethod :: (FromJSON a, Monad m)
         => (a -> Parser (ExceptT Err m Value))
         -> Value -> ExceptT Err m Value
ioMethod parse val = do
  let res = parseEither (parseJSON >=> parse) val
  join $ either (throwE . errStr InvalidParams) pure res


pureMethod :: FromJSON a => (a -> Parser (ExceptT Err Identity Value)) -> JsonMethod
pureMethod parse val = do
  let (Identity res) = runExceptT $ ioMethod parse val
  ExceptT $ pure res
