{-# LANGUAGE OverloadedStrings #-}

module Network.Komodo.RPC where

import Control.Applicative
import Control.Exception (try)

import Data.Aeson.Types
import Data.String

import Network.HTTP.Simple
import Network.Komodo.Prelude

import System.IO
import System.Exit
import System.Process


data RPCClient = RPCClient Request
  deriving (Show)


getRpcClient :: ExceptT Err IO RPCClient
getRpcClient = do
  let cmd = ". $KOMODO_CONF_PATH;\
            \ echo -n http://$rpcuser:$rpcpassword@${rpchost:-127.0.0.1}:$rpcport"
  url <- runShell cmd
  pure $ RPCClient $ fromString $ "POST " ++ url


rpc :: FromJSON a => RPCClient -> String -> [Value] -> ExceptT Err IO a
rpc (RPCClient req) method params = do
  let val = object ["method" .= method, "params" .= params]
  jsonRes <- getResult val
  ExceptT $ pure $ case parseEither parseResult jsonRes of
                        Left str -> Left (otherErr str)
                        Right r -> r
  where
    getResult reqData = do
      ebody <- liftIO $ try $
        getResponseBody <$> httpJSON (setRequestBodyJSON reqData req)
      case ebody of
           Left e -> throwE $ errStr RPCTransportError $ show (e :: HttpException)
           Right val -> pure val

    parseResult = withObject "Response" $ \o -> do
      error <- o .:? "error" :: Parser (Maybe Value)
      let err = Err $ object ["class" .= RPCMethodError, "error" .= error]
      case error of
           Nothing -> Right <$> o .: "result"
           Just e -> pure $ Left $ err


runShell :: String -> ExceptT Err IO String
runShell cmd = do
  (_, Just stdout, _, proc) <- liftIO $ createProcess $ (shell cmd) { std_out = CreatePipe }
  ec <- liftIO $ waitForProcess proc
  case ec of ExitSuccess -> liftIO $ hGetContents stdout
             ExitFailure i -> throwE $ otherErr $ "Command returned " ++ show i ++ ": " ++ cmd
