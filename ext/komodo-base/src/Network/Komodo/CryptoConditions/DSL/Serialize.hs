{-# LANGUAGE OverloadedStrings #-}

module Network.Komodo.CryptoConditions.DSL.Serialize
  ( serializeDSL
  , deserializeDSL
  ) where

import           Control.Applicative
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State

import qualified Data.Attoparsec.Text as AT
import           Data.Monoid
import qualified Data.Text as T
import qualified Data.List as List

import           Network.Komodo.Crypto
import           Network.Komodo.Crypto.B58Keys
import           Network.Komodo.CryptoConditions.DSL.Parse
import           Network.Komodo.CryptoConditions.Types


type CryptoCondition = Condition

serializeDSL :: CryptoCondition -> (T.Text, [PublicKey])
serializeDSL cond = runState (serialize cond) []


serialize :: CryptoCondition -> State [PublicKey] T.Text
serialize (Threshold t subs) = do
  subs' <- mapM serialize subs
  return $ "(" <> T.pack (show t) <> " of "
               <> T.intercalate ", " subs' <> ")"
serialize (Ed25519 pk _) = do
  local <- state $ localName (PK pk)
  return $ "%" <> T.pack (show local)
serialize _ = fail "unable to serialize"


localName :: PublicKey -> [PublicKey] -> (Int, [PublicKey])
localName pk locals =
   case List.elemIndex pk locals of
     Just i -> (i, locals)
     Nothing -> (length locals, locals ++ [pk])


--------------------------------------------------------------------------------
-- Deserialize DSL - splices variables back into expression
--
deserializeDSL :: T.Text -> [T.Text] -> Except String CryptoCondition
deserializeDSL expr locals = do
  let parse = AT.parseOnly (spliceVars locals) expr 
  spliced <- ExceptT $ return parse
  parseDSL spliced


spliceVars :: [T.Text] -> AT.Parser T.Text
spliceVars locals = do
  head' <- AT.takeWhile (/='%')
  next <- ("%" >> pure splice) <|> pure (pure "")
  (head' <>) <$> next
  where
    splice = do
      name <- AT.decimal
      case Just (locals !! name) of
           Just key -> (key <>) <$> spliceVars locals
           Nothing -> fail $ "%" ++ show name ++ " is not defined"
