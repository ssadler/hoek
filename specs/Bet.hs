{-# LANGUAGE OverloadedStrings #-}

module Network.Komodo.Specs.Bet
  ( module X
  , addressScript
  , addressOutput
  , assert
  , ecCond
  , encodePayouts
  , signEncode
  , writePrettyJson
  , execMerkleBranch
  , getMerkleBranch
  , getMerkleRoot
  , putVarList
  , getVarList
  ) where

import           Data.Aeson (ToJSON)
import           Data.Aeson.Encode.Pretty
import           Data.Bits as X
import qualified Data.ByteString.Lazy.Char8 as C8L
import           Data.Ord (comparing)
import           Data.Serialize as X
import           Debug.Trace as X
import           Network.Komodo.CryptoConditions as X
import           Network.Komodo.Prelude as X
import           Network.Komodo.Transaction as X
import qualified Network.Haskoin.Internals as H


execMerkleBranch :: (Bits a, Integral a) => (a, [H.Hash256]) -> H.Hash256 -> H.Hash256
execMerkleBranch (sides, xs) = e xs 0
  where e [] _ h = h
        e (x:xs) i h = e xs (i+1) $
          if testBit sides i then H.hash2 x h else H.hash2 h x


getMerkleBranch :: Integral a => [H.Hash256] -> Int -> (a, [H.Hash256])
getMerkleBranch xs pos
  | pos >= length xs = error "position out of range in getMerkleBranch"
  | otherwise = (fromIntegral pos, s xs pos)
  where
    s [] _ = []
    s [l] _ = []
    s leaves idx =
      let hashes = hashMerkleLeaves leaves
          newIdx = div idx 2
          side = min (xor idx 1) (length leaves - 1)
       in leaves !! side : s hashes newIdx


hashMerkleLeaves :: [H.Hash256] -> [H.Hash256]
hashMerkleLeaves leaves =
  let pairs = [0,2..length leaves - 1]
      combine n =
        case take 2 (drop n leaves) of
             [a,b] -> H.hash2 a b
             [a]   -> H.hash2 a a
   in combine <$> pairs


getMerkleRoot :: [H.Hash256] -> H.Hash256
getMerkleRoot [] = error "can't getMerkleRoot of no elements"
getMerkleRoot [h] = h
getMerkleRoot leaves = getMerkleRoot $ hashMerkleLeaves leaves


writePrettyJson :: ToJSON a => FilePath -> a -> IO ()
writePrettyJson path = C8L.writeFile path . encodePretty' conf
  where conf = defConfig { confCompare = comparing cmp }
        cmp "type" = "\0"
        cmp "amount" = "\0"
        cmp "subfulfillments" = "x"
        cmp "script" = "x"
        cmp k = k


ecCond :: H.PubKey -> Condition
ecCond pk = Secp256k1 (H.pubKeyPoint pk) Nothing


addressScript :: H.PubKey -> InputScript
addressScript = AddressInput . H.pubKeyAddr


addressOutput :: Amount -> H.PubKey -> TxOutput
addressOutput n = TxOutput n . AddressOutput . H.pubKeyAddr


signEncode :: [H.PrvKey] -> KTx -> H.Tx
signEncode keys tx = 
  let Right r = runExcept $ signTxSecp256k1 keys tx >>= signTxBitcoin keys >>= encodeTx
   in r


putVarList :: Serialize a => Putter [a]
putVarList l = do
  put $ H.VarInt $ fromIntegral $ length l
  mapM_ put l


getVarList :: Serialize a => Get [a]
getVarList = do
  H.VarInt len <- get
  replicateM (fromIntegral len) get


encodePayouts :: [TxOutput] -> ByteString
encodePayouts payouts = runPut $ putVarList $ toHaskoinOutput <$> payouts


assert :: String -> Bool -> IO ()
assert label cond = if not cond then fail ("could not assert: " ++ label) else pure ()

