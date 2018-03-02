module Network.Komodo.Specs.Bet
  ( module X
  , addressScript
  , addressOutput
  , ecCond
  , writePrettyJson
  ) where

import           Data.Aeson (ToJSON)
import           Data.Aeson.Encode.Pretty as X (encodePretty)
import           Data.Bits hiding (Bits)
import qualified Data.ByteString.Lazy.Char8 as C8L
import           Data.Serialize as X
import           Network.Komodo.CryptoConditions as X
import           Network.Komodo.Transaction as X
import qualified Network.Haskoin.Internals as H
import           Network.Komodo.Prelude as X


execMerkleBranch :: [H.Hash256] -> Int -> H.Hash256 -> H.Hash256
execMerkleBranch [] _ h = h
execMerkleBranch (n:xs) bits h = execMerkleBranch xs (shiftR bits 1) $
  (if testBit bits 0 then id else flip) H.hash2 n h


merkleRoot :: [H.Hash256] -> H.Hash256
merkleRoot [h] = h
merkleRoot hs  = H.hash2 (merkleRoot left) (merkleRoot right)
  where (left, right) = splitAt i hs
        i = until (\x -> x*2 >= length hs) (*2) 1


writePrettyJson :: ToJSON a => FilePath -> a -> IO ()
writePrettyJson path = C8L.writeFile path . encodePretty


ecCond :: H.PubKey -> Condition
ecCond pk = Secp256k1 (H.pubKeyPoint pk) Nothing


addressScript :: H.PubKey -> InputScript
addressScript = AddressInput . H.pubKeyAddr


addressOutput :: Amount -> H.PubKey -> TxOutput
addressOutput n = TxOutput n . AddressOutput . H.pubKeyAddr
