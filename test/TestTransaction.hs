{-# LANGUAGE OverloadedStrings #-}

module TestTransaction
  ( transactionTests
  ) where

import           Data.Aeson
import           Data.Set as Set
import           Data.ByteString.Lazy

import           Network.Haskoin.Test.Crypto
import           Network.Haskoin.Constants
import           Network.Komodo.Prelude
import           Network.Komodo.Transaction

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.HUnit

import           TestSupport


transactionTests :: TestTree
transactionTests = testGroup "testUnits"
  [ testSignInput
  , testWat
  ]


testSignInput :: TestTree
testSignInput = testCase "sign an input" $
  assertEqual "input is equal"
    (TxInput outPoint0 $ CCInput ed2BobF) 
    (signInput [skBob] umsg $ TxInput outPoint0 $ CCInput ed2Bob)


testWat :: TestTree
testWat = testCase "stufff" $ do
  encode <$> sample' arbitraryAddress >>= print
  --encode <$> sample' arbitraryPrvKey >>= print
  return ()


testIntegration :: TestTree
testIntegration = testCase "do all the things" $
  let inputs = [TxInput outPoint0 (CCInput ed2Alice)]
      outputs = [TxOutput 1 (CCOutput ed2Alice)]
      tx = KTx inputs outputs
      Right signed = runExcept $ signTxEd25519 tx [skAlice]
  in assertBool "signed tx is different..." $
      signed /= tx
