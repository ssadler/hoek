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
import           Network.Komodo.CryptoConditions

import           Test.QuickCheck
import           Test.Tasty
import           Test.Tasty.HUnit

import           TestSupport


transactionTests :: TestTree
transactionTests = testGroup "testUnits"
  [ testSignInputEd25519
  , testSignTxEd25519
  ]


testSignInputEd25519 :: TestTree
testSignInputEd25519 = testCase "sign an input" $
  assertEqual "input is equal"
    (TxInput outPoint0 $ CCInput ed2BobF) 
    (signInputEd25519 [skBob] umsg $ TxInput outPoint0 $ CCInput ed2Bob)


testSignTxEd25519 :: TestTree
testSignTxEd25519 = testCase "sign transaction Ed25519 input" $
  let inputs = [TxInput outPoint0 (CCInput ed2Bob)]
      outputs = [TxOutput 1 (CCOutput ed2Alice)]
      tx = KTx inputs outputs
      res = runExcept $ signTxEd25519 tx [skBob]
  in assertBool "Input condition is fulfilled" $
      let Right (KTx [TxInput _ (CCInput cond)] _) = res
       in conditionIsSigned cond
