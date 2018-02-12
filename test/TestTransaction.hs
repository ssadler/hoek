{-# LANGUAGE OverloadedStrings #-}

module TestTransaction
  ( transactionTests
  ) where

import           Data.Set as Set

import           Network.Komodo.Prelude
import           Network.Komodo.Transaction

import           Test.Tasty
import           Test.Tasty.HUnit

import           TestSupport


transactionTests :: TestTree
transactionTests = testGroup "testUnits"
  [ testSignInput
  ]


testSignInput :: TestTree
testSignInput = testCase "sign an input" $
  assertEqual "input is equal"
    (TxInput outPoint0 $ CCInput ed2BobF) 
    (signInput [skBob] umsg $ TxInput outPoint0 $ CCInput ed2Bob)


testIntegration :: TestTree
testIntegration = testCase "do all the things" $
  let inputs = [TxInput outPoint0 (CCInput ed2Alice)]
      outputs = [TxOutput 1 (CCOutput ed2Alice)]
      tx0 = KTx inputs outputs
      Right signed = runExcept $ signTx tx [skAlice]
  in assertBool "signed tx is different..." $
      signed /= tx
