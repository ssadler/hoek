{-# LANGUAGE OverloadedStrings #-}

module TestTransaction
  ( transactionTests
  ) where

import Data.Set as Set

import Network.Komodo.Transaction

import Test.Tasty
import Test.Tasty.HUnit

import TestSupport


transactionTests :: TestTree
transactionTests = testGroup "testUnits"
  [ testSignInput
  ]


testSignInput :: TestTree
testSignInput = testCase "sign an input" $
  assertEqual "input is equal"
    (TxInput outPoint0 $ CCInput ed2BobF) 
    (signInput [skBob] umsg $ TxInput outPoint0 $ CCInput ed2Bob)

