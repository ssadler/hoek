{-# LANGUAGE OverloadedStrings #-}

import Test.Tasty
import Test.Tasty.HUnit

import TestTransaction


main :: IO ()
main = defaultMain $ testGroup "Tests" [ transactionTests
                                       ]

