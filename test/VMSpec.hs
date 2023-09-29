module VMSpec where

import Cbpv
import Test.Tasty
import Test.Tasty.HUnit
import VM
import CbpvHelpers

tests :: TestTree
tests =
  testGroup
    "VM tests"
    [ testCase "example program 1" $ do
        p0 <- eval (code prog0_compiled) (stack prog0_compiled)
        p0 @?= prog0_eval,
      testCase "example program 2" $ do
        p1 <- eval (code prog1_compiled) (stack prog1_compiled)
        p1 @?= prog1_eval,
      testCase "example program 3" $ do
        p2 <- eval (code prog2_compiled) (stack prog2_compiled)
        p2 @?= prog2_eval,
      testCase "example program 4" $ do
        p3 <- eval (code prog3_compiled) (stack prog3_compiled)
        p3 @?= prog3_eval,
      testCase "example program 5" $ do
        p4 <- eval (code prog4_compiled) (stack prog4_compiled)
        p4 @?= prog4_eval
    ]
