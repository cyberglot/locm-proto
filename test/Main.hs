module Main (main) where

import CompilerSpec as Compiler
import Test.Tasty
import VMSpec as VM

tests :: TestTree
tests = testGroup "All Tests" [Compiler.tests, VM.tests]

main :: IO ()
main = defaultMain Main.tests
