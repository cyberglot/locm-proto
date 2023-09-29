module CompilerSpec (tests) where

import CbpvHelpers
import qualified Compiler as C
import Lang
import LangHelpers
import Test.Tasty
import Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Compiler Tests" [rename, ssa, typechecker, compiler]

ssa :: TestTree
ssa =
  testGroup
    "SSA tests"
    [ testCase "no assignment" $
        C.ssa prog0 @?= prog0,
      testCase "no re-binding" $
        C.ssa prog1 @?= prog1,
      testCase "re-binding" $
        C.ssa prog2 @?= prog2_ssa,
      testCase "re-binding 2x" $
        C.ssa prog3 @?= prog3_ssa,
      testCase "re-binding under if" $
        C.ssa prog4 @?= prog4_ssa,
      testCase "ssa is idempotent" $
        C.ssa prog2_ssa @?= prog2_ssa
    ]

rename :: TestTree
rename =
  testGroup
    "Var Renaming tests"
    [ testCase "no assignment" $
        C.rename prog0 "x" "z" @?= prog0,
      testCase "with assignment, var doesn't exist" $
        C.rename prog1 "a" "b" @?= prog1,
      testCase "with assignment" $
        C.rename prog2 "x" "z" @?= prog2_renamed
    ]

typechecker :: TestTree
typechecker =
  testGroup
    "Unit tests"
    [ testCase "trivial" $
        C.typecheck prog0 @?= True,
      testCase "complex program" $
        C.typecheck prog1 @?= True,
      testCase "trivial -- fails" $
        C.typecheck noType0 @?= False,
      testCase "complex program -- fails" $
        C.typecheck noType1 @?= False
    ]

compiler :: TestTree
compiler =
  testGroup
    "Unit tests"
    [ testCase "trivial program" $
        C.compile prog0 @?= prog0_compiled,
      testCase "trivial program with vars" $
        C.compile prog1 @?= prog1_compiled,
      testCase "complex program" $
        C.compile prog2 @?= prog2_compiled,
      testCase "complex program 2" $
        C.compile prog3 @?= prog3_compiled,
      testCase "complex program 3" $
        C.compile prog4 @?= prog4_compiled
    ]

--- SSA results

prog2_ssa :: Lang
prog2_ssa =
  Seq
    [ Assign (Var "x") (Inte 1),
      Assign (Var "y") (Inte 2),
      Assign (Var "x_ssa_1") (Add (Var "x") (Var "y"))
    ]

prog3_ssa :: Lang
prog3_ssa =
  Seq
    [ Assign (Var "x") (Inte 1),
      Assign (Var "y") (Inte 2),
      Assign (Var "x_ssa_1") (Add (Var "x") (Var "y")),
      Assign (Var "x_ssa_2") (Inte 4)
    ]

prog4_ssa :: Lang
prog4_ssa =
  Seq
    [ Assign (Var "x") (Inte 1),
      Assign (Var "y") (Inte 2),
      Assign (Var "x_ssa_1") (Add (Var "x") (Var "y")),
      Assign (Var "x_ssa_2") (Inte 4),
      IfElse
        (Eq (Var "x_ssa_2") (Inte 4))
        (Assign (Var "x_ssa_3") (Inte 5))
        (Assign (Var "x_ssa_4") (Inte 6))
    ]

-- Rename results

prog2_renamed :: Lang
prog2_renamed =
  Seq
    [ Assign (Var "z") (Inte 1),
      Assign (Var "y") (Inte 2),
      Assign (Var "z") (Add (Var "z") (Var "y"))
    ]

-- Typechecker examples

noType0 :: Lang
noType0 = Add (Inte 1) (Boo True)

noType1 :: Lang
noType1 =
  Seq
    [ Assign (Var "x") (Inte 1),
      Assign (Var "y") (Dou 2.0),
      Assign (Var "z") (Add (Var "x") (Var "y"))
    ]
