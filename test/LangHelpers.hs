module LangHelpers where

import Lang

prog0 :: Lang
prog0 = Print (Add (Inte 1) (Inte 2))

prog1 :: Lang
prog1 =
  Seq
    [ Assign (Var "x") (Inte 1),
      Assign (Var "y") (Inte 2),
      Assign (Var "z") (Add (Var "x") (Var "y"))
    ]

prog2 :: Lang
prog2 =
  Seq
    [ Assign (Var "x") (Inte 1),
      Assign (Var "y") (Inte 2),
      Assign (Var "x") (Add (Var "x") (Var "y"))
    ]

prog3 :: Lang
prog3 =
  Seq
    [ Assign (Var "x") (Inte 1),
      Assign (Var "y") (Inte 2),
      Assign (Var "x") (Add (Var "x") (Var "y")),
      Assign (Var "x") (Inte 4)
    ]

prog4 :: Lang
prog4 =
  Seq
    [ Assign (Var "x") (Inte 1),
      Assign (Var "y") (Inte 2),
      Assign (Var "x") (Add (Var "x") (Var "y")),
      Assign (Var "x") (Inte 4),
      IfElse
        (Eq (Var "x") (Inte 4))
        (Assign (Var "x") (Inte 5))
        (Assign (Var "x") (Inte 6))
    ]
