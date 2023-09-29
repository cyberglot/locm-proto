module Lang where

data Lang
  = Inte Int
  | Dou Double
  | Boo Bool
  | Str String
  | IfElse Lang Lang Lang
  | Assign Lang Lang
  | Var String
  | Print Lang
  | Skip
  | Seq [Lang]
  | Add Lang Lang
  | Sub Lang Lang
  | Mul Lang Lang
  | Div Lang Lang
  | Mod Lang Lang
  | Eq Lang Lang
  | Neq Lang
  | Lt Lang Lang
  | Gt Lang Lang
  deriving (Eq, Show)

data Type
  = TInt
  | TDou
  | TBoo
  | TStr
  | TUnit
  | TErr
  deriving (Eq, Show)
