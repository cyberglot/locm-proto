locm-proto
===

locm-proto is a prototype of a simple imperative programming language, Lang, and its compiler to a stack-based VM.
It also includes a static single assignment optimization and a typechecker.

Lang comprises the  following abstract syntax:

```haskell
data Lang
  = Int Int
  | Double Double
  | Bool Bool
  | String String
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
```

locm-proto is written in simple Haskell and is accessible to any programmer familiar with pattern matching and recursion.
All recursive functions are explicit except when `fold` can use an already existing function either directly or by flipping its arguments.

running the project
---

```haskell
cabal build
cabal test
```