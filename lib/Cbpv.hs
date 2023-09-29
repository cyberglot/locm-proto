module Cbpv where

import Data.Map qualified as M
import Data.Vector qualified as V

-- This is a poor's man implementation of CBPV as a stack-based bytecode VM.
-- Slogan: a value is, a computation does.

-- Values are literals and thunks.
data Val
  = I Int
  | D Double
  | B Bool
  | S String
  | F Comp -- frozen computation
  | U -- empty value
  deriving (Eq, Show)

-- Computations are instructions.
data Comp
  = Push Comp -- variable
  | Thunk Comp -- transform a computation into a frozen computation
  | Force Val -- force a frozen computation
  | Do Eff -- perform an effect
  | Seq [Comp] -- sequence computations
  | Ret Val -- transform a value into a computation
  | Op Op -- arithmetic operations & total order checkers
  | Nam Name
  | IfElse Comp Comp Comp
  | Let Comp Comp
  deriving (Eq, Show)

data Op
  = Add
  | Sub
  | Mul
  | Div
  | Mod
  | Eq
  | Neq
  | Lt
  | Gt
  deriving (Eq, Show)

-- Effects are printing and erroring.
-- For simplicity, errors just contain a Sing.
-- In a real implementation, they would contain proper stack traces.
data Eff
  = Output
  | Error String
  deriving (Eq, Show)

-- Not sure if we'd need this in CBPV.
data Var = Var
  { name :: Name,
    val :: Val
  }
  deriving (Eq, Show)

data Name = Name
  { original :: String,
    variable :: Int
  }
  deriving (Eq, Show)

data Stack = Stack
  { operads :: V.Vector Val,
    symbols :: Symbol
  }
  deriving (Eq, Show)

newStack :: Symbol -> Stack
newStack symbols =
  Stack
    { operads = V.empty,
      symbols = symbols
    }

type Symbol = M.Map Int Var

data Runtime = Runtime
  { stack :: Stack,
    code :: Comp
  }
  deriving (Eq, Show)
