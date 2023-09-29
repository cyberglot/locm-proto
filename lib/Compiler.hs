module Compiler (compile, ssa, rename, typecheck) where

import Cbpv qualified
import Compiler.SSA
import Compiler.Typechecker
import Data.Map qualified as M
import Lang qualified as L

type Env = M.Map String Int

-- compilation to CBPV is straightforward
-- we just need to push values onto the stack
-- and guarantee that variables are in scope
compile :: L.Lang -> Cbpv.Runtime
compile lang =
  let ssaLang = ssa lang
      (_, symbols) = mkSymbols ssaLang (0, M.empty)
      (program, _, _) = go ssaLang 0 M.empty
   in Cbpv.Runtime (Cbpv.newStack symbols) program
  where
    go :: L.Lang -> Int -> Env -> (Cbpv.Comp, Int, Env)
    go (L.Inte val) i env = (Cbpv.Ret (Cbpv.I val), i, env)
    go (L.Dou val) i env = (Cbpv.Ret (Cbpv.D val), i, env)
    go (L.Boo val) i env = (Cbpv.Ret (Cbpv.B val), i, env)
    go (L.Str val) i env = (Cbpv.Ret (Cbpv.S val), i, env)
    go (L.IfElse e1 e2 e3) i env =
      let (e1_, i1, env1) = go e1 i env
          (e2_, i2, env2) = go e2 i1 env1
          (e3_, i3, env3) = go e3 i2 env2
       in (Cbpv.IfElse e1_ e2_ e3_, i3, env3)
    go (L.Assign (L.Var v) e) i env =
      let i1 = i + 1
          env1 = M.insert v i1 env
          (e1, i2, env2) = go e i1 env1
          n = Cbpv.Name v i1
       in (Cbpv.Let (Cbpv.Nam n) e1, i1, env2)
    go (L.Var v) i env =
      case M.lookup v env of
        Just i1 -> (Cbpv.Nam (Cbpv.Name v i1), i, env)
        Nothing -> (Cbpv.Do (Cbpv.Error ("variable not found" ++ show v)), i, env)
    go (L.Print e) i env =
      let (val, i1, env1) = go e i env
       in (Cbpv.Seq [Cbpv.Push val, Cbpv.Do Cbpv.Output], i1, env1)
    go (L.Seq seqs) i env =
      let (seqs_, i1, env1) = goSeq seqs i env
       in (Cbpv.Seq seqs_, i1, env1)
    go (L.Add e1 e2) i env =
      let (val1, i1, env1) = go e1 i env
          (val2, i2, env2) = go e2 i1 env1
       in (Cbpv.Seq [Cbpv.Push val1, Cbpv.Push val2, Cbpv.Op Cbpv.Add], i2, env2)
    go (L.Sub e1 e2) i env =
      let (val1, i1, env1) = go e1 i env
          (val2, i2, env2) = go e2 i1 env1
       in (Cbpv.Seq [Cbpv.Push val1, Cbpv.Push val2, Cbpv.Op Cbpv.Sub], i2, env2)
    go (L.Mul e1 e2) i env =
      let (val1, i1, env1) = go e1 i env
          (val2, i2, env2) = go e2 i1 env1
       in (Cbpv.Seq [Cbpv.Push val1, Cbpv.Push val2, Cbpv.Op Cbpv.Mul], i2, env2)
    go (L.Div e1 e2) i env =
      let (val1, i1, env1) = go e1 i env
          (val2, i2, env2) = go e2 i1 env1
       in (Cbpv.Seq [Cbpv.Push val1, Cbpv.Push val2, Cbpv.Op Cbpv.Div], i2, env2)
    go (L.Eq e1 e2) i env =
      let (val1, i1, env1) = go e1 i env
          (val2, i2, env2) = go e2 i1 env1
       in (Cbpv.Seq [Cbpv.Push val1, Cbpv.Push val2, Cbpv.Op Cbpv.Eq], i2, env2)
    go (L.Neq e1) i env =
      let (val1, i1, env1) = go e1 i env
       in (Cbpv.Seq [Cbpv.Push val1, Cbpv.Op Cbpv.Neq], i1, env1)
    go (L.Lt e1 e2) i env =
      let (val1, i1, env1) = go e1 i env
          (val2, i2, env2) = go e2 i1 env1
       in (Cbpv.Seq [Cbpv.Push val1, Cbpv.Push val2, Cbpv.Op Cbpv.Lt], i2, env2)
    go (L.Gt e1 e2) i env =
      let (val1, i1, env1) = go e1 i env
          (val2, i2, env2) = go e2 i1 env1
       in (Cbpv.Seq [Cbpv.Push val1, Cbpv.Push val2, Cbpv.Op Cbpv.Gt], i2, env2)

    -- explicitly fold seqs
    goSeq :: [L.Lang] -> Int -> Env -> ([Cbpv.Comp], Int, Env)
    goSeq [] i env = ([], i, env)
    goSeq (x : xs) i env =
      let (x_, i1, env1) = go x i env
          (xs_, i2, env2) = goSeq xs i1 env1
       in (x_ : xs_, i2, env2)

-- we need to keep track of constants
-- remember: after SSA, all variables are bound only once
mkSymbols :: L.Lang -> (Int, Cbpv.Symbol) -> (Int, Cbpv.Symbol)
mkSymbols (L.Assign (L.Var v) (L.Inte val)) (i, m) =
  let i1 = i + 1
      name = Cbpv.Name v i1
      var = Cbpv.Var name (Cbpv.I val)
   in (i1, M.insert i1 var m)
mkSymbols (L.Assign (L.Var v) (L.Dou val)) (i, m) =
  let i1 = i + 1
      name = Cbpv.Name v i1
      var = Cbpv.Var name (Cbpv.D val)
   in (i1, M.insert i1 var m)
mkSymbols (L.Assign (L.Var v) (L.Boo val)) (i, m) =
  let i1 = i + 1
      name = Cbpv.Name v i1
      var = Cbpv.Var name (Cbpv.B val)
   in (i1, M.insert i1 var m)
mkSymbols (L.Assign (L.Var v) (L.Str val)) (i, m) =
  let i1 = i + 1
      name = Cbpv.Name v i1
      var = Cbpv.Var name (Cbpv.S val)
   in (i1, M.insert i1 var m)
-- cases of interest over
-- now only need to traverse the tree
mkSymbols (L.Assign var e) (i, m) =
  let (i1, m1) = mkSymbols e (i, m)
   in (i1, m1)
mkSymbols (L.IfElse e1 e2 e3) (i, m) =
  let (i1, m1) = mkSymbols e1 (i, m)
      (i2, m2) = mkSymbols e2 (i1, m1)
      (i3, m3) = mkSymbols e3 (i2, m2)
   in (i3, m3)
mkSymbols (L.Seq seqs) (i, m) =
  goSeqs seqs (i, m)
  where
    -- explicitly fold seqs
    goSeqs :: [L.Lang] -> (Int, Cbpv.Symbol) -> (Int, Cbpv.Symbol)
    goSeqs [] m = m
    goSeqs (x : xs) (i, m) =
      let (i1, m1) = mkSymbols x (i, m)
       in goSeqs xs (i1, m1)
mkSymbols (L.Var _) m = m
mkSymbols (L.Print e) (i, m) = mkSymbols e (i, m)
mkSymbols (L.Add e1 e2) (i, m) =
  let (i1, m1) = mkSymbols e1 (i, m)
      (i2, m2) = mkSymbols e2 (i1, m1)
   in (i2, m2)
mkSymbols (L.Sub e1 e2) (i, m) =
  let (i1, m1) = mkSymbols e1 (i, m)
      (i2, m2) = mkSymbols e2 (i1, m1)
   in (i2, m2)
mkSymbols (L.Mul e1 e2) (i, m) =
  let (i1, m1) = mkSymbols e1 (i, m)
      (i2, m2) = mkSymbols e2 (i1, m1)
   in (i2, m2)
mkSymbols (L.Div e1 e2) (i, m) =
  let (i1, m1) = mkSymbols e1 (i, m)
      (i2, m2) = mkSymbols e2 (i1, m1)
   in (i2, m2)
mkSymbols (L.Mod e1 e2) (i, m) =
  let (i1, m1) = mkSymbols e1 (i, m)
      (i2, m2) = mkSymbols e2 (i1, m1)
   in (i2, m2)
mkSymbols (L.Eq e1 e2) (i, m) =
  let (i1, m1) = mkSymbols e1 (i, m)
      (i2, m2) = mkSymbols e2 (i1, m1)
   in (i2, m2)
mkSymbols (L.Neq e1) (i, m) = mkSymbols e1 (i, m)
mkSymbols (L.Lt e1 e2) (i, m) =
  let (i1, m1) = mkSymbols e1 (i, m)
      (i2, m2) = mkSymbols e2 (i1, m1)
   in (i2, m2)
mkSymbols (L.Gt e1 e2) (i, m) =
  let (i1, m1) = mkSymbols e1 (i, m)
      (i2, m2) = mkSymbols e2 (i1, m1)
   in (i2, m2)
mkSymbols _ m = m
