module Compiler.Typechecker (typecheck) where

import Data.Map qualified as M
import Lang qualified as L

-- typecheck
-- if type inference succeeds, then typecheck succeeds
typecheck :: L.Lang -> Bool
typecheck lang =
  let gamma = mkGamma lang M.empty
   in case mkType lang gamma of
        L.TErr -> False
        _ -> True

-- simple type inference
-- we save the type of each variable in the environment
mkGamma :: L.Lang -> M.Map String L.Type -> M.Map String L.Type
mkGamma (L.Assign (L.Var v) (L.Var u)) m =
  if M.member u m
    then
      let t = M.findWithDefault L.TUnit u m
       in M.insert v t m
    else error "cannot assign non-existing variable"
mkGamma (L.Assign (L.Var v) e) m =
  let m1 = mkGamma e m
   in M.insert v (mkType e m1) m1
-- cases of interest over
-- now only need to traverse the tree
mkGamma (L.IfElse e1 e2 e3) m =
  let m1 = mkGamma e1 m
      m2 = mkGamma e2 m1
      m3 = mkGamma e3 m2
   in m3
mkGamma (L.Var _) m = m
mkGamma (L.Print e) m = mkGamma e m
mkGamma (L.Seq seqs) m = foldl (flip mkGamma) m seqs
mkGamma (L.Add e1 e2) m =
  let m1 = mkGamma e1 m
      m2 = mkGamma e2 m1
   in m2
mkGamma (L.Sub e1 e2) m =
  let m1 = mkGamma e1 m
      m2 = mkGamma e2 m1
   in m2
mkGamma (L.Mul e1 e2) m =
  let m1 = mkGamma e1 m
      m2 = mkGamma e2 m1
   in m2
mkGamma (L.Div e1 e2) m =
  let m1 = mkGamma e1 m
      m2 = mkGamma e2 m1
   in m2
mkGamma (L.Eq e1 e2) m =
  let m1 = mkGamma e1 m
      m2 = mkGamma e2 m1
   in m2
mkGamma (L.Neq e1) m = mkGamma e1 m
mkGamma (L.Lt e1 e2) m =
  let m1 = mkGamma e1 m
      m2 = mkGamma e2 m1
   in m2
mkGamma (L.Gt e1 e2) m =
  let m1 = mkGamma e1 m
      m2 = mkGamma e2 m1
   in m2
mkGamma _ m = m

emptyM :: M.Map String L.Type
emptyM = M.empty

-- get type based on values used
mkType :: L.Lang -> M.Map String L.Type -> L.Type
mkType (L.IfElse e1 e2 e3) m =
  let t1 = mkType e1 m
   in if t1 == L.TBoo
        then typeHelper e2 e3 m
        else L.TErr
mkType (L.Assign (L.Var _) e) m = mkType e m
mkType (L.Var v) m =
  if M.member v m
    then M.findWithDefault L.TUnit v m
    else L.TErr
mkType (L.Neq e1) m =
  let t1 = mkType e1 m
   in if t1 == L.TBoo
        then t1
        else L.TErr
-- cases of interest over
-- now only need to traverse the tree
mkType (L.Print e) m = mkType e m
mkType (L.Add e1 e2) m = typeHelper e1 e2 m
mkType (L.Sub e1 e2) m = typeHelper e1 e2 m
mkType (L.Mul e1 e2) m = typeHelper e1 e2 m
mkType (L.Div e1 e2) m = typeHelper e1 e2 m
mkType (L.Eq e1 e2) m = typeHelper e1 e2 m
mkType (L.Lt e1 e2) m = typeHelper e1 e2 m
mkType (L.Gt e1 e2) m = typeHelper e1 e2 m
mkType (L.Inte _) _ = L.TInt
mkType (L.Dou _) _ = L.TDou
mkType (L.Boo _) _ = L.TBoo
mkType (L.Str _) _ = L.TStr
mkType (L.Seq seqs) m =
  let t = map (flip mkType m) seqs
   in if all (== head t) t
        then head t
        else L.TErr
mkType _ _ = L.TUnit

-- helper function for matching types of two expressions
typeHelper :: L.Lang -> L.Lang -> M.Map String L.Type -> L.Type
typeHelper e1 e2 m =
  let t1 = mkType e1 m
      t2 = mkType e2 m
   in if t1 == t2
        then t1
        else L.TErr
