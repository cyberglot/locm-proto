module Compiler.SSA where

import Data.Map qualified as M
import Lang qualified as L

-- all variables are now
-- single assignment
ssa :: L.Lang -> L.Lang
ssa lang = fst $ go lang M.empty
  where
    -- new variable suffixed with _ssa_i
    fresh :: String -> Int -> String
    fresh v i = v ++ "_ssa_" ++ show i

    go :: L.Lang -> M.Map String Int -> (L.Lang, M.Map String Int)
    go (L.Assign (L.Var v) e) m =
      if M.member v m
        then
          let (Just counter) = M.lookup v m
              counter1 = counter + 1
              newName = fresh v counter1
              (e2, m2) = go e m
           in (L.Assign (L.Var newName) e2, M.insert v counter1 m2)
        else
          let (e_, m_) = go e m
           in (L.Assign (L.Var v) e_, M.insert v 0 m_)
    go (L.Var v) m =
      case M.lookup v m of
        Just counter ->
          if counter == 0
            then (L.Var v, m)
            else (L.Var $ fresh v counter, m)
        Nothing -> error "variable not found"
    -- cases of interest over
    -- now only need to traverse the tree
    go (L.IfElse e1 e2 e3) m =
      let (e1_, m1) = go e1 m
          (e2_, m2) = go e2 m1
          (e3_, m3) = go e3 m2
       in (L.IfElse e1_ e2_ e3_, m3)
    go (L.Seq seqs) m =
      let (seqs_, m1) = goSeqs seqs m
       in (L.Seq seqs_, m1)
    go (L.Print e) m =
      let (e_, m_) = go e m
       in (L.Print e_, m_)
    go (L.Add e1 e2) m =
      let (e1_, m1) = go e1 m
          (e2_, m2) = go e2 m1
       in (L.Add e1_ e2_, m2)
    go (L.Sub e1 e2) m =
      let (e1_, m1) = go e1 m
          (e2_, m2) = go e2 m1
       in (L.Sub e1_ e2_, m2)
    go (L.Mul e1 e2) m =
      let (e1_, m1) = go e1 m
          (e2_, m2) = go e2 m1
       in (L.Mul e1_ e2_, m2)
    go (L.Div e1 e2) m =
      let (e1_, m1) = go e1 m
          (e2_, m2) = go e2 m1
       in (L.Div e1_ e2_, m2)
    go (L.Eq e1 e2) m =
      let (e1_, m1) = go e1 m
          (e2_, m2) = go e2 m1
       in (L.Eq e1_ e2_, m2)
    go (L.Neq e1) m =
      let (e1_, m1) = go e1 m
       in (L.Neq e1_, m1)
    go (L.Lt e1 e2) m =
      let (e1_, m1) = go e1 m
          (e2_, m2) = go e2 m1
       in (L.Lt e1_ e2_, m2)
    go (L.Gt e1 e2) m =
      let (e1_, m1) = go e1 m
          (e2_, m2) = go e2 m1
       in (L.Gt e1_ e2_, m2)
    go lang m = (lang, m)

    -- explicitly fold the sequence
    goSeqs :: [L.Lang] -> M.Map String Int -> ([L.Lang], M.Map String Int)
    goSeqs [] m = ([], m)
    goSeqs (x : xs) m =
      let (x_, m1) = go x m
          (xs_, m2) = goSeqs xs m1
       in (x_ : xs_, m2)

-- rename variables under ssa
-- to avoid rebinding/mutation
-- caveat: renaming is not capture-avoiding
rename :: L.Lang -> String -> String -> L.Lang
rename (L.IfElse e1 e2 e3) x y =
  L.IfElse (rename e1 x y) (rename e2 x y) (rename e3 x y)
rename (L.Assign (L.Var v) e) x y = L.Assign (rename (L.Var v) x y) (rename e x y)
rename (L.Var v) x y = if v == x then L.Var y else L.Var v
-- cases of interest over
-- now only need to traverse the tree
rename (L.Print e) x y = L.Print (rename e x y)
rename (L.Seq seqs) x y = L.Seq (map (\e -> rename e x y) seqs)
rename (L.Add e1 e2) x y = L.Add (rename e1 x y) (rename e2 x y)
rename (L.Sub e1 e2) x y = L.Sub (rename e1 x y) (rename e2 x y)
rename (L.Mul e1 e2) x y = L.Mul (rename e1 x y) (rename e2 x y)
rename (L.Div e1 e2) x y = L.Div (rename e1 x y) (rename e2 x y)
rename (L.Eq e1 e2) x y = L.Eq (rename e1 x y) (rename e2 x y)
rename (L.Neq e1) x y = L.Neq (rename e1 x y)
rename (L.Lt e1 e2) x y = L.Lt (rename e1 x y) (rename e2 x y)
rename (L.Gt e1 e2) x y = L.Gt (rename e1 x y) (rename e2 x y)
rename lang _ _ = lang
