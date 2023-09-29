module CbpvHelpers where

import Cbpv
import qualified Data.Map as M
import qualified Data.Vector as V

prog0_compiled :: Runtime
prog0_compiled = Runtime { stack = Stack { operads = V.empty, symbols = M.empty}, code = Seq [ Push ( Seq [ Push (Ret (I 1)), Push (Ret (I 2)), Op Add]), Do Output]}

prog1_compiled :: Runtime
prog1_compiled = Runtime { stack = Stack { operads = V.empty, symbols = M.fromList [ ( 1, Var {name = Name {original = "x", variable = 1}, val = I 1}), (2, Var {name = Name {original = "y", variable = 2}, val = I 2})]}, code = Seq [Let (Nam (Name {original = "x", variable = 1})) (Ret (I 1)), Let (Nam (Name {original = "y", variable = 2})) (Ret (I 2)), Let (Nam (Name {original = "z", variable = 3})) (Seq [Push (Nam (Name {original = "x", variable = 1})), Push (Nam (Name {original = "y", variable = 2})), Op Add])]}

prog2_compiled :: Runtime
prog2_compiled = Runtime { stack = Stack { operads = V.empty, symbols = M.fromList [ ( 1, Var {name = Name {original = "x", variable = 1}, val = I 1}), (2, Var {name = Name {original = "y", variable = 2}, val = I 2})]}, code = Seq [Let (Nam (Name {original = "x", variable = 1})) (Ret (I 1)), Let (Nam (Name {original = "y", variable = 2})) (Ret (I 2)), Let (Nam (Name {original = "x_ssa_1", variable = 3})) (Seq [Push (Nam (Name {original = "x", variable = 1})), Push (Nam (Name {original = "y", variable = 2})), Op Add])]}

prog3_compiled :: Runtime
prog3_compiled = Runtime { stack = Stack { operads = V.empty, symbols = M.fromList [ ( 1, Var {name = Name {original = "x", variable = 1}, val = I 1}), (2, Var {name = Name {original = "y", variable = 2}, val = I 2}), (3, Var {name = Name {original = "x_ssa_2", variable = 3}, val = I 4})]}, code = Seq [Let (Nam (Name {original = "x", variable = 1})) (Ret (I 1)), Let (Nam (Name {original = "y", variable = 2})) (Ret (I 2)), Let (Nam (Name {original = "x_ssa_1", variable = 3})) (Seq [Push (Nam (Name {original = "x", variable = 1})), Push (Nam (Name {original = "y", variable = 2})), Op Add]), Let (Nam (Name {original = "x_ssa_2", variable = 4})) (Ret (I 4))]}

prog4_compiled :: Runtime
prog4_compiled = Runtime { stack = Stack { operads = V.empty, symbols = M.fromList [ ( 1 , Var {name = Name {original = "x", variable = 1}, val = I 1}), (2, Var {name = Name {original = "y", variable = 2}, val = I 2}), (3, Var {name = Name {original = "x_ssa_2", variable = 3}, val = I 4}), (4, Var {name = Name {original = "x_ssa_3", variable = 4}, val = I 5}), (5, Var {name = Name {original = "x_ssa_4", variable = 5}, val = I 6})]}, code = Seq [Let (Nam (Name {original = "x", variable = 1})) (Ret (I 1)), Let (Nam (Name {original = "y", variable = 2})) (Ret (I 2)), Let (Nam (Name {original = "x_ssa_1", variable = 3})) (Seq [Push (Nam (Name {original = "x", variable = 1})), Push (Nam (Name {original = "y", variable = 2})), Op Add]), Let (Nam (Name {original = "x_ssa_2", variable = 4})) (Ret (I 4)), IfElse (Seq [Push (Nam (Name {original = "x_ssa_2", variable = 4})), Push (Ret (I 4)), Op Eq]) (Let (Nam (Name {original = "x_ssa_3", variable = 5})) (Ret (I 5))) (Let (Nam (Name {original = "x_ssa_4", variable = 6})) (Ret (I 6)))]}

prog0_eval :: Stack
prog0_eval = Stack {operads = V.empty, symbols = M.empty}

prog1_eval :: Stack
prog1_eval = Stack {operads = V.empty, symbols = M.fromList [(1,Var {name = Name {original = "x", variable = 1}, val = I 1}),(2,Var {name = Name {original = "y", variable = 2}, val = I 2}),(3,Var {name = Name {original = "z", variable = 3}, val = I 3})]}

prog2_eval :: Stack
prog2_eval = Stack {operads = V.empty, symbols = M.fromList [(1,Var {name = Name {original = "x", variable = 1}, val = I 1}),(2,Var {name = Name {original = "y", variable = 2}, val = I 2}),(3,Var {name = Name {original = "x_ssa_1", variable = 3}, val = I 3})]}

prog3_eval :: Stack
prog3_eval = Stack {operads = V.empty, symbols = M.fromList [(1,Var {name = Name {original = "x", variable = 1}, val = I 1}),(2,Var {name = Name {original = "y", variable = 2}, val = I 2}),(3,Var {name = Name {original = "x_ssa_1", variable = 3}, val = I 3}),(4,Var {name = Name {original = "x_ssa_2", variable = 4}, val = I 4})]}

prog4_eval :: Stack
prog4_eval = Stack {operads = V.empty, symbols = M.fromList [(1,Var {name = Name {original = "x", variable = 1}, val = I 1}),(2,Var {name = Name {original = "y", variable = 2}, val = I 2}),(3,Var {name = Name {original = "x_ssa_1", variable = 3}, val = I 3}),(4,Var {name = Name {original = "x_ssa_2", variable = 4}, val = I 4}),(5,Var {name = Name {original = "x_ssa_3", variable = 5}, val = I 5})]}