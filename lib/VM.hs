module VM where

import Cbpv qualified as C
import Control.Monad (foldM)
import Data.Map qualified as M
import Data.Vector qualified as V

-- evaluation is straightforward as well
-- errors can no longer happen as we have already typechecked
-- and our type system is strongly-normalizing
-- we just need to implement the semantics of each instruction
eval :: C.Comp -> C.Stack -> IO C.Stack
-- push value onto the stack
eval (C.Push val) stack = eval val stack
-- push thunk (aka frozen computation) onto the stack
eval (C.Thunk c) (C.Stack {..}) =
  return (C.Stack (push (C.F c) operads) symbols)
-- remove thunk from the stack and evaluate it
eval (C.Force val) (C.Stack {..}) =
  case val of
    C.F c -> eval c (C.Stack operads symbols)
    _ -> error "not a frozen computation"
-- perform effects
eval (C.Do e) (C.Stack {..}) =
  case e of
    -- print value on top of the stack
    C.Output -> do
      print (V.unsafeHead operads)
      return (C.Stack (V.tail operads) symbols)
    -- any error that was compiled should be caught here
    C.Error msg -> error msg
-- evaluates all instructions collected by Seq
eval (C.Seq seqs) stack = do
  -- just like regular fold, but the accumulator can perform IO
  stack1 <- foldM (flip eval) stack seqs
  return (C.Stack (C.operads stack1) (C.symbols stack1))
-- because values aren't computations, they must be wrapped in a return statement
eval (C.Ret val) (C.Stack {..}) =
  return (C.Stack (push val operads) symbols)
-- variable retrieval from our collection of symbols
eval (C.Nam name@(C.Name {..})) stack@(C.Stack {..}) =
  case M.lookup variable symbols of
    Just (C.Var _ val) -> return $ C.Stack (push val operads) symbols
    Nothing -> error "variable not found"
-- operations pops two values from the stack and pushes the result back onto the stack
eval (C.Op o) (C.Stack {..}) =
  let val2 = V.unsafeHead operads
      op_1 = V.tail operads
      val1 = V.unsafeHead op_1
      op_2 = V.tail op_1
      ops = case o of
        C.Add -> push (performAdd val2 val1) op_2
        C.Sub -> push (performSub val2 val1) op_2
        C.Mul -> push (performMul val2 val1) op_2
        C.Div -> push (performDiv val2 val1) op_2
        C.Mod -> push (performMod val2 val1) op_2
        C.Eq -> push (performEq val2 val1) op_2
        C.Neq -> push (performNeq val2 val1) op_2
        C.Lt -> push (performLt val2 val1) op_2
        C.Gt -> push (performGt val2 val1) op_2
   in return (C.Stack ops symbols)
-- if-else statement
-- we only evaluate the condition and then evaluate the appropriate branch
eval (C.IfElse c1 c2 c3) stack = do
  stack1 <- eval c1 stack
  case V.unsafeHead (C.operads stack1) of
    C.B True -> eval c2 (C.Stack (V.tail $ C.operads stack1) (C.symbols stack1))
    C.B False -> eval c3 (C.Stack (V.tail $ C.operads stack1) (C.symbols stack1))
    _ -> error "not a boolean"
-- variable assignment moves the value from the top of the stack to the symbol map
eval (C.Let (C.Nam name@(C.Name {..})) c) stack@(C.Stack {..}) = do
  stack1 <- eval c stack
  let op = V.unsafeHead (C.operads stack1)
  return (C.Stack (V.tail (C.operads stack1)) (M.insert variable (C.Var name op) (C.symbols stack1)))
eval _ _ = error "not implemented"

-- helper functions
push :: a -> V.Vector a -> V.Vector a
push val operads = V.singleton val V.++ operads

-- the following functions only exist
-- because we haven't used Haskell to abstract over
-- the Cbpv.Val type
performAdd :: C.Val -> C.Val -> C.Val
performAdd (C.I val1) (C.I val2) = C.I (val1 + val2)
performAdd (C.D val1) (C.D val2) = C.D (val1 + val2)
performAdd _ _ = error "undefined operation"

performSub :: C.Val -> C.Val -> C.Val
performSub (C.I val1) (C.I val2) = C.I (val1 - val2)
performSub (C.D val1) (C.D val2) = C.D (val1 - val2)
performSub _ _ = error "undefined operation"

performMul :: C.Val -> C.Val -> C.Val
performMul (C.I val1) (C.I val2) = C.I (val1 * val2)
performMul (C.D val1) (C.D val2) = C.D (val1 * val2)
performMul _ _ = error "undefined operation"

performDiv :: C.Val -> C.Val -> C.Val
performDiv (C.D val1) (C.D val2) = C.D (val1 / val2)
performDiv _ _ = error "undefined operation"

performMod :: C.Val -> C.Val -> C.Val
performMod (C.I val1) (C.I val2) = C.I (val1 `mod` val2)
performMod _ _ = error "undefined operation"

performEq :: C.Val -> C.Val -> C.Val
performEq (C.I val1) (C.I val2) = C.B (val1 == val2)
performEq (C.D val1) (C.D val2) = C.B (val1 == val2)
performEq (C.B val1) (C.B val2) = C.B (val1 == val2)
performEq (C.S val1) (C.S val2) = C.B (val1 == val2)
performEq _ _ = error "undefined operation"

performNeq :: C.Val -> C.Val -> C.Val
performNeq val1 val2 =
  case performEq val1 val2 of
    C.B True -> C.B False
    C.B False -> C.B True
    _ -> error "not a boolean"

performLt :: C.Val -> C.Val -> C.Val
performLt (C.I val1) (C.I val2) = C.B (val1 < val2)
performLt (C.D val1) (C.D val2) = C.B (val1 < val2)
performLt _ _ = error "undefined operation"

performGt :: C.Val -> C.Val -> C.Val
performGt (C.I val1) (C.I val2) = C.B (val1 > val2)
performGt (C.D val1) (C.D val2) = C.B (val1 > val2)
performGt _ _ = error "undefined operation"
