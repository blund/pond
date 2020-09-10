module Pond.Backend
    ( compile
    , extract
    , execute
    ) where

import Prelude hiding (not, subtract)

import Pond.AST

-----------------------------------
-- | Back end
-----------------------------------

extract :: [(a, b)] -> a
extract ((a,b):xs) = a
extract _ = error "parse error"

execute :: Program -> String
execute p = runMain p
    where runMain (Program f) = case f_id f of
            (Id "main") -> getResult (f_st f)
            otherwise -> error "no function main defined"
          getResult (Return e) = show e

compile :: Program -> String
compile (Program f) = head ((\(Id id) -> id)(f_id f)) ++ body ++ "ret\n"
    where head name = ".globl _" ++ name ++ "\n\n_" ++ name ++ ":\n"
          expr = (\(Return e) -> e) $ f_st f
          body = evalExpr expr


movl :: Int -> String
movl v =  "\tmovl\t$" ++ show v ++ ", %eax\n"
       ++ "\n"

neg :: String
neg =  "\tneg\t\t%eax\n"
    ++ "\n"

not :: String
not = "\tnot\t%eax\n"
    ++ "\n"

compl :: String
compl =  "\tcmpl\t$0, %eax\n"    -- compare eax to 0
      ++ "\txor\t%eax, %eax\n"   -- zero out register
      ++ "\tsete\t%al\n"         -- iff ZF from comparison, set lower byte of eax
      ++ "\n"

add :: String -> String -> String
add e1 e2 =  e1
          ++ "\tpush\t%eax\n"
          ++ e2
          ++ "\tpop\t\t%ecx\n"
          ++ "\taddl\t%ecx, %eax\n"
          ++ "\n"

multiply :: String -> String -> String
multiply e1 e2 =  e1
          ++ "\tpush\t%eax\n"
          ++ e2
          ++ "\tpop\t\t%ecx\n"
          ++ "\timul\t%ecx, %eax\n"
          ++ "\n"

subtract :: String -> String -> String
subtract e1 e2 =  e2                -- merk! operander er byttet om
          ++ "\tpush\t%eax\n"
          ++ e1
          ++ "\tpop\t\t%ecx\n"
          ++ "\tsubl\t%ecx, %eax\n"
          ++ "\n"

divide :: String -> String -> String
divide e1 e2 =  e2              -- merk! operander er byttet om
          ++ "\tpush\t%eax\n"
          ++ "\tcdq\n"          -- sign extend eax inn i edx
          ++ e1
          ++ "\tpop\t\t%ecx\n"
          ++ "\tidivl\t%ecx, %eax\n"
          ++ "\n"

evalExpr :: Expr -> String
evalExpr (Const int) = movl int
evalExpr (UnOp op e) = (evalExpr e) ++ op'
    where op' = case op of
                    Negate -> neg
                    Not -> not
                    Complement -> compl
evalExpr (BinOp op e1 e2) = op' (evalExpr e1) (evalExpr e2)
    where op' = case op of
                    Add -> add
                    Subtract -> subtract
                    Multiply -> multiply
                    Divide -> divide
