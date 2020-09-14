module Pond.Backend
    ( compile
    , execute
    ) where

import Prelude hiding (not, subtract)

import Pond.AST

-----------------------------------
-- | Back end
-----------------------------------


execute :: Program -> String
execute p = runMain p
    where runMain (Program f) = case f_id f of
            (Id "main") -> getResult (f_st f)
            otherwise -> error "no function main defined"
          getResult (Return e) = show e

compile :: Program -> String
compile (Program f) = head ((\(Id id) -> id)(f_id f)) ++ body ++ print ++ "ret\n"
    where head name = ".globl " ++ name ++ "\n\n" ++ name ++ ":\n"
          expr = (\(Return e) -> e) $ f_st f
          print = "\tmovq\t%rax, %rdi\n"++"\tcall print\n"
          body = evalExpr expr


movl :: Int -> String
movl v =  "\tmov\t$" ++ show v ++ ", %rax\n"
       ++ "\n"

neg :: String
neg =  "\tneg\t\t%rax\n"
    ++ "\n"

not :: String
not =    "\tcmp\t$0, %rax\n"    -- compare eax to 0
      ++ "\txor\t$0, %rax\n"   -- zero out register
      ++ "\tsete\t%al\n"         -- iff ZF from comparison, set lower byte of eax
      ++ "\n"
compl :: String
compl =  "\tcmp\t$0, %rax\n"    -- compare eax to 0
      ++ "\txor\t%rax, %rax\n"   -- zero out register
      ++ "\tsete\t%al\n"         -- iff ZF from comparison, set lower byte of eax
      ++ "\n"

add :: String -> String -> String
add e1 e2 =  e1
          ++ "\tpush\t%rax\n"
          ++ e2
          ++ "\tpop\t\t%rcx\n"
          ++ "\tadd\t%rcx, %rax\n"
          ++ "\n"

multiply :: String -> String -> String
multiply e1 e2 =  e1
          ++ "\tpush\t%rax\n"
          ++ e2
          ++ "\tpop\t\t%rcx\n"
          ++ "\timul\t%rcx, %rax\n"
          ++ "\n"

subtract :: String -> String -> String
subtract e1 e2 =  e2                -- merk! operander er byttet om
          ++ "\tpush\t%rax\n"
          ++ e1
          ++ "\tpop\t\t%rcx\n"
          ++ "\tsub\t%rcx, %rax\n"
          ++ "\n"

divide :: String -> String -> String
divide e1 e2 =  e2              -- merk! operander er byttet om
          ++ "\tpush\t%rax\n"
          ++ "\tcqo\n"          -- sign extend eax inn i edx
          ++ e1
          ++ "\tpop\t\t%rcx\n"
          ++ "\tdiv\t%rcx, %rax\n"
          ++ "\n"

equal :: String -> String -> String
equal e1 e2 = e1
          ++ "\tpush\t%rax\n"
          ++ e2
          ++ "\tpop\t%rcx\n"          -- sign extend eax inn i edx
          ++ "\tcmp\t%rax, %rcx\n"          -- sign extend eax inn i edx
          ++ "\txor\t%rax, %rax\n"          -- sign extend eax inn i edx
          ++ "\tsete\t%al\n"



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
                    Equal -> equal
