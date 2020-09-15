module Pond.Backend
    ( compile
    , execute
    ) where

import Prelude hiding (not, subtract, or, and)
import Control.Monad.State
import Data.String
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
          print = makeAsm ["movq\t%rax, %rdi", "call\tprint"]
          body = evalState (evalExpr expr) 0


movl :: Int -> String
movl v =  makeAsm
    [ "mov\t$" ++ show v ++ ", %rax"
    ]

neg :: String
neg = makeAsm
    [ "neg  %rax"
    ]

not :: String
not = makeAsm
    [ "cmp  $0, %rax"    -- compare eax to 0
    , "xor  $0, %rax"   -- zero out register
    , "sete %al"         -- iff ZF from comparison, set lower byte of eax
    , ""
    ]

compl :: String
compl = makeAsm
    [ "cmp  $0, %rax"    -- compare eax to 0
    , "xor  %rax, %rax"   -- zero out register
    , "sete %al"         -- iff ZF from comparison, set lower byte of eax
    ]

add :: String -> String -> String
add e1 e2 = makeAsm
    [ e1
    , "push\t%rax"
    , e2
    , "pop\t\t%rcx"
    , "add\t%rcx, %rax"
    ]

multiply :: String -> String -> String
multiply e1 e2 = makeAsm
    [ e1
    , "push\t%rax"
    , e2
    , "pop\t\t%rcx"
    , "imul\t%rcx, %rax"
    ]

subtract :: String -> String -> String
subtract e1 e2 =  makeAsm
    [ e2
    , "push\t%rax"
    , e1
    , "pop\t\t%rcx"
    , "sub\t%rcx, %rax"
    ]

divide :: String -> String -> String
divide e1 e2 = makeAsm
    [ e2              -- merk! operander er byttet om
    , "push\t%rax"
    , "cqo"          -- sign extend eax inn i edx
    , e1
    , "pop\t\t%rcx"
    , "div\t%rcx, %rax"
    ]

equal :: String -> String -> String
equal e1 e2 = makeAsm
    [ e1
    , "push\t%rax"
    , e2
    , "pop\t%rcx"          -- sign extend eax inn i edx
    , "cmp\t%rax, %rcx"          -- sign extend eax inn i edx
    , "mov\t$0, %rax"          -- sign extend eax inn i edx
    , "sete\t%al"
    ]

or :: Int -> String -> String -> String
or count e1 e2 = makeAsm
    [ e1
    , "cmpl\t$0, %eax"               -- check if e1 is true
    , "je\t\t_clause" ++ n    -- e1 is 0, so we need to evaluate clause 2
    , "movl\t$1, %eax"                   -- we didn't jump, so e1 is true and therefore result is 1
    , "jmp\t\t_end" ++ n
    , ""
    , "_clause" ++ n ++ ":"
    , e2
    , "cmpl\t$0, %eax"            -- check if e2 is true
    , "movl\t$0, %eax"            -- zero out EAX without changing ZF
    , "setne\t%al"                -- set AL register (the low byte of EAX) to 1 iff e2 != 0
    , ""
    , "_end" ++ n ++ ":"
    ]
  where n = show count

and :: Int -> String -> String -> String
and count e1 e2 = makeAsm
    [ e1
    , "cmpl\t$0, %eax"          -- check if e1 is true
    , "jne\t\t_clause" ++ n     -- e1 isn't 0, so we need to evaluate clause 2
    , "jmp\t\t_end" ++ n
    , "_clause" ++ n ++ ":"
    , e2
    , "cmpl $0, %eax"           -- check if e2 is true
    , "movl $0, %eax"           -- zero out EAX without changing ZF
    , "setne %al"               -- set AL register (the low byte of EAX) to 1 iff e2 != 0
    , "_end" ++ n ++ ":"
    ]
  where n = show count

makeAsm :: [String] -> String
makeAsm xs = unlines $ (++ [""]) xs



type Count = Int
evalExpr :: Expr -> State Count String
evalExpr (Const int) = return $ movl int

evalExpr (UnOp op e) = do
        let op' = getUn op
        e' <- evalExpr e
        return $ e' ++ op'

evalExpr (BinOp op e1 e2) = do
        let op' = getBin op
        count <- get
        modify (+1)
        e1' <- evalExpr e1
        e2' <- evalExpr e2
        return $ (op' count) e1' e2'


getUn Negate        = neg
getUn Not           = not
getUn Complement    = compl

getBin Add      = (const add)
getBin Subtract = (const subtract)
getBin Multiply = (const multiply)
getBin Divide   = (const divide)
getBin Equal    = (const equal)
getBin Or       = (\n -> or n)
getBin And       = (\n -> and n)
