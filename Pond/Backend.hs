-- | https://cloudnative.ly/haskell-mapping-with-state-7a07e3c2cbf9

module Pond.Backend
    ( compile
    ) where

import Prelude hiding (not, subtract, or, and, lookup)

import Control.Monad.State

import Data.String
import Data.Map hiding (map)


import Pond.AST

-----------------------------------
-- | Back end
-----------------------------------

compile :: Program -> String
compile (Program f) = head (f_name f) ++ prologue ++ body ++ print ++ epilogue
    where head name = ".globl " ++ name ++ "\n\n" ++ name ++ ":\n"
          block_items = f_st f
          print = makeAsm ["movq\t%rax, %rdi", "call\tprint"]
          -- Her mi ha en funksjon som evaluerer statements, ikke bare
          -- Exprs...
          body  = unlines $ evalState (mapM compileBlockItem block_items) initialState


prologue = makeAsm [ "push %rbp"       -- save old value
                   , "mov %rsp, %rbp" -- current top of stack is bottom of new stack frame
                   ]
           
epilogue = makeAsm [ "mov %rbp, %rsp" -- restore ESP; now it points to old EBP
                   , "pop %rbp"        -- restore old EBP"   now ESP is where it was before prologue
                   , "ret"
                   ]


compileBlockItem :: BlockItem -> State CompilerState String
compileBlockItem (Statement s) = compileStatement s
compileBlockItem (Declaration d) = compileDeclaration d

compileDeclaration :: Declare -> State CompilerState String
compileDeclaration (Declare id me) = do
         index <- gets stack_index
         modify (\s@CompilerState {stack_map=m} -> s {stack_map = insert id index m})
         modify (\s@CompilerState {stack_index=i} -> s {stack_index =i-8})
         case me of
           Nothing -> return "push\t%rax\n"
           Just e  -> do
             expr' <- evalExpr e
             return $ expr' ++ "push\t%rax\n" 
                 
compileStatement :: Statement -> State CompilerState String
compileStatement (Return e) = evalExpr e
compileStatement (Expression e) = evalExpr e
compileStatement c@(Condition _ _ _) = compileCondition c

compileCondition :: Statement -> State CompilerState String
compileCondition (Condition e s ms) = do
    e1 <- evalExpr e
    e2 <- compileStatement s
    count <- gets counter
    modify (\s@CompilerState {counter=c} -> s {counter = c+1})
    case ms of
      Nothing -> do
        return $ makeAsm [ e1
                         , "cmp $0, %rax"
                         , "je _post_cond" ++ show count
                         , e2
                         , "_post_cond" ++ show count ++ ":"
                         ]
      Just s2  -> do
        e3 <- compileStatement s2
        return $ makeAsm [ e1
                         , "cmp $0, %rax"
                         , "je _e3" ++ show count
                         , e2
                         , "jmp _post_cond" ++ show count
                         , "_e3" ++ show count ++ ":"
                         , e3
                         , "_post_cond" ++ show count ++ ":"
                         ]
        


mov :: Int -> String
mov v =  makeAsm
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
    , e1
    , "cqo" -- sign extend rax
    , "pop\t\t%rcx"
    , "idiv\t%rcx"
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
    
notEqual :: String -> String -> String
notEqual e1 e2 = makeAsm
    [ e1
    , "push\t%rax"
    , e2
    , "pop\t%rcx"          -- sign extend eax inn i edx
    , "cmp\t%rax, %rcx"          -- sign extend eax inn i edx
    , "mov\t$0, %rax"          -- sign extend eax inn i edx
    , "setne\t%al"
    ]

or :: Int -> String -> String -> String
or count e1 e2 = makeAsm
    [ e1
    , "cmp\t$0, %rax"               -- check if e1 is true
    , "je\t\t_clause" ++ n    -- e1 is 0, so we need to evaluate clause 2
    , "mov\t$1, %rax"                   -- we didn't jump, so e1 is true and therefore result is 1
    , "jmp\t\t_end" ++ n
    , ""
    , "_clause" ++ n ++ ":"
    , e2
    , "cmp\t$0, %rax"            -- check if e2 is true
    , "mov\t$0, %rax"            -- zero out EAX without changing ZF
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
    , "cmp $0, %rax"           -- check if e2 is true
    , "mov $0, %rax"           -- zero out EAX without changing ZF
    , "setne %al"               -- set AL register (the low byte of EAX) to 1 iff e2 != 0
    , "_end" ++ n ++ ":"
    ]
  where n = show count

lessThan :: String -> String -> String
lessThan e1 e2 = makeAsm
    [ e1
    , "push\t%rax"
    , e2
    , "pop\t%rcx"          -- sign extend eax inn i edx
    , "cmp\t%rax, %rcx"          -- sign extend eax inn i edx
    , "mov\t$0, %rax"          -- sign extend eax inn i edx
    , "setl\t%al"
    ]

lessEqual :: String -> String -> String
lessEqual e1 e2 = makeAsm
    [ e1
    , "push\t%rax"
    , e2
    , "pop\t%rcx"          -- sign extend eax inn i edx
    , "cmp\t%rax, %rcx"          -- sign extend eax inn i edx
    , "mov\t$0, %rax"          -- sign extend eax inn i edx
    , "setle\t%al"
    ]

greaterThan :: String -> String -> String
greaterThan e1 e2 = makeAsm
    [ e1
    , "push\t%rax"
    , e2
    , "pop\t%rcx"          -- sign extend eax inn i edx
    , "cmp\t%rax, %rcx"          -- sign extend eax inn i edx
    , "mov\t$0, %rax"          -- sign extend eax inn i edx
    , "setg\t%al"
    ]

greaterEqual :: String -> String -> String
greaterEqual e1 e2 = makeAsm
    [ e1
    , "push\t%rax"
    , e2
    , "pop\t%rcx"          -- sign extend eax inn i edx
    , "cmp\t%rax, %rcx"          -- sign extend eax inn i edx
    , "mov\t$0, %rax"          -- sign extend eax inn i edx
    , "setge\t%al"
    ]


makeAsm :: [String] -> String
makeAsm xs = unlines $ (++ [""]) xs



type Count = Int
type StackIndex = Int
data CompilerState = CompilerState { counter :: Int
                     , stack_index :: Int
                     , stack_map :: Map String Int
                     }

initialState = CompilerState {counter = 0, stack_index = 0, stack_map = empty}


evalExpr :: Expr -> State CompilerState String
evalExpr (Const int) = return $ mov int

evalExpr (UnOp op e) = do
        let op' = getUn op
        e' <- evalExpr e
        return $ e' ++ op'

evalExpr (BinOp op e1 e2) = do
        let op' = getBin op
        count <- gets counter
        modify (\s@CompilerState {counter=c} -> s {counter = c+1})
        e1' <- evalExpr e1
        e2' <- evalExpr e2
        return $ op' count e1' e2'

evalExpr (Assign id expr) = do
         mpos <- gets $ lookup id . stack_map
         case mpos of
              Nothing -> error "Variable does not exist!"
              Just pos -> do
                   expr' <- evalExpr expr
                   return $ expr' ++ "mov\t%rax, " ++ show (pos-8) ++ "(%rbp)\n"
         


evalExpr (Var id) = do
         mpos <- gets $ lookup id . stack_map
         case mpos of
              Nothing -> error "yall fucked up"
              Just pos -> return $ "mov " ++ show (pos-8) ++ "(%rbp), %rax"
              -- @HACK: Her bare trekker vi fra 8, ettersom at det er
              -- indeks-feilen... Kanskje se om det er en fornuftig
              -- måte å gjøre dette på... Og! Dette vil avhenge av
              -- hva som pushes på stacken.. Kanskje man kan ha 
              -- mindre offsets for 32-bits variabler?
              -- Og hva med structs? Vi kommer vel dit!


getUn Negate        = neg
getUn Not           = not
getUn Complement    = compl

getBin Add          = const add
getBin Subtract     = const subtract
getBin Multiply     = const multiply
getBin Divide       = const divide
getBin Equal        = const equal
getBin NotEqual     = const notEqual
getBin Or           = or
getBin And          = and
getBin LessThan     = const lessThan
getBin LessEqual    = const lessEqual
getBin GreaterThan  = const greaterThan
getBin GreaterEqual = const greaterEqual
