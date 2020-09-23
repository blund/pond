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
printResult = makeAsm ["movq\t%rax, %rdi", "call\tprint"]
            ++ epilogue


compile :: Program -> String
compile (Program f) = ".globl main\n\n" ++ compileFunction f


prologue = makeAsm [ "push %rbp"       -- save old value
                   , "mov %rsp, %rbp" -- current top of stack is bottom of new stack frame
                   ]
           
epilogue = makeAsm [ "mov %rbp, %rsp" -- restore ESP; now it points to old EBP
                   , "pop %rbp"        -- restore old EBP"   now ESP is where it was before prologue
                   , "ret"
                   ]



makeAsm :: [String] -> String
makeAsm xs = unlines $ (++ [""]) xs

type Count = Int
type StackIndex = Int
type VarMap = Map String Int
data CompilerState = CompilerState { counter :: Int
                     , stack_index :: Int
                     , var_map :: VarMap
                     , current_scope :: VarMap
                     , scope_bytes_declared :: Int
                     }

initialState = CompilerState { counter = 0
                             , stack_index = -8  -- begin at 8 bytes offset
                             , var_map = empty
                             , current_scope = empty
                             , scope_bytes_declared = 0
                             }


-- compileFunction :: Fun -> String
compileFunction f = do
  let st = initialState
      name        = f_name f ++ ":\n\n"
      block_items = f_st f
      body        = evalState (compileBlock block_items st) st
  name ++ prologue ++ body ++ epilogue


-- @UKLART: Poenget her er å fange en kopi av tilstanden til
-- compileren, men ikke modifisere den. Slik får vi laget 'scopes' som
-- ikke påvirker hverandre, selv om at de er nøstet inn i hverandre..
-- @UKLART: Det som skjer med 'scope_count' er at vi vil bevare denne
-- telleren når vi går ut av en scope (siden den brukes for å
-- spesifiere jumps i koden). Derfor må denne kodesnutten forbli i
-- State-monaden - vi vil sende denne verdien videre. Resten av operasjonene
-- som utføres er gjort i let-statements (som man kan se)
compileBlock :: [BlockItem] -> CompilerState -> State CompilerState String
compileBlock block state = do
  let state' = state { current_scope = empty }
      (block', state'') = runState (unlines <$> mapM compileBlockItem block) state'
      -- Her deallokerer vi antal bytes som ble brukt i scopen,
      -- altså vi minker bare stacken, slik at vi kan skrive
      -- over verdiene som allerede var der :)
      to_deallocate = show $ scope_bytes_declared state''
      scope_count = counter state''
  modify (\s -> s {counter = scope_count})
  return $ block' ++ "add $" ++ to_deallocate ++ ", %rsp\n"

  
compileExpr :: Expr -> State CompilerState String
compileExpr (Const int) = return $ mov int

compileExpr (UnOp op e) = do
        let op' = getUn op
        e' <- compileExpr e
        return $ e' ++ "\n"++ op'

compileExpr (BinOp op e1 e2) = do
        let op' = getBin op
        count <- gets counter
        modify (\s@CompilerState {counter=c} -> s {counter = c+1})
        e1' <- compileExpr e1
        e2' <- compileExpr e2
        return $ op' count e1' e2'

compileExpr (Assign id expr) = do
         mpos <- gets $ lookup id . var_map
         case mpos of
              Nothing -> error $ "Cannot assign value to variable " ++ id ++", it has not defined"
              Just pos -> do
                   expr' <- compileExpr expr
                   return $ expr' ++ "mov\t%rax, " ++ show pos ++ "(%rbp)\n"
         


compileExpr (Var id) = do
         mpos <- gets $ lookup id . var_map
         case mpos of
              Nothing -> error $ "Can't refer to variable" ++ id ++ ", it has not been defined"
              Just pos -> return $ "mov " ++ show pos ++ "(%rbp), %rax\n"
              -- @HACK: Her bare trekker vi fra 8, ettersom at det er
              -- indeks-feilen... Kanskje se om det er en fornuftig
              -- måte å gjøre dette på... Og! Dette vil avhenge av
              -- hva som pushes på stacken.. Kanskje man kan ha 
              -- mindre offsets for 32-bits variabler?
              -- Og hva med structs? Vi kommer vel dit!


compileBlockItem :: BlockItem -> State CompilerState String
compileBlockItem (Statement s) = compileStatement s
compileBlockItem (Declaration d) = compileDeclaration d

compileDeclaration :: Declare -> State CompilerState String
compileDeclaration (Declare id me) = do
         index <- gets stack_index
         modify (\s@CompilerState {var_map=m} -> s {var_map = insert id index m})
         modify (\s@CompilerState {stack_index=i} -> s {stack_index =i-8})
         -- @HACK her må faktisk avgjøre hvor mange bytes det er snakk om,
         -- men siden vi kun bruker ints for nå er det bare 8 om gangen
         modify (\s@CompilerState {scope_bytes_declared=b} -> s { scope_bytes_declared = b+8 })
         case me of
           Nothing -> return "push\t%rax\n"
           Just e  -> do
             expr' <- compileExpr e
             return $ expr' ++ "push\t%rax\n" 
                 
compileStatement :: Statement -> State CompilerState String
compileStatement (Return e) = do
  e' <- compileExpr e
  return $ e' ++ printResult
compileStatement (Expression e) = compileExpr e
compileStatement c@(Condition {}) = compileCondition c
compileStatement (Compound b) = do
  state <- get
  compileBlock b state


compileCondition :: Statement -> State CompilerState String
compileCondition (Condition e s ms) = do
    e1 <- compileExpr e
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

