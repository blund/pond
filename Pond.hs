module Main (main) where

import Prelude hiding (not)

import Data.Char
import Control.Monad
import Control.Applicative hiding (Const)
import System.IO
import System.Environment

import Parser

---------------------------------------
-- | Datatypes that make the AST
---------------------------------------
--
data Program = Program Fun

data Fun = Fun
    { f_type    :: Type
    , f_id      :: Id
    , f_vars    :: VarList
    , f_st      :: Statement
    }

data Statement = Return Exp

data VarList = VarList [Variable]
             | VarEmpty

data Variable = VarDef
    { v_type :: Type
    , v_name :: String
    }

data Exp = Const Int | UnOp Operator Exp

data Type = Type String

data Operator = Negate | Complement | Not deriving
    (Show)

data Id = Id String


---------------------------------------
-- | Instance declarations for the AST
---------------------------------------

instance Show Program where
    show (Program fd) = show fd

instance Show Fun where
    show f = foldr (++) [] [ "fun "
                           , show (f_id f), ": ", show (f_type f), "\n"
                           , "params:\n\t", show (f_vars f), "\n"
                           , "body:\n\treturn: ", show (f_st f), "\n"
                           ]

instance Show Statement where
    show (Return e) = show e

instance Show VarList where
    show VarEmpty = "none"
    show (VarList vs) = show vs

instance Show Variable where
    show v =  v_name v ++ ":" ++  show (v_type v)

instance Show Exp where
    show (Const v) = "Int<" ++ show v ++ ">"
    show (UnOp o v) = show o ++ show v-- "Int<" ++ show v ++ ">"

instance Show Type where
    show (Type t) = t

instance Show Id where
    show (Id string) = string


---------------------------------------
-- | Front end / Parsers
---------------------------------------

getOperator :: String -> Operator
getOperator "~" = Complement
getOperator "-" = Negate
getOperator "!" = Not

program :: Parser Program
program = Program <$> function

function :: Parser Fun
function = do
    typ <- Type <$> symbol "int"
    id <- Id <$> identifier
    vars <- varList
    st <- between statement (symbol "{") (symbol "}")
    return $ Fun { f_type = typ
                 , f_id = id
                 , f_vars = vars
                 , f_st = st
                 }

statement :: Parser Statement
statement = do
    symbol "return"
    e <- Return <$> val
    symbol ";"
    return e

val :: Parser Exp
val = op <|> const
    where const = (binary <|> hexadecimal <|> integer) >>= \const -> return $ Const const
          op = do
            o <- getOperator <$> (symbol "!" <|> symbol "~" <|> symbol "-")
            v <- val
            return (UnOp o v)
        -- @TODO: må også parse for expr...



varDecl :: Parser Variable
varDecl = do
    typ <- Type <$> identifier
    space
    name <- identifier
    return $ VarDef typ name

-- @Fiks: ordne en penere definisjon, se på liste-abstaksjonen
varList :: Parser VarList
varList = VarList <$> list varDecl "(" ")" <|> do symbol "("
                                                  symbol ")"
                                                  pure VarEmpty

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
movl v = "\tmovl\t$" ++ show v ++ ", %eax\n"

neg :: String
neg = "\tneg\t%eax\n"

not :: String
not = "\tnot\t%eax\n"

compl :: String
compl = "\tcmpl\t$0, %eax\n"    -- compare eax to 0
      ++"\txor\t%eax, %eax\n"   -- zero out register
      ++"\tsete\t%al\n"         -- iff ZF from comparison, set lower byte of eax

evalExpr :: Exp -> String
evalExpr (Const int) = movl int
evalExpr (UnOp op expr) = (evalExpr expr) ++ op'
    where op' = case op of
                    Negate -> neg
                    Not -> not
                    Complement -> compl



main :: IO ()
main = do
    mode <- getArgs

    handle <- openFile "test.c" ReadMode
    file <- hGetContents handle

    let ast = extract $ parse program file

    case mode of
        ["compile"] -> do
            writeFile "test.s" $ compile ast
        otherwise -> do
            print ast
            print $ execute ast

    hClose handle
