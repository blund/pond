{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Prelude hiding (not, subtract)

import Data.Char

import Control.Monad
import Control.Applicative hiding (Const)

import System.IO
import System.Environment
import System.Process

import Parser

---------------------------------------
-- | Datatypes that make the AST
---------------------------------------

data Program = Program Fun

data Fun = Fun
    { f_type    :: Type
    , f_id      :: Id
    , f_vars    :: VarList
    , f_st      :: Statement
    }

data Statement = Return Expr

data VarList = VarList [Variable]
             | VarEmpty

data Variable = VarDef
    { v_type :: Type
    , v_name :: String
    }

data Expr = BinOp BOperator Expr Expr | UnOp UOperator Expr | Const Int

data Type = Type String

data UOperator = Negate | Complement | Not
    deriving (Show)

data BOperator = Add | Subtract | Divide | Multiply
    deriving (Show)

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

instance Show Expr where
    show (Const v) = "Int<" ++ show v ++ ">"
    show (UnOp o v) = show o ++ show v-- "Int<" ++ show v ++ ">"
    show (BinOp b v1 v2) = show b ++  "(" ++ show v1 ++ ", " ++ show v2 ++ ")"-- "Int<" ++ show v ++ ">"

instance Show Type where
    show (Type t) = t

instance Show Id where
    show (Id string) = string


---------------------------------------
-- | Front end / Parsers
---------------------------------------
--
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
    e <- Return <$> expr
    symbol ";"
    return e

getUOperator :: String -> UOperator
getUOperator "-" = Negate
getUOperator "!" = Not
getUOperator "~" = Complement

getBOperator :: String -> BOperator
getBOperator "+" = Add
getBOperator "-" = Subtract
getBOperator "*" = Multiply
getBOperator "/" = Divide

-- https://norasandler.com/2017/12/15/Write-a-Compiler-3.html

expr :: Parser Expr
expr = do
        e1 <- term
        op <- getBOperator <$> (symbol "+" <|> symbol "-")
        e2 <- expr
        return $!(BinOp op e1 e2)
       <|> term

term :: Parser Expr
term = do
        t1 <- factor
        op <- getBOperator <$> (symbol "*" <|> symbol "/")
        t2 <- term
        return (BinOp op t1 t2)
       <|> factor

factor :: Parser Expr
factor = do
            symbol "("
            e <- expr
            symbol ")"
            return e
         <|> do
            o <- getUOperator <$> (symbol "-" <|> symbol "~" <|> symbol "!")
            f <- factor
            return (UnOp o f)
         <|> (Const <$> integer)

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
compl =  "\tcmpl\t$0, %eax\n"    -- compare eax to 0
      ++ "\txor\t%eax, %eax\n"   -- zero out register
      ++ "\tsete\t%al\n"         -- iff ZF from comparison, set lower byte of eax

add :: String -> String -> String
add e1 e2 =  e1
          ++ "\tpush\t%eax\n"
          ++ e2
          ++ "\tpop\t%ecx\n"
          ++ "\taddl\t%ecx, %eax\n"

multiply :: String -> String -> String
multiply e1 e2 =  e1
          ++ "\tpush\t%eax\n"
          ++ e2
          ++ "\tpop\t%ecx\n"
          ++ "\timul\t%ecx, %eax\n"

subtract :: String -> String -> String
subtract e1 e2 =  e2                -- merk! operander er byttet om
          ++ "\tpush\t%eax\n"
          ++ e1
          ++ "\tpop\t%ecx\n"
          ++ "\tsubl\t%ecx, %eax\n"

divide :: String -> String -> String
divide e1 e2 =  e2              -- merk! operander er byttet om
          ++ "\tpush\t%eax\n"
          ++ "\tcdq\n"          -- sign extend eax inn i edx
          ++ e1
          ++ "\tpop\t%ecx\n"
          ++ "\tidivl\t%ecx, %eax\n"

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
            -- print $ execute ast

    hClose handle
