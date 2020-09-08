module Main where
import System.IO
import Control.Monad
import Control.Applicative
import Data.Char

import Parser
import MDC

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

data Exp = Int Int

data Type = Type String

data Id = Id String



---------------------------------------
-- | Instance declarations for the AST
---------------------------------------

instance Show Program where
    show (Program fd) = show fd

instance Show Fun where
    show f = foldr (++) [] [ "fun '"
                           , show (f_id f), "': ", show (f_type f), "\n"
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
    show (Int v) = "Int<" ++ show v ++ ">"

instance Show Type where
    show (Type t) = t

instance Show Id where
    show (Id string) = string



---------------------------------------
-- | Front end / Parsers
---------------------------------------

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
val = Int <$> binary
    <|> Int <$> hexadecimal
    <|> Int <$> integer


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

executeC :: Program -> String
executeC p = runMain p
    where runMain (Program f) = case f_id f of
            (Id "main") -> getResult (f_st f)
            otherwise -> error "no function main defined"
          getResult (Return e) = show e

compile :: Program -> String
compile p = function (fname p) (rvalue p)
    where function name value = head name ++ return value
          head name = ".globl _" ++ name ++ "\n\n_" ++ name ++ ":\n"
          return value = "movl $" ++ show value ++ ", %eax\nret\n"
          fname (Program f) = show $ f_id f
          rvalue (Program f) = show $ f_st f

extract :: [(a, b)] -> a
extract ((a,b):xs) = a
extract _ = error "parse error"

main :: IO ()
main = do
    handle <- openFile "test.c" ReadMode
    file <- hGetContents handle
    print $ extract $ parse program file
    hClose handle
    print $ parse hex "0xf"
    print $ parse bin "  0b1001     \n\t "
    print $ parse binary "0b001"
    print $ parse hexadecimal "0x0f"
    {-
    print $ parse (array decimal) "[0.4, 0.3, 3.14, 0.3]"
    print $ parse identifier " t_test'"
    print $ parse mdcExpr "2 + 2"
    print $ parse program "int main() { return 2; }"
    print $ compile $ extract $ parse program "int main() { return 2; }"
    -}
