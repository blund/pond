module Frontend
    ( program
    ) where

import AST
import Parser
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
         <|> (Const <$> (hexadecimal <|> binary <|> integer))

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
