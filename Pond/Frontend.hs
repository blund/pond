module Pond.Frontend
    ( parseSource
    ) where

import Pond.AST
import Pond.Parser
---------------------------------------
-- | Front end / Parsers
---------------------------------------

extract :: [(a, b)] -> a
extract ((a,b):xs) = a
extract _ = error "parse error"

parseSource = extract . parse program

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
getBOperator "+"    = Add
getBOperator "-"    = Subtract
getBOperator "*"    = Multiply
getBOperator "/"    = Divide
getBOperator "=="   = Equal
getBOperator "!="   = NotEqual
getBOperator "<"    = LessThan
getBOperator ">"    = GreaterThan
getBOperator "<="   = LessEqual
getBOperator ">="   = GreaterEqual
getBOperator "&&"   = And
getBOperator "||"   = Or

-- https://norasandler.com/2017/12/15/Write-a-Compiler-3.html

expr :: Parser Expr
expr = pres6

pres6 :: Parser Expr
pres6 = doLeftRecur pres5 ["||"]

pres5 :: Parser Expr
pres5 = doLeftRecur pres4 ["&&"]

pres4 :: Parser Expr
pres4 = doLeftRecurJoin pres3 And ["!=","=="]

pres3 :: Parser Expr
pres3 = doLeftRecurJoin pres2 And ["<=", ">=", "<", ">"]

pres2 :: Parser Expr
pres2 = doLeftRecur pres1 ["+", "-"]

pres1 :: Parser Expr
pres1 = doLeftRecur factor ["*", "/"]

factor :: Parser Expr
factor = do
            symbol "("
            e <- expr
            symbol ")"
            return e
         <|> do
            o <- getUOperator <$> (symbol "-" <|> symbol "~" <|> symbol "!")
            v <- factor
            return (UnOp o v)
         <|> (Const <$> (hexadecimal <|> binary <|> integer))


doLeftRecur :: Parser Expr -> [String] -> Parser Expr
doLeftRecur p ops = do
                v1 <- p
                leftRecur v1 p ops

leftRecur :: Expr -> Parser Expr -> [String] -> Parser Expr
leftRecur v1 pv ops = (do
                    op <- getBOperator <$> choice' symbol ops
                    v2 <- pv
                    leftRecur (BinOp op v1 v2) pv ops) <|> return v1


doLeftRecurJoin :: Parser Expr -> BOperator -> [String] -> Parser Expr
doLeftRecurJoin p join ops = do
                v1 <- p
                leftRecurJoin v1 p join ops

leftRecurJoin :: Expr -> Parser Expr -> BOperator -> [String] -> Parser Expr
leftRecurJoin v1 pv join ops = (do
            op <- getBOperator <$> choice' symbol ops
            v2 <- pv
            (do
             v3 <- leftRecur' v2 pv ops
             return (BinOp join (BinOp op v1 v2) v3)) <|> return (BinOp op v1 v2)) <|> return v1

-- Hjelpefunksjon for leftRecurBind som kjører kun en gang, brukes f.eks for
-- 8 > 7 > 6, hvor vi bare vil parse en rekurson :)
leftRecur' :: Expr -> Parser Expr -> [String] -> Parser Expr
leftRecur' v1 pv ops = do
                    op <- getBOperator <$> (choice' symbol ops)
                    v2 <- pv
                    return (BinOp op v1 v2)



-- test = [(Infix, ")]

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
