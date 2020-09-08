
module MDC
    ( mdcExpr )
    where

import Parser

data Expr = SUM Term Expr | Term Term deriving (Show)
data Term = PROD Factor Term | Factor Factor deriving (Show)
data Factor = PARENS Expr | Integer Int deriving (Show)

mdcExpr = expr

expr :: Parser Expr
expr = do
    t <- term
    sum t <|> return (Term t)
    where sum t = do
                symbol "+"
                e <- expr
                return $ SUM t e

term :: Parser Term
term = do
    f <- factor
    prod f <|> return (Factor f)
    where prod f = do
                symbol "*"
                t <- term
                return (PROD f t)

factor :: Parser Factor
factor = expr' <|> (Integer <$> integer)
    where expr' = do
                symbol "("
                e <- expr
                symbol ")"
                return $ PARENS e
