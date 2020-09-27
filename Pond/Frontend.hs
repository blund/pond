
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
program = do
  fs <- many functionDecl
  return $ Program fs

functionDecl :: Parser FunctionDecl
functionDecl = do
    typ <- symbol "int"
    name <- identifier
    vars <- varList
    bi <- between (many blockItem) (symbol "{") (symbol "}")
    -- @TODO: Her kan vi sjekke etter mangel på return statement!
    return $ FunctionDecl
      { f_type = typ
      , f_name = name
      , f_vars = vars
      , f_st = bi
      }


blockItem :: Parser BlockItem
blockItem = choice [ Statement <$> statement
                   , Declaration <$> declare
                   ]

statement :: Parser Statement
statement = choice [ do
                      symbol "return"
                      e <- Return <$> expr
                      symbol ";"
                      return e
                   , do
                      e <- Expression <$> expr
                      symbol ";"
                      return e
                   , condition
                   , compound
                   ]


compound :: Parser Statement
compound = do
  symbol "{"
  es <- some blockItem
  symbol "}"
  return (Compound es)


condition :: Parser Statement
condition = do
  symbol "if"
  e <- expr
  symbol "{"
  s1 <- statement
  symbol "}"
  else_block e s1 <|> return (Condition e s1 Nothing)
    where
      else_block e s1 = do
        symbol "else"
        symbol "{"
        s2 <- statement
        symbol "}"
        return (Condition e s1 (Just s2))
      else_st e s1 = do
                   symbol "else"
                   Condition e s1 . Just <$> statement



declare :: Parser Declare
declare = do
 symbol "int"
 name <- identifier
 (do
   symbol "="
   e <- expr
   symbol ";"
   return (Declare name (Just e))) <|> do
          symbol ";"
          return (Declare name Nothing)

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
expr = assign <|> pres6 <|> functionCall

functionCall :: Parser Expr
functionCall = do
  id <- identifier
  args <- some_args <|> no_args
  return $ FunctionCall id args
  where some_args = list expr "(" ")"
        no_args = do
          symbol "("
          symbol ")"
          return []

assign :: Parser Expr
assign = do
    name <- identifier
    symbol "="
    Assign name <$> expr

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
            UnOp o <$> factor
         <|> functionCall
         <|> Const <$> (hexadecimal <|> binary <|> integer)
         <|> Var <$> identifier


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
                    op <- getBOperator <$> choice' symbol ops
                    BinOp op v1 <$> pv



-- test = [(Infix, ")]

varDecl :: Parser Variable
varDecl = do
    typ <- identifier
    space
    VarDef typ <$> identifier

-- @Fiks: ordne en penere definisjon, se på liste-abstaksjonen
varList :: Parser VarList
varList = do
    vars <- list varDecl "(" ")"
    return $ Just vars
  <|> do
       symbol "("
       symbol ")"
       return Nothing
