{-# LANGUAGE BlockArguments #-}

module Parser
    ( Parser
    , parse
    , choice
    , between
    , symbol
    , identifier
    , string
    , array
    , list
    , hex
    , bin
    , integer
    , decimal
    , character
    , (<|>)
    , space
    ) where

import Control.Monad
import Control.Applicative
import Data.Char
import Data.Foldable (asum)


newtype Parser a = P (String -> [(a, String)])

parse :: Parser a -> String -> [(a, String)]
parse (P p) inp = p inp

instance Functor Parser where
    fmap g p = P (\inp -> case parse p inp of
                    [] -> []
                    [(v, out)] -> [(g v, out)])

instance Applicative Parser where
    pure v = P (\inp -> [(v, inp)])
    pg <*> px = P (\inp -> case parse pg inp of
        []  -> []
        [(g, out)] -> parse (fmap g px) out)

instance Monad Parser where
    p >>= f = P (\inp -> case parse p inp of
        []  -> []
        [(v, out)] -> parse (f v) out)

instance Alternative Parser where
    empty = P (\inp -> [])
    some x = pure (:) <*> x <*> many x  -- Parsingen feiler om det ikke er noen forekomst.
    many x = some x <|> pure []         -- Parsingen tillater 0 eller flere elementer
    -- @BUG se om 'some' og 'many' er byttet om..
    -- alternativt skriv ned definisjonene av de her.
    p <|> q = P (\inp -> case parse p inp of
        [] -> parse q inp
        [(v, out)] -> [(v,out)])


item :: Parser Char
item = P (\inp -> case inp of
            [] -> []
            (x:xs) -> [(x,xs)])

-- | Derived primitives

sat :: (Char -> Bool) -> Parser Char
sat p = do
    x <- item
    if p x then return x else empty

choice :: [Parser a] -> Parser a
choice = asum -- asum bare folder over en liste med <|>

between :: Parser a -> Parser b -> Parser c -> Parser a
between parser open close = do
            open
            p <- parser
            close
            return p


digit = sat isDigit
lower = sat isLower
upper = sat isUpper
letter = sat isAlpha
alphanum = sat isAlphaNum
char x = sat (== x)

string :: String -> Parser String
string [] = return []
string (x:xs) = do
    char x
    string xs
    return (x:xs)

ident :: Parser String
ident = do
    x <- lower
    xs <- many $ alphanum <|> char '_'
    ys <- many $ char '\''
    return (x:(xs++ys))

nat :: Parser String
nat = do
    xs <- some digit
    return xs

space :: Parser ()
space = do
    many (sat isSpace)
    return ()

decimal :: Parser Float
decimal = do
    x <- nat
    char '.'
    y <- nat
    return $ read $ x ++ "." ++ y

int :: Parser Int
int = do
    x <- nat
    return $ read x

bin' :: Parser String
bin' = do
    x <- char '0'
    y <- char 'b'
    ys <- some $ char '0' <|> char '1'
    return (x:y:ys)

hex' :: Parser String
hex' = do
    x <- char '0'
    y <- char 'x'
    ys <- some $ digit <|> sat (\x -> 65 <= ord x && ord x <= 70) <|> sat (\x -> 97 <= ord x && ord x <= 102)
    return (x:y:ys)

token :: Parser a -> Parser a
token p = do
    space
    v <- p
    space
    return v

identifier :: Parser String
identifier = token ident

integer :: Parser Int
integer = token int

bin :: Parser String
bin = token bin'

hex :: Parser String
hex = token hex'


character :: Char -> Parser Char
character c = token (char c)

symbol :: String -> Parser String
symbol xs = token (string xs)

list :: Parser a -> String -> String -> Parser [a]
list parser open close = do symbol open
                            n <- parser
                            ns <- many (do
                                        symbol ","
                                        parser)
                            symbol close
                            return (n:ns)

array :: Parser a -> Parser [a]
array p = do
    symbol "["
    n <- p
    ns <- many (do symbol ","
                   p)
    symbol "]"
    return (n:ns)
