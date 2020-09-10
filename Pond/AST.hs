module Pond.AST
    ( Program (..)
    , Fun (..)
    , Statement (..)
    , VarList (..)
    , Variable (..)
    , Expr (..)
    , Type (..)
    , UOperator (..)
    , BOperator (..)
    , Id (..)
    ) where


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
