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
    ) where


---------------------------------------
-- | Datatypes that make the AST
---------------------------------------

data Program = Program Fun

data Fun = Fun
    { f_type    :: Type
    , f_name    :: String
    , f_vars    :: VarList
    , f_st      :: [Statement]
    }

data Statement = Return Expr
               | Declare String (Maybe Expr)
               | Expression Expr

data VarList = VarList [Variable]
             | VarEmpty

data Variable = VarDef
    { v_type :: Type
    , v_name :: String
    }

data Expr = Assign String Expr
     	  | Var String
          | BinOp BOperator Expr Expr
          | UnOp UOperator Expr
          | Const Int

data Type = Type String

data UOperator = Negate | Complement | Not
    deriving (Show)

data BOperator = Add | Subtract | Divide | Multiply
               | Equal | NotEqual
               | LessThan | GreaterThan
               | LessEqual | GreaterEqual
               | And | Or
    deriving (Show)


---------------------------------------
-- | Instance declarations for the AST
---------------------------------------

instance Show Program where
    show (Program fd) = show fd

instance Show Fun where
    show f = foldr (++) [] $ [ "fun "
                           , show (f_name f), ": ", show (f_type f), "\n"
                           , "params:\n\t", show (f_vars f), "\n"
                           -- , map show (f_st f), "\n"
                           ] ++ map ((++ "\n") . show) (f_st f)

instance Show Statement where
    show (Return e) = "Return:\n\t" ++ show e
    show (Expression e) = "Expression:\n\t" ++ show e
    show (Declare name e) = "Declare:\n\t" ++ show name ++ " = " ++ show e

instance Show VarList where
    show VarEmpty = "none"
    show (VarList vs) = show vs

instance Show Variable where
    show v =  v_name v ++ ":" ++  show (v_type v)

instance Show Expr where
    show (Var id) = "Var: " ++ id
    show (Const v) = "Int<" ++ show v ++ ">"
    show (Assign id v) = "Assign: " ++ id ++ "=" ++ show v
    show (UnOp o v) = show o ++ show v-- "Int<" ++ show v ++ ">"
    show (BinOp b v1 v2) = show b ++  "(" ++ show v1 ++ ", " ++ show v2 ++ ")"-- "Int<" ++ show v ++ ">"

instance Show Type where
    show (Type t) = t
