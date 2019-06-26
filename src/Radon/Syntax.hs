module Radon.Syntax where


import Data.List


type Name = String


data Module
    = Module Name [Stmt]
    | ModuleParseError String
    deriving (Show, Eq)

data Expr
    = Var Name
    | IntConst Integer
    | FloatConst Double
    | If Expr Expr Expr
    | Let Name Expr Expr
    deriving (Show, Eq)


data Stmt
    = Binding Name [Name] Expr
    deriving (Show, Eq)
