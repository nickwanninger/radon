module Radon.Syntax where

import Data.List

type Name = String

data Module =
    Module Name [TopDecl]
    deriving (Eq, Show)

data RaModule =
    RaModule
        { modName :: Maybe String
        , modStmts :: Maybe [TopDecl]
        }
    deriving (Show, Eq)

data Expr
    = Var Name
    | IntConst Integer
    | FloatConst Double
    | If Expr Expr Expr
    | Let Name Expr Expr
    deriving (Eq)

instance Show Expr where
    show (Var n) = n
    show (IntConst n) = show n
    show (FloatConst n) = show n
    show (If c e1 e2) =
        "if " <> (show c) <> " then " <> (show e1) <> " else " <> (show e2)
    show (Let n e1 e2) =
        "let " <> n <> " = " <> (show e1) <> " in " <> (show e2)

data TopDecl =
    Binding Name [Name] Expr
    deriving (Eq)

instance Show TopDecl where
    show (Binding name args val) =
        name <> (show args) <> " = " <> (show val)
