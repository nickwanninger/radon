module Radon.Syntax where


import Data.List


type Name = String


data Module
    = Module Name [Stmt]
    | ModuleParseError String
    deriving (Eq)

instance Show Module where
    show (Module name stmts)
        = "# module " <> name <> "\n"
          <> intercalate "\n" (map show stmts)

    show (ModuleParseError err)
        = "Parse Error: " <> err

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
    show (If c e1 e2)
        = "if " <> (show c) <> " then " <> (show e1) <> " else " <> (show e2)
    show (Let n e1 e2)
        = "let " <> n <> " = " <> (show e1) <> " in " <> (show e2)


data Stmt
    = Binding Name [Name] Expr
    deriving (Eq)

instance Show Stmt where
    show (Binding name args val)
        = "let " <> name <> concat (map (" " <>) args) <> " = " <> (show val)





