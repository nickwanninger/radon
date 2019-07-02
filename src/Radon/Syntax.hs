module Radon.Syntax where

import Data.List

type Name = String

data Module =
    Module Name [TopDecl]
    deriving (Eq)


data RaModule =
    RaModule
        { modName :: Maybe String
        , modStmts :: Maybe [TopDecl]
        }
    deriving (Eq)

instance Show RaModule where
    show m = intercalate "\n\n" (map show (case (modStmts m) of
                                               Nothing -> []
                                               Just l -> l))

type Id = String

data Expr
    = Var Id
    | Lit Lit
    | If Expr Expr Expr
    | Let Name Expr Expr
    | App Expr Expr
    | Neg Expr
    | LCons Expr Expr -- The cons oper, basically :
    | EmptyList -- []
    deriving (Eq)

instance Show Expr where
    show (Var n) = n
    show (Lit l) = show l
    show (If c e1 e2) =
        "if " <> (show c) <> " then " <> (show e1) <> " else " <> (show e2)
    show (Let n e1 e2) =
        "let " <> n <> " = " <> (show e1) <> " in " <> (show e2)
    show (App f a) = "(" <> (show f) <> " " <> (show a) <> ")"
    show (Neg e) = "-" ++ (show e)
    show (LCons l r) = "(" <> (show l) <> ":" <> (show r) <> ")"
    show (EmptyList) = "[]"

-- Converts a haskell list of expressions to a nested lcons expr
toLCons :: [Expr] -> Expr
toLCons [] = EmptyList
toLCons (x:xs) = LCons x (toLCons xs)

data Lit
    = LitInt Integer
    | LitDouble Double
    deriving (Eq)

instance Show Lit where
    show (LitInt i) = show i
    show (LitDouble i) = show i

data TopDecl =
    Binding Name [ArgCase]
    deriving (Eq)

instance Show TopDecl where
    show (Binding name args) = label <> (showArgCases indent args)
      where
        label = "let " <> name <> " "
        indent = length label

data Pat
    = VarPat (Id)
    | WildPat -- Basically an underscore
    | ParPat (Pat) -- Parenthesised pattern. We have to record that they were parenthesised
    | LitPat (Lit)
    | ListPat Pat Pat -- x:xs
    | TuplePat [Pat] -- (a, b)
    deriving (Eq)

instance Show Pat where
    show (VarPat i) = i
    show (WildPat) = "_"
    show (ParPat p) = "(" ++ (show p) ++ ")"
    show (LitPat lit) = show lit
    show (ListPat l r) = (show l) ++ ":" ++ (show r)
    show (TuplePat ps) = "(" ++ (intercalate ", " (map show ps)) ++ ")"

-- Arguments are just a list of possible pattern matches
type Arguments = [Pat]

type ArgCase = (Arguments, Expr)

showArgs :: Arguments -> String
showArgs args = (intercalate ", " (map show args))

showArgCase :: ArgCase -> String
showArgCase (args, val) = (showArgs args) ++ " = " ++ (show val)

showArgCases :: Int -> [ArgCase] -> String
-- One case should print without the leading of
showArgCases _ [c] = showArgCase c
showArgCases ind cases =
    (intercalate indent (map ("of " ++) (map (showArgCase) cases)))
    where indent = "\n" ++ (concat $ replicate ind " ")
