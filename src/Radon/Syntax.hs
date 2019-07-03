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
    show m =
        intercalate
            "\n\n"
            (map show
                 (case (modStmts m) of
                      Nothing -> []
                      Just l -> l))

type Id = String

data Expr
    = Var Id
    | Lit Lit
    | If Expr Expr Expr
    | Let Name Expr Expr -- let expression, `let _ in _`
    | OpApp Expr Expr Expr -- Three Expressions, left op right
    | App Expr [Expr] -- A funciton applied to arguments, `f x y z...`
    | Neg Expr -- negation, `-x`
    | LCons Expr Expr -- The cons oper, basically :
    | EmptyList -- []
    | TupleLit [Expr]
    | Lambda [Pat] Expr -- a lambda is a list of patterns (arguments) and a body
                        -- Just because they are patterns, doesnt mean you can do
                        -- pattern matching in the lambda literal. We will end up
                        -- checking that all the arguments are just variable patterns
    deriving (Eq)

instance Show Expr where
    show (Var n) = n
    show (Lit l) = show l
    show (If c e1 e2) =
        "if " <> (show c) <> " then " <> (show e1) <> " else " <> (show e2)
    show (Let n e1 e2) =
        "let " <> n <> " = " <> (show e1) <> " in " <> (show e2)
    show (OpApp lhs op rhs) =
        "(" <> (intercalate " " (map show [lhs, op, rhs])) <> ")"
    show (App f args) =
        "(" <> (show f) <> " " <> (intercalate " " $ map show args) <> ")"
    show (Neg e) = "-" ++ (show e)
    show (LCons l r) = "(" <> (show l) <> ":" <> (show r) <> ")"
    show (EmptyList) = "[]"
    show (TupleLit elems) = showTuple elems
    show (Lambda args body) =
        "(\\" <> (intercalate " " (map show args)) <> " -> " <> (show body) <>
        ")"

showTuple :: [Expr] -> String
showTuple [] = "()"
showTuple [x] = "(" <> (show x) <> ",)"
showTuple elms = "(" <> (intercalate ", " $ map show elms) <> ")"

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
        label = "let " <> name
        indent = length label

data Pat
    = VarPat (Id)
    | WildPat -- Basically an underscore
    | ParPat (Pat) -- Parenthesised pattern. We have to record that they were parenthesised
    | LitPat (Lit)
    | ListPat Pat Pat -- x:xs
    | TuplePat [Pat] -- (a, b)
    deriving (Eq)

patternIsVar :: Pat -> Bool
patternIsVar (VarPat _) = True
patternIsVar _ = False

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
showArgs [] = ""
showArgs args = " " <> (intercalate " " (map show args))

showArgCase :: ArgCase -> String
showArgCase (args, val) = (showArgs args) ++ " = " ++ (show val)

showArgCases :: Int -> [ArgCase] -> String
-- One case should print without the leading of
showArgCases _ [c] = showArgCase c
showArgCases ind cases =
    indent ++ (intercalate indent (map ("of " ++) (map (showArgCase) cases)))
  where
    indent = "\n    "
