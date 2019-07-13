module Radon.Syntax where

import Data.List
import Text.PrettyPrint hiding ((<>))

type Name = String

data Module =
    Module
        { modName :: Maybe String
        , modStmts :: [TopDecl]
        }
    deriving (Eq)

instance Show Module where
    show m = render $ ppModule m

type Id = String



data Expr
    = Var Id
    | Lit Lit
    | If Expr Expr Expr
    | Let [(Pat, Expr)] Expr -- let expression, `let _ in _`
    | OpChain [Expr] -- An operator chain, parsed fro
    | App Expr [Expr] -- A funciton applied to arguments, `f x y z...`
    | BApp Id [Expr] -- An operator application
    | Neg Expr -- negation, `-x`
    | LCons Expr Expr -- The cons oper, basically :
    | EmptyList -- []
    | TupleLit [Expr]
    | Paren Expr -- parenthesized expression
    | Lambda [Pat] Expr -- a lambda is a list of patterns (arguments) and a body
                        -- Just because they are patterns, doesnt mean you can do
                        -- pattern matching in the lambda literal. We will end up
                        -- checking that all the arguments are just variable patterns
    deriving (Eq)

instance Show Expr where
    show x = render $ ppExpr x

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
    show x = render $ ppTop x

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
showArgs args = " " ++ (intercalate " " (map show args))

showArgCase :: ArgCase -> String
showArgCase (args, val) = (showArgs args) ++ " = " ++ (show val)

showArgCases :: Int -> [ArgCase] -> String
-- One case should print without the leading of
showArgCases _ [c] = showArgCase c
showArgCases ind cases =
    indent ++ (intercalate indent (map ("of " ++) (map (showArgCase) cases)))
  where
    indent = "\n    "

-- Pretty print a module into a document
ppModule :: Module -> Doc
ppModule m =
    vcat $
    map ppTop $ modStmts m

-- pretty print a top level declaration
ppTop :: TopDecl -> Doc
ppTop (Binding name args) =
    text "def" <+> (text name) $$ nest 4 (ppArgCases args)

ppPat :: Pat -> Doc
ppPat p = text $ show p

ppArgs :: Arguments -> Doc
ppArgs [] = text ""
ppArgs args = hsep (map ppPat args)

ppArgCase :: ArgCase -> Doc
ppArgCase (args, val) = (ppArgs args) <+> (char '=') <+> (ppExpr val)

ppArgCases :: [ArgCase] -> Doc
ppArgCases cases = vcat $ map ((text "of") <+>) $ map ppArgCase cases

ppExpr :: Expr -> Doc
ppExpr (Var n) = text n
ppExpr (Lit l) = text $ show l
ppExpr (Paren e) = parens $ ppExpr e
ppExpr (If cond e1 e2) =
    text "if" <+>
    (ppExpr cond) <+>
    (nest 3 $ vcat [text "then" <+> ppExpr e1, text "else" <+> ppExpr e2])
ppExpr (Let bindings e2) =
    (text "let") <+> (ppBindings bindings ) $$ (nest 1 $ text "in" <+> ppExpr e2)
ppExpr (App f args) = parens $ sep $ (ppExpr f) : (map ppExpr args)
ppExpr (BApp op args) =
    parens $ ((parens $ text op) <+> (sep $ map ppExpr args))
ppExpr (Neg e) = char '-' <+> (ppExpr e)
ppExpr (LCons l r) = (ppExpr l) <> (char ':')
ppExpr (EmptyList) = text "[]"
ppExpr (TupleLit elems) = parens $ sep $ map ppExpr elems
ppExpr (Lambda args body) =
    ((text "\\") <> (sep $ map ppPat args)) <+> (text "->") <+> (ppExpr body)
ppExpr (OpChain elems) = parens $ sep $ map ppExpr elems

ppBindings :: [(Pat, Expr)] -> Doc
ppBindings bs = vcat $ map (\(n, e) -> (ppPat n <+> (text "=") <+> ppExpr e) <> text ";") bs
