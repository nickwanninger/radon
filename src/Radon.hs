{-# LANGUAGE DeriveDataTypeable #-}

module Radon where

import Data.Data
import Data.List
import Data.Set (Set)
import Data.Typeable
import Data.Void
import System.IO

import Control.Monad

import Control.Applicative.Permutations
import Control.Monad.Combinators.Expr
-- | Megaparsec imports
import Text.Megaparsec
import Text.Megaparsec.Error
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Radon.Syntax
import Radon.Types

data SyntaxError =
    SyntaxError String
    deriving (Eq, Data, Typeable, Ord, Read, Show)

instance ShowErrorComponent SyntaxError where
    showErrorComponent (SyntaxError msg) = "Syntax Error error: " ++ msg

-- | What a parser is in the radon parser combinator
type Parser = Parsec SyntaxError String

-- | Parse a string+path into a module
parseString :: String -> String -> Module
parseString src path =
    case parse topLevel path src of
        Left (ParseErrorBundle err syntaxerr) -> ModuleParseError $ show err
        Right vals -> Module path vals

-- | Read a file and parse the contents
parseFile path = do
    source <- readFile path
    return $ parseString source path

-- | Helper function
-- | TODO: DELETE
example = parseFile "example.rad"

-- | Space Consumer: Absorbs whitespace, line comments, and block comments
sc :: Parser ()
sc = L.space space1 lineCmnt blockCmnt
  where
    lineCmnt = L.skipLineComment "--"
    blockCmnt = L.skipBlockComment "{-" "-}"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

test = parse expr ""

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

rword :: String -> Parser ()
rword w = (lexeme . try) (string w *> notFollowedBy alphaNumChar)

reservedWords :: [String] -- list of reserved words
reservedWords = ["if", "then", "else", "let"]

identifier :: Parser String
identifier = (lexeme . try) (p >>= check)
  where
    p = (:) <$> letterChar <*> many alphaNumChar
    check x =
        if x `elem` reservedWords
            then fail $ "keyword " ++ show x ++ " cannot be an identifier"
            else return x

variable :: Parser Expr
variable = do
    x <- identifier
    return $ Var x

-- | parser for integer literals
integer :: Parser Expr
integer = do
    x <- some digitChar
    return $ IntConst $ read x

-- | parse the number types
number :: Parser Expr
number = try integer

-- | parse `if x then y else z` constructs
ifExpr :: Parser Expr
ifExpr = do
    rword "if"
    cond <- aexpr
    rword "then"
    e1 <- aexpr
    rword "else"
    e2 <- aexpr
    return $ If cond e1 e2

-- | parser for the `let x = y in z` construct
letIn :: Parser Expr
letIn = do
    rword "let"
    name <- identifier
    char '='
    e1 <- aexpr
    rword "in"
    e2 <- aexpr
    return $ Let name e1 e2



expr :: Parser Expr
expr = do
    sc
    x <- integer <|> variable <|> parens aexpr <|> letIn <|> ifExpr
    sc
    return x


aexpr :: Parser Expr
aexpr = do
    sc
    x <- expr
    sc
    return x


-- | Top Level statement parsing
-- | Let bindings, so `let id x = x'
letBinding :: Parser Stmt
letBinding = do
    rword "let"
    name <- identifier
    args <- many identifier
    symbol "="
    val <- aexpr
    return $ Binding name args val

statement :: Parser Stmt
statement = do
    sc
    x <- letBinding
    sc
    return x

topLevel :: Parser [Stmt]
topLevel = do
    many statement
