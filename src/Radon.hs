module Radon where

import Data.List
import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token
import Radon.Types
import Radon.Syntax


-- | Parse a string+path into a module
parseString :: String -> String -> Module
parseString src path =
    case parse topLevel path src of
        Left err -> ModuleParseError $ show err
        Right vals -> Module path vals

-- | Read a file and parse the contents
parseFile path = do
    source <- readFile path
    return $ parseString source path


example = parseFile "example.rad"


test :: String -> Either ParseError Expr
test = parse expr ""


languageDef =
  emptyDef { Token.commentStart    = "{-"
           , Token.commentEnd      = "-}"
           , Token.commentLine     = "--"
           , Token.identStart      = letter
           , Token.identLetter     = alphaNum
           , Token.reservedNames   = [ "let"
                                     , "in"
                                     , "if"
                                     , "then"
                                     , "else"
                                     , "while"
                                     , "do"
                                     , "skip"
                                     , "true"
                                     , "false"
                                     , "not"
                                     , "and"
                                     , "or"
                                     ]
           , Token.reservedOpNames = ["+", "-", "*", "/", ":="
                                     , "<", ">", "and", "or", "not"
                                     ]
           }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator
parens     = Token.parens     lexer -- parses surrounding parenthesis:
                                    --   parens p


variable :: Parser Expr
variable = do
    x <- identifier
    return $ Var x


-- | Whitespace and comments simply ignore some run of chars
whiteSpace :: Parser ()
whiteSpace = skipMany1 (oneOf " \t\n")



comment :: Parser ()
comment =
    (string "--" >> manyTill anyChar newline >> spaces >> return ()) <|>
    (string "{-" >> manyTill anyChar ((try (string "-}") >> return ()) <|> eof) >> spaces >> return ())


-- | skip over and ignore buffer chars
ignored :: Parser ()
ignored = skipMany (whiteSpace <|> comment)


-- | parser for integer literals
integer :: Parser Expr
integer = do
    x <- many1 digit
    return $ IntConst $ read x


-- | parse the number types
number :: Parser Expr
number = try integer


-- | parse `if x then y else z` constructs
ifExpr :: Parser Expr
ifExpr =
    do reserved "if"
       cond <- aexpr
       reserved "then"
       e1 <- aexpr
       reserved "else"
       e2 <- aexpr
       return $ If cond e1 e2



-- | parser for the `let x = y in z` construct
letIn :: Parser Expr
letIn =
    do reserved "let"
       name <- identifier
       char '='
       e1 <- aexpr
       reserved "in"
       e2 <- aexpr
       return $ Let name e1 e2


expr :: Parser Expr
expr = do
    ignored
    x <- integer <|> variable <|> parens aexpr <|> letIn <|> ifExpr
    ignored
    return x

aexpr :: Parser Expr
aexpr = do
    ignored
    x <- expr
    ignored
    return x

-- | Top Level statement parsing




-- | Let bindings, so `let id x = x'
letBinding :: Parser Stmt
letBinding = do
    reserved "let"
    name <- identifier
    args <- many identifier
    char '='
    val <- aexpr
    return $ Binding name args val


statement :: Parser Stmt
statement = do
    ignored
    x <- letBinding
    ignored
    return x


topLevel :: Parser [Stmt]
topLevel = do
    many statement
