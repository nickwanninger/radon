-- vim: filetype=happy
{
module Radon.Parser.Parser (parseModule, ParseResult(..)) where

import Radon.Parser.Lexer (Token(..), TokenData(..), lexString)

import Radon.Syntax
}


%name parseModule module
%tokentype { Token }
%error { parseError }

%monad { ParseResult } { >>= } { return }

%token
	'let'  {Tok _ (TLet)}
	'of'   {Tok _ (TOf)}
    'if'   {Tok _ (TIf)}
    'then'   {Tok _ (TThen)}
    'else'   {Tok _ (TElse)}
	';'    {Tok _ (TSemi)}
	'='    {Tok _ (TEquals)}
	var    {Tok _ (TIdent $$)}
    '-'    {Tok _ (TOper "-")}
    varop  {Tok _ (TOper $$)}
    '('    {Tok _ (TLParen)}
    ')'    {Tok _ (TRParen)}
    double {Tok _ (TDouble $$)}
    int    {Tok _ (TInt $$)}
    ':'    {Tok _ (TColon)}
    ','    {Tok _ (TComma)}
    '['    {Tok _ (TLSquare)}
    ']'    {Tok _ (TRSquare)}
    '_'    {Tok _ (TUnder)}
    '->'    {Tok _ (TArrow)}
    lam    {Tok _ (TLam)}

%%

  -- a module is a grouping of top level expressions, like imports,
	-- declarations, types, etc.
module :: { RaModule }
       : topdecls    {% return $ RaModule {modName = Nothing, modStmts = Just (reverse $1)} }


	-- Zero or more semicolons
semis : semis ';'   {}
      | {- empty -} {}


topdecls :: { [TopDecl] }
topdecls : topdecls topdecl semis  {% return ($2:$1) }
         | {- empty -}             {% return [] }


topdecl :: { TopDecl }
topdecl : topLevelBinding { $1 }


bindArgs :: { [String] }
         : bindArgs ',' var {% return ($3:$1)}
         | var  {% return [$1] }

optionalArgs :: { [String] }
             : bindArgs {% return $1 }
             | {- empty -} {% return [] }

topLevelBinding :: { TopDecl }
                -- any top level let statement
                : letBinding {% return $1 }


-- Arguments is just a list of patterns
arguments :: { [Pat] }
          : arguments_ {% return $ reverse $1 }


arguments_ :: { [Pat] }
           : arguments_ apat {% return $ ($2:$1)}
           | {- empty -} {% return [] }


ifstmt :: { Expr }
       : 'if' exp 'then' exp 'else' exp {% return $ If $2 $4 $6 }

apat :: { Pat }
    : var {% return $ VarPat $1 }
    | '_' {% return $ WildPat }
    | int     {% return $ LitPat $ LitInt $1 }
    | double  {% return $ LitPat $ LitDouble $1 }

ofcases :: { [([Pat], Expr)] }
    : ofcases_ {% return $ reverse $1 }

ofcases_ :: { [([Pat], Expr)] }
    : ofcases_ 'of' arguments '=' exp {% return $ (($3, $5):$1)}
    | {- empty -} {% return [] }

letBinding :: { TopDecl }
    -- First type of let statement is simply without any extra cases.
    -- for example: let id x = x
    : 'let' var arguments '=' exp {% return $ (Binding $2 [($3, $5)]) }
    | 'let' var ofcases {% return $ Binding $2 $3 }

aexp :: { Expr }
    : '(' ')'        {% return $ TupleLit [] } -- tuples are parsed before parenthesized exprs
    -- parsing a parenthesis can be a little tricky because the trailing comma changes the expression
    -- from the expression it contains to a tuple of length one
    | '(' listcontents ')' {% return $ makeTupleOrExpr (reverse $2) }
    | '-' aexp     {% return $ Neg $2 } -- Negation parser, simply negate the second expression
    | var          {% return $ Var $1 } -- simple variable, just a name
    | literal      {% return $1 }
    | lambda       {% return $1 }
    | ifstmt       {% return $1 }


literal :: { Expr }
literal : listlit {% return $1 }
        | int     {% return $ Lit $ LitInt $1 }
        | double  {% return $ Lit $ LitDouble $1 }

 -- A literal list, will end up as a series of cons calls
listlit :: { Expr }
listlit : '[' ']' {% return $ EmptyList }
        | '[' listcontents ']' {% return $ toLCons $ reverse $2 }
        | aexp ':' aexp        {% return $ LCons $1 $3 }


listcontents :: { [Expr] }
         : listcontents ',' exp {% return ($3:$1)}
         | exp  {% return [$1] }


 -- fexp is a function application expression, which basically
 -- just means doing left associative juxtaposition for function application
fexp :: { Expr }
     : aexp appArgs {% return $ App $1 (reverse $2)}
     | aexp         {% return $1 }
     | infixop      {% return $1 }


lambda :: { Expr }
lambda : lam arguments '->' exp {% return $ Lambda $2 $4 }

     -- arguments to a function application
appArgs :: { [Expr] }
      : appArgs aexp {% return $ ($2:$1)}
      | aexp {% return [$1]}


op :: { Expr }   -- used in infix decls
op : varop {% return $ Var $1 }


infixop :: { Expr }
infixop
    : fexp {% return $1 }
    | infixop op fexp {% return $ OpApp $1 $2 $3 }

exp :: { Expr }
exp : infixop {% return $1}






{
-- Take a list of expressions and if the list of 1 long, it is simpy the first value,
-- if it is any more, create a tuple
makeTupleOrExpr :: [Expr] -> Expr
makeTupleOrExpr [e] = e
makeTupleOrExpr es@(_:_) = TupleLit es

-- | handle a parser error with the token stream where the error occurs.
--  TODO: create a better error message reporting system with a pretty printer
parseError toks
    = fail $ "Parse Error: " ++ if (length toks == 0)
                            then "Unexpected EOF"
                            else "Unexpected token around " ++ (show $ head toks)

-- Monad for parse result
data ParseResult a
    = Success a
    | ParseError String
    deriving (Show, Eq)

instance Monad ParseResult where
    (ParseError err) >>= f = (ParseError err)
    (Success x) >>= f = f x
    return         = Success

instance  Functor ParseResult  where
    fmap _ (ParseError e)       = ParseError e
    fmap f (Success a)      = Success (f a)

instance Applicative ParseResult where
    pure = Success

    Success f  <*> m       = fmap f m
    ParseError e <*> _m      = ParseError e

    liftA2 f (Success x) (Success y) = Success (f x y)
    liftA2 _ _ _ = ParseError "HERE"

    Success _m1 *> m2      = m2
    ParseError e  *> _m2     = ParseError e
}
