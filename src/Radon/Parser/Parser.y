-- vim: filetype=happy
{
module Radon.Parser.Parser (parseModule, parseExpr, ParseResult(..)) where

import Radon.Parser.Lexer (Token(..), TokenData(..), lexString)

import Radon.Syntax
import Radon.Types
}


%name parseModule module
%name parseExpr exp
%tokentype { Token }
%error { parseError }

%monad { ParseResult } { >>= } { return }

%token
	'def'  {Tok _ (TDef)}
	'let'  {Tok _ (TLet)}
	'of'   {Tok _ (TOf)}
    'if'   {Tok _ (TIf)}
    'then' {Tok _ (TThen)}
    'else' {Tok _ (TElse)}
    'in'   {Tok _ (TIn)}
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
module :: { Module }
       : topdecls    {% return $ Module {modName = Nothing, modStmts = Just (reverse $1)} }


topdecls :: { [TopDecl] }
topdecls : topdecls topdecl  {% return ($2:$1) }
         | {- empty -}       {% return [] }


topdecl :: { TopDecl }
topdecl : letBinding { $1 }


-- Arguments is just a list of patterns
arguments :: { [Pat] }
          : arguments_ {% return $ reverse $1 }


arguments_ :: { [Pat] }
           : arguments_ apat {% return $ ($2:$1)}
           | {- empty -} {% return [] }



optionalSemi
    : ';' {}
    | {- empty -} {}

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
    : 'def' var arguments '=' exp {% return $ (Binding $2 [($3, $5)]) }
    | 'def' var ofcases {% return $ Binding $2 $3 }

aexp :: { Expr }
    : '(' ')'         {% return $ TupleLit [] }
    | '(' explist ')' {% return $ makeTupleOrExpr (reverse $2) }
    | '-' aexp        {% return $ Neg $2 }
    | 'if' exp 'then' exp 'else' exp {% return $ If $2 $4 $6 }
    | 'let' bindings optionalSemi 'in' exp {% return $ Let $2 $5 }
    | aexp1           { $1 }

aexp1 :: { Expr }
aexp1
    : var             {% return $ Var $1 } -- simple variable, just a name
    | literal         {% return $1 }
    | lam arguments '->' exp {% mkLambda $2 $4 }


literal :: { Expr }
literal : listlit {% return $1 }
        | int     {% return $ Lit $ LitInt $1 }
        | double  {% return $ Lit $ LitDouble $1 }

 -- A literal list, will end up as a series of cons calls
listlit :: { Expr }
listlit : '[' ']' {% return $ EmptyList }
        | '[' explist ']' {% return $ toLCons $ reverse $2 }
        | aexp ':' aexp        {% return $ LCons $1 $3 }


explist :: { [Expr] }
         : explist ',' exp {% return ($3:$1)}
         | exp  {% return [$1] }




bindings :: { [(Pat, Expr)] }
         : bindings ';' apat '=' exp {% return (($3, $5):$1) }
         | apat '=' exp {% return [($1, $3)]}



 -- fexp is a function application expression, which basically
 -- just means doing left associative juxtaposition for function application
fexp :: { Expr }
     : aexp appargs {% return $ App $1 (reverse $2)}
     | aexp         {% return $1 }

     -- arguments to a function application
appargs :: { [Expr] }
      : appargs aexp {% return $ ($2:$1)}
      | aexp {% return [$1]}


op :: { Expr }   -- used in infix decls
op : varop {% return $ Var $1 }


infixop :: { Expr }
infixop
    : fexp {% return $1 }
    | infixop op fexp {% return $ joinOper $1 $2 $3 }

exp :: { Expr }
exp : infixop {% return $1}



{


joinOper :: Expr -> Expr -> Expr -> Expr
joinOper (OpChain ops) op r = OpChain $ ops ++ [op, r]
joinOper l op r = OpChain [l, op, r]


-- Take a list of expressions and if the list of 1 long, it is simpy the first value,
-- if it is any more, create a tuple
makeTupleOrExpr :: [Expr] -> Expr
makeTupleOrExpr [e] = Paren e
makeTupleOrExpr es@(_:_) = TupleLit es




mkLambda :: [Pat] -> Expr -> ParseResult Expr
mkLambda args bod = if not allVars
                        then fail "Lambda literals must have all variable name arguments"
                        else return $ Lambda args bod
                    where allVars = all (patternIsVar) args


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
