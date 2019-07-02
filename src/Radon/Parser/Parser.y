-- vim: filetype=happy
{
module Radon.Parser.Parser (parseModule, ParseResult(..)) where

import Radon.Parser.Lexer (Token(..), TokenData(..), lexString)

import Radon.Syntax
}


%name parseModule module
%tokentype { Token }
%error { parseError }

-- %monad { ParseResult } { >>= } { return }

%token
	'let' {Tok _ (TLet)}
	';'   {Tok _ (TSemi)}
	'='   {Tok _ (TEquals)}
	var   {Tok _ (TIdent $$)}


%%

  -- a module is a grouping of top level expressions, like imports,
	-- declarations, types, etc.
module :: { RaModule }
       : topdecls    {% RaModule {modName = Nothing, modStmts = Just $1} }

	-- Zero or more semicolons
semis : semis ';'   {}
      | {- empty -} {}

topdecls :: { [TopDecl] }
         : topdecls topdecl        {% reverse ($2:$1) }
         | {- empty -}             { [] }

topdecl :: { TopDecl }
        : topLevelBinding { $1 }


bindingArgs :: { [String] }
            : bindingArgs var {% ($2:$1)}
            | {- empty -} { [] }

topLevelBinding :: { TopDecl }
                : var bindingArgs '=' aexpr {% (Binding $1 $2 $4) }


aexpr :: { Expr }
      : var  {% Var $1 }
{

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


parseError :: [Token] -> a
parseError _ = error "Parse error"

}
