-- vim: filetype=happy
{
module Radon.Parser.Parser (parseModule) where

import Radon.Parser.Lexer (Token(..), TokenData(..), lexString)

import Radon.Syntax
}


%name parseModule module
%tokentype { Token }
%error { parseError }

%monad { ParseResult } { >>= } { return }

%token
	'let' {Tok _ (TLet)}
	';'   {Tok _ (TSemi)}
	'='   {Tok _ (TEquals)}
	var   {Tok _ (TIdent $$)}


%%

  -- a module is a grouping of top level expressions, like imports,
	-- declarations, types, etc.
module :: { RaModule }
       : topdecls    {% return $ RaModule {modName = Nothing, modStmts = Just (reverse $1)} }

	-- Zero or more semicolons
semis : semis ';'   {}
      | {- empty -} {}


topdecls :: { [TopDecl] }
topdecls : topdecls topdecl        {% return ($2:$1) }
         | {- empty -}             {% return [] }


topdecl :: { TopDecl }
topdecl : topLevelBinding { $1 }


bindArgs :: { [String] }
         : bindArgs var {% return ($2:$1)}
         | {- empty -}  {% return [] }

topLevelBinding :: { TopDecl }
                : var bindArgs '=' aexpr {% return $ (Binding $1 (reverse $2) $4) }


aexpr :: { Expr }
      : var  {% return $ Var $1 }
{


-- parseError :: [Token] -> a
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
