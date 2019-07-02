-- vim: filetype=happy
{
module Radon.Parser.Parser (parseModule) where


import Radon.Parser.Lexer (Token(..), TokenData(..), lexString)
}


%name parseModule module
%tokentype { Token }
%error { parseError }


%token
	'let' {Tok _ (TLet)}


%%


module : 'let' {}

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

}
