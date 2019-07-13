-- vim: filetype=haskell

{
module Radon.Parser.Lexer (Token(..), TokenData(..), lexString) where
}


%wrapper "posn"

$digit  = 0-9

$small  = [a-z] -- any ascii lowercase char
$large  = [A-Z] -- any ascii lowercase char

$idchar    = [$small $large $digit \' \_]


$operator = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~\:]

radon :-

  $white+				          ;
  "--".*                          ;
  $digit+ \. $digit+              { infuse $ \s -> TDouble (read s) }
  $digit+                         { infuse $ \s -> TInt (read s) }
  [\(\)\,\;\[\]\`\{\}]            { infuse $ makeOper } -- we have to handle these grouping operators specially
                                                              -- because they need to be one char long each
  $operator+                      { infuse $ makeOper }
  $small $idchar*                 { infuse $ makeIdent }

  _                               { infuse $ \s -> TUnder }


{

-- Abstract the real token data behind a datatype that holds positions
data Token = Tok AlexPosn TokenData
  deriving (Eq)

instance Show Token where
  show (Tok (AlexPn _ line col) tok) = "(" ++ (show line) ++ ":" ++ (show col) ++ " " ++ (show tok) ++ ")"



tokLine :: Token -> Int
tokLine (Tok (AlexPn _ line _) _) = line


tokCol :: Token -> Int
tokCol (Tok (AlexPn _ _ col) _) = col

-- The token type:
data TokenData
    = TLet -- special operators
    | TIn
    | TIf
    | TThen
    | TElse
    | TDef
    | TEquals
    | TOf
    -- Variable-esque things
    | TOper String
    | TIdent String
    -- Literals
    | TDouble Double
    | TInt Integer
    -- Grouping symbols
    | TLParen -- Round parenthesis
    | TRParen
    | TLSquare -- Square brackets
    | TRSquare
    | TLCurly -- Curly Brackets
    | TRCurly
    -- reserved operators
    | TSemi -- ;
    | TColon -- :
    | TDColon -- ::
    | TLam -- backslash
    | TDot -- .
    | TPipe -- |
    | TError
    | TComma
    | TUnder
    | TArrow
    deriving (Eq, Show)

-- | infuse lets the token definitions above be easily converted into
infuse :: (String -> TokenData) -> (AlexPosn -> String -> Token)
infuse f = (\pos s -> (Tok pos (f s)))


-- | Convert operators into either a variable operator or a special operator like =
makeOper :: String -> TokenData
makeOper "=" = TEquals
-- Parenthesis
makeOper "(" = TLParen
makeOper ")" = TRParen
-- Square brackets
makeOper "[" = TLSquare
makeOper "]" = TRSquare
-- Square brackets
makeOper "{" = TLCurly
makeOper "}" = TRCurly

makeOper ";"  = TSemi
makeOper ":"  = TColon
makeOper "::" = TDColon
makeOper "\\" = TLam
makeOper "|"  = TPipe
makeOper "."  = TDot
makeOper ","  = TComma
makeOper "->" = TArrow

-- Fall back and make a new generic oper
makeOper s = TOper s

-- | convert an identifier to either a variable or one of the special ident tokens
makeIdent :: String -> TokenData
makeIdent "def" = TDef
makeIdent "let" = TLet
makeIdent "in" = TIn
makeIdent "if" = TIf
makeIdent "then" = TThen
makeIdent "else" = TElse
makeIdent "of" = TOf
makeIdent s = TIdent s


-- | run the lexer over a set of strings
lexString :: String -> [Token]
lexString = alexScanTokens
}
