{
module Radon.Parser.Lexer (lexString) where
}


%wrapper "posn"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

radon :-

  $white+				                  ;
  "--".*                          ;
  let                             { infuse $ \s -> Let }
  in                              { infuse $ \s -> In }
  $digit+                         { infuse $ \s -> Int (read s) }
  [\=\+\-\*\/\(\)]+               { infuse $ makeOper }
  $alpha [$alpha $digit \_ \']*		{ infuse $ makeIdent }




{



infuse :: (String -> TokenData) -> (AlexPosn -> String -> Token)
infuse f = (\pos s -> (Tok pos (f s)))


-- | Convert operators into either a variable operator or a special operator like =
makeOper :: String -> TokenData
makeOper "=" = Equals
makeOper s = Oper s

-- | convert an identifier to either a variable or one of the special ident tokens
makeIdent :: String -> TokenData
makeIdent "let" = Let
makeIdent "in" = In
makeIdent s = Ident s

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
  = Let
  | In
  | Equals
  | Oper String
  | Ident String
  | Int Int
  deriving (Eq, Show)

-- | run the lexer over a set of strings
lexString :: String -> [Token]
lexString = alexScanTokens
}
