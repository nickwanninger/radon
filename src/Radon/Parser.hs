module Radon.Parser
    ( parseString, parseExpString
    ) where

import Radon.Parser.Lexer (lexString)
import Radon.Parser.Parser (parseModule, parseExpr)

parseString = parseModule . lexString

parseExpString = parseExpr . lexString
