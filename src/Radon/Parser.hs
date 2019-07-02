module Radon.Parser
    ( parseString
    ) where

import Radon.Parser.Lexer (lexString)
import Radon.Parser.Parser (parseModule)

parseString = parseModule . lexString
