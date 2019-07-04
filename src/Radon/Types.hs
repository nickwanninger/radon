module Radon.Types where


import Data.ByteString
import Data.Data

type Kind = Type

data Type
    = TyVar String -- Type variables
    | FTy [Type] Type -- Function types are simply a comma separated list, an arrow, and another type.
    deriving (Show, Eq)
