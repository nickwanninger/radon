module Radon.Types where


import Data.ByteString
import Data.Data

type Kind = Type

data Type
    = TyVar String -- Type variables
    | FTy [Type] Type --
    deriving (Show, Eq)
