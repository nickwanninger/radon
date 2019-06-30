module Radon.Types where


import Data.ByteString
import Data.Data

data Literal
    = LitInt Integer Type
    | LitDouble Rational
    | LitString ByteString
    deriving (Show, Eq)


type Kind = Type

data Type
    = TyVar String -- Type variables
    | FTy [Type] Type --
    deriving (Show, Eq)
