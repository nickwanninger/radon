{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Main
    ( main
    ) where

import Control.Applicative hiding (some)
import Data.List (intercalate)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec (some)
import Text.Megaparsec.Char

data SyntaxError
    = TrivialWithLocation
          [String] -- position stack
          (Maybe (ErrorItem Char))
          (Set (ErrorItem Char))
    | FancyWithLocation
          [String] -- position stack
          (ErrorFancy Void) -- Void, because we do not want to allow to nest Customs
    deriving (Eq, Ord, Show)

instance ShowErrorComponent SyntaxError where
    showErrorComponent (TrivialWithLocation stack us es) =
        parseErrorTextPretty (TrivialError @Char @Void undefined Nothing es) ++
        showPosStack stack
    showErrorComponent (FancyWithLocation stack cs) =
        showErrorComponent cs ++ showPosStack stack

showPosStack :: [String] -> String
showPosStack = intercalate ", " . fmap ("in " ++)

type Parser = Parsec SyntaxError Text

inside :: String -> Parser a -> Parser a
inside location p = do
    r <- observing p
    case r of
        Left (TrivialError _ us es) ->
            fancyFailure . Set.singleton . ErrorCustom $
            TrivialWithLocation [location] us es
        Left (FancyError _ xs) -> do
            let f (ErrorFail msg) =
                    ErrorCustom $ FancyWithLocation [location] (ErrorFail msg)
                f (ErrorIndentation ord rlvl alvl) =
                    ErrorCustom $
                    FancyWithLocation
                        [location]
                        (ErrorIndentation ord rlvl alvl)
                f (ErrorCustom (TrivialWithLocation ps us es)) =
                    ErrorCustom $ TrivialWithLocation (location : ps) us es
                f (ErrorCustom (FancyWithLocation ps cs)) =
                    ErrorCustom $ FancyWithLocation (location : ps) cs
            fancyFailure (Set.map f xs)
        Right x -> return x

myParser :: Parser String
myParser = some (char 'a') *> some (char 'b')

main :: IO ()
main = do
    parseTest (inside "foo" myParser) "aaacc"
    parseTest (inside "foo" $ inside "bar" myParser) "aaacc"
{-
module Main where

import Data.List
-- For getArgs
import System.Environment

import Radon (parseString)

commaSep = intercalate ", "

main :: IO ()
main = do
    args <- getArgs
    let input = "example.rad"
    source <- readFile input
    putStrLn $ show $ parseString source input
-}
