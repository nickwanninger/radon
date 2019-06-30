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
