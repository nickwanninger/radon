module Main where

-- For getArgs
import System.Environment
import Data.List

import Radon (parseString)

commaSep = intercalate ", "


main :: IO ()
main = do
    args <- getArgs
    let input = "example.rad"
    source <- readFile input
    putStrLn $ show $ parseString source input
