module Main where

import Data.List

import System.Console.GetOpt

import Control.Monad
import Data.Maybe
import System.Console.ANSI

-- For getArgs
import System.Environment
import System.Exit

import Radon.Parser
import Radon.Parser.Lexer

version :: String
version = "0.0.1"

data Options =
    Options
        { optHelp :: Bool
        , optVersion :: Bool
        }
    deriving (Show)

defaultOptions :: Options
defaultOptions = Options {optHelp = False, optVersion = False}

options :: [OptDescr (Options -> Options)]
options =
    [ Option
          ['v']
          ["version"]
          (NoArg (\opts -> opts {optVersion = True}))
          "Show the version of the compiler"
    , Option
          ['h']
          ["version"]
          (NoArg (\opts -> opts {optHelp = True}))
          "Show help"
    ]

usageMsg :: String
usageMsg = usageInfo header options
  where
    header = "Usage: radc [OPTIONS...] filename"

usageErr :: String -> IO ()
usageErr msg = do
    putStrLn ""
    setSGR [SetColor Foreground Vivid Red]
    putStrLn $ "Fatal error: " <> msg
    setSGR [Reset]
    usage

usage :: IO ()
usage = do
    putStrLn usageMsg
    exitSuccess

compilerOptions :: [String] -> IO (Options, Maybe String)
compilerOptions argv =
    case getOpt Permute options argv of
        (o, [n], []) -> return (foldl (flip id) defaultOptions o, Just n)
        (o, _, []) -> return (foldl (flip id) defaultOptions o, Nothing)
        (_, _, errs) -> ioError $ userError $ concat errs ++ usageMsg

main :: IO ()
main = do
    argv <- getArgs
    (opts, fname) <- compilerOptions argv
    -- handle printing version when the user provides `-v`
    when (optVersion opts) $ do
        putStrLn version
        exitSuccess
    -- handle printing the help when the user provides `-h`
    when (optHelp opts) usage
    -- At this point, we need to check if the user provides a file or not
    case fname of
        Nothing -> usageErr "No file provided"
        Just fname' -> do
            source <- readFile fname'
            putStrLn $ show $ lexString source
            -- putStrLn $ show $ parseString source fname'
