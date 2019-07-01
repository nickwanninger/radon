module Main where

import Data.List

import System.Console.GetOpt

import Control.Monad
import Data.Maybe
import Radon (parseString)
-- For getArgs
import System.Environment
import System.Exit

version :: String
version = "0.0.1"

-- | What action to do
data Action =
    BuildAction
    deriving (Show)

data Options =
    Options
        { optHelp :: Bool
        , optVersion :: Bool
        , optAction :: Action
        }
    deriving (Show)

defaultOptions :: Options
defaultOptions =
    Options {optHelp = False, optVersion = False, optAction = BuildAction}

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
    , Option
          ['c']
          ["build"]
          (NoArg (\opts -> opts {optAction = BuildAction}))
          "Build a file"
    ]


usage :: String
usage = usageInfo header options
    where header = "Usage: radc [OPTIONS...] filename\n\n"

compilerOptions :: [String] -> IO (Options, Maybe String)
compilerOptions argv =
    case getOpt Permute options argv of
        (o, [n], []) -> return (foldl (flip id) defaultOptions o, Just n)
        (o, _, []) -> return (foldl (flip id) defaultOptions o, Nothing)
        (_, _, errs) ->
            ioError $ userError $ concat errs ++ usage
  where
    exactlyone = "There must be exactly one rad file given\n\n"

main :: IO ()
main = do
    argv <- getArgs
    (opts, fname) <- compilerOptions argv
    when (optVersion opts) $ do
        putStrLn version
        exitSuccess

    when (optHelp opts) $ do
        putStrLn $ usage
        exitSuccess
    print opts
    let fname' = fromJust fname
    source <- readFile fname'
    putStrLn $ show $ parseString source fname'
