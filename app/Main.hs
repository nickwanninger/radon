{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}

-- The main entrypoint for the radon compiler/interpreter
module Main where

import Data.List

import Control.Monad
import qualified Data.Map as M
import Data.Maybe
import System.Console.ANSI
import System.Console.GetOpt
import System.Environment
import System.Exit

import Radon
import Radon.Parser.Parser
import Radon.Syntax
import Radon.Rename

import Control.Monad.Except
import qualified Data.ByteString.Char8 as BS

import Foreign
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.ForeignPtr.Unsafe
import System.IO.Unsafe


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

radon_main :: [String] -> IO ()
radon_main argv = do
    (opts, fname) <- compilerOptions argv
    -- handle printing version when the user provides `-v`
    when (optVersion opts) $ do
        putStrLn version -- just print the version and exit
        exitSuccess
    -- handle printing the help when the user provides `-h`
    when (optHelp opts) usage -- print the usage, which exits
    -- At this point, we need to check if the user provides a file or not
    case fname of
        Nothing -> usageErr "No file provided"
        Just fname' -> do
            source <- readFile fname'
            case parseString source of
                Success mod -> putStrLn $ show $ runRename mod
                err -> putStrLn $ show err

main :: IO ()
main = do
    args <- getArgs
    radon_main args

-- foo === bar (they are the same)
foo = [x*2 | x<-[1..10], odd x]
bar = do
    x <- [1..]
    guard (odd x)
    return (x * 2)
