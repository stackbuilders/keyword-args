{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Console.GetOpt

import System.Environment (getArgs, getProgName)
import System.Exit (ExitCode(..), exitWith, exitSuccess)
import System.IO.Error (ioError)

import Paths_keyword_args (version)
import Data.Maybe (fromMaybe)

import Data.Version (showVersion)

import Data.ByteString.Lazy.Char8 (unpack)

import Data.KeywordArgs.Parse

import Text.Parsec
import Text.ParserCombinators.Parsec.Error (ParseError)

import Data.Csv (encode)

data Options = Options
    { optShowVersion :: Bool
    , optShowHelp    :: Bool
    } deriving Show

defaultOptions = Options
    { optShowVersion = False
    , optShowHelp    = False
    }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['V'] ["version"] (NoArg (\opts -> opts { optShowVersion = True }))
    "show version number"

  , Option ['h'] ["help"]    (NoArg (\opts -> opts { optShowHelp = True }))
    "show help"
  ]

showHelp = do
  prg <- getProgName
  putStrLn (usageInfo prg options)
  exitSuccess

headerMessage :: String -> String
headerMessage progName = "Usage: " ++ progName ++ " [OPTION...]"

lintOpts :: [String] -> IO (Options, [String])
lintOpts argv = do
  name <- getProgName

  case getOpt Permute options argv of
    (o, n, []  ) -> return (foldl (flip id) defaultOptions o, n)
    (_, _, errs) ->
      ioError (userError (concat errs ++ usageInfo (headerMessage name) options))

parseConfig :: String -> Either ParseError [(String, String)]
parseConfig = parse configParser "(unknown)"

runCheck :: IO ()
runCheck = do

  f <- getContents

  case parseConfig f of
    Left e -> do
      ioError $ userError $ "Parse error: " ++ show e

    Right config -> putStr $ unpack $ encode config


main :: IO ()
main = do
  opts <- getArgs >>= lintOpts

  name <- getProgName

  if optShowHelp $ fst opts
  then
    putStrLn $ usageInfo (headerMessage name) options
  else
    if optShowVersion $ fst opts then
      putStrLn $ name ++ " " ++ showVersion version ++
      " (C) 2015 Stack Builders Inc."
    else
      runCheck
