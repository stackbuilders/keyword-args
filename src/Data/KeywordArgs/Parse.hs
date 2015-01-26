module Data.KeywordArgs.Parse (configParser) where

import Data.Maybe (catMaybes)
import Data.Char (isSpace)

import Text.Parsec ((<|>), many, try, manyTill, char, anyChar, many1, satisfy)
import Text.Parsec.String (Parser)

import Text.ParserCombinators.Parsec.Char (string, space, tab, newline,
                                           alphaNum)

import Text.Parsec.Combinator (eof)

import Control.Applicative ((<*), (*>), (<$>))

configParser :: Parser [(String, String)]
configParser = catMaybes <$> many line


configurationOption :: Parser (String, String)
configurationOption = do
  many space

  option <- many1 (satisfy (not . isSpace))

  many1 space

  let
    endOfOption   = comment <|> endOfLineOrInput
    quotedValue   = char '"' *> manyTill anyChar (try (char '"')) <* endOfOption
    unquotedValue = manyTill anyChar endOfOption

  value <- quotedValue <|> unquotedValue

  return (option, value)

comment :: Parser ()
comment =
  -- ^ use `try` to avoid consuming leading spaces if no match
  try (many space *> char '#')
  *> manyTill anyChar endOfLineOrInput
  *> return ()

endOfLineOrInput :: Parser ()
endOfLineOrInput = newline *> return () <|> eof

line :: Parser (Maybe (String, String))
line = comment *> return Nothing <|> Just <$> configurationOption
