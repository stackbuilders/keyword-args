module Data.KeywordArgs.Parse (configParser) where

import Data.Maybe (catMaybes)

import Text.Parsec ((<|>), many, try, manyTill, char, anyChar, many1)
import Text.Parsec.String (Parser)

import Text.ParserCombinators.Parsec.Prim (GenParser)
import Text.ParserCombinators.Parsec.Char (space, newline, oneOf, noneOf)

import Control.Monad (liftM2)

import Text.Parsec.Combinator (eof)

import Control.Applicative ((<*), (*>), (<$>))

-- | Returns a parser for input of a keyword, followed by a space,
-- followed by one or more arguments. Any comments (text preceeded
-- by a '#') will be ignored until the end of the line.
configParser :: Parser [(String, String)]
configParser = catMaybes <$> many line

line :: Parser (Maybe (String, String))
line = comment *> return Nothing <|> Just <$> configurationOption

configurationOption :: Parser (String, String)
configurationOption =
  many space *> liftM2 (,) (manyTill1 anyChar keywordArgSeparator) value

value :: Parser String
value =
  unquotedValue <|> quotedValue

  where
    endOfOption   = endOfLineOrInput <|> comment

    quotedValue   = quote
                    *> manyTill1 argumentChar quote
                    <* endOfOption

    unquotedValue = manyTill1 argumentChar endOfOption

comment :: Parser ()
comment =
  try (many space *> char '#')
  *> manyTill anyChar endOfLineOrInput
  *> return ()

endOfLineOrInput :: Parser ()
endOfLineOrInput = newline *> return () <|> eof

manyTill1 :: GenParser tok st a -> GenParser tok st end -> GenParser tok st [a]
manyTill1 p end = liftM2 (:) p (manyTill p end)

argumentChar :: Parser Char
argumentChar =  noneOf "#\""

keywordArgSeparator :: Parser ()
keywordArgSeparator = many1 (oneOf "\t ") *> return ()

quote :: Parser Char
quote = char '"'
