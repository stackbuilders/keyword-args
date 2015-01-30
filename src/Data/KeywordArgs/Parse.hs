module Data.KeywordArgs.Parse (configParser) where

import Data.Maybe (catMaybes)
import Data.Char (isSpace)

import Text.Parsec ((<|>), many, try, manyTill, char, anyChar, many1, satisfy)
import Text.Parsec.String (Parser)

import Text.ParserCombinators.Parsec.Prim hiding (try)
import Text.ParserCombinators.Parsec.Char (string, space, tab, newline,
                                           alphaNum, oneOf)

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
configurationOption = do
  many space

  keyword <- manyTill1 anyChar keywordArgSeparator
  val     <- value

  return (keyword, val)

value :: Parser String
value =
  unquotedValue <|> quotedValue

  where
    endOfOption   = endOfLineOrInput <|> comment

    quotedValue   = quote
                    *> manyTill1 (satisfy isValidValueChar) quote
                    <* endOfOption

    unquotedValue = manyTill1 (satisfy isValidValueChar) endOfOption

comment :: Parser ()
comment =
  try (many space *> char '#')
  *> manyTill anyChar endOfLineOrInput
  *> return ()

endOfLineOrInput :: Parser ()
endOfLineOrInput = newline *> return () <|> eof

manyTill1 :: GenParser tok st a -> GenParser tok st end -> GenParser tok st [a]
manyTill1 p end = liftM2 (:) p (manyTill p end)

isValidValueChar :: Char -> Bool
isValidValueChar c =  c /= '#' && c /= '"'

keywordArgSeparator :: Parser ()
keywordArgSeparator = many1 (oneOf "\t ") *> return ()

quote :: Parser Char
quote = char '"'
