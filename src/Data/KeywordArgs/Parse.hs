module Data.KeywordArgs.Parse (configParser) where

import Data.Maybe (catMaybes)

import Text.Parsec
  ((<|>), many, try, lookAhead, manyTill, char, anyChar, many1)

import Text.Parsec.String (Parser)

import Text.ParserCombinators.Parsec.Prim (GenParser)

import Text.ParserCombinators.Parsec.Char (space, newline, oneOf, noneOf)

import Control.Monad (liftM2)

import Text.Parsec.Combinator (eof)

import Control.Applicative ((<*), (*>), (<$>))

configParser :: Parser [(String, [String])]
configParser = catMaybes <$> many lineWithArguments

lineWithArguments :: Parser (Maybe (String, [String]))
lineWithArguments =
  comment *> return Nothing
  <|> Just <$> configurationOptionWithArguments

configurationOptionWithArguments :: Parser (String, [String])
configurationOptionWithArguments = do
  _ <- many space

  keyword   <- manyTill1 (noneOf "\n") keywordArgSeparator

  arguments <- argumentParser

  return (keyword, arguments)

argumentParser :: Parser [String]
argumentParser =
  manyTill1 (try quotedArgument <|> try unquotedArgument)
  (try endOfLineOrInput <|> try comment)

quotedArgument :: Parser String
quotedArgument =
  many (oneOf " \t") *> quote *> manyTill1 (noneOf "#") quote

unquotedArgument :: Parser String
unquotedArgument =
  many (oneOf " \t") *> many1 (noneOf "\" \t\n#") <*
  (try comment <|> try (oneOf " \t") *> return () <|>
   lookAhead (try endOfLineOrInput))

comment :: Parser ()
comment =
  try (many (oneOf " \t") *> char '#')
  *> manyTill anyChar endOfLineOrInput
  *> return ()

endOfLineOrInput :: Parser ()
endOfLineOrInput = newline *> return () <|> eof

manyTill1 :: GenParser tok st a -> GenParser tok st end -> GenParser tok st [a]
manyTill1 p end = liftM2 (:) p (manyTill p end)

keywordArgSeparator :: Parser ()
keywordArgSeparator = many1 (oneOf "\t ") *> return ()

quote :: Parser Char
quote = char '"'
