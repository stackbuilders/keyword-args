{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.KeywordArgs.ParseSpec (spec) where

import Data.KeywordArgs.Parse (configParser)

import Test.Hspec (it, describe, shouldBe, Spec)

import Text.Parsec (ParseError, parse, errorPos)
import Text.Parsec.Pos (newPos)
import Text.ParserCombinators.Parsec.Error(errorMessages)


spec :: Spec
spec =
  describe "parse" $ do
    it "parses a config line with multiple arguments" $
      parseConfig "Keyword With My Arguments" `shouldBe`
      Right [("Keyword", ["With", "My", "Arguments"])]

    it "allows arguments in quotes" $
      parseConfig "Keyword With \"My Arguments\"" `shouldBe`
      Right [("Keyword", ["With", "My Arguments"])]

    it "parses a simple multi-line configuration" $ do
      let f = unlines [ "# Some useful comment", "PermitEmptyPasswords no" ]
      parseConfig f `shouldBe` Right [ ("PermitEmptyPasswords", ["no"]) ]

    it "allows leading whitespace on comment lines" $ do
      let f = unlines [ "   # Some useful comment", "PermitEmptyPasswords no" ]
      parseConfig f `shouldBe` Right [ ("PermitEmptyPasswords", ["no"]) ]

    it "allows comments after lines with configuration options" $
      parseConfig "Key-One 1 # test" `shouldBe` Right [ ("Key-One", ["1"]) ]

    it "allows comments after quoted values" $
      parseConfig "Key-One \"One value\" # test" `shouldBe`
        Right [ ("Key-One", ["One value"]) ]

    it "parses multiple unquoted arguments followed by a comment" $
      parseConfig "one two three # comment" `shouldBe`
        Right [("one", ["two", "three"])]

    it "parses quoted and unquoted args followed by a comment" $
      parseConfig "one \"two\" three # comment" `shouldBe`
        Right [("one", ["two", "three"])]

    it "parses an arg list ending in an unquoted arg with no comment" $
      parseConfig "one \"two\" three" `shouldBe`
        Right [("one", ["two", "three" ])]

    it "parses an arg list ending in a quoted arg with no comment" $
      parseConfig "one two \"three\"" `shouldBe`
        Right [("one", ["two", "three"])]

    it "allows non-alphanumeric characters in keys" $
      parseConfig "Key-One Value" `shouldBe` Right [ ("Key-One", ["Value"]) ]

    it "allows tab separators between keys and values" $
      parseConfig "Key-One\tValue" `shouldBe` Right [ ("Key-One", ["Value"]) ]

    it "allows comments after quoted values, followed by a newline" $
      parseConfig "Key-One \"One value\" # test\n" `shouldBe`
        Right [ ("Key-One", ["One value"]) ]

    it "allows leading spaces for configuration options" $
      parseConfig "  Key 1" `shouldBe` Right [ ("Key", ["1"]) ]

    it "parses strings in quotes" $
      parseConfig "Key \"My Val\"" `shouldBe` Right [ ("Key", ["My Val"]) ]

    it "parses until the end of a line" $ do
      let f = unlines [ "PermitEmptyPasswords no", "Test_Protocol  1" ]

      parseConfig f `shouldBe` Right [ ("PermitEmptyPasswords", ["no"])
                                     , ("Test_Protocol", ["1"]) ]

    it "doesn't allow keywords with no arguments" $
      isLeft (parseConfig "Key") `shouldBe` True

    it "doesn't allow keywords followed by a space and no args" $
      isLeft (parseConfig "Key ") `shouldBe` True

    it "doesn't allow a line with a missing argument, followed by a valid line" $
      isLeft (parseConfig $ unlines ["Key", "Key2 value"]) `shouldBe` True

    it "doesn't allow an empty argument in quotes" $
      isLeft (parseConfig "Key \"\"") `shouldBe` True

    it "doesn't parse when an invalid line is embedded in a file" $ do
      let f = unlines [ "# Some useful comment", "PermitEmptyPasswords" ]
      isLeft (parseConfig f) `shouldBe` True

    it "parses a value until a hashmark" $
      parseConfig "Key something#else" `shouldBe` Right [("Key", ["something"])]

    it "doesn't accept a value containing a Hash character" $
      isLeft (parseConfig "Key #something") `shouldBe` True

    it "doesn't accept a value containing a Hash character in quotes" $
      isLeft (parseConfig "Key \"some#thing\"") `shouldBe` True

    it "returns accurate source position for invalid quoted strings" $ do
      let f   = unlines [ "PermitEmptyPasswords \"foo#\"", "Test_Protocol 1" ]
          pos = newPos "(unknown)" 1 26
          res = errorPos $ fromLeft $ parseConfig f

      res `shouldBe` pos

    it "allows blank lines" $
      parseConfig "Key val\n\nKey2 val2" `shouldBe`
        Right [("Key", ["val"]), ("Key2", ["val2"])]


instance Eq ParseError where
  a == b = errorMessages a == errorMessages b

parseConfig :: String -> Either ParseError [(String, [String])]
parseConfig = parse configParser "(unknown)"

isLeft :: Either a b -> Bool
isLeft ( Left _ ) = True
isLeft _          = False

fromLeft :: Either ParseError b -> ParseError
fromLeft (Left p) = p
fromLeft (Right _) = undefined
