module Data.KeywordArgs.ParseSpec (spec) where

import Test.Hspec

import Text.Parsec
import Text.Parsec.String

import Data.KeywordArgs.Parse

import Text.ParserCombinators.Parsec.Error(ParseError, Message(..),
                                           errorMessages, messageEq)

instance Eq ParseError where
  a == b = errorMessages a == errorMessages b

parseConfig :: String -> Either ParseError [(String, String)]
parseConfig = parse configParser "(unknown)"

isLeft :: Either a b -> Bool
isLeft ( Left _ ) = True
isLeft _          = False

spec :: Spec
spec =
  describe "parse" $ do
    it "parses a simple multi-line configuration" $ do
      let f = unlines [ "# Some useful comment", "PermitEmptyPasswords no" ]
      parseConfig f `shouldBe` Right [ ("PermitEmptyPasswords", "no") ]

    it "allows leading whitespace on comment lines" $ do
      let f = unlines [ "   # Some useful comment", "PermitEmptyPasswords no" ]
      parseConfig f `shouldBe` Right [ ("PermitEmptyPasswords", "no") ]

    it "allows comments after lines with configuration options" $
      parseConfig "Key-One 1 # test" `shouldBe` Right [ ("Key-One", "1") ]

    it "allows comments after quoted values" $
      parseConfig "Key-One \"One value\" # test" `shouldBe`
        Right [ ("Key-One", "One value") ]

    it "allows non-alphanumeric characters in keys" $
      parseConfig "Key-One Value" `shouldBe` Right [ ("Key-One", "Value") ]

    it "allows comments after quoted values, followed by a newline" $
      parseConfig "Key-One \"One value\" # test\n" `shouldBe`
        Right [ ("Key-One", "One value") ]

    it "allows leading spaces for configuration options" $
      parseConfig "  Key 1" `shouldBe` Right [ ("Key", "1") ]

    it "parses strings in quotes" $
      parseConfig "Key \"My Val\"" `shouldBe` Right [ ("Key", "My Val") ]

    it "parses until the end of a line" $ do
      let f = unlines [ "PermitEmptyPasswords no", "Test_Protocol  1" ]

      parseConfig f `shouldBe` Right [ ("PermitEmptyPasswords", "no")
                                     , ("Test_Protocol", "1") ]

    it "should not parse when there is no argument" $
      isLeft (parseConfig "Key") `shouldBe` True

    it "should not parse when there is no arg after a space" $
      isLeft (parseConfig "Key ") `shouldBe` True

    it "should not allow an empty argument in quotes" $
      isLeft (parseConfig "Key \"\"") `shouldBe` True

    it "should not parse when an invalid line is embedded in a file" $ do
      let f = unlines [ "# Some useful comment", "PermitEmptyPasswords" ]
      isLeft (parseConfig f) `shouldBe` True

    it "should not accept a value containing a Hash character" $
      isLeft (parseConfig "Key #something") `shouldBe` True

    it "should not accept a value containing a Hash character in quotes" $
      isLeft (parseConfig "Key \"some#thing\"") `shouldBe` True
