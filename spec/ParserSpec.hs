module ParserSpec (spec) where

import Test.Hspec
import Lib
import AST

spec :: Spec
spec = do
  describe "parseRuby" $ do
    it "parses 0" $ do
      parseRuby "0" `shouldBe` (Right SZero)
