module LexerSpec (spec) where

import Test.Hspec
import Data.List (intercalate)
import Lexer
import Codec.Binary.UTF8.String (encode)

spec :: Spec
spec = describe "Lexer:" $ do
  -- it "comments" $ do
  --   testLex "# abcdef " `shouldBe` "[CommentToken]"

  it "integers" $ do
    testLex "123"    `shouldBe` (TINTEGER 123)

  it "octal integers" $ do
    testLex "037"    `shouldBe` (TINTEGER 037)

  it "hex integers" $ do
    testLex "0xab"   `shouldBe` (TINTEGER 0xab)

  it "floats" $ do
    testLex "0.5" `shouldBe` (TFLOAT 0.5)

  -- it "invalid numbers" $ do
  --   testLex "089"    `shouldBe` "[TINTEGER 0,TINTEGER 89]"
  --   testLex "0xGh"   `shouldBe` "[TINTEGER 0,IdentifierToken 'xGx']"

  it "symbol" $ do
    testLex ":cat"  `shouldBe` (TSYMBOL "cat")

  it "string double quote" $ do
    testLex "\"dog\""   `shouldBe` (TSTRING "dog")

  it "string single quote" $ do
    testLex "'cat'"  `shouldBe` (TSTRING "cat")

  it "identifier" $ do
    testLex "var"  `shouldBe` (TIDENTIFIER "var")

  it "gvar" $ do
    testLex "$var"  `shouldBe` (TGVAR "$var")

  it "ivar" $ do
    testLex "@var"  `shouldBe` (TIVAR "@var")

  it "tconstant" $ do
    testLex "Var"  `shouldBe` (TCONSTANT "Var")

  it "tcvar" $ do
    testLex "@@var"  `shouldBe` (TCVAR "@@var")

  it "KSELF" $ do
    testLex "self"  `shouldBe` KSELF

  it "KTRUE" $ do
    testLex "true"  `shouldBe` KTRUE

  it "KNIL" $ do
    testLex "nil"  `shouldBe` KNIL

  -- it "strings with escape chars" $ do
  --     testLex "'\t'"   `shouldBe` "[StringToken '\t']"
  --     testLex "'\\n'"  `shouldBe` "[StringToken '\\n']"
  --     testLex "'\\\\n'"   `shouldBe` "[StringToken '\\\\n']"
  --     testLex "'\\\\'" `shouldBe` "[StringToken '\\\\']"
  --     testLex "'\\0'"  `shouldBe` "[StringToken '\\0']"
  --     testLex "'\\12'" `shouldBe` "[StringToken '\\12']"
  --     testLex "'\\s'"   `shouldBe` "[StringToken '\\s']"
  --     testLex "'\\-'"   `shouldBe` "[StringToken '\\-']"

  -- it "strings with non-escaped chars" $
  --     testLex "'\\/'"  `shouldBe` "[StringToken '\\/']"

  -- it "strings with escaped quotes" $ do
  --     testLex "'\"'"   `shouldBe` "[StringToken '\"']"
  --     testLex "\"\\\"\""  `shouldBe` "[StringToken \"\\\\\"\"]"
  --     testLex "'\\\''" `shouldBe` "[StringToken '\\\\'']"
  --     testLex "'\"'"   `shouldBe` "[StringToken '\"']"
  --     testLex "\"\\'\""   `shouldBe` "[StringToken \"\\'\"]"

  -- it "spread token" $ do
  --     testLex "...a" `shouldBe` "[SpreadToken,IdentifierToken 'a']"

  -- it "assignment" $ do
  --     testLex "x=1"    `shouldBe` "[IdentifierToken 'x',SimpleAssignToken,DecimalToken 1]"
  --     testLex "x=1\ny=2"  `shouldBe` "[IdentifierToken 'x',SimpleAssignToken,DecimalToken 1,WsToken,IdentifierToken 'y',SimpleAssignToken,DecimalToken 2]"

  -- it "break/continue/return" $ do
  --     testLex "break\nx=1"  `shouldBe` "[BreakToken,WsToken,IdentifierToken 'x',SimpleAssignToken,DecimalToken 1]"
  --     testLex "continue\nx=1"  `shouldBe` "[ContinueToken,WsToken,IdentifierToken 'x',SimpleAssignToken,DecimalToken 1]"
  --     testLex "return\nx=1" `shouldBe` "[ReturnToken,WsToken,IdentifierToken 'x',SimpleAssignToken,DecimalToken 1]"


testLex :: String -> Token
testLex = either undefined id . evalP (lexer return) . encode
