module ParserSpec (spec) where

import Test.Hspec
import Lib
import AST
import Data.Either (either)
import Data.Ratio ((%))
import Data.Complex (Complex((:+)))

run :: String -> Term
run = either error id . parseRuby

test title code term = it title $ do
                          run code `shouldBe` term

spec :: Spec
spec = do
  describe "parseRuby" $ do
    test "empty stmt" "" Empty

    test "nil" "nil" Nil

    test "nil_expression" "()" Begin
    test "nil_expression" "begin end" KWBegin

    test "true" "true" RTrue

    test "false" "false" RFalse

    test "int" "42" (RInt 42)
    test "int" "+42" (RInt 42)
    test "int" "-42" (RInt $ -42)

    test "int___LINE__" "__LINE__" (RInt 1)

    test "float" "1.33" (RFloat 1.33)
    test "float" "-1.33" (RFloat $ -1.33)

    test "float" "-1.33" (RFloat $ -1.335)

    test "rational" "42r" (RRational $ 42 % 1)
    test "rational" "42.1r" (RRational $ 421 % 10)

    test "complex" "42i" (RComplex $ 0 :+ 42)
    -- test "complex" "42ri" (RComplex $ 0 :+ (42 % 1))
    test "complex" "42.1i" (RComplex $ 0 :+ 42.1)
    -- test "complex" "42.1ri" (RComplex $ 0 :+ (421 % 10))

    test "string_plain" "'foobar'" (Str "foobar")
    test "string_plain" "%q(foobar)" (Str "foobar")
