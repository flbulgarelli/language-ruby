module LexerSpec (spec) where

import Test.Hspec
import Data.List (intercalate)
import Lexer
import Codec.Binary.UTF8.String (encode)

test title code ts = it (title ++ " [ " ++ code ++ " ]") $ do
  tokens code `shouldBe` ts

spec :: Spec
spec = describe "Lexer:" $ do
  test "integers" "123" [TINTEGER 123]

  test "positive integers" "+123" [TINTEGER 123]

  test "negative integers" "-123" [TINTEGER (-123)]

  test "octal integers" "037" [TINTEGER 037]

  test "hex integers" "0xab"  [TINTEGER 0xab]

  -- test "rationals"   tokens "123r" [TRATIONAL 123]

  -- test "complex"   tokens "123i" [TCOMPLEX 123]

  test "floats" "0.5" [TFLOAT 0.5]

  test "char" "?c" [TCHARACTER 'c']

  test "symbol" ":cat" [TSYMBOL "cat"]

  test "string double quote" "\"dog\""  [TSTRING "dog"]

  test "string single quote with padding" "    'cat'    " [TSTRING "cat"]

  test "string single quote with spaces" "'the cat'" [TSTRING "the cat"]

  test "string single quote" "'cat'" [TSTRING "cat"]

  test "identifier" "var" [TIDENTIFIER "var"]

  test "gvar" "$var" [TGVAR "$var"]

  test "backref" "$+" [TBACK_REF "$+"]
  test "backref" "$`" [TBACK_REF "$`"]
  test "backref" "$&" [TBACK_REF "$&"]

  test "nthref" "$1" [TNTH_REF 1]

  test "nthref" "$10"[TNTH_REF 10]

  test "nthref" "$0" [TNTH_REF 0]

  test "ivar" "@var" [TIVAR "@var"]

  test "tconstant" "Var" [TCONSTANT "Var"]

  test "tcvar" "@@var" [TCVAR "@@var"]

  test "KSELF" "self" [KSELF]

  test "KTRUE" "true" [KTRUE]

  test "KNIL" "nil" [KNIL]

  test "KDEF" "def" [KDEF]

  test "KIF" "if" [KIF]

  test "KDEFINED" "defined?" [KDEFINED]
  test "KDEFINED" "defined? a" [KDEFINED, TIDENTIFIER "a"]
  test "KDEFINED" "defined?(a)" [KDEFINED, TIDENTIFIER "a"]
  test "KDEFINED" "defined?(@a)" [KDEFINED, TIDENTIFIER "a"]

  test "KUNDEF" "undef" [KUNDEF]

  test "assign" "@var = 10" [TIVAR "@var", TOP_ASGN, TINTEGER 10]
  test "assign" "$var = 10" [TGVAR "$var", TOP_ASGN, TINTEGER 10]
  test "assign" "var = 10" [TIDENTIFIER "var", TOP_ASGN, TINTEGER 10]

  test "masgn with splat" "@foo, @@bar = *foo" [TIVAR "@foo", TCOMMA, TCVAR "@@bar",TOP_ASGN, TSTAR, TIDENTIFIER "foo"]
  test "masgn with splat" "* = foo" [TSTAR, TOP_ASGN, TIDENTIFIER "foo"]

  -- test "strings with escape chars"     tokens "'\t'"  "[StringToken '\t']]
  --     tokens "'\\n'" "[StringToken '\\n']]
  --     tokens "'\\\\n'"  "[StringToken '\\\\n']]
  --     tokens "'\\\\'""[StringToken '\\\\']]
  --     tokens "'\\0'" "[StringToken '\\0']]
  --     tokens "'\\12'""[StringToken '\\12']]
  --     tokens "'\\s'"  "[StringToken '\\s']]
  --     tokens "'\\-'"  "[StringToken '\\-']]

  -- test "strings with non-escaped     tokens "'\\/'" "[StringToken '\\/']]

  -- test "strings with escaped quotes"     tokens "'\"'"  "[StringToken '\"']]
  --     tokens "\"\\\"\"" "[StringToken \"\\\\\"\"]]
  --     tokens "'\\\''""[StringToken '\\\\'']]
  --     tokens "'\"'"  "[StringToken '\"']]
  --     tokens "\"\\'\""  "[StringToken \"\\'\"]]

  -- test "spread token"     tokens "...a""[SpreadToken,IdentifierToken 'a']]

  -- test "assignment"     tokens "x=1" "[IdentifierToken 'x',SimpleAssignToken,DecimalToken 1]]
  --     tokens "x=1\ny=2" "[IdentifierToken 'x',SimpleAssignToken,DecimalToken 1,WsToken,IdentifierToken 'y',SimpleAssignToken,DecimalToken 2]]

  test "return" "return 1" [KRETURN, TINTEGER 1]

  test "yield" "yield 1" [KYIELD, TINTEGER 1]

