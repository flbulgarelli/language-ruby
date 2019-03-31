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
    test "empty stmt" "" Nil

    test "nil" "nil" Nil

    test "nil_expression" "()" (Begin [])
    test "nil_expression" "begin end" KWBegin

    test "true" "true" RTrue

    test "false" "false" RFalse

    test "int" "42" (RInt 42)
    test "int" "+42" (RInt 42)
    test "int" "-42" (RInt $ -42)

    -- test "int___LINE__" "__LINE__" (RInt 1)
    test "int___LINE__" "__LINE__" Line

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

    test "self" "self" Self

    test "lvar" "foo" (Lvar "foo")
    test "ivar" "@foo" (Ivar "@foo")
    test "cvar" "@@foo" (Cvar "@@foo")
    test "gvar" "$foo" (Gvar "$foo")

    test "back_ref" "$+" (BackRef "$+")
    test "nth_ref" "$10" (NthRef 10)
    test "const_toplevel" "::Foo" (Const Cbase "Foo")
    test "const_scoped" "Bar::Foo" (Const (Const Nil "Bar") "Foo")
    test "const_unscoped" "Foo" (Const Nil "Foo")
    test "const__ENCODING__" "__ENCODING__" Encoding

    test "defined?" "defined? foo" (Defined (Lvar "foo"))
    test "defined?" "defined?(foo)" (Defined (Lvar "foo"))
    test "defined?" "defined? @foo" (Defined (Ivar "@foo"))

    test "lvasgn" "var = 10; var" (Begin [lvasgn "var" (RInt 10), Lvar "var"])
    test "ivasgn" "@var = 10"  (ivasgn "@var" (RInt 10))
    test "cvasgn" "@@var = 10"  (cvasgn "@@var" (RInt 10))
    test "gvasgn" "$var = 10"  (gvasgn "$var" (RInt 10))

    test "asgn_cmd" "foo = m foo"  (lvasgn "foo" (Send Nil "m" [Lvar "foo"]))
    test "asgn_cmd" "foo = bar = m foo"  (lvasgn "foo" (lvasgn "bar" (Send Nil "m" [Lvar "foo"])))

    test "casgn_toplevel" "::Foo = 10" (casgn Cbase "Foo"  (RInt 10))
    test "casgn_scoped" "Bar::Foo = 10" (casgn (Const Nil "Bar") "Foo"  (RInt 10))
    test "casgn_unscoped" "Foo = 10" (casgn Nil "Foo" (RInt 10))

    test "masgn" "foo, bar = 1, 2"   (Masgn (Mlhs [Lvasgn "foo" Nothing, Lvasgn "bar" Nothing]) (RArray [RInt 1, RInt 2]))
    test "masgn" "(foo, bar) = 1, 2" (Masgn (Mlhs [Lvasgn "foo" Nothing, Lvasgn "bar" Nothing]) (RArray [RInt 1, RInt 2]))
    test "masgn" "foo, bar, baz = 1, 2" (Masgn (Mlhs [Lvasgn "foo" Nothing, Lvasgn "bar" Nothing, Lvasgn "baz" Nothing]) (RArray [RInt 1, RInt 2]))

    test "masgn_splat" "@foo, @@bar = *foo" (Masgn (Mlhs [Ivasgn "@foo" Nothing, Cvasgn "@@bar" Nothing]) (RArray [Splat (Just (Lvar "foo"))]))
    test "masgn_splat" "a, b = *foo, bar"   (Masgn (Mlhs [Lvasgn "a" Nothing, Lvasgn "b" Nothing]) (RArray [Splat (Just (Lvar "foo")), Lvar "bar"]))
    test "masgn_splat" "a, *b = bar"        (Masgn (Mlhs [Lvasgn "a" Nothing, Splat (Just (Lvasgn "b" Nothing))]) (Lvar "bar"))
    test "masgn_splat" "a, *b, c = bar"     (Masgn (Mlhs [Lvasgn "a" Nothing, Splat (Just (Lvasgn "b" Nothing)), Lvasgn "c" Nothing]) (Lvar "bar"))

    test "masgn_splat" "a, * = bar"         (Masgn (Mlhs [Lvasgn "a" Nothing, Splat Nothing]) (Lvar "bar"))
    test "masgn_splat" "a, *, c = bar"      (Masgn (Mlhs [Lvasgn "a" Nothing, Splat Nothing, Lvasgn "c" Nothing]) (Lvar "bar"))
    test "masgn_splat" "*b = bar"           (Masgn (Mlhs [Splat (Just (Lvasgn "b" Nothing))]) (Lvar "bar"))
    test "masgn_splat" "*b, c = bar"        (Masgn (Mlhs [Splat (Just (Lvasgn "b" Nothing)), Lvasgn "c" Nothing]) (Lvar "bar"))
    test "masgn_splat" "* = bar"            (Masgn (Mlhs [Splat Nothing]) (Lvar "bar"))
    test "masgn_splat" "*, c, d = bar"      (Masgn (Mlhs [Splat Nothing, Lvasgn "c" Nothing, Lvasgn "d" Nothing]) (Lvar "bar"))

  --  test "masgn_nested" (
  --     s(:masgn,
  --       s(:mlhs,
  --         s(:lvasgn, :a),
  --         s(:mlhs,
  --           s(:lvasgn, :b),
  --           s(:lvasgn, :c))),
  --       s(:lvar, :foo)),
  --     %q{a, (b, c) = foo},

  -- test "masgn_nested" "((b, )) = foo" (
  --     s(:masgn,
  --       s(:mlhs,
  --         s(:lvasgn, :b)),
  --       s(:lvar, :foo)),

  -- test "masgn_attr" (
  --     s(:masgn,
  --       s(:mlhs,
  --         s(:send, s(:self), :a=),
  --         s(:indexasgn, s(:self), s(:int, 1), s(:int, 2))),
  --       s(:lvar, :foo)),
  --     %q{self.a, self[1, 2] = foo},
  --     %q{~~~~~~ expression (mlhs.send)
  --       |     ~ selector (mlhs.send)
  --       |            ^ begin (mlhs.indexasgn)
  --       |                 ^ end (mlhs.indexasgn)
  --       |        ~~~~~~~~~~ expression (mlhs.indexasgn)})

  -- test "masgn_attr" "self::a, foo = foo" (
  --     s(:masgn,
  --       s(:mlhs,
  --         s(:send, s(:self), :a=),
  --         s(:lvasgn, :foo)),
  --       s(:lvar, :foo)),

  -- test "masgn_attr" "self.A, foo = foo" (
  --     s(:masgn,
  --       s(:mlhs,
  --         s(:send, s(:self), :A=),
  --         s(:lvasgn, :foo)),
  --       s(:lvar, :foo)),

  -- test "masgn_const" "self::A, foo = foo" (
  --     s(:masgn,
  --       s(:mlhs,
  --         s(:casgn, s(:self), :A),
  --         s(:lvasgn, :foo)),
  --       s(:lvar, :foo)),

  -- test "masgn_const" "::A, foo = foo" (
  --     s(:masgn,
  --       s(:mlhs,
  --         s(:casgn, s(:cbase), :A),
  --         s(:lvasgn, :foo)),
  --       s(:lvar, :foo)),

    test "masgn_cmd" "foo, bar = m foo" (Masgn (Mlhs [Lvasgn "foo" Nothing, Lvasgn "bar" Nothing]) (Send Nil "m" [Lvar "foo"]))

    test "asgn_mrhs" "foo = bar, 1" (Lvasgn "foo" (Just (RArray [Lvar "bar", RInt 1])))
    test "asgn_mrhs" "foo = *bar" (Lvasgn "foo" (Just (RArray [Splat (Just(Lvar "bar"))])))
    test "asgn_mrhs" "foo = baz, *bar" (Lvasgn "foo" (Just (RArray [Lvar "baz", Splat (Just (Lvar "bar"))])))


    test "def" "def foo; end" (Def "foo" (Args []) Nil)
    test "def" "def String; end" (Def "String" (Args []) Nil)
    test "def" "def String=; end" (Def "String=" (Args []) Nil)
    test "def" "def until; end" (Def "until" (Args []) Nil)
    test "def" "def BEGIN; end" (Def "BEGIN" (Args []) Nil)
    test "def" "def END; end" (Def "END" (Args []) Nil)

    test "defs" "def self.foo; end" (Defs Self "foo" (Args []) Nil)
    test "defs" "def self::foo; end" (Defs Self "foo" (Args []) Nil)
    test "defs" "def (foo).foo; end" (Defs (Lvar "foo") "foo" (Args []) Nil)
    test "defs" "def String.foo; end" (Defs (Const Nil "String") "foo" (Args []) Nil)
    test "defs" "def String::foo; end" (Defs (Const Nil "String") "foo" (Args []) Nil)

    test "undef" "undef foo, :bar, :\"foo#{1}\"" (Undef (Sym "foo") (Sym "bar") (Dsym (Str "foo") (Begin [RInt 1])))


    test "alias" "alias :foo bar" (Alias (Sym "foo") (Sym "bar"))

    test "alias_gvar" "alias $a $b" (Alias (Gvar "$a") (Gvar "$b"))
    test "alias_gvar" "alias $a $+" (Alias (Gvar "$a") (BackRef "$+"))

    test "string_interp" "\"foo#{bar}baz\"" (Dstr [Str "foo", Begin [Lvar "bar"], Str "baz"])
    test "string_dvar" "\"#@a #@@a #$a\"" (Dstr [Ivar "@a", Str " ", Cvar "@@a", Str " ", Gvar "$a"])
    test "string_concat" "\"foo#@a\" \"bar\"" (Dstr [Dstr [Str "foo", Ivar "@a"], Str "bar"])
  --  test "string___FILE__" "__FILE__" (Str "(assert_parses)")
    test "string___FILE__" "__FILE__" File
    test "character" "?a" (Str "a")
