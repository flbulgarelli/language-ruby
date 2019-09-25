module ParserSpec (spec) where

import Test.Hspec
import Data.Either (either)
import Data.Ratio ((%))
import Data.Complex (Complex((:+)))

import Language.Ruby.Parser
import Language.Ruby.Parser.Builder
import Language.Ruby.AST

run :: String -> Term
run = either error id . parseRuby

test title code term = it (title ++ " [ " ++ code ++ " ]") $ do
                          run code `shouldBe` term

spec :: Spec
spec = do
  describe "parseRuby" $ do
    test "empty stmt" "" Nil

    test "nil" "nil" Nil

    test "nil_expression" "()" (Begin [])
    test "nil_expression" "begin end" (KWBegin [])

    test "true" "true" RTrue

    test "false" "false" RFalse

    test "int" "42" (RInt 42)
    test "int" "+42" (RInt 42)
    test "int" "-42" (RInt $ -42)

    -- test "int___LINE__" "__LINE__" (RInt 1)
    test "int___LINE__" "__LINE__" Line

    test "float" "1.33" (RFloat 1.33)
    test "float" "-1.33" (RFloat $ -1.33)
    test "float" "-1.335" (RFloat $ -1.335)

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

    test "defined?" "defined? foo" (Defined [Lvar "foo"])
    test "defined?" "defined?(foo)" (Defined [Lvar "foo"])
    test "defined?" "defined? @foo" (Defined [Ivar "@foo"])

    test "lvasgn" "var = 10; var" (Begin [lvasgn "var" (RInt 10), Lvar "var"])
    test "ivasgn" "@var = 10"  (ivasgn "@var" (RInt 10))
    test "cvasgn" "@@var = 10"  (cvasgn "@@var" (RInt 10))
    test "gvasgn" "$var = 10"  (gvasgn "$var" (RInt 10))

    test "asgn_cmd" "foo = m foo"  (lvasgn "foo" (Send Nil "m" [Lvar "foo"]))
    test "asgn_cmd" "foo = bar = m foo"  (lvasgn "foo" (lvasgn "bar" (Send Nil "m" [Lvar "foo"])))

    test "casgn_toplevel" "::Foo = 10" (casgn Cbase "Foo"  (RInt 10))
    test "casgn_scoped" "Bar::Foo = 10" (casgn (Const Nil "Bar") "Foo"  (RInt 10))
    test "casgn_unscoped" "Foo = 10" (casgn Nil "Foo" (RInt 10))

    test "masgn" "foo, bar = 1, 2"          (Masgn (Mlhs [Lvasgn "foo" Nothing, Lvasgn "bar" Nothing]) (RArray [RInt 1, RInt 2]))
    test "masgn" "(foo, bar) = 1, 2"        (Masgn (Mlhs [Lvasgn "foo" Nothing, Lvasgn "bar" Nothing]) (RArray [RInt 1, RInt 2]))
    test "masgn" "foo, bar, baz = 1, 2"     (Masgn (Mlhs [Lvasgn "foo" Nothing, Lvasgn "bar" Nothing, Lvasgn "baz" Nothing]) (RArray [RInt 1, RInt 2]))

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

    test "asgn_mrhs" "foo = bar, 1"     (Lvasgn "foo" (Just (RArray [Lvar "bar", RInt 1])))
    test "asgn_mrhs" "foo = *bar"       (Lvasgn "foo" (Just (RArray [Splat (Just (Lvar "bar"))])))
    test "asgn_mrhs" "foo = baz, *bar"  (Lvasgn "foo" (Just (RArray [Lvar "baz", Splat (Just (Lvar "bar"))])))


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

    test "undef" "undef foo" (Undef [Sym "foo"])
    test "undef" "undef foo, :bar, :\"foo#{1}\"" (Undef [Sym "foo", Sym "bar", Dsym [Str "foo", Begin [RInt 1]]])


    test "alias" "alias :foo bar" (Alias (Sym "foo") (Sym "bar"))

    test "alias_gvar" "alias $a $b" (Alias (Gvar "$a") (Gvar "$b"))
    test "alias_gvar" "alias $a $+" (Alias (Gvar "$a") (BackRef "$+"))

    test "string_interp" "\"foo#{bar}baz\"" (Dstr [Str "foo", Begin [Lvar "bar"], Str "baz"])
    test "string_dvar" "\"#@a #@@a #$a\"" (Dstr [Ivar "@a", Str " ", Cvar "@@a", Str " ", Gvar "$a"])
    test "string_concat" "\"foo#@a\" \"bar\"" (Dstr [Dstr [Str "foo", Ivar "@a"], Str "bar"])
    test "string_concat" "'aaa' 'bbb'" (Dstr [Str "aaa", Str "bbb"])
    test "string_concat" "\"aaa\" \"bbb\"" (Dstr [Str "aaa", Str "bbb"])
  --  test "string___FILE__" "__FILE__" (Str "(assert_parses)")
    test "string___FILE__" "__FILE__" File
    test "character" "?a" (Str "a")


  --   test "heredoc" "<<HERE\nfoo\nbar\nHERE" (Dstr [Str "foo\n", Str "bar\n"])
  --   test "heredoc" "<<'HERE'\nfoo\nbar\nHERE" (Dstr [Str "foo\n", Str "bar\n"])
  --   test "heredoc" "<<`HERE`\nfoo\nbar\nHERE" (Dstr [Str "foo\n", Str "bar\n"])
  -- test "dedenting_heredoc" "p <<~E\nE" (Send Nil "p" (Dstr [])
  -- test "dedenting_heredoc" "p <<~E\n  E" (Send Nil "p" (Dstr [])
  -- test "dedenting_heredoc" "p <<~E\n  x\nE" (Send Nil "p" (Str "x\n")
  -- test "dedenting_heredoc" "p <<~E\n  x\n    y\nE" (Send Nil "p" (Dstr [Str "x\n", Str "  y\n"]))

  -- test "dedenting_heredoc" "p <<~E\n\tx\n    y\nE" (Send Nil "p" (Dstr [Str "x\n", Str "y\n"]))
  -- test "dedenting_heredoc" "p <<~E\n\tx\n        y\nE" (Send Nil "p" (Dstr [Str "x\n", Str "y\n"]))
  -- test "dedenting_heredoc" "p <<~E\n    \tx\n        y\nE" (Send Nil "p" (Dstr [Str "x\n", Str "y\n"]))
  -- test "dedenting_heredoc" "p <<~E\n        \tx\n\ty\nE" (Send Nil "p" (Dstr [Str "\tx\n", Str "y\n"]))
  -- test "dedenting_heredoc" "p <<~E\n  x\n\ny\nE" (Send Nil "p" (Dstr [Str "  x\n", Str "\n", Str "y\n"]))
  -- test "dedenting_heredoc" "p <<~E\n  x\n    \n  y\nE" (Send Nil "p" (Dstr [Str "x\n", Str "   \n", Str "y\n"]))


  --   assert_parses(
  --     (Send Nil, :p,
  --       (Dstr
  --         (Str "  x\n"),
  --         (Str "  y\n"))),
  --     %Q{p <<~E\n    x\n  \\  y\nE},
  --     %q{},

  --   assert_parses(
  --     (Send Nil, :p,
  --       (Dstr
  --         (Str "  x\n"),
  --         (Str "\ty\n"))),
  --     %Q{p <<~E\n    x\n  \\\ty\nE},
  --     %q{},

  --   assert_parses(
  --     (Send Nil, :p,
  --       (Dstr
  --         (Str "  x\n"),
  --         (Str ""),
  --         (Begin
  --           (Lvar :foo)),
  --         (Str "\n"))),
  --     %Q{p <<~"E"\n    x\n  \#{foo}\nE},
  --     %q{},

  --   assert_parses(
  --     (Send Nil, :p,
  --       s(:xstr,
  --         (Str "  x\n"),
  --         (Str ""),
  --         (Begin
  --           (Lvar :foo)),
  --         (Str "\n"))),
  --     %Q{p <<~`E`\n    x\n  \#{foo}\nE},
  --     %q{},

  --   assert_parses(
  --     (Send Nil, :p,
  --       (Dstr
  --         (Str "  x\n"),
  --         (Str ""),
  --         (Begin
  --           (Str "  y")),
  --         (Str "\n"))),
  --     %Q{p <<~"E"\n    x\n  \#{"  y"}\nE},
  --     %q{},
  -- end

  -- # Symbols

  -- test "symbol_plain" ":foo" (Sym :foo)
  -- test "symbol_plain" ":'foo'" (Sym :foo)

  -- test "symbol_interp"
  --   assert_parses(
  --     (Dsym
  --       (Str 'foo'),
  --       (Begin (Lvar :bar)),
  --       (Str 'baz')),
  --     %q{:"foo#{bar}baz"},

  -- end

  -- # Execute-strings

  -- test "xstring_plain"
  --   assert_parses(
  --     s(:xstr, (Str 'foobar')),
  --     %q{`foobar`},
  --     %q{^ begin
  --       |       ^ end
  --       |~~~~~~~~ expression})
  -- end

  -- test "xstring_interp"
  --   assert_parses(
  --     s(:xstr,
  --       (Str 'foo'),
  --       (Begin (Lvar :bar)),
  --       (Str 'baz')),
  --     %q{`foo#{bar}baz`},

  -- -- Regexp

  -- test "regex_plain"
  --   assert_parses(
  --     s(:regexp, (Str 'source'), s(:regopt, :i, :m)),
  --     %q{/source/im},

  -- test "regex_interp"
  --   assert_parses(
  --     s(:regexp,
  --       (Str 'foo'),
  --       (Begin (Lvar :bar)),
  --       (Str 'baz'),
  --       s(:regopt)),
  --     %q{/foo#{bar}baz/},

  -- end


  -- -- Arrays

  -- test "array_plain"
  --   assert_parses(
  --     (Array, (RInt 1), (RInt 2)),
  --     %q{[1, 2]},

  -- test "array_splat"
  --   assert_parses(
  --     (Array,
  --       (RInt 1),
  --       s(:splat, (Lvar :foo)),
  --       (RInt 2)),
  --     %q{[1, *foo, 2]},

  --   assert_parses(
  --     (Array,
  --       (RInt 1),
  --       s(:splat, (Lvar :foo))),
  --     %q{[1, *foo]},

  --   assert_parses(
  --     (Array,
  --       s(:splat, (Lvar :foo))),
  --     %q{[*foo]})
  -- end

  -- test "array_assocs"
  --   assert_parses(
  --     (Array,
  --       (Hash (Pair (RInt 1), (RInt 2)))),
  --     %q{[ 1 => 2 ]},

  --   assert_parses(
  --     (Array,
  --       (RInt 1),
  --       (Hash (Pair (RInt 2), (RInt 3)))),
  --     %q{[ 1, 2 => 3 ]},
  -- end

  -- test "array_words" "%w[foo bar]" (Array [Str 'foo', Str 'bar'])
  -- test "array_words_interp" "%W[foo #{bar}" (Array [Str 'foo', Dstr [Begin [Lvar "bar"]]])
  -- test "array_words_interp" "%W[foo #{bar}foo#@baz]" (Array [Str 'foo', Dstr [Begin [Lvar "bar"], Str "foo", Ivar "@baz"]])
  -- test "array_words_empty" "%w[]" (Array [])
  -- test "array_symbols" "%i[foo bar]" (Array [Sym "foo", Sym "bar"])
  -- test "array_symbols_interp" "%I[foo #{bar}]" (Array [Dsym [Str 'foo', Begin [Lvar "bar"]]])
  -- test "array_symbols_interp" "%I[foo#{bar}]" (Array [Dsym [Str 'foo', Begin [Lvar "bar"]]])

  -- test "array_symbols_empty" "%i[]" (Array [])
  -- test "array_symbols_empty" "%I()" (Array [])

  -- test "hash_empty" "{ }" (Hash [])
  -- test "hash_hashrocket" "{ 1 => 2 }" (Hash [Pair (RInt 1) (RInt 2)])
  -- test "hash_hashrocket" "{ 1 => 2, :foo => "bar" }" (Hash [Pair (RInt 1) (RInt 2), Pair (Sym "foo") (Str "bar")]

  -- test "hash_label" "{ foo: 2 }" (Hash (Pair (Sym "foo"), (RInt 2)))

  -- test "hash_label_end" "{ 'foo': 2 }"
  --     (Hash (Pair (Sym "foo"), (RInt 2))),

  -- test "hash_label_end" "{ 'foo': 2, 'bar': {}}"
  --     (Hash
  --       (Pair (Sym :foo) (RInt 2)),
  --       (Pair (Sym :bar) (Hash))),

  -- test "hash_label_end" "f(a ? "a":1)" (Send Nil :f (If (Send Nil :a) (Str "a") (RInt 1)))
  -- test "hash_kwsplat" "{ foo: 2, **bar }" (Hash (Pair (Sym :foo) (RInt 2)) (Kwsplat (Lvar :bar)))

  -- -- Range

    test "range_inclusive" "1..2" (IRange (RInt 1) (RInt 2))
    test "range_inclusive" "1 .. 2" (IRange (RInt 1) (RInt 2))
    test "range_exclusive" "1...2"  (ERange (RInt 1) (RInt 2))
    test "range_exclusive" "1 ... 2"  (ERange (RInt 1) (RInt 2))

    test "range_endless" "1.." (IRange (RInt 1) Nil)
    test "range_endless" "1 .. " (IRange (RInt 1) Nil)
    test "range_endless" "1..." (ERange (RInt 1) Nil)
    test "range_endless" "1 ... " (ERange (RInt 1) Nil)

    test "if" "if foo then bar; end" (If (Lvar "foo") (Lvar "bar") Nil)
    test "if" "if foo; bar; end" (If (Lvar "foo") (Lvar "bar") Nil)

    test "if_nl_then" "if foo\nthen bar end" (If (Lvar "foo") (Lvar "bar") Nil)

    test "if_mod" "bar if foo" (If (Lvar "foo") (Lvar "bar") Nil)

    test "unless" "unless foo then bar; end" (If (Lvar "foo") Nil (Lvar "bar"))
    test "unless" "unless foo; bar; end" (If (Lvar "foo") Nil (Lvar "bar"))

    test "unless_mod" "bar unless foo" (If (Lvar "foo") Nil (Lvar "bar"))

    test "if_else" "if foo then bar; else baz; end" (If (Lvar "foo") (Lvar "bar") (Lvar "baz"))
    test "if_else" "if foo; bar; else baz; end" (If (Lvar "foo") (Lvar "bar") (Lvar "baz"))

    test "unless_else" "unless foo then bar; else baz; end" (If (Lvar "foo") (Lvar "baz") (Lvar "bar"))
    test "unless_else" "unless foo; bar; else baz; end" (If (Lvar "foo") (Lvar "baz") (Lvar "bar"))

    test "if_elsif" "if foo; bar; elsif baz; 1; else 2; end" (If (Lvar "foo") (Lvar "bar") (If (Lvar "baz") (RInt 1) (RInt 2)))

    test "ternary" "foo ? 1 : 2" (If (Lvar "foo") (RInt 1) (RInt 2))

    test "ternary_ambiguous_symbol" "t=1;(foo)?t:T" (Begin [Lvasgn "t" (Just $ RInt 1), If (Begin [Lvar "foo"]) (Lvar "t") (Const Nil "T")])

    test "if_masgn__24" "if (a, b = foo); end" (If (Begin [Masgn (Mlhs [Lvasgn "a" Nothing, Lvasgn "b" Nothing]) (Lvar "foo")]) Nil Nil)

    test "not_masgn__24" "!(a, b = foo)" (Send (Begin [Masgn (Mlhs [Lvasgn "a" Nothing, Lvasgn "b" Nothing]) (Lvar "foo")]) "!" [])

    test "cond_begin" "if (bar); foo; end" (If (Begin [Lvar "bar"]) (Lvar "foo") Nil)

    test "cond_begin_masgn" "if (bar; a, b = foo); end" (If (Begin [Lvar "bar", Masgn (Mlhs [Lvasgn "a" Nothing, Lvasgn "b" Nothing]) (Lvar "foo")]) Nil Nil)

    --test "cond_match_current_line" "if /wat/; end" (If (MatchCurrentLine (Regexp (Str "wat") (Regopt))) Nil Nil)

    --test "cond_match_current_line" "!/wat/" (Send (MatchCurrentLine (Regexp (Str "wat") (Regopt))) "!")

    test "not" "not foo" (Send (Lvar "foo") "!" [])
    test "not" "not(foo)" (Send (Lvar "foo") "!" [])
    test "not" "not()" (Send (Begin []) "!" [])

    test "not_cmd" "not m foo" (Send (Send Nil "m" [Lvar "foo"]) "!" [])

    test "send_binary_op" "foo + 1"   (Send (Lvar "foo") "+"    [RInt 1])
    test "send_binary_op" "foo - 1"   (Send (Lvar "foo") "-"    [RInt 1])
    test "send_binary_op" "foo * 1"   (Send (Lvar "foo") "*"    [RInt 1])
    test "send_binary_op" "foo / 1"   (Send (Lvar "foo") "/"    [RInt 1])
    test "send_binary_op" "foo % 1"   (Send (Lvar "foo") "%"    [RInt 1])
    test "send_binary_op" "foo ** 1"  (Send (Lvar "foo") "**"   [RInt 1])
    test "send_binary_op" "foo | 1"   (Send (Lvar "foo") "|"    [RInt 1])
    test "send_binary_op" "foo ^ 1"   (Send (Lvar "foo") "^"    [RInt 1])
    test "send_binary_op" "foo & 1"   (Send (Lvar "foo") "&"    [RInt 1])
    test "send_binary_op" "foo <=> 1" (Send (Lvar "foo") "<=>"  [RInt 1])
    test "send_binary_op" "foo < 1"   (Send (Lvar "foo") "<"    [RInt 1])
    test "send_binary_op" "foo <= 1"  (Send (Lvar "foo") "<="   [RInt 1])
    test "send_binary_op" "foo > 1"   (Send (Lvar "foo") ">"    [RInt 1])
    test "send_binary_op" "foo >= 1"  (Send (Lvar "foo") ">="   [RInt 1])
    test "send_binary_op" "foo == 1"  (Send (Lvar "foo") "=="   [RInt 1])
    test "send_binary_op" "foo != 1"  (Send (Lvar "foo") "!="   [RInt 1])
    test "send_binary_op" "foo === 1" (Send (Lvar "foo") "==="  [RInt 1])
    test "send_binary_op" "foo =~ 1"  (Send (Lvar "foo") "=~"   [RInt 1])
    test "send_binary_op" "foo !~ 1"  (Send (Lvar "foo") "!~"   [RInt 1])
    test "send_binary_op" "foo << 1"  (Send (Lvar "foo") "<<"   [RInt 1])
    test "send_binary_op" "foo >> 1"  (Send (Lvar "foo") ">>"   [RInt 1])

    test "send_unary_op" "-foo" (Send (Lvar "foo") "-@" [])
    test "send_unary_op" "+foo" (Send (Lvar "foo") "+@" [])
    test "send_unary_op" "~foo" (Send (Lvar "foo") "~" [])

    test "and" "foo and bar" (And (Lvar "foo") (Lvar "bar"))
    test "and" "foo && bar" (And (Lvar "foo") (Lvar "bar"))

    test "or" "foo or bar" (Or (Lvar "foo") (Lvar "bar"))
    test "or" "foo || bar" (Or (Lvar "foo") (Lvar "bar"))

    test "and_or_masgn" "foo && (a, b = bar)" (And (Lvar "foo") (Begin [Masgn (Mlhs [Lvasgn "a" Nothing, Lvasgn "b" Nothing]) (Lvar "bar")]))
    test "and_or_masgn" "foo || (a, b = bar)" (Or (Lvar "foo") (Begin [Masgn (Mlhs [Lvasgn "a" Nothing, Lvasgn "b" Nothing]) (Lvar "bar")]))

    test "case_expr" "case foo; when 'bar'; bar; end" (Case [Lvar "foo", When [Str "bar", Lvar "bar"], Nil])

    test "case_expr_else" "case foo; when 'bar'; bar; else baz; end" (Case [Lvar "foo", When [Str "bar", Lvar "bar"], Lvar "baz"])

    test "case_cond" "case; when foo; 'foo'; end" (Case [Nil, When [Lvar "foo", Str "foo"], Nil])

    test "case_cond_else" "case; when foo; 'foo'; else 'bar'; end" (Case [Nil, When [Lvar "foo", Str "foo"], Str "bar"])

    test "case_cond_just_else" "case; else 'bar'; end" (Case [Nil, Str "bar"])

    test "when_then" "case foo; when 'bar' then bar; end" (Case [Lvar "foo", When [Str "bar", Lvar "bar"], Nil])

    test "when_multi" "case foo; when 'bar', 'baz'; bar; end" (Case [Lvar "foo", When [Str "bar", Str "baz", Lvar "bar"], Nil])

    test "when_splat" "case foo; when 1, *baz; bar; when *foo; end" (Case [Lvar "foo", When [RInt 1, Splat (Just (Lvar "baz")), Lvar "bar"], When [Splat (Just (Lvar "foo")), Nil], Nil])

    test "for" "for a in foo do p a; end" (For (Lvasgn "a" Nothing) (Lvar "foo") (Send Nil "p" [Lvar "a"]))

    test "for" "for a in foo; p a; end" (For (Lvasgn "a" Nothing) (Lvar "foo") (Send Nil "p" [Lvar "a"]))

    test "for_mlhs" "for a, b in foo; p a, b; end" (For (Mlhs [Lvasgn "a" Nothing, Lvasgn "b" Nothing]) (Lvar "foo") (Send Nil "p" [Lvar "a", Lvar "b"]))

    test "postexe" "END { 1 }" (Postexe (RInt 1))

    test "while" "while foo do meth end" (While (Lvar "foo") (Send Nil "meth" []))
    test "while" "while foo; meth end" (While (Lvar "foo") (Send Nil "meth" []))

    test "until" "until foo do meth end" (Until (Lvar "foo") (Send Nil "meth" []))
    test "until" "until foo; meth end" (Until (Lvar "foo") (Send Nil "meth" []))

    test "while_mod" "meth while foo" (While (Lvar "foo") (Send Nil "meth" []))

    test "until_mod" "meth until foo" (Until (Lvar "foo") (Send Nil "meth" []))

    test "while_post" "begin meth end while foo" (WhilePost (Lvar "foo") (KWBegin [Send Nil "meth" []]))

    test "until_post" "begin meth end until foo" (UntilPost (Lvar "foo") (KWBegin [Send Nil "meth" []]))

    test "rescue" "begin; meth; rescue; foo; end" (KWBegin [Rescue (Send Nil "meth" []) [Resbody Nil Nil (Lvar "foo"), Nil]])

    test "rescue_else" "begin; meth; rescue; foo; else; bar; end" (KWBegin [Rescue (Send Nil "meth" []) [Resbody Nil Nil (Lvar "foo"), (Lvar "bar")]])

    test "ensure" "begin; meth; ensure; bar; end" (KWBegin [Ensure (Send Nil "meth" []) (Lvar "bar")])

    test "ensure_empty" "begin ensure end" (KWBegin [Ensure Nil Nil])

    test "rescue_ensure" "begin; meth; rescue; baz; ensure; bar; end" (KWBegin [Ensure (Rescue (Send Nil "meth" []) [Resbody Nil Nil (Lvar "baz"), Nil]) (Lvar "bar")])

    test "rescue_else_ensure" "begin; meth; rescue; baz; else foo; ensure; bar end" (KWBegin [Ensure (Rescue (Send Nil "meth" []) [Resbody Nil Nil (Lvar "baz"), Lvar "foo"]) (Lvar "bar")])

    test "rescue_mod" "meth rescue bar" (Rescue (Send Nil "meth" []) [Resbody Nil Nil (Lvar "bar"), Nil])

    test "rescue_mod_asgn" "foo = meth rescue bar" (Lvasgn "foo" (Just $ Rescue (Send Nil "meth" []) [Resbody Nil Nil (Lvar "bar"), Nil]))


    --test "rescue_mod_op_assign" "foo += meth rescue bar" (Op_asgn (Lvasgn "foo" Nothing), "+", (Rescue (Send Nil "meth" []), (Resbody Nil Nil (Lvar "bar")), Nil))

    --test "rescue_without_begin_end" "meth do; foo; rescue; bar; end" (Block, (Send Nil "meth" []), (Args), (Rescue (Lvar "foo"), (Resbody Nil Nil (Lvar "bar")), Nil))

    test "resbody_list" "begin; meth; rescue Exception; bar; end" (KWBegin [Rescue (Send Nil "meth" []) [Resbody (RArray [Const Nil "Exception"]) Nil (Lvar "bar"), Nil]])

    test "resbody_list_mrhs" "begin; meth; rescue Exception, foo; bar; end" (KWBegin [Rescue (Send Nil "meth" []) [Resbody (RArray [Const Nil "Exception", Lvar "foo"]) Nil (Lvar "bar"), Nil]])

    test "resbody_var" "begin; meth; rescue => ex; bar; end" (KWBegin [Rescue (Send Nil "meth" []) [Resbody Nil (Lvasgn "ex" Nothing) (Lvar "bar"), Nil]])

    test "resbody_var" "begin; meth; rescue => @ex; bar; end" (KWBegin [Rescue (Send Nil "meth" []) [Resbody Nil (Ivasgn "@ex" Nothing) (Lvar "bar"), Nil]])

    test "resbody_list_var" "begin; meth; rescue foo => ex; bar; end" (KWBegin [Rescue (Send Nil "meth" []) [Resbody (RArray [Lvar "foo"]) (Lvasgn "ex" Nothing) (Lvar "bar"), Nil]])

    test "retry" "retry" (Retry [])

    test "hash_empty" "{ }" (Hash [])

    test "hash_hashrocket" "{ 1 => 2 }" (Hash [Pair (RInt 1) (RInt 2)])

    test "hash_hashrocket" "{ 1 => 2, :foo => \"bar\" }]" (Hash [Pair (RInt 1) (RInt 2), Pair (Sym "foo") (Str "bar")])

    test "hash_label" "{ foo: 2 }" (Hash [Pair (Sym "foo") (RInt 2)])

    test "hash_label_end" "{ 'foo': 2 }" (Hash [Pair (Sym "foo") (RInt 2)])

    test "hash_label_end" "{ 'foo': 2, 'bar': {}}" (Hash [Pair (Sym "foo") (RInt 2), Pair (Sym "bar") (Hash [])])

    test "hash_label_end" "f(a ? \"a\":1)" (Send Nil "f" [If (Send Nil "a" []) (Str "a") (RInt 1)])

    test "hash_kwsplat" "{ foo: 2, **bar }" (Hash [Pair (Sym "foo") (RInt 2), KWSplat (Lvar "bar")])

    test "hash_no_hashrocket" "{ 1, 2 }" (Hash [Pair (RInt 1) (RInt 2)])

    test "args_cmd" "fun(f bar)" (Send Nil "fun" [Send Nil "f" [Lvar "bar"]])

    test "args_args_star" "fun(foo, *bar)" (Send Nil "fun" [Lvar "foo", Splat $ Just (Lvar "bar")])

    test "args_args_star" "fun(foo, *bar, &baz)" (Send Nil "fun" [Lvar "foo", Splat $ Just (Lvar "bar"), BlockPass (Lvar "baz")])

    test "args_star" "fun(*bar)" (Send Nil "fun" [Splat $ Just (Lvar "bar")])

    test "args_star" "fun(*bar, &baz)" (Send Nil "fun" [Splat $ Just (Lvar "bar"), BlockPass (Lvar "baz")])

    test "args_block_pass" "fun(&bar)" (Send Nil "fun" [BlockPass (Lvar "bar")])

    --test "args_args_comma" "foo[bar,]" (Index, (Lvar "foo"), (Lvar "bar"))

    test "args_assocs" "fun(:foo => 1)" (Send Nil "fun" [Hash [Pair (Sym "foo") (RInt 1)]])

    test "args_assocs" "fun(:foo => 1, &baz)" (Send Nil "fun" [Hash [Pair (Sym "foo") (RInt 1)], BlockPass (Lvar "baz")])

    --test "args_assocs_comma" "foo[:baz => 1,]" (Index, (Lvar "foo"), (Hash (Pair, (Sym "baz"), (RInt, 1))))

    test "args_args_assocs" "fun(foo, :foo => 1)" (Send Nil "fun" [Lvar "foo", Hash [Pair (Sym "foo") (RInt 1)]])

    test "args_args_assocs" "fun(foo, :foo => 1, &baz)" (Send Nil "fun" [Lvar "foo", Hash [Pair (Sym "foo") (RInt 1)], BlockPass (Lvar "baz")])

    --test "args_args_assocs_comma" "foo[bar, :baz => 1,]" (Index, (Lvar "foo"), (Lvar "bar"), (Hash (Pair, (Sym "baz"), (RInt, 1))))

    test "space_args_cmd" "fun (f bar)" (Send Nil "fun" [Begin [Send Nil "f" [Lvar "bar"]]])

    test "space_args_arg" "fun (1)" (Send Nil "fun" [Begin [RInt 1]])

    test "space_args_arg_newline" "fun (1\n)" (Send Nil "fun" [Begin [RInt 1]])

    --test "space_args_arg_block" "fun (1) {}" (Block (Send Nil "fun", (Begin (RInt, 1))), (Args), Nil)

    --test "space_args_arg_block" "foo.fun (1) {}" (Block (Send (Lvar "foo"), "fun", (Begin (RInt, 1))), (Args), Nil)

    test "space_args_arg_call" "fun (1).to_i" (Send Nil "fun" [Send (Begin [RInt 1]) "to_i" []])
