-- {-# LANGUAGE ExistentialQuantification, RankNTypes #-}

module AST where

import Data.Ratio (Rational)
import Data.Complex
import Lexer (Token)

data Term =
       Empty
       | Begin [Term]
       | KWBegin
       | Nil
       | RTrue
       | RFalse
       | RInt Int
       | RFloat Float
       | RRational Rational
       -- | RComplex (forall a. Num a => Complex (a))
       | RComplex (Complex Double)
       | Str String
       | Self
       | And
       | Or
       | Until
       | While
       | Anddot
       | Dot
       | Cbase
       | Alias Term Term
       | RArray [Term]
       | Masgn Mlhs Term
       | Send Term String Term
       | Splat (Maybe Term)
       | Lvasgn String (Maybe Term) -- variables
       | Lvar String
       | Ivasgn String (Maybe Term) -- instance variables
       | Ivar String
       | Cvasgn String (Maybe Term) -- class variable
       | Cvar String
       | Gvasgn String (Maybe Term) -- global variables
       | Gvar String
       | Casgn Term String (Maybe Term) -- constants
       | Const Term String
       | Defined Term
       | Encoding
       | NthRef Integer
       | BackRef String
       | Defs Term String Args Term
       | Def String Args Term
       | Sym String
       | Dsym Term Term
       | Undef Term Term Term
       deriving (Eq, Show)

data Mlhs = Mlhs [Term] deriving (Eq, Show)
data Args = Args [Term] deriving (Eq, Show)

lvasgn name = Lvasgn name . Just
ivasgn name = Ivasgn name . Just
cvasgn name = Cvasgn name . Just
gvasgn name = Gvasgn name . Just
casgn name parent = Casgn name parent . Just

mk_unary_op = undefined
mk_kwoptarg = undefined
mk_def_singleton = undefined
-- converts a list of expressions into a single expression
mkExpression = undefined
mk_def_sclass = undefined
mk_preexe = undefined
mk_condition_mod = undefined
mk_begin_body = undefined
mk_loop_mod = undefined
mkLogicalOp = undefined
mk_rescue_body = undefined
mk_not_op = undefined
mk_binary_op = undefined
mk_match_op = undefined
mk_kwarg = undefined
mk_block_pass = undefined
mk_call_method = undefined


mk_ident :: Token -> t1
mk_ident = undefined

mk_ivar :: Token -> t1
mk_ivar = undefined

mk_gvar :: Token -> t1
mk_gvar = undefined

mk_const :: Token -> t1
mk_const = undefined

mk_cvar :: Token -> t1
mk_cvar = undefined

mk_integer :: Token -> t1
mk_integer = undefined

mk_float :: Token -> t1
mk_float = undefined

mk_complex :: Token -> t1
mk_complex = undefined

mk_unary_num = undefined

mk_back_ref = undefined

mk_symbols_compose = undefined

mk_string_compose = undefined

mk_string :: Token -> t1
mk_string = undefined

mk_character :: Token -> t1
mk_character = undefined

mk_words_compose = undefined

mk_word :: t194 -> a
mk_word = undefined

mk_xstring_compose = undefined

mk_string_internal :: Token -> t1
mk_string_internal = undefined

mk_begin :: Token -> t -> Token -> t196
mk_begin = undefined

mk_symbol :: Token -> t1
mk_symbol = undefined

mk_symbol_compose :: Token -> t -> Token -> t196
mk_symbol_compose = undefined

mk_nth_ref :: Token -> t1
mk_nth_ref = undefined


mk_assignable = undefined
mk_accessible = undefined

mk_undef_method = undefined
