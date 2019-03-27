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
       | Line
       | NthRef Integer
       | BackRef String
       | Defs Term String Args Term
       | Def String Args Term
       | Sym String
       | Dsym Term Term
       | Undef Term Term Term
       | Break
       | Next
       | Redo
       | Retry
       | Dstr [Term]
       | File
       | Yield
       | Super
       | Return
       deriving (Eq, Show)

data Mlhs = Mlhs [Term] deriving (Eq, Show)
data Args = Args [Term] deriving (Eq, Show)

lvasgn name = Lvasgn name . Just
ivasgn name = Ivasgn name . Just
cvasgn name = Cvasgn name . Just
gvasgn name = Gvasgn name . Just
casgn name parent = Casgn name parent . Just

mkLogicalOp = undefined
mkExpression = undefined

mk___ENCODING__ = undefined
mk___FILE__ = undefined
mk___LINE__ = undefined
mk_accessible = undefined
mk_alias = undefined
mk_arg = undefined
mk_args = undefined
mk_array = undefined
mk_assign = undefined
mk_assignable = undefined
mk_associate = undefined
mk_attr_asgn = undefined
mk_back_ref = undefined
mk_begin = undefined
mk_begin_body = undefined
mk_begin_keyword = undefined
mk_binary_op = undefined
mk_block = undefined
mk_block_pass = undefined
mk_blockarg = undefined
mk_call_lambda = undefined
mk_call_method = undefined
mk_case = undefined
mk_character = undefined
mk_complex = undefined
mk_condition = undefined
mk_condition_mod = undefined
mk_const = undefined
mk_const_fetch = undefined
mk_const_global = undefined
mk_const_op_assignable = undefined
mk_cvar = undefined
mk_def_class = undefined
mk_def_method = undefined
mk_def_module = undefined
mk_def_sclass = undefined
mk_def_singleton = undefined
mk_false = undefined
mk_float = undefined
mk_for = undefined
mk_gvar = undefined
mk_ident = undefined
mk_index = undefined
mk_index_asgn = undefined
mk_integer = undefined
mk_ivar = undefined
mk_keyword_cmd = undefined
mk_kwarg = undefined
mk_kwoptarg = undefined
mk_kwrestarg = undefined
mk_kwsplat = undefined
mk_loop = undefined
mk_loop_mod = undefined
mk_match_op = undefined
mk_multi_lhs = undefined
mk_Nil = undefined
mk_not_op = undefined
mk_nth_ref = undefined
mk_op_assign = undefined
mk_optarg = undefined
mk_pair = undefined
mk_pair_keyword = undefined
mk_pair_quoted = undefined
mk_preexe = undefined
mk_procarg0 = undefined
mk_range_exclusive = undefined
mk_range_inclusive = undefined
mk_rational = undefined
mk_regexp_compose = undefined
mk_regexp_options = undefined
mk_rescue_body = undefined
mk_restarg = undefined
mk_self = undefined
mk_shadowarg = undefined
mk_splat = undefined
mk_string = undefined
mk_string_compose = undefined
mk_string_internal = undefined
mk_symbol = undefined
mk_symbol_compose = undefined
mk_symbol_internal = undefined
mk_symbols_compose = undefined
mk_ternary = undefined
mk_true = undefined
mk_unary_num = undefined
mk_unary_op = undefined
mk_undef_method = undefined
mk_when = undefined
mk_word = undefined
mk_words_compose = undefined
mk_xstring_compose = undefined
