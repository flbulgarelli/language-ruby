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

mkLogicalOp = error "mkLogicalOp"
mkExpression = error "mkExpression"
mk_multiassign = error "mk_multiassign"
mk_postexe = error "mk_postexe"
mk___ENCODING__ = error "mk___ENCODING__"
mk___FILE__ = error "mk___FILE__"
mk___LINE__ = error "mk___LINE__"
mk_accessible = error "mk_accessible"
mk_alias = error "mk_alias"
mk_arg = error "mk_arg"
mk_args = error "mk_args"
mk_array = error "mk_array"
mk_assign = error "mk_assign"
mk_assignable = error "mk_assignable"
mk_associate = error "mk_associate"
mk_attr_asgn = error "mk_attr_asgn"
mk_back_ref = error "mk_back_ref"
mk_begin = error "mk_begin"
mk_begin_body = error "mk_begin_body"
mk_begin_keyword = error "mk_begin_keyword"
mk_binary_op = error "mk_binary_op"
mk_block = error "mk_block"
mk_block_pass = error "mk_block_pass"
mk_blockarg = error "mk_blockarg"
mk_call_lambda = error "mk_call_lambda"
mk_call_method = error "mk_call_method"
mk_case = error "mk_case"
mk_character = error "mk_character"
mk_complex = error "mk_complex"
mk_condition = error "mk_condition"
mk_condition_mod = error "mk_condition_mod"
mk_const = error "mk_const"
mk_const_fetch = error "mk_const_fetch"
mk_const_global = error "mk_const_global"
mk_const_op_assignable = error "mk_const_op_assignable"
mk_cvar = error "mk_cvar"
mk_def_class = error "mk_def_class"
mk_def_method = error "mk_def_method"
mk_def_module = error "mk_def_module"
mk_def_sclass = error "mk_def_sclass"
mk_def_singleton = error "mk_def_singleton"
mk_false = error "mk_false"
mk_float = error "mk_float"
mk_for = error "mk_for"
mk_gvar = error "mk_gvar"
mk_ident = error "mk_ident"
mk_index = error "mk_index"
mk_index_asgn = error "mk_index_asgn"
mk_integer = error "mk_integer"
mk_ivar = error "mk_ivar"
mk_keyword_cmd = error "mk_keyword_cmd"
mk_kwarg = error "mk_kwarg"
mk_kwoptarg = error "mk_kwoptarg"
mk_kwrestarg = error "mk_kwrestarg"
mk_kwsplat = error "mk_kwsplat"
mk_loop = error "mk_loop"
mk_loop_mod = error "mk_loop_mod"
mk_match_op = error "mk_match_op"
mk_multi_lhs = error "mk_multi_lhs"
mk_Nil = error "mk_Nil"
mk_not_op = error "mk_not_op"
mk_nth_ref = error "mk_nth_ref"
mk_op_assign = error "mk_op_assign"
mk_optarg = error "mk_optarg"
mk_pair = error "mk_pair"
mk_pair_keyword = error "mk_pair_keyword"
mk_pair_quoted = error "mk_pair_quoted"
mk_preexe = error "mk_preexe"
mk_procarg0 = error "mk_procarg0"
mk_range_exclusive = error "mk_range_exclusive"
mk_range_inclusive = error "mk_range_inclusive"
mk_rational = error "mk_rational"
mk_regexp_compose = error "mk_regexp_compose"
mk_regexp_options = error "mk_regexp_options"
mk_rescue_body = error "mk_rescue_body"
mk_restarg = error "mk_restarg"
mk_self = error "mk_self"
mk_shadowarg = error "mk_shadowarg"
mk_splat = error "mk_splat"
mk_string = error "mk_string"
mk_string_compose = error "mk_string_compose"
mk_string_internal = error "mk_string_internal"
mk_symbol = error "mk_symbol"
mk_symbol_compose = error "mk_symbol_compose"
mk_symbol_internal = error "mk_symbol_internal"
mk_symbols_compose = error "mk_symbols_compose"
mk_ternary = error "mk_ternary"
mk_true = error "mk_true"
mk_unary_num = error "mk_unary_num"
mk_unary_op = error "mk_unary_op"
mk_undef_method = error "mk_undef_method"
mk_when = error "mk_when"
mk_word = error "mk_word"
mk_words_compose = error "mk_words_compose"
mk_xstring_compose = error "mk_xstring_compose"

-- mk_block' (begin_t, args, body, end_t) = mk_block (mk_call_method $1 $2 $3 $4) begin_t Args body end_t
mk_block' = error "mk_block"
