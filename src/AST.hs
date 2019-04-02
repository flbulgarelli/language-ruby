-- {-# LANGUAGE ExistentialQuantification, RankNTypes #-}

module AST where

import Data.Ratio (Rational)
import Data.Complex
import Lexer (Token (..))

data Term
       = Begin [Term]
       | KWBegin [Term]
       | Nil
       | RTrue
       | RFalse
       | RInt Int
       | RFloat Double
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
       | Send Term String [Term]
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
       | NthRef Int
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

type StaticEnv = [String]

mkLogicalOp = error "mkLogicalOp"

mkExpression :: [Term] -> Term
mkExpression []  = Nil
mkExpression [e] = e
mkExpression xs  = Begin xs

mk_multiassign = error "mk_multiassign"
mk_postexe = error "mk_postexe"

mk_accessible :: Term -> StaticEnv -> Term
mk_accessible (Lvar name) env | elem name env = Send Nil name []
mk_accessible term        _   = term

mk_alias :: Term -> Term -> Term
mk_alias = Alias

mk_arg = error "mk_arg"
mk_args = error "mk_args"
mk_array = error "mk_array"
mk_assign = error "mk_assign"
mk_assignable = error "mk_assignable"
mk_associate = error "mk_associate"
mk_attr_asgn = error "mk_attr_asgn"

mk_back_ref, mk_nth_ref :: Token -> Term
mk_back_ref (TBACK_REF s) = BackRef s
mk_nth_ref  (TNTH_REF i)  = NthRef i

mk_begin = error "mk_begin"

mk_begin_body :: Term -> [Term] -> Term -> Term -> Term
mk_begin_body stmt rescues els ensure = Begin [stmt] -- TODO

mk_begin_keyword :: Term -> Term
mk_begin_keyword term = KWBegin [term] -- TODO

mk_binary_op = error "mk_binary_op"
mk_block = error "mk_block"
mk_block_pass = error "mk_block_pass"
mk_blockarg = error "mk_blockarg"
mk_call_lambda = error "mk_call_lambda"
mk_call_method = error "mk_call_method"
mk_case = error "mk_case"

mk_character :: Token -> Term
mk_character (TCHARACTER c)  = Str [c]

mk_complex = error "mk_complex"
mk_condition = error "mk_condition"
mk_condition_mod = error "mk_condition_mod"

mk_const_fetch :: Term -> Token -> Term
mk_const_fetch first (TCONSTANT second) = Const first second

mk_const_global = error "mk_const_global"
mk_const_op_assignable = error "mk_const_op_assignable"

mk_cvar, mk_gvar, mk_ivar, mk_const, mk_ident :: Token -> Term
mk_cvar (TCVAR i)        = Cvar i
mk_gvar (TGVAR i)        = Gvar i
mk_ivar (TIVAR i)        = Ivar i
mk_const (TCONSTANT i)   = Const Nil i
mk_ident (TIDENTIFIER i) = Lvar i

mk_def_class = error "mk_def_class"
mk_def_module = error "mk_def_module"
mk_def_sclass = error "mk_def_sclass"

mk_def_singleton :: Term -> Token -> args -> body -> Term
mk_def_singleton singleton fname args body = Defs singleton (value fname) (Args []) Nil

mk_def_method :: Token -> args -> body -> Term
mk_def_method fname args body = Def (value fname) (Args []) Nil

mk_float :: Token -> Term
mk_float (TFLOAT f) = RFloat f

mk_for = error "mk_for"
mk_index = error "mk_index"
mk_index_asgn = error "mk_index_asgn"

mk_integer :: Token -> Term
mk_integer (TINTEGER i) = RInt i


mk_keyword_cmd :: Term -> a -> Term
mk_keyword_cmd term args = error ("mk_keyword_cmd" ++ show term)

mk_kwarg = error "mk_kwarg"
mk_kwoptarg = error "mk_kwoptarg"
mk_kwrestarg = error "mk_kwrestarg"
mk_kwsplat = error "mk_kwsplat"
mk_loop = error "mk_loop"
mk_loop_mod = error "mk_loop_mod"
mk_match_op = error "mk_match_op"
mk_multi_lhs = error "mk_multi_lhs"
mk_not_op = error "mk_not_op"
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
mk_shadowarg = error "mk_shadowarg"
mk_splat = error "mk_splat"

mk_string_compose :: [Term] -> Term
mk_string_compose [t]   = t
mk_string_compose ts    = Dstr ts

mk_string, mk_string_internal :: Token -> Term
mk_string = mk_string_internal
mk_string_internal (TSTRING s) = Str s

mk_symbol_compose :: [Term] -> Term
mk_symbol_compose = error "mk_symbol_compose"

mk_symbol, mk_symbol_internal :: Token -> Term
mk_symbol = mk_symbol_internal
mk_symbol_internal (TSYMBOL s) = Sym s
mk_symbol_internal (TIDENTIFIER s) = Sym s

mk_symbols_compose = error "mk_symbols_compose"
mk_ternary = error "mk_ternary"
mk_unary_num = error "mk_unary_num"
mk_unary_op = error "mk_unary_op"
mk_undef_method = error "mk_undef_method"
mk_when = error "mk_when"
mk_word = error "mk_word"
mk_words_compose = error "mk_words_compose"
mk_xstring_compose = error "mk_xstring_compose"

-- mk_block' (begin_t, args, body, end_t) = mk_block (mk_call_method $1 $2 $3 $4) begin_t Args body end_t
mk_block' = error "mk_block"


--- private

value :: Token -> String
value (TIDENTIFIER i) = i
value (TCONSTANT i)   = i
