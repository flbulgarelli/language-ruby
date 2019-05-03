-- {-# LANGUAGE ExistentialQuantification, RankNTypes #-}

module AST where

import Data.Ratio (Rational)
import Data.Complex
import Lexer (Token (..))

data Term
       = Begin [Term]
       -- | RComplex (forall a. Num a => Complex (a))
       | Alias Term Term
       | And Term Term
       | Anddot
       | BackRef String
       | Break [Term]
       | Case [Term]
       | Casgn Term String (Maybe Term) -- constants
       | Cbase
       | Const Term String
       | Cvar String
       | Cvasgn String (Maybe Term) -- class variable
       | Def String Args Term
       | Defined [Term]
       | Defs Term String Args Term
       | Dot
       | Dstr [Term]
       | Dsym [Term]
       | Encoding
       | Ensure Term Term
       | ERange Term Term
       | File
       | For Term Term Term
       | Gvar String
       | Gvasgn String (Maybe Term) -- global variables
       | Hash [Term]
       | If Term Term Term
       | IRange Term Term
       | Ivar String
       | Ivasgn String (Maybe Term) -- instance variables
       | KWBegin [Term]
       | KWSplat Term
       | Line
       | Lvar String
       | Lvasgn String (Maybe Term) -- variables
       | Masgn Term Term
       | Mlhs [Term]
       | Next [Term]
       | Nil
       | NthRef Int
       | Pair Term Term
       | Postexe Term
       | Or Term Term
       | RArray [Term]
       | RComplex (Complex Double)
       | Redo [Term]
       | Retry [Term]
       | Return [Term]
       | RFalse
       | RFloat Double
       | RInt Int
       | RRational Rational
       | RTrue
       | Self
       | Send Term String [Term]
       | Csend Term String [Term]
       | Resbody Term Term Term
       | Rescue Term [Term]
       | Splat (Maybe Term)
       | Str String
       | Super [Term]
       | Sym String
       | Undef [Term]
       | Until Term Term
       | UntilPost Term Term
       | When [Term]
       | While Term Term
       | WhilePost Term Term
       | Yield [Term]
       | Zsuper [Term]
       deriving (Eq, Show)

data Args = Args [Term] deriving (Eq, Show)


lvasgn name = Lvasgn name . Just
ivasgn name = Ivasgn name . Just
cvasgn name = Cvasgn name . Just
gvasgn name = Gvasgn name . Just
casgn name parent = Casgn name parent . Just

type StaticEnv = [String]

mkLogicalOp :: (Term -> Term -> Term) -> Term -> Term -> Term
mkLogicalOp = id

mkExpression :: [Term] -> Term
mkExpression []  = Nil
mkExpression [e] = e
mkExpression xs  = Begin xs

mk_multiassign :: Term -> Term -> Term
mk_multiassign = Masgn

mk_postexe :: Term -> Term
mk_postexe = Postexe

mk_accessible :: Term -> StaticEnv -> Term
mk_accessible (Lvar name) env | elem name env = Send Nil name []
mk_accessible term        _   = term

mk_alias :: Term -> Term -> Term
mk_alias = Alias

mk_arg = error "mk_arg"
mk_args = error "mk_args"

mk_array :: [Term] -> Term
mk_array = RArray

mk_assign :: Term -> Term -> Term
mk_assign = mk_op_assign -- FIXME maybe mk_op_assign should be removed. Maybe also the corresponding grammar rules should be dropped

mk_assignable :: Term -> Term
mk_assignable (Lvar i) = Lvasgn i Nothing
mk_assignable (Ivar i) = Ivasgn i Nothing
mk_assignable (Cvar i) = Cvasgn i Nothing
mk_assignable (Gvar i) = Gvasgn i Nothing
mk_assignable (Const parent i) = Casgn parent i Nothing

mk_associate :: [Term] -> Term
mk_associate = Hash

mk_attr_asgn = error "mk_attr_asgn"

mk_back_ref, mk_nth_ref :: Token -> Term
mk_back_ref (TBACK_REF s) = BackRef s
mk_nth_ref  (TNTH_REF i)  = NthRef i

mk_begin = error "mk_begin"

mk_begin_body' :: Term -> [Term] -> Term
mk_begin_body' compoundStmt []            = compoundStmt
mk_begin_body' compoundStmt rescue_bodies = Rescue compoundStmt $ rescue_bodies ++ [Nil]

mk_begin_body :: Term -> [Term] -> Term -> Term -> Term
mk_begin_body Nil [] els ensure                     = wrap_in_ensure [] els ensure 
mk_begin_body (Begin terms) [] els ensure           = wrap_in_ensure terms els ensure
mk_begin_body compoundStmt [] els ensure            = wrap_in_ensure [compoundStmt] els ensure
mk_begin_body compoundStmt rescue_bodies els ensure = Ensure (Rescue compoundStmt $ rescue_bodies ++ [els]) ensure 

wrap_in_ensure terms els ensure = Ensure (Begin $ terms ++ [Begin [els]]) ensure

mk_begin_keyword :: Term -> Term
mk_begin_keyword Nil        = KWBegin []
mk_begin_keyword (Begin ts) = KWBegin ts
mk_begin_keyword t          = KWBegin [t]

mk_binary_op :: Term -> String -> Term -> Term
mk_binary_op receiver op arg = Send receiver op [arg]

mk_block = error "mk_block"
mk_block_pass = error "mk_block_pass"
mk_blockarg = error "mk_blockarg"
mk_call_lambda = error "mk_call_lambda"

mk_call_method :: Term -> Token -> Token -> [Term] -> Term
mk_call_method receiver TANDDOT selector args = Csend receiver (mk_selector selector) args
mk_call_method receiver _       selector args = Send receiver (mk_selector selector) args

mk_case :: Term -> [Term] -> Term
mk_case expr bodies = Case $ expr : bodies

mk_character :: Token -> Term
mk_character (TCHARACTER c)  = Str [c]

mk_complex = error "mk_complex"

mk_condition :: Term -> Term -> Term -> Term
mk_condition = If . check_condition

check_condition :: Term -> Term
check_condition (Begin [term])    = Begin [check_condition term]
check_condition (And lhs rhs)     = And (check_condition lhs) (check_condition rhs)
check_condition (Or lhs rhs)      = Or (check_condition lhs) (check_condition rhs)
--check_condition (Regexp)        = 
check_condition condition         = condition

mk_condition_mod :: Term -> Term -> Term -> Term
mk_condition_mod ifTrue ifFalse cond = If (check_condition cond) ifTrue ifFalse

mk_const_fetch :: Term -> Token -> Term
mk_const_fetch first (TCONSTANT second) = Const first second

mk_const_global = error "mk_const_global"

mk_const_op_assignable :: Term -> Term
mk_const_op_assignable (Const parent i) = Casgn parent i Nothing

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

mk_for :: Term -> Term -> Term -> Term
mk_for = For

mk_index = error "mk_index"
mk_index_asgn = error "mk_index_asgn"

mk_integer :: Token -> Term
mk_integer (TINTEGER i) = RInt i


mk_keyword_cmd :: ([Term] -> Term) -> [Term] -> Term
mk_keyword_cmd f args = f args -- TODO check for yield with block

mk_kwarg = error "mk_kwarg"
mk_kwoptarg = error "mk_kwoptarg"
mk_kwrestarg = error "mk_kwrestarg"

mk_kwsplat :: Term -> Term
mk_kwsplat = KWSplat

mk_loop :: (Term -> Term -> Term) -> Term -> Term -> Term
mk_loop = id

mk_loop_mod :: Term -> Token -> Term -> Term
mk_loop_mod body@(KWBegin _) KWHILE cond = WhilePost (check_condition cond) body
mk_loop_mod body@(KWBegin _) KUNTIL cond = UntilPost (check_condition cond) body
mk_loop_mod body             KWHILE cond = While (check_condition cond) body
mk_loop_mod body             KUNTIL cond = Until (check_condition cond) body

mk_match_op = error "mk_match_op"

mk_multi_lhs :: [Term] -> Term
mk_multi_lhs = Mlhs

mk_not_op :: Term -> Term
mk_not_op Nil   = Send (Begin []) "!" []
mk_not_op expr  = Send (check_condition expr) "!" []

mk_op_assign :: Term -> Term -> Term
mk_op_assign (Lvasgn i Nothing) val = lvasgn i val
mk_op_assign (Ivasgn i Nothing) val = ivasgn i val
mk_op_assign (Cvasgn i Nothing) val = cvasgn i val
mk_op_assign (Gvasgn i Nothing) val = gvasgn i val
mk_op_assign (Casgn i parent Nothing) val = casgn i parent val
mk_op_assign t1 t2 = error ("mk_op_assign" ++ show t1 ++ " " ++ show t2)

mk_optarg = error "mk_optarg"

mk_pair :: Term -> Term -> Term
mk_pair = Pair

mk_pair_keyword = error "mk_pair_keyword"
mk_pair_quoted = error "mk_pair_quoted"

mk_preexe = error "mk_preexe"
mk_procarg0 = error "mk_procarg0"

mk_range_exclusive :: Term -> Term -> Term
mk_range_exclusive = ERange

mk_range_inclusive :: Term -> Term -> Term
mk_range_inclusive = IRange

mk_rational = error "mk_rational"
mk_regexp_compose = error "mk_regexp_compose"
mk_regexp_options = error "mk_regexp_options"

mk_rescue_body :: Term -> Term -> Term -> Term
mk_rescue_body = Resbody

mk_restarg = error "mk_restarg"
mk_shadowarg = error "mk_shadowarg"

mk_splat :: Term -> Term
mk_splat Nil  = Splat Nothing
mk_splat term = Splat . Just $ term

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

mk_ternary :: Term -> Term -> Term -> Term
mk_ternary = mk_condition

mk_unary_num = error "mk_unary_num"

mk_unary_op :: String -> Term -> Term
mk_unary_op op receiver = Send receiver op []

mk_undef_method :: [Term] -> Term
mk_undef_method = Undef

mk_when :: [Term] -> Term -> Term
mk_when cases body = When $ cases ++ [body]

mk_word = error "mk_word"
mk_words_compose = error "mk_words_compose"
mk_xstring_compose = error "mk_xstring_compose"

-- mk_block' (begin_t, args, body, end_t) = mk_block (mk_call_method $1 $2 $3 $4) begin_t Args body end_t
mk_block' = error "mk_block"


--- private

value :: Token -> String
value (TIDENTIFIER i) = i
value (TCONSTANT i)   = i
value other           = error (show other)

mk_selector :: Token -> String
mk_selector KNIL = "call"
mk_selector s   = value s
