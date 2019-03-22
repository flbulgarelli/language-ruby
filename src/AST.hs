-- {-# LANGUAGE ExistentialQuantification, RankNTypes #-}

module AST where

import Data.Ratio (Rational)
import Data.Complex
import Lexer (Token)

data Term =
       Empty
       | Begin
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
       deriving (Eq, Show)


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

