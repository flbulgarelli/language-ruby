-- {-# LANGUAGE ExistentialQuantification, RankNTypes #-}

module AST where

import Data.Ratio (Rational)
import Data.Complex

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
       deriving (Eq, Show)

-- converts a list of expressions into a single expression

mk_def_singleton = undefined
mkExpression = undefined
mk_def_sclass = undefined
mk_preexe = undefined
mk_condition_mod = undefined
mk_begin_body = undefined
mk_loop_mod = undefined
mkLogicalOp = undefined
mk_rescue_body = undefined
mk_not_op = undefined
