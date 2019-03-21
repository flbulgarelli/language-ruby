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
       | STrue
       | SFalse
       | SZero
       | SIsZero Term
       | SSucc Term
       | SPred Term
       | SIfThen Term Term Term
       deriving (Eq, Show)
