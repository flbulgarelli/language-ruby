-- {-# LANGUAGE ExistentialQuantification, RankNTypes #-}

module Language.Ruby.AST (Term(..)) where

import Data.Ratio (Rational)
import Data.Complex

data Term
       = Begin [Term]
       -- | RComplex (forall a. Num a => Complex (a))
       | Alias Term Term
       | And Term Term
       | Anddot
       | Arg String
       | Args [Term]
       | BackRef String
       | BlockArg String
       | BlockPass Term
       | Break [Term]
       | Case [Term]
       | Casgn Term String (Maybe Term) -- constants
       | Cbase
       | Const Term String
       | Cvar String
       | Cvasgn String (Maybe Term) -- class variable
       | Def String Term Term
       | Defined [Term]
       | Defs Term String Term Term
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
       | Index Term [Term]
       | IndexAsgn Term [Term]
       | If Term Term Term
       | IRange Term Term
       | Ivar String
       | Ivasgn String (Maybe Term) -- instance variables
       | KWArg String
       | KWBegin [Term]
       | KWOptArg String Term
       | KWRestArg (Maybe String)
       | KWSplat Term
       | Lambda
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
       | Preexe Term
       | Or Term Term
       | OptArg String Term
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
       | RestArg (Maybe String)
       | ShadowArg String
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
