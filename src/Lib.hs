module Lib
    ( parseRuby
    ) where

import Codec.Binary.UTF8.String (encode)
import Parser (parse)
import Lexer (evalP)
import AST (Term)

parseRuby :: String -> Either String Term
parseRuby = evalP parse . encode

