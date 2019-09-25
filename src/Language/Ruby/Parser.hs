module Language.Ruby.Parser (parseRuby) where

import Codec.Binary.UTF8.String (encode)
import Language.Ruby.Parser.Parser (parse)
import Language.Ruby.Parser.Lexer (evalP)
import Language.Ruby.AST (Term)

parseRuby :: String -> Either String Term
parseRuby = evalP parse . encode

