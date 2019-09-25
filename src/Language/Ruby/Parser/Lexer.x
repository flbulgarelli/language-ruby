{
module Language.Ruby.Parser.Lexer (Token(..),P,evalP,lexer, tokens) where
import Control.Monad.State
import Control.Monad.Error
import Data.Word
import Codec.Binary.UTF8.String as UTF8 (encode, decode)

import qualified Data.Map as Map
import Control.Monad (liftM)
import Data.List (foldl')
import Numeric (readHex, readOct)

}


-- character sets
$lf = \n  -- line feed
$cr = \r  -- carriage return
$eol_char = [$lf $cr] -- any end of line character
$not_eol_char = ~$eol_char -- anything but an end of line character
$white_char   = [\ \n\r\f\v\t]
$white_no_nl = $white_char # $eol_char
$ident_letter = [a-zA-Z_]
$upper_letter = [A-Z]
$lower_letter = [a-z_]
$digit    = 0-9
$non_zero_digit = 1-9
$oct_digit = 0-7
$hex_digit = [$digit a-fA-F]
$bin_digit = 0-1
$short_str_char = [^ \n \r ' \" \\]
$long_str_char = [. \n] # [' \"]
$short_byte_str_char = \0-\127 # [\n \r ' \" \\]
$long_byte_str_char = \0-\127 # [' \"]
$not_single_quote = [. \n] # '
$not_double_quote = [. \n] # \"

-- macro definitions
@exponent = (e | E) (\+ | \-)? $digit+
@fraction = \. $digit+
@int_part = $digit+
@point_float = (@int_part? @fraction) | @int_part \.
@exponent_float = (@int_part | @point_float) @exponent
@float_number = @point_float | @exponent_float
@eol_pattern = $lf | $cr $lf | $cr $lf
@one_single_quote = ' $not_single_quote
@two_single_quotes = '' $not_single_quote
@one_double_quote = \" $not_double_quote
@two_double_quotes = \"\" $not_double_quote
@byte_str_prefix = b | B
@raw_str_prefix = r | R
@unicode_str_prefix = u | U
@format_str_prefix = f | F
@raw_byte_str_prefix = @byte_str_prefix @raw_str_prefix | @raw_str_prefix @byte_str_prefix
@format_raw_str_prefix = @format_str_prefix @raw_str_prefix | @raw_str_prefix @format_str_prefix
@backslash_pair = \\ (\\|'|\"|@eol_pattern|$short_str_char)
@backslash_pair_bs = \\ (\\|'|\"|@eol_pattern|$short_byte_str_char)
@short_str_item_single = $short_str_char|@backslash_pair|\"
@short_str_item_double = $short_str_char|@backslash_pair|'
@short_byte_str_item_single = $short_byte_str_char|@backslash_pair_bs|\"
@short_byte_str_item_double = $short_byte_str_char|@backslash_pair_bs|'
@long_str_item_single = $long_str_char|@backslash_pair|@one_single_quote|@two_single_quotes|\"
@long_str_item_double = $long_str_char|@backslash_pair|@one_double_quote|@two_double_quotes|'
@long_byte_str_item_single = $long_byte_str_char|@backslash_pair_bs|@one_single_quote|@two_single_quotes|\"
@long_byte_str_item_double = $long_byte_str_char|@backslash_pair_bs|@one_double_quote|@two_double_quotes|'

tokens :-

  $white+  ;  -- skip whitespace

  ' @short_str_item_single* ' { mkString }
  \" @short_str_item_double* \" { mkString }

  ("+" | "-")? @float_number { token TFLOAT readFloat }
  ("+" | "-")? $non_zero_digit $digit* { token TINTEGER readInt }
  ("+" | "-")? (@float_number | @int_part) (j | J) { token TIMAGINARY (readFloat . init) }
  0+ $oct_digit* { token TINTEGER (fst . head . readOct) }
  0 (o | O) $oct_digit+ { token TINTEGER read }
  0 (x | X) $hex_digit+ { token TINTEGER read }

--    "("   { openParen TLPAREN }
--    ")"   { closeParen TRPAREN }
--    "["   { openParen TLBRACK }
--    "]"   { closeParen TRBRACK }
--    "{"   { openParen TLCURLY }
--    "}"   { closeParen TRCURLY }
--  "->"  { symbolToken RightArrowToken }
    "."   { symbolToken TDOT }
    ".."   { symbolToken TDOT2 }
    "..." { symbolToken TDOT3 }
    "~"   { symbolToken TTILDE }
    "+"   { symbolToken TPLUS }
    "-"   { symbolToken TMINUS }
    "*"   { symbolToken TSTAR }
    "**"  { symbolToken TSTAR2 }
    "/"   { symbolToken TDIVIDE }
    "%"   { symbolToken TPERCENT }
    "<<"  { symbolToken TLSHFT }
    ">>"  { symbolToken TRSHFT }
  -- "<"   { symbolToken TLE }
    "<="  { symbolToken TLEQ }
    ">"   { symbolToken TGT }
    ">="  { symbolToken TGEQ }
    "=="  { symbolToken TEQ }
    "==="  { symbolToken TEQQ }
    "!="  { symbolToken TNEQ }
    "^"   { symbolToken TCARET }
    "|"   { symbolToken TPIPE }
    "&&"  { symbolToken TAMPER2 }
    "&"   { symbolToken TAMPER }
    "&."  { symbolToken TANDDOT }
  -- "||"  { symbolToken OrToken }
     ":"   { symbolToken TCOLON }
    "::"   { symbolToken TCOLON2 }
    ":::"  { symbolToken TCOLON3 }

    "'"   { symbolToken (TSTRING_BEG False) }
    "<<'" { symbolToken (TSTRING_BEG False) }
    "%q"  { symbolToken (TSTRING_BEG False) }
    """   { symbolToken (TSTRING_BEG True) }
    "<<"" { symbolToken (TSTRING_BEG True) }
    "%"   { symbolToken (TSTRING_BEG True) }
    "%Q"  { symbolToken (TSTRING_BEG True) }
    "%w"  { symbolToken (TQWORDS_BEG False) }
    "%W"  { symbolToken (TWORDS_BEG True)  }
    "%i"  { symbolToken (TQSYMBOLS_BEG False) }
    "%I"  { symbolToken (TSYMBOLS_BEG True)  }
    ":""  { symbolToken (TSYMBEG False) }
    "%s"  { symbolToken (TSYMBEG False) }
    ":""  { symbolToken (TSYMBEG True)  }
    "/"   { symbolToken (TREGEXP_BEG True)  }
    "%r"  { symbolToken (TREGEXP_BEG True)  }
    "%x"  { symbolToken (TXSTRING_BEG True)  }
    "`"   { symbolToken (TXSTRING_BEG True)  }
    "<<`" { symbolToken (TXSTRING_BEG True)  }


    "="   { symbolToken TEQL }
    ","   { symbolToken TCOMMA }
    \;    { symbolToken TSEMI }

  ":" $ident_letter($ident_letter|$digit)*            { mkSymbol }
  $lower_letter($ident_letter|$digit)*("?"|"!"|"=")?  { keywordOrIdent } -- FIXME not true for Lvars
  $upper_letter($ident_letter|$digit)*                { token TCONSTANT id }
  "$" $ident_letter($ident_letter|$digit)*            { token TGVAR id }
  "$" ($digit | $non_zero_digit($digit)*)             { token TNTH_REF (read . drop 1) }
  "$" ("&" | "`" | "'" | "+" )                        { token TBACK_REF id }
  "@" $ident_letter($ident_letter|$digit)*            { token TIVAR id }
  "@@" $ident_letter($ident_letter|$digit)*           { token TCVAR id }
  "?" $short_str_char                                 { token TCHARACTER last }

{
data Token =
  KCLASS
  | KMODULE
  | KDEF
  | KUNDEF
  | KBEGIN
  | KRESCUE
  | KENSURE
  | KEND
  | KIF
  | KUNLESS
  | KTHEN
  | KELSIF
  | KELSE
  | KCASE
  | KWHEN
  | KWHILE
  | KUNTIL
  | KFOR
  | KBREAK
  | KNEXT
  | KREDO
  | KRETRY
  | KIN
  | KDO
  | KDO_COND
  | KDO_BLOCK
  | KDO_LAMBDA
  | KRETURN
  | KYIELD
  | KSUPER
  | KSELF
  | KNIL
  | KTRUE
  | KFALSE
  | KAND
  | KOR
  | KNOT
  | KIF_MOD
  | KUNLESS_MOD
  | KWHILE_MOD
  | KUNTIL_MOD
  | KRESCUE_MOD
  | KALIAS
  | KDEFINED
  | KlBEGIN
  | KlEND
  | K__LINE__
  | K__FILE__
  | K__ENCODING__
  | TIDENTIFIER String
  | TFID
  | TGVAR String
  | TIVAR String
  | TCONSTANT String
  | TLABEL -- hashes labels bareword + !/?
  | TLOWEST -- ??
  | TCVAR String
  | TNTH_REF Int
  | TBACK_REF String
  | TSTRING_CONTENT
  | TINTEGER Int
  | TFLOAT Double
  | TUPLUS
  | TUMINUS
  | TUNARY_NUM -- maybe could be removed
  | TPOW
  | TCMP
  | TEQ
  | TEQL -- ????
  | TEQQ
  | TNEQ
  | TGEQ
  | TLEQ
  | TANDOP
  | TOROP
  | TMATCH
  | TNMATCH
  | TDOT
  | TDOT2
  | TDOT3
  | TAREF
  | TASET
  | TLSHFT
  | TRSHFT
  | TCOLON2
  | TCOLON3
  | TOP_ASGN
  | TASSOC
  | TLPAREN
  | TLPAREN2
  | TRPAREN
  | TLPAREN_ARG
  | TLBRACK
  | TLBRACK2
  | TRBRACK
  | TLBRACE
  | TLBRACE_ARG
  | TSTAR
  | TSTAR2
  | TAMPER
  | TAMPER2
  | TTILDE
  | TPERCENT
  | TDIVIDE
  | TDSTAR
  | TPLUS
  | TMINUS
  | TLT
  | TGT
  | TPIPE
  | TBANG
  | TCARET
  | TLCURLY
  | TRCURLY
  | TBACK_REF2
  | TSYMBEG Bool
  | TSTRING_BEG Bool
  | TXSTRING_BEG Bool
  | TREGEXP_BEG Bool
  | TREGEXP_OPT
  | TWORDS_BEG Bool
  | TQWORDS_BEG Bool
  | TSYMBOLS_BEG Bool
  | TQSYMBOLS_BEG Bool
  | TSTRING_DBEG
  | TSTRING_DVAR
  | TSTRING_END
  | TSTRING_DEND
  | TSTRING String
  | TSYMBOL String
  | TNL
  | TEH
  | TCOLON
  | TCOMMA
  | TSPACE
  | TSEMI
  | TLAMBDA
  | TLAMBEG
  | TCHARACTER Char
  | TRATIONAL
  | TIMAGINARY Double
  | TLABEL_END
  | TANDDOT
  | TMETHREF
  | TEOF
  deriving (Eq,Show)

-- The functions that must be provided to Alex's basic interface
type AlexInput = [Word8]
alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte (b:bs) = Just (b,bs)
alexGetByte []    = Nothing

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = undefined

-- Our Parser monad
type P a = StateT AlexInput (Either String) a

evalP::P a -> AlexInput -> Either String a
evalP = evalStateT

-- Action to read a token
readToken::P Token
readToken = do
    s <- get
    case alexScan s 0 of
      AlexEOF -> return TEOF
      AlexError _ -> throwError "!Lexical error"
      AlexSkip inp' _ -> do
        put inp'
        readToken
      AlexToken inp' len action -> do
        put inp'
        (action (take len (decode s)))

-- The lexer function to be passed to Happy
lexer::(Token -> P a)->P a
lexer cont = readToken >>= cont

tokens :: String -> [Token]
tokens = loop . encode
  where
    run = runStateT (lexer return)
    loop input = case run input of
                  Right (TEOF, _) -> []
                  Right (t, tail) -> t : loop tail
                  _               -> []

----------

type Action = String -> P Token

readFloat :: String -> Double
readFloat str@('.':cs) = read ('0':readFloatRest str)
readFloat str = read (readFloatRest str)
readFloatRest :: String -> String
readFloatRest [] = []
readFloatRest ['.'] = ".0"
readFloatRest (c:cs) = c : readFloatRest cs

readInt :: String -> Int
readInt ('+':s) = read s
readInt s       = read s

token :: (a -> Token) -> (String -> a) -> Action
token mkToken read str = return $ mkToken (read str)

symbolToken :: Token -> Action
symbolToken t _ = return t

mkString :: Action
mkString = token TSTRING (\str -> drop 1 . take (length str - 1) $ str)

mkSymbol :: Action
mkSymbol = token TSYMBOL (drop 1)

-------------


-- a keyword or an identifier (the syntax overlaps)
keywordOrIdent :: String -> P Token
keywordOrIdent str
   = return $ case Map.lookup str keywords of
         Just symbol -> symbol
         Nothing -> TIDENTIFIER str

-- mapping from strings to keywords
keywords :: Map.Map String Token
keywords = Map.fromList keywordNames

keywordNames :: [(String, Token)]
keywordNames =
   [
    ("class", KCLASS),
    ("module", KMODULE),
    ("def", KDEF),
    ("undef", KUNDEF),
    ("begin", KBEGIN),
    ("rescue", KRESCUE),
    ("ensure", KENSURE),
    ("end", KEND),
    ("if", KIF),
    ("unless", KUNLESS),
    ("then", KTHEN),
    ("elsif", KELSIF),
    ("else", KELSE),
    ("case", KCASE),
    ("when", KWHEN),
    ("while", KWHILE),
    ("until", KUNTIL),
    ("for", KFOR),
    ("break", KBREAK),
    ("next", KNEXT),
    ("redo", KREDO),
    ("retry", KRETRY),
    ("in", KIN),
    ("do", KDO),
    ("do_cond", KDO_COND),
    ("do_block", KDO_BLOCK),
    ("do_lambda", KDO_LAMBDA),
    ("return", KRETURN),
    ("yield", KYIELD),
    ("super", KSUPER),
    ("self", KSELF),
    ("nil", KNIL),
    ("true", KTRUE),
    ("false", KFALSE),
    ("and", KAND),
    ("or", KOR),
    ("not", KNOT),
    ("if_mod", KIF_MOD),
    ("unless_mod", KUNLESS_MOD),
    ("while_mod", KWHILE_MOD),
    ("until_mod", KUNTIL_MOD),
    ("rescue_mod", KRESCUE_MOD),
    ("alias", KALIAS),
    ("defined?", KDEFINED),
    ("__LINE__", K__LINE__),
    ("__FILE__", K__FILE__),
    ("__ENCODING__", K__ENCODING__)
   ]

}
