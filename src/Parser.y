{
module Parser(parse) where
import AST
import qualified Lexer as L
import Control.Monad.Error
}

%monad{L.P}
%lexer{L.lexer}{L.TEOF}
%name parse
%tokentype{L.Token}
%error {parseError}

%token
    kCLASS {L.KCLASS}
    kMODULE {L.KMODULE}
    kDEF {L.KDEF}
    kUNDEF {L.KUNDEF}
    kBEGIN {L.KBEGIN}
    kRESCUE {L.KRESCUE}
    kENSURE {L.KENSURE}
    kEND {L.KEND}
    kIF {L.KIF}
    kUNLESS {L.KUNLESS}
    kTHEN {L.KTHEN}
    kELSIF {L.KELSIF}
    kELSE {L.KELSE}
    kCASE {L.KCASE}
    kWHEN {L.KWHEN}
    kWHILE {L.KWHILE}
    kUNTIL {L.KUNTIL}
    kFOR {L.KFOR}
    kBREAK {L.KBREAK}
    kNEXT {L.KNEXT}
    kREDO {L.KREDO }
    kRETRY {L.KRETRY}
    kIN {L.KIN}
    kDO {L.KDO}
    kDO_COND {L.KDO_COND}
    kDO_BLOCK {L.KDO_BLOCK}
    kDO_LAMBDA {L.KDO_LAMBDA}
    kRETURN {L.KRETURN}
    kYIELD {L.KYIELD}
    kSUPER {L.KSUPER}
    kSELF {L.KSELF}
    kNIL {L.KNIL}
    kTRUE {L.KTRUE}
    kFALSE {L.KFALSE}
    kAND {L.KAND}
    kOR {L.KOR}
    kNOT {L.KNOT}
    kIF_MOD {L.KIF_MOD}
    kUNLESS_MOD {L.KUNLESS_MOD}
    kWHILE_MOD {L.KWHILE_MOD}
    kUNTIL_MOD {l.KUNTIL_MOD}
    kRESCUE_MOD {L.KRESCUE_MOD}
    kALIAS {L.KALIAS}
    kDEFINED {L.KDEFINED}
    klBEGIN {L.KlBEGIN}
    klEND {L.KlEND}
    k__LINE__ {L.K__LINE__}
    k__FILE__ {L.K__FILE__}
    k__ENCODING__ {L.K__ENCODING__}
    tIDENTIFIER {L.TIDENTIFIER}
    tFID {L.TFID}
    tGVAR {L.TGVAR}
    tIVAR {L.TIVAR}
    tCONSTANT {L.TCONSTANT}
    tLABEL {L.TLABEL}
    tCVAR {L.TCVAR}
    tNTH_REF {L.TNTH_REF}
    tBACK_REF {L.TBACK_REF}
    tSTRING_CONTENT {L.TSTRING_CONTENT}
    tINTEGER {L.TINTEGER}
    tFLOAT {L.TFLOAT}
    tUPLUS {L.TUPLUS}
    tUMINUS {L.TUMINUS}
    tUNARY_NUM {L.TUNARY_NUM}
    tPOW {L.TPOW}
    tCMP {L.TCMP}
    tEQ {L.TEQ}
    tEQQ {L.TEQQ}
    tNEQ {L.TNEQ}
    tGEQ {t.GEQ}
    tLEQ {L.TLEQ}
    tANDOP {L.TANDOP}
    tOROP {L.TOROP}
    tMATCH {L.TMATCH}
    tNMATCH {L.TNMATCH}
    tDOT {L.TDOT}
    tDOT2 {L.TDOT2}
    tDOT3 {L.TDOT3}
    tAREF {L.TAREF}
    tASET {L.TASET}
    tLSHFT {L.TLSHFT}
    tRSHFT {L.TRSHFT}
    tCOLON2 {L.TCOLON2}
    tCOLON3 {L.TCOLON3}
    tOP_ASGN {L.TOP_ASGN}
    tASSOC {L.TASSOC}
    tLPAREN {L.TLPAREN}
    tLPAREN2 {L.TLPAREN2}
    tRPAREN {L.TRPAREN}
    tLPAREN_ARG {L.TLPAREN_ARG}
    tLBRACK {L.TLBRACK}
    tLBRACK2 {L.TLBRACK2}
    tRBRACK {L.TRBRACK}
    tLBRACE {L.TLBRACE}
    tLBRACE_ARG {T.LBRACE_ARG}
    tSTAR {L.TSTAR}
    tSTAR2 {L.TSTAR2}
    tAMPER {L.TAMPER}
    tAMPER2 {L.TAMPER2}
    tTILDE {L.TTILDE}
    tPERCENT {L.TPERCENT}
    tDIVIDE {L.TDIVIDE}
    tDSTAR {L.TDSTAR}
    tPLUS {L.TPLUS}
    tMINUS {L.TMINUS}
    tLT {L.TLT}
    tGT {L.TGT}
    tPIPE {L.TPIPE}
    tBANG {L.TBANG}
    tCARET {L.TCARET}
    tLCURLY {L.TLCURLY}
    tRCURLY {L.TRCURLY}
    tBACK_REF2 {L.TBACK_REF2}
    tSYMBEG {L.TSYMBEG}
    tSTRING_BEG {L.TSTRING_BEG}
    tXSTRING_BEG {L.TXSTRING_BEG}
    tREGEXP_BEG {L.TREGEXP_BEG}
    tREGEXP_OPT {L.TREGEXP_OPT}
    tWORDS_BEG {L.TWORDS_BEG }
    tQWORDS_BEG {L.TQWORDS_BEG}
    tSYMBOLS_BEG {L.TSYMBOLS_BEG}
    tQSYMBOLS_BEG {L.TQSYMBOLS_BEG}
    tSTRING_DBEG {L.TSTRING_DBEG}
    tSTRING_DVAR {L.TSTRING_DVAR}
    tSTRING_END {L.TSTRING_END}
    tSTRING_DEND {L.TSTRING_DEND}
    tSTRING {L.TSTRING}
    tSYMBOL {L.TSYMBOL}
    tNL {L.TNL}
    tEH {L.TEH}
    tCOLON {L.TCOLON}
    tCOMMA {L.TCOMMA}
    tSPACE {L.TSPACE}
    tSEMI {L.TSEMI}
    tLAMBDA {L.TLAMBDA}
    tLAMBEG {L.TLAMBEG}
    tCHARACTER {L.TCHARACTER}
    tRATIONAL {L.TRATIONAL}
    tIMAGINARY {L.TIMAGINARY}
    tLABEL_END {L.TLABEL_END}
    tANDDOT {L.TANDDOT}
    tMETHREF {L.TMETHREF}

%%

KeywordVariable : kNIL {Nil}
   | kSELF {Self}
   | kTRUE {RTrue}
   | kFALSE {RFalse}
-- | k__FILE__ {__FILE__}
-- | k__LINE__ {__LINE__}
-- | k__ENCODING__ {__ENCODING__}


Program: TopCompstmt { $0 }
TopCompstmt: TopStmts OptTerms { mkExpression $0 }
TopStmts: -- nothing { Nil }
  TopStmt { [ $0 ] }
  | TopStmts Terms TopStmt { $0 ++ [$2] }
  | error TopStmt { [$1] }

TopStmt: Stmt { $0 } | klBEGIN BeginBlock { Preexe $0 $1 }

BeginBlock: tLCURLY TopCompstmt tRCURLY { val }

{-
bodystmt: Compstmt opt_rescue OptElse opt_ensure {
            rescue_bodies     = $1
            else_t,   else_   = $2
            ensure_t, ensure_ = $3

            if rescue_bodies.empty? && !else_.Nil?
              error ":useless_else, Nil, else_t"
            end

            mk_begin_body($0,
                        rescue_bodies,
                        else_t,   else_,
                        ensure_t, ensure_) }
-}

Compstmt: Stmts OptTerms { mkExpression $0 }
Stmts: -- nothing { [] }
  StmtOrBegin { [ $0 ] }
  | Stmts Terms StmtOrBegin { $0 ++ [$2] }
  | error Stmt {  [ $1 ] }


StmtOrBegin: Stmt { $0 }
  | klBEGIN BeginBlock { error ("begin_in_method " ++ show $0) }

Stmt: -- kALIAS Fitem { @lexer.state = :expr_fname } Fitem { mk_alias($0, $1, $3) }
  --  | kALIAS tGVAR tGVAR { mk_alias($0, mk_gvar($1), mk_gvar($2)) }
  --  | kALIAS tGVAR tBACK_REF { mk_alias($0, mk_gvar($1), mk_back_ref($2)) }
  --  | kALIAS tGVAR tNTH_REF { error ":nth_ref_alias, Nil, $2" }
  --  | kUNDEF UndefList { mk_undef_method($0, $1) }
    Stmt kIF_MOD Expr { ConditionMod $0 Nil $1 $2 }
    | Stmt kUNLESS_MOD Expr { ConditionMod Nil $0 $1 $2 }
    | Stmt kWHILE_MOD Expr { mk_loop_mod While $0 $1 $2 }
    | Stmt kUNTIL_MOD Expr { mk_loop_mod Until $0 $1 $2 }
    | Stmt kRESCUE_MOD Stmt {  mk_begin_body $0 [mk_rescue_body $1 Nil Nil Nil Nil $2] }
  --  | klEND tLCURLY Compstmt tRCURLY { Postexe $ $1 $2 $3 }
  --  | CommandAsgn
  --  | Mlhs tEQL CommandCall { MultiAssign $0 $1 $2 }
  --  | Lhs tEQL mrhs { mk_assign($0, $1, mk_array(Nil, $2, Nil)) }
  --  | Mlhs tEQL mrhs_arg { MultiAssign $0 $1 $2 }
  --  | Expr { $0 }

{-
CommandAsgn: Lhs tEQL CommandRhs { mk_assign($0, $1, $2) }
      | var_lhs tOP_ASGN CommandRhs { mk_op_assign($0, $1, $2) }
      | Primary tLBRACK2 opt_call_args RBracket tOP_ASGN CommandRhs {
            mk_op_assign(
                        mk_index(
                          $0, $1, $2, $3),
                        $4, val[5]) }
      | Primary CallOp tIDENTIFIER tOP_ASGN CommandRhs {
            mk_op_assign(
                        mk_call_method(
                          $0, $1, $2),
                        $3, $4) }
      | Primary CallOp tCONSTANT tOP_ASGN CommandRhs {
            mk_op_assign( mk_call_method( $0, $1, $2) $3, $4) }
      | Primary tCOLON2 tCONSTANT tOP_ASGN CommandRhs { mk_op_assign (mk_const_op_assignable (mk_const_fetch $0 $1 $2)) $3 $4 }
      | Primary tCOLON2 tIDENTIFIER tOP_ASGN CommandRhs {
            mk_op_assign(
                        mk_call_method(
                          $0, $1, $2),
                        $3, $4) }
      | backref tOP_ASGN CommandRhs {
            mk_op_assign($0, $1, $2) }

CommandRhs: CommandCall =tOP_ASGN
      | CommandCall kRESCUE_MOD Stmt { mk_begin_body $0 [mk_rescue_body $1 Nil Nil Nil Nil $2] }
      | CommandAsgn { $0 }
-}

Expr: CommandCall { $0 }
    | Expr kAND Expr { mkLogicalOp And $0 $1 $2 }
    | Expr kOR Expr { mkLogicalOp Or $0 $1 $2 }
    -- | kNOT OptNl Expr { mk_not_op $0 Nil $2 Nil) }
    | tBANG CommandCall { mk_not_op $0 Nil $1 Nil }
    -- | Arg

{-
ExprValueDo: --  { @lexer.cond.push(true) }
  Expr do { @lexer.cond.pop; [ $1, $2 ] }

-}

CommandCall: Command { $0 }
  -- | BlockCommand { $0 }

{-
BlockCommand: BlockCall { $0 }
  | BlockCall DotOrColon Operation2 CommandArgs { mk_call_method($0, $1, $2, Nil, $3, Nil) }

CmdBraceBlock: tLBRACE_ARG { @context.push(:block) } BraceBody tRCURLY { [ $0, *$2, $3 ] @context.pop }
-}

Command: KeywordVariable { $0 }
{-
Command: -- Operation CommandArgs =tLOWEST { mk_call_method(Nil, Nil, $0, Nil, $1, Nil) }
  --    | Operation CommandArgs CmdBraceBlock
  --  {
  --              MethodCall = mk_call_method(Nil, Nil, $0,
  --                                Nil, $1, Nil)
  --
  --              begin_t, args, body, end_t = $2
  --              result      = mk_block(MethodCall,
  --                              begin_t, args, body, end_t)
  --  }
  -- Primary CallOp Operation2 CommandArgs =tLOWEST { mk_call_method($0, $1, $2, Nil, $3, Nil) }
  Primary CallOp Operation2 CommandArgs CmdBraceBlock {
            MethodCall = mk_call_method($0, $1, $2,
                              Nil, $3, Nil)

            begin_t, args, body, end_t = $4
            result      = mk_block(MethodCall,
                            begin_t, args, body, end_t) }
      | Primary tCOLON2 Operation2 CommandArgs =tLOWEST {
            mk_call_method($0, $1, $2,
                        Nil, $3, Nil) }
      | Primary tCOLON2 Operation2 CommandArgs CmdBraceBlock {
            MethodCall = mk_call_method($0, $1, $2,
                              Nil, $3, Nil)

            begin_t, args, body, end_t = $4
            result      = mk_block(MethodCall,
                            begin_t, args, body, end_t) }
      | kSUPER CommandArgs {
            mk_keyword_cmd(:super, $0,
                        Nil, $1, Nil) }
      | kYIELD CommandArgs {
            mk_keyword_cmd Yield $0,
                        Nil, $1, Nil) }
      | KReturn call_args {
            mk_keyword_cmd Return, $0,
                        Nil, $1, Nil) }
      | kBREAK call_args {
            mk_keyword_cmd(:break, $0,
                        Nil, $1, Nil) }
      | kNEXT call_args {
            mk_keyword_cmd(:next, $0,
                        Nil, $1, Nil) }

Mlhs: MlhsBasic { mk_multi_lhs Nil $0 Nil }
    | tLPAREN MlhsInner rparen { mk_begin($0, $1, $2) }

MlhsInner: MlhsBasic {
            mk_multi_lhs(Nil, $0, Nil) }
      | tLPAREN MlhsInner rparen {
            mk_multi_lhs($0, $1, $2) }

MlhsBasic: MlhsHead
      | MlhsHead mlhs_item {
            $0.
                        push($1) }
      | MlhsHead tSTAR mlhs_node {
            $0.
                        push(mk_splat($1, $2)) }
      | MlhsHead tSTAR mlhs_node tCOMMA mlhs_post {
            $0.
                        push(mk_splat($1, $2)).
                        concat($4) }
      | MlhsHead tSTAR {
            $0.
                        push(mk_splat($1)) }
      | MlhsHead tSTAR tCOMMA mlhs_post {
            $0.
                        push(mk_splat($1)).
                        concat($3) }
      | tSTAR mlhs_node {
            [ mk_splat($0, $1) ] }
      | tSTAR mlhs_node tCOMMA mlhs_post {
            [ mk_splat($0, $1),
                        *$3 ] }
      | tSTAR {
            [ mk_splat($0) ] }
      | tSTAR tCOMMA mlhs_post {
            [ mk_splat($0),
                        *$2 ] }

mlhs_item: mlhs_node
      | tLPAREN MlhsInner rparen {
            mk_begin($0, $1, $2) }

MlhsHead: mlhs_item tCOMMA { [ $0 ] }
      | MlhsHead mlhs_item tCOMMA {
            $0 << $1 }

mlhs_post: mlhs_item { [ $0 ] }
      | mlhs_post tCOMMA mlhs_item {
            $0 << $2 }

mlhs_node: user_variable {
            mk_assignable($0) }
      | keyword_variable {
            mk_assignable($0) }
      | Primary tLBRACK2 opt_call_args RBracket {
            mk_index_asgn($0, $1, $2, $3) }
      | Primary CallOp tIDENTIFIER {
            mk_attr_asgn($0, $1, $2) }
      | Primary tCOLON2 tIDENTIFIER {
            mk_attr_asgn($0, $1, $2) }
      | Primary CallOp tCONSTANT {
            mk_attr_asgn($0, $1, $2) }
      | Primary tCOLON2 tCONSTANT {
            mk_assignable(
                        mk_const_fetch($0, $1, $2)) }
      | tCOLON3 tCONSTANT {
            mk_assignable(
                        mk_const_global($0, $1)) }
      | backref {
            mk_assignable($0) }

    Lhs: user_variable {
            mk_assignable($0) }
      | keyword_variable {
            mk_assignable($0) }
      | Primary tLBRACK2 opt_call_args RBracket {
            mk_index_asgn($0, $1, $2, $3) }
      | Primary CallOp tIDENTIFIER {
            mk_attr_asgn($0, $1, $2) }
      | Primary tCOLON2 tIDENTIFIER {
            mk_attr_asgn($0, $1, $2) }
      | Primary CallOp tCONSTANT {
            mk_attr_asgn($0, $1, $2) }
      | Primary tCOLON2 tCONSTANT {
            mk_assignable(
                        mk_const_fetch($0, $1, $2)) }
      | tCOLON3 tCONSTANT {
            mk_assignable(
                        mk_const_global($0, $1)) }
      | backref {
            mk_assignable($0) }

  cname: tIDENTIFIER {
            error ":module_name_const, Nil, $0" }
      | tCONSTANT

  cpath: tCOLON3 cname {
            mk_const_global($0, $1) }
      | cname {
            mk_const($0) }
      | Primary tCOLON2 cname {
            mk_const_fetch($0, $1, $2) }

  fname: tIDENTIFIER | tCONSTANT | tFID
      | op
      | reswords

  fsym: fname {
            mk_symbol($0) }
      | symbol

  Fitem: fsym
      | dsym

UndefList: Fitem { [ $0 ] }
      | UndefList tCOMMA {
            @lexer.state = :expr_fname }
          Fitem {
            $0 << $3 }

    op:   tPIPE    | tCARET  | tAMPER2  | tCMP  | tEQ     | tEQQ
      |   tMATCH   | tNMATCH | tGT      | tGEQ  | tLT     | tLEQ
      |   tNEQ     | tLSHFT  | tRSHFT   | tPLUS | tMINUS  | tSTAR2
      |   tSTAR    | tDIVIDE | tPERCENT | tPOW  | tBANG   | tTILDE
      |   tUPLUS   | tUMINUS | tAREF    | tASET | tDSTAR  | tBACK_REF2

reswords: k__LINE__ | k__FILE__ | k__ENCODING__ | klBEGIN | klEND
      | kALIAS    | kAND      | kBEGIN        | kBREAK  | kCASE
      | kCLASS    | kDEF      | kDEFINED      | kDO     | kELSE
      | kELSIF    | kEND      | kENSURE       | kFALSE  | kFOR
      | kIN       | kMODULE   | kNEXT         | kNIL    | kNOT
      | kOR       | kREDO     | kRESCUE       | kRETRY  | kRETURN
      | kSELF     | kSUPER    | kTHEN         | kTRUE   | kUNDEF
      | kWHEN     | kYIELD    | kIF           | kUNLESS | kWHILE
      | kUNTIL

    Arg: Lhs tEQL arg_rhs {
            mk_assign($0, $1, $2) }
      | var_lhs tOP_ASGN arg_rhs {
            mk_op_assign($0, $1, $2) }
      | Primary tLBRACK2 opt_call_args RBracket tOP_ASGN arg_rhs {
            mk_op_assign(
                        mk_index(
                          $0, $1, $2, $3),
                        $4, val[5]) }
      | Primary CallOp tIDENTIFIER tOP_ASGN arg_rhs {
            mk_op_assign(
                        mk_call_method(
                          $0, $1, $2),
                        $3, $4) }
      | Primary CallOp tCONSTANT tOP_ASGN arg_rhs {
            mk_op_assign(
                        mk_call_method(
                          $0, $1, $2),
                        $3, $4) }
      | Primary tCOLON2 tIDENTIFIER tOP_ASGN arg_rhs {
            mk_op_assign(
                        mk_call_method(
                          $0, $1, $2),
                        $3, $4) }
      | Primary tCOLON2 tCONSTANT tOP_ASGN arg_rhs {
            const  = mk_const_op_assignable(
                        mk_const_fetch($0, $1, $2))
            mk_op_assign(const, $3, $4) }
      | tCOLON3 tCONSTANT tOP_ASGN arg_rhs {
            const  = mk_const_op_assignable(
                        mk_const_global($0, $1))
            mk_op_assign(const, $2, $3) }
      | backref tOP_ASGN arg_rhs {
            mk_op_assign($0, $1, $2) }
      | Arg tDOT2 Arg {
            mk_range_inclusive($0, $1, $2) }
      | Arg tDOT3 Arg {
            mk_range_exclusive($0, $1, $2) }
      | Arg tDOT2 {
            mk_range_inclusive($0, $1, Nil) }
      | Arg tDOT3 {
            mk_range_exclusive($0, $1, Nil) }
      | Arg tPLUS Arg {
            mk_binary_op($0, $1, $2) }
      | Arg tMINUS Arg {
            mk_binary_op($0, $1, $2) }
      | Arg tSTAR2 Arg {
            mk_binary_op($0, $1, $2) }
      | Arg tDIVIDE Arg {
            mk_binary_op($0, $1, $2) }
      | Arg tPERCENT Arg {
            mk_binary_op($0, $1, $2) }
      | Arg tPOW Arg {
            mk_binary_op($0, $1, $2) }
      | tUNARY_NUM simple_numeric tPOW Arg {
            mk_unary_op($0,
                        mk_binary_op(
                          $1, $2, $3)) }
      | tUPLUS Arg {
            mk_unary_op($0, $1) }
      | tUMINUS Arg {
            mk_unary_op($0, $1) }
      | Arg tPIPE Arg {
            mk_binary_op($0, $1, $2) }
      | Arg tCARET Arg {
            mk_binary_op($0, $1, $2) }
      | Arg tAMPER2 Arg {
            mk_binary_op($0, $1, $2) }
      | Arg tCMP Arg {
            mk_binary_op($0, $1, $2) }
      | rel_expr =tCMP
      | Arg tEQ Arg {
            mk_binary_op($0, $1, $2) }
      | Arg tEQQ Arg {
            mk_binary_op($0, $1, $2) }
      | Arg tNEQ Arg {
            mk_binary_op($0, $1, $2) }
      | Arg tMATCH Arg {
            mk_match_op($0, $1, $2) }
      | Arg tNMATCH Arg {
            mk_binary_op($0, $1, $2) }
      | tBANG Arg {
            mk_not_op($0, Nil, $1, Nil) }
      | tTILDE Arg {
            mk_unary_op($0, $1) }
      | Arg tLSHFT Arg {
            mk_binary_op($0, $1, $2) }
      | Arg tRSHFT Arg {
            mk_binary_op($0, $1, $2) }
      | Arg tANDOP Arg {
            mkLogicalOp and, $0, $1, $2) }
      | Arg tOROP Arg {
            mkLogicalOp or, $0, $1, $2) }
      | kDEFINED OptNl Arg {
            mk_keyword_cmd(:defined?, $0, Nil, [ $2 ], Nil) }
      | Arg tEH Arg OptNl tCOLON Arg {
            mk_ternary($0, $1,
                                      $2, $4, val[5]) }
      | Primary

  relop: tGT | tLT | tGEQ | tLEQ

rel_expr: Arg relop Arg =tGT {
            mk_binary_op($0, $1, $2) }
      | rel_expr relop Arg =tGT {
            mk_binary_op($0, $1, $2) }

arg_value: Arg

aref_args: None
      | args Trailer
      | args tCOMMA assocs Trailer {
            $0 << mk_associate(Nil, $2, Nil) }
      | assocs Trailer {
            [ mk_associate(Nil, $0, Nil) ] }

arg_rhs: Arg =tOP_ASGN
      | Arg kRESCUE_MOD Arg {
            rescue_body = mk_rescue_body $1,
                              Nil, Nil, Nil,
                              Nil, $2)

            mk_begin_body($0, [ rescue_body ]) }

paren_args: tLPAREN2 opt_call_args rparen {
            val }

opt_paren_args: # nothing {
            [ Nil, [], Nil ] }
      | paren_args

opt_call_args: # nothing { [] }
      | call_args
      | args tCOMMA
      | args tCOMMA assocs tCOMMA {
            $0 << mk_associate(Nil, $2, Nil) }
      | assocs tCOMMA {
            [ mk_associate(Nil, $0, Nil) ] }

call_args: Command { [ $0 ] }
      | args opt_block_arg {
            $0.concat($1) }
      | assocs opt_block_arg {
            [ mk_associate(Nil, $0, Nil) ]
            result.concat($1) }
      | args tCOMMA assocs opt_block_arg {
            assocs = mk_associate(Nil, $2, Nil)
            $0 << assocs
            result.concat($3) }
      | block_arg {
 [ $0 ] }

CommandArgs:   {
            # When branch gets invoked by RACC's lookahead
            # and Command args start with '[' or '('
            # we need to put `true` to the cmdarg stack
            # **before** `false` pushed by lexer
            #   m [], n
            #     ^
            # Right here we have cmdarg [...0] because
            # lexer pushed it on '['
            # We need to modify cmdarg stack to [...10]
            #
            # For all other cases (like `m n` or `m n, []`) we simply put 1 to the stack
            # and later lexer pushes corresponding bits on top of it.
            last_token = @last_token[0]
            lookahead = last_token == :tLBRACK || last_token == :tLPAREN_ARG

            if lookahead
              top = @lexer.cmdarg.pop
              @lexer.cmdarg.push(true)
              @lexer.cmdarg.push(top)
            else
              @lexer.cmdarg.push(true)
            end }
        call_args {
            # call_args can be followed by tLBRACE_ARG (that does cmdarg.push(0) in the lexer)
            # but the push must be done after cmdarg.pop() in the parser.
            # So this code does cmdarg.pop() to pop 0 pushed by tLBRACE_ARG,
            # cmdarg.pop() to pop 1 pushed by CommandArgs,
            # and cmdarg.push(0) to restore back the flag set by tLBRACE_ARG.
            last_token = @last_token[0]
            lookahead = last_token == :tLBRACE_ARG
            if lookahead
              top = @lexer.cmdarg.pop
              @lexer.cmdarg.pop
              @lexer.cmdarg.push(top)
            else
              @lexer.cmdarg.pop
            end
 $1 }

block_arg: tAMPER arg_value {
            mk_block_pass($0, $1) }

opt_block_arg: tCOMMA block_arg { [ $1 ] }
      | # nothing { [] }

  args: arg_value { [ $0 ] }
      | tSTAR arg_value {
            [ mk_splat($0, $1) ] }
      | args tCOMMA arg_value {
            $0 << $2 }
      | args tCOMMA tSTAR arg_value {
            $0 << mk_splat($2, $3) }

mrhs_arg: mrhs {
            mk_array(Nil, $0, Nil) }
      | arg_value

  mrhs: args tCOMMA arg_value {
            $0 << $2 }
      | args tCOMMA tSTAR arg_value {
            $0 << mk_splat($2, $3) }
      | tSTAR arg_value {
            [ mk_splat($0, $1) ] }

Primary: literal
      | strings
      | xstring
      | regexp
      | words
      | qwords
      | symbols
      | qsymbols
      | VarRef
      | backref
      | tFID {
            mk_call_method(Nil, Nil, $0) }
      | kBEGIN {
            @lexer.cmdarg.push(false) }
          bodystmt kEND {
            @lexer.cmdarg.pop

            mk_begin_keyword($0, $2, $3) }
      | tLPAREN_ARG Stmt {
            @lexer.state = :expr_endarg }
          rparen {
            mk_begin($0, $1, $3) }
      | tLPAREN_ARG {
            @lexer.state = :expr_endarg }
          OptNl tRPAREN {
            mk_begin($0, Nil, $3) }
      | tLPAREN Compstmt tRPAREN {
            mk_begin($0, $1, $2) }
      | Primary tCOLON2 tCONSTANT {
            mk_const_fetch($0, $1, $2) }
      | tCOLON3 tCONSTANT {
            mk_const_global($0, $1) }
      | tLBRACK aref_args tRBRACK {
            mk_array($0, $1, $2) }
      | tLBRACE assoc_list tRCURLY { mk_associate $0 $1 $2 }
      | KReturn { mk_keyword_cmd Return $0 }
      | kYIELD tLPAREN2 call_args rparen { mk_keyword_cmd Yield $0, $1, $2, $3) }
      | kYIELD tLPAREN2 rparen { mk_keyword_cmd Yield $0, $1, [], $2) }
      | kYIELD { mk_keyword_cmd Yield $0 }
      | kDEFINED OptNl tLPAREN2 Expr rparen { mk_keyword_cmd(:defined?, $0, $2, [ $3 ], $4) }
      | kNOT tLPAREN2 Expr rparen { mk_not_op $0 $1 $2 $3 }
      | kNOT tLPAREN2 rparen { mk_not_op $0 $1 Nil $2 }
      | Operation BraceBlock {
            MethodCall = mk_call_method(Nil, Nil, $0)

            begin_t, args, body, end_t = $1
            result      = mk_block MethodCall begin_t args body end_t }
      | MethodCall
      | MethodCall BraceBlock {
            begin_t, args, body, end_t = $1
            result      = mk_block($0,
                            begin_t, args, body, end_t) }
      | tLAMBDA lambda {
            lambda_call = mk_call_lambda($0)

            args, (begin_t, body, end_t) = $1
            result      = mk_block(lambda_call,
                            begin_t, args, body, end_t) }
      | kIF Expr then Compstmt IfTail kEND {
            else_t, else_ = $4
            mk_condition($0, $1, $2,
                                        $3, else_t,
                                        else_,  val[5]) }
      | kUNLESS Expr then Compstmt OptElse kEND {
            else_t, else_ = $4
            mk_condition($0, $1, $2,
                                        else_,  else_t,
                                        $3, val[5]) }
    --  | kWHILE ExprValueDo Compstmt kEND  { mk_loop(:while, $0, *$1, $2, $3)
    }
--  | kUNTIL ExprValueDo Compstmt kEND  { mk_loop(:until, $0, *$1, $2, $3)
    }
| kCASE Expr OptTerms case_body kEND
    {
      *when_bodies, (else_t, else_body) = *$3

      mk_case($0, $1,
                                    when_bodies, else_t, else_body,
                                    $4) }
      | kCASE            OptTerms case_body kEND {
            *when_bodies, (else_t, else_body) = *$2

            mk_case($0, Nil,
                                    when_bodies, else_t, else_body,
                                    $3) }
      | kFOR for_--var kIN ExprValueDo Compstmt kEND  { mk_for($0, $1, $2, *$3, $4, val[5])
    }
| kCLASS cpath superclass
    {
      @static_env.extend_static
      @lexer.cmdarg.push(false)
      @lexer.cond.push(false)
            @context.push(:class) }
          bodystmt kEND {
            unless @context.class_definition_allowed?
              error ":class_in_def, Nil, $0"
            end

            lt_t, superclass = $2
            mk_def_class($0, $1,
                                        lt_t, superclass,
                                        $4, val[5])

            @lexer.cmdarg.pop
            @lexer.cond.pop
            @static_env.unextend
            @context.pop }
      | kCLASS tLSHFT Expr Term {
            @static_env.extend_static
            @lexer.cmdarg.push(false)
            @lexer.cond.push(false)
            @context.push(:sclass) }
          bodystmt kEND {
            mk_def_sclass($0, $1, $2,
                                          val[5], val[6])

            @lexer.cmdarg.pop
            @lexer.cond.pop
            @static_env.unextend
            @context.pop }
      | kMODULE cpath {
            @static_env.extend_static
            @lexer.cmdarg.push(false) }
          bodystmt kEND {
            unless @context.module_definition_allowed?
              error ":module_in_def, Nil, $0"
            end

            mk_def_module($0, $1,
                                          $3, $4)

            @lexer.cmdarg.pop
            @static_env.unextend }
      | kDEF fname {
            @static_env.extend_static
            @lexer.cmdarg.push(false)
            @lexer.cond.push(false)
            @context.push(:def) }
          f_arglist bodystmt kEND {
            mk_def_method($0, $1,
                        $3, $4, val[5])

            @lexer.cmdarg.pop
            @lexer.cond.pop
            @static_env.unextend
            @context.pop }
      | kDEF singleton DotOrColon {
            @lexer.state = :expr_fname }
          fname {
            @static_env.extend_static
            @lexer.cmdarg.push(false)
            @lexer.cond.push(false)
            @context.push(:defs) }
          f_arglist bodystmt kEND {
            mk_def_singleton($0, $1, $2,
                        $4, val[6], val[7], val[8])

            @lexer.cmdarg.pop
            @lexer.cond.pop
            @static_env.unextend
            @context.pop }
      | kBREAK { mk_keyword_cmd(:break, $0) }
      | kNEXT {
            mk_keyword_cmd(:next, $0) }
      | kREDO {
            mk_keyword_cmd(:redo, $0) }
      | kRETRY {
            mk_keyword_cmd(:retry, $0) }

KReturn: kRETURN { error ":invalid_return, Nil, $0" if @context.in_class?  }

then: Term { $0 }
  | kTHEN { $0 }
  | Term kTHEN { $0 } { $1 }

do: Term
  | kDO_COND

IfTail: OptElse
      | kELSIF Expr then Compstmt IfTail {
            else_t, else_ = $4
            [ $0,
                        mk_condition($0, $1, $2,
                                          $3, else_t,
                                          else_,  Nil),
                      ] }

OptElse: None
  | kELSE Compstmt { val }

FMarg: FNormArg { mk_Arg $0 }
  | tLPAREN FMargs rparen { mk_multi_lhs $0 $1 $2 }

FMargList: FMarg { [ $0 ] }
      | FMargList tCOMMA FMarg {
            $0 << $2 }

FMargs: FMargList
      | FMargList tCOMMA tSTAR FNormArg {
            $0.
                        push(mk_restarg $2 $3) }
      | FMargList tCOMMA tSTAR FNormArg tCOMMA FMargList {
            $0.
                        push(mk_restarg $2 $3).
                        concat(val[5]) }
      | FMargList tCOMMA tSTAR {
            $0.
                        push(mk_restarg($2)) }
      | FMargList tCOMMA tSTAR            tCOMMA FMargList {
            $0.
                        push(mk_restarg($2)).
                        concat($4) }
      |                    tSTAR FNormArg {
            [ mk_restarg($0, $1) ] }
      |                    tSTAR FNormArg tCOMMA FMargList {
            [ mk_restarg($0, $1),
                        *$3 ] }
      |                    tSTAR {
            [ mk_restarg($0) ] }
      |                    tSTAR tCOMMA FMargList {
            [ mk_restarg($0),
                        *$2 ] }

block_args_tail: f_block_kwarg tCOMMA f_kwrest opt_f_block_arg {
            $0.concat($2).concat($3) }
      | f_block_kwarg opt_f_block_arg {
            $0.concat($1) }
      | f_kwrest opt_f_block_arg {
            $0.concat($1) }
      | FBlockArg { [ $0 ] }

opt_block_args_tail:
        tCOMMA block_args_tail { $1 }
      | # nothing { [] }

block_param: f_arg tCOMMA f_block_optarg tCOMMA f_rest_arg              opt_block_args_tail {
            $0.
                        concat($2).
                        concat($4).
                        concat(val[5]) }
      | f_arg tCOMMA f_block_optarg tCOMMA f_rest_arg tCOMMA f_arg opt_block_args_tail {
            $0.
                        concat($2).
                        concat($4).
                        concat(val[6]).
                        concat(val[7]) }
      | f_arg tCOMMA f_block_optarg                                opt_block_args_tail {
            $0.
                        concat($2).
                        concat($3) }
      | f_arg tCOMMA f_block_optarg tCOMMA                   f_arg opt_block_args_tail {
            $0.
                        concat($2).
                        concat($4).
                        concat(val[5]) }
      | f_arg tCOMMA                       f_rest_arg              opt_block_args_tail {
            $0.
                        concat($2).
                        concat($3) }
      | f_arg tCOMMA
      | f_arg tCOMMA                       f_rest_arg tCOMMA f_arg opt_block_args_tail {
            $0.
                        concat($2).
                        concat($4).
                        concat(val[5]) }
      | f_arg                                                      opt_block_args_tail {
            if $1.empty? && $0.size == 1
              [mk_procarg0($0[0])]
            else
              $0.concat($1)
            end }
      | f_block_optarg tCOMMA              f_rest_arg              opt_block_args_tail {
            $0.
                        concat($2).
                        concat($3) }
      | f_block_optarg tCOMMA              f_rest_arg tCOMMA f_arg opt_block_args_tail {
            $0.
                        concat($2).
                        concat($4).
                        concat(val[5]) }
      | f_block_optarg                                             opt_block_args_tail {
            $0.
                        concat($1) }
      | f_block_optarg tCOMMA                                f_arg opt_block_args_tail {
            $0.
                        concat($2).
                        concat($3) }
      |                                    f_rest_arg              opt_block_args_tail {
            $0.
                        concat($1) }
      |                                    f_rest_arg tCOMMA f_arg opt_block_args_tail {
            $0.
                        concat($2).
                        concat($3) }
      |                                                                block_args_tail

opt_block_param: # nothing {
            mk_args(Nil, [], Nil) }
      | block_param_def {
            @lexer.state = :Expr }

block_param_def: tPIPE opt_bv_decl tPIPE {
            mk_args($0, $1, $2) }
      | tOROP {
            mk_args($0, [], $0) }
      | tPIPE block_param opt_bv_decl tPIPE {
            mk_args($0, $1.concat($2), $3) }

opt_bv_decl: OptNl { [] }
      | OptNl tSEMI bv_decls OptNl {
            $2 }

bv_decls: bvar { [ $0 ] }
      | bv_decls tCOMMA bvar {
            $0 << $2 }

  bvar: tIDENTIFIER {
            @static_env.declare $0[0]
            mk_shadowarg($0) }
      | f_bad_arg

lambda:   {
            @static_env.extend_dynamic }
        f_larglist {
            @lexer.cmdarg.push(false) }
        lambda_body {
            @lexer.cmdarg.pop

            [ $1, $3 ]

            @static_env.unextend }

f_larglist: tLPAREN2 f_args opt_bv_decl tRPAREN {
            mk_args($0, $1.concat($2), $3) }
      | f_args {
            mk_args(Nil, $0, Nil) }

lambda_body: tLAMBEG {
            @context.push(:lambda) }
        Compstmt tRCURLY {
            [ $0, $2, $3 ]
            @context.pop }
      | kDO_LAMBDA {
            @context.push(:lambda) }
        bodystmt kEND {
            [ $0, $2, $3 ]
            @context.pop }

do_block: kDO_BLOCK {
            @context.push(:block) }
        do_body kEND {
            [ $0, *$2, $3 ]
            @context.pop }

BlockCall: Command do_block {
            begin_t, block_args, body, end_t = $1
            result      = mk_block($0,
                            begin_t, block_args, body, end_t) }
      | BlockCall DotOrColon Operation2 opt_paren_args {
            lparen_t, args, rparen_t = $3
            mk_call_method($0, $1, $2,
                        lparen_t, args, rparen_t) }
      | BlockCall DotOrColon Operation2 opt_paren_args BraceBlock {
            lparen_t, args, rparen_t = $3
            MethodCall = mk_call_method($0, $1, $2,
                            lparen_t, args, rparen_t)

            begin_t, args, body, end_t = $4
            result      = mk_block(MethodCall,
                            begin_t, args, body, end_t) }
      | BlockCall DotOrColon Operation2 CommandArgs do_block {
            MethodCall = mk_call_method($0, $1, $2,
                            Nil, $3, Nil)

            begin_t, args, body, end_t = $4
            result      = mk_block(MethodCall,
                            begin_t, args, body, end_t) }

MethodCall: Operation paren_args {
            lparen_t, args, rparen_t = $1
            mk_call_method(Nil, Nil, $0,
                        lparen_t, args, rparen_t) }
      | Primary CallOp Operation2 opt_paren_args {
            lparen_t, args, rparen_t = $3
            mk_call_method($0, $1, $2,
                        lparen_t, args, rparen_t) }
      | Primary tCOLON2 Operation2 paren_args {
            lparen_t, args, rparen_t = $3
            mk_call_method($0, $1, $2,
                        lparen_t, args, rparen_t) }
      | Primary tCOLON2 operation3 {
            mk_call_method($0, $1, $2) }
      | Primary CallOp paren_args {
            lparen_t, args, rparen_t = $2
            mk_call_method($0, $1, Nil,
                        lparen_t, args, rparen_t) }
      | Primary tCOLON2 paren_args {
            lparen_t, args, rparen_t = $2
            mk_call_method($0, $1, Nil,
                        lparen_t, args, rparen_t) }
      | kSUPER paren_args {
            lparen_t, args, rparen_t = $1
            mk_keyword_cmd(:super, $0,
                        lparen_t, args, rparen_t) }
      | kSUPER {
            mk_keyword_cmd(:zsuper, $0) }
      | Primary tLBRACK2 opt_call_args RBracket {
            mk_index($0, $1, $2, $3) }

BraceBlock: tLCURLY {
            @context.push(:block) }
        BraceBody tRCURLY {
            [ $0, *$2, $3 ]
            @context.pop }
      | kDO {
            @context.push(:block) }
        do_body kEND {
            [ $0, *$2, $3 ]
            @context.pop }

BraceBody:   {
            @static_env.extend_dynamic }
          opt_block_param Compstmt {
            [ $1, $2 ]

            @static_env.unextend }

do_body:   {
            @static_env.extend_dynamic } {
            @lexer.cmdarg.push(false) }
          opt_block_param bodystmt {
            [ $2, $3 ]

            @static_env.unextend
            @lexer.cmdarg.pop }

case_body: kWHEN args then Compstmt cases {
            [ mk_when($0, $1, $2, $3),
                        *$4 ] }

  cases: OptElse { [ $0 ] }
      | case_body

opt_rescue: kRESCUE exc_list exc_var then Compstmt opt_rescue {
            assoc_t, exc_var = $2

            if $1
              exc_list = mk_array(Nil, $1, Nil)
            end

            [ mk_rescue_body $0,
                            exc_list, assoc_t, exc_var,
                            $3, $4),
                        *val[5] ] }
      | { [] }

exc_list: arg_value { [ $0 ] }
      | mrhs
      | None

exc_var: tASSOC Lhs {
            [ $0, $1 ] }
      | None

opt_ensure: kENSURE Compstmt {
            [ $0, $1 ] }
      | None

literal: numeric
      | symbol
      | dsym

strings: string {
            mk_string_compose(Nil, $0, Nil) }

string: string1 { [ $0 ] }
      | string string1 {
            $0 << $1 }

string1: tSTRING_BEG string_contents tSTRING_END {
            string = mk_string_compose($0, $1, $2)
            mk_dedent_string(string, @lexer.dedent_level) }
      | tSTRING {
            string = mk_string($0)
            mk_dedent_string(string, @lexer.dedent_level) }
      | tCHARACTER {
            mk_character($0) }

xstring: tXSTRING_BEG xstring_contents tSTRING_END {
            string = mk_xstring_compose($0, $1, $2)
            mk_dedent_string(string, @lexer.dedent_level) }

regexp: tREGEXP_BEG regexp_contents tSTRING_END tREGEXP_OPT {
            opts   = mk_regexp_options($3)
            mk_regexp_compose($0, $1, $2, opts) }

  words: tWORDS_BEG WordList tSTRING_END {
            mk_words_compose($0, $1, $2) }

WordList: # nothing { [] }
      | WordList word tSPACE {
            $0 << mk_word($1) }

word: StringContent { [ $0 ] }
      | word StringContent {
            $0 << $1 }

symbols: tSYMBOLS_BEG symbol_list tSTRING_END {
            mk_symbols_compose($0, $1, $2) }

symbol_list: # nothing { [] }
      | symbol_list word tSPACE {
            $0 << mk_word($1) }

qwords: tQWORDS_BEG qword_list tSTRING_END {
            mk_words_compose($0, $1, $2) }

qsymbols: tQSYMBOLS_BEG qsym_list tSTRING_END {
            mk_symbols_compose($0, $1, $2) }

qword_list: # nothing { [] }
      | qword_list tSTRING_CONTENT tSPACE {
            $0 << mk_string_internal($1) }

qsym_list: # nothing { [] }
      | qsym_list tSTRING_CONTENT tSPACE {
            $0 << mk_symbol_internal($1) }

string_contents: # nothing { [] }
      | string_contents StringContent {
            $0 << $1 }

xstring_contents: # nothing { [] }
      | xstring_contents StringContent {
            $0 << $1 }

regexp_contents: # nothing { [] }
      | regexp_contents StringContent {
            $0 << $1 }

StringContent: tSTRING_CONTENT {
            mk_string_internal($0) }
      | tSTRING_DVAR string_dvar { $1 }
      | tSTRING_DBEG {
            @lexer.cmdarg.push(false)
            @lexer.cond.push(false) }
          Compstmt tSTRING_DEND {
            @lexer.cmdarg.pop
            @lexer.cond.pop

            mk_begin($0, $2, $3) }

string_dvar: tGVAR {
            mk_gvar($0) }
      | tIVAR {
            mk_ivar($0) }
      | tCVAR {
            mk_cvar($0) }
      | backref


symbol: tSYMBOL {
            @lexer.state = :expr_end
            mk_symbol($0) }

  dsym: tSYMBEG xstring_contents tSTRING_END {
            @lexer.state = :expr_end
            mk_symbol_compose($0, $1, $2) }

numeric: simple_numeric {
            $0 }
      | tUNARY_NUM simple_numeric =tLOWEST {
            if mk_respond_to? :negate
              # AST builder interface compatibility
              mk_negate($0, $1)
            else
              mk_unary_num($0, $1)
            end }

simple_numeric: tINTEGER {
            @lexer.state = :expr_end
            mk_integer($0) }
      | tFLOAT {
            @lexer.state = :expr_end
            mk_float($0) }
      | tRATIONAL {
            @lexer.state = :expr_end
            mk_rational($0) }
      | tIMAGINARY {
            @lexer.state = :expr_end
            mk_complex($0) }

user_variable: tIDENTIFIER {
            mk_ident($0) }
      | tIVAR {
            mk_ivar($0) }
      | tGVAR {
            mk_gvar($0) }
      | tCONSTANT {
            mk_const($0) }
      | tCVAR {
            mk_cvar($0) }

keyword_variable: kNIL {
            mk_Nil($0) }
      | kSELF {
            mk_self($0) }
      | kTRUE {
            mk_true($0) }
      | kFALSE {
            mk_false($0) }
      | k__FILE__ {
            mk___FILE__($0) }
      | k__LINE__ {
            mk___LINE__($0) }
      | k__ENCODING__ {
            mk___ENCODING__($0) }

VarRef: user_variable {
            mk_accessible($0) }
      | keyword_variable {
            mk_accessible($0) }

var_lhs: user_variable {
            mk_assignable($0) }
      | keyword_variable {
            mk_assignable($0) }

backref: tNTH_REF {
            mk_nth_ref($0) }
      | tBACK_REF {
            mk_back_ref($0) }

superclass: tLT {
            @lexer.state = :Expr }
          Expr Term {
            [ $0, $2 ] }
      | # nothing {
            Nil }

f_arglist: tLPAREN2 f_args rparen {
            mk_args($0, $1, $2)

            @lexer.state = :Expr }
      |   {
            @lexer.in_kwarg
            @lexer.in_kwarg = true }
        f_args Term {
            @lexer.in_kwarg = $0
            mk_args(Nil, $1, Nil) }

args_tail: f_kwarg tCOMMA f_kwrest opt_f_block_arg {
            $0.concat($2).concat($3) }
      | f_kwarg opt_f_block_arg {
            $0.concat($1) }
      | f_kwrest opt_f_block_arg {
            $0.concat($1) }
      | FBlockArg { [ $0 ] }

opt_args_tail: tCOMMA args_tail { $1 }
      | # nothing { [] }

f_args: f_arg tCOMMA f_optarg tCOMMA f_rest_arg              opt_args_tail {
            $0.
                        concat($2).
                        concat($4).
                        concat(val[5]) }
      | f_arg tCOMMA f_optarg tCOMMA f_rest_arg tCOMMA f_arg opt_args_tail {
            $0.
                        concat($2).
                        concat($4).
                        concat(val[6]).
                        concat(val[7]) }
      | f_arg tCOMMA f_optarg                                opt_args_tail {
            $0.
                        concat($2).
                        concat($3) }
      | f_arg tCOMMA f_optarg tCOMMA                   f_arg opt_args_tail {
            $0.
                        concat($2).
                        concat($4).
                        concat(val[5]) }
      | f_arg tCOMMA                 f_rest_arg              opt_args_tail {
            $0.
                        concat($2).
                        concat($3) }
      | f_arg tCOMMA                 f_rest_arg tCOMMA f_arg opt_args_tail {
            $0.
                        concat($2).
                        concat($4).
                        concat(val[5]) }
      | f_arg                                                opt_args_tail {
            $0.
                        concat($1) }
      |              f_optarg tCOMMA f_rest_arg              opt_args_tail {
            $0.
                        concat($2).
                        concat($3) }
      |              f_optarg tCOMMA f_rest_arg tCOMMA f_arg opt_args_tail {
            $0.
                        concat($2).
                        concat($4).
                        concat(val[5]) }
      |              f_optarg                                opt_args_tail {
            $0.
                        concat($1) }
      |              f_optarg tCOMMA                   f_arg opt_args_tail {
            $0.
                        concat($2).
                        concat($3) }
      |                              f_rest_arg              opt_args_tail {
            $0.
                        concat($1) }
      |                              f_rest_arg tCOMMA f_arg opt_args_tail {
            $0.
                        concat($2).
                        concat($3) }
      |                                                          args_tail {
            $0 }
      | # nothing { [] }

f_bad_arg: tCONSTANT {
            error ":argument_const, Nil, $0" }
      | tIVAR {
            error ":argument_ivar, Nil, $0" }
      | tGVAR {
            error ":argument_gvar, Nil, $0" }
      | tCVAR {
            error ":argument_cvar, Nil, $0" }

FNormArg: f_bad_arg
      | tIDENTIFIER {
            @static_env.declare $0[0]

            $0 }

f_arg_asgn: FNormArg {
            $0 }

f_arg_item: f_arg_asgn {
            mk_Arg($0) }
      | tLPAREN FMargs rparen {
            mk_multi_lhs($0, $1, $2) }

  f_arg: f_arg_item { [ $0 ] }
      | f_arg tCOMMA f_arg_item {
            $0 << $2 }

f_label: tLABEL {
            check_kwarg_name($0)

            @static_env.declare $0[0]

            $0 }

  f_kw: f_label arg_value {
            mk_kwoptarg($0, $1) }
      | f_label {
            mk_kwarg($0) }

f_block_kw: f_label Primary {
            mk_kwoptarg($0, $1) }
      | f_label {
            mk_kwarg($0) }

f_block_kwarg: f_block_kw { [ $0 ] }
      | f_block_kwarg tCOMMA f_block_kw {
            $0 << $2 }

f_kwarg: f_kw { [ $0 ] }
      | f_kwarg tCOMMA f_kw {
            $0 << $2 }

kwrest_mark: tPOW | tDSTAR

f_kwrest: kwrest_mark tIDENTIFIER {
            @static_env.declare $1[0]

            [ mk_kwrestarg($0, $1) ] }
      | kwrest_mark {
            [ mk_kwrestarg($0) ] }

  f_opt: f_arg_asgn tEQL arg_value {
            mk_optarg($0, $1, $2) }

f_block_opt: f_arg_asgn tEQL Primary {
            mk_optarg($0, $1, $2) }

f_block_optarg: f_block_opt { [ $0 ] }
      | f_block_optarg tCOMMA f_block_opt {
            $0 << $2 }

f_optarg: f_opt { [ $0 ] }
      | f_optarg tCOMMA f_opt {
            $0 << $2 }

restarg_mark: tSTAR2 | tSTAR

f_rest_arg: restarg_mark tIDENTIFIER {
            @static_env.declare $1[0]

            [ mk_restarg($0, $1) ] }
      | restarg_mark {
            [ mk_restarg($0) ] }

blkarg_mark: tAMPER2 | tAMPER

FBlockArg: blkarg_mark tIDENTIFIER {
            @static_env.declare $1[0]

            mk_blockarg($0, $1) }

opt_f_block_arg: tCOMMA FBlockArg { [ $1 ] }
      | { [] }

singleton: VarRef
      | tLPAREN2 Expr rparen { $1 }

assoc_list: # nothing { [] }
      | assocs Trailer

assocs: assoc { [ $0 ] }
      | assocs tCOMMA assoc {
            $0 << $2 }

  assoc: arg_value tASSOC arg_value {
            mk_pair($0, $1, $2) }
      | tLABEL arg_value {
            mk_pair_keyword($0, $1) }
      | tSTRING_BEG string_contents tLABEL_END arg_value {
            mk_pair_quoted($0, $1, $2, $3) }
      | tDSTAR arg_value {
            mk_kwsplat($0, $1) }

operation: tIDENTIFIER | tCONSTANT | tFID
Operation2: tIDENTIFIER | tCONSTANT | tFID | op
operation3: tIDENTIFIER | tFID | op
DotOrColon: CallOp | tCOLON2
CallOp: tDOT {
            [:dot, $0[1]] }
      | tANDDOT {
            [:anddot, $0[1]] }
-}

OptTerms: -- |
  -- | Terms OptNl
  tNL { $0 }

{-
RBracket: OptNl tRBRACK { $1 }
-}

Trailer: -- |
  tNL { $0 }
  | tCOMMA { $0 }

Term: -- tSEMI { yyerrok }
  tNL { Nil }

Terms: Term
-- |  $0 { Nil }

{
parseError _ = throwError "!Parse Error"
}
