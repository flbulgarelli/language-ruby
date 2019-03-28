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
    kUNTIL_MOD {L.KUNTIL_MOD}
    kRESCUE_MOD {L.KRESCUE_MOD}
    kALIAS {L.KALIAS}
    kDEFINED {L.KDEFINED}
    klBEGIN {L.KlBEGIN}
    klEND {L.KlEND}
    k__LINE__ {L.K__LINE__}
    k__FILE__ {L.K__FILE__}
    k__ENCODING__ {L.K__ENCODING__}
    tIDENTIFIER {L.TIDENTIFIER {}}
    tFID {L.TFID}
    tGVAR {L.TGVAR {}}
    tIVAR {L.TIVAR {}}
    tCONSTANT {L.TCONSTANT {}}
    tLABEL {L.TLABEL}
    tCVAR {L.TCVAR {}}
    tNTH_REF {L.TNTH_REF}
    tBACK_REF {L.TBACK_REF}
    tSTRING_CONTENT {L.TSTRING_CONTENT}
    tINTEGER {L.TINTEGER {}}
    tFLOAT {L.TFLOAT {}}
    tUPLUS {L.TUPLUS}
    tUMINUS {L.TUMINUS}
    tUNARY_NUM {L.TUNARY_NUM}
    tPOW {L.TPOW}
    tCMP {L.TCMP}
    tEQ {L.TEQ}
    tEQL {L.TEQL}
    tEQQ {L.TEQQ}
    tNEQ {L.TNEQ}
    tGEQ {L.TGEQ}
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
    tLBRACE_ARG {L.TLBRACE_ARG}
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
    tLOWEST {L.TLOWEST}
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
    tSTRING {L.TSTRING {} }
    tSYMBOL {L.TSYMBOL {}}
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
    tIMAGINARY {L.TIMAGINARY {}}
    tLABEL_END {L.TLABEL_END}
    tANDDOT {L.TANDDOT}
    tMETHREF {L.TMETHREF}


%right    tBANG tTILDE tUPLUS
%right    tPOW
%right    tUNARY_NUM tUMINUS
%left     tSTAR2 tDIVIDE tPERCENT
%left     tPLUS tMINUS
%left     tLSHFT tRSHFT
%left     tAMPER2
%left     tPIPE tCARET
%left     tGT tGEQ tLT tLEQ
%nonassoc tCMP tEQ tEQQ tNEQ tMATCH tNMATCH
%left     tANDOP
%left     tOROP
%nonassoc tDOT2 tDOT3
%right    tEH tCOLON
%left     kRESCUE_MOD
%right    tEQL tOP_ASGN
%nonassoc kDEFINED
%right    kNOT
%left     kOR kAND
%nonassoc kIF_MOD kUNLESS_MOD kWHILE_MOD kUNTIL_MOD
%nonassoc tLBRACE_ARG
%nonassoc tLOWEST

%%

Program :: { Term }
Program: TopCompstmt { $1 }

TopCompstmt :: { Term }
TopCompstmt: TopStmts OptTerms { mkExpression $1 }

TopStmts :: { [Term] }
TopStmts: {- nothing -} { [] }
  | TopStmt { [ $1 ] }
  | TopStmts Terms TopStmt { $1 ++ [$3] }
  | error TopStmt { [$2] }

TopStmt :: { Term }
TopStmt: Stmt { $1 } | klBEGIN BeginBlock { mk_preexe $2 }

BeginBlock: tLCURLY TopCompstmt tRCURLY { $1 }

Bodystmt: Compstmt OptRescue OptElse OptEnsure { error "Bodystmt"  }
-- {-  {
--             rescue_bodies     = $2;
--             else_t,   else_   = $3;
--             ensure_t, ensure_ = $4;

--             if rescue_bodies.empty? && !else_.Nil?
--               error ":useless_else, Nil, else_t"
--             end

--             mk_begin_body($1,
--                         rescue_bodies,
--                         else_t,   else_,
--                         ensure_t, ensure_) }
-- -}

Compstmt: Stmts OptTerms { mkExpression $1 }
Stmts :: { [Term] }
Stmts: {- nothing -} { [] }
  | StmtOrBegin { [ $1 ] }
  | Stmts Terms StmtOrBegin { $1 ++ [$3] }
  | error Stmt {  [ $2 ] }

StmtOrBegin: Stmt { $1 }
  | klBEGIN BeginBlock { error ("begin_in_method " ++ show $1) }

Stmt :: { Term }
Stmt: kALIAS Fitem Fitem { mk_alias $2 $3 }
  | kALIAS tGVAR tGVAR { (mk_alias $1 mk_gvar($2) mk_gvar($3)) }
  | kALIAS tGVAR tBACK_REF { (mk_alias $1 (mk_gvar $2) (mk_back_ref $3)) }
  | kALIAS tGVAR tNTH_REF { error ":nth_ref_alias, Nil, $3" }
  | kUNDEF UndefList { (mk_undef_method $1 $2) }
  | Stmt kIF_MOD Expr { mk_condition_mod $1 Nil $2 $3 }
  | Stmt kUNLESS_MOD Expr { mk_condition_mod Nil $1 $2 $3 }
  | Stmt kWHILE_MOD Expr { mk_loop_mod While $1 $2 $3 }
  | Stmt kUNTIL_MOD Expr { mk_loop_mod Until $1 $2 $3 }
  | Stmt kRESCUE_MOD Stmt {  mk_begin_body $1 [mk_rescue_body $2 Nil Nil Nil Nil $3] }
  | klEND tLCURLY Compstmt tRCURLY { mk_postexe $3 }
  | CommandAsgn { $1 }
  | Mlhs tEQL CommandCall { mk_multiassign $1 $2 $3 }
  | Lhs tEQL Mrhs { undefined } -- { mk_assign $1 $2 mk_array Nil $3 Nil }
  | Mlhs tEQL MrhsArg { mk_multiassign $1 $2 $3 }  --  | Expr {$1 }

CommandAsgn :: { Term }
CommandAsgn: Lhs tEQL CommandRhs { (mk_assign $1 $2 $3) }
  | VarLhs tOP_ASGN CommandRhs { (mk_op_assign $1 $2 $3) }
  | Primary tLBRACK2 OptCallArgs RBracket tOP_ASGN CommandRhs { (mk_op_assign (mk_index $1 $2 $3 $4) $5 $6) }
  | Primary CallOp tIDENTIFIER tOP_ASGN CommandRhs { mk_op_assign (mk_call_method $1 $2 $3) $4 $5 }
  | Primary CallOp tCONSTANT tOP_ASGN CommandRhs { mk_op_assign  (mk_call_method  $1 $2 $3) $4 $5 }
  | Primary tCOLON2 tCONSTANT tOP_ASGN CommandRhs { mk_op_assign (mk_const_op_assignable (mk_const_fetch $1 $2 $3)) $4 $5 }
  | Primary tCOLON2 tIDENTIFIER tOP_ASGN CommandRhs { mk_op_assign (mk_call_method $1 $2 $3) $4 $5 }
  | Backref tOP_ASGN CommandRhs { mk_op_assign $1 $2 $3 }

CommandRhs: CommandCall {-=-} tOP_ASGN { $1 }
  | CommandCall kRESCUE_MOD Stmt { mk_begin_body $1 [mk_rescue_body $2 Nil Nil Nil Nil $3] }
  | CommandAsgn { $1 }

Expr: CommandCall { $1 }
  | Expr kAND Expr { mkLogicalOp And $1 $2 $3 }
  | Expr kOR Expr { mkLogicalOp Or $1 $2 $3 }
  | kNOT OptNl Expr { mk_not_op $2 $3 }
  | tBANG CommandCall{ mk_not_op $1 Nil $2 Nil }
  | Arg { $1 }

ExprValueDo: --  { @lexer.cond.push(true) }
  Expr Do { undefined } -- { @lexer.cond.pop; [ $2, $3 ] }

CommandCall: Command { $1 }
  | BlockCommand { $1 }

BlockCommand: BlockCall { $1 }
  | BlockCall DotOrColon Operation2 CommandArgs { mk_call_method $1 $2 $3 Nil $4 Nil }

CmdBraceBlock: tLBRACE_ARG BraceBody tRCURLY { undefined }

Command :: { Term }
Command: Operation CommandArgs {-=-}tLOWEST { mk_call_method Nil Nil $1 Nil $2 Nil }
  | Operation CommandArgs CmdBraceBlock { undefined }  -- { let (begin_t, args, body, end_t) = $3 in (mk_block (mk_call_method Nil Nil $1 Nil $2 Nil) begin_t args body end_t) }
  | Primary CallOp Operation2 CommandArgs {-=-}tLOWEST { mk_call_method $1 $2 $3 $4 }
  | Primary CallOp Operation2 CommandArgs CmdBraceBlock { mk_block' $1 $2 $3 $4 $5 }
  | Primary tCOLON2 Operation2 CommandArgs {-=-}tLOWEST { mk_call_method $1 $3 $4 }
  | Primary tCOLON2 Operation2 CommandArgs CmdBraceBlock { mk_block' $1 $3 $4 $5 }
  | kSUPER CommandArgs { mk_keyword_cmd Super $1 Nil $2 Nil }
  | kYIELD CommandArgs { mk_keyword_cmd Yield $1 Nil $2 Nil }
  | KReturn CallArgs { mk_keyword_cmd Return $1 Nil $2 Nil }
  | kBREAK CallArgs { mk_keyword_cmd Break $1 Nil $2 Nil }
  | kNEXT CallArgs { mk_keyword_cmd Next $1 Nil $2 Nil }

Mlhs: MlhsBasic { mk_multi_lhs Nil $1 Nil }
  | tLPAREN MlhsInner Rparen { mk_begin $1 $2 $3 }

MlhsInner: MlhsBasic { mk_multi_lhs Nil $1 Nil }
  | tLPAREN MlhsInner Rparen { mk_multi_lhs $1 $2 $3 }

MlhsBasic: MlhsHead { $1 }
  | MlhsHead MlhsItem { error "$1. push($2)" }
  | MlhsHead tSTAR MlhsNode { error " $1. push((mk_splat $2 $3)) " }
  | MlhsHead tSTAR MlhsNode tCOMMA MlhsPost { error " $1. push((mk_splat $2 $3)). concat($5) " }
  | MlhsHead tSTAR { error " $1. push(mk_splat($2)) " }
  | MlhsHead tSTAR tCOMMA MlhsPost { error " $1. push(mk_splat($2)). concat($4) " }
  | tSTAR MlhsNode { error " [ mk_splat $1 $2 ] " }
  | tSTAR MlhsNode tCOMMA MlhsPost { error " [ (mk_splat $1 $2), *$4 ] " }
  | tSTAR { error " [ mk_splat $1 ] " }
  | tSTAR tCOMMA MlhsPost { error " [ mk_splat $1 *$3 ] " }

MlhsItem: MlhsNode { $1 }
      | tLPAREN MlhsInner Rparen { mk_begin $1 $2 $3 }

MlhsHead: MlhsItem tCOMMA { [ $1 ] }
  | MlhsHead MlhsItem tCOMMA { $1 ++ [$2] }

MlhsPost: MlhsItem { [ $1 ] }
  | MlhsPost tCOMMA MlhsItem { $1 ++ [$3] }

MlhsNode: UserVariable { mk_assignable $1  }
  | KeywordVariable { mk_assignable $1 }
  | Primary tLBRACK2 OptCallArgs RBracket { mk_index_asgn $1 $2 $3 $4 }
  | Primary CallOp tIDENTIFIER { mk_attr_asgn $1 $2 $3 }
  | Primary tCOLON2 tIDENTIFIER { mk_attr_asgn $1 $2 $3 }
  | Primary CallOp tCONSTANT { mk_attr_asgn $1 $2 $3 }
  | Primary tCOLON2 tCONSTANT { mk_assignable (mk_const_fetch $1 $2 $3) }
  | tCOLON3 tCONSTANT { mk_assignable (mk_const_global $1 $2) }
  | Backref { mk_assignable $1 }

Lhs: UserVariable { mk_assignable $1 }
  | KeywordVariable { mk_assignable $1 }
  | Primary tLBRACK2 OptCallArgs RBracket { mk_index_asgn $1 $2 $3 $4 }
  | Primary CallOp tIDENTIFIER { (mk_attr_asgn $1 $2 $3) }
  | Primary tCOLON2 tIDENTIFIER { (mk_attr_asgn $1 $2 $3) }
  | Primary CallOp tCONSTANT { (mk_attr_asgn $1 $2 $3) }
  | Primary tCOLON2 tCONSTANT { mk_assignable((mk_const_fetch $1 $2 $3)) }
  | tCOLON3 tCONSTANT { mk_assignable((mk_const_global $1 $2)) }
  | Backref { mk_assignable $1 }

Cname: tIDENTIFIER { error ":module_name_const, Nil, $1" }
  | tCONSTANT { $1 }

Cpath: tCOLON3 Cname { mk_const_global $1 $2 }
  | Cname { mk_const $1 }
  | Primary tCOLON2 Cname { mk_const_fetch $1 $2 $3 }

Fname: tIDENTIFIER { undefined }
  | tCONSTANT { undefined }
  | tFID { undefined }
  | Op { undefined }
  | Reswords { undefined }

Fsym :: { Term }
Fsym: Fname { mk_symbol $1 }
  | Symbol { $1 }

Fitem: Fsym { $1 }
     | Dsym { $1 }

UndefList :: { [Term] }
UndefList: Fitem { [ $1 ] }
  | UndefList tCOMMA Fitem { $1 ++ [$3] }

Op:   tPIPE   {$1} | tCARET {$1} | tAMPER2 {$1} | tCMP {$1} | tEQ    {$1} | tEQQ         {$1}
  |   tMATCH  {$1} | tNMATCH{$1} | tGT     {$1} | tGEQ {$1} | tLT    {$1} | tLEQ         {$1}
  |   tNEQ    {$1} | tLSHFT {$1} | tRSHFT  {$1} | tPLUS{$1} | tMINUS {$1} | tSTAR2       {$1}
  |   tSTAR   {$1} | tDIVIDE{$1} | tPERCENT{$1} | tPOW {$1} | tBANG  {$1} | tTILDE       {$1}
  |   tUPLUS  {$1} | tUMINUS{$1} | tAREF   {$1} | tASET{$1} | tDSTAR {$1} | tBACK_REF2   {$1}

Reswords: k__LINE__ {$1} | k__FILE__ {$1} | k__ENCODING__ {$1} | klBEGIN {$1} | klEND {$1}
    | kALIAS    {$1} | kAND      {$1} | kBEGIN        {$1} | kBREAK  {$1} | kCASE     {$1}
    | kCLASS    {$1} | kDEF      {$1} | kDEFINED      {$1} | kDO     {$1} | kELSE     {$1}
    | kELSIF    {$1} | kEND      {$1} | kENSURE       {$1} | kFALSE  {$1} | kFOR      {$1}
    | kIN       {$1} | kMODULE   {$1} | kNEXT         {$1} | kNIL    {$1} | kNOT      {$1}
    | kOR       {$1} | kREDO     {$1} | kRESCUE       {$1} | kRETRY  {$1} | kRETURN   {$1}
    | kSELF     {$1} | kSUPER    {$1} | kTHEN         {$1} | kTRUE   {$1} | kUNDEF    {$1}
    | kWHEN     {$1} | kYIELD    {$1} | kIF           {$1} | kUNLESS {$1} | kWHILE    {$1}
    | kUNTIL    {$1}

Arg :: { Term }
Arg: Lhs tEQL ArgRhs { mk_assign $1 $2 $3 }
  | VarLhs tOP_ASGN ArgRhs { (mk_op_assign $1 $2 $3) }
 --  | Primary tLBRACK2 OptCallArgs RBracket tOP_ASGN ArgRhs { mk_op_assign( mk_index($1, $2, $3, $4), $5, $6) }
 --  | Primary CallOp tIDENTIFIER tOP_ASGN ArgRhs { mk_op_assign( mk_call_method $1, $2, $3), $4, $5) }
 --  | Primary CallOp tCONSTANT tOP_ASGN ArgRhs { mk_op_assign( mk_call_method $1, $2, $3), $4, $5) }
 --  | Primary tCOLON2 tIDENTIFIER tOP_ASGN ArgRhs { mk_op_assign( mk_call_method $1, $2, $3), $4, $5) }
 --  | Primary tCOLON2 tCONSTANT tOP_ASGN ArgRhs {
 --       const  = mk_const_op_assignable( (mk_const_fetch $1 $2 $3))
 --       (mk_op_assign const $4 $5) }
 --  | tCOLON3 tCONSTANT tOP_ASGN ArgRhs {
 --       const  = mk_const_op_assignable((mk_const_global $1 $2))
 --       (mk_op_assign const $3 $4) }
 --  | Backref tOP_ASGN ArgRhs { mk_op_assign $1 $2 $3) }
  | Arg tDOT2 Arg { (mk_range_inclusive $1 $2 $3) }
  | Arg tDOT3 Arg { (mk_range_exclusive $1 $2 $3) }
  | Arg tDOT2 { (mk_range_inclusive $1 $2 Nil) }
  | Arg tDOT3 { (mk_range_exclusive $1 $2 Nil) }
  | Arg tPLUS Arg { (mk_binary_op $1 $2 $3) }
  | Arg tMINUS Arg { (mk_binary_op $1 $2 $3) }
  | Arg tSTAR2 Arg { (mk_binary_op $1 $2 $3) }
  | Arg tDIVIDE Arg { error "divide" }
  | Arg tPERCENT Arg { (mk_binary_op $1 $2 $3) }
  | Arg tPOW Arg { (mk_binary_op $1 $2 $3) }
 --  | tUNARY_NUM SimpleNumeric tPOW Arg { mk_unary_op($1, mk_binary_op( $2, $3, $4)) }
  |   tUPLUS Arg { (mk_unary_op $1 $2) }
  | tUMINUS Arg { (mk_unary_op $1 $2) }
  | Arg tPIPE Arg { (mk_binary_op $1 $2 $3) }
  | Arg tCARET Arg { (mk_binary_op $1 $2 $3) }
  | Arg tAMPER2 Arg { (mk_binary_op $1 $2 $3) }
  | Arg tCMP Arg { (mk_binary_op $1 $2 $3) }
--  | RelExpr =tCMP
  | Arg tEQ Arg { (mk_binary_op $1 $2 $3) }
  | Arg tEQQ Arg { (mk_binary_op $1 $2 $3) }
  | Arg tNEQ Arg { (mk_binary_op $1 $2 $3) }
  | Arg tMATCH Arg { (mk_match_op $1 $2 $3) }
  | Arg tNMATCH Arg { (mk_binary_op $1 $2 $3) }
  | tBANG Arg { (mk_not_op $1 Nil $2 Nil) }
  | tTILDE Arg { (mk_unary_op $1 $2) }
  | Arg tLSHFT Arg { (mk_binary_op $1 $2 $3) }
  | Arg tRSHFT Arg { (mk_binary_op $1 $2 $3) }
  | Arg tANDOP Arg { mkLogicalOp And $1 $2 $3 }
  | Arg tOROP Arg { mkLogicalOp Or $1 $2 $3 }
 --  | kDEFINED OptNl Arg { (mk_keyword_cmd Defined $1 Nil [ $3 ] Nil }
 --  | Arg tEH Arg OptNl tCOLON Arg { mk_ternary $1 $2 $3 $5 $6 }
 --  | Primary

Relop: tGT { $1 }
  | tLT { $1 }
  | tGEQ { $1 }
  | tLEQ { $1 }

RelExpr: Arg Relop Arg {-=-} tGT { mk_binary_op $1 $2 $3 }
  | RelExpr Relop Arg {-=-} tGT { mk_binary_op $1 $2 $3 }

ArefArgs: None { undefined }
  | Args Trailer { undefined }
  | Args tCOMMA Assocs Trailer { $1 ++ [mk_associate Nil $3 Nil] }
  | Assocs Trailer { [ (mk_associate Nil $1 Nil) ] }

ArgRhs: Arg {-=-} tOP_ASGN { $1 }
  | Arg kRESCUE_MOD Arg { mk_begin_body $1 [ (mk_rescue_body $2 Nil Nil Nil Nil $3) ] }

ParenArgs: tLPAREN2 OptCallArgs Rparen { $1 }

OptParenArgs: {- nothing -} { error "[ Nil, [], Nil ]" }
  | ParenArgs { $1 }

OptCallArgs: {- nothing -} { [] }
  | CallArgs { undefined } -- { $1 }
  | Args tCOMMA { undefined } -- { $1 }
  | Args tCOMMA Assocs tCOMMA { undefined } -- { $1 ++ [mk_associate Nil $3 Nil] }
  | Assocs tCOMMA { undefined } -- { [ mk_associate Nil $1 Nil ] }

CallArgs: Command { [ $1 ] }
  | Args OptBlockArg { $1 ++ $2 }
  | Assocs OptBlockArg { [ mk_associate Nil $1 Nil ] ++ $2  }
  | Args tCOMMA Assocs OptBlockArg { $1 ++ [mk_associate Nil $3 Nil] ++ $4 }
  | BlockArg { [ $1 ] }

CommandArgs: CallArgs { $1 }

{-
CommandArgs:   {
            # When branch gets invoked by RACC's lookahead
            # and Command Args start with '[' or '('
            # we need to put `true` to the cmdarg stack
            # **before** `false` pushed by lexer
            #   m [], n
            #     ^
            # Right here we have cmdarg [...0] because
            # lexer pushed it on '['
            # We need to modify cmdarg stack to [...10]
            #
            # For all other Cases (like `m n` or `m n, []`) we simply put 1 to the stack
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
        CallArgs {
            # CallArgs can be followed by tLBRACE_ARG (that does cmdarg.push(0) in the lexer)
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
 $2 }

 -}
BlockArg: tAMPER Arg { mk_block_pass $1 $2 }
OptBlockArg: tCOMMA BlockArg { [ $2 ] }
  | {- nothing -} { [] }

Args :: { [Term] }
Args: Arg { [ $1 ] }
  | tSTAR Arg { [mk_splat $1 $2] }
  | Args tCOMMA Arg { $1 ++ [$3] }
  | Args tCOMMA tSTAR Arg { $1 ++ [mk_splat $3 $4] }

MrhsArg: Mrhs { mk_array Nil $1 Nil }
  | Arg { undefined }

Mrhs: Args tCOMMA Arg { $1 ++ [$3] }
  | Args tCOMMA tSTAR Arg { $1 ++ [mk_splat $3 $4] }
  | tSTAR Arg { undefined } -- { [ mk_splat $1 $2 ] }

Primary :: { Term }
Primary: Literal { $1 }
  | Strings { $1 }
  | Xstring { $1 }
--      | Regexp
  | Words { $1 }
  | Qwords { $1 }
  | Symbols { $1 }
  | Qsymbols { $1 }
  | VarRef { $1 }
  | Backref { $1 }
  | tFID { mk_call_method $1 }
  | kBEGIN Bodystmt kEND { mk_begin_keyword $2 }
  | tLPAREN_ARG Stmt Rparen { mk_begin $2 $3 }
  | tLPAREN_ARG OptNl tRPAREN { mk_begin $2 }
  | tLPAREN Compstmt tRPAREN { mk_begin $2 }
  | Primary tCOLON2 tCONSTANT { mk_const_fetch $1 }
  | tCOLON3 tCONSTANT { mk_const_global $1 $2 }
  | tLBRACK ArefArgs tRBRACK { mk_array $1 $2 $3 }
  | tLBRACE AssocList tRCURLY KReturn { mk_associate $1 $2 $3 }  -- { mkeyword_cmd Return $1 }
  |  kYIELD tLPAREN2 CallArgs Rparen { mk_keyword_cmd Yield $1 $2 $3 $4 }
  | kYIELD tLPAREN2 Rparen { mk_keyword_cmd Yield $1 $2 [] $3 }
      -- | kYIELD { mk_keyword_cmd Yield $1 }
      -- | kDEFINED OptNl tLPAREN2 Expr Rparen { (mk_keyword_cmd :defined?, $1, $3 [ $4 ] $5) }
      -- | kNOT tLPAREN2 Expr Rparen { mk_not_op $1 $2 $3 $4 }
      -- | kNOT tLPAREN2 Rparen { mk_not_op $1 $2 Nil $3 }
      -- | Operation BraceBlock { let (begin_t, Args, body, end_t) = $2 in mk_block (mk_call_method Nil Nil $1) begin_t Args body end_t }
      -- | MethodCall
      -- | MethodCall BraceBlock { let (begin_t, Args, body, end_t) = $2 in (mk_block $1 begin_t Args body end_t) }
      -- | tLAMBDA Lambda { let (args, (begin_t, body, end_t)) = $2 in (mk_block (mk_call_lambda $1) begin_t args body end_t) }
      -- | kIF Expr Then Compstmt IfTail kEND { let (else_t, else_) = $5 in (mk_condition $1 $2 $3 $4 else_t else_  $6) }
      -- | kUNLESS Expr Then Compstmt OptElse kEND { let (else_t, else_) = $5 in (mk_condition $1 $2 $3 else_  else_t $4 $6) }
      -- | kWHILE ExprValueDo Compstmt kEND  { (mk_loop While $1 $2 $3 $4)  }
      -- | kUNTIL ExprValueDo Compstmt kEND  { (mk_loop Until $1 $2 $3 $4) }
      -- | kCASE Expr OptTerms CaseBody kEND
      --     {
      --       *when_bodies, (else_t, else_body) = *$4
      --       (mk_case $1 $2 when_bodies else_t else_body $5) }
      --       | kCASE OptTerms CaseBody kEND {
      --             *when_bodies, (else_t, else_body) = *$3
      --             mk_case($1, Nil, when_bodies, else_t, else_body, $4) }
      --       | kFOR for_--var kIN ExprValueDo Compstmt kEND  { (mk_for $1, $2, $3, *$4 $5 $6)
      --     }
      -- | kCLASS Cpath superclass Bodystmt kEND { let (lt_t, superclass) = $3 in (mk_def_class $1 $2 lt_t superclass $5 $6) }
      -- | kCLASS tLSHFT Expr Term Bodystmt kEND { mk_def_sclass $1 $2 $3 $6 $7 }
      -- | kMODULE Cpath Bodystmt kEND { (mk_def_module $1 $2 $4 $5) }
      -- | kDEF Fname FArglist Bodystmt kEND { (mk_def_method $1 $2 $4 $5 $6) }
      -- | kDEF Singleton DotOrColon Fname FArglist Bodystmt kEND { mk_def_singleton $1 $2 $3 $5 $7 $8 $9 }
  | kBREAK { mk_keyword_cmd Break $1 }
  | kNEXT { mk_keyword_cmd Next $1 }
  | kREDO { mk_keyword_cmd Redo $1 }
  | kRETRY { mk_keyword_cmd Retry $1 }

KReturn: kRETURN { error ":invalid_return, Nil, $1 if @context.in_class?"  }

Then: Term { $1 }
  | kTHEN { undefined }
  | Term kTHEN { $1 }

Do: Term { undefined }
  | kDO_COND { undefined }

{-

IfTail: OptElse { undefined }
  | kELSIF Expr Then Compstmt IfTail { else_t, else_ = $5 [ $1, mk_condition($1, $2, $3, $4, else_t, else_,  Nil) ] }

-}

OptElse: None { undefined } -- { $1 }
  | kELSE Compstmt { undefined } --{ $2 }

FMarg: FNormArg { mk_arg $1 }
  | tLPAREN FMargs Rparen { mk_multi_lhs $1 $2 $3 }

FMargList: FMarg { [ $1 ] }
  | FMargList tCOMMA FMarg { $1 ++ [$3] }

FMargs: FMargList { $1 }
  -- | FMargList tCOMMA tSTAR FNormArg { $1. push(mk_restarg $3 $4) }
  -- | FMargList tCOMMA tSTAR FNormArg tCOMMA FMargList { $1. push(mk_restarg $3 $4). ++ $6 }
  -- | FMargList tCOMMA tSTAR { $1. push(mk_restarg($3)) }
  -- | FMargList tCOMMA tSTAR tCOMMA FMargList { $1. push(mk_restarg($3)). ++ $5 }
  -- | tSTAR FNormArg { [ (mk_restarg $1 $2) ] }
  -- | tSTAR FNormArg tCOMMA FMargList { [ (mk_restarg $1 $2), *$4 ] }
  | tSTAR { [ mk_restarg $1 ] }
  -- | tSTAR tCOMMA FMargList { [ mk_restarg($1), *$3 ] }
{-
BlockArgsTail: FBlockKwarg tCOMMA FKwrest OptFBlockArg { $1 ++ $3 ++ $4 }
  | FBlockKwarg OptFBlockArg { $1 ++ $2) }
  | FKwrest OptFBlockArg { $1 ++ $2) }
  | FBlockArg { [ $1 ] }

OptBlockArgsTail: tCOMMA BlockArgsTail { $2 }
  -- | {- nothing -} { [] }

BlockParam: FArg tCOMMA FBlockOptarg tCOMMA FRestArg OptBlockArgsTail { $1 ++ $3 ++ $5 ++ $6 }
      | FArg tCOMMA FBlockOptarg tCOMMA FRestArg tCOMMA FArg OptBlockArgsTail { $1 ++ $3 ++ $5 ++ $7 ++ $8 }
      | FArg tCOMMA FBlockOptarg OptBlockArgsTail { $1 ++ $3 ++ $4 }
      | FArg tCOMMA FBlockOptarg tCOMMA FArg OptBlockArgsTail { $1 ++ $3 ++ $5 ++ $6 }
      | FArg tCOMMA FRestArg OptBlockArgsTail { $1 ++ $3 ++ $4 }
      | FArg tCOMMA
      | FArg tCOMMA FRestArg tCOMMA FArg OptBlockArgsTail { $1 ++ $3 ++ $5 ++ $6 }
      | FArg OptBlockArgsTail { if null $2 && length $1 == 1 then [mk_procarg0 $1[0]] else $1 ++ $2 }
      | FBlockOptarg tCOMMA FRestArg OptBlockArgsTail { $1 ++ $3 ++ $4 }
      | FBlockOptarg tCOMMA FRestArg tCOMMA FArg OptBlockArgsTail { $1 ++ $3 ++ $5 ++ $6 }
      | FBlockOptarg OptBlockArgsTail { $1 ++ $2 }
      | FBlockOptarg tCOMMA FArg OptBlockArgsTail { $1 ++ $3 ++ $4 }
      | FRestArg OptBlockArgsTail { $1 ++ $2 }
      | FRestArg tCOMMA FArg OptBlockArgsTail { $1 ++ $3 ++ $4 }
      | BlockArgsTail
-}

OptBlockParam: {- nothing -} { mk_args Nil [] Nil }
  | BlockParamDef { $1 }

BlockParamDef: -- tPIPE opt_bv_decl tPIPE { mk_args $1 $2 $3 }
  tOROP { (mk_args $1 [] $1) }
--  | tPIPE BlockParam opt_bv_decl tPIPE { mk_args $1 ($2 ++ $3) $4 }



{-
opt_bv_decl: OptNl { [] }
  | OptNl tSEMI BvDecls OptNl { $3 }

BvDecls: Bvar { [ $1 ] }
      | BvDecls tCOMMA Bvar { $1 ++ [$3] }

Bvar: tIDENTIFIER { mk_shadowarg($1) }
    | FBadArg

Lambda: FLarglist { @lexer.cmdarg.push(false) } LambdaBody { @lexer.cmdarg.pop; [ $2, $4 ]; @static_env.unextend }

FLarglist: tLPAREN2 FArgs opt_bv_decl tRPAREN { (mk_args $1 $2.concat($3) $4) }
  | FArgs { (mk_args Nil $1 Nil) }

LambdaBody: tLAMBEG { @context.push(:Lambda) } Compstmt tRCURLY { [ $1, $3, $4 ] @context.pop }
  | kDO_LAMBDA { @context.push(:Lambda) } Bodystmt kEND { [ $1, $3, $4 ] @context.pop }

            -}
DoBlock: kDO_BLOCK  DoBody kEND { undefined } -- { @context.push(:block) } { [ $1, *$3, $4 ] @context.pop }
BlockCall: Command DoBlock { undefined }
{-
BlockCall: Command DoBlock { let (begin_t, block_args, body, end_t) = $2 in (mk_block $1 begin_t block_args body end_t) }
  | BlocAall DotOrColon Operation2 OptParen_args { let (lparen_t, Args, rparen_t) = $4 in mk_call_method $1 $2 $3 lparen_t Args rparen_t } -- | BlockCall DotOrColon Operation2 OptParen_args BraceBlock { lparen_t, Args, rparen_t = $4
            MethodCall = mk_call_method $1, $2, $3,
                            lparen_t, Args, rparen_t)

            begin_t, Args, body, end_t = $5
            result      = mk_block(MethodCall,
                            begin_t, Args, body, end_t) }
      | BlockCall DotOrColon Operation2 CommandArgs DoBlock {
            MethodCall = mk_call_method $1, $2, $3,
                            Nil, $4, Nil)

            begin_t, Args, body, end_t = $5
            result      = mk_block(MethodCall,
                            begin_t, Args, body, end_t) }

MethodCall: Operation ParenArgs { let (lparen_t, Args, rparen_t) = $2 in mk_call_method Nil Nil $1 lparen_t Args rparen_t }
      | PrAary CallOp Operation2 OptParen_args { let let (lparen_t, Args, rparen_t) = $4 in mk_call_method $1, $2, $3, lparen_t, Args, rparen_t }
      | Primary tCOLON2 Operation2 ParenArgs { let (lparen_t, Args, rparen_t) = $4 in mk_call_method $1 $2 $3 lparen_t Args rparen_t }
      | Primary tCOLON2 operation3 { mk_call_method $1 $2 $3 }
      | Primary CallOp ParenArgs { let (lparen_t, Args, rparen_t) = $3 in mk_call_method $1, $2, Nil, lparen_t, Args, rparen_t }
      | Primary tCOLON2 ParenArgs { let (lparen_t, Args, rparen_t) = $3 in mk_call_method $1, $2, Nil, lparen_t, Args, rparen_t }
      | kSUPER ParenArgs { let (lparen_t, Args, rparen_t) = $2 in mk_keyword_cmd Super $1 lparen_t Args rparen_t }
      | kSUPER { mk_keyword_cmd Zsuper $1 }
      | Primary tLBRACK2 OptCallArgs RBracket { (mk_index $1, $2 $3 $4 }

-}
BraceBlock: tLCURLY BraceBody tRCURLY { undefined }
  | kDO DoBody kEND  { undefined }

BraceBody: OptBlockParam Compstmt { undefined }

DoBody: OptBlockParam Bodystmt { undefined }-- { @static_env.extend_dynamic } { @lexer.cmdarg.push(false) } { result = [ val[2], val[3] ] @static_env.unextend @lexer.cmdarg.pop  }

CaseBody: kWHEN Args Then Compstmt Cases { undefined } -- { [mk_when $1 $2 $3 $4] ++ $5 }

Cases: OptElse { undefined } -- { [ $1 ] }
 | CaseBody { undefined } -- { $1 }

OptRescue: kRESCUE ExcList ExcVar Then Compstmt OptRescue { undefined } {-
            assoc_t, ExcVar = $3
            if $2
              ExcList = (mk_array Nil $2 Nil)
            end
            [ mk_rescue_body $1, ExcList, assoc_t, ExcVar, $4, $5), *$6 ] -}
  -- | { [] }

ExcList: Arg { undefined } -- { [ $1 ] }
  | Mrhs { undefined } -- { $1 }
  | None { undefined } -- { $1 }

ExcVar: tASSOC Lhs { undefined } --{ [ $1, $2 ] }
  | None { undefined } -- { $1 }

OptEnsure: kENSURE Compstmt { undefined } -- { undefined }
  | None { undefined } -- { $1 }

Literal :: { Term }
Literal: Numeric { $1 }
  | Symbol { $1 }
  | Dsym { $1 }

Strings :: { Term }
Strings: String { mk_string_compose $1 }

String :: { [Term] }
String: String1 { [ $1 ] }
  | String String1 { $1 ++ [$2] }

String1: tSTRING_BEG StringContents tSTRING_END { mk_string_compose $2 }
  | tSTRING { mk_string $1 }
  | tCHARACTER { mk_character $1 }

Words :: { Term }
Words: tWORDS_BEG WordList tSTRING_END { mk_words_compose $1 $2 $3 }

WordList :: { [Term] }
WordList: {- nothing -} { [] }
  | WordList Word tSPACE { $1 ++ [mk_word $2] }

Word: StringContent { [ $1 ] }
  | Word StringContent { $1 ++ [$2] }

Symbols :: { Term }
Symbols: tSYMBOLS_BEG SymbolList tSTRING_END { mk_symbols_compose $1 $2 $3 }

SymbolList :: { [Term] }
SymbolList:  {- nothing -} { [] }
  | SymbolList Word tSPACE { $1 ++ [mk_word $2] }

Xstring :: { Term  }
Xstring: tXSTRING_BEG XStringContents tSTRING_END { mk_xstring_compose $2 }

XStringContents :: { [Term] }
XStringContents: {- nothing -} { [] }
  | XStringContents StringContent { $1 ++ [$2] }

{-

RLgexp: tREGEXP_BEG RegexpContents tSTRING_END tREGEXP_OPT {
            opts   = mk_regexp_options($4)
            (mk_regexp_compose $1, $2 $3 opts) }
-}
Qwords :: { Term }
Qwords: tQWORDS_BEG QwordList tSTRING_END {  mk_words_compose $1 $2 $3 }

Qsymbols :: { Term }
Qsymbols: tQSYMBOLS_BEG QsymList tSTRING_END { mk_symbols_compose $1 $2 $3 }

QwordList :: { [Term] }
QwordList: {- nothing -} { [] }
  | QwordList tSTRING_CONTENT tSPACE { $1 ++ [mk_string_internal $2 ] }

QsymList :: { [Term] }
QsymList: {- nothing -} { [] }
  | QsymList tSTRING_CONTENT tSPACE { $1 ++ [mk_symbol_internal $2] }

{-
RegexpContents: {- nothing -} { [] }
  | RegexpContents StringContent { $1 ++ [$2] }
-}

StringContents: {- nothing -} { [] }
  | StringContents StringContent { $1 ++ [$2] }

StringContent: tSTRING_CONTENT { mk_string_internal $1 }
  | tSTRING_DVAR StringDvar { $2 }
  | tSTRING_DBEG Compstmt tSTRING_DEND { mk_begin $1 $2 $3 } -- { @lexer.cmdarg.push(false); @lexer.cond.push(false); @lexer.cmdarg.pop @lexer.cond.pop

StringDvar: tGVAR { mk_gvar $1 }
  | tIVAR { mk_ivar $1 }
  | tCVAR { mk_cvar $1 }
  | Backref { undefined }

Symbol :: { Term }
Symbol: tSYMBOL { mk_symbol $1 }

Dsym: tSYMBEG XStringContents tSTRING_END { mk_symbol_compose $1 $2 $3 }

Numeric: SimpleNumeric { $1 }
  | tUNARY_NUM SimpleNumeric tLOWEST { mk_unary_num $1 $2 }

SimpleNumeric: tINTEGER { mk_integer $1 }
  | tFLOAT { mk_float $1 }
-- -- | tRATIONAL { mk_rational($1) }
  | tIMAGINARY { mk_complex $1 }

UserVariable :: { Term }
UserVariable: tIDENTIFIER { mk_ident $1 }
  | tIVAR { mk_ivar $1 }
  | tGVAR { mk_gvar $1 }
  | tCONSTANT { mk_const $1 }
  | tCVAR { mk_cvar $1 }

KeywordVariable :: { Term }
KeywordVariable : kNIL {Nil}
  | kSELF {Self}
  | kTRUE {RTrue}
  | kFALSE {RFalse}
  | k__FILE__ {File}
  | k__LINE__ {Line}
  | k__ENCODING__ {Encoding}

VarRef: UserVariable { mk_accessible($1) }
  | KeywordVariable { mk_accessible($1) }

VarLhs: UserVariable { mk_assignable $1 }
  | KeywordVariable { mk_assignable $1 }

Backref: tNTH_REF { mk_nth_ref $1 }
  | tBACK_REF { mk_back_ref $1 }
{-
superclass: tLT {
            @lexer.state = :Expr }
          Expr Term {
            [ $1, $3 ] }
      | {- nothing -} {
            Nil }

FArglist: tLPAREN2 FArgs Rparen { (mk_args $1 $2 $3) }
  | FArgs Term { mk_args Nil $2 Nil }

ArgsTail: FKwarg tCOMMA FKwrest OptFBlockArg { $1.concat($3).concat($4) }
  | FKwarg OptFBlockArg { $1.concat($2) }
  | FKwrest OptFBlockArg { $1.concat($2) }
  | FBlockArg { [ $1 ] }

OptArgsTail: tCOMMA ArgsTail { $2 }
  -- | {- nothing -} { [] }

FArgs: FArg tCOMMA FOptarg tCOMMA FRestArg OptArgsTail { $1 ++ $3 ++ $5 ++ $6 }
      | FArg tCOMMA FOptarg tCOMMA FRestArg tCOMMA FArg OptArgsTail { $1 ++ $3 ++ $5 ++ $7 ++ $8 }
      | FArg tCOMMA FOptarg OptArgsTail { $1 ++ $3 ++ $4 }
      | FArg tCOMMA FOptarg tCOMMA FArg OptArgsTail { $1 ++ $3 ++ $5 ++ $6 }
      | FArg tCOMMA FRestArg OptArgsTail { $1 ++ $3 ++ $4 }
      | FArg tCOMMA FRestArg tCOMMA FArg OptArgsTail { $1 ++ $3 ++ $5 ++ $6 }
      | FArg OptArgsTail { $1 ++ $2 }
      | FOptarg tCOMMA FRestArg OptArgsTail { $1 ++ $3 ++ $4 }
      | FOptarg tCOMMA FRestArg tCOMMA FArg OptArgsTail { $1 ++ $3 ++ $5 ++ $6 }
      | FOptarg OptArgsTail { $1 ++ $2 }
      | FOptarg tCOMMA FArg OptArgsTail { $1 ++ $3 ++ $4 }
      | FRestArg OptArgsTail { $1 ++ $2 }
      | FRestArg tCOMMA FArg OptArgsTail { $1 ++ $3 ++ $4 }
      | ArgsTail { $1 }
--    | {- nothing -} { [] }

-}

FBadArg: tCONSTANT { error ":argument_const, Nil, $1" }
  | tIVAR { error ":argument_ivar, Nil, $1" }
  | tGVAR { error ":argument_gvar, Nil, $1" }
  | tCVAR { error ":argument_cvar, Nil, $1" }

FNormArg: FBadArg { $1 }
  | tIDENTIFIER { $1 }

FArgAsgn: FNormArg { $1 }

FArgItem: FArgAsgn { mk_arg $1 }
  | tLPAREN FMargs Rparen { mk_multi_lhs $1 $2 $3 }

FArg: FArgItem { [ $1 ] }
  | FArg tCOMMA FArgItem { $1 ++ [$3] }

FLabel: tLABEL { undefined } -- { check_kwarg_name($1) @static_env.declare $1[0] $1 }

FKw: FLabel Arg { mk_kwoptarg $1 $2 }
  | FLabel { mk_kwarg $1 }

FBlockKw: FLabel Primary { mk_kwoptarg $1 $2 }
  | FLabel { mk_kwarg $1 }

FBlockKwarg: FBlockKw { [ $1 ] }
  | FBlockKwarg tCOMMA FBlockKw { $1 ++ [$3] }

FKwarg: FKw { [ $1 ] }
  | FKwarg tCOMMA FKw { $1 ++ [$3] }

KwrestMark: tPOW { $1 }
  | tDSTAR { $1 }

FKwrest: KwrestMark tIDENTIFIER {  [ mk_kwrestarg $1 $2 ] }
  | KwrestMark { [ mk_kwrestarg($1) ] }

FOpt: FArgAsgn tEQL Arg { mk_optarg $1 $2 $3 }
{-
FBlockOpt: FArgAsgn tEQL Primary { mk_optarg $1 $2 $3 }

FBlockOptarg: FBlockOpt { [ $1 ] }
  | FBlockOptarg tCOMMA FBlockOpt { $1 ++ [$3] }

FOptarg: FOpt { [ $1 ] }
  | FOptarg tCOMMA FOpt { $1 ++ [$3] }
-}
RestargMark: tSTAR2 { undefined }
  | tSTAR { undefined }

FRestArg :: { [Term] }
FRestArg: RestargMark tIDENTIFIER { [ mk_restarg $1 $2 ] }
  | RestargMark { [ mk_restarg $1 ] }

BlkargMark: tAMPER2 { undefined }
  | tAMPER { undefined }
{-
FBlockArg: BlkargMark tIDENTIFIER { mk_blockarg $1 $2 }

OptFBlockArg: tCOMMA FBlockArg { [ $2 ] }
  | { [] }
-}
Singleton: VarRef { $1 }
  | tLPAREN2 Expr Rparen { $2 }

AssocList: {- nothing -} { [] }
  | Assocs Trailer { $1 }

Assocs: Assoc { [ $1 ] }
  | Assocs tCOMMA Assoc { $1 ++ [$3] }

Assoc: Arg tASSOC Arg { mk_pair $1 $2 $3 }
   | tLABEL Arg { mk_pair_keyword $1 $2 }
--    | tSTRING_BEG StringContents tLABEL_END Arg { mk_pair_quoted $1, $2 $3 $4 }
   | tDSTAR Arg { mk_kwsplat $1 $2 }

Operation: tIDENTIFIER { $1 }
  | tCONSTANT { $1 }
  | tFID { $1 }

Operation2: tIDENTIFIER { $1 }
  | tCONSTANT { $1 }
  | tFID { $1 }
  | Op { $1 }

operation3: tIDENTIFIER { $1 }
  | tFID { $1 }
  | Op { $1 }

DotOrColon: CallOp { $1 }
  | tCOLON2 { $1 }

CallOp: tDOT { error "CallOp" } -- [Dot, ($1 !! 2)]
  | tANDDOT { error "CallOp" } -- [Anddot, ($1 !! 2)]

OptTerms: { undefined }
  | Terms { undefined }

OptNl: { undefined }
  | tNL { undefined }

Rparen: OptNl tRPAREN { undefined }
RBracket: OptNl tRBRACK { undefined }

Trailer: { undefined }
  | tNL { undefined }
  | tCOMMA { undefined }

Term: tSEMI { undefined }
  | tNL { undefined }

Terms: Term { undefined }
  | Terms tSEMI { undefined }

None: { undefined } -- { Nil }

{
parseError _ = throwError "!Parse Error"
}
