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
    tNTH_REF {L.TNTH_REF {}}
    tBACK_REF {L.TBACK_REF {}}
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
    -- tOP_ASGN {L.TOP_ASGN} FIXME lexer should produce different tokens
    tOP_ASGN {L.TEQL}
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
    tSYMBEG {L.TSYMBEG {}}
    tSTRING_BEG {L.TSTRING_BEG {} }
    tXSTRING_BEG {L.TXSTRING_BEG {}}
    tREGEXP_BEG {L.TREGEXP_BEG {}}
    tREGEXP_OPT {L.TREGEXP_OPT}
    tWORDS_BEG {L.TWORDS_BEG {}}
    tQWORDS_BEG {L.TQWORDS_BEG {}}
    tSYMBOLS_BEG {L.TSYMBOLS_BEG {}}
    tQSYMBOLS_BEG {L.TQSYMBOLS_BEG {}}
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
    tCHARACTER {L.TCHARACTER {}}
    tRATIONAL {L.TRATIONAL}
    tIMAGINARY {L.TIMAGINARY {}}
    tLABEL_END {L.TLABEL_END}
    tANDDOT {L.TANDDOT}


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
TopStmt: Stmt { $1 }
  | klBEGIN BeginBlock { error "mk_preexe $2" }

BeginBlock: tLCURLY TopCompstmt tRCURLY { $1 }

Bodystmt :: { Term }
Bodystmt: Compstmt OptRescue OptElse OptEnsure { mk_begin_body $1 $2 $3 $4  }

Compstmt :: { Term }
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
  | kALIAS tGVAR tGVAR { mk_alias (mk_gvar $2) (mk_gvar $3) }
  | kALIAS tGVAR tBACK_REF { mk_alias (mk_gvar $2) (mk_back_ref $3) }
  | kALIAS tGVAR tNTH_REF { error ":nth_ref_alias, Nil, $3" }
  | kUNDEF UndefList { (mk_undef_method $2) }
  | Stmt kIF_MOD Expr { mk_condition_mod $1 Nil $3 }
  | Stmt kUNLESS_MOD Expr { mk_condition_mod Nil $1 $3}
  | Stmt kWHILE_MOD Expr { mk_loop_mod $1 $2 $3 }
  | Stmt kUNTIL_MOD Expr { mk_loop_mod $1 $2 $3 }
  | Stmt kRESCUE_MOD Stmt { mk_begin_body' $1 [mk_rescue_body Nil Nil $3] }
  | klEND tLCURLY Compstmt tRCURLY { mk_postexe $3 }
  | CommandAsgn { $1 }
  | Mlhs tEQL CommandCall { mk_multiassign $1 $3 }
  | Lhs tEQL Mrhs { mk_assign $1 (mk_array $3) }
  | Mlhs tEQL MrhsArg { mk_multiassign $1 $3 }
  | Expr { $1 }

CommandAsgn :: { Term }
CommandAsgn: Lhs tEQL CommandRhs { mk_assign $1 $3 }
  | VarLhs tOP_ASGN CommandRhs { (mk_op_assign $1 $3) }
  | Primary tLBRACK2 OptCallArgs RBracket tOP_ASGN CommandRhs { mk_op_assign (mk_index $1 $2 $3 $4) $6 }
  | Primary CallOp tIDENTIFIER tOP_ASGN CommandRhs { mk_op_assign (mk_call_method $1 $2 $3 []) $5 }
  | Primary CallOp tCONSTANT tOP_ASGN CommandRhs { mk_op_assign  (mk_call_method $1 $2 $3 []) $5 }
  | Primary tCOLON2 tCONSTANT tOP_ASGN CommandRhs { mk_op_assign (mk_const_op_assignable (mk_const_fetch $1 $3)) $5 }
  | Primary tCOLON2 tIDENTIFIER tOP_ASGN CommandRhs { mk_op_assign (mk_call_method $1 $2 $3 []) $5 }
  | Backref tOP_ASGN CommandRhs { mk_op_assign $1 $3 }

CommandRhs :: { Term }
CommandRhs: CommandCall %prec tOP_ASGN { $1 }
  | CommandCall kRESCUE_MOD Stmt { mk_begin_body' $1 [mk_rescue_body Nil Nil $3] }
  | CommandAsgn { $1 }

Expr :: { Term }
Expr: CommandCall { $1 }
  | Expr kAND Expr { mkLogicalOp And $1 $3 }
  | Expr kOR Expr { mkLogicalOp Or $1 $3 }
  | kNOT OptNl Expr { mk_not_op $3 }
  | tBANG CommandCall { mk_not_op $2 }
  | Arg { $1 }

ExprValueDo: --  { @lexer.cond.push(true) }
  Expr Do { undefined } -- { @lexer.cond.pop; [ $2, $3 ] }

CommandCall :: { Term }
CommandCall: Command { $1 }
  | BlockCommand { $1 }

BlockCommand :: { Term }
BlockCommand: BlockCall { $1 }
  | BlockCall DotOrColon Operation2 CommandArgs { mk_call_method $1 $2 $3 $4 }

CmdBraceBlock: tLBRACE_ARG BraceBody tRCURLY { error "CmdBraceBlock" }

Command :: { Term }
Command: Operation CommandArgs %prec tLOWEST { mk_call_method Nil L.KNIL $1 $2 }
  | Operation CommandArgs CmdBraceBlock { error "Command" }  -- { let (begin_t, args, body, end_t) = $3 in (mk_block (mk_call_method Nil Nil $1 Nil $2 Nil) begin_t args body end_t) }
  | Primary CallOp Operation2 CommandArgs %prec tLOWEST { error "mk_call_method $1 $2 $3 $4" }
  | Primary CallOp Operation2 CommandArgs CmdBraceBlock { error "mk_block' $1 $2 $3 $4 $5" }
  | Primary tCOLON2 Operation2 CommandArgs %prec tLOWEST { error "mk_call_method $1 $3 $4" }
  | Primary tCOLON2 Operation2 CommandArgs CmdBraceBlock { error "mk_block' $1 $3 $4 $5" }
  | kSUPER CommandArgs { mk_keyword_cmd Super $2 }
  | kYIELD CommandArgs { mk_keyword_cmd Yield $2 }
  | KReturn CallArgs { mk_keyword_cmd Return $2 }
  | kBREAK CallArgs { mk_keyword_cmd Break $2 }
  | kNEXT CallArgs { mk_keyword_cmd Next $2 }

Mlhs :: { Term }
Mlhs: MlhsBasic { mk_multi_lhs $1 }
  | tLPAREN MlhsInner Rparen { mk_begin $2 }

MlhsInner :: { Term }
MlhsInner: MlhsBasic { mk_multi_lhs $1 }
  | tLPAREN MlhsInner Rparen { mk_multi_lhs [$2] }

MlhsBasic :: { [Term] }
MlhsBasic: MlhsHead { $1 }
  | MlhsHead MlhsItem { $1 ++ [$2] }
  | MlhsHead tSTAR MlhsNode { $1 ++ [mk_splat $3] }
  | MlhsHead tSTAR MlhsNode tCOMMA MlhsPost { $1 ++ mk_splat $3 : $5 }
  | MlhsHead tSTAR { $1 ++ [mk_splat Nil] }
  | MlhsHead tSTAR tCOMMA MlhsPost { $1 ++ mk_splat Nil : $4 }
  | tSTAR MlhsNode { [mk_splat $2] }
  | tSTAR MlhsNode tCOMMA MlhsPost { mk_splat $2 : $4 }
  | tSTAR { [mk_splat Nil] }
  | tSTAR tCOMMA MlhsPost { mk_splat Nil : $3 }

MlhsItem :: { Term }
MlhsItem: MlhsNode { $1 }
      | tLPAREN MlhsInner Rparen { mk_begin $2 }

MlhsHead :: { [Term] }
MlhsHead: MlhsItem tCOMMA { [ $1 ] }
  | MlhsHead MlhsItem tCOMMA { $1 ++ [$2] }

MlhsPost :: { [Term] }
MlhsPost: MlhsItem { [ $1 ] }
  | MlhsPost tCOMMA MlhsItem { $1 ++ [$3] }

MlhsNode :: { Term }
MlhsNode: UserVariable { mk_assignable $1 }
  | KeywordVariable { mk_assignable $1 }
  | Primary tLBRACK2 OptCallArgs RBracket { error "mk_index_asgn $1 $2 $3 $4" }
  | Primary CallOp tIDENTIFIER { mk_attr_asgn $1 $2 $3 }
  | Primary tCOLON2 tIDENTIFIER { mk_attr_asgn $1 $2 $3 }
  | Primary CallOp tCONSTANT { mk_attr_asgn $1 $2 $3 }
  | Primary tCOLON2 tCONSTANT { mk_assignable (mk_const_fetch $1 $3) }
  | tCOLON3 tCONSTANT { mk_assignable (mk_const_global $1 $2) }
  | Backref { mk_assignable $1 }

Lhs :: { Term }
Lhs: UserVariable { mk_assignable $1 }
  | KeywordVariable { mk_assignable $1 }
  | Primary tLBRACK2 OptCallArgs RBracket { error "mk_index_asgn $1 $2 $3 $4" }
  | Primary CallOp tIDENTIFIER { (mk_attr_asgn $1 $2 $3) }
  | Primary tCOLON2 tIDENTIFIER { (mk_attr_asgn $1 $2 $3) }
  | Primary CallOp tCONSTANT { (mk_attr_asgn $1 $2 $3) }
  | Primary tCOLON2 tCONSTANT { mk_assignable (mk_const_fetch $1 $3) }
  | tCOLON3 tCONSTANT { mk_assignable (mk_const_global $1 $2) }
  | Backref { mk_assignable $1 }

Cname :: { L.Token }
Cname: tIDENTIFIER { error ":module_name_const, Nil, $1" }
  | tCONSTANT { $1 }

Cpath :: { Term }
Cpath: tCOLON3 Cname { error "mk_const_global $1 $2" }
  | Cname { error "mk_const $1" }
  | Primary tCOLON2 Cname { mk_const_fetch $1 $3 }

Fname :: { L.Token }
Fname: tIDENTIFIER { $1 }
  | tCONSTANT { $1 }
  | tFID { $1 }
  | Op { $1 }
  | Reswords { $1 }

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
Arg: Lhs tEQL ArgRhs { mk_assign $1 $3 }
  | VarLhs tOP_ASGN ArgRhs { (mk_op_assign $1 $3) }
  | Primary tLBRACK2 OptCallArgs RBracket tOP_ASGN ArgRhs { (mk_op_assign (mk_index $1 $2 $3 $4) $6) }
  | Primary CallOp tIDENTIFIER tOP_ASGN ArgRhs { (mk_op_assign (mk_call_method $1 $2 $3 []) $5) }
  | Primary CallOp tCONSTANT tOP_ASGN ArgRhs { (mk_op_assign (mk_call_method $1 $2 $3 []) $5) }
  | Primary tCOLON2 tIDENTIFIER tOP_ASGN ArgRhs { (mk_op_assign (mk_call_method $1 $2 $3 []) $5) }
  | Primary tCOLON2 tCONSTANT tOP_ASGN ArgRhs { mk_op_assign (mk_const_op_assignable (mk_const_fetch $1 $3)) $5 }
  | tCOLON3 tCONSTANT tOP_ASGN ArgRhs { mk_op_assign (mk_const_op_assignable (mk_const_global $1 $2)) $4 }
  | Backref tOP_ASGN ArgRhs { mk_op_assign $1 $3 }
  | Arg tDOT2 Arg { (mk_range_inclusive $1 $3) }
  | Arg tDOT3 Arg { (mk_range_exclusive $1 $3) }
  | Arg tDOT2 { (mk_range_inclusive $1 Nil) }
  | Arg tDOT3 { (mk_range_exclusive $1 Nil) }
  | Arg tPLUS Arg { mk_binary_op $1 "+" $3 }
  | Arg tMINUS Arg { mk_binary_op $1 "-" $3 }
  | Arg tSTAR2 Arg { mk_binary_op $1 "**" $3 }
  | Arg tDIVIDE Arg { mk_binary_op $1 "/" $3 }
  | Arg tPERCENT Arg { mk_binary_op $1 "%" $3 }
  | Arg tPOW Arg { error "mk_binary_op $1 $2 $3" }
  | tUNARY_NUM SimpleNumeric tPOW Arg { error "mk_unary_op $1 (mk_binary_op $2 $3 $4)" }
  | tUPLUS Arg { (mk_unary_op "+@" $2) }
  | tUMINUS Arg { (mk_unary_op "-@" $2) }
  | Arg tPIPE Arg { mk_binary_op $1 "|" $3 }
  | Arg tCARET Arg { mk_binary_op $1 "^" $3 }
  | Arg tAMPER2 Arg { mk_binary_op $1 "&&" $3 }
  | Arg tCMP Arg { error "mk_binary_op $1 $2 $3" }
  | RelExpr %prec tCMP { $1 }
  | Arg tEQ Arg { mk_binary_op $1 "==" $3 }
  | Arg tEQQ Arg { mk_binary_op $1 "===" $3 }
  | Arg tNEQ Arg { mk_binary_op $1 "!=" $3 }
  | Arg tMATCH Arg { (mk_match_op $1 $2 $3) }
  | Arg tNMATCH Arg { error "mk_binary_op $1 $2 $3" }
  | tBANG Arg { (mk_not_op $2) }
  | tTILDE Arg { (mk_unary_op "~" $2) }
  | Arg tLSHFT Arg { mk_binary_op $1 "<<" $3 }
  | Arg tRSHFT Arg { mk_binary_op $1 ">>" $3 }
  | Arg tANDOP Arg { mkLogicalOp And $1 $3 }
  | Arg tOROP Arg { mkLogicalOp Or $1 $3 }
  | kDEFINED OptNl Arg { mk_keyword_cmd Defined [$3] }
  | Arg tEH Arg OptNl tCOLON Arg { mk_ternary $1 $3 $6 }
  | Primary { $1 }

Relop :: { String }
Relop: tGT { ">" }
  | tLT { "<" }
  | tGEQ { ">=" }
  | tLEQ { "<=" }

RelExpr :: { Term }
RelExpr: Arg Relop Arg %prec tGT { mk_binary_op $1 $2 $3 }
  | RelExpr Relop Arg %prec tGT { mk_binary_op $1 $2 $3 }

ArefArgs :: { [Term] }
ArefArgs: None { [] }
  | Args Trailer { $1 }
  | Args tCOMMA Assocs Trailer { $1 ++ [mk_associate $3] }
  | Assocs Trailer { [ (mk_associate $1) ] }

ArgRhs :: { Term }
ArgRhs: Arg %prec tOP_ASGN { $1 }
  | Arg kRESCUE_MOD Arg { mk_begin_body' $1 [mk_rescue_body Nil Nil $3] }

ParenArgs :: { [Term] }
ParenArgs: tLPAREN2 OptCallArgs Rparen { $2 }

OptParenArgs :: { [Term] }
OptParenArgs: {- nothing -} { [] }
  | ParenArgs { $1 }

OptCallArgs :: { [Term] }
OptCallArgs: {- nothing -} { [] }
  | CallArgs { $1 }
  | Args tCOMMA { $1 }
  | Args tCOMMA Assocs tCOMMA { $1 ++ [mk_associate $3] }
  | Assocs tCOMMA { [mk_associate $1] }

CallArgs :: { [Term] }
CallArgs: Command { [ $1 ] }
  | Args OptBlockArg { $1 ++ $2 }
  | Assocs OptBlockArg { [ mk_associate $1 ] ++ $2  }
  | Args tCOMMA Assocs OptBlockArg { $1 ++ [mk_associate $3] ++ $4 }
  | BlockArg { [ $1 ] }

CommandArgs :: { [Term] }
CommandArgs: CallArgs { $1 }

BlockArg :: { Term }
BlockArg: tAMPER Arg { mk_block_pass $2 }

OptBlockArg: tCOMMA BlockArg { [ $2 ] }
  | {- nothing -} { [] }

Args :: { [Term] }
Args: Arg { [ $1 ] }
  | tSTAR Arg { [mk_splat $2] }
  | Args tCOMMA Arg { $1 ++ [$3] }
  | Args tCOMMA tSTAR Arg { $1 ++ [mk_splat $4] }

MrhsArg :: { Term }
MrhsArg: Mrhs { mk_array $1 }
  | Arg { $1 }

Mrhs :: { [Term] }
Mrhs: Args tCOMMA Arg { $1 ++ [$3] }
  | Args tCOMMA tSTAR Arg { $1 ++ [mk_splat $4] }
  | tSTAR Arg { [mk_splat $2] }

Primary :: { Term }
Primary: Literal { $1 }
  | Strings { $1 }
  | Xstring { $1 }
  | Regexp { $1 }
  | Words { $1 }
  | Qwords { $1 }
  | Symbols { $1 }
  | Qsymbols { $1 }
  | VarRef { $1 }
  | Backref { $1 }
  | tFID { error "mk_call_method $1" }
  | kBEGIN Bodystmt kEND { mk_begin_keyword $2 }
  | tLPAREN_ARG Stmt Rparen { mk_begin $2 }
  | tLPAREN_ARG OptNl tRPAREN { mk_begin Nil }
  | tLPAREN Compstmt tRPAREN { mk_begin $2 }
  | Primary tCOLON2 tCONSTANT { mk_const_fetch $1 $3 }
  | tCOLON3 tCONSTANT { error "mk_const_global $1 $2" }
  | tLBRACK ArefArgs tRBRACK { mk_array $2 }
  | tLBRACE AssocList tRCURLY { mk_associate $2 }
  | KReturn { error "mkeyword_cmd Return $1" }
  | kYIELD tLPAREN2 CallArgs Rparen { mk_keyword_cmd Yield $3 }
  | kYIELD tLPAREN2 Rparen { mk_keyword_cmd Yield [] }
  | kYIELD { mk_keyword_cmd Yield [] }
  | kDEFINED OptNl tLPAREN2 Expr Rparen { mk_keyword_cmd Defined [$4] }
  | kNOT tLPAREN2 Expr Rparen { mk_not_op $3 }
  | kNOT tLPAREN2 Rparen { mk_not_op Nil }
  | Operation BraceBlock { error "let (begin_t, Args, body, end_t) = $2 in mk_block (mk_call_method Nil Nil $1) begin_t Args body end_t" }
  | MethodCall { $1 }
  | MethodCall BraceBlock { error "let (begin_t, Args, body, end_t) = $2 in (mk_block $1 begin_t Args body end_t)" }
  | tLAMBDA Lambda { error "let (args, (begin_t, body, end_t)) = $2 in (mk_block (mk_call_lambda $1) begin_t args body end_t)" }
  | kIF Expr Then Compstmt IfTail kEND { mk_condition $2 $4 $5 }
  | kUNLESS Expr Then Compstmt OptElse kEND { mk_condition $2 $5 $4 }
  | kWHILE ExprValueDo Compstmt kEND  { mk_loop While $2 $3 }
  | kUNTIL ExprValueDo Compstmt kEND  { mk_loop Until $2 $3 }
  | kCASE Expr OptTerms CaseBody kEND { mk_case $2 $4 }
  | kCASE OptTerms CaseBody kEND { mk_case Nil $3 }
  | kFOR ForVar kIN ExprValueDo Compstmt kEND  { mk_for $2 $4 $5 }
  | kCLASS Cpath Superclass Bodystmt kEND { error "let (lt_t, Superclass) = $3 in (mk_def_class $1 $2 lt_t Superclass $5 $6)" }
  | kCLASS tLSHFT Expr Term Bodystmt kEND { error "mk_def_sclass $3 $4 $5" }
  | kMODULE Cpath Bodystmt kEND { error "mk_def_module $2 $3" }
  | kDEF Fname FArglist Bodystmt kEND { mk_def_method $2 $3 $4 }
  | kDEF Singleton DotOrColon Fname FArglist Bodystmt kEND { mk_def_singleton $2 $4 $5 $6 }
  | kBREAK { mk_keyword_cmd Break [] }
  | kNEXT { mk_keyword_cmd Next [] }
  | kREDO { mk_keyword_cmd Redo [] }
  | kRETRY { mk_keyword_cmd Retry [] }

KReturn: kRETURN { error ":invalid_return, Nil, $1 if @context.in_class?"  }

Then :: { L.Token }
Then: Term { $1 }
  | kTHEN { $1 }
  | Term kTHEN { $2 }

Do :: { L.Token }
Do: Term { $1 }
  | kDO_COND { $1 }

IfTail :: { Term }
IfTail: OptElse { $1 }
  | kELSIF Expr Then Compstmt IfTail { mk_condition $2 $4 $5 }

ForVar :: { Term }
ForVar: Lhs { $1 }
  | Mlhs { $1 }

OptElse :: { Term }
OptElse: None { $1 }
  | kELSE Compstmt { $2 }

FMarg :: { Term }
FMarg: FNormArg { error "mk_arg $1" }
  | tLPAREN FMargs Rparen { mk_multi_lhs $2 }

FMargList :: { [Term] }
FMargList: FMarg { [ $1 ] }
  | FMargList tCOMMA FMarg { $1 ++ [$3] }

FMargs ::  { [Term] }
FMargs: FMargList { $1 }
  | FMargList tCOMMA tSTAR FNormArg { error "$1. push(mk_restarg $3 $4)" }
  | FMargList tCOMMA tSTAR FNormArg tCOMMA FMargList { error "$1. push(mk_restarg $3 $4). ++ $6" }
  | FMargList tCOMMA tSTAR { error "$1. push(mk_restarg($3))" }
  | FMargList tCOMMA tSTAR tCOMMA FMargList { error "$1. push(mk_restarg($3)). ++ $5" }
  | tSTAR FNormArg { error "[ (mk_restarg $1 $2) ]" }
  | tSTAR FNormArg tCOMMA FMargList { error "[ (mk_restarg $1 $2), *$4 ]" }
  | tSTAR { [ mk_restarg $1 ] }
  | tSTAR tCOMMA FMargList { [ mk_restarg $1 $3 ] }

BlockArgsTail :: { [Term] }
BlockArgsTail: FBlockKwarg tCOMMA FKwrest OptFBlockArg { $1 ++ $3 ++ $4 }
  | FBlockKwarg OptFBlockArg { $1 ++ $2 }
  | FKwrest OptFBlockArg { $1 ++ $2 }
  | FBlockArg { [ $1 ] }

OptBlockArgsTail: tCOMMA BlockArgsTail { $2 }
  | {- nothing -} { [] }

BlockParam :: { [Term] }
BlockParam: FArg tCOMMA FBlockOptarg tCOMMA FRestArg OptBlockArgsTail { error "$1 ++ $3 ++ $5 ++ $6" }
  | FArg tCOMMA FBlockOptarg tCOMMA FRestArg tCOMMA FArg OptBlockArgsTail { error "$1 ++ $3 ++ $5 ++ $7 ++ $8" }
  | FArg tCOMMA FBlockOptarg OptBlockArgsTail { error "$1 ++ $3 ++ $4" }
  | FArg tCOMMA FBlockOptarg tCOMMA FArg OptBlockArgsTail { error "$1 ++ $3 ++ $5 ++ $6" }
  | FArg tCOMMA FRestArg OptBlockArgsTail { error "$1 ++ $3 ++ $4" }
  | FArg tCOMMA { error "$1" }
  | FArg tCOMMA FRestArg tCOMMA FArg OptBlockArgsTail { error "$1 ++ $3 ++ $5 ++ $6" }
  | FArg OptBlockArgsTail { error "if null $2 && length $1 == 1 then [mk_procarg0 $1[0]] else $1 ++ $2" }
  | FBlockOptarg tCOMMA FRestArg OptBlockArgsTail { error "$1 ++ $3 ++ $4" }
  | FBlockOptarg tCOMMA FRestArg tCOMMA FArg OptBlockArgsTail { error "$1 ++ $3 ++ $5 ++ $6" }
  | FBlockOptarg OptBlockArgsTail { error "$1 ++ $2" }
  | FBlockOptarg tCOMMA FArg OptBlockArgsTail { error "$1 ++ $3 ++ $4" }
  | FRestArg OptBlockArgsTail { error "$1 ++ $2" }
  | FRestArg tCOMMA FArg OptBlockArgsTail { error "$1 ++ $3 ++ $4" }
  | BlockArgsTail { error "$1" }

OptBlockParam: {- nothing -} { error "mk_args Nil [] Nil" }
  | BlockParamDef { $1 }

BlockParamDef: tPIPE OptBvDecl tPIPE { error "mk_args $1 $2 $3" }
 | tOROP { (mk_args $1 [] $1) }
 | tPIPE BlockParam OptBvDecl tPIPE { error "mk_args $1 ($2 ++ $3) $4" }

OptBvDecl: OptNl { [] }
  | OptNl tSEMI BvDecls OptNl { $3 }

BvDecls: Bvar { [ $1 ] }
      | BvDecls tCOMMA Bvar { $1 ++ [$3] }

Bvar: tIDENTIFIER { error "mk_shadowarg $1" }
    | FBadArg { $1 }

Lambda: FLarglist LambdaBody { error "[ $2, $4 ]" }

FLarglist: tLPAREN2 FArgs OptBvDecl tRPAREN { error "mk_args $1 $2.concat($3) $4" }
  | FArgs { error "mk_args Nil $1 Nil" }

LambdaBody: tLAMBEG Compstmt tRCURLY { error "[ $1, $3, $4 ]" }
  | kDO_LAMBDA Bodystmt kEND { error "[ $1, $3, $4 ]" }

DoBlock: kDO_BLOCK  DoBody kEND { undefined } -- { @context.push(:block) } { [ $1, *$3, $4 ] @context.pop }
BlockCall: Command DoBlock { error "let (begin_t, block_args, body, end_t) = $2 in (mk_block $1 begin_t block_args body end_t)" }
  | BlockCall DotOrColon Operation2 OptParenArgs { undefined } {- let (lparen_t, Args, rparen_t) = $4 in mk_call_method $1 $2 $3 lparen_t Args rparen_t } -- | BlockCall DotOrColon Operation2 OptParenArgs BraceBlock { lparen_t, Args, rparen_t = $4
            MethodCall = mk_call_method $1, $2, $3,
                            lparen_t, Args, rparen_t)

            begin_t, Args, body, end_t = $5
            result      = mk_block(MethodCall,
                            begin_t, Args, body, end_t) -}
  | BlockCall DotOrColon Operation2 CommandArgs DoBlock { undefined } {-
            MethodCall = mk_call_method $1, $2, $3,
                            Nil, $4, Nil)

            begin_t, Args, body, end_t = $5
            result      = mk_block(MethodCall,
                            begin_t, Args, body, end_t) -}

MethodCall: Operation ParenArgs { error "let (lparen_t, Args, rparen_t) = $2 in mk_call_method Nil Nil $1 lparen_t Args rparen_t" }
  | Primary CallOp Operation2 OptParenArgs { error "let let (lparen_t, Args, rparen_t) = $4 in mk_call_method $1, $2, $3, lparen_t, Args, rparen_t" }
  | Primary tCOLON2 Operation2 ParenArgs { error "let (lparen_t, Args, rparen_t) = $4 in mk_call_method $1 $2 $3 lparen_t Args rparen_t" }
  | Primary tCOLON2 Operation3 { mk_call_method $1 $2 $3 [] }
  | Primary CallOp ParenArgs { error "let (lparen_t, Args, rparen_t) = $3 in mk_call_method $1, $2, Nil, lparen_t, Args, rparen_t" }
  | Primary tCOLON2 ParenArgs { error "let (lparen_t, Args, rparen_t) = $3 in mk_call_method $1, $2, Nil, lparen_t, Args, rparen_t" }
  | kSUPER ParenArgs { error "let (lparen_t, Args, rparen_t) = $2 in mk_keyword_cmd Super $1 lparen_t Args rparen_t" }
  | kSUPER { mk_keyword_cmd Zsuper [] }
  | Primary tLBRACK2 OptCallArgs RBracket { error "(mk_index $1, $2 $3 $4" }

BraceBlock: tLCURLY BraceBody tRCURLY { undefined }
  | kDO DoBody kEND  { undefined }

BraceBody: OptBlockParam Compstmt { undefined }

DoBody: OptBlockParam Bodystmt { undefined }-- { @static_env.extend_dynamic } { @lexer.cmdarg.push(false) } { result = [ val[2], val[3] ] @static_env.unextend @lexer.cmdarg.pop  }

CaseBody :: { [Term] }
CaseBody: kWHEN Args Then Compstmt Cases { [mk_when $2 $4] ++ $5 }

Cases :: { [Term] }
Cases: OptElse { [$1] }
 | CaseBody { $1 }

OptRescue :: { [Term] }
OptRescue: kRESCUE ExcList ExcVar Then Compstmt OptRescue { mk_rescue_body (mk_array $2) $3 $5 : $6 }
  | {- nothing -} { [] }

ExcList :: { [Term] }
ExcList: Arg { [$1] }
  | Mrhs { $1 }
  | None { [] }

ExcVar :: { Term }
ExcVar: tASSOC Lhs { $2 }
  | None { $1 }

OptEnsure :: { Term }
OptEnsure: kENSURE Compstmt { $2 }
  | None { $1 }

Literal :: { Term }
Literal: Numeric { $1 }
  | Symbol { $1 }
  | Dsym { $1 }

Strings :: { Term }
Strings: String { mk_string_compose $1 }

String :: { [Term] }
String: String1 { [ $1 ] }
  | String String1 { $1 ++ [$2] }

String1 :: { Term }
String1: tSTRING_BEG StringContents tSTRING_END { mk_string_compose $2 }
  | tSTRING { mk_string $1 }
  | tCHARACTER { mk_character $1 }

Xstring :: { Term }
Xstring: tXSTRING_BEG XStringContents tSTRING_END { error "mk_xstring_compose $2" }

Regexp: tREGEXP_BEG RegexpContents tSTRING_END tREGEXP_OPT { error "mk_regexp_compose $1 $2 $3 (mk_regexp_options $4)" }

Words :: { Term }
Words: tWORDS_BEG WordList tSTRING_END { error "mk_words_compose $1 $2 $3" }

WordList :: { [Term] }
WordList: {- nothing -} { [] }
  | WordList Word tSPACE { $1 ++ [mk_word $2] }

Word: StringContent { [ $1 ] }
  | Word StringContent { $1 ++ [$2] }

Symbols :: { Term }
Symbols: tSYMBOLS_BEG SymbolList tSTRING_END { error "mk_symbols_compose $1 $2 $3" }

SymbolList :: { [Term] }
SymbolList:  {- nothing -} { [] }
  | SymbolList Word tSPACE { $1 ++ [mk_word $2] }

Qwords :: { Term }
Qwords: tQWORDS_BEG QwordList tSTRING_END { error "mk_words_compose $1 $2 $3" }

Qsymbols :: { Term }
Qsymbols: tQSYMBOLS_BEG QsymList tSTRING_END { error "mk_symbols_compose $1 $2 $3" }

QwordList :: { [Term] }
QwordList: {- nothing -} { [] }
  | QwordList tSTRING_CONTENT tSPACE { $1 ++ [mk_string_internal $2 ] }

QsymList :: { [Term] }
QsymList: {- nothing -} { [] }
  | QsymList tSTRING_CONTENT tSPACE { $1 ++ [mk_symbol_internal $2] }

StringContents :: { [Term] }
StringContents: {- nothing -} { [] }
  | StringContents StringContent { $1 ++ [$2] }

XStringContents :: { [Term] }
XStringContents: {- nothing -} { [] }
  | XStringContents StringContent { $1 ++ [$2] }

RegexpContents :: { [Term] }
RegexpContents: {- nothing -} { [] }
  | RegexpContents StringContent { $1 ++ [$2] }

StringContent: tSTRING_CONTENT { mk_string_internal $1 }
  | tSTRING_DVAR StringDvar { $2 }
  | tSTRING_DBEG Compstmt tSTRING_DEND { error "mk_begin $1 $2 $3" } -- { @lexer.cmdarg.push(false); @lexer.cond.push(false); @lexer.cmdarg.pop @lexer.cond.pop

StringDvar :: { Term }
StringDvar: tGVAR { mk_gvar $1 }
  | tIVAR { mk_ivar $1 }
  | tCVAR { mk_cvar $1 }
  | Backref { $1 }

Symbol :: { Term }
Symbol: tSYMBOL { mk_symbol $1 }

Dsym: tSYMBEG XStringContents tSTRING_END { mk_symbol_compose $2 }

Numeric: SimpleNumeric { $1 }
  | tUNARY_NUM SimpleNumeric tLOWEST { error "mk_unary_num $1 $2" }

SimpleNumeric: tINTEGER { mk_integer $1 }
  | tFLOAT { mk_float $1 }
  | tRATIONAL { error "mk_rational $1" }
  | tIMAGINARY { error "mk_complex $1" }

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

VarRef :: { Term }
VarRef: UserVariable { mk_accessible $1 [] }
  | KeywordVariable { mk_accessible $1 [] }

VarLhs :: { Term }
VarLhs: UserVariable { mk_assignable $1 }
  | KeywordVariable { mk_assignable $1 }

Backref :: { Term }
Backref: tNTH_REF { mk_nth_ref $1 }
  | tBACK_REF { mk_back_ref $1 }

Superclass: tLT Expr Term { error "[ $1, $3 ]" }
  | {- nothing -} { Nil }


FArglist: tLPAREN2 FArgs Rparen { (mk_args $1 $2 $3) }
  | FArgs Term { error "mk_args Nil $2 Nil" }

ArgsTail: FKwarg tCOMMA FKwrest OptFBlockArg { error "$1.concat($3).concat($4)" }
  | FKwarg OptFBlockArg { error "$1.concat($2)" }
  | FKwrest OptFBlockArg { error "$1.concat($2)" }
  | FBlockArg { [ $1 ] }

OptArgsTail: tCOMMA ArgsTail { $2 }
  | {- nothing -} { [] }

FArgs :: { [Term] }
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
  | {- nothing -} { [] }


FBadArg: tCONSTANT { error ":argument_const, Nil, $1" }
  | tIVAR { error ":argument_ivar, Nil, $1" }
  | tGVAR { error ":argument_gvar, Nil, $1" }
  | tCVAR { error ":argument_cvar, Nil, $1" }

FNormArg: FBadArg { $1 }
  | tIDENTIFIER { $1 }

FArgAsgn: FNormArg { $1 }

FArgItem :: { Term }
FArgItem: FArgAsgn { error "mk_arg $1" }
  | tLPAREN FMargs Rparen { mk_multi_lhs $2 }

FArg: FArgItem { [ $1 ] }
  | FArg tCOMMA FArgItem { $1 ++ [$3] }

FLabel: tLABEL { undefined } -- { check_kwarg_name($1) @static_env.declare $1[0] $1 }

FKw: FLabel Arg { error "mk_kwoptarg $1 $2" }
  | FLabel { error "mk_kwarg $1" }

FBlockKw: FLabel Primary { error "mk_kwoptarg $1 $2" }
  | FLabel { error "mk_kwarg $1" }

FBlockKwarg: FBlockKw { [ $1 ] }
  | FBlockKwarg tCOMMA FBlockKw { $1 ++ [$3] }

FKwarg: FKw { [ $1 ] }
  | FKwarg tCOMMA FKw { $1 ++ [$3] }

KwrestMark: tPOW { $1 }
  | tDSTAR { $1 }

FKwrest: KwrestMark tIDENTIFIER {  [ mk_kwrestarg $1 $2 ] }
  | KwrestMark { [ mk_kwrestarg($1) ] }

FOpt: FArgAsgn tEQL Arg { error "mk_optarg $1 $2 $3" }

FBlockOpt: FArgAsgn tEQL Primary { error "mk_optarg $1 $2 $3" }

FBlockOptarg: FBlockOpt { [ $1 ] }
  | FBlockOptarg tCOMMA FBlockOpt { $1 ++ [$3] }

FOptarg :: { [Term] }
FOptarg: FOpt { [ $1 ] }
  | FOptarg tCOMMA FOpt { $1 ++ [$3] }

RestargMark :: { L.Token }
RestargMark: tSTAR2 { $1 }
  | tSTAR { $1 }

FRestArg :: { [Term] }
FRestArg: RestargMark tIDENTIFIER { [ mk_restarg $1 $2 ] }
  | RestargMark { [ mk_restarg $1 ] }

BlkargMark :: { L.Token }
BlkargMark: tAMPER2 { $1 }
  | tAMPER { $1 }

FBlockArg: BlkargMark tIDENTIFIER { error "mk_blockarg $1 $2" }

OptFBlockArg: tCOMMA FBlockArg { [ $2 ] }
  | {- nothing -} { [] }

Singleton :: { Term }
Singleton: VarRef { $1 }
  | tLPAREN2 Expr Rparen { $2 }

AssocList: {- nothing -} { [] }
  | Assocs Trailer { $1 }

Assocs :: { [Term] }
Assocs: Assoc { [ $1 ] }
  | Assocs tCOMMA Assoc { $1 ++ [$3] }

Assoc :: { Term }
Assoc: Arg tASSOC Arg { error "mk_pair $1 $2 $3" }
   | tLABEL Arg { error "mk_pair_keyword $1 $2" }
   | tSTRING_BEG StringContents tLABEL_END Arg { error "mk_pair_quoted $1 $2 $3 $4" }
   | tDSTAR Arg { mk_kwsplat $2 }

Operation :: { L.Token }
Operation: tIDENTIFIER { $1 }
  | tCONSTANT { $1 }
  | tFID { $1 }

Operation2 :: { L.Token }
Operation2: tIDENTIFIER { $1 }
  | tCONSTANT { $1 }
  | tFID { $1 }
  | Op { $1 }

Operation3 :: { L.Token }
Operation3: tIDENTIFIER { $1 }
  | tFID { $1 }
  | Op { $1 }

DotOrColon :: { L.Token }
DotOrColon: CallOp { $1 }
  | tCOLON2 { $1 }

CallOp :: { L.Token }
CallOp: tDOT { $1 }
  | tANDDOT { $1 }

OptTerms :: { L.Token }
OptTerms: { L.KNIL }
  | Terms { $1 }

OptNl :: { L.Token }
OptNl: { L.KNIL }
  | tNL { $1 }
 
Rparen :: { L.Token }
Rparen: OptNl tRPAREN { $2 }
 
RBracket :: { L.Token }
RBracket: OptNl tRBRACK { $2 }
 
Trailer :: { L.Token }
Trailer: {- nothing -} { L.KNIL }
  | tNL { $1 }
  | tCOMMA { $1 }
 
Term :: { L.Token }
Term: tSEMI { error "Term: tSEMI" }
  | tNL { $1 }
 
Terms :: { L.Token }
Terms: Term { $1 }
  | Terms tSEMI { $1 }

None :: { Term }
None: { Nil } -- { Nil }

{
parseError _ = throwError "!Parse Error"
}
