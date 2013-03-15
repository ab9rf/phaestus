{
module PHPParse ( phpParse ) where

import PHPLex (AlexState, Token(..), mLexer, P, lexError)
import ParseTree 

}

%name phpParse start

%lexer { mLexer } { EOF }
%monad { P }

%tokentype { Token }

%token INLINE_HTML      { InlineHTML $$ }

%token '(int)'          { CastInt }
%token '(double)'       { CastReal }
%token '(string)'       { CastString }
%token '(array)'        { CastArray }
%token '(object)'       { CastObject }
%token '(bool)'         { CastBool }
%token '(unset)'        { CastUnset }

%token '=='             { OpEqEq }
%token '==='            { OpEqEqEq }
%token '!='             { OpNotEq }
%token '!=='            { OpNotEqEq }
%token '<='             { OpLE }
%token '>='             { OpGE }
%token '++'             { OpInc }
%token '--'             { OpDec }
%token '=>'             { OpDoubleArrow }
%token '->'             { OpSingleArrow }
%token '<<'             { OpSL }
%token '>>'             { OpSR }
%token '+='             { OpPlusEq }
%token '-='             { OpMinusEq }
%token '*='             { OpMultEq }
%token '/='             { OpDivEq }
%token '.='             { OpConcatEq }
%token '%='             { OpModEq }
%token '&='             { OpAndEq }
%token '|='             { OpOrEq }
%token '^='             { OpXorEq }
%token '<<='            { OpSLEq }
%token '>>='            { OpSREq }
%token '::'             { OpColonColon }
%token '&&'             { OpLogicAnd }
%token '||'             { OpLogicOr }
%token '+'              { OpPlus }
%token '-'              { OpMinus }
%token '/'              { OpSlash }
%token '*'              { OpStar }
%token '%'              { OpPercent }
%token '^'              { OpCaret }
%token '&'              { OpAmpersand }
%token '|'              { OpPipe }
%token '~'              { OpTilde }
%token '='              { OpEq }
%token '<'              { OpLt }
%token '>'              { OpGt }
%token '.'              { OpDot }
%token '!'              { OpBang }
%token ','              { OpComma }
%token '?'              { OpQuestion }
%token ':'              { OpColon }
%token '@'              { OpAtSign }
%token '$'              { OpDollars }
%token '\\'             { Backslash }
%token '`'              { Backquote }
%token '"'              { DoubleQuote }

%token '${'             { DollarOpenCurlyBrace }

%token ';'              { Semicolon }
%token '('              { LParen }
%token ')'              { RParen }
%token '{'              { LBrace }
%token '}'              { RBrace }
%token '['              { LBracket }
%token ']'              { RBracket }

%token LT_HEREDOC_END   { EndHeredoc }
%token LT_HEREDOC_START { StartHeredoc }

%token 'and'            { KeywordAnd }
%token 'or'             { KeywordOr }
%token 'xor'            { KeywordXor }
%token '__FILE__'       { Keyword__FILE__ }
%token '__LINE__'       { Keyword__LINE__ }
%token '__DIR__'        { Keyword__DIR__ }
%token 'array'          { KeywordArray }
%token 'as'             { KeywordAs }
%token 'break'          { KeywordBreak }
%token 'case'           { KeywordCase }
%token 'class'          { KeywordClass }
%token 'const'          { KeywordConst }
%token 'continue'       { KeywordContinue }
%token 'declare'        { KeywordDeclare }
%token 'default'        { KeywordDefault }
%token 'do'             { KeywordDo }
%token 'echo'           { KeywordEcho }
%token 'else'           { KeywordElse }
%token 'elseif'         { KeywordElseif }
%token 'empty'          { KeywordEmpty }
%token 'enddeclare'     { KeywordEnddeclare }
%token 'endfor'         { KeywordEndfor }
%token 'endforeach'     { KeywordEndforeach }
%token 'endif'          { KeywordEndif }
%token 'endswitch'      { KeywordEndswitch }
%token 'endwhile'       { KeywordEndwhile }
%token 'eval'           { KeywordEval }
%token 'exit'           { KeywordExit }
%token 'extends'        { KeywordExtends }
%token 'for'            { KeywordFor }
%token 'foreach'        { KeywordForeach }
%token 'function'       { KeywordFunction }
%token 'global'         { KeywordGlobal }
%token 'if'             { KeywordIf }
%token 'include'        { KeywordInclude }
%token 'include_once'   { KeywordIncludeOnce }
%token 'instanceof'     { KeywordInstanceOf }
%token 'isset'          { KeywordIsset }
%token 'list'           { KeywordList }
%token 'new'            { KeywordNew }
%token 'print'          { KeywordPrint }
%token 'require'        { KeywordRequire }
%token 'require_once'   { KeywordRequireOnce }
%token 'return'         { KeywordReturn }
%token 'static'         { KeywordStatic }
%token 'switch'         { KeywordSwitch }
%token 'unset'          { KeywordUnset }
%token 'use'            { KeywordUse }
%token 'var'            { KeywordVar }
%token 'while'          { KeywordWhile }
%token '__FUNCTION__'   { Keyword__FUNCTION__ }
%token '__CLASS__'      { Keyword__CLASS__ }
%token '__METHOD__'     { Keyword__METHOD__ }
%token 'final'          { KeywordFinal }
%token 'interface'      { KeywordInterface }
%token 'implements'     { KeywordImplements }
%token 'public'         { KeywordPublic }
%token 'private'        { KeywordPrivate }
%token 'protected'      { KeywordProtected }
%token 'abstract'       { KeywordAbstract }
%token 'clone'          { KeywordClone }
%token 'try'            { KeywordTry }
%token 'catch'          { KeywordCatch }
%token 'throw'          { KeywordThrow }
%token 'namespace'      { KeywordNamespace }
%token 'goto'           { KeywordGoto }
%token 'finally'        { KeywordFinally }
%token 'trait'          { KeywordTrait }
%token 'callable'       { KeywordCallable }
%token 'insteadof'      { KeywordInsteadof }
%token 'yield'          { KeywordYield }
%token '__TRAIT__'      { Keyword__TRAIT__ }
%token '__NAMESPACE__'  { Keyword__NAMESPACE__ }

%token LT_VARNAME       { VariableToken $$ }
%token LT_IDENT         { IdentToken $$ }
%token LT_VARNAME_IMBED { VariableTokenInStr $$ }

%token LT_INTEGER       { IntegerToken $$ }
%token LT_DOUBLE        { RealToken $$ }
%token LT_STRING        { StringToken $$ }

%left 'include' 'include_once' 'eval' 'require' 'require_once'
%left ','
%left 'or'
%left 'xor'
%left 'and'
%right 'print'
%right 'yield'
%left '=' '+=' '-=' '*=' '/=' '.=' '%=' '&=' '|=' '^=' '<<=' '>>='
%left '?' ':'
%left '||'
%left '&&' 
%left '|'
%left '^'
%left '&'
%nonassoc '==' '!=' '===' '!=='
%nonassoc '<' '<=' '>' '>='
%left '<<' '>>'
%left '+' '-' '.'
%left '*' '/' '%'
%right '!'
%nonassoc 'instanceof'
%right '~' '++' '--' '(int)' '(double)' '(string)' '(array)' '(object)' '(bool)' '(unset)' '@'
%right '['
%nonassoc 'new' 'clone'
%left 'elseif'
%left 'else' 
%left 'endif' 
%right 'static' 'abstract' 'final' 'private' 'protected' 'public'

%expect 3

%%


start :: { PTStart }
   :  top_statement_list
      { PTStart_1 $1 }

top_statement_list :: { PTTopStatementList }
   :  top_statement_list top_statement
      { PTTopStatementList_1 $1 $2 }
   |  {- empty -}
      { PTTopStatementList_2 }

namespace_name :: { PTNamespaceName }
   :  LT_IDENT
      { PTNamespaceName_1 (PVIdent $1) }
   |  namespace_name '\\' LT_IDENT
      { PTNamespaceName_2 $1 (PVIdent $3) }

top_statement :: { PTTopStatement }
   :  statement
      { PTTopStatement_1 $1 }
   |  function_declaration_statement
      { PTTopStatement_2 $1 }
   |  class_declaration_statement
      { PTTopStatement_3 $1 }
   |  'namespace' namespace_name ';'
      { PTTopStatement_4 $2 }
   |  'namespace' namespace_name '{' top_statement_list '}'
      { PTTopStatement_5 $2 $4 }
   |  'namespace' '{' top_statement_list '}'
      { PTTopStatement_6 $3 }
   |  'use' use_declarations ';'
      { PTTopStatement_7 $2 }
   |  constant_declaration ';'
      { PTTopStatement_8 $1 }

use_declarations :: { PTUseDeclarations }
   :  use_declarations ',' use_declaration
      { PTUseDeclarations_1 $1 $3 }
   |  use_declaration
      { PTUseDeclarations_2 $1 }

use_declaration :: { PTUseDeclaration }
   :  namespace_name
      { PTUseDeclaration_1 $1 }
   |  namespace_name 'as' LT_IDENT
      { PTUseDeclaration_2 $1 (PVIdent $3) }
   |  '\\' namespace_name
      { PTUseDeclaration_3 $2 }
   |  '\\' namespace_name 'as' LT_IDENT
      { PTUseDeclaration_4 $2 (PVIdent $4) }

constant_declaration :: { PTConstantDeclaration }
   :  constant_declaration ',' LT_IDENT '=' static_scalar
      { PTConstantDeclaration_1 $1 (PVIdent $3) $5 }
   |  'const' LT_IDENT '=' static_scalar
      { PTConstantDeclaration_2 (PVIdent $2) $4 }

inner_statement_list :: { PTInnerStatementList }
   :  inner_statement_list inner_statement
      { PTInnerStatementList_1 $1 $2 }
   |  {- empty -}
      { PTInnerStatementList_2 }

inner_statement :: { PTInnerStatement }
   :  statement
      { PTInnerStatement_1 $1 }
   |  function_declaration_statement
      { PTInnerStatement_2 $1 }
   |  class_declaration_statement
      { PTInnerStatement_3 $1 }

statement :: { PTStatement }
   :  unticked_statement
      { PTStatement_1 $1 }
   |  LT_IDENT ':'
      { PTStatement_2 (PVIdent $1) }

unticked_statement :: { PTUntickedStatement }
   :  '{' inner_statement_list '}'
      { PTUntickedStatement_1 $2 }
   |  'if' parenthesis_expr statement elseif_list else_single
      { PTUntickedStatement_2 $2 $3 $4 $5 }
   |  'if' parenthesis_expr ':' inner_statement_list new_elseif_list new_else_single 'endif' ';'
      { PTUntickedStatement_3 $2 $4 $5 $6 }
   |  'while' parenthesis_expr while_statement
      { PTUntickedStatement_4 $2 $3 }
   |  'do' statement 'while' parenthesis_expr ';'
      { PTUntickedStatement_5 $2 $4 }
   |  'for' '(' for_expr ';' for_expr ';' for_expr ')' for_statement
      { PTUntickedStatement_6 $3 $5 $7 $9 }
   |  'switch' parenthesis_expr switch_case_list
      { PTUntickedStatement_7 $2 $3 }
   |  'break' ';'
      { PTUntickedStatement_8 }
   |  'break' expr ';'
      { PTUntickedStatement_9 $2 }
   |  'continue' ';'
      { PTUntickedStatement_10 }
   |  'continue' expr ';'
      { PTUntickedStatement_11 $2 }
   |  'return' ';'
      { PTUntickedStatement_12 }
   |  'return' expr_without_variable ';'
      { PTUntickedStatement_13 $2 }
   |  'return' variable ';'
      { PTUntickedStatement_14 $2 }
   |  yield_expr ';'
      { PTUntickedStatement_15 $1 }
   |  'global' global_var_list ';'
      { PTUntickedStatement_16 $2 }
   |  'static' static_var_list ';'
      { PTUntickedStatement_17 $2 }
   |  'echo' echo_expr_list ';'
      { PTUntickedStatement_18 $2 }
   |  INLINE_HTML
      { PTUntickedStatement_19 (PVInline $1) }
   |  expr ';'
      { PTUntickedStatement_20 $1 }
   |  'unset' '(' unset_variables ')' ';'
      { PTUntickedStatement_21 $3 }
   |  'foreach' '(' variable 'as' foreach_variable foreach_optional_arg ')' foreach_statement
      { PTUntickedStatement_22 $3 $5 $6 $8 }
   |  'foreach' '(' expr_without_variable 'as' foreach_variable foreach_optional_arg ')' foreach_statement
      { PTUntickedStatement_23 $3 $5 $6 $8 }
   |  'declare' '(' declare_list ')' declare_statement
      { PTUntickedStatement_24 $3 $5 }
   |  ';'
      { PTUntickedStatement_25 }
   |  'try' '{' inner_statement_list '}' catch_statement finally_statement
      { PTUntickedStatement_26 $3 $5 $6 }
   |  'throw' expr ';'
      { PTUntickedStatement_27 $2 }
   |  'goto' LT_IDENT ';'
      { PTUntickedStatement_28 (PVIdent $2) }

catch_statement :: { PTCatchStatement }
   :  {- empty -}
      { PTCatchStatement_1 }
   |  'catch' '(' fully_qualified_class_name LT_VARNAME ')' '{' inner_statement_list '}' additional_catches
      { PTCatchStatement_2 $3 (PVVariableName $4) $7 $9 }

finally_statement :: { PTFinallyStatement }
   :  {- empty -}
      { PTFinallyStatement_1 }
   |  'finally' '{' inner_statement_list '}'
      { PTFinallyStatement_2 $3 }

additional_catches :: { PTAdditionalCatches }
   :  non_empty_additional_catches
      { PTAdditionalCatches_1 $1 }
   |  {- empty -}
      { PTAdditionalCatches_2 }

non_empty_additional_catches :: { PTNonEmptyAdditionalCatches }
   :  additional_catch
      { PTNonEmptyAdditionalCatches_1 $1 }
   |  non_empty_additional_catches additional_catch
      { PTNonEmptyAdditionalCatches_2 $1 $2 }

additional_catch :: { PTAdditionalCatch }
   :  'catch' '(' fully_qualified_class_name LT_VARNAME ')' '{' inner_statement_list '}'
      { PTAdditionalCatch_1 $3 (PVVariableName $4) $7 }

unset_variables :: { PTUnsetVariables }
   :  unset_variable
      { PTUnsetVariables_1 $1 }
   |  unset_variables ',' unset_variable
      { PTUnsetVariables_2 $1 $3 }

unset_variable :: { PTUnsetVariable }
   :  variable
      { PTUnsetVariable_1 $1 }

function_declaration_statement :: { PTFunctionDeclarationStatement }
   :  unticked_function_declaration_statement
      { PTFunctionDeclarationStatement_1 $1 }

class_declaration_statement :: { PTClassDeclarationStatement }
   :  unticked_class_declaration_statement
      { PTClassDeclarationStatement_1 $1 }

is_reference :: { PTIsReference }
   :  {- empty -}
      { PTIsReference_1 }
   |  '&'
      { PTIsReference_2 }

unticked_function_declaration_statement :: { PTUntickedFunctionDeclarationStatement }
   :  function is_reference LT_IDENT '(' parameter_list ')' '{' inner_statement_list '}'
      { PTUntickedFunctionDeclarationStatement_1 $1 $2 (PVIdent $3) $5 $8 }

unticked_class_declaration_statement :: { PTUntickedClassDeclarationStatement }
   :  class_entry_type LT_IDENT extends_from implements_list '{' class_statement_list '}'
      { PTUntickedClassDeclarationStatement_1 $1 (PVIdent $2) $3 $4 $6 }
   |  interface_entry LT_IDENT interface_extends_list '{' class_statement_list '}'
      { PTUntickedClassDeclarationStatement_2 $1 (PVIdent $2) $3 $5 }

class_entry_type :: { PTClassEntryType }
   :  'class'
      { PTClassEntryType_1 }
   |  'abstract' 'class'
      { PTClassEntryType_2 }
   |  'trait'
      { PTClassEntryType_3 }
   |  'final' 'class'
      { PTClassEntryType_4 }

extends_from :: { PTExtendsFrom }
   :  {- empty -}
      { PTExtendsFrom_1 }
   |  'extends' fully_qualified_class_name
      { PTExtendsFrom_2 $2 }

interface_entry :: { PTInterfaceEntry }
   :  'interface'
      { PTInterfaceEntry_1 }

interface_extends_list :: { PTInterfaceExtendsList }
   :  {- empty -}
      { PTInterfaceExtendsList_1 }
   |  'extends' interface_list
      { PTInterfaceExtendsList_2 $2 }

implements_list :: { PTImplementsList }
   :  {- empty -}
      { PTImplementsList_1 }
   |  'implements' interface_list
      { PTImplementsList_2 $2 }

interface_list :: { PTInterfaceList }
   :  fully_qualified_class_name
      { PTInterfaceList_1 $1 }
   |  interface_list ',' fully_qualified_class_name
      { PTInterfaceList_2 $1 $3 }

foreach_optional_arg :: { PTForeachOptionalArg }
   :  {- empty -}
      { PTForeachOptionalArg_1 }
   |  '=>' foreach_variable
      { PTForeachOptionalArg_2 $2 }

foreach_variable :: { PTForeachVariable }
   :  variable
      { PTForeachVariable_1 $1 }
   |  '&' variable
      { PTForeachVariable_2 $2 }
   |  'list' '(' assignment_list ')'
      { PTForeachVariable_3 $3 }

for_statement :: { PTForStatement }
   :  statement
      { PTForStatement_1 $1 }
   |  ':' inner_statement_list 'endfor' ';'
      { PTForStatement_2 $2 }

foreach_statement :: { PTForeachStatement }
   :  statement
      { PTForeachStatement_1 $1 }
   |  ':' inner_statement_list 'endforeach' ';'
      { PTForeachStatement_2 $2 }

declare_statement :: { PTDeclareStatement }
   :  statement
      { PTDeclareStatement_1 $1 }
   |  ':' inner_statement_list 'enddeclare' ';'
      { PTDeclareStatement_2 $2 }

declare_list :: { PTDeclareList }
   :  LT_IDENT '=' static_scalar
      { PTDeclareList_1 (PVIdent $1) $3 }
   |  declare_list ',' LT_IDENT '=' static_scalar
      { PTDeclareList_2 $1 (PVIdent $3) $5 }

switch_case_list :: { PTSwitchCaseList }
   :  '{' case_list '}'
      { PTSwitchCaseList_1 $2 }
   |  '{' ';' case_list '}'
      { PTSwitchCaseList_2 $3 }
   |  ':' case_list 'endswitch' ';'
      { PTSwitchCaseList_3 $2 }
   |  ':' ';' case_list 'endswitch' ';'
      { PTSwitchCaseList_4 $3 }

case_list :: { PTCaseList }
   :  {- empty -}
      { PTCaseList_1 }
   |  case_list 'case' expr case_separator inner_statement_list
      { PTCaseList_2 $1 $3 $4 $5 }
   |  case_list 'default' case_separator inner_statement_list
      { PTCaseList_3 $1 $3 $4 }

case_separator :: { PTCaseSeparator }
   :  ':'
      { PTCaseSeparator_1 }
   |  ';'
      { PTCaseSeparator_2 }

while_statement :: { PTWhileStatement }
   :  statement
      { PTWhileStatement_1 $1 }
   |  ':' inner_statement_list 'endwhile' ';'
      { PTWhileStatement_2 $2 }

elseif_list :: { PTElseifList }
   :  {- empty -}
      { PTElseifList_1 }
   |  elseif_list 'elseif' parenthesis_expr statement
      { PTElseifList_2 $1 $3 $4 }

new_elseif_list :: { PTNewElseifList }
   :  {- empty -}
      { PTNewElseifList_1 }
   |  new_elseif_list 'elseif' parenthesis_expr ':' inner_statement_list
      { PTNewElseifList_2 $1 $3 $5 }

else_single :: { PTElseSingle }
   :  {- empty -}
      { PTElseSingle_1 }
   |  'else' statement
      { PTElseSingle_2 $2 }

new_else_single :: { PTNewElseSingle }
   :  {- empty -}
      { PTNewElseSingle_1 }
   |  'else' ':' inner_statement_list
      { PTNewElseSingle_2 $3 }

parameter_list :: { PTParameterList }
   :  non_empty_parameter_list
      { PTParameterList_1 $1 }
   |  {- empty -}
      { PTParameterList_2 }

non_empty_parameter_list :: { PTNonEmptyParameterList }
   :  optional_class_type LT_VARNAME
      { PTNonEmptyParameterList_1 $1 (PVVariableName $2) }
   |  optional_class_type '&' LT_VARNAME
      { PTNonEmptyParameterList_2 $1 (PVVariableName $3) }
   |  optional_class_type '&' LT_VARNAME '=' static_scalar
      { PTNonEmptyParameterList_3 $1 (PVVariableName $3) $5 }
   |  optional_class_type LT_VARNAME '=' static_scalar
      { PTNonEmptyParameterList_4 $1 (PVVariableName $2) $4 }
   |  non_empty_parameter_list ',' optional_class_type LT_VARNAME
      { PTNonEmptyParameterList_5 $1 $3 (PVVariableName $4) }
   |  non_empty_parameter_list ',' optional_class_type '&' LT_VARNAME
      { PTNonEmptyParameterList_6 $1 $3 (PVVariableName $5) }
   |  non_empty_parameter_list ',' optional_class_type '&' LT_VARNAME '=' static_scalar
      { PTNonEmptyParameterList_7 $1 $3 (PVVariableName $5) $7 }
   |  non_empty_parameter_list ',' optional_class_type LT_VARNAME '=' static_scalar
      { PTNonEmptyParameterList_8 $1 $3 (PVVariableName $4) $6 }

optional_class_type :: { PTOptionalClassType }
   :  {- empty -}
      { PTOptionalClassType_1 }
   |  'array'
      { PTOptionalClassType_2 }
   |  'callable'
      { PTOptionalClassType_3 }
   |  fully_qualified_class_name
      { PTOptionalClassType_4 $1 }

function_call_parameter_list :: { PTFunctionCallParameterList }
   :  '(' ')'
      { PTFunctionCallParameterList_1 }
   |  '(' non_empty_function_call_parameter_list ')'
      { PTFunctionCallParameterList_2 $2 }
   |  '(' yield_expr ')'
      { PTFunctionCallParameterList_3 $2 }

non_empty_function_call_parameter_list :: { PTNonEmptyFunctionCallParameterList }
   :  expr_without_variable
      { PTNonEmptyFunctionCallParameterList_1 $1 }
   |  variable
      { PTNonEmptyFunctionCallParameterList_2 $1 }
   |  '&' w_variable
      { PTNonEmptyFunctionCallParameterList_3 $2 }
   |  non_empty_function_call_parameter_list ',' expr_without_variable
      { PTNonEmptyFunctionCallParameterList_4 $1 $3 }
   |  non_empty_function_call_parameter_list ',' variable
      { PTNonEmptyFunctionCallParameterList_5 $1 $3 }
   |  non_empty_function_call_parameter_list ',' '&' w_variable
      { PTNonEmptyFunctionCallParameterList_6 $1 $4 }

global_var_list :: { PTGlobalVarList }
   :  global_var_list ',' global_var
      { PTGlobalVarList_1 $1 $3 }
   |  global_var
      { PTGlobalVarList_2 $1 }

global_var :: { PTGlobalVar }
   :  LT_VARNAME
      { PTGlobalVar_1 (PVVariableName $1) }
   |  '$' r_variable
      { PTGlobalVar_2 $2 }
   |  '$' '{' expr '}'
      { PTGlobalVar_3 $3 }

static_var_list :: { PTStaticVarList }
   :  static_var_list ',' LT_VARNAME
      { PTStaticVarList_1 $1 (PVVariableName $3) }
   |  static_var_list ',' LT_VARNAME '=' static_scalar
      { PTStaticVarList_2 $1 (PVVariableName $3) $5 }
   |  LT_VARNAME
      { PTStaticVarList_3 (PVVariableName $1) }
   |  LT_VARNAME '=' static_scalar
      { PTStaticVarList_4 (PVVariableName $1) $3 }

class_statement_list :: { PTClassStatementList }
   :  class_statement_list class_statement
      { PTClassStatementList_1 $1 $2 }
   |  {- empty -}
      { PTClassStatementList_2 }

class_statement :: { PTClassStatement }
   :  variable_modifiers class_variable_declaration ';'
      { PTClassStatement_1 $1 $2 }
   |  class_constant_declaration ';'
      { PTClassStatement_2 $1 }
   |  trait_use_statement
      { PTClassStatement_3 $1 }
   |  method_modifiers function is_reference LT_IDENT '(' parameter_list ')' method_body
      { PTClassStatement_4 $1 $2 $3 (PVIdent $4) $6 $8 }

trait_use_statement :: { PTTraitUseStatement }
   :  'use' trait_list trait_adaptations
      { PTTraitUseStatement_1 $2 $3 }

trait_list :: { PTTraitList }
   :  fully_qualified_class_name
      { PTTraitList_1 $1 }
   |  trait_list ',' fully_qualified_class_name
      { PTTraitList_2 $1 $3 }

trait_adaptations :: { PTTraitAdaptations }
   :  ';'
      { PTTraitAdaptations_1 }
   |  '{' trait_adaptation_list '}'
      { PTTraitAdaptations_2 $2 }

trait_adaptation_list :: { PTTraitAdaptationList }
   :  {- empty -}
      { PTTraitAdaptationList_1 }
   |  non_empty_trait_adaptation_list
      { PTTraitAdaptationList_2 $1 }

non_empty_trait_adaptation_list :: { PTNonEmptyTraitAdaptationList }
   :  trait_adaptation_statement
      { PTNonEmptyTraitAdaptationList_1 $1 }
   |  non_empty_trait_adaptation_list trait_adaptation_statement
      { PTNonEmptyTraitAdaptationList_2 $1 $2 }

trait_adaptation_statement :: { PTTraitAdaptationStatement }
   :  trait_precedence ';'
      { PTTraitAdaptationStatement_1 $1 }
   |  trait_alias ';'
      { PTTraitAdaptationStatement_2 $1 }

trait_precedence :: { PTTraitPrecedence }
   :  trait_method_reference_fully_qualified 'insteadof' trait_reference_list
      { PTTraitPrecedence_1 $1 $3 }

trait_reference_list :: { PTTraitReferenceList }
   :  fully_qualified_class_name
      { PTTraitReferenceList_1 $1 }
   |  trait_reference_list ',' fully_qualified_class_name
      { PTTraitReferenceList_2 $1 $3 }

trait_method_reference :: { PTTraitMethodReference }
   :  LT_IDENT
      { PTTraitMethodReference_1 (PVIdent $1) }
   |  trait_method_reference_fully_qualified
      { PTTraitMethodReference_2 $1 }

trait_method_reference_fully_qualified :: { PTTraitMethodReferenceFullyQualified }
   :  fully_qualified_class_name '::' LT_IDENT
      { PTTraitMethodReferenceFullyQualified_1 $1 (PVIdent $3) }

trait_alias :: { PTTraitAlias }
   :  trait_method_reference 'as' trait_modifiers LT_IDENT
      { PTTraitAlias_1 $1 $3 (PVIdent $4) }
   |  trait_method_reference 'as' member_modifier
      { PTTraitAlias_2 $1 $3 }

trait_modifiers :: { PTTraitModifiers }
   :  {- empty -}
      { PTTraitModifiers_1 }
   |  member_modifier
      { PTTraitModifiers_2 $1 }

method_body :: { PTMethodBody }
   :  ';'
      { PTMethodBody_1 }
   |  '{' inner_statement_list '}'
      { PTMethodBody_2 $2 }

variable_modifiers :: { PTVariableModifiers }
   :  non_empty_member_modifiers
      { PTVariableModifiers_1 $1 }
   |  'var'
      { PTVariableModifiers_2 }

method_modifiers :: { PTMethodModifiers }
   :  {- empty -}
      { PTMethodModifiers_1 }
   |  non_empty_member_modifiers
      { PTMethodModifiers_2 $1 }

non_empty_member_modifiers :: { PTNonEmptyMemberModifiers }
   :  member_modifier
      { PTNonEmptyMemberModifiers_1 $1 }
   |  non_empty_member_modifiers member_modifier
      { PTNonEmptyMemberModifiers_2 $1 $2 }

member_modifier :: { PTMemberModifier }
   :  'public'
      { PTMemberModifier_1 }
   |  'protected'
      { PTMemberModifier_2 }
   |  'private'
      { PTMemberModifier_3 }
   |  'static'
      { PTMemberModifier_4 }
   |  'abstract'
      { PTMemberModifier_5 }
   |  'final'
      { PTMemberModifier_6 }

class_variable_declaration :: { PTClassVariableDeclaration }
   :  class_variable_declaration ',' LT_VARNAME
      { PTClassVariableDeclaration_1 $1 (PVVariableName $3) }
   |  class_variable_declaration ',' LT_VARNAME '=' static_scalar
      { PTClassVariableDeclaration_2 $1 (PVVariableName $3) $5 }
   |  LT_VARNAME
      { PTClassVariableDeclaration_3 (PVVariableName $1) }
   |  LT_VARNAME '=' static_scalar
      { PTClassVariableDeclaration_4 (PVVariableName $1) $3 }

class_constant_declaration :: { PTClassConstantDeclaration }
   :  class_constant_declaration ',' LT_IDENT '=' static_scalar
      { PTClassConstantDeclaration_1 $1 (PVIdent $3) $5 }
   |  'const' LT_IDENT '=' static_scalar
      { PTClassConstantDeclaration_2 (PVIdent $2) $4 }

echo_expr_list :: { PTEchoExprList }
   :  echo_expr_list ',' expr
      { PTEchoExprList_1 $1 $3 }
   |  expr
      { PTEchoExprList_2 $1 }

for_expr :: { PTForExpr }
   :  {- empty -}
      { PTForExpr_1 }
   |  non_empty_for_expr
      { PTForExpr_2 $1 }

non_empty_for_expr :: { PTNonEmptyForExpr }
   :  non_empty_for_expr ',' expr
      { PTNonEmptyForExpr_1 $1 $3 }
   |  expr
      { PTNonEmptyForExpr_2 $1 }

chaining_method_or_property :: { PTChainingMethodOrProperty }
   :  chaining_method_or_property variable_property
      { PTChainingMethodOrProperty_1 $1 $2 }
   |  variable_property
      { PTChainingMethodOrProperty_2 $1 }

chaining_dereference :: { PTChainingDereference }
   :  chaining_dereference '[' dim_offset ']'
      { PTChainingDereference_1 $1 $3 }
   |  '[' dim_offset ']'
      { PTChainingDereference_2 $2 }

chaining_instance_call :: { PTChainingInstanceCall }
   :  chaining_dereference chaining_method_or_property
      { PTChainingInstanceCall_1 $1 $2 }
   |  chaining_dereference
      { PTChainingInstanceCall_2 $1 }
   |  chaining_method_or_property
      { PTChainingInstanceCall_3 $1 }

instance_call :: { PTInstanceCall }
   :  {- empty -}
      { PTInstanceCall_1 }
   |  chaining_instance_call
      { PTInstanceCall_2 $1 }

new_expr :: { PTNewExpr }
   :  'new' class_name_reference ctor_arguments
      { PTNewExpr_1 $2 $3 }

expr_without_variable :: { PTExprWithoutVariable }
   :  'list' '(' assignment_list ')' '=' expr
      { PTExprWithoutVariable_1 $3 $6 }
   |  variable '=' expr
      { PTExprWithoutVariable_2 $1 $3 }
   |  variable '=' '&' variable
      { PTExprWithoutVariable_3 $1 $4 }
   |  variable '=' '&' 'new' class_name_reference ctor_arguments
      { PTExprWithoutVariable_4 $1 $5 $6 }
   |  'clone' expr
      { PTExprWithoutVariable_5 $2 }
   |  variable '+=' expr
      { PTExprWithoutVariable_6 $1 $3 }
   |  variable '-=' expr
      { PTExprWithoutVariable_7 $1 $3 }
   |  variable '*=' expr
      { PTExprWithoutVariable_8 $1 $3 }
   |  variable '/=' expr
      { PTExprWithoutVariable_9 $1 $3 }
   |  variable '.=' expr
      { PTExprWithoutVariable_10 $1 $3 }
   |  variable '%=' expr
      { PTExprWithoutVariable_11 $1 $3 }
   |  variable '&=' expr
      { PTExprWithoutVariable_12 $1 $3 }
   |  variable '|=' expr
      { PTExprWithoutVariable_13 $1 $3 }
   |  variable '^=' expr
      { PTExprWithoutVariable_14 $1 $3 }
   |  variable '<<=' expr
      { PTExprWithoutVariable_15 $1 $3 }
   |  variable '>>=' expr
      { PTExprWithoutVariable_16 $1 $3 }
   |  rw_variable '++'
      { PTExprWithoutVariable_17 $1 }
   |  '++' rw_variable
      { PTExprWithoutVariable_18 $2 }
   |  rw_variable '--'
      { PTExprWithoutVariable_19 $1 }
   |  '--' rw_variable
      { PTExprWithoutVariable_20 $2 }
   |  expr '||' expr
      { PTExprWithoutVariable_21 $1 $3 }
   |  expr '&&' expr
      { PTExprWithoutVariable_22 $1 $3 }
   |  expr 'or' expr
      { PTExprWithoutVariable_23 $1 $3 }
   |  expr 'and' expr
      { PTExprWithoutVariable_24 $1 $3 }
   |  expr 'xor' expr
      { PTExprWithoutVariable_25 $1 $3 }
   |  expr '|' expr
      { PTExprWithoutVariable_26 $1 $3 }
   |  expr '&' expr
      { PTExprWithoutVariable_27 $1 $3 }
   |  expr '^' expr
      { PTExprWithoutVariable_28 $1 $3 }
   |  expr '.' expr
      { PTExprWithoutVariable_29 $1 $3 }
   |  expr '+' expr
      { PTExprWithoutVariable_30 $1 $3 }
   |  expr '-' expr
      { PTExprWithoutVariable_31 $1 $3 }
   |  expr '*' expr
      { PTExprWithoutVariable_32 $1 $3 }
   |  expr '/' expr
      { PTExprWithoutVariable_33 $1 $3 }
   |  expr '%' expr
      { PTExprWithoutVariable_34 $1 $3 }
   |  expr '<<' expr
      { PTExprWithoutVariable_35 $1 $3 }
   |  expr '>>' expr
      { PTExprWithoutVariable_36 $1 $3 }
   |  '+' expr %prec '++'
      { PTExprWithoutVariable_37 $2 }
   |  '-' expr %prec '--'
      { PTExprWithoutVariable_38 $2 }
   |  '!' expr
      { PTExprWithoutVariable_39 $2 }
   |  '~' expr
      { PTExprWithoutVariable_40 $2 }
   |  expr '===' expr
      { PTExprWithoutVariable_41 $1 $3 }
   |  expr '!==' expr
      { PTExprWithoutVariable_42 $1 $3 }
   |  expr '==' expr
      { PTExprWithoutVariable_43 $1 $3 }
   |  expr '!=' expr
      { PTExprWithoutVariable_44 $1 $3 }
   |  expr '<' expr
      { PTExprWithoutVariable_45 $1 $3 }
   |  expr '<=' expr
      { PTExprWithoutVariable_46 $1 $3 }
   |  expr '>' expr
      { PTExprWithoutVariable_47 $1 $3 }
   |  expr '>=' expr
      { PTExprWithoutVariable_48 $1 $3 }
   |  expr 'instanceof' class_name_reference
      { PTExprWithoutVariable_49 $1 $3 }
   |  parenthesis_expr
      { PTExprWithoutVariable_50 $1 }
   |  new_expr
      { PTExprWithoutVariable_51 $1 }
   |  '(' new_expr ')' instance_call
      { PTExprWithoutVariable_52 $2 $4 }
   |  expr '?' expr ':' expr
      { PTExprWithoutVariable_53 $1 $3 $5 }
   |  expr '?' ':' expr
      { PTExprWithoutVariable_54 $1 $4 }
   |  internal_functions_in_yacc
      { PTExprWithoutVariable_55 $1 }
   |  '(int)' expr
      { PTExprWithoutVariable_56 $2 }
   |  '(double)' expr
      { PTExprWithoutVariable_57 $2 }
   |  '(string)' expr
      { PTExprWithoutVariable_58 $2 }
   |  '(array)' expr
      { PTExprWithoutVariable_59 $2 }
   |  '(object)' expr
      { PTExprWithoutVariable_60 $2 }
   |  '(bool)' expr
      { PTExprWithoutVariable_61 $2 }
   |  '(unset)' expr
      { PTExprWithoutVariable_62 $2 }
   |  'exit' exit_expr
      { PTExprWithoutVariable_63 $2 }
   |  '@' expr
      { PTExprWithoutVariable_64 $2 }
   |  scalar
      { PTExprWithoutVariable_65 $1 }
   |  combined_scalar_offset
      { PTExprWithoutVariable_66 $1 }
   |  combined_scalar
      { PTExprWithoutVariable_67 $1 }
   |  '`' backticks_expr '`'
      { PTExprWithoutVariable_68 $2 }
   |  'print' expr
      { PTExprWithoutVariable_69 $2 }
   |  'yield'
      { PTExprWithoutVariable_70 }
   |  function is_reference '(' parameter_list ')' lexical_vars '{' inner_statement_list '}'
      { PTExprWithoutVariable_71 $1 $2 $4 $6 $8 }
   |  'static' function is_reference '(' parameter_list ')' lexical_vars '{' inner_statement_list '}'
      { PTExprWithoutVariable_72 $2 $3 $5 $7 $9 }

yield_expr :: { PTYieldExpr }
   :  'yield' expr_without_variable
      { PTYieldExpr_1 $2 }
   |  'yield' variable
      { PTYieldExpr_2 $2 }
   |  'yield' expr '=>' expr_without_variable
      { PTYieldExpr_3 $2 $4 }
   |  'yield' expr '=>' variable
      { PTYieldExpr_4 $2 $4 }

combined_scalar_offset :: { PTCombinedScalarOffset }
   :  combined_scalar '[' dim_offset ']'
      { PTCombinedScalarOffset_1 $1 $3 }
   |  combined_scalar_offset '[' dim_offset ']'
      { PTCombinedScalarOffset_2 $1 $3 }
   |  LT_STRING '[' dim_offset ']'
      { PTCombinedScalarOffset_3 (PVString $1) $3 }

combined_scalar :: { PTCombinedScalar }
   :  'array' '(' array_pair_list ')'
      { PTCombinedScalar_1 $3 }
   |  '[' array_pair_list ']'
      { PTCombinedScalar_2 $2 }

function :: { PTFunction }
   :  'function'
      { PTFunction_1 }

lexical_vars :: { PTLexicalVars }
   :  {- empty -}
      { PTLexicalVars_1 }
   |  'use' '(' lexical_var_list ')'
      { PTLexicalVars_2 $3 }

lexical_var_list :: { PTLexicalVarList }
   :  lexical_var_list ',' LT_VARNAME
      { PTLexicalVarList_1 $1 (PVVariableName $3) }
   |  lexical_var_list ',' '&' LT_VARNAME
      { PTLexicalVarList_2 $1 (PVVariableName $4) }
   |  LT_VARNAME
      { PTLexicalVarList_3 (PVVariableName $1) }
   |  '&' LT_VARNAME
      { PTLexicalVarList_4 (PVVariableName $2) }

function_call :: { PTFunctionCall }
   :  namespace_name function_call_parameter_list
      { PTFunctionCall_1 $1 $2 }
   |  'namespace' '\\' namespace_name function_call_parameter_list
      { PTFunctionCall_2 $3 $4 }
   |  '\\' namespace_name function_call_parameter_list
      { PTFunctionCall_3 $2 $3 }
   |  class_name '::' variable_name function_call_parameter_list
      { PTFunctionCall_4 $1 $3 $4 }
   |  class_name '::' variable_without_objects function_call_parameter_list
      { PTFunctionCall_5 $1 $3 $4 }
   |  variable_class_name '::' variable_name function_call_parameter_list
      { PTFunctionCall_6 $1 $3 $4 }
   |  variable_class_name '::' variable_without_objects function_call_parameter_list
      { PTFunctionCall_7 $1 $3 $4 }
   |  variable_without_objects function_call_parameter_list
      { PTFunctionCall_8 $1 $2 }

class_name :: { PTClassName }
   :  'static'
      { PTClassName_1 }
   |  namespace_name
      { PTClassName_2 $1 }
   |  'namespace' '\\' namespace_name
      { PTClassName_3 $3 }
   |  '\\' namespace_name
      { PTClassName_4 $2 }

fully_qualified_class_name :: { PTFullyQualifiedClassName }
   :  namespace_name
      { PTFullyQualifiedClassName_1 $1 }
   |  'namespace' '\\' namespace_name
      { PTFullyQualifiedClassName_2 $3 }
   |  '\\' namespace_name
      { PTFullyQualifiedClassName_3 $2 }

class_name_reference :: { PTClassNameReference }
   :  class_name
      { PTClassNameReference_1 $1 }
   |  dynamic_class_name_reference
      { PTClassNameReference_2 $1 }

dynamic_class_name_reference :: { PTDynamicClassNameReference }
   :  base_variable '->' object_property dynamic_class_name_variable_properties
      { PTDynamicClassNameReference_1 $1 $3 $4 }
   |  base_variable
      { PTDynamicClassNameReference_2 $1 }

dynamic_class_name_variable_properties :: { PTDynamicClassNameVariableProperties }
   :  dynamic_class_name_variable_properties dynamic_class_name_variable_property
      { PTDynamicClassNameVariableProperties_1 $1 $2 }
   |  {- empty -}
      { PTDynamicClassNameVariableProperties_2 }

dynamic_class_name_variable_property :: { PTDynamicClassNameVariableProperty }
   :  '->' object_property
      { PTDynamicClassNameVariableProperty_1 $2 }

exit_expr :: { PTExitExpr }
   :  {- empty -}
      { PTExitExpr_1 }
   |  '(' ')'
      { PTExitExpr_2 }
   |  parenthesis_expr
      { PTExitExpr_3 $1 }

backticks_expr :: { PTBackticksExpr }
   :  {- empty -}
      { PTBackticksExpr_1 }
   |  LT_STRING
      { PTBackticksExpr_2 (PVString $1) }
   |  encaps_list
      { PTBackticksExpr_3 $1 }

ctor_arguments :: { PTCtorArguments }
   :  {- empty -}
      { PTCtorArguments_1 }
   |  function_call_parameter_list
      { PTCtorArguments_2 $1 }

common_scalar :: { PTCommonScalar }
   :  LT_INTEGER
      { PTCommonScalar_1 (PVInteger $1) }
   |  LT_DOUBLE
      { PTCommonScalar_2 (PVDouble $1) }
   |  LT_STRING
      { PTCommonScalar_3 (PVString $1) }
   |  '__LINE__'
      { PTCommonScalar_4 }
   |  '__FILE__'
      { PTCommonScalar_5 }
   |  '__DIR__'
      { PTCommonScalar_6 }
   |  '__TRAIT__'
      { PTCommonScalar_7 }
   |  '__METHOD__'
      { PTCommonScalar_8 }
   |  '__FUNCTION__'
      { PTCommonScalar_9 }
   |  '__NAMESPACE__'
      { PTCommonScalar_10 }
   |  LT_HEREDOC_START LT_STRING LT_HEREDOC_END
      { PTCommonScalar_11 (PVString $2) }
   |  LT_HEREDOC_START LT_HEREDOC_END
      { PTCommonScalar_12 }

static_scalar :: { PTStaticScalar }
   :  common_scalar
      { PTStaticScalar_1 $1 }
   |  static_class_name_scalar
      { PTStaticScalar_2 $1 }
   |  namespace_name
      { PTStaticScalar_3 $1 }
   |  'namespace' '\\' namespace_name
      { PTStaticScalar_4 $3 }
   |  '\\' namespace_name
      { PTStaticScalar_5 $2 }
   |  '+' static_scalar
      { PTStaticScalar_6 $2 }
   |  '-' static_scalar
      { PTStaticScalar_7 $2 }
   |  'array' '(' static_array_pair_list ')'
      { PTStaticScalar_8 $3 }
   |  '[' static_array_pair_list ']'
      { PTStaticScalar_9 $2 }
   |  static_class_constant
      { PTStaticScalar_10 $1 }
   |  '__CLASS__'
      { PTStaticScalar_11 }

static_class_constant :: { PTStaticClassConstant }
   :  class_name '::' LT_IDENT
      { PTStaticClassConstant_1 $1 (PVIdent $3) }

scalar :: { PTScalar }
   :  LT_VARNAME_IMBED
      { PTScalar_1 (PVVariableNameImbed $1) }
   |  class_name_scalar
      { PTScalar_2 $1 }
   |  class_constant
      { PTScalar_3 $1 }
   |  namespace_name
      { PTScalar_4 $1 }
   |  'namespace' '\\' namespace_name
      { PTScalar_5 $3 }
   |  '\\' namespace_name
      { PTScalar_6 $2 }
   |  common_scalar
      { PTScalar_7 $1 }
   |  '"' encaps_list '"'
      { PTScalar_8 $2 }
   |  '"' LT_STRING '"'
      { PTScalar_9 (PVString $2) }
   |  LT_HEREDOC_START encaps_list LT_HEREDOC_END
      { PTScalar_10 $2 }
   |  '__CLASS__'
      { PTScalar_11 }

static_array_pair_list :: { PTStaticArrayPairList }
   :  {- empty -}
      { PTStaticArrayPairList_1 }
   |  non_empty_static_array_pair_list possible_comma
      { PTStaticArrayPairList_2 $1 $2 }

possible_comma :: { PTPossibleComma }
   :  {- empty -}
      { PTPossibleComma_1 }
   |  ','
      { PTPossibleComma_2 }

non_empty_static_array_pair_list :: { PTNonEmptyStaticArrayPairList }
   :  non_empty_static_array_pair_list ',' static_scalar '=>' static_scalar
      { PTNonEmptyStaticArrayPairList_1 $1 $3 $5 }
   |  non_empty_static_array_pair_list ',' static_scalar
      { PTNonEmptyStaticArrayPairList_2 $1 $3 }
   |  static_scalar '=>' static_scalar
      { PTNonEmptyStaticArrayPairList_3 $1 $3 }
   |  static_scalar
      { PTNonEmptyStaticArrayPairList_4 $1 }

expr :: { PTExpr }
   :  r_variable
      { PTExpr_1 $1 }
   |  expr_without_variable
      { PTExpr_2 $1 }

parenthesis_expr :: { PTParenthesisExpr }
   :  '(' expr ')'
      { PTParenthesisExpr_1 $2 }
   |  '(' yield_expr ')'
      { PTParenthesisExpr_2 $2 }

r_variable :: { PTRVariable }
   :  variable
      { PTRVariable_1 $1 }

w_variable :: { PTWVariable }
   :  variable
      { PTWVariable_1 $1 }

rw_variable :: { PTRwVariable }
   :  variable
      { PTRwVariable_1 $1 }

variable :: { PTVariable }
   :  base_variable_with_function_calls '->' object_property method_or_not variable_properties
      { PTVariable_1 $1 $3 $4 $5 }
   |  base_variable_with_function_calls
      { PTVariable_2 $1 }

variable_properties :: { PTVariableProperties }
   :  variable_properties variable_property
      { PTVariableProperties_1 $1 $2 }
   |  {- empty -}
      { PTVariableProperties_2 }

variable_property :: { PTVariableProperty }
   :  '->' object_property method_or_not
      { PTVariableProperty_1 $2 $3 }

array_method_dereference :: { PTArrayMethodDereference }
   :  array_method_dereference '[' dim_offset ']'
      { PTArrayMethodDereference_1 $1 $3 }
   |  method '[' dim_offset ']'
      { PTArrayMethodDereference_2 $1 $3 }

method :: { PTMethod }
   :  function_call_parameter_list
      { PTMethod_1 $1 }

method_or_not :: { PTMethodOrNot }
   :  method
      { PTMethodOrNot_1 $1 }
   |  array_method_dereference
      { PTMethodOrNot_2 $1 }
   |  {- empty -}
      { PTMethodOrNot_3 }

variable_without_objects :: { PTVariableWithoutObjects }
   :  reference_variable
      { PTVariableWithoutObjects_1 $1 }
   |  simple_indirect_reference reference_variable
      { PTVariableWithoutObjects_2 $1 $2 }

static_member :: { PTStaticMember }
   :  class_name '::' variable_without_objects
      { PTStaticMember_1 $1 $3 }
   |  variable_class_name '::' variable_without_objects
      { PTStaticMember_2 $1 $3 }

variable_class_name :: { PTVariableClassName }
   :  reference_variable
      { PTVariableClassName_1 $1 }

array_function_dereference :: { PTArrayFunctionDereference }
   :  array_function_dereference '[' dim_offset ']'
      { PTArrayFunctionDereference_1 $1 $3 }
   |  function_call '[' dim_offset ']'
      { PTArrayFunctionDereference_2 $1 $3 }

base_variable_with_function_calls :: { PTBaseVariableWithFunctionCalls }
   :  base_variable
      { PTBaseVariableWithFunctionCalls_1 $1 }
   |  array_function_dereference
      { PTBaseVariableWithFunctionCalls_2 $1 }
   |  function_call
      { PTBaseVariableWithFunctionCalls_3 $1 }

base_variable :: { PTBaseVariable }
   :  reference_variable
      { PTBaseVariable_1 $1 }
   |  simple_indirect_reference reference_variable
      { PTBaseVariable_2 $1 $2 }
   |  static_member
      { PTBaseVariable_3 $1 }

reference_variable :: { PTReferenceVariable }
   :  reference_variable '[' dim_offset ']'
      { PTReferenceVariable_1 $1 $3 }
   |  reference_variable '{' expr '}'
      { PTReferenceVariable_2 $1 $3 }
   |  compound_variable
      { PTReferenceVariable_3 $1 }

compound_variable :: { PTCompoundVariable }
   :  LT_VARNAME
      { PTCompoundVariable_1 (PVVariableName $1) }
   |  '$' '{' expr '}'
      { PTCompoundVariable_2 $3 }

dim_offset :: { PTDimOffset }
   :  {- empty -}
      { PTDimOffset_1 }
   |  expr
      { PTDimOffset_2 $1 }

object_property :: { PTObjectProperty }
   :  object_dim_list
      { PTObjectProperty_1 $1 }
   |  variable_without_objects
      { PTObjectProperty_2 $1 }

object_dim_list :: { PTObjectDimList }
   :  object_dim_list '[' dim_offset ']'
      { PTObjectDimList_1 $1 $3 }
   |  object_dim_list '{' expr '}'
      { PTObjectDimList_2 $1 $3 }
   |  variable_name
      { PTObjectDimList_3 $1 }

variable_name :: { PTVariableName }
   :  LT_IDENT
      { PTVariableName_1 (PVIdent $1) }
   |  '{' expr '}'
      { PTVariableName_2 $2 }

simple_indirect_reference :: { PTSimpleIndirectReference }
   :  '$'
      { PTSimpleIndirectReference_1 }
   |  simple_indirect_reference '$'
      { PTSimpleIndirectReference_2 $1 }

assignment_list :: { PTAssignmentList }
   :  assignment_list ',' assignment_list_element
      { PTAssignmentList_1 $1 $3 }
   |  assignment_list_element
      { PTAssignmentList_2 $1 }

assignment_list_element :: { PTAssignmentListElement }
   :  variable
      { PTAssignmentListElement_1 $1 }
   |  'list' '(' assignment_list ')'
      { PTAssignmentListElement_2 $3 }
   |  {- empty -}
      { PTAssignmentListElement_3 }

array_pair_list :: { PTArrayPairList }
   :  {- empty -}
      { PTArrayPairList_1 }
   |  non_empty_array_pair_list possible_comma
      { PTArrayPairList_2 $1 $2 }

non_empty_array_pair_list :: { PTNonEmptyArrayPairList }
   :  non_empty_array_pair_list ',' expr '=>' expr
      { PTNonEmptyArrayPairList_1 $1 $3 $5 }
   |  non_empty_array_pair_list ',' expr
      { PTNonEmptyArrayPairList_2 $1 $3 }
   |  expr '=>' expr
      { PTNonEmptyArrayPairList_3 $1 $3 }
   |  expr
      { PTNonEmptyArrayPairList_4 $1 }
   |  non_empty_array_pair_list ',' expr '=>' '&' w_variable
      { PTNonEmptyArrayPairList_5 $1 $3 $6 }
   |  non_empty_array_pair_list ',' '&' w_variable
      { PTNonEmptyArrayPairList_6 $1 $4 }
   |  expr '=>' '&' w_variable
      { PTNonEmptyArrayPairList_7 $1 $4 }
   |  '&' w_variable
      { PTNonEmptyArrayPairList_8 $2 }

encaps_list :: { PTEncapsList }
   :  encaps_list encaps_var
      { PTEncapsList_1 $1 $2 }
   |  encaps_list LT_STRING
      { PTEncapsList_2 $1 (PVString $2) }
   |  encaps_var
      { PTEncapsList_3 $1 }
   |  LT_STRING encaps_var
      { PTEncapsList_4 (PVString $1) $2 }

encaps_var :: { PTEncapsVar }
   :  LT_VARNAME
      { PTEncapsVar_1 (PVVariableName $1) }
   |  LT_VARNAME '[' encaps_var_offset ']'
      { PTEncapsVar_2 (PVVariableName $1) $3 }
   |  LT_VARNAME '->' LT_IDENT
      { PTEncapsVar_3 (PVVariableName $1) (PVIdent $3) }
   |  '${' expr '}'
      { PTEncapsVar_4 $2 }
   |  '${' LT_VARNAME_IMBED '[' expr ']' '}'
      { PTEncapsVar_5 (PVVariableNameImbed $2) $4 }
   |  '{' variable '}'
      { PTEncapsVar_6 $2 }

encaps_var_offset :: { PTEncapsVarOffset }
   :  LT_IDENT
      { PTEncapsVarOffset_1 (PVIdent $1) }
   |  LT_INTEGER
      { PTEncapsVarOffset_2 (PVInteger $1) }
   |  LT_VARNAME
      { PTEncapsVarOffset_3 (PVVariableName $1) }

internal_functions_in_yacc :: { PTInternalFunctionsInYacc }
   :  'isset' '(' isset_variables ')'
      { PTInternalFunctionsInYacc_1 $3 }
   |  'empty' '(' variable ')'
      { PTInternalFunctionsInYacc_2 $3 }
   |  'empty' '(' expr_without_variable ')'
      { PTInternalFunctionsInYacc_3 $3 }
   |  'include' expr
      { PTInternalFunctionsInYacc_4 $2 }
   |  'include_once' expr
      { PTInternalFunctionsInYacc_5 $2 }
   |  'eval' '(' expr ')'
      { PTInternalFunctionsInYacc_6 $3 }
   |  'require' expr
      { PTInternalFunctionsInYacc_7 $2 }
   |  'require_once' expr
      { PTInternalFunctionsInYacc_8 $2 }

isset_variables :: { PTIssetVariables }
   :  isset_variable
      { PTIssetVariables_1 $1 }
   |  isset_variables ',' isset_variable
      { PTIssetVariables_2 $1 $3 }

isset_variable :: { PTIssetVariable }
   :  variable
      { PTIssetVariable_1 $1 }
   |  expr_without_variable
      { PTIssetVariable_2 $1 }

class_constant :: { PTClassConstant }
   :  class_name '::' LT_IDENT
      { PTClassConstant_1 $1 (PVIdent $3) }
   |  variable_class_name '::' LT_IDENT
      { PTClassConstant_2 $1 (PVIdent $3) }

static_class_name_scalar :: { PTStaticClassNameScalar }
   :  class_name '::' 'class'
      { PTStaticClassNameScalar_1 $1 }

class_name_scalar :: { PTClassNameScalar }
   :  class_name '::' 'class'
      { PTClassNameScalar_1 $1 }

{

happyError :: P a
happyError = lexError
           
}
