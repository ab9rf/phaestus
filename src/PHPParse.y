{
module PHPParse ( phpParse ) where

import PHPLex (AlexState, Ctx(..), Token(..), Token'(..), mLexer, P, lexError)
import ParseTree 

}

%name phpParse start

%lexer { mLexer } { Ctx _ EOF }
%monad { P }

%tokentype { Token }

%token INLINE_HTML      { Ctx _ (InlineHTML _) }
%token LT_VARNAME       { Ctx _ (VariableToken _) }
%token LT_IDENT         { Ctx _ (IdentToken _) }
%token LT_VARNAME_IMBED { Ctx _ (VariableTokenInStr _) }
%token LT_INTEGER       { Ctx _ (IntegerToken _) }
%token LT_DOUBLE        { Ctx _ (RealToken _) }
%token LT_STRING        { Ctx _ (StringToken _) }

%token '(int)'          { Ctx _ CastInt }
%token '(double)'       { Ctx _ CastReal }
%token '(string)'       { Ctx _ CastString }
%token '(array)'        { Ctx _ CastArray }
%token '(object)'       { Ctx _ CastObject }
%token '(bool)'         { Ctx _ CastBool }
%token '(unset)'        { Ctx _ CastUnset }

%token '=='             { Ctx _ OpEqEq }
%token '==='            { Ctx _ OpEqEqEq }
%token '!='             { Ctx _ OpNotEq }
%token '!=='            { Ctx _ OpNotEqEq }
%token '<='             { Ctx _ OpLE }
%token '>='             { Ctx _ OpGE }
%token '++'             { Ctx _ OpInc }
%token '--'             { Ctx _ OpDec }
%token '=>'             { Ctx _ OpDoubleArrow }
%token '->'             { Ctx _ OpSingleArrow }
%token '<<'             { Ctx _ OpSL }
%token '>>'             { Ctx _ OpSR }
%token '+='             { Ctx _ OpPlusEq }
%token '-='             { Ctx _ OpMinusEq }
%token '*='             { Ctx _ OpMultEq }
%token '/='             { Ctx _ OpDivEq }
%token '.='             { Ctx _ OpConcatEq }
%token '%='             { Ctx _ OpModEq }
%token '&='             { Ctx _ OpAndEq }
%token '|='             { Ctx _ OpOrEq }
%token '^='             { Ctx _ OpXorEq }
%token '<<='            { Ctx _ OpSLEq }
%token '>>='            { Ctx _ OpSREq }
%token '::'             { Ctx _ OpColonColon }
%token '&&'             { Ctx _ OpLogicAnd }
%token '||'             { Ctx _ OpLogicOr }
%token '+'              { Ctx _ OpPlus }
%token '-'              { Ctx _ OpMinus }
%token '/'              { Ctx _ OpSlash }
%token '*'              { Ctx _ OpStar }
%token '%'              { Ctx _ OpPercent }
%token '^'              { Ctx _ OpCaret }
%token '&'              { Ctx _ OpAmpersand }
%token '|'              { Ctx _ OpPipe }
%token '~'              { Ctx _ OpTilde }
%token '='              { Ctx _ OpEq }
%token '<'              { Ctx _ OpLt }
%token '>'              { Ctx _ OpGt }
%token '.'              { Ctx _ OpDot }
%token '!'              { Ctx _ OpBang }
%token ','              { Ctx _ OpComma }
%token '?'              { Ctx _ OpQuestion }
%token ':'              { Ctx _ OpColon }
%token '@'              { Ctx _ OpAtSign }
%token '$'              { Ctx _ OpDollars }
%token '\\'             { Ctx _ Backslash }
%token '`'              { Ctx _ Backquote }
%token '"'              { Ctx _ DoubleQuote }

%token '${'             { Ctx _ DollarOpenCurlyBrace }

%token ';'              { Ctx _ Semicolon }
%token '('              { Ctx _ LParen }
%token ')'              { Ctx _ RParen }
%token '{'              { Ctx _ LBrace }
%token '}'              { Ctx _ RBrace }
%token '['              { Ctx _ LBracket }
%token ']'              { Ctx _ RBracket }

%token LT_HEREDOC_END   { Ctx _ EndHeredoc }
%token LT_HEREDOC_START { Ctx _ StartHeredoc }

%token 'and'            { Ctx _ KeywordAnd }
%token 'or'             { Ctx _ KeywordOr }
%token 'xor'            { Ctx _ KeywordXor }
%token '__FILE__'       { Ctx _ Keyword__FILE__ }
%token '__LINE__'       { Ctx _ Keyword__LINE__ }
%token '__DIR__'        { Ctx _ Keyword__DIR__ }
%token 'array'          { Ctx _ KeywordArray }
%token 'as'             { Ctx _ KeywordAs }
%token 'break'          { Ctx _ KeywordBreak }
%token 'case'           { Ctx _ KeywordCase }
%token 'class'          { Ctx _ KeywordClass }
%token 'const'          { Ctx _ KeywordConst }
%token 'continue'       { Ctx _ KeywordContinue }
%token 'declare'        { Ctx _ KeywordDeclare }
%token 'default'        { Ctx _ KeywordDefault }
%token 'do'             { Ctx _ KeywordDo }
%token 'echo'           { Ctx _ KeywordEcho }
%token 'else'           { Ctx _ KeywordElse }
%token 'elseif'         { Ctx _ KeywordElseif }
%token 'empty'          { Ctx _ KeywordEmpty }
%token 'enddeclare'     { Ctx _ KeywordEnddeclare }
%token 'endfor'         { Ctx _ KeywordEndfor }
%token 'endforeach'     { Ctx _ KeywordEndforeach }
%token 'endif'          { Ctx _ KeywordEndif }
%token 'endswitch'      { Ctx _ KeywordEndswitch }
%token 'endwhile'       { Ctx _ KeywordEndwhile }
%token 'eval'           { Ctx _ KeywordEval }
%token 'exit'           { Ctx _ KeywordExit }
%token 'extends'        { Ctx _ KeywordExtends }
%token 'for'            { Ctx _ KeywordFor }
%token 'foreach'        { Ctx _ KeywordForeach }
%token 'function'       { Ctx _ KeywordFunction }
%token 'global'         { Ctx _ KeywordGlobal }
%token 'if'             { Ctx _ KeywordIf }
%token 'include'        { Ctx _ KeywordInclude }
%token 'include_once'   { Ctx _ KeywordIncludeOnce }
%token 'instanceof'     { Ctx _ KeywordInstanceOf }
%token 'isset'          { Ctx _ KeywordIsset }
%token 'list'           { Ctx _ KeywordList }
%token 'new'            { Ctx _ KeywordNew }
%token 'print'          { Ctx _ KeywordPrint }
%token 'require'        { Ctx _ KeywordRequire }
%token 'require_once'   { Ctx _ KeywordRequireOnce }
%token 'return'         { Ctx _ KeywordReturn }
%token 'static'         { Ctx _ KeywordStatic }
%token 'switch'         { Ctx _ KeywordSwitch }
%token 'unset'          { Ctx _ KeywordUnset }
%token 'use'            { Ctx _ KeywordUse }
%token 'var'            { Ctx _ KeywordVar }
%token 'while'          { Ctx _ KeywordWhile }
%token '__FUNCTION__'   { Ctx _ Keyword__FUNCTION__ }
%token '__CLASS__'      { Ctx _ Keyword__CLASS__ }
%token '__METHOD__'     { Ctx _ Keyword__METHOD__ }
%token 'final'          { Ctx _ KeywordFinal }
%token 'interface'      { Ctx _ KeywordInterface }
%token 'implements'     { Ctx _ KeywordImplements }
%token 'public'         { Ctx _ KeywordPublic }
%token 'private'        { Ctx _ KeywordPrivate }
%token 'protected'      { Ctx _ KeywordProtected }
%token 'abstract'       { Ctx _ KeywordAbstract }
%token 'clone'          { Ctx _ KeywordClone }
%token 'try'            { Ctx _ KeywordTry }
%token 'catch'          { Ctx _ KeywordCatch }
%token 'throw'          { Ctx _ KeywordThrow }
%token 'namespace'      { Ctx _ KeywordNamespace }
%token 'goto'           { Ctx _ KeywordGoto }
%token 'finally'        { Ctx _ KeywordFinally }
%token 'trait'          { Ctx _ KeywordTrait }
%token 'callable'       { Ctx _ KeywordCallable }
%token 'insteadof'      { Ctx _ KeywordInsteadof }
%token 'yield'          { Ctx _ KeywordYield }
%token '__TRAIT__'      { Ctx _ Keyword__TRAIT__ }
%token '__NAMESPACE__'  { Ctx _ Keyword__NAMESPACE__ }

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
      { PTStart1 $1 }

top_statement_list :: { PTTopStatementList }
   :  top_statement_list top_statement
      { PTTopStatementList1 $1 $2 }
   |  {- empty -}
      { PTTopStatementList2 }

namespace_name :: { PTNamespaceName }
   :  LT_IDENT
      { PTNamespaceName1 $1 }
   |  namespace_name '\\' LT_IDENT
      { PTNamespaceName2 $1 $3 }

top_statement :: { PTTopStatement }
   :  statement
      { PTTopStatement1 $1 }
   |  function_declaration_statement
      { PTTopStatement2 $1 }
   |  class_declaration_statement
      { PTTopStatement3 $1 }
   |  'namespace' namespace_name ';'
      { PTTopStatement4 $2 }
   |  'namespace' namespace_name '{' top_statement_list '}'
      { PTTopStatement5 $2 $4 }
   |  'namespace' '{' top_statement_list '}'
      { PTTopStatement6 $3 }
   |  'use' use_declarations ';'
      { PTTopStatement7 $2 }
   |  constant_declaration ';'
      { PTTopStatement8 $1 }

use_declarations :: { PTUseDeclarations }
   :  use_declarations ',' use_declaration
      { PTUseDeclarations1 $1 $3 }
   |  use_declaration
      { PTUseDeclarations2 $1 }

use_declaration :: { PTUseDeclaration }
   :  namespace_name
      { PTUseDeclaration1 $1 }
   |  namespace_name 'as' LT_IDENT
      { PTUseDeclaration2 $1 $3 }
   |  '\\' namespace_name
      { PTUseDeclaration3 $2 }
   |  '\\' namespace_name 'as' LT_IDENT
      { PTUseDeclaration4 $2 $4 }

constant_declaration :: { PTConstantDeclaration }
   :  constant_declaration ',' LT_IDENT '=' static_scalar
      { PTConstantDeclaration1 $1 $3 $5 }
   |  'const' LT_IDENT '=' static_scalar
      { PTConstantDeclaration2 $2 $4 }

inner_statement_list :: { PTInnerStatementList }
   :  inner_statement_list inner_statement
      { PTInnerStatementList1 $1 $2 }
   |  {- empty -}
      { PTInnerStatementList2 }

inner_statement :: { PTInnerStatement }
   :  statement
      { PTInnerStatement1 $1 }
   |  function_declaration_statement
      { PTInnerStatement2 $1 }
   |  class_declaration_statement
      { PTInnerStatement3 $1 }

statement :: { PTStatement }
   :  unticked_statement
      { PTStatement1 $1 }
   |  LT_IDENT ':'
      { PTStatement2 $1 }

unticked_statement :: { PTUntickedStatement }
   :  '{' inner_statement_list '}'
      { PTUntickedStatement1 $2 }
   |  'if' parenthesis_expr statement elseif_list else_single
      { PTUntickedStatement2 $2 $3 $4 $5 }
   |  'if' parenthesis_expr ':' inner_statement_list new_elseif_list new_else_single 'endif' ';'
      { PTUntickedStatement3 $2 $4 $5 $6 }
   |  'while' parenthesis_expr while_statement
      { PTUntickedStatement4 $2 $3 }
   |  'do' statement 'while' parenthesis_expr ';'
      { PTUntickedStatement5 $2 $4 }
   |  'for' '(' for_expr ';' for_expr ';' for_expr ')' for_statement
      { PTUntickedStatement6 $3 $5 $7 $9 }
   |  'switch' parenthesis_expr switch_case_list
      { PTUntickedStatement7 $2 $3 }
   |  'break' ';'
      { PTUntickedStatement8 }
   |  'break' expr ';'
      { PTUntickedStatement9 $2 }
   |  'continue' ';'
      { PTUntickedStatement10 }
   |  'continue' expr ';'
      { PTUntickedStatement11 $2 }
   |  'return' ';'
      { PTUntickedStatement12 }
   |  'return' expr_without_variable ';'
      { PTUntickedStatement13 $2 }
   |  'return' variable ';'
      { PTUntickedStatement14 $2 }
   |  yield_expr ';'
      { PTUntickedStatement15 $1 }
   |  'global' global_var_list ';'
      { PTUntickedStatement16 $2 }
   |  'static' static_var_list ';'
      { PTUntickedStatement17 $2 }
   |  'echo' echo_expr_list ';'
      { PTUntickedStatement18 $2 }
   |  INLINE_HTML
      { PTUntickedStatement19 $1 }
   |  expr ';'
      { PTUntickedStatement20 $1 }
   |  'unset' '(' unset_variables ')' ';'
      { PTUntickedStatement21 $3 }
   |  'foreach' '(' variable 'as' foreach_variable foreach_optional_arg ')' foreach_statement
      { PTUntickedStatement22 $3 $5 $6 $8 }
   |  'foreach' '(' expr_without_variable 'as' foreach_variable foreach_optional_arg ')' foreach_statement
      { PTUntickedStatement23 $3 $5 $6 $8 }
   |  'declare' '(' declare_list ')' declare_statement
      { PTUntickedStatement24 $3 $5 }
   |  ';'
      { PTUntickedStatement25 }
   |  'try' '{' inner_statement_list '}' catch_statement finally_statement
      { PTUntickedStatement26 $3 $5 $6 }
   |  'throw' expr ';'
      { PTUntickedStatement27 $2 }
   |  'goto' LT_IDENT ';'
      { PTUntickedStatement28 $2 }

catch_statement :: { PTCatchStatement }
   :  {- empty -}
      { PTCatchStatement1 }
   |  'catch' '(' fully_qualified_class_name LT_VARNAME ')' '{' inner_statement_list '}' additional_catches
      { PTCatchStatement2 $3 $4 $7 $9 }

finally_statement :: { PTFinallyStatement }
   :  {- empty -}
      { PTFinallyStatement1 }
   |  'finally' '{' inner_statement_list '}'
      { PTFinallyStatement2 $3 }

additional_catches :: { PTAdditionalCatches }
   :  non_empty_additional_catches
      { PTAdditionalCatches1 $1 }
   |  {- empty -}
      { PTAdditionalCatches2 }

non_empty_additional_catches :: { PTNonEmptyAdditionalCatches }
   :  additional_catch
      { PTNonEmptyAdditionalCatches1 $1 }
   |  non_empty_additional_catches additional_catch
      { PTNonEmptyAdditionalCatches2 $1 $2 }

additional_catch :: { PTAdditionalCatch }
   :  'catch' '(' fully_qualified_class_name LT_VARNAME ')' '{' inner_statement_list '}'
      { PTAdditionalCatch1 $3 $4 $7 }

unset_variables :: { PTUnsetVariables }
   :  unset_variable
      { PTUnsetVariables1 $1 }
   |  unset_variables ',' unset_variable
      { PTUnsetVariables2 $1 $3 }

unset_variable :: { PTUnsetVariable }
   :  variable
      { PTUnsetVariable1 $1 }

function_declaration_statement :: { PTFunctionDeclarationStatement }
   :  unticked_function_declaration_statement
      { PTFunctionDeclarationStatement1 $1 }

class_declaration_statement :: { PTClassDeclarationStatement }
   :  unticked_class_declaration_statement
      { PTClassDeclarationStatement1 $1 }

is_reference :: { PTIsReference }
   :  {- empty -}
      { PTIsReference1 }
   |  '&'
      { PTIsReference2 }

unticked_function_declaration_statement :: { PTUntickedFunctionDeclarationStatement }
   :  function is_reference LT_IDENT '(' parameter_list ')' '{' inner_statement_list '}'
      { PTUntickedFunctionDeclarationStatement1 $1 $2 $3 $5 $8 }

unticked_class_declaration_statement :: { PTUntickedClassDeclarationStatement }
   :  class_entry_type LT_IDENT extends_from implements_list '{' class_statement_list '}'
      { PTUntickedClassDeclarationStatement1 $1 $2 $3 $4 $6 }
   |  interface_entry LT_IDENT interface_extends_list '{' class_statement_list '}'
      { PTUntickedClassDeclarationStatement2 $1 $2 $3 $5 }

class_entry_type :: { PTClassEntryType }
   :  'class'
      { PTClassEntryType1 }
   |  'abstract' 'class'
      { PTClassEntryType2 }
   |  'trait'
      { PTClassEntryType3 }
   |  'final' 'class'
      { PTClassEntryType4 }

extends_from :: { PTExtendsFrom }
   :  {- empty -}
      { PTExtendsFrom1 }
   |  'extends' fully_qualified_class_name
      { PTExtendsFrom2 $2 }

interface_entry :: { PTInterfaceEntry }
   :  'interface'
      { PTInterfaceEntry1 }

interface_extends_list :: { PTInterfaceExtendsList }
   :  {- empty -}
      { PTInterfaceExtendsList1 }
   |  'extends' interface_list
      { PTInterfaceExtendsList2 $2 }

implements_list :: { PTImplementsList }
   :  {- empty -}
      { PTImplementsList1 }
   |  'implements' interface_list
      { PTImplementsList2 $2 }

interface_list :: { PTInterfaceList }
   :  fully_qualified_class_name
      { PTInterfaceList1 $1 }
   |  interface_list ',' fully_qualified_class_name
      { PTInterfaceList2 $1 $3 }

foreach_optional_arg :: { PTForeachOptionalArg }
   :  {- empty -}
      { PTForeachOptionalArg1 }
   |  '=>' foreach_variable
      { PTForeachOptionalArg2 $2 }

foreach_variable :: { PTForeachVariable }
   :  variable
      { PTForeachVariable1 $1 }
   |  '&' variable
      { PTForeachVariable2 $2 }
   |  'list' '(' assignment_list ')'
      { PTForeachVariable3 $3 }

for_statement :: { PTForStatement }
   :  statement
      { PTForStatement1 $1 }
   |  ':' inner_statement_list 'endfor' ';'
      { PTForStatement2 $2 }

foreach_statement :: { PTForeachStatement }
   :  statement
      { PTForeachStatement1 $1 }
   |  ':' inner_statement_list 'endforeach' ';'
      { PTForeachStatement2 $2 }

declare_statement :: { PTDeclareStatement }
   :  statement
      { PTDeclareStatement1 $1 }
   |  ':' inner_statement_list 'enddeclare' ';'
      { PTDeclareStatement2 $2 }

declare_list :: { PTDeclareList }
   :  LT_IDENT '=' static_scalar
      { PTDeclareList1 $1 $3 }
   |  declare_list ',' LT_IDENT '=' static_scalar
      { PTDeclareList2 $1 $3 $5 }

switch_case_list :: { PTSwitchCaseList }
   :  '{' case_list '}'
      { PTSwitchCaseList1 $2 }
   |  '{' ';' case_list '}'
      { PTSwitchCaseList2 $3 }
   |  ':' case_list 'endswitch' ';'
      { PTSwitchCaseList3 $2 }
   |  ':' ';' case_list 'endswitch' ';'
      { PTSwitchCaseList4 $3 }

case_list :: { PTCaseList }
   :  {- empty -}
      { PTCaseList1 }
   |  case_list 'case' expr case_separator inner_statement_list
      { PTCaseList2 $1 $3 $4 $5 }
   |  case_list 'default' case_separator inner_statement_list
      { PTCaseList3 $1 $3 $4 }

case_separator :: { PTCaseSeparator }
   :  ':'
      { PTCaseSeparator1 }
   |  ';'
      { PTCaseSeparator2 }

while_statement :: { PTWhileStatement }
   :  statement
      { PTWhileStatement1 $1 }
   |  ':' inner_statement_list 'endwhile' ';'
      { PTWhileStatement2 $2 }

elseif_list :: { PTElseifList }
   :  {- empty -}
      { PTElseifList1 }
   |  elseif_list 'elseif' parenthesis_expr statement
      { PTElseifList2 $1 $3 $4 }

new_elseif_list :: { PTNewElseifList }
   :  {- empty -}
      { PTNewElseifList1 }
   |  new_elseif_list 'elseif' parenthesis_expr ':' inner_statement_list
      { PTNewElseifList2 $1 $3 $5 }

else_single :: { PTElseSingle }
   :  {- empty -}
      { PTElseSingle1 }
   |  'else' statement
      { PTElseSingle2 $2 }

new_else_single :: { PTNewElseSingle }
   :  {- empty -}
      { PTNewElseSingle1 }
   |  'else' ':' inner_statement_list
      { PTNewElseSingle2 $3 }

parameter_list :: { PTParameterList }
   :  non_empty_parameter_list
      { PTParameterList1 $1 }
   |  {- empty -}
      { PTParameterList2 }

non_empty_parameter_list :: { PTNonEmptyParameterList }
   :  optional_class_type LT_VARNAME
      { PTNonEmptyParameterList1 $1 $2 }
   |  optional_class_type '&' LT_VARNAME
      { PTNonEmptyParameterList2 $1 $3 }
   |  optional_class_type '&' LT_VARNAME '=' static_scalar
      { PTNonEmptyParameterList3 $1 $3 $5 }
   |  optional_class_type LT_VARNAME '=' static_scalar
      { PTNonEmptyParameterList4 $1 $2 $4 }
   |  non_empty_parameter_list ',' optional_class_type LT_VARNAME
      { PTNonEmptyParameterList5 $1 $3 $4 }
   |  non_empty_parameter_list ',' optional_class_type '&' LT_VARNAME
      { PTNonEmptyParameterList6 $1 $3 $5 }
   |  non_empty_parameter_list ',' optional_class_type '&' LT_VARNAME '=' static_scalar
      { PTNonEmptyParameterList7 $1 $3 $5 $7 }
   |  non_empty_parameter_list ',' optional_class_type LT_VARNAME '=' static_scalar
      { PTNonEmptyParameterList8 $1 $3 $4 $6 }

optional_class_type :: { PTOptionalClassType }
   :  {- empty -}
      { PTOptionalClassType1 }
   |  'array'
      { PTOptionalClassType2 }
   |  'callable'
      { PTOptionalClassType3 }
   |  fully_qualified_class_name
      { PTOptionalClassType4 $1 }

function_call_parameter_list :: { PTFunctionCallParameterList }
   :  '(' ')'
      { PTFunctionCallParameterList1 }
   |  '(' non_empty_function_call_parameter_list ')'
      { PTFunctionCallParameterList2 $2 }
   |  '(' yield_expr ')'
      { PTFunctionCallParameterList3 $2 }

non_empty_function_call_parameter_list :: { PTNonEmptyFunctionCallParameterList }
   :  expr_without_variable
      { PTNonEmptyFunctionCallParameterList1 $1 }
   |  variable
      { PTNonEmptyFunctionCallParameterList2 $1 }
   |  '&' w_variable
      { PTNonEmptyFunctionCallParameterList3 $2 }
   |  non_empty_function_call_parameter_list ',' expr_without_variable
      { PTNonEmptyFunctionCallParameterList4 $1 $3 }
   |  non_empty_function_call_parameter_list ',' variable
      { PTNonEmptyFunctionCallParameterList5 $1 $3 }
   |  non_empty_function_call_parameter_list ',' '&' w_variable
      { PTNonEmptyFunctionCallParameterList6 $1 $4 }

global_var_list :: { PTGlobalVarList }
   :  global_var_list ',' global_var
      { PTGlobalVarList1 $1 $3 }
   |  global_var
      { PTGlobalVarList2 $1 }

global_var :: { PTGlobalVar }
   :  LT_VARNAME
      { PTGlobalVar1 $1 }
   |  '$' r_variable
      { PTGlobalVar2 $2 }
   |  '$' '{' expr '}'
      { PTGlobalVar3 $3 }

static_var_list :: { PTStaticVarList }
   :  static_var_list ',' LT_VARNAME
      { PTStaticVarList1 $1 $3 }
   |  static_var_list ',' LT_VARNAME '=' static_scalar
      { PTStaticVarList2 $1 $3 $5 }
   |  LT_VARNAME
      { PTStaticVarList3 $1 }
   |  LT_VARNAME '=' static_scalar
      { PTStaticVarList4 $1 $3 }

class_statement_list :: { PTClassStatementList }
   :  class_statement_list class_statement
      { PTClassStatementList1 $1 $2 }
   |  {- empty -}
      { PTClassStatementList2 }

class_statement :: { PTClassStatement }
   :  variable_modifiers class_variable_declaration ';'
      { PTClassStatement1 $1 $2 }
   |  class_constant_declaration ';'
      { PTClassStatement2 $1 }
   |  trait_use_statement
      { PTClassStatement3 $1 }
   |  method_modifiers function is_reference LT_IDENT '(' parameter_list ')' method_body
      { PTClassStatement4 $1 $2 $3 $4 $6 $8 }

trait_use_statement :: { PTTraitUseStatement }
   :  'use' trait_list trait_adaptations
      { PTTraitUseStatement1 $2 $3 }

trait_list :: { PTTraitList }
   :  fully_qualified_class_name
      { PTTraitList1 $1 }
   |  trait_list ',' fully_qualified_class_name
      { PTTraitList2 $1 $3 }

trait_adaptations :: { PTTraitAdaptations }
   :  ';'
      { PTTraitAdaptations1 }
   |  '{' trait_adaptation_list '}'
      { PTTraitAdaptations2 $2 }

trait_adaptation_list :: { PTTraitAdaptationList }
   :  {- empty -}
      { PTTraitAdaptationList1 }
   |  non_empty_trait_adaptation_list
      { PTTraitAdaptationList2 $1 }

non_empty_trait_adaptation_list :: { PTNonEmptyTraitAdaptationList }
   :  trait_adaptation_statement
      { PTNonEmptyTraitAdaptationList1 $1 }
   |  non_empty_trait_adaptation_list trait_adaptation_statement
      { PTNonEmptyTraitAdaptationList2 $1 $2 }

trait_adaptation_statement :: { PTTraitAdaptationStatement }
   :  trait_precedence ';'
      { PTTraitAdaptationStatement1 $1 }
   |  trait_alias ';'
      { PTTraitAdaptationStatement2 $1 }

trait_precedence :: { PTTraitPrecedence }
   :  trait_method_reference_fully_qualified 'insteadof' trait_reference_list
      { PTTraitPrecedence1 $1 $3 }

trait_reference_list :: { PTTraitReferenceList }
   :  fully_qualified_class_name
      { PTTraitReferenceList1 $1 }
   |  trait_reference_list ',' fully_qualified_class_name
      { PTTraitReferenceList2 $1 $3 }

trait_method_reference :: { PTTraitMethodReference }
   :  LT_IDENT
      { PTTraitMethodReference1 $1 }
   |  trait_method_reference_fully_qualified
      { PTTraitMethodReference2 $1 }

trait_method_reference_fully_qualified :: { PTTraitMethodReferenceFullyQualified }
   :  fully_qualified_class_name '::' LT_IDENT
      { PTTraitMethodReferenceFullyQualified1 $1 $3 }

trait_alias :: { PTTraitAlias }
   :  trait_method_reference 'as' trait_modifiers LT_IDENT
      { PTTraitAlias1 $1 $3 $4 }
   |  trait_method_reference 'as' member_modifier
      { PTTraitAlias2 $1 $3 }

trait_modifiers :: { PTTraitModifiers }
   :  {- empty -}
      { PTTraitModifiers1 }
   |  member_modifier
      { PTTraitModifiers2 $1 }

method_body :: { PTMethodBody }
   :  ';'
      { PTMethodBody1 }
   |  '{' inner_statement_list '}'
      { PTMethodBody2 $2 }

variable_modifiers :: { PTVariableModifiers }
   :  non_empty_member_modifiers
      { PTVariableModifiers1 $1 }
   |  'var'
      { PTVariableModifiers2 }

method_modifiers :: { PTMethodModifiers }
   :  {- empty -}
      { PTMethodModifiers1 }
   |  non_empty_member_modifiers
      { PTMethodModifiers2 $1 }

non_empty_member_modifiers :: { PTNonEmptyMemberModifiers }
   :  member_modifier
      { PTNonEmptyMemberModifiers1 $1 }
   |  non_empty_member_modifiers member_modifier
      { PTNonEmptyMemberModifiers2 $1 $2 }

member_modifier :: { PTMemberModifier }
   :  'public'
      { PTMemberModifier1 }
   |  'protected'
      { PTMemberModifier2 }
   |  'private'
      { PTMemberModifier3 }
   |  'static'
      { PTMemberModifier4 }
   |  'abstract'
      { PTMemberModifier5 }
   |  'final'
      { PTMemberModifier6 }

class_variable_declaration :: { PTClassVariableDeclaration }
   :  class_variable_declaration ',' LT_VARNAME
      { PTClassVariableDeclaration1 $1 $3 }
   |  class_variable_declaration ',' LT_VARNAME '=' static_scalar
      { PTClassVariableDeclaration2 $1 $3 $5 }
   |  LT_VARNAME
      { PTClassVariableDeclaration3 $1 }
   |  LT_VARNAME '=' static_scalar
      { PTClassVariableDeclaration4 $1 $3 }

class_constant_declaration :: { PTClassConstantDeclaration }
   :  class_constant_declaration ',' LT_IDENT '=' static_scalar
      { PTClassConstantDeclaration1 $1 $3 $5 }
   |  'const' LT_IDENT '=' static_scalar
      { PTClassConstantDeclaration2 $2 $4 }

echo_expr_list :: { PTEchoExprList }
   :  echo_expr_list ',' expr
      { PTEchoExprList1 $1 $3 }
   |  expr
      { PTEchoExprList2 $1 }

for_expr :: { PTForExpr }
   :  {- empty -}
      { PTForExpr1 }
   |  non_empty_for_expr
      { PTForExpr2 $1 }

non_empty_for_expr :: { PTNonEmptyForExpr }
   :  non_empty_for_expr ',' expr
      { PTNonEmptyForExpr1 $1 $3 }
   |  expr
      { PTNonEmptyForExpr2 $1 }

chaining_method_or_property :: { PTChainingMethodOrProperty }
   :  chaining_method_or_property variable_property
      { PTChainingMethodOrProperty1 $1 $2 }
   |  variable_property
      { PTChainingMethodOrProperty2 $1 }

chaining_dereference :: { PTChainingDereference }
   :  chaining_dereference '[' dim_offset ']'
      { PTChainingDereference1 $1 $3 }
   |  '[' dim_offset ']'
      { PTChainingDereference2 $2 }

chaining_instance_call :: { PTChainingInstanceCall }
   :  chaining_dereference chaining_method_or_property
      { PTChainingInstanceCall1 $1 $2 }
   |  chaining_dereference
      { PTChainingInstanceCall2 $1 }
   |  chaining_method_or_property
      { PTChainingInstanceCall3 $1 }

instance_call :: { PTInstanceCall }
   :  {- empty -}
      { PTInstanceCall1 }
   |  chaining_instance_call
      { PTInstanceCall2 $1 }

new_expr :: { PTNewExpr }
   :  'new' class_name_reference ctor_arguments
      { PTNewExpr1 $2 $3 }

expr_without_variable :: { PTExpr }
   :  'list' '(' assignment_list ')' '=' expr
      { ListAssignment $3 $6 }
   |  variable '=' expr
      { VariableAssignment $1 $3 }
   |  variable '=' '&' variable
      { ReferenceAssignment $1 $4 }
   |  variable '=' '&' 'new' class_name_reference ctor_arguments
      { PTExprWithoutVariable4 $1 $5 $6 }
   |  'clone' expr
      { PTExprWithoutVariable5 $2 }
   |  variable '+=' expr
      { PTExprWithoutVariable6 $1 $3 }
   |  variable '-=' expr
      { PTExprWithoutVariable7 $1 $3 }
   |  variable '*=' expr
      { PTExprWithoutVariable8 $1 $3 }
   |  variable '/=' expr
      { PTExprWithoutVariable9 $1 $3 }
   |  variable '.=' expr
      { PTExprWithoutVariable10 $1 $3 }
   |  variable '%=' expr
      { PTExprWithoutVariable11 $1 $3 }
   |  variable '&=' expr
      { PTExprWithoutVariable12 $1 $3 }
   |  variable '|=' expr
      { PTExprWithoutVariable13 $1 $3 }
   |  variable '^=' expr
      { PTExprWithoutVariable14 $1 $3 }
   |  variable '<<=' expr
      { PTExprWithoutVariable15 $1 $3 }
   |  variable '>>=' expr
      { PTExprWithoutVariable16 $1 $3 }
   |  rw_variable '++'
      { PTExprWithoutVariable17 $1 }
   |  '++' rw_variable
      { PTExprWithoutVariable18 $2 }
   |  rw_variable '--'
      { PTExprWithoutVariable19 $1 }
   |  '--' rw_variable
      { PTExprWithoutVariable20 $2 }
   |  expr '||' expr
      { PTExprWithoutVariable21 $1 $3 }
   |  expr '&&' expr
      { PTExprWithoutVariable22 $1 $3 }
   |  expr 'or' expr
      { PTExprWithoutVariable23 $1 $3 }
   |  expr 'and' expr
      { PTExprWithoutVariable24 $1 $3 }
   |  expr 'xor' expr
      { PTExprWithoutVariable25 $1 $3 }
   |  expr '|' expr
      { PTExprWithoutVariable26 $1 $3 }
   |  expr '&' expr
      { PTExprWithoutVariable27 $1 $3 }
   |  expr '^' expr
      { PTExprWithoutVariable28 $1 $3 }
   |  expr '.' expr
      { PTExprWithoutVariable29 $1 $3 }
   |  expr '+' expr
      { PTExprWithoutVariable30 $1 $3 }
   |  expr '-' expr
      { PTExprWithoutVariable31 $1 $3 }
   |  expr '*' expr
      { PTExprWithoutVariable32 $1 $3 }
   |  expr '/' expr
      { PTExprWithoutVariable33 $1 $3 }
   |  expr '%' expr
      { PTExprWithoutVariable34 $1 $3 }
   |  expr '<<' expr
      { PTExprWithoutVariable35 $1 $3 }
   |  expr '>>' expr
      { PTExprWithoutVariable36 $1 $3 }
   |  '+' expr %prec '++'
      { PTExprWithoutVariable37 $2 }
   |  '-' expr %prec '--'
      { PTExprWithoutVariable38 $2 }
   |  '!' expr
      { PTExprWithoutVariable39 $2 }
   |  '~' expr
      { PTExprWithoutVariable40 $2 }
   |  expr '===' expr
      { PTExprWithoutVariable41 $1 $3 }
   |  expr '!==' expr
      { PTExprWithoutVariable42 $1 $3 }
   |  expr '==' expr
      { PTExprWithoutVariable43 $1 $3 }
   |  expr '!=' expr
      { PTExprWithoutVariable44 $1 $3 }
   |  expr '<' expr
      { PTExprWithoutVariable45 $1 $3 }
   |  expr '<=' expr
      { PTExprWithoutVariable46 $1 $3 }
   |  expr '>' expr
      { PTExprWithoutVariable47 $1 $3 }
   |  expr '>=' expr
      { PTExprWithoutVariable48 $1 $3 }
   |  expr 'instanceof' class_name_reference
      { PTExprWithoutVariable49 $1 $3 }
   |  parenthesis_expr
      { PTExprWithoutVariable50 $1 }
   |  new_expr
      { PTExprWithoutVariable51 $1 }
   |  '(' new_expr ')' instance_call
      { PTExprWithoutVariable52 $2 $4 }
   |  expr '?' expr ':' expr
      { PTExprWithoutVariable53 $1 $3 $5 }
   |  expr '?' ':' expr
      { PTExprWithoutVariable54 $1 $4 }
   |  internal_functions_in_yacc
      { PTExprWithoutVariable55 $1 }
   |  '(int)' expr
      { PTExprWithoutVariable56 $2 }
   |  '(double)' expr
      { PTExprWithoutVariable57 $2 }
   |  '(string)' expr
      { PTExprWithoutVariable58 $2 }
   |  '(array)' expr
      { PTExprWithoutVariable59 $2 }
   |  '(object)' expr
      { PTExprWithoutVariable60 $2 }
   |  '(bool)' expr
      { PTExprWithoutVariable61 $2 }
   |  '(unset)' expr
      { PTExprWithoutVariable62 $2 }
   |  'exit' exit_expr
      { PTExprWithoutVariable63 $2 }
   |  '@' expr
      { PTExprWithoutVariable64 $2 }
   |  scalar
      { PTExprWithoutVariable65 $1 }
   |  combined_scalar_offset
      { PTExprWithoutVariable66 $1 }
   |  combined_scalar
      { PTExprWithoutVariable67 $1 }
   |  '`' backticks_expr '`'
      { PTExprWithoutVariable68 $2 }
   |  'print' expr
      { PTExprWithoutVariable69 $2 }
   |  'yield'
      { PTExprWithoutVariable70 }
   |  function is_reference '(' parameter_list ')' lexical_vars '{' inner_statement_list '}'
      { PTExprWithoutVariable71 $1 $2 $4 $6 $8 }
   |  'static' function is_reference '(' parameter_list ')' lexical_vars '{' inner_statement_list '}'
      { PTExprWithoutVariable72 $2 $3 $5 $7 $9 }

yield_expr :: { PTYieldExpr }
   :  'yield' expr_without_variable
      { PTYieldExpr1 $2 }
   |  'yield' variable
      { PTYieldExpr2 $2 }
   |  'yield' expr '=>' expr_without_variable
      { PTYieldExpr3 $2 $4 }
   |  'yield' expr '=>' variable
      { PTYieldExpr4 $2 $4 }

combined_scalar_offset :: { PTCombinedScalarOffset }
   :  combined_scalar '[' dim_offset ']'
      { PTCombinedScalarOffset1 $1 $3 }
   |  combined_scalar_offset '[' dim_offset ']'
      { PTCombinedScalarOffset2 $1 $3 }
   |  LT_STRING '[' dim_offset ']'
      { PTCombinedScalarOffset3 $1 $3 }

combined_scalar :: { PTCombinedScalar }
   :  'array' '(' array_pair_list ')'
      { PTCombinedScalar1 $3 }
   |  '[' array_pair_list ']'
      { PTCombinedScalar2 $2 }

function :: { PTFunction }
   :  'function'
      { PTFunction1 }

lexical_vars :: { PTLexicalVars }
   :  {- empty -}
      { PTLexicalVars1 }
   |  'use' '(' lexical_var_list ')'
      { PTLexicalVars2 $3 }

lexical_var_list :: { PTLexicalVarList }
   :  lexical_var_list ',' LT_VARNAME
      { PTLexicalVarList1 $1 $3 }
   |  lexical_var_list ',' '&' LT_VARNAME
      { PTLexicalVarList2 $1 $4 }
   |  LT_VARNAME
      { PTLexicalVarList3 $1 }
   |  '&' LT_VARNAME
      { PTLexicalVarList4 $2 }

function_call :: { PTFunctionCall }
   :  namespace_name function_call_parameter_list
      { PTFunctionCall1 $1 $2 }
   |  'namespace' '\\' namespace_name function_call_parameter_list
      { PTFunctionCall2 $3 $4 }
   |  '\\' namespace_name function_call_parameter_list
      { PTFunctionCall3 $2 $3 }
   |  class_name '::' variable_name function_call_parameter_list
      { PTFunctionCall4 $1 $3 $4 }
   |  class_name '::' variable_without_objects function_call_parameter_list
      { PTFunctionCall5 $1 $3 $4 }
   |  variable_class_name '::' variable_name function_call_parameter_list
      { PTFunctionCall6 $1 $3 $4 }
   |  variable_class_name '::' variable_without_objects function_call_parameter_list
      { PTFunctionCall7 $1 $3 $4 }
   |  variable_without_objects function_call_parameter_list
      { PTFunctionCall8 $1 $2 }

class_name :: { PTClassName }
   :  'static'
      { PTClassName1 }
   |  namespace_name
      { PTClassName2 $1 }
   |  'namespace' '\\' namespace_name
      { PTClassName3 $3 }
   |  '\\' namespace_name
      { PTClassName4 $2 }

fully_qualified_class_name :: { PTFullyQualifiedClassName }
   :  namespace_name
      { PTFullyQualifiedClassName1 $1 }
   |  'namespace' '\\' namespace_name
      { PTFullyQualifiedClassName2 $3 }
   |  '\\' namespace_name
      { PTFullyQualifiedClassName3 $2 }

class_name_reference :: { PTClassNameReference }
   :  class_name
      { PTClassNameReference1 $1 }
   |  dynamic_class_name_reference
      { PTClassNameReference2 $1 }

dynamic_class_name_reference :: { PTDynamicClassNameReference }
   :  base_variable '->' object_property dynamic_class_name_variable_properties
      { PTDynamicClassNameReference1 $1 $3 $4 }
   |  base_variable
      { PTDynamicClassNameReference2 $1 }

dynamic_class_name_variable_properties :: { PTDynamicClassNameVariableProperties }
   :  dynamic_class_name_variable_properties dynamic_class_name_variable_property
      { PTDynamicClassNameVariableProperties1 $1 $2 }
   |  {- empty -}
      { PTDynamicClassNameVariableProperties2 }

dynamic_class_name_variable_property :: { PTDynamicClassNameVariableProperty }
   :  '->' object_property
      { PTDynamicClassNameVariableProperty1 $2 }

exit_expr :: { PTExitExpr }
   :  {- empty -}
      { PTExitExpr1 }
   |  '(' ')'
      { PTExitExpr2 }
   |  parenthesis_expr
      { PTExitExpr3 $1 }

backticks_expr :: { PTBackticksExpr }
   :  {- empty -}
      { PTBackticksExpr1 }
   |  LT_STRING
      { PTBackticksExpr2 $1 }
   |  encaps_list
      { PTBackticksExpr3 $1 }

ctor_arguments :: { PTCtorArguments }
   :  {- empty -}
      { PTCtorArguments1 }
   |  function_call_parameter_list
      { PTCtorArguments2 $1 }

common_scalar :: { PTCommonScalar }
   :  LT_INTEGER
      { PTCommonScalar1 $1 }
   |  LT_DOUBLE
      { PTCommonScalar2 $1 }
   |  LT_STRING
      { PTCommonScalar3 $1 }
   |  '__LINE__'
      { PTCommonScalar4 }
   |  '__FILE__'
      { PTCommonScalar5 }
   |  '__DIR__'
      { PTCommonScalar6 }
   |  '__TRAIT__'
      { PTCommonScalar7 }
   |  '__METHOD__'
      { PTCommonScalar8 }
   |  '__FUNCTION__'
      { PTCommonScalar9 }
   |  '__NAMESPACE__'
      { PTCommonScalar10 }
   |  LT_HEREDOC_START LT_STRING LT_HEREDOC_END
      { PTCommonScalar11 $2 }
   |  LT_HEREDOC_START LT_HEREDOC_END
      { PTCommonScalar12 }

static_scalar :: { PTStaticScalar }
   :  common_scalar
      { PTStaticScalar1 $1 }
   |  static_class_name_scalar
      { PTStaticScalar2 $1 }
   |  namespace_name
      { PTStaticScalar3 $1 }
   |  'namespace' '\\' namespace_name
      { PTStaticScalar4 $3 }
   |  '\\' namespace_name
      { PTStaticScalar5 $2 }
   |  '+' static_scalar
      { PTStaticScalar6 $2 }
   |  '-' static_scalar
      { PTStaticScalar7 $2 }
   |  'array' '(' static_array_pair_list ')'
      { PTStaticScalar8 $3 }
   |  '[' static_array_pair_list ']'
      { PTStaticScalar9 $2 }
   |  static_class_constant
      { PTStaticScalar10 $1 }
   |  '__CLASS__'
      { PTStaticScalar11 }

static_class_constant :: { PTStaticClassConstant }
   :  class_name '::' LT_IDENT
      { PTStaticClassConstant1 $1 $3 }

scalar :: { PTScalar }
   :  LT_VARNAME_IMBED
      { PTScalar1 $1 }
   |  class_name_scalar
      { PTScalar2 $1 }
   |  class_constant
      { PTScalar3 $1 }
   |  namespace_name
      { PTScalar4 $1 }
   |  'namespace' '\\' namespace_name
      { PTScalar5 $3 }
   |  '\\' namespace_name
      { PTScalar6 $2 }
   |  common_scalar
      { PTScalar7 $1 }
   |  '"' encaps_list '"'
      { PTScalar8 $2 }
   |  '"' LT_STRING '"'
      { PTScalar9 $2 }
   |  LT_HEREDOC_START encaps_list LT_HEREDOC_END
      { PTScalar10 $2 }
   |  '__CLASS__'
      { PTScalar11 }

static_array_pair_list :: { PTStaticArrayPairList }
   :  {- empty -}
      { PTStaticArrayPairList1 }
   |  non_empty_static_array_pair_list possible_comma
      { PTStaticArrayPairList2 $1 $2 }

possible_comma :: { PTPossibleComma }
   :  {- empty -}
      { PTPossibleComma1 }
   |  ','
      { PTPossibleComma2 }

non_empty_static_array_pair_list :: { PTNonEmptyStaticArrayPairList }
   :  non_empty_static_array_pair_list ',' static_scalar '=>' static_scalar
      { PTNonEmptyStaticArrayPairList1 $1 $3 $5 }
   |  non_empty_static_array_pair_list ',' static_scalar
      { PTNonEmptyStaticArrayPairList2 $1 $3 }
   |  static_scalar '=>' static_scalar
      { PTNonEmptyStaticArrayPairList3 $1 $3 }
   |  static_scalar
      { PTNonEmptyStaticArrayPairList4 $1 }

expr :: { PTExpr }
   :  r_variable
      { RValueAsLvalue $1 }
   |  expr_without_variable
      { $1 }

parenthesis_expr :: { PTParenthesisExpr }
   :  '(' expr ')'
      { PTParenthesisExpr1 $2 }
   |  '(' yield_expr ')'
      { PTParenthesisExpr2 $2 }

r_variable :: { PTRVariable }
   :  variable
      { PTRVariable1 $1 }

w_variable :: { PTWVariable }
   :  variable
      { PTWVariable1 $1 }

rw_variable :: { PTRwVariable }
   :  variable
      { PTRwVariable1 $1 }

variable :: { PTVariable }
   :  base_variable_with_function_calls '->' object_property method_or_not variable_properties
      { PTVariable1 $1 $3 $4 $5 }
   |  base_variable_with_function_calls
      { PTVariable2 $1 }

variable_properties :: { PTVariableProperties }
   :  variable_properties variable_property
      { PTVariableProperties1 $1 $2 }
   |  {- empty -}
      { PTVariableProperties2 }

variable_property :: { PTVariableProperty }
   :  '->' object_property method_or_not
      { PTVariableProperty1 $2 $3 }

array_method_dereference :: { PTArrayMethodDereference }
   :  array_method_dereference '[' dim_offset ']'
      { PTArrayMethodDereference1 $1 $3 }
   |  method '[' dim_offset ']'
      { PTArrayMethodDereference2 $1 $3 }

method :: { PTMethod }
   :  function_call_parameter_list
      { PTMethod1 $1 }

method_or_not :: { PTMethodOrNot }
   :  method
      { PTMethodOrNot1 $1 }
   |  array_method_dereference
      { PTMethodOrNot2 $1 }
   |  {- empty -}
      { PTMethodOrNot3 }

variable_without_objects :: { PTVariableWithoutObjects }
   :  reference_variable
      { PTVariableWithoutObjects1 $1 }
   |  simple_indirect_reference reference_variable
      { PTVariableWithoutObjects2 $1 $2 }

static_member :: { PTStaticMember }
   :  class_name '::' variable_without_objects
      { PTStaticMember1 $1 $3 }
   |  variable_class_name '::' variable_without_objects
      { PTStaticMember2 $1 $3 }

variable_class_name :: { PTVariableClassName }
   :  reference_variable
      { PTVariableClassName1 $1 }

array_function_dereference :: { PTArrayFunctionDereference }
   :  array_function_dereference '[' dim_offset ']'
      { PTArrayFunctionDereference1 $1 $3 }
   |  function_call '[' dim_offset ']'
      { PTArrayFunctionDereference2 $1 $3 }

base_variable_with_function_calls :: { PTBaseVariableWithFunctionCalls }
   :  base_variable
      { PTBaseVariableWithFunctionCalls1 $1 }
   |  array_function_dereference
      { PTBaseVariableWithFunctionCalls2 $1 }
   |  function_call
      { PTBaseVariableWithFunctionCalls3 $1 }

base_variable :: { PTBaseVariable }
   :  reference_variable
      { PTBaseVariable1 $1 }
   |  simple_indirect_reference reference_variable
      { PTBaseVariable2 $1 $2 }
   |  static_member
      { PTBaseVariable3 $1 }

reference_variable :: { PTReferenceVariable }
   :  reference_variable '[' dim_offset ']'
      { PTReferenceVariable1 $1 $3 }
   |  reference_variable '{' expr '}'
      { PTReferenceVariable2 $1 $3 }
   |  compound_variable
      { PTReferenceVariable3 $1 }

compound_variable :: { PTCompoundVariable }
   :  LT_VARNAME
      { PTCompoundVariable1 $1 }
   |  '$' '{' expr '}'
      { PTCompoundVariable2 $3 }

dim_offset :: { PTDimOffset }
   :  {- empty -}
      { PTDimOffset1 }
   |  expr
      { PTDimOffset2 $1 }

object_property :: { PTObjectProperty }
   :  object_dim_list
      { PTObjectProperty1 $1 }
   |  variable_without_objects
      { PTObjectProperty2 $1 }

object_dim_list :: { PTObjectDimList }
   :  object_dim_list '[' dim_offset ']'
      { PTObjectDimList1 $1 $3 }
   |  object_dim_list '{' expr '}'
      { PTObjectDimList2 $1 $3 }
   |  variable_name
      { PTObjectDimList3 $1 }

variable_name :: { PTVariableName }
   :  LT_IDENT
      { PTVariableName1 $1 }
   |  '{' expr '}'
      { PTVariableName2 $2 }

simple_indirect_reference :: { PTSimpleIndirectReference }
   :  '$'
      { PTSimpleIndirectReference1 }
   |  simple_indirect_reference '$'
      { PTSimpleIndirectReference2 $1 }

assignment_list :: { PTAssignmentList }
   :  assignment_list ',' assignment_list_element
      { PTAssignmentList1 $1 $3 }
   |  assignment_list_element
      { PTAssignmentList2 $1 }

assignment_list_element :: { PTAssignmentListElement }
   :  variable
      { PTAssignmentListElement1 $1 }
   |  'list' '(' assignment_list ')'
      { PTAssignmentListElement2 $3 }
   |  {- empty -}
      { PTAssignmentListElement3 }

array_pair_list :: { PTArrayPairList }
   :  {- empty -}
      { PTArrayPairList1 }
   |  non_empty_array_pair_list possible_comma
      { PTArrayPairList2 $1 $2 }

non_empty_array_pair_list :: { PTNonEmptyArrayPairList }
   :  non_empty_array_pair_list ',' expr '=>' expr
      { PTNonEmptyArrayPairList1 $1 $3 $5 }
   |  non_empty_array_pair_list ',' expr
      { PTNonEmptyArrayPairList2 $1 $3 }
   |  expr '=>' expr
      { PTNonEmptyArrayPairList3 $1 $3 }
   |  expr
      { PTNonEmptyArrayPairList4 $1 }
   |  non_empty_array_pair_list ',' expr '=>' '&' w_variable
      { PTNonEmptyArrayPairList5 $1 $3 $6 }
   |  non_empty_array_pair_list ',' '&' w_variable
      { PTNonEmptyArrayPairList6 $1 $4 }
   |  expr '=>' '&' w_variable
      { PTNonEmptyArrayPairList7 $1 $4 }
   |  '&' w_variable
      { PTNonEmptyArrayPairList8 $2 }

encaps_list :: { PTEncapsList }
   :  encaps_list encaps_var
      { PTEncapsList1 $1 $2 }
   |  encaps_list LT_STRING
      { PTEncapsList2 $1 $2 }
   |  encaps_var
      { PTEncapsList3 $1 }
   |  LT_STRING encaps_var
      { PTEncapsList4 $1 $2 }

encaps_var :: { PTEncapsVar }
   :  LT_VARNAME
      { PTEncapsVar1 $1 }
   |  LT_VARNAME '[' encaps_var_offset ']'
      { PTEncapsVar2 $1 $3 }
   |  LT_VARNAME '->' LT_IDENT
      { PTEncapsVar3 $1 $3 }
   |  '${' expr '}'
      { PTEncapsVar4 $2 }
   |  '${' LT_VARNAME_IMBED '[' expr ']' '}'
      { PTEncapsVar5 $2 $4 }
   |  '{' variable '}'
      { PTEncapsVar6 $2 }

encaps_var_offset :: { PTEncapsVarOffset }
   :  LT_IDENT
      { PTEncapsVarOffset1 $1 }
   |  LT_INTEGER
      { PTEncapsVarOffset2 $1 }
   |  LT_VARNAME
      { PTEncapsVarOffset3 $1 }

internal_functions_in_yacc :: { PTInternalFunctionsInYacc }
   :  'isset' '(' isset_variables ')'
      { PTInternalFunctionsInYacc1 $3 }
   |  'empty' '(' variable ')'
      { PTInternalFunctionsInYacc2 $3 }
   |  'empty' '(' expr_without_variable ')'
      { PTInternalFunctionsInYacc3 $3 }
   |  'include' expr
      { PTInternalFunctionsInYacc4 $2 }
   |  'include_once' expr
      { PTInternalFunctionsInYacc5 $2 }
   |  'eval' '(' expr ')'
      { PTInternalFunctionsInYacc6 $3 }
   |  'require' expr
      { PTInternalFunctionsInYacc7 $2 }
   |  'require_once' expr
      { PTInternalFunctionsInYacc8 $2 }

isset_variables :: { PTIssetVariables }
   :  isset_variable
      { PTIssetVariables1 $1 }
   |  isset_variables ',' isset_variable
      { PTIssetVariables2 $1 $3 }

isset_variable :: { PTIssetVariable }
   :  variable
      { PTIssetVariable1 $1 }
   |  expr_without_variable
      { PTIssetVariable2 $1 }

class_constant :: { PTClassConstant }
   :  class_name '::' LT_IDENT
      { PTClassConstant1 $1 $3 }
   |  variable_class_name '::' LT_IDENT
      { PTClassConstant2 $1 $3 }

static_class_name_scalar :: { PTStaticClassNameScalar }
   :  class_name '::' 'class'
      { PTStaticClassNameScalar1 $1 }

class_name_scalar :: { PTClassNameScalar }
   :  class_name '::' 'class'
      { PTClassNameScalar1 $1 }
{

happyError :: P a
happyError = lexError
           
}
