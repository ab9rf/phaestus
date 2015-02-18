{
module PHPParse ( phpParse ) where

import PHPLex (AlexState, Context(..), Ctx(..), Token(..), Token'(..), mLexer, P, lexError)
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

start :: { }
   :  top_statement_list
      { }

top_statement_list :: { }
   :  top_statement_list top_statement
      { }
   |  {- empty -}
      { }

namespace_name :: { }
   :  LT_IDENT
      { }
   |  namespace_name '\\' LT_IDENT
      { }

top_statement :: { }
   :  statement
      { }
   |  function_declaration_statement
      { }
   |  class_declaration_statement
      { }
   |  'namespace' namespace_name ';'
      { }
   |  'namespace' namespace_name '{' top_statement_list '}'
      { }
   |  'namespace' '{' top_statement_list '}'
      { }
   |  'use' use_declarations ';'
      { }
   |  constant_declaration ';'
      { }

use_declarations :: { }
   :  use_declarations ',' use_declaration
      { }
   |  use_declaration
      { }

use_declaration :: { }
   :  namespace_name
      { }
   |  namespace_name 'as' LT_IDENT
      { }
   |  '\\' namespace_name
      { }
   |  '\\' namespace_name 'as' LT_IDENT
      { }

constant_declaration :: { }
   :  constant_declaration ',' LT_IDENT '=' static_scalar
      { }
   |  'const' LT_IDENT '=' static_scalar
      { }

inner_statement_list :: { }
   :  inner_statement_list inner_statement
      { }
   |  {- empty -}
      { }

inner_statement :: { }
   :  statement
      { }
   |  function_declaration_statement
      { }
   |  class_declaration_statement
      { }

statement :: { }
   :  LT_IDENT ':'
      { }
   |  '{' inner_statement_list '}'
      { }
   |  'if' parenthesis_expr statement elseif_list else_single
      { }
   |  'if' parenthesis_expr ':' inner_statement_list new_elseif_list new_else_single 'endif' ';'
      { }
   |  'while' parenthesis_expr while_statement
      { }
   |  'do' statement 'while' parenthesis_expr ';'
      { }
   |  'for' '(' for_expr ';' for_expr ';' for_expr ')' for_statement
      { }
   |  'switch' parenthesis_expr switch_case_list
      { }
   |  'break' ';'
      { }
   |  'break' expr ';'
      { }
   |  'continue' ';'
      { }
   |  'continue' expr ';'
      { }
   |  'return' ';'
      { }
   |  'return' expr ';'
      { }
   |  yield_expr ';'
      { }
   |  'global' global_var_list ';'
      { }
   |  'static' static_var_list ';'
      { }
   |  'echo' echo_expr_list ';'
      { }
   |  INLINE_HTML
      { }
   |  expr ';'
      { }
   |  'unset' '(' unset_variables ')' ';'
      { }
   |  'foreach' '(' expr 'as' foreach_variable foreach_optional_arg ')' foreach_statement
      { }
   |  'declare' '(' declare_list ')' declare_statement
      { }
   |  ';'
      { }
   |  'try' '{' inner_statement_list '}' catch_statement finally_statement
      { }
   |  'throw' expr ';'
      { }
   |  'goto' LT_IDENT ';'
      { }

catch_statement :: { }
   :  {- empty -}
      { }
   |  'catch' '(' fully_qualified_class_name LT_VARNAME ')' '{' inner_statement_list '}' additional_catches
      { }

finally_statement :: { }
   :  {- empty -}
      { }
   |  'finally' '{' inner_statement_list '}'
      { }

additional_catches :: { }
   :  non_empty_additional_catches
      { }
   |  {- empty -}
      { }

non_empty_additional_catches :: { }
   :  additional_catch
      { }
   |  non_empty_additional_catches additional_catch
      { }

additional_catch :: { }
   :  'catch' '(' fully_qualified_class_name LT_VARNAME ')' '{' inner_statement_list '}'
      { }

unset_variables :: { }
   :  unset_variable
      { }
   |  unset_variables ',' unset_variable
      { }

unset_variable :: { }
   :  variable
      { }

function_declaration_statement :: { }
   :  unticked_function_declaration_statement
      { }

class_declaration_statement :: { }
   :  unticked_class_declaration_statement
      { }

is_reference :: { }
   :  {- empty -}
      { }
   |  '&'
      { }

unticked_function_declaration_statement :: { }
   :  function is_reference LT_IDENT '(' parameter_list ')' '{' inner_statement_list '}'
      { }

unticked_class_declaration_statement :: { }
   :  class_entry_type LT_IDENT extends_from implements_list '{' class_statement_list '}'
      { }
   |  interface_entry LT_IDENT interface_extends_list '{' class_statement_list '}'
      { }

class_entry_type :: { }
   :  'class'
      { }
   |  'abstract' 'class'
      { }
   |  'trait'
      { }
   |  'final' 'class'
      { }

extends_from :: { }
   :  {- empty -}
      { }
   |  'extends' fully_qualified_class_name
      { }

interface_entry :: { }
   :  'interface'
      { }

interface_extends_list :: { }
   :  {- empty -}
      { }
   |  'extends' interface_list
      { }

implements_list :: { }
   :  {- empty -}
      { }
   |  'implements' interface_list
      { }

interface_list :: { }
   :  fully_qualified_class_name
      { }
   |  interface_list ',' fully_qualified_class_name
      { }

foreach_optional_arg :: { }
   :  {- empty -}
      { }
   |  '=>' foreach_variable
      { }

foreach_variable :: { }
   :  variable
      { }
   |  '&' variable
      { }
   |  'list' '(' assignment_list ')'
      { }

for_statement :: { }
   :  statement
      { }
   |  ':' inner_statement_list 'endfor' ';'
      { }

foreach_statement :: { }
   :  statement
      { }
   |  ':' inner_statement_list 'endforeach' ';'
      { }

declare_statement :: { }
   :  statement
      { }
   |  ':' inner_statement_list 'enddeclare' ';'
      { }

declare_list :: { }
   :  LT_IDENT '=' static_scalar
      { }
   |  declare_list ',' LT_IDENT '=' static_scalar
      { }

switch_case_list :: { }
   :  '{' case_list '}'
      { }
   |  '{' ';' case_list '}'
      { }
   |  ':' case_list 'endswitch' ';'
      { }
   |  ':' ';' case_list 'endswitch' ';'
      { }

case_list :: { }
   :  {- empty -}
      { }
   |  case_list 'case' expr case_separator inner_statement_list
      { }
   |  case_list 'default' case_separator inner_statement_list
      { }

case_separator :: { }
   :  ':'
      { }
   |  ';'
      { }

while_statement :: { }
   :  statement
      { }
   |  ':' inner_statement_list 'endwhile' ';'
      { }

elseif_list :: { }
   :  {- empty -}
      { }
   |  elseif_list 'elseif' parenthesis_expr statement
      { }

new_elseif_list :: { }
   :  {- empty -}
      { }
   |  new_elseif_list 'elseif' parenthesis_expr ':' inner_statement_list
      { }

else_single :: { }
   :  {- empty -}
      { }
   |  'else' statement
      { }

new_else_single :: { }
   :  {- empty -}
      { }
   |  'else' ':' inner_statement_list
      { }

parameter_list :: { }
   :  non_empty_parameter_list
      { }
   |  {- empty -}
      { }

non_empty_parameter_list :: { }
   :  optional_class_type LT_VARNAME
      { }
   |  optional_class_type '&' LT_VARNAME
      { }
   |  optional_class_type '&' LT_VARNAME '=' static_scalar
      { }
   |  optional_class_type LT_VARNAME '=' static_scalar
      { }
   |  non_empty_parameter_list ',' optional_class_type LT_VARNAME
      { }
   |  non_empty_parameter_list ',' optional_class_type '&' LT_VARNAME
      { }
   |  non_empty_parameter_list ',' optional_class_type '&' LT_VARNAME '=' static_scalar
      { }
   |  non_empty_parameter_list ',' optional_class_type LT_VARNAME '=' static_scalar
      { }

optional_class_type :: { }
   :  {- empty -}
      { }
   |  'array'
      { }
   |  'callable'
      { }
   |  fully_qualified_class_name
      { }

function_call_parameter_list :: { }
   :  '(' ')'
      { }
   |  '(' non_empty_function_call_parameter_list ')'
      { }
   |  '(' yield_expr ')'
      { }

non_empty_function_call_parameter_list :: { }
   :  expr
      { }
   |  '&' w_variable
      { }
   |  non_empty_function_call_parameter_list ',' expr
      { }
   |  non_empty_function_call_parameter_list ',' '&' w_variable
      { }

global_var_list :: { }
   :  global_var_list ',' global_var
      { }
   |  global_var
      { }

global_var :: { }
   :  LT_VARNAME
      { }
   |  '$' r_variable
      { }
   |  '$' '{' expr '}'
      { }

static_var_list :: { }
   :  static_var_list ',' LT_VARNAME
      { }
   |  static_var_list ',' LT_VARNAME '=' static_scalar
      { }
   |  LT_VARNAME
      { }
   |  LT_VARNAME '=' static_scalar
      { }

class_statement_list :: { }
   :  class_statement_list class_statement
      { }
   |  {- empty -}
      { }

class_statement :: { }
   :  variable_modifiers class_variable_declaration ';'
      { }
   |  class_constant_declaration ';'
      { }
   |  trait_use_statement
      { }
   |  method_modifiers function is_reference LT_IDENT '(' parameter_list ')' method_body
      { }

trait_use_statement :: { }
   :  'use' trait_list trait_adaptations
      { }

trait_list :: { }
   :  fully_qualified_class_name
      { }
   |  trait_list ',' fully_qualified_class_name
      { }

trait_adaptations :: { }
   :  ';'
      { }
   |  '{' trait_adaptation_list '}'
      { }

trait_adaptation_list :: { }
   :  {- empty -}
      { }
   |  non_empty_trait_adaptation_list
      { }

non_empty_trait_adaptation_list :: { }
   :  trait_adaptation_statement
      { }
   |  non_empty_trait_adaptation_list trait_adaptation_statement
      { }

trait_adaptation_statement :: { }
   :  trait_precedence ';'
      { }
   |  trait_alias ';'
      { }

trait_precedence :: { }
   :  trait_method_reference_fully_qualified 'insteadof' trait_reference_list
      { }

trait_reference_list :: { }
   :  fully_qualified_class_name
      { }
   |  trait_reference_list ',' fully_qualified_class_name
      { }

trait_method_reference :: { }
   :  LT_IDENT
      { }
   |  trait_method_reference_fully_qualified
      { }

trait_method_reference_fully_qualified :: { }
   :  fully_qualified_class_name '::' LT_IDENT
      { }

trait_alias :: { }
   :  trait_method_reference 'as' trait_modifiers LT_IDENT
      { }
   |  trait_method_reference 'as' member_modifier
      { }

trait_modifiers :: { }
   :  {- empty -}
      { }
   |  member_modifier
      { }

method_body :: { }
   :  ';'
      { }
   |  '{' inner_statement_list '}'
      { }

variable_modifiers :: { }
   :  non_empty_member_modifiers
      { }
   |  'var'
      { }

method_modifiers :: { }
   :  {- empty -}
      { }
   |  non_empty_member_modifiers
      { }

non_empty_member_modifiers :: { }
   :  member_modifier
      { }
   |  non_empty_member_modifiers member_modifier
      { }

member_modifier :: { }
   :  'public'
      { }
   |  'protected'
      { }
   |  'private'
      { }
   |  'static'
      { }
   |  'abstract'
      { }
   |  'final'
      { }

class_variable_declaration :: { }
   :  class_variable_declaration ',' LT_VARNAME
      { }
   |  class_variable_declaration ',' LT_VARNAME '=' static_scalar
      { }
   |  LT_VARNAME
      { }
   |  LT_VARNAME '=' static_scalar
      { }

class_constant_declaration :: { }
   :  class_constant_declaration ',' LT_IDENT '=' static_scalar
      { }
   |  'const' LT_IDENT '=' static_scalar
      { }

echo_expr_list :: { }
   :  echo_expr_list ',' expr
      { }
   |  expr
      { }

for_expr :: { }
   :  {- empty -}
      { }
   |  non_empty_for_expr
      { }

non_empty_for_expr :: { }
   :  non_empty_for_expr ',' expr
      { }
   |  expr
      { }

chaining_method_or_property :: { }
   :  chaining_method_or_property variable_property
      { }
   |  variable_property
      { }

chaining_dereference :: { }
   :  chaining_dereference '[' dim_offset ']'
      { }
   |  '[' dim_offset ']'
      { }

chaining_instance_call :: { }
   :  chaining_dereference chaining_method_or_property
      { }
   |  chaining_dereference
      { }
   |  chaining_method_or_property
      { }

instance_call :: { }
   :  {- empty -}
      { }
   |  chaining_instance_call
      { }

new_expr :: { }
   :  'new' class_name_reference ctor_arguments
      { }

expr :: { }
   :  'list' '(' assignment_list ')' '=' expr
      { }
   |  variable 
      { }
   |  variable '=' expr
      { }
   |  variable '=' '&' variable
      { }
   |  variable '=' '&' 'new' class_name_reference ctor_arguments
      { }
   |  'clone' expr
      { }
   |  variable '+=' expr
      { }
   |  variable '-=' expr
      { }
   |  variable '*=' expr
      { }
   |  variable '/=' expr
      { }
   |  variable '.=' expr
      { }
   |  variable '%=' expr
      { }
   |  variable '&=' expr
      { }
   |  variable '|=' expr
      { }
   |  variable '^=' expr
      { }
   |  variable '<<=' expr
      { }
   |  variable '>>=' expr
      { }
   |  rw_variable '++'
      { }
   |  '++' rw_variable
      { }
   |  rw_variable '--'
      { }
   |  '--' rw_variable
      { }
   |  expr '||' expr
      { }
   |  expr '&&' expr
      { }
   |  expr 'or' expr
      { }
   |  expr 'and' expr
      { }
   |  expr 'xor' expr
      { }
   |  expr '|' expr
      { }
   |  expr '&' expr
      { }
   |  expr '^' expr
      { }
   |  expr '.' expr
      { }
   |  expr '+' expr
      { }
   |  expr '-' expr
      { }
   |  expr '*' expr
      { }
   |  expr '/' expr
      { }
   |  expr '%' expr
      { }
   |  expr '<<' expr
      { }
   |  expr '>>' expr
      { }
   |  '+' expr %prec '++'
      { }
   |  '-' expr %prec '--'
      { }
   |  '!' expr
      { }
   |  '~' expr
      { }
   |  expr '===' expr
      { }
   |  expr '!==' expr
      { }
   |  expr '==' expr
      { }
   |  expr '!=' expr
      { }
   |  expr '<' expr
      { }
   |  expr '<=' expr
      { }
   |  expr '>' expr
      { }
   |  expr '>=' expr
      { }
   |  expr 'instanceof' class_name_reference
      { }
   |  parenthesis_expr
      { }
   |  new_expr
      { }
   |  '(' new_expr ')' instance_call
      { }
   |  expr '?' expr ':' expr
      { }
   |  expr '?' ':' expr
      { }
   |  internal_functions_in_yacc
      { }
   |  '(int)' expr
      { }
   |  '(double)' expr
      { }
   |  '(string)' expr
      { }
   |  '(array)' expr
      { }
   |  '(object)' expr
      { }
   |  '(bool)' expr
      { }
   |  '(unset)' expr
      { }
   |  'exit' exit_expr
      { }
   |  '@' expr
      { }
   |  scalar
      { }
   |  combined_scalar_offset
      { }
   |  combined_scalar
      { }
   |  '`' backticks_expr '`'
      { }
   |  'print' expr
      { }
   |  'yield'
      { }
   |  function is_reference '(' parameter_list ')' lexical_vars '{' inner_statement_list '}'
      { }
   |  'static' function is_reference '(' parameter_list ')' lexical_vars '{' inner_statement_list '}'
      { }

yield_expr :: { }
   :  'yield' expr
      { }
   |  'yield' expr '=>' expr
      { }

combined_scalar_offset :: { }
   :  combined_scalar '[' dim_offset ']'
      { }
   |  combined_scalar_offset '[' dim_offset ']'
      { }
   |  LT_STRING '[' dim_offset ']'
      { }

combined_scalar :: { }
   :  'array' '(' array_pair_list ')'
      { }
   |  '[' array_pair_list ']'
      { }

function :: { }
   :  'function'
      { }

lexical_vars :: { }
   :  {- empty -}
      { }
   |  'use' '(' lexical_var_list ')'
      { }

lexical_var_list :: { }
   :  lexical_var_list ',' LT_VARNAME
      { }
   |  lexical_var_list ',' '&' LT_VARNAME
      { }
   |  LT_VARNAME
      { }
   |  '&' LT_VARNAME
      { }

function_call :: { }
   :  namespace_name function_call_parameter_list
      { }
   |  'namespace' '\\' namespace_name function_call_parameter_list
      { }
   |  '\\' namespace_name function_call_parameter_list
      { }
   |  class_name '::' variable_name function_call_parameter_list
      { }
   |  class_name '::' variable_without_objects function_call_parameter_list
      { }
   |  variable_class_name '::' variable_name function_call_parameter_list
      { }
   |  variable_class_name '::' variable_without_objects function_call_parameter_list
      { }
   |  variable_without_objects function_call_parameter_list
      { }

class_name :: { }
   :  'static'
      { }
   |  namespace_name
      { }
   |  'namespace' '\\' namespace_name
      { }
   |  '\\' namespace_name
      { }

fully_qualified_class_name :: { }
   :  namespace_name
      { }
   |  'namespace' '\\' namespace_name
      { }
   |  '\\' namespace_name
      { }

class_name_reference :: { }
   :  class_name
      { }
   |  dynamic_class_name_reference
      { }

dynamic_class_name_reference :: { }
   :  base_variable '->' object_property dynamic_class_name_variable_properties
      { }
   |  base_variable
      { }

dynamic_class_name_variable_properties :: { }
   :  dynamic_class_name_variable_properties dynamic_class_name_variable_property
      { }
   |  {- empty -}
      { }

dynamic_class_name_variable_property :: { }
   :  '->' object_property
      { }

exit_expr :: { }
   :  {- empty -}
      { }
   |  '(' ')'
      { }
   |  parenthesis_expr
      { }

backticks_expr :: { }
   :  {- empty -}
      { }
   |  LT_STRING
      { }
   |  encaps_list
      { }

ctor_arguments :: { }
   :  {- empty -}
      { }
   |  function_call_parameter_list
      { }

common_scalar :: { }
   :  LT_INTEGER
      { }
   |  LT_DOUBLE
      { }
   |  LT_STRING
      { }
   |  '__LINE__'
      { }
   |  '__FILE__'
      { }
   |  '__DIR__'
      { }
   |  '__TRAIT__'
      { }
   |  '__METHOD__'
      { }
   |  '__FUNCTION__'
      { }
   |  '__NAMESPACE__'
      { }
   |  LT_HEREDOC_START LT_STRING LT_HEREDOC_END
      { }
   |  LT_HEREDOC_START LT_HEREDOC_END
      { }

static_scalar :: { }
   :  common_scalar
      { }
   |  static_class_name_scalar
      { }
   |  namespace_name
      { }
   |  'namespace' '\\' namespace_name
      { }
   |  '\\' namespace_name
      { }
   |  '+' static_scalar
      { }
   |  '-' static_scalar
      { }
   |  'array' '(' static_array_pair_list ')'
      { }
   |  '[' static_array_pair_list ']'
      { }
   |  static_class_constant
      { }
   |  '__CLASS__'
      { }

static_class_constant :: { }
   :  class_name '::' LT_IDENT
      { }

scalar :: { }
   :  LT_VARNAME_IMBED
      { }
   |  class_name_scalar
      { }
   |  class_constant
      { }
   |  namespace_name
      { }
   |  'namespace' '\\' namespace_name
      { }
   |  '\\' namespace_name
      { }
   |  common_scalar
      { }
   |  '"' encaps_list '"'
      { }
   |  '"' LT_STRING '"'
      { }
   |  LT_HEREDOC_START encaps_list LT_HEREDOC_END
      { }
   |  '__CLASS__'
      { }

static_array_pair_list :: { }
   :  {- empty -}
      { }
   |  non_empty_static_array_pair_list possible_comma
      { }

possible_comma :: { }
   :  {- empty -}
      { }
   |  ','
      { }

non_empty_static_array_pair_list :: { }
   :  non_empty_static_array_pair_list ',' static_scalar '=>' static_scalar
      { }
   |  non_empty_static_array_pair_list ',' static_scalar
      { }
   |  static_scalar '=>' static_scalar
      { }
   |  static_scalar
      { }

parenthesis_expr :: { }
   :  '(' expr ')'
      { }
   |  '(' yield_expr ')'
      { }

r_variable :: { }
   :  variable
      { }

w_variable :: { }
   :  variable
      { }

rw_variable :: { }
   :  variable
      { }

variable :: { }
   :  base_variable_with_function_calls '->' object_property method_or_not variable_properties
      { }
   |  base_variable_with_function_calls
      { }

variable_properties :: { }
   :  variable_properties variable_property
      { }
   |  {- empty -}
      { }

variable_property :: { }
   :  '->' object_property method_or_not
      { }

array_method_dereference :: { }
   :  array_method_dereference '[' dim_offset ']'
      { }
   |  method '[' dim_offset ']'
      { }

method :: { }
   :  function_call_parameter_list
      { }

method_or_not :: { }
   :  method
      { }
   |  array_method_dereference
      { }
   |  {- empty -}
      { }

variable_without_objects :: { }
   :  reference_variable
      { }
   |  simple_indirect_reference reference_variable
      { }

static_member :: { }
   :  class_name '::' variable_without_objects
      { }
   |  variable_class_name '::' variable_without_objects
      { }

variable_class_name :: { }
   :  reference_variable
      { }

array_function_dereference :: { }
   :  array_function_dereference '[' dim_offset ']'
      { }
   |  function_call '[' dim_offset ']'
      { }

base_variable_with_function_calls :: { }
   :  base_variable
      { }
   |  array_function_dereference
      { }
   |  function_call
      { }

base_variable :: { }
   :  reference_variable
      { }
   |  simple_indirect_reference reference_variable
      { }
   |  static_member
      { }

reference_variable :: { }
   :  reference_variable '[' dim_offset ']'
      { }
   |  reference_variable '{' expr '}'
      { }
   |  compound_variable
      { }

compound_variable :: { }
   :  LT_VARNAME
      { }
   |  '$' '{' expr '}'
      { }

dim_offset :: { }
   :  {- empty -}
      { }
   |  expr
      { }

object_property :: { }
   :  object_dim_list
      { }
   |  variable_without_objects
      { }

object_dim_list :: { }
   :  object_dim_list '[' dim_offset ']'
      { }
   |  object_dim_list '{' expr '}'
      { }
   |  variable_name
      { }

variable_name :: { }
   :  LT_IDENT
      { }
   |  '{' expr '}'
      { }

simple_indirect_reference :: { }
   :  '$'
      { }
   |  simple_indirect_reference '$'
      { }

assignment_list :: { }
   :  assignment_list ',' assignment_list_element
      { }
   |  assignment_list_element
      { }

assignment_list_element :: { }
   :  variable
      { }
   |  'list' '(' assignment_list ')'
      { }
   |  {- empty -}
      { }

array_pair_list :: { }
   :  {- empty -}
      { }
   |  non_empty_array_pair_list possible_comma
      { }

non_empty_array_pair_list :: { }
   :  non_empty_array_pair_list ',' expr '=>' expr
      { }
   |  non_empty_array_pair_list ',' expr
      { }
   |  expr '=>' expr
      { }
   |  expr
      { }
   |  non_empty_array_pair_list ',' expr '=>' '&' w_variable
      { }
   |  non_empty_array_pair_list ',' '&' w_variable
      { }
   |  expr '=>' '&' w_variable
      { }
   |  '&' w_variable
      { }

encaps_list :: { }
   :  encaps_list encaps_var
      { }
   |  encaps_list LT_STRING
      { }
   |  encaps_var
      { }
   |  LT_STRING encaps_var
      { }

encaps_var :: { }
   :  LT_VARNAME
      { }
   |  LT_VARNAME '[' encaps_var_offset ']'
      { }
   |  LT_VARNAME '->' LT_IDENT
      { }
   |  '${' expr '}'
      { }
   |  '${' LT_VARNAME_IMBED '[' expr ']' '}'
      { }
   |  '{' variable '}'
      { }

encaps_var_offset :: { }
   :  LT_IDENT
      { }
   |  LT_INTEGER
      { }
   |  LT_VARNAME
      { }

internal_functions_in_yacc :: { }
   :  'isset' '(' isset_variables ')'
      { }
   |  'empty' '(' expr ')'
      { }
   |  'include' expr
      { }
   |  'include_once' expr
      { }
   |  'eval' '(' expr ')'
      { }
   |  'require' expr
      { }
   |  'require_once' expr
      { }

isset_variables :: { }
   :  expr
      { }
   |  isset_variables ',' expr
      { }

class_constant :: { }
   :  class_name '::' LT_IDENT
      { }
   |  variable_class_name '::' LT_IDENT
      { }

static_class_name_scalar :: { }
   :  class_name '::' 'class'
      { }

class_name_scalar :: { }
   :  class_name '::' 'class'
      { }
{

identToken (Ctx _ (IdentToken str)) = Identifier str

happyError :: P a
happyError = lexError
           
}
