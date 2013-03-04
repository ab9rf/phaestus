{
module PHPParse ( phpParse ) where

import PHPLex (AlexState, Token(..), mLexer, P)
import ParseTree 

}

%name phpParse start

%lexer { mLexer } { EOF }
%monad { P }

%tokentype { Token }

%token INLINE_HTML   { InlineHTML $$ }

%token T_INT_CAST   { CastInt }
%token T_DOUBLE_CAST   { CastReal }
%token T_STRING_CAST   { CastString }
%token T_ARRAY_CAST   { CastArray }
%token T_OBJECT_CAST   { CastObject }
%token T_BOOL_CAST   { CastBool }
%token T_UNSET_CAST   { CastUnset }

%token '=='   { OpEqEq }
%token '==='   { OpEqEqEq }
%token '!='   { OpNotEq }
%token '!=='   { OpNotEqEq }
%token '<='   { OpLE }
%token '>='   { OpGE }
%token '++'   { OpInc }
%token '--'   { OpDec }
%token '=>'   { OpDoubleArrow }
%token '->'   { OpSingleArrow }
%token '<<'   { OpSL }
%token '>>'   { OpSR }
%token '+='   { OpPlusEq }
%token '-='   { OpMinusEq }
%token '*='   { OpMultEq }
%token '/='   { OpDivEq }
%token '.='   { OpConcatEq }
%token '%='   { OpModEq }
%token '&='   { OpAndEq }
%token '|='   { OpOrEq }
%token '^='   { OpXorEq }
%token '<<='   { OpSLEq }
%token '>>='   { OpSREq }
%token '::'   { OpColonColon }
%token '&&'   { OpLogicAnd }
%token '||'   { OpLogicOr }
%token '+'   { OpPlus }
%token '-'   { OpMinus }
%token '/'   { OpSlash }
%token '*'   { OpStar }
%token '%'   { OpPercent }
%token '^'   { OpCaret }
%token '&'   { OpAmpersand }
%token '|'   { OpPipe }
%token '~'   { OpTilde }
%token '='   { OpEq }
%token '<'   { OpLt }
%token '>'   { OpGt }
%token '.'   { OpDot }
%token '!'   { OpBang }
%token ','   { OpComma }
%token '?'   { OpQuestion }
%token ':'   { OpColon }
%token '@'   { OpAtSign }
%token '$'   { OpDollars }
%token '\\'   { Backslash }
%token '`'   { Backquote }
%token '"'   { DoubleQuote }

%token '${'   { DollarOpenCurlyBrace }

%token ';'   { Semicolon }
%token '('   { LParen }
%token ')'   { RParen }
%token '{'   { LBrace }
%token '}'   { RBrace }
%token '['   { LBracket }
%token ']'   { RBracket }

%token T_END_HEREDOC   { EndHeredoc }
%token T_START_HEREDOC  { StartHeredoc }

%token 'and'   { KeywordAnd }
%token 'or'   { KeywordOr }
%token 'xor'   { KeywordXor }
%token T_FILE { Keyword__FILE__ }
%token T_LINE { Keyword__LINE__ }
%token T_DIR  { Keyword__DIR__ }
%token T_ARRAY   { KeywordArray }
%token T_AS   { KeywordAs }
%token T_BREAK   { KeywordBreak }
%token T_CASE   { KeywordCase }
%token T_CLASS   { KeywordClass }
%token T_CONST   { KeywordConst }
%token T_CONTINUE { KeywordContinue }
%token T_DECLARE   { KeywordDeclare }
%token T_DEFAULT   { KeywordDefault }
%token T_DO   { KeywordDo }
%token T_ECHO   { KeywordEcho }
%token T_ELSE   { KeywordElse }
%token T_ELSEIF   { KeywordElseif }
%token T_EMPTY   { KeywordEmpty }
%token T_ENDDECLARE { KeywordEnddeclare }
%token T_ENDFOR   { KeywordEndfor }
%token T_ENDFOREACH { KeywordEndforeach }
%token T_ENDIF   { KeywordEndif }
%token T_ENDSWITCH  { KeywordEndswitch }
%token T_ENDWHILE { KeywordEndwhile }
%token T_EVAL   { KeywordEval }
%token T_EXIT   { KeywordExit }
%token T_EXTENDS   { KeywordExtends }
%token T_FOR   { KeywordFor }
%token T_FOREACH   { KeywordForeach }
%token T_FUNCTION { KeywordFunction }
%token T_GLOBAL   { KeywordGlobal }
%token T_IF   { KeywordIf }
%token T_INCLUDE   { KeywordInclude }
%token T_INCLUDE_ONCE   { KeywordIncludeOnce }
%token 'instanceof'   { KeywordInstanceOf }
%token T_ISSET   { KeywordIsset }
%token T_LIST   { KeywordList }
%token T_NEW   { KeywordNew }
%token T_PRINT   { KeywordPrint }
%token T_REQUIRE   { KeywordRequire }
%token T_REQUIRE_ONCE   { KeywordRequireOnce }
%token T_RETURN   { KeywordReturn }
%token T_STATIC   { KeywordStatic }
%token T_SWITCH   { KeywordSwitch }
%token T_UNSET   { KeywordUnset }
%token T_USE   { KeywordUse }
%token T_VAR   { KeywordVar }
%token T_WHILE   { KeywordWhile }
%token T_FUNC_C   { Keyword__FUNCTION__ }
%token T_CLASS_C   { Keyword__CLASS__ }
%token T_METHOD_C   { Keyword__METHOD__ }
%token T_FINAL   { KeywordFinal }
%token T_INTERFACE   { KeywordInterface }
%token T_IMPLEMENTS   { KeywordImplements }
%token T_PUBLIC   { KeywordPublic }
%token T_PRIVATE   { KeywordPrivate }
%token T_PROTECTED   { KeywordProtected }
%token T_ABSTRACT   { KeywordAbstract }
%token T_CLONE   { KeywordClone }
%token T_TRY   { KeywordTry }
%token T_CATCH   { KeywordCatch }
%token T_THROW   { KeywordThrow }
%token T_NAMESPACE   { KeywordNamespace }
%token T_GOTO   { KeywordGoto }
%token T_FINALLY   { KeywordFinally }
%token T_TRAIT   { KeywordTrait }
%token T_CALLABLE   { KeywordCallable }
%token T_INSTEADOF   { KeywordInsteadof }
%token T_YIELD   { KeywordYield }
%token T_TRAIT_C   { Keyword__TRAIT__ }
%token T_NS_C   { Keyword__NAMESPACE__ }

%token T_VARIABLE       { VariableToken $$ }
%token IDENT            { IdentToken $$ }
%token T_VARIABLE_STR   { VariableTokenInStr $$ }

%token T_LNUMBER        { IntegerToken $$ }
%token T_DNUMBER        { RealToken $$ }
%token T_STRING_CONST   { StringToken $$ }

%left T_INCLUDE T_INCLUDE_ONCE T_EVAL T_REQUIRE T_REQUIRE_ONCE
%left ','
%left 'or'
%left 'xor'
%left 'and'
%right T_PRINT
%right T_YIELD
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
%right '~' '++' '--' T_INT_CAST T_DOUBLE_CAST T_STRING_CAST T_ARRAY_CAST T_OBJECT_CAST T_BOOL_CAST T_UNSET_CAST '@'
%right '['
%nonassoc T_NEW T_CLONE
%left T_ELSEIF
%left T_ELSE 
%left T_ENDIF 
%right T_STATIC T_ABSTRACT T_FINAL T_PRIVATE T_PROTECTED T_PUBLIC

%expect 3

%%

start :: { [PHPStatement] }
   :  top_statement_list
            { reverse $1 }
;

top_statement_list :: { [PHPStatement] }
   :  top_statement_list top_statement
            { $2:$1 }
   |  {- empty -}
            { [] }
;

namespace_name :: { [PHPIdent] }
   :  IDENT
            { [$1] }
   |  namespace_name '\\' IDENT
            { $3:$1 }
;

top_statement :: { [PHPStatement] }
   :  statement
            { $1 }
   |  function_declaration_statement
            { $1 }
   |  class_declaration_statement
            { $1 }
   |  T_NAMESPACE namespace_name ';'
            { PHPChangeNamespace $2 }
   |  T_NAMESPACE namespace_name '{' top_statement_list '}'
            { PHPNamespace $2 (reverse $4) }
   |  T_NAMESPACE '{'   top_statement_list '}'
            { PHPNamespace [] (reverse $3) }
   |  T_USE use_declarations ';'
            { PHPUseDeclaration (reverse $2) }
   |  constant_declaration ';'
            { PHPConstantDeclarations (reverse $1) }
;

use_declarations :: { [(PHPQualifiedIdentifier, PHPIdent)] }
   :  use_declarations ',' use_declaration
            { ($3:$1) }
   |  use_declaration
            { [$1] }   
;

use_declaration :: { (PHPQualifiedIdentifier, PHPIdent) }
   :  namespace_name
            { (namespaceRelative $1, head $1)}   
   |  namespace_name T_AS IDENT
            { (namespaceRelative $1, $3) }
   |  '\\' namespace_name
            { (namespaceAbsolute $2, head $2)}
   |  '\\' namespace_name T_AS IDENT
            { (namespaceAbsolute $2, $4) }
;

constant_declaration :: { [(PHPIdent, PHPScalar)] }
   :  constant_declaration ',' IDENT '=' static_scalar
            { ($3,$5):$1 }
   |  T_CONST IDENT '=' static_scalar
            { ($2,$4):[] }
;

inner_statement_list :: { [PHPStatement] }
   :  inner_statement_list inner_statement
            { $2:$1 }
   |
            {- empty -}
            { [] }   
;

inner_statement :: { PHPStatement }
   :  statement
            { $1 }
   |  function_declaration_statement
            { $1 }
   |  class_declaration_statement
            { $1 }
;

statement :: { PHPStatement }
   :  unticked_statement
            { $1 } 
   |  IDENT ':'
            { PHPLabelDecl $1 }  
;

unticked_statement :: { PHPStatement }
   :  '{' inner_statement_list '}'
            { PHPStatementGroup $2 }
   |  T_IF parenthesis_expr  statement  elseif_list else_single
            { PHPIf ($2,$3):(reverse $4) $5 } 
   |  T_IF parenthesis_expr ':' inner_statement_list new_elseif_list new_else_single T_ENDIF ';'
            { PHPIf ($2,(StatementGroup (reverse $4))):(reverse $5) $6 } 
   |  T_WHILE  parenthesis_expr  while_statement
            { PHPWhile $2 $3 } 
   |  T_DO  statement T_WHILE  parenthesis_expr ';'
            { PHPDo $2 $4 }
   |  T_FOR '(' for_expr ';' for_expr ';' for_expr ')' for_statement
            { PHPFor $3 $5 $7 $9 }
   |  T_SWITCH parenthesis_expr switch_case_list
            { PHPSwitch $2 (reverse $3) }
   |  T_BREAK ';'
            { PHPBreak Nothing  }
   |  T_BREAK expr ';'
            { PHPBreak (Just $2) }
   |  T_CONTINUE ';'
            { PHPContinue Nothing }   
   |  T_CONTINUE expr ';'
            { PHPContinue (Just $2) }
   |  T_RETURN ';'
            { PHPReturn Nothing }   
   |  T_RETURN expr_without_variable ';'
            { PHPReturn (Just $2) }
   |  T_RETURN variable ';'
            { PHPReturn (Just $2) }
   |  yield_expr ';'
            { PHPYieldStmt $2 }
   |  T_GLOBAL global_var_list ';'
            { PHPGlobalStmt $2 }
   |  T_STATIC static_var_list ';'
            { PHPStaticStmt $2 }
   |  T_ECHO echo_expr_list ';'
            { PHPEcho $2 }
   |  INLINE_HTML
            { PHPInline $1 }
   |  expr ';'
            { PHPExprStmt $1 }   
   |  T_UNSET '(' unset_variables ')' ';'
            { PHPUnsetStmt (reverse $3) }
   |  T_FOREACH '(' variable T_AS foreach_variable foreach_optional_arg ')' foreach_statement
            { PHPForeach $3 $5 $6 $8 } 
   |  T_FOREACH '(' expr_without_variable T_AS foreach_variable foreach_optional_arg ')' foreach_statement
            { PHPForeach $3 $5 $6 $8 }
   |  T_DECLARE  '(' declare_list ')' declare_statement
            { PHPDeclare (reverse $3) $5 }
   |  ';'
            { }
   |  T_TRY  '{' inner_statement_list '}' catch_statement finally_statement
            { PHPTry $3 $5 $6 }
   |  T_THROW expr ';'
            { PHPThrow $2 }
   |  T_GOTO IDENT ';'
            { PHPGoto $2 }
;

catch_statement :: { [PHPCatch] }
   :  {- empty -}
            { [] }
   |  T_CATCH '('  fully_qualified_class_name T_VARIABLE ')' '{' inner_statement_list '}' additional_catches
            { (PHPCatch $3 $4 $7):(reverse $9) }  

finally_statement :: { [PHPStatement] }
   :  {- empty -}
            { [] } 
   |  T_FINALLY  '{' inner_statement_list '}'
            { $3 }   
;

additional_catches :: { [PHPCatch] }
   :  non_empty_additional_catches
            { $1 } 
   |  {- empty -}
            { [] }
;

non_empty_additional_catches :: { [PHPCatch] }
   :  additional_catch
            { [$1] } 
   |  non_empty_additional_catches additional_catch
            { $2:$1 }
;

additional_catch :: { PHPCatch }
   :  T_CATCH '(' fully_qualified_class_name  T_VARIABLE ')'  '{' inner_statement_list '}'
            { PHPCatch $3 $4 $7  } 
;

unset_variables :: { [PHPVariable] }
   :  unset_variable
            { [$1] }
   |  unset_variables ',' unset_variable
            { $3:$1 }
;

unset_variable :: { PHPVariable }
   :  variable
            { $1 }   
;

function_declaration_statement :: { PHPStatement }
   :  unticked_function_declaration_statement
            { $1 }   
;

class_declaration_statement :: { PHPStatement }
   :  unticked_class_declaration_statement
            { $1 }
;

is_reference :: { Boolean }
   :  {- empty -}
            { True }   
   |  '&'
            { False }
;

unticked_function_declaration_statement :: { PHPStatement }
   :  function is_reference IDENT '(' parameter_list ')' '{' inner_statement_list '}'
            { PHPFunctionDeclaration $2 $1 (reverse $4) (reverse $7) } 
;

unticked_class_declaration_statement :: { PHPStatement }
      class_entry_type IDENT extends_from implements_list '{' class_statement_list '}'
            { PHPClassDeclaration $2 $1 $3 $4 (reverse $6) }
   |  interface_entry IDENT interface_extends_list '{' class_statement_list '}'
            { PHPInterfaceDeclaration $2 $1 $3 (reverse $5) }
;


class_entry_type :: { PHPClassType }
   :  T_CLASS
            { PHPClassStandard }
   |  T_ABSTRACT T_CLASS
            { PHPClassAbstract }
   |  T_TRAIT
            { PHPClassTrait }
   |  T_FINAL T_CLASS
            { PHPClassFinal }
;

extends_from :: { Maybe PHPQualifiedIdent }
   :  {- empty -}
            { Nothing }   
   |  T_EXTENDS fully_qualified_class_name
            { Just $2 }
;

interface_entry :: { PHPInterfaceType }
   :  T_INTERFACE
            { PHPInterfaceStandard }
;

interface_extends_list :: { [PHPQualifiedIdent] }
   :  {- empty -}
            { [] }
   |  T_EXTENDS interface_list
            { reverse $2 }
;

implements_list :: { [PHPQualifiedIdent] }
   :  {- empty -}
            { [] }
   |  T_IMPLEMENTS interface_list
            { reverse $2 }
;

interface_list :: { [PHPQualifiedIdent] }
   : fully_qualified_class_name
            { [$1] }   
   |  interface_list ',' fully_qualified_class_name
            { $3:$1 }
;

foreach_optional_arg:: { Maybe PHPForeachArg }
   :    {- empty -}
            { Nothing }   
   |  '=>' foreach_variable
            { Just $2 }
;

foreach_variable:: { PHPForeachArg }
   :  variable
            { PHPForeachVar $1 }
   |  '&' variable
            { PHPForeachRef $2 }
   |  T_LIST '('  assignment_list ')'
            { PHPForeachList $3 }
;

for_statement :: { PHPStatement }
   :  statement
            { $1 }
   |  ':' inner_statement_list T_ENDFOR ';'
            { PHPStatementGroup $2 }
;


foreach_statement :: { PHPStatement }
   :  statement
            { $1 }
   |  ':' inner_statement_list T_ENDFOREACH ';'
            { StatementGroup $2 }
;


declare_statement :: { PHPStatement } 
   :  statement
            { $1 }
   |  ':' inner_statement_list T_ENDDECLARE ';'
            { StatementGroup $2 }
;


declare_list :: { [(PHPIdent, PHPScalar)] }
   :  IDENT '=' static_scalar
            { ($1,$3):[] }   
   |  declare_list ',' IDENT '=' static_scalar
            { ($3,$5):$1 }
;

switch_case_list :: { [PHPSwitchCase] }
   :  '{' case_list '}'
            { $2 }   
   |  '{' ';' case_list '}'
            { $3 }
   |  ':' case_list T_ENDSWITCH ';'
            { $2 }
   |  ':' ';' case_list T_ENDSWITCH ';'
            { $3 }
;

case_list :: { [PHPSwitchCase] }
   :  {- empty -}
            { [] }   
   |  case_list T_CASE expr case_separator  inner_statement_list
            { (PHPSwitchCase $3 $5):$1 }
   |  case_list T_DEFAULT case_separator  inner_statement_list
            { (PHPSwitchDefault $4):$1 }
;

case_separator :: { () }
   ':'
            { }
   |  ';'
            { }
;


while_statement :: { PHPStatement }
   :  statement
            { $1 }
   |  ':' inner_statement_list T_ENDWHILE ';'
            { StatementGroup $2 }
;

elseif_list :: { [(PHPExpr,PHPStatement)] }
   :  {- empty -}
            { [] }
   |  elseif_list T_ELSEIF parenthesis_expr  statement
            { ($3,$4):$1 }
;

new_elseif_list :: { [(PHPExpr,PHPStatement)] }
   :   {- empty -}
            { [] }
   |  new_elseif_list T_ELSEIF parenthesis_expr ':'  inner_statement_list
            { ($3,(StatementGroup $5)):$1 }
;

else_single :: { Maybe PHPStatement }
   :  {- empty -}
            { Nothing }
   |  T_ELSE statement
            { Just $2 }
;

new_else_single :: { Maybe PHPStatement }
   :  {- empty -}
            { Nothing }
   |  T_ELSE ':' inner_statement_list
            { Just (StatementGroup $3) }
;


parameter_list :: { [PHPFormalParameter] }
   :  non_empty_parameter_list
            { reverse $1 }
   |  {- empty -}
            { [] }
;


non_empty_parameter_list :: { [PHPFormalParameter] }
   :  optional_class_type T_VARIABLE
            { [PHPFormalParameter $2 False $1 Nothing] }   
   |  optional_class_type '&' T_VARIABLE
            { [PHPFormalParameter $3 True $1 Nothing] }
   |  optional_class_type '&' T_VARIABLE '=' static_scalar
            { [PHPFormalParameter $3 True $1 (Just $5)] }
   |  optional_class_type T_VARIABLE '=' static_scalar
            { [PHPFormalParameter $2 False $1 (Just $4)] }
   |  non_empty_parameter_list ',' optional_class_type T_VARIABLE
            { (PHPFormalParameter $4 False $3 Nothing):$1 }
   |  non_empty_parameter_list ',' optional_class_type '&' T_VARIABLE
            { (PHPFormalParameter $5 True $3 Nothing):$1 }
   |  non_empty_parameter_list ',' optional_class_type '&' T_VARIABLE   '=' static_scalar
            { (PHPFormalParameter $5 True $3 (Just $7)):$1 }
   |  non_empty_parameter_list ',' optional_class_type T_VARIABLE '=' static_scalar
            { (PHPFormalParameter $5 False $3 (Just $6)):$1 }
;

optional_class_type :: { Maybe PHPParameterType }
   :  {- empty -}
            { Nothing }   
   |  T_ARRAY
            { Just PHPTypeArray }   
   |  T_CALLABLE
            { Just PHPTypeCallable }   
   |  fully_qualified_class_name
            { Just (PHPTypeClass $1) }   
;

function_call_parameter_list :: { [PHPActualParameter] }
   :  '(' ')'
            { [] }
   |  '(' non_empty_function_call_parameter_list ')'
            { (reverse $2) }
   |  '(' yield_expr ')'
            { [$2] }
;

non_empty_function_call_parameter_list :: { [PHPActualParameter] }
   :  expr_without_variable
            { [PHPActualParameter $1] }
   |  variable
            { [PHPActualParameter (PHPVariableInExpr $1)] }
   |  '&' w_variable
            { [PHPActualRefParameter $2] }
   |  non_empty_function_call_parameter_list ',' expr_without_variable
            { (PHPActualParameter $3):$1 }   
   |  non_empty_function_call_parameter_list ',' variable
            { (PHPActualParameter (PHPVariableInExpr $3)):$1 }   
   |  non_empty_function_call_parameter_list ',' '&' w_variable
            { (PHPActualRefParameter $4):$1 }   
;

global_var_list :: { [PHPGlobalVarSpec] }
   :  global_var_list ',' global_var
            { $3:$1 }
   |  global_var
            { [$1] }   
;


global_var :: { PHPGlobalVarSpec }
   :  T_VARIABLE
            { PHPGlobalVar $1 }   
   |  '$' r_variable
            { PHPIndirectGlobalVar $2 }
   |  '$' '{' expr '}'
            { PHPIndirectGlobalVar $3 }
;


static_var_list :: { [(PHPVariableToken, Maybe PHPScalar)] }
   :  static_var_list ',' T_VARIABLE
            { ($3,Nothing):$1 }
   |  static_var_list ',' T_VARIABLE '=' static_scalar
            { ($3,Just $5):$1 }
   |  T_VARIABLE
            { ($1,Nothing):[] }
   |  T_VARIABLE '=' static_scalar
            { ($1,Just $3):[] }
;


class_statement_list :: { [PHPClassStatement] }
   :  class_statement_list class_statement
            { $2:$1 }
   |  {- empty -}
            { [] }
;

class_statement :: { PHPClassStatement }
   :  variable_modifiers  class_variable_declaration ';'
            { PHPClassVariableDeclaration $1 $2 }
   |  class_constant_declaration ';'
            { PHPClassConstantDeclaration $1 }
   |  trait_use_statement
            { $1 }
   |  method_modifiers function is_reference IDENT '(' parameter_list ')' method_body
            { PHPMethodDeclaration $2 $1 $3 $4 $6 $8 } 
;

trait_use_statement :: { PHPClassStatement }
   :  T_USE trait_list trait_adaptations
            { PHPTraitUseStatement (reverse $2) $3 }
;

trait_list :: { [PHPQualifiedIdent] } 
   fully_qualified_class_name
            { [$1] }   
   |  trait_list ',' fully_qualified_class_name
            { $3:$1 }   
;

trait_adaptations :: { [PHPTraitAdaptationStatement] }
   :  ';'
            { [] }
   |  '{' trait_adaptation_list '}'
            { $2 }
;

trait_adaptation_list :: { [PHPTraitAdaptationStatement] }
   :  {- empty -}
            { [] }
   |  non_empty_trait_adaptation_list
            { reverse $1 }
;

non_empty_trait_adaptation_list :: { [PHPTraitAdaptationStatement] }
   :  trait_adaptation_statement
            { [$1] }
   |  non_empty_trait_adaptation_list trait_adaptation_statement
            { $2:$1 }
;

trait_adaptation_statement :: { PHPTraitAdaptationStatement }
      trait_precedence ';'
            { $1 }
   |  trait_alias ';'
            { $1 }
;

trait_precedence :: { PHPTraitAdaptationStatement }
   :  trait_method_reference_fully_qualified T_INSTEADOF trait_reference_list
            { PHPTraitPrecedence $1 $3 }   
;

trait_reference_list :: { PHPTraitAdaptationStatement }
   :  fully_qualified_class_name
            { [$1] }   
   |  trait_reference_list ',' fully_qualified_class_name
            { $3:$1 }   
;

trait_method_reference:
      IDENT
            { TraitMethodReference $1 }   
   |  trait_method_reference_fully_qualified
            { $1 }   
;

trait_method_reference_fully_qualified:
   fully_qualified_class_name '::' IDENT
            { TraitMethodReferenceFQ $1 $3 } 
;

trait_alias:
      trait_method_reference T_AS trait_modifiers IDENT
            { TraitAliasTrait $1 $3 $4 }   
   |  trait_method_reference T_AS member_modifier
            { TraitAliasMember $1 $3 }   
;

trait_modifiers:
      {- empty -}
            { Nothing }   
   |  member_modifier
            { Just $1 }
;

method_body:
   ';'
            { Nothing }   
   |  '{' inner_statement_list '}'
            { Just $2 }
;

variable_modifiers:
   non_empty_member_modifiers
            { reverse $1 }   
   |  T_VAR
            { [] }   
;

method_modifiers:
            {- empty -}
            { [] }   
   |  non_empty_member_modifiers
            { reverse $1 }
;

non_empty_member_modifiers:
   member_modifier
            { [$1] }   
   |  non_empty_member_modifiers member_modifier
            { $2:$1 }
;

member_modifier:
   T_PUBLIC
            { ModifierPublic }   
   |  T_PROTECTED
            { ModifierProtected }   
   |  T_PRIVATE
            { ModifierPrivate }   
   |  T_STATIC
            { ModifierStatic }   
   |  T_ABSTRACT
            { ModifierAbstract }   
   |  T_FINAL
            { ModifierFinal }   
;

class_variable_declaration:
   class_variable_declaration ',' T_VARIABLE
            { (ClassVariableDecl $3):$1 }   
   |  class_variable_declaration ',' T_VARIABLE '=' static_scalar
            { (ClassVariableDeclWithInit $3 $5):$1 }
   |  T_VARIABLE
            { [ClassVariableDecl $1] }
   |  T_VARIABLE '=' static_scalar
            { [ClassVariableDeclWithInit $1 $3] }
;

class_constant_declaration:
   class_constant_declaration ',' IDENT '=' static_scalar
            { (ClassConstantDecl $3 $5):$1 }
   |  T_CONST IDENT '=' static_scalar
            { [ClassConstantDecl $1 $3] }
;

echo_expr_list:
   echo_expr_list ',' expr
            { $3:$1 }
   |  expr
            { [$1] }   
;


for_expr:
            {- empty -}
            { [] }
   |  non_empty_for_expr
            { reverse $1 }
;

non_empty_for_expr:
   non_empty_for_expr ','   expr
            { $3:$1 }
   |  expr
            { [$1] }   
;

chaining_method_or_property:
   chaining_method_or_property variable_property
            { $2:$1 }
   |  variable_property
            { [$1] }   
;

chaining_dereference:
   chaining_dereference '[' dim_offset ']'
            { $3:$1 }
   |  '[' dim_offset ']'
            { [$2] }
;

chaining_instance_call:
   chaining_dereference chaining_method_or_property
            { ChainingInstanceCall (Just $1) (reverse $2) } 
   |  chaining_dereference
            { ChainingInstanceCall (Just $1) [] }
   |  chaining_method_or_property
            { ChainingInstanceCall Nothing (reverse $1) }
;

instance_call:
      {- empty -}
            { Nothing }   
   |  chaining_instance_call
            { Just $1 }
;

new_expr:
   T_NEW class_name_reference  ctor_arguments
            { New $1 $2 }
;

expr_without_variable :: { PHPExpr }
   :  T_LIST '('  assignment_list ')' '=' expr
            { PHPListAssignment $3 $5 }
   |  variable '=' expr
            { PHPAssignment $1 $3 }
   |  variable '=' '&' variable
            { PHPRefAssignment $1 $4 }
   |  variable '=' '&' T_NEW class_name_reference  ctor_arguments
            { PHPRefAssignmentFromNew $1 $5 $6 }
   |  T_CLONE expr
            { PHPClone $2 }
   |  variable '+=' expr
            { PHPAddInto $1 $3 }
   |  variable '-=' expr
            { PHPSubtractInto $1 $3 }
   |  variable '*=' expr
            { PHPMultiplyInto $1 $3 }   
   |  variable '/=' expr
            { PHPDivideInto $1 $3 }   
   |  variable '.=' expr
            { PHPConcatInto $1 $3 }
   |  variable '%=' expr
            { PHPModulusInto $1 $3 }   
   |  variable '&=' expr
            { PHPAndInto $1 $3 }   
   |  variable '|=' expr
            { PHPOrInto $1 $3 }   
   |  variable '^=' expr
            { PHPXorInto $1 $3 }   
   |  variable '<<=' expr
            { PHPShiftLeftInto $1 $3 }
   |  variable '>>=' expr
            { PHPShiftRightInto $1 $3 }
   |  rw_variable '++'
            { PHPPostincrement $2 }
   |  '++' rw_variable
            { PHPPreincrement $2 }
   |  rw_variable '--'
            { PHPPostdecrement $2 } 
   |  '--' rw_variable
            { PHPPredecrement $2 }
   |  expr '||'  expr
            { PHPBooleanOr $1 $3 }
   |  expr '&&' expr
            { PHPBooleanAnd $1 $3 } 
   |  expr 'or'  expr
            { PHPLogicalOr $1 $3 }
   |  expr 'and' expr
            { PHPLogicalAnd $1 $3 }
   |  expr 'xor' expr
            { PHPLogicalXor $1 $3 }
   |  expr '|' expr
            { PHPBinaryOr $1 $3 }
   |  expr '&' expr
            { PHPBinaryAnd $1 $3 }
   |  expr '^' expr
            { PHPBinaryXor $1 $3 }
   |  expr '.' expr
            { PHPConcat $1 $3 }
   |  expr '+' expr
            { PHPAdd $1 $3 }
   |  expr '-' expr
            { PHPSubtract $1 $3 }
   |  expr '*' expr
            { PHPMultiply $1 $3 }
   |  expr '/' expr
            { PHPDivide $1 $3 }
   |  expr '%' expr
            { PHPModulus $1 $3 }
   |  expr '<<' expr
            { PHPShiftLeft $1 $3 }
   |  expr '>>' expr
            { PHPShiftRight $1 $3 }
   |  '+' expr %prec '++'
            { PHPUnaryPlus $2 }
   |  '-' expr %prec '--'
            { PHPUnaryMinus $2 }
   |  '!' expr
            { PHPLogicalNot $2 }
   |  '~' expr
            { PHPBinaryNegate $2 }
   |  expr '===' expr
            { PHPIsIdentical $1 $3 }   
   |  expr '!==' expr
            { PHPIsNotIdentical $1 $3 }
   |  expr '==' expr
            { PHPIsEqual $1 $3 }   
   |  expr '!=' expr
            { PHPIsNotEqual $1 $3 }   
   |  expr '<' expr
            { PHPLessThan $1 $3 }   
   |  expr '<=' expr
            { PHPLessThanOrEqual $1 $3 }
   |  expr '>' expr
            { PHPGreaterThan $1 $3 }   
   |  expr '>=' expr
            { PHPGreaterThanOrEqual $1 $3 }
   |  expr 'instanceof' class_name_reference
            { PHPInstanceOf $1 $3 }
   |  parenthesis_expr
            { $1 }
   |  new_expr
            { $1 }
   |  '(' new_expr ')'  instance_call
            { PHPInstanceCall $2 $4 }
   |  expr '?' expr ':' expr
            { PHPTernaryOp $1 (Just $3) $5 }   
   |  expr '?' ':' expr
            { PHPTernaryOp $1 Nothing $4 }
   |  internal_functions_in_yacc
            { $1 }
   |  T_INT_CAST expr
            { PHPIntCast $2 }
   |  T_DOUBLE_CAST expr
            { PHPDoubleCast $2 }
   |  T_STRING_CAST expr
            { PHPStringCast $2 }
   |  T_ARRAY_CAST expr
            { PHPArrayCast $2 }
   |  T_OBJECT_CAST expr
            { PHPObjectCast $2 }
   |  T_BOOL_CAST expr
            { PHPBoolCast $2 }
   |  T_UNSET_CAST expr
            { PHPUnsetCast $2 }
   |  T_EXIT exit_expr
            { PHPExit $2 }
   |  '@'  expr
            { PHPDisableErrors $2 }
   |  scalar
            { PHPScalarExpr $1 }
   |  combined_scalar_offset
            { PHPScalarExpr $1 }
   |  combined_scalar
            { PHPScalarExpr $1 }
   |  '`' backticks_expr '`'
            { PHPBacktick $2 }
   |  T_PRINT expr
            { PHPPrint $2 }
   |  T_YIELD
            { PHPYield0 }
   |  function is_reference '(' parameter_list ')' lexical_vars '{' inner_statement_list '}'
            { PHPAnonymousFunction $2 $4 $6 $8 }
   |  T_STATIC function is_reference '(' parameter_list ')' lexical_vars '{' inner_statement_list '}'
            { PHPAnonymousStaticFunction $3 $5 $7 $9 }
;

yield_expr :: { PHPExpr }
   :  T_YIELD expr_without_variable
            { PHPYield1 $2 } 
   |  T_YIELD variable
            { PHPYield1 $2 }
   |  T_YIELD expr '=>' expr_without_variable
            { PHPYield2 $2 $4 } 
   |  T_YIELD expr '=>' variable
            { PHPYield2 $2 $4 } 
;

combined_scalar_offset:
   combined_scalar '[' dim_offset ']'
            { ScalarWithOffset (SWOArray $1) [$3] }
   | combined_scalar_offset '[' dim_offset ']'
            { (\(CombinedScalarWithOffset x [y]) -> CombinedScalarWithOffset x [$3:y]) $1 }
   | T_STRING_CONST '[' dim_offset ']'
            { ScalarWithOffset (SWOString $1) [$3] }

combined_scalar:
   T_ARRAY '(' array_pair_list ')'
            { $3 }
   | '[' array_pair_list ']'
            { $3 }

function:
   T_FUNCTION
            { }
;

lexical_vars :: { [PHPLexicalVariable] }
   :  {- empty -}
            { [] }
   |  T_USE '(' lexical_var_list ')'
            { reverse $3 }
;

lexical_var_list :: { [PHPLexicalVariable] }
   :  lexical_var_list ',' T_VARIABLE
            { (PHPLexicalVariable $3):$1 }   
   |  lexical_var_list ',' '&' T_VARIABLE
            { (PHPLexicalVariableRef $4):$1 }   
   |  T_VARIABLE
            { [PHPLexicalVariable $1] }   
   |  '&' T_VARIABLE
            { [PHPLexicalVariableRef $2] }   
;

function_call:
   namespace_name function_call_parameter_list
            { PHPFunctionCall (namespaceRelative $1) $2 }
   |  T_NAMESPACE '\\' namespace_name function_call_parameter_list
            { PHPFunctionCall (namespaceSelf $3) $4 }
   |  '\\' namespace_name function_call_parameter_list
            { PHPFunctionCall (namespaceAbsolute $2) $3 }
   |  class_name '::' variable_name function_call_parameter_list
            { ClassFunctionCall $1 $3 $4 }
   |  class_name '::' variable_without_objects function_call_parameter_list
            { ClassFunctionCall $1 $3 $4 }
   |  variable_class_name '::' variable_name function_call_parameter_list
            { VariableClassFunctionCall $1 $3 $4 }
   |  variable_class_name '::' variable_without_objects function_call_parameter_list
            { VariableClassFunctionCall $1 $3 $4 }
   |  variable_without_objects function_call_parameter_list
            { IndirectFunctionCall $1 $2 }
;

class_name:
   T_STATIC
            { PHPClassNameStatic }
   |  namespace_name
            { PHPClassNameNamespaceRelative $1 }
   |  T_NAMESPACE '\\' namespace_name
            { PHPClassNameNamespaceSelf $3 }
   |  '\\' namespace_name
            { PHPClassNameNamespaceAbsolute $2 }
;

fully_qualified_class_name :: { PHPFullyQualifiedIdentifer }
   :  namespace_name
            { namespaceRelative $1 }
   |  T_NAMESPACE '\\' namespace_name
            { namespaceSelf $3 } 
   |  '\\' namespace_name
            { namespaceAbsolute $2 }
;



class_name_reference:
   class_name
            { $1 }
   |  dynamic_class_name_reference
            { $1 }
;


dynamic_class_name_reference:
   base_variable '->' object_property  dynamic_class_name_variable_properties
            { DynamicClassName3 $1 $3 (reverse $4) }   
   |  base_variable
            { DynamicClassName1 $1 }
;


dynamic_class_name_variable_properties:
   dynamic_class_name_variable_properties dynamic_class_name_variable_property
            { $2:$1 } 
   |
            {- empty -}
            { [] }
;


dynamic_class_name_variable_property:
   '->' object_property
            { $2 }
;

exit_expr:
            {- empty -}
            { ExitEmpty }
   |  '(' ')'
            { ExitAlmostEmpty }
   |  parenthesis_expr
            { ExitNotEmpty $1 }
;

backticks_expr :: { [PHPStringValue] }
   :  {- empty -}
            { [] }
   |  T_STRING_CONST
            { [$1] }
   |  encaps_list
            { reverse $1 }
;


ctor_arguments :: { [PHPExpr] }
   :  {- empty -}
            { [] }
   |  function_call_parameter_list
            { $1 }
;


common_scalar:
   T_LNUMBER
            { Constant $1 }   
   |  T_DNUMBER
            { Constant $1 }   
   |  T_STRING_CONST
            { Constant $1 }
   |  T_LINE
            { MagicLine }   
   |  T_FILE
            { MagicFile }   
   |  T_DIR
            { MagicDir }   
   |  T_TRAIT_C
            { MagicTrait }   
   |  T_METHOD_C
            { MagicMethod }   
   |  T_FUNC_C
            { MagicFunc }   
   |  T_NS_C
            { MagicNamespace }   
   |  T_START_HEREDOC T_STRING_CONST T_END_HEREDOC
            { Constant $2 } 
   |  T_START_HEREDOC T_END_HEREDOC
            { Constant "" }
;


static_scalar :: { PHPScalar }
   :  common_scalar
            { $1 }
   |  static_class_name_scalar
            { $1 }
   |  namespace_name
            { PHPConstant (namespaceRelative $1) }
   |  T_NAMESPACE '\\' namespace_name
            { PHPConstant (namespaceSelf $3) }
   |  '\\' namespace_name
            { PHPConstant (namespaceAbsolute $2) }
   |  '+' static_scalar
            { PHPStaticUnaryPlus $2 }
   |  '-' static_scalar
            { PHPStaticUnaryMinus $2 }
   |  T_ARRAY '(' static_array_pair_list ')'
            { PHPStaticArray $3 }
   |  '[' static_array_pair_list ']'
            { PHPStaticArray $2 }
   |  static_class_constant
            { $1 }
   |  T_CLASS_C
            { PHPMagicClass }
;

static_class_constant:
   class_name '::' IDENT
            { StaticClassConstant $1 $3 }
;

scalar:
   T_VARIABLE_STR
            { Variable $1 }   
   |  class_name_scalar
            { ClassScalar $1 }
   |  class_constant
            { ClassConstant $1 } 
   |  namespace_name
            { PHPConstant (namespaceRelative $1) }
   |  T_NAMESPACE '\\' namespace_name
            { PHPConstant (namespaceSelf $3) }
   |  '\\' namespace_name
            { PHPConstant (namespaceAbsolute $2) }
   |  common_scalar
            { $1 }
   |  '"' encaps_list '"'
            { ScalarString $2 }
   |  T_START_HEREDOC encaps_list T_END_HEREDOC
            { ScalarString $2 }
   |  T_CLASS_C
            { MagicClass $1 }
;


static_array_pair_list:
            {- empty -}
            { [] } 
   |  non_empty_static_array_pair_list possible_comma
            { reverse $1 }   
;

possible_comma:
            {- empty -}
            {}
   |  ','
            {}
;

non_empty_static_array_pair_list :: { [PHPStaticArrayPair] }
   :  non_empty_static_array_pair_list ',' static_scalar '=>' static_scalar
            { (PHPStaticArrayPairKV $3 $5):$1 }   
   |  non_empty_static_array_pair_list ',' static_scalar
            { (PHPStaticArrayPairV $1):$3 }
   |  static_scalar '=>' static_scalar
            { [PHPStaticArrayPairKV $1 $3] }
   |  static_scalar
            { [PHPStaticArrayPairV $1] }
;

expr :: { PHPExpr }
   :  r_variable
            { $1 }
   |  expr_without_variable
            { $1 }   
;

parenthesis_expr :: { PHPExpr }
   :  '(' expr ')'
            { $2 }
   |  '(' yield_expr ')'
            { $2 }
;


r_variable :: { PHPVariable }
   :  variable
            { $1 }
;


w_variable :: { PHPVariable }
   :  variable
            { $1 }
;

rw_variable :: { PHPVariable }
   :  variable
            { $1 }
;

variable :: { PHPVariable }
   :  base_variable_with_function_calls '->' object_property  method_or_not variable_properties
            { VariableWithArrow $1 $3 $4 (reverse $5) }
   |  base_variable_with_function_calls
            { VariableWithoutArrow $1 }
;

variable_properties:
   variable_properties variable_property
            { ($2:$1) }
   |
            {- empty -}
            { [] }
;


variable_property:
   '->' object_property  method_or_not
            { VariableProperty $1 $2 }
;

array_method_dereference:
   array_method_dereference '[' dim_offset ']'
            { (\(ArrayMethodDereference m x) -> ArrayMethodDereference m ($3:x)) $1 } 
   |  method '[' dim_offset ']'
            { ArrayMethodDereference $1 [$3] }
;

method :: { [PHPExpr] }
   :  function_call_parameter_list
            { $1 }
;

method_or_not:
      method 
            { Method $1 }   
   |  array_method_dereference
            { ArrayMethod $1 }
   |  {- empty -}
            { NotMethod } 
;

variable_without_objects:
   reference_variable
            { VariableWithoutObjects 0 $1 }
   |  simple_indirect_reference reference_variable
            { VariableWithoutObjects $1 $2 }
;

static_member:
   class_name '::' variable_without_objects
            { StaticMember $1 $3 }
   |  variable_class_name '::' variable_without_objects
            { StaticMemberDynamic $1 $3 }

;

variable_class_name:
   reference_variable
            { $1 } 
;

array_function_dereference:
   array_function_dereference '[' dim_offset ']'
            { (\(ArrayFunctionDereference f x) -> ArrayFunctionDereference f ($3:x)) $1 }
   |  function_call '[' dim_offset ']'
            { ArrayFunctionDereference $1 [$3] }
;

base_variable_with_function_calls:
   base_variable
            { $1 }   
   |  array_function_dereference
            { $1 }
   |  function_call
            { $1 }
;


base_variable:
   reference_variable
            { BaseVariable 0 $1 }
   |  simple_indirect_reference reference_variable
            { BaseVariable $1 $2 } 
   |  static_member
            { $1 }
;

reference_variable:
   reference_variable '[' dim_offset ']'
            { (\(ReferenceVariable v x) -> ReferenceVariable v ($3:(RVOffset x))) $1 }
   |  reference_variable '{' expr '}'
            { (\(ReferenceVariable v x) -> ReferenceVariable v ($3:(RVIndex x)))  $1 }
   |  compound_variable
            { ReferenceVariable $1 [] }
;


compound_variable:
   T_VARIABLE
            { CompoundVariable $1 }
   |  '$' '{' expr '}'
            { IndirectCompoundVariable $3 }
;

dim_offset:
            {- empty -}
            { OffsetEmpty }
   |  expr
            { Offset $1 }
;


object_property:
   object_dim_list
            { ObjectDimList $1 }
   |  variable_without_objects
            { VariableWithoutObjects $1 }
;

object_dim_list:
   object_dim_list '[' dim_offset ']'
            { (\(DimList v x) -> DimList v ($3:(ODOffset x))) $1 }
   |  object_dim_list '{' expr '}'
            { (\(DimList v x) -> DimList v ($3:(ODIndex x)))  $1 }
   |  variable_name
            { DimList $1 [] }
;

variable_name:
   IDENT
            { Variable $1 }
   |  '{' expr '}'
            { Indirect $2 }
;

simple_indirect_reference:
   '$'
            { 1 }
   |  simple_indirect_reference '$'
            { (+1) $1 } 
;

assignment_list :: { [PHPALE] }
   :  assignment_list ',' assignment_list_element
            { $3:$1 }
   |  assignment_list_element
            { [$1] }
;


assignment_list_element:: { PHPALE }
   :  variable
            { PHPALEVariable $1 }   
   |  T_LIST '('  assignment_list ')'
            { PHPALEListElement $3 }
   |
            {- empty -}
            { PHPALEEmpty }   
;


array_pair_list:
            {- empty -}
            { [] }
   |  non_empty_array_pair_list possible_comma
            { reverse $1 }
;

non_empty_array_pair_list:
   non_empty_array_pair_list ',' expr '=>' expr
            { (ArrayPairKV $3 $5) : $1 }
   |  non_empty_array_pair_list ',' expr
            { (ArrayPairV $3) : $1 }   
   |  expr '=>' expr
            { (ArrayPairKV $1 $3) : [] }   
   |  expr
            { (ArrayPairV $1) : [] }   
   |  non_empty_array_pair_list ',' expr '=>' '&' w_variable
            { (ArrayPairKR $3 $6) : $1 } 
   |  non_empty_array_pair_list ',' '&' w_variable
            { (ArrayPairR $4) : $1 }
   |  expr '=>' '&' w_variable
            { (ArrayPairKR $1 $4) : [] }   
   |  '&' w_variable
            { (ArrayPairR $2) : [] }   
;

encaps_list :: { [PHPStringValue] }
   :  encaps_list encaps_var
            { $2 : $1 }
   |  encaps_list T_STRING_CONST
            { $2 : $1 }
   |  encaps_var
            { $1 : [] }
   |  T_STRING_CONST encaps_var
            { $1 : [] }
;

encaps_var :: { PHPStringValue }
   :  T_VARIABLE
            { PHPVariableString $1 }
   |  T_VARIABLE '['  encaps_var_offset ']'
            { PHPVariableOffsetString $1 $3 }
   |  T_VARIABLE '->' IDENT
            { PHPVariablePropertyString $1 $3 } 
   |  '${' expr '}'
            { PHPExprString $2 }
   |  '${' T_VARIABLE_STR '[' expr ']' '}'
            { PHPVariableOffsetString $2 [$4] }
   |  '{' variable '}'
            { PHPExprString $2 }
;

encaps_var_offset :: { PHPVariableOffset }
   :  IDENT
            { EncapsVarOffsetIdent $1 }   
   |  T_LNUMBER
            { EncapsVarOffsetConstant $1 }
   |  T_VARIABLE
            { EncapsVarOffsetVariable $1 }   
;

internal_functions_in_yacc:
   T_ISSET '(' isset_variables ')'
            { IsSet (reverse $3) }  
   |  T_EMPTY '(' variable ')'
            { Empty $3 }
   |  T_EMPTY '(' expr_without_variable ')'
            { Empty $3 }
   |  T_INCLUDE expr
            { Include $2 }
   |  T_INCLUDE_ONCE expr
            { IncludeOnce $2 }
   |  T_EVAL '(' expr ')'
            { Eval $3 }
   |  T_REQUIRE expr
            { Require $2 }
   |  T_REQUIRE_ONCE expr
            { RequireOnce $2 }
;

isset_variables:
   isset_variable
            { $1 : [] }
   |  isset_variables ','  isset_variable
            { $3 : $1 }
;

isset_variable:
   variable
            { $1 }
   |  expr_without_variable
            { $1 }
;

class_constant:
   class_name '::' IDENT
            { ClassConstantO $1 $3 }
   |  variable_class_name '::' IDENT
            { ClassConstantV $1 $3 }
;

static_class_name_scalar:
   class_name '::' T_CLASS
            { $1 }
;

class_name_scalar:   
   class_name '::' T_CLASS
            { PHPClassNameScalar $1 }
;


{

happyError :: [PHPStatement] -> a
happyError _ = error ("Parse error\n")
           
}
