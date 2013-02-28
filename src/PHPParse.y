{
module PHPParse where

import PHPLex (AlexState, Token(..), mLexer)
}

%name phpParse start
%tokentype { Token }

%token INLINE_HTML         { InlineHTML $$ }

%token CAST_INT                { CastInt }
%token CAST_REAL        { CastReal }
%token CASIDENT        { CastString }
%token CAST_ARRAY        { CastArray }
%token CAST_OBJECT        { CastObject }
%token CAST_BOOL        { CastBool }
%token CAST_UNSET        { CastUnset }

%token OP_EQ_EQ                { OpEqEq  }
%token OP_EQ_EQ_EQ             { OpEqEqEq  }
%token OP_NOT_EQ               { OpNotEq  }
%token OP_NOT_EQ_EQ            { OpNotEqEq  }
%token OP_LE                  { OpLE  }
%token OP_GE                  { OpGE  }
%token OP_INC                  { OpInc  }
%token OP_DEC                  { OpDec  }
%token OP_DOUBLE_ARROW         { OpDoubleArrow  }
%token OP_SINGLE_ARROW         { OpSingleArrow  }
%token OP_SL                  { OpSL  }
%token OP_SR                  { OpSR  }
%token OP_PLUS_EQ              { OpPlusEq  }
%token OP_MINUS_EQ             { OpMinusEq  }
%token OP_MULT_EQ              { OpMultEq  }
%token OP_DIV_EQ               { OpDivEq  }
%token OP_CONCAT_EQ            { OpConcatEq  }
%token OP_MOD_EQ               { OpModEq  }
%token OP_AND_EQ               { OpAndEq  }
%token OP_OR_EQ                { OpOrEq  }
%token OP_XOR_EQ               { OpXorEq  }
%token OP_SL_EQ               { OpSLEq  }
%token OP_SR_EQ               { OpSREq  }
%token OP_COLON_COLON          { OpColonColon  }
%token OP_LOGIC_AND            { OpLogicAnd  }
%token OP_LOGIC_OR             { OpLogicOr  }
%token OP_PLUS                 { OpPlus }
%token OP_MINUS                { OpMinus }
%token OP_SLASH                { OpSlash }
%token OP_STAR                 { OpStar }
%token OP_PERCENT              { OpPercent }
%token OP_CARET                { OpCaret }
%token OP_AMPERSAND            { OpAmpersand }
%token OP_PIPE                 { OpPipe }
%token OP_TILDE                { OpTilde }
%token OP_EQ                   { OpEq }
%token OP_LT                   { OpLt }
%token OP_GT                   { OpGt }
%token OP_DOT                  { OpDot }
%token OP_BANG                 { OpBang }
%token OP_COMMA                { OpComma }
%token OP_QUESTION             { OpQuestion }
%token OP_COLON                { OpColon }
%token OP_AT_SIGN              { OpAtSign }
%token '$' 		{ OpDollars }

%token ';'              { Semicolon }
%token '('              { LParen }
%token ')'              { RParen }
%token '{'              { LBrace }
%token '}'              { RBrace }
%token '['              { LBracket }
%token ']'              { RBracket }

%token OP                      { Op String }
%token VARIABLE                { Variable String }
%token IDENT                   { Ident String }

%token KEYWORD_AND             { KeywordAnd  }
%token KEYWORD_OR              { KeywordOr  }
%token KEYWORD_XOR             { KeywordXor  }
%token KEYWORD___FILE__ { Keyword__FILE__  }
%token KEYWORD___LINE__ { Keyword__LINE__  }
%token KEYWORD_ARRAY           { KeywordArray  }
%token KEYWORD_AS              { KeywordAs  }
%token KEYWORD_BREAK           { KeywordBreak  }
%token KEYWORD_CASE            { KeywordCase  }
%token KEYWORD_CLASS           { KeywordClass  }
%token KEYWORD_CONST           { KeywordConst  }
%token KEYWORD_CONTINUE { KeywordContinue  }
%token KEYWORD_DECLARE         { KeywordDeclare  }
%token KEYWORD_DEFAULT         { KeywordDefault  }
%token KEYWORD_DO              { KeywordDo  }
%token KEYWORD_ECHO            { KeywordEcho  }
%token KEYWORD_ELSE            { KeywordElse  }
%token KEYWORD_ELSEIF          { KeywordElseif  }
%token KEYWORD_EMPTY           { KeywordEmpty  }
%token KEYWORD_ENDDECLARE { KeywordEnddeclare  }
%token KEYWORD_ENDFOR          { KeywordEndfor  }
%token KEYWORD_ENDFOREACH { KeywordEndforeach  }
%token KEYWORD_ENDIF           { KeywordEndif  }
%token KEYWORD_ENDSWITCH  { KeywordEndswitch  }
%token KEYWORD_ENDWHILE { KeywordEndwhile  }
%token KEYWORD_EVAL            { KeywordEval  }
%token KEYWORD_EXIT            { KeywordExit  }
%token KEYWORD_DIE             { KeywordDie  }
%token KEYWORD_EXTENDS         { KeywordExtends  }
%token KEYWORD_FOR             { KeywordFor  }
%token KEYWORD_FOREACH         { KeywordForeach  }
%token KEYWORD_FUNCTION { KeywordFunction  }
%token KEYWORD_GLOBAL          { KeywordGlobal  }
%token KEYWORD_IF              { KeywordIf  }
%token KEYWORD_INCLUDE         { KeywordInclude  }
%token KEYWORD_INCLUDE_ONCE    { KeywordIncludeOnce  }
%token KEYWORD_INSTANCE_OF     { KeywordInstanceOf  }
%token KEYWORD_ISSET           { KeywordIsset  }
%token KEYWORD_LIST            { KeywordList  }
%token KEYWORD_NEW             { KeywordNew  }
%token KEYWORD_PRINT           { KeywordPrint  }
%token KEYWORD_REQUIRE         { KeywordRequire  }
%token KEYWORD_REQUIRE_ONCE    { KeywordRequireOnce  }
%token KEYWORD_RETURN          { KeywordReturn  }
%token KEYWORD_STATIC          { KeywordStatic  }
%token KEYWORD_SWITCH          { KeywordSwitch  }
%token KEYWORD_UNSET           { KeywordUnset  }
%token KEYWORD_USE             { KeywordUse  }
%token KEYWORD_VAR             { KeywordVar  }
%token KEYWORD_WHILE           { KeywordWhile  }
%token KEYWORD___FUNCTION__     { Keyword__FUNCTION__  }
%token KEYWORD___CLASS__         { Keyword__CLASS__  }
%token KEYWORD___METHOD__         { Keyword__METHOD__  }
%token KEYWORD_FINAL           { KeywordFinal  }
%token KEYWORD_INTERFACE        { KeywordInterface  }
%token KEYWORD_IMPLEMENT        { KeywordImplement  }
%token KEYWORD_PUBLIC          { KeywordPublic  }
%token KEYWORD_PRIVATE         { KeywordPrivate  }
%token KEYWORD_PROTECTED               { KeywordProtected  }
%token KEYWORD_ABSTRACT                { KeywordAbstract  }
%token KEYWORD_CLONE           { KeywordClone  }
%token KEYWORD_TRY             { KeywordTry  }
%token KEYWORD_CATCH           { KeywordCatch  }
%token KEYWORD_THROW           { KeywordThrow  }
%token KEYWORD_CFUNCTION               { KeywordCfunction  }
%token KEYWORD_OLD_FUNCTION            { KeywordOldFunction  }
%token KEYWORD_TRUE            { KeywordTrue  }
%token KEYWORD_FALSE           { KeywordFalse  }
%token KEYWORD_NULL            { KeywordNull  }

%token INTEGER           { PHPInteger $$ }
%token REAL              { PHPReal $$ }
%token STRING            { PHPString $$ }
       
%lexer { mLexer } { EOF }
%monad { P }

%%

start:
	top_statement_list		{ reverse $1 }
;

top_statement_list:
		top_statement_list   top_statement 	{ ($2:$1) }
	|	{- empty -}				{ [] }
;

namespace_name:
		IDENT 					{ [$1] }
	|	namespace_name T_NS_SEPARATOR IDENT 	{ $3:$1 }
;

top_statement:
		statement				{ $1 }
	|	function_declaration_statement		{ $1 }
	|	class_declaration_statement		{ $1 }
	|	T_NAMESPACE namespace_name ';'		{ NamespaceD $2 }
	|	T_NAMESPACE namespace_name '{' top_statement_list '}'	{ Namespace $2 (reverse $4) }
	|	T_NAMESPACE '{'	top_statement_list '}'	{ NamespaceG (reverse $3) }
	|	T_USE use_declarations ';'      	{ UseDecl (reverse $2) }
	|	constant_declaration ';'		{ ConstantDecl (reverse $1) }
;

use_declarations:
		use_declarations ',' use_declaration	{ ($3:$1) }
	|	use_declaration				{ [$1] }			
;

use_declaration:
		namespace_name 					{ Use $1 }	
	|	namespace_name T_AS IDENT			{ UseAs $1 $3 }
	|	T_NS_SEPARATOR namespace_name 			{ UseG $2 }
	|	T_NS_SEPARATOR namespace_name T_AS IDENT 	{ UseGAs $2 $4 }
;

constant_declaration:
		constant_declaration ',' IDENT '=' static_scalar		{ (ConstantDeclaration $3 $5):$1 }
	|	T_CONST IDENT '=' static_scalar 				{ [ConstantDeclaration $2 $4] }
;

inner_statement_list:
		inner_statement_list   inner_statement  	{ $2:$1 }
	|	{- empty -} 					{ [] }	
;


inner_statement:
		statement			{ $1 }
	|	function_declaration_statement  { $1 }
	|	class_declaration_statement     { $1 }
;


statement:
		unticked_statement		{ $1 } 
	|	IDENT ':'			{ Label $1 }  
;

unticked_statement:
		'{' inner_statement_list '}'									{ StatementGroup $2 }
	|	T_IF parenthesis_expr  statement  elseif_list else_single 					{ If $2 $3 (reverse $4) $5 } 
	|	T_IF parenthesis_expr ':' inner_statement_list new_elseif_list new_else_single T_ENDIF ';' 	{ If $2 (reverse $4) (reverse $5) $6 } 
	|	T_WHILE  parenthesis_expr  while_statement 							{ While $2 $3 } 
	|	T_DO  statement T_WHILE  parenthesis_expr ';' 							{ Do $2 $4 }
	|	T_FOR '(' for_expr ';' for_expr ';' for_expr ')' for_statement 					{ For $3 $5 $7 $9 }
	|	T_SWITCH parenthesis_expr switch_case_list 							{ Switch $2 (reverse $3) }
	|	T_BREAK ';'											{ Break1 }
	|	T_BREAK expr ';'										{ Break $2 }
	|	T_CONTINUE ';'											{ Continue1 }					
	|	T_CONTINUE expr ';'										{ Continue $2 }
	|	T_RETURN ';'											{ ReturnNULL }						
	|	T_RETURN expr_without_variable ';'								{ Return $2 }
	|	T_RETURN variable ';'										{ Return $2 }
	|	yield_expr ';' 											{ Yield $2 }
	|	T_GLOBAL global_var_list ';'									{ Global $2 }
	|	T_STATIC static_var_list ';'									{ Static $2 }
	|	T_ECHO echo_expr_list ';'									{ Echo $2 }
	|	T_INLINE_HTML											{ Inline $1 }
	|	expr ';'											{ Expr $1 }				
	|	T_UNSET '(' unset_variables ')' ';'								{ Unset (reverse $3) }
	|	T_FOREACH '(' variable T_AS foreach_variable foreach_optional_arg ')' foreach_statement		{ Foreach $3 $5 $6 $8 } 
	|	T_FOREACH '(' expr_without_variable T_AS foreach_variable foreach_optional_arg ')' foreach_statement 	{ Foreach $3 $5 $6 $8 }
	|	T_DECLARE  '(' declare_list ')' declare_statement 						{ Declare $3 $5 }
	|	';'										{ }
	|	T_TRY  '{' inner_statement_list '}' catch_statement finally_statement 				{ Try $3 $5 $6 }
	|	T_THROW expr ';' 										{ Throw $2 }
	|	T_GOTO IDENT ';' 										{ Goto $2 }
;

catch_statement:
		{- empty -} 											{ [] }
	|	T_CATCH '('  fully_qualified_class_name T_VARIABLE ')' '{' inner_statement_list '}' additional_catches { (Catch $3 $4 $7):(reverse $9) }  

finally_statement:
		{- empty -}					{ Nothing } 
	|	T_FINALLY  '{' inner_statement_list '}' 	{ Just (Finally $3) } 						
;

additional_catches:
		non_empty_additional_catches			{ $1 } 
	|	{- empty -} 					{ [] }
;

non_empty_additional_catches:
		additional_catch				{ [$1] } 
	|	non_empty_additional_catches additional_catch 	{ $2:$1 }
;

additional_catch:
	T_CATCH '(' fully_qualified_class_name  T_VARIABLE ')'  '{' inner_statement_list '}'	{ Catch $3 $4 $7 } 
;

unset_variables:
		unset_variable					{ [$1] }
	|	unset_variables ',' unset_variable		{ $3:$1 }
;

unset_variable:
		variable	{ $1 }	
;

function_declaration_statement:
		unticked_function_declaration_statement		{ $1 }	
;

class_declaration_statement:
		unticked_class_declaration_statement		{ $1 }
;

is_reference:
		{- empty -}					{ false }	
	|	'&'						{ true }
;

unticked_function_declaration_statement:
		function is_reference IDENT '(' parameter_list ')' '{' inner_statement_list '}'		{ FunctionDecl $1 $2 (reverse $4) (reverse $7) } 
;

unticked_class_declaration_statement:
		class_entry_type IDENT extends_from implements_list '{' class_statement_list '}'	{ ClassDecl $1 $2 $3 $4 (reverse $6) }
	|	interface_entry IDENT interface_extends_list '{' class_statement_list '}' 		{ InterfaceDecl $1 $2 $3 (reverse $5) }
;


class_entry_type:
		T_CLASS			{ ClassStandard }
	|	T_ABSTRACT T_CLASS 	{ ClassAbstract }
	|	T_TRAIT 		{ ClassTrait }
	|	T_FINAL T_CLASS 	{ ClassFinal }
;

extends_from:
		{- empty -}				{ Nothing }			
	|	T_EXTENDS fully_qualified_class_name	{ Just $2 }
;

interface_entry:
	T_INTERFACE			{ ClassInterface }
;

interface_extends_list:
		{- empty -}			{ [] }
	|	T_EXTENDS interface_list	{ reverse $2 }
;

implements_list:
		{- empty -}			{ [] }
	|	T_IMPLEMENTS interface_list	{ reverse $2 }
;

interface_list:
		fully_qualified_class_name			{ [$1] }		
	|	interface_list ',' fully_qualified_class_name 	{ $3:$1 }
;

foreach_optional_arg:
		{- empty -}			{ Nothing }		
	|	T_DOUBLE_ARROW foreach_variable	{ Just $2 }
;

foreach_variable:
		variable			{ ForeachVar $1 }
	|	'&' variable			{ ForeachRef $2 }
	|	T_LIST '('  assignment_list ')' { ForeachList $3 }
;

for_statement:
		statement				{ $1 }
	|	':' inner_statement_list T_ENDFOR ';'	{ StatementGroup $2 }
;


foreach_statement:
		statement					{ $1 }
	|	':' inner_statement_list T_ENDFOREACH ';'	{ StatementGroup $2 }
;


declare_statement:
		statement					{ $1 }
	|	':' inner_statement_list T_ENDDECLARE ';'	{ StatementGroup $2 }
;


declare_list:
		IDENT '=' static_scalar				{ [Declaration $1 $3] }	
	|	declare_list ',' IDENT '=' static_scalar	{ (Declaration $3 $5):$1 }
;

switch_case_list:
		'{' case_list '}'				{ $2 }					
	|	'{' ';' case_list '}'				{ $3 }
	|	':' case_list T_ENDSWITCH ';'			{ $2 }
	|	':' ';' case_list T_ENDSWITCH ';'		{ $3 }
;


case_list:
		{- empty -}							{ [] }	
	|	case_list T_CASE expr case_separator  inner_statement_list 	{ (Case $3 $5):$1 }
	|	case_list T_DEFAULT case_separator  inner_statement_list 	{ (CaseDefault $4):$1 }
;


case_separator:
		':'	{}
	|	';'	{}
;


while_statement:
		statement				{ $1 }
	|	':' inner_statement_list T_ENDWHILE ';' { StatementGroup $2 }
;



elseif_list:
		{- empty -}						{ [] }
	|	elseif_list T_ELSEIF parenthesis_expr  statement 	{ (ElseIf $3 $4):$1 }
;


new_elseif_list:
		{- empty -}								{ [] }
	|	new_elseif_list T_ELSEIF parenthesis_expr ':'  inner_statement_list 	{ (ElseIf $3 (StatementGroup $5)):$1 }
;


else_single:
		{- empty -}		{ Nothing }
	|	T_ELSE statement	{ Just $2 }
;


new_else_single:
		{- empty -}				{ Nothing }
	|	T_ELSE ':' inner_statement_list		{ Just (StatementGroup $3) }
;


parameter_list:
		non_empty_parameter_list		{ reverse $1 }
	|	{- empty -}				{ [] }
;


non_empty_parameter_list:
		optional_class_type T_VARIABLE								{ [Parameter $1 $2] }	
	|	optional_class_type '&' T_VARIABLE							{ [RefParameter $1 $3] }
	|	optional_class_type '&' T_VARIABLE '=' static_scalar					{ [RefParameterWithDefault $1 $3 $5] }
	|	optional_class_type T_VARIABLE '=' static_scalar					{ [ParameterWithDefault $1 $2 $4] }
	|	non_empty_parameter_list ',' optional_class_type T_VARIABLE 				{ (Parameter $3 $4):$1 }
	|	non_empty_parameter_list ',' optional_class_type '&' T_VARIABLE				{ (RefParameter $3 $5):$1 }
	|	non_empty_parameter_list ',' optional_class_type '&' T_VARIABLE	 '=' static_scalar 	{ (RefParameterWithDefault $3 $5 $7):$1 }
	|	non_empty_parameter_list ',' optional_class_type T_VARIABLE '=' static_scalar 		{ (ParameterWithDefault $3 $5 $6):$1 }
;


optional_class_type:
		{- empty -}			{ TypeUnspecified }					
	|	T_ARRAY				{ TypeArray }		
	|	T_CALLABLE			{ TypeCallable }		
	|	fully_qualified_class_name	{ TypeClass $1 }		
;


function_call_parameter_list:
		'(' ')'						{ ParameterList [] }
	|	'(' non_empty_function_call_parameter_list ')'	{ ParameterList (reverse $2) }
	|	'(' yield_expr ')'				{ YieldParameter $2 }
;


non_empty_function_call_parameter_list:
		expr_without_variable				{ [$1] }
	|	variable					{ [$1] }
	|	'&' w_variable 					{ [Reference $2] }
	|	non_empty_function_call_parameter_list ',' expr_without_variable	{ $3:$1 }		
	|	non_empty_function_call_parameter_list ',' variable			{ $3:$1 }	
	|	non_empty_function_call_parameter_list ',' '&' w_variable		{ (Reference $4):$1 }	
;

global_var_list:
		global_var_list ',' global_var		{ $3:$1 }
	|	global_var				{ [$1] }		
;


global_var:
		T_VARIABLE		{ $1 }	
	|	'$' r_variable		{ Indirect $2 }
	|	'$' '{' expr '}'	{ Indirect $3 }
;


static_var_list:
		static_var_list ',' T_VARIABLE 				{ (StaticVar $3):$1 }
	|	static_var_list ',' T_VARIABLE '=' static_scalar 	{ (StaticVarWithInitializer $3 $5):$1 }
	|	T_VARIABLE  						{ [StaticVar $1] }
	|	T_VARIABLE '=' static_scalar 				{ [StaticVarWithInitializer $1 $3] }

;


class_statement_list:
		class_statement_list class_statement	{ $3:$1 }
	|	{- empty -}				{ [] }
;


class_statement:
		variable_modifiers  class_variable_declaration ';'	{ ClassVariableDeclaration $1 $2 }
	|	class_constant_declaration ';'				{ $1 }
	|	trait_use_statement					{ $1 }
	|	method_modifiers function is_reference IDENT '(' parameter_list ')' method_body		{ MethodDeclaration $1 $2 $3 $4 $6 $8 } 
;

trait_use_statement:
		T_USE trait_list trait_adaptations	{ TraitUseStatement (reverse $2) $3 }
;

trait_list:
		fully_qualified_class_name			{ [$1] }				
	|	trait_list ',' fully_qualified_class_name	{ $3:$1 }	
;

trait_adaptations:
		';'						{ [] }
	|	'{' trait_adaptation_list '}'			{ $2 }
;

trait_adaptation_list:
		{- empty -}					{ [] }
	|	non_empty_trait_adaptation_list			{ reverse $1 }
;

non_empty_trait_adaptation_list:
		trait_adaptation_statement					{ [$1] }
	|	non_empty_trait_adaptation_list trait_adaptation_statement	{ $3:$1 }
;

trait_adaptation_statement:
		trait_precedence ';'	{ $1 }
	|	trait_alias ';'		{ $1 }
;

trait_precedence:
	trait_method_reference_fully_qualified T_INSTEADOF trait_reference_list { TraitPrecedence $1 $3 }	
;

trait_reference_list:
		fully_qualified_class_name				{ [$1] }					
	|	trait_reference_list ',' fully_qualified_class_name	{ $3:$1 }	
;

trait_method_reference:
		IDENT							{ TraitMethodReference $1 }						
	|	trait_method_reference_fully_qualified			{ $1 } 		
;

trait_method_reference_fully_qualified:
	fully_qualified_class_name '::' IDENT		{ TraitMethodReferenceFQ $1 $3 } 
;

trait_alias:
		trait_method_reference T_AS trait_modifiers IDENT	{ TraitAliasTrait $1 $3 $4 }	
	|	trait_method_reference T_AS member_modifier		{ TraitAliasMember $1 $3 }			
;

trait_modifiers:
		{- empty -}		{ Nothing }	
	|	member_modifier	 	{ Just $1 }
;

method_body:
		';' 				{ AbstractMethod }		
	|	'{' inner_statement_list '}'	{ $2 }
;

variable_modifiers:
		non_empty_member_modifiers	{ reverse $1 }		
	|	T_VAR				{ [] }			
;

method_modifiers:
		{- empty -}			{ [] }				
	|	non_empty_member_modifiers	{ reverse $1 }
;

non_empty_member_modifiers:
		member_modifier					{ [$1] }	
	|	non_empty_member_modifiers member_modifier	{ $2:$1 }
;

member_modifier:
		T_PUBLIC	{ ModifierPublic }			
	|	T_PROTECTED	{ ModifierProtected }			
	|	T_PRIVATE	{ ModifierPrivate }			
	|	T_STATIC	{ ModifierStatic }			
	|	T_ABSTRACT	{ ModifierAbstract }			
	|	T_FINAL		{ ModifierFinal }			
;

class_variable_declaration:
		class_variable_declaration ',' T_VARIABLE			{ (ClassVariableDecl $3):$1 }					
	|	class_variable_declaration ',' T_VARIABLE '=' static_scalar	{ (ClassVariableDeclWithInit $3 $5):$1 }
	|	T_VARIABLE							{ [ClassVariableDecl $1] }
	|	T_VARIABLE '=' static_scalar					{ [ClassVariableDeclWithInit $1 $3] }
;

class_constant_declaration:
		class_constant_declaration ',' IDENT '=' static_scalar		{ (ClassConstantDecl $3 $5):$1 }
	|	T_CONST IDENT '=' static_scalar					{ [ClassConstantDecl $1 $3] }
;

echo_expr_list:
		echo_expr_list ',' expr 	{ $3:$1 }
	|	expr				{ [$1] }	
;


for_expr:
		{- empty -}			{ Nothing }
	|	non_empty_for_expr		{ Just (reverse $1) }
;

non_empty_for_expr:
		non_empty_for_expr ','	 expr 	{ $3:$1 }
	|	expr				{ [$1] }	
;

chaining_method_or_property:
		chaining_method_or_property variable_property 	{ $2:$1 }
	|	variable_property 				{ [$1] }			
;

chaining_dereference:
		chaining_dereference '[' dim_offset ']'		{ ChainDeref $1 $3 }
	|	'[' dim_offset ']'				{ Offset $2 }
;

chaining_instance_call:
		chaining_dereference chaining_method_or_property	{ ChainingInstanceCall (Just $1) (reverse $2) } 
	|	chaining_dereference 					{ ChainingInstanceCall (Just $1) [] }
	|	chaining_method_or_property 				{ ChainingInstanceCall Nothing (reverse $1) }
;

instance_call:
		{- empty -}			{ Nothing } 		
	|	chaining_instance_call		{ Just $1 }
;

new_expr:
		T_NEW class_name_reference  ctor_arguments 	{ New $1 $2 }
;

expr_without_variable:
		T_LIST '('  assignment_list ')' '=' expr 			{ ListAssignment $3 $5 }
	|	variable '=' expr						{ Assignment $1 $3 }
	|	variable '=' '&' variable 					{ RefAssignment $1 $4 }
	|	variable '=' '&' T_NEW class_name_reference  ctor_arguments 	{ RefAssignmentNew $1 $5 $6 }
	|	T_CLONE expr 							{ Clone $2 }
	|	variable T_PLUS_EQUAL expr 	{ AddInto $1 $3 }
	|	variable T_MINUS_EQUAL expr	{ SubtractInto $1 $3 }
	|	variable T_MUL_EQUAL expr	{ MultiplyInto $1 $3 }	
	|	variable T_DIV_EQUAL expr	{ DivideInto $1 $3 }	
	|	variable T_CONCAT_EQUAL expr	{ ConcatInto $1 $3 }
	|	variable T_MOD_EQUAL expr	{ ModulusInto $1 $3 }	
	|	variable T_AND_EQUAL expr	{ AndInto $1 $3 }	
	|	variable T_OR_EQUAL expr 	{ OrInto $1 $3 }	
	|	variable T_XOR_EQUAL expr 	{ XorInto $1 $3 }	
	|	variable T_SL_EQUAL expr	{ ShiftLeftInto $1 $3 }
	|	variable T_SR_EQUAL expr	{ ShiftRightInto $1 $3 }
	|	rw_variable T_INC 		{ Postincrement $2 }
	|	T_INC rw_variable 		{ Preincrement $2 }
	|	rw_variable T_DEC		{ Postdecrement $2 } 
	|	T_DEC rw_variable 		{ Predecrement $2 }
	|	expr T_BOOLEAN_OR  expr 	{ BooleanOr $1 $3 }
	|	expr T_BOOLEAN_AND  expr	{ BooleanAnd $1 $3 } 
	|	expr T_LOGICAL_OR  expr 	{ LogicalOr $1 $3 }
	|	expr T_LOGICAL_AND  expr 	{ LogicalAnd $1 $3 }
	|	expr T_LOGICAL_XOR expr 	{ LogicalXor $1 $3 }
	|	expr '|' expr			{ BinaryOr $1 $3 }
	|	expr '&' expr			{ BinaryAnd $1 $3 }
	|	expr '^' expr			{ BinaryXor $1 $3 }
	|	expr '.' expr 			{ Concat $1 $3 }
	|	expr '+' expr 			{ Add $1 $3 }
	|	expr '-' expr 			{ Subtract $1 $3 }
	|	expr '*' expr			{ Multiply $1 $3 }
	|	expr '/' expr			{ Divide $1 $3 }
	|	expr '%' expr 			{ Modulus $1 $3 }
	| 	expr T_SL expr			{ ShiftLeft $1 $3 }
	|	expr T_SR expr			{ ShiftRight $1 $3 }
	|	'+' expr 			{ UnaryPlus $2 }
	|	'-' expr 			{ UnaryMinus $2 }
	|	'!' expr 			{ LogicalNot $2 }
	|	'~' expr 			{ BinaryNegate $2 }
	|	expr T_IS_IDENTICAL expr	{ IsIdentical $1 $3 }		
	|	expr T_IS_NOT_IDENTICAL expr	{ IsNotIdentical $1 $3 }
	|	expr T_IS_EQUAL expr		{ IsEqual $1 $3 }	
	|	expr T_IS_NOT_EQUAL expr 	{ IsNotEqual $1 $3 }	
	|	expr '<' expr 			{ LessThan $1 $3 }		
	|	expr T_IS_SMALLER_OR_EQUAL expr { LessThanOrEqual $1 $3 }
	|	expr '>' expr 			{ GreaterThan $1 $3 }		
	|	expr T_IS_GREATER_OR_EQUAL expr { GreaterThanOrEqual $1 $3 }
	|	expr T_INSTANCEOF class_name_reference 	{ InstanceOf $1 $3 }
	|	parenthesis_expr 		{ $1 }
	|	new_expr			{ $1 }
	|	'(' new_expr ')'  instance_call { NewWithInstanceCall $2 $4 }
	|	expr '?' expr ':' expr		{ IfThenElseOp $1 $3 $5 }	 
	|	expr '?' ':' expr		{ IfElseOp $1 $4 }
	|	internal_functions_in_yacc 	{ $1 }
	|	T_INT_CAST expr 		{ IntCast $2 }
	|	T_DOUBLE_CAST expr 		{ DoubleCast $2 }
	|	IDENT_CAST expr			{ IdentCast $2 }
	|	T_ARRAY_CAST expr 		{ ArrayCast $2 }
	|	T_OBJECT_CAST expr 		{ ObjectCast $2 }
	|	T_BOOL_CAST expr		{ BoolCast $2 }
	|	T_UNSET_CAST expr		{ UnsetCast $2 }
	|	T_EXIT exit_expr		{ Exit $2 }
	|	'@'  expr 			{ DisableErrors $2 }
	|	scalar				{ $1 }
	|	combined_scalar_offset 		{ $1 }
	|	combined_scalar 		{ $1 }
	|	'`' backticks_expr '`' 		{ Backtick $2 }
	|	T_PRINT expr  			{ Print $2 }
	|	T_YIELD 			{ Yield0 }
	|	function is_reference '(' parameter_list ')' lexical_vars '{' inner_statement_list '}' 
						{ AnonymousFunction $2 $4 $6 $8 }
	|	T_STATIC function is_reference '(' parameter_list ')' lexical_vars '{' inner_statement_list '}'
						{ AnonymousStaticFunction $3 $5 $7 $9 }
;

yield_expr:
		T_YIELD expr_without_variable	{ Yield $2 } 
	|	T_YIELD variable 		{ Yield $2 }
	|	T_YIELD expr T_DOUBLE_ARROW expr_without_variable	{ Yield2 $2 $4 } 
	|	T_YIELD expr T_DOUBLE_ARROW variable 			{ Yield2 $2 $4 } 
;

combined_scalar_offset:
	  combined_scalar '[' dim_offset ']' 		{ CombinedScalarWithOffset $1 [$3] }
	| combined_scalar_offset '[' dim_offset ']' 	{ (\(CombinedScalarWithOffset x [y]) -> CombinedScalarWithOffset x [$3:y]) $1 }
    | T_CONSTANT_ENCAPSED_STRING '[' dim_offset ']' 	{ CombinedScalarWithOffset $1 [$3] }

combined_scalar:
      T_ARRAY '(' array_pair_list ')' 	{ CombinedScalar $3 }
    | '[' array_pair_list ']' 		{ CombinedScalar $3 }

function:
	T_FUNCTION 	{ }
;

lexical_vars:
		{- empty -}			{ [] }
	|	T_USE '(' lexical_var_list ')'	{ reverse $3 }
;

lexical_var_list:
		lexical_var_list ',' T_VARIABLE		{ (Variable $3):$1 }	
	|	lexical_var_list ',' '&' T_VARIABLE	{ (VariableRef $4):$1 }	
	|	T_VARIABLE				{ [Variable $1] }				
	|	'&' T_VARIABLE				{ [VariableRef $2] }			
;

function_call:
		namespace_name function_call_parameter_list 					 { FunctionCall (RelativeNamespace $1) $2 }
	|	T_NAMESPACE T_NS_SEPARATOR namespace_name function_call_parameter_list		 { FunctionCall (SelfNamespace $3) $4 $5 }
	|	T_NS_SEPARATOR namespace_name function_call_parameter_list 			 { FunctionCall (AbsoluteNamespace $2) $3 $4 }
	|	class_name '::' variable_name function_call_parameter_list 			 { ClassFunctionCall $1 $3 $4 }
	|	class_name '::' variable_without_objects function_call_parameter_list 		 { ClassFunctionCall $1 $3 $4 }
	|	variable_class_name '::' variable_name function_call_parameter_list 		 { VariableClassFunctionCall $1 $3 $4 }
	|	variable_class_name '::' variable_without_objects function_call_parameter_list   { VariableClassFunctionCall $1 $3 $4 }
	|	variable_without_objects function_call_parameter_list 				 { IndirectFunctionCall $1 $2 }
;

class_name:
		T_STATIC 					{ LateStaticBinding }
	|	namespace_name 					{ RelativeNamespace $1 }
	|	T_NAMESPACE T_NS_SEPARATOR namespace_name 	{ SelfNamespace $3 }
	|	T_NS_SEPARATOR namespace_name 			{ AbsoluteNamespace $2 }
;

fully_qualified_class_name:
		namespace_name					 { RelativeNamespace $1 }
	|	T_NAMESPACE T_NS_SEPARATOR namespace_name	{ SelfNamespace $3 } 
	|	T_NS_SEPARATOR namespace_name 			{ AbsoluteNamespace $2 }
;



class_name_reference:
		class_name					{ $1 }
	|	dynamic_class_name_reference			{ $1 }
;


dynamic_class_name_reference:
		base_variable T_OBJECT_OPERATOR object_property  dynamic_class_name_variable_properties 
					{ DynamicClassName3 $1 $3 (reverse $4) }		
	|	base_variable 		{ DynamicClassName1 $1 }
;


dynamic_class_name_variable_properties:
		dynamic_class_name_variable_properties dynamic_class_name_variable_property
					{ (DynClassNameVarProp $2 $3):$1 } 
	|	{- empty -}		{ [] }
;


dynamic_class_name_variable_property:
		T_OBJECT_OPERATOR object_property 	{ $2 }
;

exit_expr:
		{- empty -}		{ ExitEmpty }
	|	'(' ')'			{ ExitAlmostEmpty }
	|	parenthesis_expr	{ ExitNotEmpty $1 }
;

backticks_expr:
		{- empty -}			{ BacktickEmpty }
	|	T_ENCAPSED_AND_WHITESPACE	{ $1 }
	|	encaps_list			{ $1 }
;


ctor_arguments:
		{- empty -}			{ [] }
	|	function_call_parameter_list 	{ $1 }
;


common_scalar:
		T_LNUMBER 			{ Constant $1 }		
	|	T_DNUMBER 			{ Constant $1 }		
	|	T_CONSTANT_ENCAPSED_STRING	{ Constant $1 }
	|	T_LINE 				{ Magic $1 }		
	|	T_FILE 				{ Magic $1 }		
	|	T_DIR   			{ Magic $1 }		
	|	T_TRAIT_C			{ Magic $1 }		
	|	T_METHOD_C			{ Magic $1 }		
	|	T_FUNC_C			{ Magic $1 }		
	|	T_NS_C				{ Magic $1 }		
	|	T_START_HEREDOC T_ENCAPSED_AND_WHITESPACE T_END_HEREDOC
						{ Constant $2 } 
	|	T_START_HEREDOC T_END_HEREDOC 	{ Constant "" }
;


static_scalar: 
		common_scalar			{ $1 }
	|	static_class_name_scalar	{ $1 }
	|	namespace_name 			{ NamespaceRelative $1 }
	|	T_NAMESPACE T_NS_SEPARATOR namespace_name { NamespaceSelf $3 }
	|	T_NS_SEPARATOR namespace_name 	{ NamespaceAbsolute $2 }
	|	'+' static_scalar 		{ UnaryPlus $2 }
	|	'-' static_scalar 		{ UnaryMinus $2 }
	|	T_ARRAY '(' static_array_pair_list ')' { Array $3 }
	|	'[' static_array_pair_list ']' 	{ Array $2 }
	|	static_class_constant 		{ $2 }
	|	T_CLASS_C			{ Magic $1 }
;

static_class_constant:
		class_name '::' IDENT 		{ ClassConstant $1 $3 }
;

scalar:
		IDENT_VARNAME			{ Variable $1 }		
	|	class_name_scalar		{ ClassScalar $1 }
	|	class_constant			{ ClassConstant $1 } 
	|	namespace_name			{ NameSpaceRelative $1 }
	|	T_NAMESPACE T_NS_SEPARATOR namespace_name { NameSpaceSelf $3 }
	|	T_NS_SEPARATOR namespace_name 	{ NameSpaceAbsolute $2 }
	|	common_scalar			{ $1 }
	|	'"' encaps_list '"' 		{ String $2 }
	|	T_START_HEREDOC encaps_list T_END_HEREDOC { String $2 }
	|	T_CLASS_C			{ Magic $1 }
;


static_array_pair_list:
		{- empty -}			{ [] } 
	|	non_empty_static_array_pair_list possible_comma	{ reverse $1 }	
;

possible_comma:
		{- empty -}	{}
	|	','		{}
;

non_empty_static_array_pair_list:
		non_empty_static_array_pair_list ',' static_scalar T_DOUBLE_ARROW static_scalar	{ (ArrayPairKV $3 $5):$1 }	
	|	non_empty_static_array_pair_list ',' static_scalar 				{ (ArrayPairV $1):$3 }
	|	static_scalar T_DOUBLE_ARROW static_scalar 					{ [ArrayPairKV $1 $3] }
	|	static_scalar 									{ [ArrayPairV $1] }
;

expr:
		r_variable		{ $1 }
	|	expr_without_variable	{ $1 }	
;

parenthesis_expr:
		'(' expr ')'		{ $2 }
	|	'(' yield_expr ')'	{ $2 }
;


r_variable:
	variable 	{ $1 }
;


w_variable:
	variable	{ $1 }
;

rw_variable:
	variable	{ $1 }
;

variable:
		base_variable_with_function_calls T_OBJECT_OPERATOR object_property  method_or_not variable_properties
				{ VariableWithArrow $1 $3 $4 (reverse $5) }
	|	base_variable_with_function_calls 
				{ VariableWithoutArrow $1 }
;

variable_properties:
		variable_properties variable_property 	{ ($2:$1) }
	|	{- empty -} 				{ [] }
;


variable_property:
		T_OBJECT_OPERATOR object_property  method_or_not 	{ VariableProperty $1 $2 }
;

array_method_dereference:
		array_method_dereference '[' dim_offset ']'		{ (\(ArrayMethodDereference m x) -> ArrayMethodDereference m ($3:x)) $1 } 
	|	method '[' dim_offset ']' 				{ ArrayMethodDereference $1 [$3] }
;

method:
		
		function_call_parameter_list 		{ $1 }
;

method_or_not:
		method					{ Method $1 }	
	|	array_method_dereference		{ $1 }
	|	{- empty -}				{ NotMethod } 
;

variable_without_objects:
		reference_variable 			{ ReferenceVariable $1 }
	|	simple_indirect_reference reference_variable 	{ IndirectReference $1 }
;

static_member:
		class_name '::' variable_without_objects 		{ StaticMember $1 $3 }
	|	variable_class_name '::' variable_without_objects 	{ StaticMemberDynamic $1 $3 }

;

variable_class_name:
		reference_variable	{ $1 } 
;

array_function_dereference:
		array_function_dereference '[' dim_offset ']' { (\(ArrayFunctionDereference f x) -> ArrayFunctionDereference f ($3:x)) $1 }
	|	function_call '[' dim_offset ']'	      { ArrayFunctionDereference $1 [$3] }
;

base_variable_with_function_calls:
		base_variable			{ BaseVariable $1 }	
	|	array_function_dereference	{ $1 }
	|	function_call 			{ $1 }
;


base_variable:
		reference_variable 		{ $1 }
	|	simple_indirect_reference reference_variable	{ IndirectReference $1 } 
	|	static_member 			{ $1 }
;

reference_variable:
		reference_variable '[' dim_offset ']'	{ (\(ReferenceVariable v x) -> ReferenceVariable v ($3:(Offset x))) $1 }
	|	reference_variable '{' expr '}'		{ (\(ReferenceVariable v x) -> ReferenceVariable v ($3:(Index x)))  $1 }
	|	compound_variable			{ ReferenceVariable $1 [] }
;


compound_variable:
		T_VARIABLE		{ Variable $1 }
	|	'$' '{' expr '}'	{ Indirect $3 }
;

dim_offset:
		{- empty -}		{ OffsetEmpty }
	|	expr			{ Offset $1 }
;


object_property:
		object_dim_list 		{ $1 }
	|	variable_without_objects  	{ $1 }
;

object_dim_list:
		object_dim_list '[' dim_offset ']'	{ (\DimList v x) -> DimList v ($3:(Offset x))) $1 }
	|	object_dim_list '{' expr '}'		{ (\DimList v x) -> DimList v ($3:(Index x)))  $1 }
	|	variable_name 				{ DimList v [] }
;

variable_name:
		IDENT			{ Variable $1 }
	|	'{' expr '}'		{ Indirect $2 }
;

simple_indirect_reference:
		'$' 				{ Indirection }
	|	simple_indirect_reference '$'	{ Indirection $1 } 
;

assignment_list:
		assignment_list ',' assignment_list_element	{ $3:$1 }
	|	assignment_list_element				{ [$1] }
;


assignment_list_element:
		variable					{ VariableElement $1 }			
	|	T_LIST '('  assignment_list ')'			{ ListElement $3 }
	|	{- empty -}					{ EmptyElement }		
;


array_pair_list:
		{- empty -} 					{ [] }
	|	non_empty_array_pair_list possible_comma	{ reverse $1 }
;

non_empty_array_pair_list:
		non_empty_array_pair_list ',' expr T_DOUBLE_ARROW expr	
				{ (ArrayPairKV $3 $5) : $1 }
	|	non_empty_array_pair_list ',' expr
				{ (ArrayPairV $3) : $1 }			
	|	expr T_DOUBLE_ARROW expr
				{ (ArrayPairKV $3 $5) : [] }	
	|	expr 		{ (ArrayPairV $1) : [] }  		
	|	non_empty_array_pair_list ',' expr T_DOUBLE_ARROW '&' w_variable
				{ (ArrayPairKR $3 $6) : $1 } 
	|	non_empty_array_pair_list ',' '&' w_variable 
				{ (ArrayPairR $4) : $1 }
	|	expr T_DOUBLE_ARROW '&' w_variable
				{ (ArrayPairKR $1 $4) : [] }	
	|	'&' w_variable	{ (ArrayPairR $2) : [] } 			
;

encaps_list:
		encaps_list encaps_var 			{ (EncapsVar $2) : $1 }
	|	encaps_list T_ENCAPSED_AND_WHITESPACE	{ (Encaps $2) : $1 }
	|	encaps_var 				{ (EncapsVar $1) : [] }
	|	T_ENCAPSED_AND_WHITESPACE encaps_var	{ (Encaps $1) : [] }
;

encaps_var:
		T_VARIABLE 				{ Variable $1 }
	|	T_VARIABLE '['  encaps_var_offset ']'	{ VariableOffset $1 $3 }
	|	T_VARIABLE T_OBJECT_OPERATOR IDENT 	{ VariableProperty $1 $3 } 
	|	T_DOLLAR_OPEN_CURLY_BRACES expr '}' 	{ EncapsExpr $2 }
	|	T_DOLLAR_OPEN_CURLY_BRACES IDENT_VARNAME '[' expr ']' '}' 
							{ VariableOffsetE $2 $4 }
	|	T_CURLY_OPEN variable '}' 		{ VariableAlt $2 }
;

encaps_var_offset:
		IDENT		{ Ident $$ }		
	|	T_NUM_STRING	{ Constant $$ }
	|	T_VARIABLE	{ Variable $$ }	
;

internal_functions_in_yacc:
		T_ISSET '(' isset_variables ')'	{ IsSet (reverse $3) }  
	|	T_EMPTY '(' variable ')'	{ Empty $3 }
	|	T_EMPTY '(' expr_without_variable ')' 
						{ Empty $3 }
	|	T_INCLUDE expr 			{ Include $2 }
	|	T_INCLUDE_ONCE expr 		{ IncludeOnce $2 }
	|	T_EVAL '(' expr ')' 		{ Eval $3 }
	|	T_REQUIRE expr			{ Require $2 }
	|	T_REQUIRE_ONCE expr		{ RequireOnce $2 }
;

isset_variables:
		isset_variable				{ $1 : [] }
	|	isset_variables ','  isset_variable 	{ $3 : $1 }
;

isset_variable:
		variable		{ $1 }
	|	expr_without_variable	{ $1 }
;

class_constant:
		class_name '::' IDENT 		{ ClassConstant $1 $3 }
	|	variable_class_name '::' IDENT 	{ ClassConstant $1 $3 }
;

static_class_name_scalar:
	class_name '::' T_CLASS { ClassNameScalar $1 }
;

class_name_scalar:				
	class_name '::' T_CLASS { ClassNameScalar $1 }
;


{

happyError :: [Token] -> a
happyError _ = error ("Parse error\n")

data Prase

data Token
  = TokenLet
  | TokenIn
  | TokenInt Int
  | TokenVar String
  | TokenEq
  | TokenPlus
  | TokenMinus
  | TokenTimes
  | TokenDiv
  | TokenOB
  | TokenCB

data ParseResult a = Ok a | Fail String
newtype P a = P (AlexState -> [Token] -> ParseResult a)
runP :: P a -> AlexState -> [Token] -> ParseResult a
runP (P f) = f

instance Monad P where
  return m = P $ \ _ _ -> Ok m
  m >>= k =  P $ \s st -> case runP m s st of Ok a -> runP (k a) s st
                                              Fail err -> FailP err
  fail s = P $ \ _ _ -> FailP s

}