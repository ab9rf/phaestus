{
module PHPParse where

import PHPLex (AlexState, Token(..), mLexer)
}

%name phpParse start
%tokentype { Token }

%token INLINE_HTML 	{ InlineHTML $$ }

%token CAST_INT		{ CastInt }
%token CAST_REAL	{ CastReal }
%token CAST_STRING	{ CastString }
%token CAST_ARRAY	{ CastArray }
%token CAST_OBJECT	{ CastObject }
%token CAST_BOOL	{ CastBool }
%token CAST_UNSET	{ CastUnset }

%token OP_EQ_EQ        	{ OpEqEq  }
%token OP_EQ_EQ_EQ     	{ OpEqEqEq  }
%token OP_NOT_EQ       	{ OpNotEq  }
%token OP_NOT_EQ_EQ    	{ OpNotEqEq  }
%token OP_LE	  	{ OpLE  }
%token OP_GE	  	{ OpGE  }
%token OP_INC  		{ OpInc  }
%token OP_DEC  		{ OpDec  }
%token OP_DOUBLE_ARROW 	{ OpDoubleArrow  }
%token OP_SINGLE_ARROW 	{ OpSingleArrow  }
%token OP_SL	  	{ OpSL  }
%token OP_SR	  	{ OpSR  }
%token OP_PLUS_EQ      	{ OpPlusEq  }
%token OP_MINUS_EQ     	{ OpMinusEq  }
%token OP_MULT_EQ      	{ OpMultEq  }
%token OP_DIV_EQ       	{ OpDivEq  }
%token OP_CONCAT_EQ    	{ OpConcatEq  }
%token OP_MOD_EQ       	{ OpModEq  }
%token OP_AND_EQ       	{ OpAndEq  }
%token OP_OR_EQ        	{ OpOrEq  }
%token OP_XOR_EQ       	{ OpXorEq  }
%token OP_SL_EQ       	{ OpSLEq  }
%token OP_SR_EQ       	{ OpSREq  }
%token OP_COLON_COLON  	{ OpColonColon  }
%token OP_LOGIC_AND    	{ OpLogicAnd  }
%token OP_LOGIC_OR     	{ OpLogicOr  }
%token OP_PLUS 		{ OpPlus  }
%token OP_MINUS        	{ OpMinus  }
%token OP_SLASH        	{ OpSlash  }
%token OP_STAR 		{ OpStar  }
%token OP_PERCENT      	{ OpPercent  }
%token OP_CARET        	{ OpCaret  }
%token OP_AMPERSAND    	{ OpAmpersand  }
%token OP_PIPE 		{ OpPipe  }
%token OP_TILDE        	{ OpTilde  }
%token OP_EQ   		{ OpEq  }
%token OP_LT   		{ OpLt  }
%token OP_GT   		{ OpGt  }
%token OP_DOT  		{ OpDot  }
%token OP_BANG 		{ OpBang  }
%token OP_COMMA        	{ OpComma  }
%token OP_QUESTION     	{ OpQuestion  }
%token OP_COLON        	{ OpColon  }
%token OP_AT_SIGN      	{ OpAtSign  }
%token OP_DOLLARS      	{ OpDollars  }

%token SEMICOLON       	{ Semicolon  }
%token L_PAREN 		{ LParen  }
%token R_PAREN 		{ RParen  }
%token L_BRACE 		{ LBrace  }
%token R_BRACE 		{ RBrace  }
%token L_BRACKET       	{ LBracket  }
%token R_BRACKET       	{ RBracket  }

%token OP      		{ Op String }
%token VARIABLE        	{ Variable String }
%token IDENT   		{ Ident String }

%token KEYWORD_AND     	{ KeywordAnd  }
%token KEYWORD_OR      	{ KeywordOr  }
%token KEYWORD_XOR     	{ KeywordXor  }
%token KEYWORD___FILE__ { Keyword__FILE__  }
%token KEYWORD___LINE__ { Keyword__LINE__  }
%token KEYWORD_ARRAY   	{ KeywordArray  }
%token KEYWORD_AS      	{ KeywordAs  }
%token KEYWORD_BREAK   	{ KeywordBreak  }
%token KEYWORD_CASE    	{ KeywordCase  }
%token KEYWORD_CLASS   	{ KeywordClass  }
%token KEYWORD_CONST   	{ KeywordConst  }
%token KEYWORD_CONTINUE { KeywordContinue  }
%token KEYWORD_DECLARE 	{ KeywordDeclare  }
%token KEYWORD_DEFAULT 	{ KeywordDefault  }
%token KEYWORD_DO      	{ KeywordDo  }
%token KEYWORD_ECHO    	{ KeywordEcho  }
%token KEYWORD_ELSE    	{ KeywordElse  }
%token KEYWORD_ELSEIF  	{ KeywordElseif  }
%token KEYWORD_EMPTY   	{ KeywordEmpty  }
%token KEYWORD_ENDDECLARE { KeywordEnddeclare  }
%token KEYWORD_ENDFOR  	{ KeywordEndfor  }
%token KEYWORD_ENDFOREACH { KeywordEndforeach  }
%token KEYWORD_ENDIF   	{ KeywordEndif  }
%token KEYWORD_ENDSWITCH  { KeywordEndswitch  }
%token KEYWORD_ENDWHILE { KeywordEndwhile  }
%token KEYWORD_EVAL    	{ KeywordEval  }
%token KEYWORD_EXIT    	{ KeywordExit  }
%token KEYWORD_DIE     	{ KeywordDie  }
%token KEYWORD_EXTENDS 	{ KeywordExtends  }
%token KEYWORD_FOR     	{ KeywordFor  }
%token KEYWORD_FOREACH 	{ KeywordForeach  }
%token KEYWORD_FUNCTION { KeywordFunction  }
%token KEYWORD_GLOBAL  	{ KeywordGlobal  }
%token KEYWORD_IF      	{ KeywordIf  }
%token KEYWORD_INCLUDE 	{ KeywordInclude  }
%token KEYWORD_INCLUDE_ONCE    { KeywordIncludeOnce  }
%token KEYWORD_INSTANCE_OF     { KeywordInstanceOf  }
%token KEYWORD_ISSET   	{ KeywordIsset  }
%token KEYWORD_LIST    	{ KeywordList  }
%token KEYWORD_NEW     	{ KeywordNew  }
%token KEYWORD_PRINT   	{ KeywordPrint  }
%token KEYWORD_REQUIRE 	{ KeywordRequire  }
%token KEYWORD_REQUIRE_ONCE    { KeywordRequireOnce  }
%token KEYWORD_RETURN  	{ KeywordReturn  }
%token KEYWORD_STATIC  	{ KeywordStatic  }
%token KEYWORD_SWITCH  	{ KeywordSwitch  }
%token KEYWORD_UNSET   	{ KeywordUnset  }
%token KEYWORD_USE     	{ KeywordUse  }
%token KEYWORD_VAR     	{ KeywordVar  }
%token KEYWORD_WHILE   	{ KeywordWhile  }
%token KEYWORD___FUNCTION__     { Keyword__FUNCTION__  }
%token KEYWORD___CLASS__ 	{ Keyword__CLASS__  }
%token KEYWORD___METHOD__ 	{ Keyword__METHOD__  }
%token KEYWORD_FINAL   	{ KeywordFinal  }
%token KEYWORD_INTERFACE	{ KeywordInterface  }
%token KEYWORD_IMPLEMENT        { KeywordImplement  }
%token KEYWORD_PUBLIC  	{ KeywordPublic  }
%token KEYWORD_PRIVATE 	{ KeywordPrivate  }
%token KEYWORD_PROTECTED       	{ KeywordProtected  }
%token KEYWORD_ABSTRACT        	{ KeywordAbstract  }
%token KEYWORD_CLONE   	{ KeywordClone  }
%token KEYWORD_TRY     	{ KeywordTry  }
%token KEYWORD_CATCH   	{ KeywordCatch  }
%token KEYWORD_THROW   	{ KeywordThrow  }
%token KEYWORD_CFUNCTION       	{ KeywordCfunction  }
%token KEYWORD_OLD_FUNCTION    	{ KeywordOldFunction  }
%token KEYWORD_TRUE    	{ KeywordTrue  }
%token KEYWORD_FALSE   	{ KeywordFalse  }
%token KEYWORD_NULL    	{ KeywordNull  }

%token INTEGER   	{ PHPInteger String }
%token REAL      	{ PHPReal String }
%token STRING    	{ PHPString String }
       
%lexer { mLexer } { EOF }
%monad { P }

%%

start : 	stmt_list		{ $1 }

stmt_list :	stmt_list stmt		{ StmtList $2 $1 }
	|	{- empty -}		{ Nop }

stmt	: 	INLINE_HTML		{ Echo $1 }
	|	KEYWORD_IF LPAREN expr RPAREN stmt elseif_list else_single
					{ If $3 $5 $6 $7 }
	|	KEYWORD_IF LPAREN expr RPAREN OP_COLON stmt_list elseif_list_alt else_single_alt KEYWORD_ENDIF SEMICOLON
					{ If $3 $6 $7 $8 }
	|	KEYWORD_WHILE LPAREN expr RPAREN stmt
					{ While $3 $5 }
	|	KEYWORD_WHILE LPAREN OP_COLON stmt_list KEYWORD_ENDWHILE SEMICOLON
					{ While $3 $6 }
	|	KEYWORD_DO stmt KEYWORD_WHILE expr SEMICOLON	 
					{ Do $2 $4 }
	|	KEYWORD_FOR LPAREN expr SEMICOLON expr SEMICOLON expr RPAREN stmt 
					{ For $3 $5 $7 $9 }
	|	KEYWORD_FOR LPAREN expr SEMICOLON expr SEMICOLON expr RPAREN OP_COLON stmt_list KEYWORD_ENDFOR SEMICOLON 
					{ For $3 $5 $7 $10 }
	|	KEYWORD_FOREACH LPAREN foreach_header RPAREN stmt 
					{ Foreach $2 $3 }
	|	KEYWORD_FOREACH LPAREN foreach_header RPAREN OP_COLON stmt_list KEYWORD_ENDFOREACH SEMICOLON
					{ Foreach $2 $4 }
	|	KEYWORD_BREAK INTEGER	{ Break $2 }
	|	KEYWORD_BREAK		{ Break1 }
	|	KEYWORD_CONTINUE INTEGER{ Continue $2 }
	|	KEYWORD_CONTINUE	{ Continue1 }
	|	KEYWORD_SWITCH LPAREN expr RPAREN LBRACE case_list RBRACE 
					{ Switch $3 $5 }						
	|	KEYWORD_SWITCH LPAREN expr RPAREN COLON case_list KEYWORD_ENDSWITCH
					{ Switch $3 $5 }						

foreach_header:	expr KEYWORD_AS variable	
					{ ForeachHeader $1 $3 }
	|	expr KEYWORD_AS OP_AMPERSAND variable
					{ ForeachHeaderR $1 $4 }
	|	expr KEYWORD_AS variable OP_DOUBLE_ARROW variable
					{ ForeachHeader2 $1 $3 $5 }
	|	expr KEYWORD_AS variable OP_DOUBLE_ARROW OP_AMPERSAND variable
					{ ForeachHeader2R $1 $3 $6 }
					
case_list:	case_list case_stmt	{ CaseList $2 $1 }
	|	{- empty -}		{ Nop }

case_stmt:	stmt			{ $1 }
	|	KEYWORD_CASE expr case_sep
					{ Case $2 }
	|	KEYWORD_DEFAULT case_sep
					{ CaseDefault }
case_sep:	OP_COLON		{ }
	|	SEMICOLON		{ }


 


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