{
module PHPLex (Token(..), lexer) where

import Data.Char (toLower, chr)
import Data.List (isPrefixOf) 

}

%wrapper "monadUserState"

@NL                             = \r?\n?
@WS                             = [\ \t\n\r]
@ANY                            = [\x00-\xff]     

@PHP                            = [pP][hH][pP]
@PHP_START                      = "<?" @PHP?
@SCRIPT_START                   = 
                "<script" @WS+ "language" @WS* "=" @WS* ['\"] @WS* @PHP @WS* ['\"] @WS* ">"
@START                          = @PHP_START | @SCRIPT_START
@START_ECHO                     = "<?="
@PHP_STOP                       = "?>"
@SCRIPT_STOP                    = "</script" @WS* ">"
@STOP                           = (@PHP_STOP | @SCRIPT_STOP) @NL?

@IDENT                          = [a-zA-Z_\x7F-\xFF][a-zA-Z0-9_\x7F-\xFF]*

@DEC                            = ([1-9][0-9]*)|0
@HEX                            = 0[xX][0-9a-fA-F]+
@OCT                            = 0[0-7]+
@INT                            = (@DEC|@HEX|@OCT)

@LNUM                           = [0-9]+
@DNUM                           = ([0-9]*[\.]@LNUM)|(@LNUM[\.][0-9]*)
@EXPONENT_DNUM                  = ((@LNUM|@DNUM)[eE](\+|\-)?@LNUM)
@REAL                           = @DNUM|@EXPONENT_DNUM

@BRACKET                        = [ \(\) \{\} \[\] ]
@ARITHMETIC                     = [ \+ \- \/ \* \% \^ ]
@BITWISE                        = [ \& \| \~ ]
@RELATIONAL                     = [ \= \> \< ]
@OTHER_OP                       = [ \. \! \, \? \: \@ \$ ]
@SIMPLE_OP                      = @BRACKET|@ARITHMETIC|@BITWISE|@RELATIONAL|@OTHER_OP

@CAST_WS                        = [\t ]
@CS                             = "("@CAST_WS*
@CE                             = @CAST_WS*")"

@C_INTEGER                      = [iI][nN][tT][eE][gG][eE][rR]
@C_INT                          = [iI][nN][tT]
@INT_CAST                       = @CS(@C_INTEGER|@C_INT)@CE

@C_FLOAT                        = [fF][lL][oO][aA][tT]
@C_REAL                         = [rR][eE][aA][lL]
@C_DOUBLE                       = [dD][oO][uU][bB][lL][eE]
@REAL_CAST                      = @CS(@C_FLOAT|@C_REAL|@C_DOUBLE)@CE

@C_STRING                       = [sS][tT][rR][iI][nN][gG]
@STRING_CAST                    = @CS@C_STRING@CE 

@C_ARRAY                        = [aA][rR][rR][aA][yY]
@ARRAY_CAST                     = @CS@C_ARRAY@CE

@C_OBJECT                       = [oO][bB][jJ][eE][cC][tT]
@OBJECT_CAST                    = @CS@C_OBJECT@CE

@C_BOOL                         = [bB][oO][oO][lL]
@C_BOOLEAN                      = [bB][oO][oO][lL][eE][aA][nN]
@BOOL_CAST                      = @CS(@C_BOOL|@C_BOOLEAN)@CE

@C_UNSET                        = [uU][nN][sS][eE][tT]
@UNSET_CAST                     = @CS@C_UNSET@CE

tokens :-

<0> @START_ECHO    { \(_,_,_,inp) len -> do ret <- getPushBack;
                                            clearPushBack; 
                                            alexSetStartCode php;
                                            case ret of "" -> return [Op ";", KeywordEcho]
                                                        _  -> return [InlineHTML ret, KeywordEcho]
                   }
<0> @START           { \(_,_,_,inp) len -> do ret <- getPushBack;
                                            clearPushBack; 
                                            alexSetStartCode php;
                                            case ret of "" -> alexMonadScan
                                                        _  -> return [InlineHTML ret]
                   }                                  
<0>  @ANY          { \(_,_,_,inp) len -> do addToPushBack (head inp); alexMonadScan }

-- casts --
 
<php> @INT_CAST    { go CastInt }
<php> @REAL_CAST   { go CastReal }
<php> @STRING_CAST { go CastString }
<php> @ARRAY_CAST  { go CastArray }
<php> @OBJECT_CAST { go CastObject }
<php> @BOOL_CAST   { go CastBool }
<php> @UNSET_CAST  { go CastUnset }  

-- operators --

<php> "=="         { go OpEqEq }
<php> "==="        { go OpEqEqEq }
<php> "!="         { go OpNotEq }
<php> "<>"         { go OpNotEq }
<php> "!=="        { go OpNotEqEq }
<php> "<="         { go OpLE }
<php> ">="         { go OpGE }

<php> "++"         { go OpInc }
<php> "--"         { go OpDec }
<php> "=>"         { go OpDoubleArrow }
<php> "->"         { go OpSingleArrow }

<php> "<<"         { go OpSL }
<php> ">>"         { go OpSR }

<php> "+="         { go OpPlusEq }
<php> "-="         { go OpMinusEq }
<php> "*="         { go OpMultEq }
<php> "/="         { go OpDivEq }
<php> ".="         { go OpConcatEq }
<php> "%="         { go OpModEq }
<php> "&="         { go OpAndEq }
<php> "|="         { go OpOrEq }
<php> "^="         { go OpXorEq }
<php> "<<="        { go OpSLEq }
<php> ">>="        { go OpSREq }
<php> "::"         { go OpColonColon }

<php> "&&"         { go OpLogicAnd }
<php> "||"         { go OpLogicOr }

<php> @SIMPLE_OP   { goStr Op }
<php> ";"          { goStr Op } 

-- tokens --

<php> "$" @IDENT   { variable }
<php> @IDENT       { keywordOrIdent }
<php> @INT         { goStr PHPInteger }
<php> @REAL        { goStr PHPReal }
<php> @STOP           { \input len -> do clearPushBack; alexSetStartCode 0; return [Op ";"] } 

-- strings --
<php> \'           { \input len -> do clearPushBack; alexSetStartCode sqStr; alexMonadScan }
<php> \`           { \input len -> do clearPushBack; alexSetStartCode btStr; return [Ident "`", Op "("] }
<php> \"           { \input len -> do clearPushBack; alexSetStartCode dqStr; alexMonadScan }
<php> "<<<" " "?   { \input len -> do clearPushBack; alexSetStartCode hdStr; alexMonadScan }

-- comments --
<php> ^[\ \t]*\n   ;
<php> "/*"           { begin mlComm }
<php> "#" | "//"   { begin slComm }

<mlComm> "*/"           { begin php }
<mlComm> @ANY      ;

<slComm> @NL           { begin php }
<slComm> @PHP_STOP { \input len -> do clearPushBack; alexSetStartCode 0; alexMonadScan  }
<slComm> .           ;

-- any other character --
<php> @WS            ;
<php> @ANY           { goStr Invalid }

-- singly-quoted strings --
<sqStr> \'           { \input len -> do str <- getPushBack; clearPushBack; alexSetStartCode php; return [(PHPString str)] }
<sqStr> \\           { \(_,_,_,inp) len -> do alexSetStartCode sqEsc; alexMonadScan }
<sqStr> @ANY           { \(_,_,_,inp) len -> do addToPushBack (head inp); alexMonadScan }

<sqEsc> \'           { \(_,_,_,inp) len -> do addToPushBack (head inp); alexSetStartCode sqStr; alexMonadScan }
<sqEsc> \\           { \(_,_,_,inp) len -> do addToPushBack (head inp); alexSetStartCode sqStr; alexMonadScan }
<sqEsc> @ANY           { \(_,_,_,inp) len -> do addToPushBack '\\'; addToPushBack (head inp); alexSetStartCode sqStr; alexMonadScan }

-- backticked string --
<btStr> \`           { \input len -> do str <- getPushBack; clearPushBack; alexSetStartCode php; return [PHPString str, Op ")"] }
<btStr> \\\`           { \input len -> do addToPushBack '`'; alexMonadScan }
<btStr> \\           { \input len -> do pushState escape; alexMonadScan }
<btStr> @ANY           { \(_,_,_,inp) len -> do addToPushBack (head inp); alexMonadScan }

-- in-string syntax --
<dqStr,hdMain,btStr> "$" @IDENT         
                   { \(_,_,_,inp) len -> do str <- getPushBack; clearPushBack; return [PHPString str, Op ".", Variable (tail (take len inp)), Op "."] }

<dqStr,hdMain,btStr> "${" @IDENT "}"         
                   { \(_,_,_,inp) len -> do str <- getPushBack; clearPushBack; return [PHPString str, Op ".", Variable (tail (tail (take (len - 1) inp))), Op "."] }

<dqStr,hdMain,btStr> "$" @IDENT "[" @INT "]" { quotedArrayIntIdx }
<dqStr,hdMain,btStr> "$" @IDENT "[" @IDENT "]" { quotedArrayStrIdx } 
<dqStr,hdMain,btStr> "$" @IDENT "[$" @IDENT "]" { quotedArrayVarIdx } 
<dqStr,hdMain,btStr> "$" @IDENT "->" @IDENT "]" { quotedMethodCall } 

<escape> n           { \ inp len -> do addToPushBack '\n'; popState; alexMonadScan }
<escape> t           { \ inp len -> do addToPushBack '\t'; popState; alexMonadScan }
<escape> r           { \ inp len -> do addToPushBack '\r'; popState; alexMonadScan }                    
<escape> \\           { \ inp len -> do addToPushBack '\\'; popState; alexMonadScan }
<escape> \$           { \ inp len -> do addToPushBack '$'; popState; alexMonadScan }
<escape> x[0-9A-Fa-f]{1,2}
                   { \(_,_,_,inp) len -> do addToPushBack (chr (fromHex (take len inp))); popState; alexMonadScan }
<escape> [0-7]{1,3}
                   { \(_,_,_,inp) len -> do addToPushBack (chr (fromOctal (take len inp))); popState; alexMonadScan }
<escape> @ANY           { \(_,_,_,inp) len -> do addToPushBack '\\'; addToPushBack (head inp); popState; alexMonadScan }

-- double-quoted strings --

<dqStr> \"           { \input len -> do str <- getPushBack; clearPushBack; alexSetStartCode php; return [(PHPString str)] }
<dqStr> \\\"           { \input len -> do addToPushBack '"'; alexMonadScan }
<dqStr> \\           { \input len -> do pushState escape; alexMonadScan }
<dqStr> @ANY       { \(_,_,_,inp) len -> do addToPushBack (head inp); alexMonadScan }

-- heredoc syntax --

<hdStr> @IDENT      { \(_,_,_,inp) len -> do setHeredocId (take len inp); alexSetStartCode hdNl; alexMonadScan }
<hdStr> .           { \(_,_,_,inp) len -> do alexSetStartCode php; return [Invalid (take len inp)] }

<hdNl> @NL          { begin hdMain }
<hdNl> .            { goStr Invalid }

<hdMain> \\         { \input len -> do pushState escape; alexMonadScan }
<hdMain> @ANY       { hereDocAny }                     

{
data Token = 
        InlineHTML String |
        CastInt | CastReal | CastString | CastArray | CastObject | CastBool | CastUnset | 
        OpEqEq | OpEqEqEq | OpNotEq | OpNotEqEq | OpLE | OpGE | OpInc | OpDec | 
        OpDoubleArrow | OpSingleArrow | OpSL | OpSR | OpPlusEq | OpMinusEq | OpMultEq | 
        OpDivEq | OpConcatEq | OpModEq | OpAndEq | OpOrEq | OpXorEq | OpSLEq | OpSREq | 
        OpColonColon | OpLogicAnd | OpLogicOr | Op String |
        Variable String | Ident String |  
        KeywordAnd | KeywordOr | KeywordXor | Keyword__FILE__ | Keyword__LINE__ | 
        KeywordArray | KeywordAs | KeywordBreak | KeywordCase | KeywordClass | 
        KeywordConst | KeywordContinue | KeywordDeclare | KeywordDefault | 
        KeywordDo | KeywordEcho | KeywordElse | KeywordElseif | KeywordEmpty | 
        KeywordEnddeclare | KeywordEndfor | KeywordEndforeach | KeywordEndif | 
        KeywordEndswitch | KeywordEndwhile | KeywordEval | KeywordExit | 
        KeywordDie | KeywordExtends | KeywordFor | KeywordForeach | 
        KeywordFunction | KeywordGlobal | KeywordIf | KeywordInclude | 
        KeywordIncludeOnce | KeywordInstanceOf | KeywordIsset | KeywordList | 
        KeywordNew | KeywordPrint | KeywordRequire | KeywordRequireOnce | 
        KeywordReturn | KeywordStatic | KeywordSwitch | KeywordUnset | 
        KeywordUse | KeywordVar | KeywordWhile | Keyword__FUNCTION__ | 
        Keyword__CLASS__ | Keyword__METHOD__ | KeywordFinal | KeywordInterface | 
        KeywordImplement | KeywordPublic | KeywordPrivate | KeywordProtected | 
        KeywordAbstract | KeywordClone | KeywordTry | KeywordCatch | 
        KeywordThrow | KeywordCfunction | KeywordOldFunction | KeywordTrue | 
        KeywordFalse | KeywordNull |
        PHPInteger String | PHPReal String | PHPString String | 
        ERROR | Invalid String | EOF
        deriving (Eq,Show)

alexEOF = do str <- getPushBack;
             clearPushBack; 
             case str of "" -> return [EOF]
                         _  -> return [InlineHTML str, EOF]

go cstr = \ _ len -> return $ [cstr] 
goStr cstr = \ (_,_,_,inp) len -> return $ [cstr (take len inp)]

variable (_,_,_,inp) len = return $ [Variable (tail (take len inp))]

keywordOrIdent (posn,_,_,inp) len =
  return $ [keyword (toLowerStr str)]
    where str = (take len inp)
          toLowerStr s = map toLower s
          keyword "and"                = KeywordAnd  
          keyword "or"            = KeywordOr
          keyword "xor"           = KeywordXor
          keyword "__FILE__"         = Keyword__FILE__
          keyword "__LINE__"         = Keyword__LINE__
          keyword "array"         = KeywordArray
          keyword "as"                 = KeywordAs
          keyword "break"         = KeywordBreak
          keyword "case"                 = KeywordCase
          keyword "class"         = KeywordClass
          keyword "const"         = KeywordConst
          keyword "continue"         = KeywordContinue
          keyword "declare"         = KeywordDeclare
          keyword "default"         = KeywordDefault
          keyword "do"                 = KeywordDo
          keyword "echo"                 = KeywordEcho
          keyword "else"                 = KeywordElse
          keyword "elseif"         = KeywordElseif
          keyword "empty"         = KeywordEmpty
          keyword "enddeclare"         = KeywordEnddeclare
          keyword "endfor"         = KeywordEndfor
          keyword "endforeach"         = KeywordEndforeach
          keyword "endif"         = KeywordEndif
          keyword "endswitch"         = KeywordEndswitch
          keyword "endwhile"         = KeywordEndwhile
          keyword "eval"                 = KeywordEval
          keyword "exit"                 = KeywordExit
          keyword "die"                 = KeywordDie
          keyword "extends"         = KeywordExtends
          keyword "for"                 = KeywordFor
          keyword "foreach"         = KeywordForeach
          keyword "function"         = KeywordFunction
          keyword "global"         = KeywordGlobal
          keyword "if"                 = KeywordIf
          keyword "include"         = KeywordInclude
          keyword "include_once"         = KeywordIncludeOnce
          keyword "instanceof"         = KeywordInstanceOf
          keyword "isset"         = KeywordIsset
          keyword "list"                 = KeywordList
          keyword "new"                 = KeywordNew
          keyword "print"         = KeywordPrint
          keyword "require"         = KeywordRequire
          keyword "require_once"         = KeywordRequireOnce
          keyword "return"         = KeywordReturn
          keyword "static"         = KeywordStatic
          keyword "switch"         = KeywordSwitch
          keyword "unset"         = KeywordUnset
          keyword "use"                 = KeywordUse
          keyword "var"                 = KeywordVar
          keyword "while"         = KeywordWhile
          keyword "__FUNCTION__"         = Keyword__FUNCTION__
          keyword "__CLASS__"         = Keyword__CLASS__
          keyword "__METHOD__"         = Keyword__METHOD__
          keyword "final"         = KeywordFinal
          keyword "interface"         = KeywordInterface
          keyword "implements"         = KeywordImplement
          keyword "public"         = KeywordPublic
          keyword "private"         = KeywordPrivate
          keyword "protected"         = KeywordProtected
          keyword "abstract"         = KeywordAbstract
          keyword "clone"         = KeywordClone
          keyword "try"                 = KeywordTry
          keyword "catch"         = KeywordCatch
          keyword "throw"         = KeywordThrow
          keyword "cfunction"         = KeywordCfunction
          keyword "old_function"         = KeywordOldFunction
          keyword "true"                 = KeywordTrue
          keyword "false"         = KeywordFalse
          keyword "null"                 = KeywordNull
          keyword _                 = Ident str 
            
data AlexUserState = AlexUserState { pushBack :: String, stack :: [Int], heredocId :: String }

alexInitUserState = AlexUserState { pushBack = "", stack = [], heredocId = "" }

getPushBack :: Alex String
getPushBack = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, reverse (pushBack ust))

setPushBack :: String -> Alex ()
setPushBack ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){pushBack=ss}}, ())
                                               
clearPushBack :: Alex ()
clearPushBack = Alex $ \s -> Right (s{alex_ust=(alex_ust s){pushBack=""}}, ())

addToPushBack :: Char -> Alex ()
addToPushBack c = Alex $ \s -> Right (s{alex_ust=(alex_ust s){pushBack=c:pushBack (alex_ust s)}}, ())

pushState :: Int -> Alex ()
pushState sc = Alex $ \s -> Right (s{alex_scd=sc, alex_ust=(alex_ust s){stack=(alex_scd s):stack (alex_ust s)}}, ())

popState :: Alex()
popState = Alex $ \s -> Right (s{alex_scd=(head (stack (alex_ust s))), alex_ust=(alex_ust s){stack=tail (stack (alex_ust s))}}, ())

getHeredocId :: Alex String
getHeredocId = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, (heredocId ust))

setHeredocId :: String -> Alex ()
setHeredocId ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){heredocId=ss}}, ())

initState :: String -> AlexState 
initState input = AlexState {alex_pos = alexStartPos,
                             alex_inp = input,       
                             alex_chr = '\n',
                             alex_bytes = [],
                             alex_scd = 0,
                             alex_ust = alexInitUserState
                             }

fromHex ('x':s) = fromHex s
fromHex s = fx (reverse s)
  where fx [] = 0
        fx (c:t) = (hd c) + 16 * fx t
        hd c    | c `elem` ['0'..'9'] = (ord c) - (ord '0')
                | c `elem` ['a'..'f'] = (ord c) - (ord 'a') + 10 
                | c `elem` ['A'..'F'] = (ord c) - (ord 'A') + 10
                | otherwise = 0

fromOctal s = fo (reverse s)
  where fo [] = 0
        fo (c:t) = (od c) + 8 * fo t
        od c    | c `elem` ['0'..'7'] = (ord c) - (ord '0')
                | otherwise = 0
                         
-- runAlex' :: String -> Alex a -> Either (String a
lexer input  
   = run (initState input) 
  where 
    Alex f = alexMonadScan
    run st = case f st of Left msg         -> [ERROR]
                          Right (_, [EOF]) -> [EOF]
                          Right (st', t)   -> t ++ (run st')

quotedArrayIntIdx (_,_,_,inp) len = do str <- getPushBack; clearPushBack; return [PHPString str, Op ".", Variable ary, Op "[", PHPInteger idx, Op "]", Op "."]
		  		    where (_:m1)       = take len inp
		        		  (ary,(_:m2)) = break (== '[') m1
		        		  (idx,_)      = break (== ']') m2

quotedArrayStrIdx (_,_,_,inp) len = do str <- getPushBack; clearPushBack; return [PHPString str, Op ".", Variable ary, Op "[", PHPString idx, Op "]", Op "."]
                                    where (_:m1)       = take len inp
                                          (ary,(_:m2)) = break (== '[') m1
                                          (idx,_)      = break (== ']') m2  

quotedArrayVarIdx (_,_,_,inp) len = do str <- getPushBack; clearPushBack; return [PHPString str, Op ".", Variable ary, Op "[", Variable idx, Op "]", Op "."]
                                    where (_:m1)         = take len inp
                                          (ary,(_:_:m2)) = break (== '[') m1
                                          (idx,_)        = break (== ']') m2

quotedMethodCall (_,_,_,inp) len = do str <- getPushBack; clearPushBack; return [PHPString str, Op ".", Variable obj, OpSingleArrow, Ident mth, Op "."] 
                                   where (_:m1)          = take len inp
                                         (obj,(_:_:mth)) = break (== '-') m1
                                         
hereDocAny (_,_,_,inp) len = do hd <- getHeredocId
    				addToPushBack (head inp)                                         
				if (isPrefixOf hd inpTail) 
				  then do str <- getPushBack; clearPushBack; alexSetStartCode php; return [PHPString str]
				  else alexMonadScan  
			     where (ch:inpTail) = inp
}

