{
module PHPLex (Token(..), lexer, AlexState, mLexer, P) where

import Data.Char (toLower, chr)
import Data.List (isPrefixOf, splitAt) 

}

%wrapper "monadUserState"

@NL            = \r?\n?
@WS            = [\ \t\n\r]
@ANY           = [\x00-\xff]

@TABS_AND_SPACES = [\t\ ]     

@PHP           = [pP][hH][pP]
@PHP_START     = "<?" @PHP?
@SCRIPT_START  = "<script" @WS+ "language" @WS* "=" @WS* ['\"] @WS* @PHP @WS* ['\"] @WS* ">"
@START         = @PHP_START | @SCRIPT_START
@START_ECHO    = "<?="
@PHP_STOP      = "?>"
@SCRIPT_STOP   = "</script" @WS* ">"
@STOP          = (@PHP_STOP | @SCRIPT_STOP) @NL?

@IDENT         = [a-zA-Z_\x7F-\xFF][a-zA-Z0-9_\x7F-\xFF]*

@DEC           = ([1-9][0-9]*)|0
@HEX           = 0[xX][0-9a-fA-F]+
@OCT           = 0[0-7]+
@INT           = (@DEC|@HEX|@OCT)

@LNUM          = [0-9]+
@DNUM          = ([0-9]*[\.]@LNUM)|(@LNUM[\.][0-9]*)
@EXPONENT_DNUM = ((@LNUM|@DNUM)[eE](\+|\-)?@LNUM)
@REAL          = @DNUM|@EXPONENT_DNUM

@CAST_WS       = [\t ]
@CS            = "("@CAST_WS*
@CE            = @CAST_WS*")"

@C_INTEGER     = [iI][nN][tT][eE][gG][eE][rR]
@C_INT         = [iI][nN][tT]
@INT_CAST      = @CS(@C_INTEGER|@C_INT)@CE

@C_FLOAT       = [fF][lL][oO][aA][tT]
@C_REAL        = [rR][eE][aA][lL]
@C_DOUBLE      = [dD][oO][uU][bB][lL][eE]
@REAL_CAST     = @CS(@C_FLOAT|@C_REAL|@C_DOUBLE)@CE

@C_STRING      = [sS][tT][rR][iI][nN][gG]
@STRING_CAST   = @CS@C_STRING@CE 

@C_ARRAY       = [aA][rR][rR][aA][yY]
@ARRAY_CAST    = @CS@C_ARRAY@CE

@C_OBJECT      = [oO][bB][jJ][eE][cC][tT]
@OBJECT_CAST   = @CS@C_OBJECT@CE

@C_BOOL        = [bB][oO][oO][lL]
@C_BOOLEAN     = [bB][oO][oO][lL][eE][aA][nN]
@BOOL_CAST     = @CS(@C_BOOL|@C_BOOLEAN)@CE

@C_UNSET       = [uU][nN][sS][eE][tT]
@UNSET_CAST    = @CS@C_UNSET@CE

tokens :-

<0> @START_ECHO        { \(_,_,_,inp) len -> do ret <- getPushBack;
                                         clearPushBack; 
                                         alexSetStartCode php;
                                         case ret of "" -> return [Op ";", KeywordEcho]
                                                     _  -> return [InlineHTML ret, KeywordEcho]
                }
<0> @START      { \(_,_,_,inp) len -> do ret <- getPushBack;
                                         clearPushBack; 
                                         alexSetStartCode php;
                                         case ret of "" -> alexMonadScan
                                                     _  -> return [InlineHTML ret]
                }                                  
<0>  @ANY       { \(_,_,_,inp) len -> do addToPushBack (head inp); alexMonadScan }

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

<php> "("           { go LParen }
<php> ")"           { go RParen }
<php> "{"           { go LBrace }
<php> "}"           { go RBrace }
<php> "["           { go LBracket }
<php> "]"           { go RBracket }
<php> "+"           { go OpPlus }
<php> "-"           { go OpMinus }
<php> "/"            { go OpSlash }
<php> "*"           { go OpStar }
<php> "%"           { go OpPercent }
<php> "^"           { go OpCaret }
<php> "&"           { go OpAmpersand }
<php> "|"           { go OpPipe }
<php> "~"           { go OpTilde }
<php> "="           { go OpEq }
<php> "<"           { go OpLt }
<php> ">"           { go OpGt }
<php> "."           { go OpDot }
<php> "!"           { go OpBang }
<php> ","           { go OpComma }
<php> "?"           { go OpQuestion }
<php> ":"            { go OpColon }
<php> "@"           { go OpAtSign }
<php> "$"           { go OpDollars }
<php> ";"          { go Semicolon } 
<php> \\           { go Backslash }

-- tokens --

<php> "$" @IDENT   { variable }
<php> @IDENT       { keywordOrIdent }
<php> @INT         { goStr PHPInteger }
<php> @REAL        { goStr PHPReal }
<php> @STOP           { \input len -> do clearPushBack; alexSetStartCode 0; return [Op ";"] } 

-- strings --
<php> \'           { \input len -> do clearPushBack; alexSetStartCode sqStr; alexMonadScan }
<php> \`           { \input len -> do clearPushBack; alexSetStartCode btStr; return [ Backquote ] }
<php> \"           { \input len -> do clearPushBack; alexSetStartCode dqStr; alexMonadScan }
<php> b? "<<<" @TABS_AND_SPACES ( @IDENT | ( \' @IDENT \' )| ( \" @IDENT \" ) ) @NL { startHereDoc }   

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
<dqStr,hereDoc,btStr> "$" @IDENT         
                   { \(_,_,_,inp) len -> do str <- getPushBack; clearPushBack; return [PHPString str, Op ".", Variable (tail (take len inp)), Op "."] }

<dqStr,hereDoc,btStr> "${" 
                   { \(_,_,_,inp) len -> do pushState looking_for_var_name; return [DollarOpenCurlyBrace]; }          

<dqStr,hereDoc,btStr> "$" @IDENT "[" @INT "]" { quotedArrayIntIdx }
<dqStr,hereDoc,btStr> "$" @IDENT "[" @IDENT "]" { quotedArrayStrIdx } 
<dqStr,hereDoc,btStr> "$" @IDENT "[$" @IDENT "]" { quotedArrayVarIdx } 
<dqStr,hereDoc,btStr> "$" @IDENT "->" @IDENT "]" { quotedMethodCall }

<looking_for_var_name> @IDENT ( "[" | "{" ) { \(_,_,_,inp) len -> return $ [VariableInStr (tail (take (len - 1) inp))] }

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

<nowDoc,hereDoc> @ANY  { hereDocAny }

<endHereDoc> @IDENT { \input len -> do str <- getPushBack; clearPushBack; alexSetStartCode php; return [(HeredocContents str)] }   

{
data Token = 
        InlineHTML String |
        CastInt | CastReal | CastString | CastArray | CastObject | CastBool | CastUnset | 
        OpEqEq | OpEqEqEq | OpNotEq | OpNotEqEq | OpLE | OpGE | OpInc | OpDec | 
        OpDoubleArrow | OpSingleArrow | OpSL | OpSR | OpPlusEq | OpMinusEq | OpMultEq | 
        OpDivEq | OpConcatEq | OpModEq | OpAndEq | OpOrEq | OpXorEq | OpSLEq | OpSREq | 
        OpColonColon | OpLogicAnd | OpLogicOr | Op String | OpPlus | OpMinus | OpSlash |
        OpStar | OpPercent | OpCaret | OpAmpersand | OpPipe | OpTilde | OpEq | OpLt |
        OpGt | OpDot | OpBang | OpComma | OpQuestion | OpColon | OpAtSign | OpDollars |
        Semicolon | LParen | RParen | LBrace | RBrace | LBracket | RBracket | Backslash |
        Backquote | DoubleQuote |
        Variable String | Ident String |
        DollarOpenCurlyBrace |  
        KeywordAnd | KeywordOr | KeywordXor | Keyword__FILE__ | Keyword__LINE__ |
        Keyword__DIR__ | 
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
        KeywordImplements | KeywordPublic | KeywordPrivate | KeywordProtected | 
        KeywordAbstract | KeywordClone | KeywordTry | KeywordCatch | 
        KeywordThrow | KeywordNamespace | KeywordGoto | KeywordFinally |
        KeywordTrait | KeywordCallable | KeywordInsteadof | KeywordYield |
        Keyword__TRAIT__ | Keyword__NAMESPACE__ |
        PHPInteger String | PHPReal String | PHPString String |
        VariableInStr String |
        StartHeredoc | EndHeredoc | HeredocContents String |
        ERROR | Invalid String | EOF
        deriving (Eq,Show)

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
          keyword "__DIR__"          = Keyword__DIR__
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
          keyword "implements"         = KeywordImplements
          keyword "public"         = KeywordPublic
          keyword "private"         = KeywordPrivate
          keyword "protected"         = KeywordProtected
          keyword "abstract"         = KeywordAbstract
          keyword "clone"         = KeywordClone
          keyword "try"                 = KeywordTry
          keyword "catch"         = KeywordCatch
          keyword "throw"         = KeywordThrow
          keyword "namespace"        = KeywordNamespace
          keyword "goto"        = KeywordGoto
          keyword "finally"        = KeywordFinally
          keyword "trait"        = KeywordTrait
          keyword "callable"        = KeywordCallable
          keyword "insteadof"        = KeywordInsteadof
          keyword "yield"        = KeywordYield
          keyword "__TRAIT__"  = Keyword__TRAIT__
          keyword "__NAMESPACE__" = Keyword__NAMESPACE__
          keyword _                 = Ident str 
            
data AlexUserState = AlexUserState { uPushBack :: String, uStack :: [Int], uHeredocId :: String }
alexInitUserState = AlexUserState { uPushBack = "", uStack = [], uHeredocId = "" }

getPushBack :: Alex String
getPushBack = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, reverse (uPushBack ust))

setPushBack :: String -> Alex ()
setPushBack ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){uPushBack=ss}}, ())
                                               
clearPushBack :: Alex ()
clearPushBack = Alex $ \s -> Right (s{alex_ust=(alex_ust s){uPushBack=""}}, ())

addToPushBack :: Char -> Alex ()
addToPushBack c = Alex $ \s -> Right (s{alex_ust=(alex_ust s){uPushBack=c:uPushBack (alex_ust s)}}, ())

pushState :: Int -> Alex ()
pushState sc = Alex $ \s -> Right (s{alex_scd=sc, alex_ust=(alex_ust s){uStack=(alex_scd s):uStack (alex_ust s)}}, ())

popState :: Alex()
popState = Alex $ \s -> Right (s{alex_scd=(head (uStack (alex_ust s))), alex_ust=(alex_ust s){uStack=tail (uStack (alex_ust s))}}, ())

getHeredocId :: Alex String
getHeredocId = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, (uHeredocId ust))

setHeredocId :: String -> Alex ()
setHeredocId ss = Alex $ \s -> Right (s{alex_ust=(alex_ust s){uHeredocId=ss}}, ())

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
                                addToPushBack ch                                         
                                if (isPrefixOf hd inpTail) 
                                  then do str <- getPushBack; clearPushBack; alexSetStartCode php; return [PHPString str]
                                  else alexMonadScan  
                             where (ch:inpTail) = inp
                             
startHereDoc (_,_,_,inp) len = 
  do alexSetStartCode mode; setHeredocId docId; return [StartHeredoc]
  where (str0,tail) = splitAt len inp 
        str = dropWhile (flip elem "\r\n") str0 
        (bprefix,str') = case str of 'b':rest -> (True,rest)
                                     rest     -> (False,rest)
        docId' = dropWhile (flip elem "> \t" ) str'
        (mode',docId) = case docId' of '\'':rest -> (nowDoc, rest)
                                       '"':rest  -> (hereDoc, rest)
                                       rest        -> (hereDoc, rest)
        isEmpty = (isPrefixOf docId tail) && tailtest ttail 
        tailtest str = any (flip isPrefixOf str) [";\r", ";\n", "\r", "\n"]  
        ttail = drop (length docId) tail
        mode = if isEmpty then endHereDoc else mode'
                                                                  
alexEOF = do str <- getPushBack;
             clearPushBack; 
             case str of "" -> return [EOF]
                         _  -> return [InlineHTML str, EOF]

lexer :: String -> [Token]
lexer input  
   = run (initState input) 
  where 
    Alex f = alexMonadScan
    run st = case f st of Left msg         -> [ERROR]
                          Right (_, [EOF]) -> [EOF]
                          Right (st', t)   -> t ++ (run st')

mLexer :: (Token -> P a) -> P a
mLexer cont = P lexer'
  where lexer' (x:xs) = returnToken cont x xs  
        lexer' []     = run 
        Alex f = alexMonadScan
        run st = case f st of Left msg ->          returnToken cont ERROR [] st
                              Right (st', t:tx) -> returnToken cont t tx st'
          
returnToken :: (t -> P a) -> t -> [Token] -> AlexState -> ParseResult a          
returnToken cont tok = runP (cont tok )

data ParseResult a = Ok a | Fail String
newtype P a = P ([Token] -> AlexState -> ParseResult a)

runP :: P a -> [Token] -> AlexState -> ParseResult a
runP (P f) = f

instance Monad P where
  return m = P $ \ _ _ -> Ok m
  m >>= k =  P $ \s st -> case runP m s st of Ok a -> runP (k a) s st
                                              Fail err -> Fail err
  fail s = P $ \ _ _ -> Fail s

                               
}

