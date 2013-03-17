{
module PHPLex (Token(..), Token'(..), lexer, AlexState, mLexer, initState, P, runP, parse, lexError) where

import Data.Char (toLower, chr)
import Data.List (isPrefixOf, splitAt)
import Data.Maybe
import Control.Applicative 

}

%wrapper "monadUserState"

@NL            = \r?\n?
@WS            = [\ \t\n\r]
@ANY           = [\x00-\x10ffff]

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

<0> @START_ECHO { \(_,_,_,inp) len -> do alexSetStartCode php; emit KeywordEcho; next }
<0> @START      { \(_,_,_,inp) len -> do emitAccum; alexSetStartCode php; next }
<0> @ANY        { \(_,_,_,inp) len -> do addToAccum (head inp); next }

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

<php> "("          { go LParen }
<php> ")"          { go RParen }
<php> "{"          { \input len -> do pushState php; emit LBrace; next }
<php> "}"          { \input len -> do popState; emit RBrace; next }
<php> "["          { go LBracket }
<php> "]"          { go RBracket }
<php> "+"          { go OpPlus }
<php> "-"          { go OpMinus }
<php> "/"          { go OpSlash }
<php> "*"          { go OpStar }
<php> "%"          { go OpPercent }
<php> "^"          { go OpCaret }
<php> "&"          { go OpAmpersand }
<php> "|"          { go OpPipe }
<php> "~"          { go OpTilde }
<php> "="          { go OpEq }
<php> "<"          { go OpLt }
<php> ">"          { go OpGt }
<php> "."          { go OpDot }
<php> "!"          { go OpBang }
<php> ","          { go OpComma }
<php> "?"          { go OpQuestion }
<php> ":"          { go OpColon }
<php> "@"          { go OpAtSign }
<php> "$"          { go OpDollars }
<php> ";"          { go Semicolon } 
<php> \\           { go Backslash }

-- tokens --

<php> "$" @IDENT   { variable }
<php> @IDENT       { keywordOrIdent }
<php> @INT         { goStr IntegerToken }
<php> @REAL        { goStr RealToken }
<php> @STOP        { \input len -> do emit Semicolon; startAccum; alexSetStartCode 0; next }

-- strings --
<php> \'           { \input len -> do startAccum; alexSetStartCode sqStr; next }
<php> \`           { \input len -> do emit Backquote; startAccum; alexSetStartCode btStr; next }
<php> \"           { \input len -> do emit DoubleQuote; startAccum; alexSetStartCode dqStr; next }
<php> b? "<<<" @TABS_AND_SPACES ( @IDENT | ( \' @IDENT \' )| ( \" @IDENT \" ) ) @NL { startHereDoc }   

-- comments --
<php> ^[\ \t]*\n   ;
<php> "/*"         { \_ _ -> do startAccum; alexSetStartCode mlComm; next }
<php> "#" | "//"   { \_ _ -> do startAccum; alexSetStartCode slComm; next }

<mlComm> "*/"      { \_ _ -> do emitAccum; alexSetStartCode php; next }
<mlComm> @ANY      { \(_,_,_,inp) len -> do addToAccum (head inp); next }

<slComm> @NL       { \_ _ -> do emitAccum; alexSetStartCode php; next }
<slComm> @PHP_STOP { \_ _ -> do emitAccum; alexSetStartCode 0; startAccum; next }
<slComm> .         { \(_,_,_,inp) len -> do addToAccum (head inp); next }

-- any other character --
<php> @WS          ;
<php> @ANY         { goStr Invalid }

-- singly-quoted strings --
<sqStr> \'         { \_ _ -> do emitAccum; alexSetStartCode php; next }
<sqStr> \\         { \_ _ -> do alexSetStartCode sqEsc; next}
<sqStr> @ANY       { \(_,_,_,inp) len -> do addToAccum (head inp); next }

<sqEsc> \'         { \(_,_,_,inp) len -> do addToAccum (head inp); alexSetStartCode sqStr; next }
<sqEsc> \\         { \(_,_,_,inp) len -> do addToAccum (head inp); alexSetStartCode sqStr; next }
<sqEsc> @ANY       { \(_,_,_,inp) len -> do addToAccum '\\'; addToAccum (head inp); alexSetStartCode sqStr; next }

-- backticked string --
<btStr> \`         { \input len -> do emitAccum; alexSetStartCode php; emit Backquote; next }
<btStr> \\\`       { \input len -> do addToAccum '`'; next}
<btStr> \\         { \input len -> do pushState escape; next }
<btStr> @ANY       { \(_,_,_,inp) len -> do addToAccum (head inp); next }

-- in-string syntax --
<dqStr,hereDoc,btStr> "$" @IDENT                 { quotedVariable }
<dqStr,hereDoc,btStr> "${"                       { quotedExpression }
<dqStr,hereDoc,btStr> "{" / "$"                  { quotedInterpolated } 
<dqStr,hereDoc,btStr> "$" @IDENT "[" @INT "]"    { quotedArrayIntIdx }
<dqStr,hereDoc,btStr> "$" @IDENT "[" @IDENT "]"  { quotedArrayStrIdx } 
<dqStr,hereDoc,btStr> "$" @IDENT "[$" @IDENT "]" { quotedArrayVarIdx } 
<dqStr,hereDoc,btStr> "$" @IDENT "->" @IDENT     { quotedMethodCall }

<escape> n         { \ inp len -> do addToAccum '\n'; popState; next }
<escape> t         { \ inp len -> do addToAccum '\t'; popState; next }
<escape> r         { \ inp len -> do addToAccum '\r'; popState; next }                    
<escape> \\        { \ inp len -> do addToAccum '\\'; popState; next }
<escape> \$        { \ inp len -> do addToAccum '$'; popState; next }
<escape> x[0-9A-Fa-f]{1,2}
                   { \(_,_,_,inp) len -> do addToAccum (chr (fromHex (take len inp))); popState; next }
<escape> [0-7]{1,3}
                   { \(_,_,_,inp) len -> do addToAccum (chr (fromOctal (take len inp))); popState; next  }
<escape> @ANY      { \(_,_,_,inp) len -> do addToAccum '\\'; addToAccum (head inp); popState; next }

-- double-quoted strings --

<dqStr> \"         { \input len -> do emitAccum; alexSetStartCode php; emit DoubleQuote; next }
<dqStr> \\\"       { \input len -> do addToAccum '"'; next }
<dqStr> \\         { \input len -> do pushState escape; next }
<dqStr> @ANY       { \(_,_,_,inp) len -> do addToAccum (head inp); next }

-- heredoc syntax --

<nowDoc,hereDoc> @ANY { hereDocAny }

<endHereDoc> @IDENT { \input len -> do emitAccum; alexSetStartCode php; emit EndHeredoc; next }   

{

data Wrapped t = Wrapped t AlexPosn [String]
  deriving (Show, Eq)
  
type Token = Wrapped Token'

data Token' = 
        CastInt | CastReal | CastString | CastArray | CastObject | CastBool | CastUnset | 
        OpEqEq | OpEqEqEq | OpNotEq | OpNotEqEq | OpLE | OpGE | OpInc | OpDec | 
        OpDoubleArrow | OpSingleArrow | OpSL | OpSR | OpPlusEq | OpMinusEq | OpMultEq | 
        OpDivEq | OpConcatEq | OpModEq | OpAndEq | OpOrEq | OpXorEq | OpSLEq | OpSREq | 
        OpColonColon | OpLogicAnd | OpLogicOr | OpPlus | OpMinus | OpSlash |
        OpStar | OpPercent | OpCaret | OpAmpersand | OpPipe | OpTilde | OpEq | OpLt |
        OpGt | OpDot | OpBang | OpComma | OpQuestion | OpColon | OpAtSign | OpDollars |
        Semicolon | LParen | RParen | LBrace | RBrace | LBracket | RBracket | Backslash |
        Backquote | DoubleQuote |
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
        StartHeredoc | EndHeredoc |
        ERROR | EOF |
        InlineHTML String | VariableToken String | IdentToken String |
        IntegerToken String | RealToken String | StringToken String |
        VariableTokenInStr String | Invalid String
        deriving (Eq,Show)

data AlexUserState = AlexUserState { 
                uAccum :: String, 
                uAccumPos :: AlexPosn,
                uStack :: [Int], 
                uHeredocId :: String, 
                uTokens :: [Token],
                uComments :: [String]
                }
                
alexInitUserState = AlexUserState { uAccum = "", uAccumPos = alexStartPos, uStack = [], uHeredocId = "", uTokens = [], uComments = [] }

startAccum :: Alex ()
startAccum = do 
  emitAccum
  Alex $ \s@AlexState{alex_ust=ust} -> Right (s{alex_ust=ust{uAccum="", uAccumPos = alex_pos s}}, ()) 

addToAccum :: Char -> Alex ()
addToAccum c = Alex $ \s@AlexState{alex_ust=ust} -> Right (s{alex_ust=ust{uAccum=c:uAccum ust}}, ())

getTokens :: Alex [Token]
getTokens = Alex $ \s@AlexState{alex_ust=ust} -> Right (s{alex_ust=ust{uTokens=[]}}, reverse (uTokens ust))

pushState :: Int -> Alex ()
pushState sc = Alex $ \s@AlexState{alex_ust=ust} -> Right (s{alex_scd=sc, alex_ust=ust{uStack=(alex_scd s):uStack ust}}, ())

addComment :: String -> Alex ()
addComment cmt = Alex $ \s@AlexState{alex_ust=ust} -> Right (s{alex_ust=ust{uComments=cmt:uComments ust}}, ())

popState :: Alex()
popState = Alex $ \s@AlexState{alex_ust=ust} -> Right (s{alex_scd=(head (uStack ust)), alex_ust=ust{uStack=tail (uStack ust)}}, ())

getHeredocId :: Alex String
getHeredocId = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, (uHeredocId ust))

setHeredocId :: String -> Alex ()
setHeredocId ss = Alex $ \s@AlexState{alex_ust=ust} -> Right (s{alex_ust=ust{uHeredocId=ss}}, ())

nopAccum :: String -> Alex ()
nopAccum _ = nop

next :: Alex [Token]
next = do tokens <- getTokens
          case tokens of [] -> alexMonadScan
                         _  -> return tokens

emit :: Token' -> Alex ()
emit t = do
   emitAccum
   emit' t

emit' :: Token' -> Alex ()
emit' t = do
   comments <- Alex $ \s@AlexState{alex_ust=ust} -> Right (s{alex_ust=ust{uComments = []}}, reverse (uComments ust))
   Alex $ \s@AlexState{alex_ust=ust} -> Right (s{alex_ust=ust{uTokens = (Wrapped t (alex_pos s) comments):uTokens ust}}, ())

accumFn :: Int -> (String -> Alex ())
accumFn i = case i of 
                0      -> accumEmit InlineHTML
                sqStr  -> accumEmit' StringToken
                dqStr  -> accumEmit StringToken
                btStr  -> accumEmit StringToken
                mlComm -> addComment
                dlComm -> addComment
                _      -> \_ -> nop

emitAccum :: Alex ()
emitAccum = do
   (fn, str) <- Alex $ \s@AlexState{alex_ust=ust} -> Right (s{alex_ust=ust{uAccum = ""}}, (accumFn (alex_scd s), reverse (uAccum ust)))
   fn str
   Alex $ \s@AlexState{alex_ust=ust} -> Right (s, ())

nop :: Alex ()
nop = Alex $ \s -> Right (s, ())   

accumEmit :: (String -> Token') -> String -> Alex ()
accumEmit t str = do if str /= "" then emit' (t str) else nop

accumEmit' :: (String -> Token') -> String -> Alex ()
accumEmit' t str = do emit' (t str)

initState :: String -> AlexState 
initState input = AlexState {alex_pos = alexStartPos,
                             alex_inp = input,       
                             alex_chr = '\n',
                             alex_bytes = [],
                             alex_scd = 0,
                             alex_ust = alexInitUserState
                             }

fromHex :: String -> Int
fromHex ('x':s) = fromHex s
fromHex s = fx (reverse s)
  where fx [] = 0
        fx (c:t) = (hd c) + 16 * fx t
        hd c    | c `elem` ['0'..'9'] = (ord c) - (ord '0')
                | c `elem` ['a'..'f'] = (ord c) - (ord 'a') + 10 
                | c `elem` ['A'..'F'] = (ord c) - (ord 'A') + 10
                | otherwise = 0

fromOctal :: String -> Int
fromOctal s = fo (reverse s)
  where fo [] = 0
        fo (c:t) = (od c) + 8 * fo t
        od c    | c `elem` ['0'..'7'] = (ord c) - (ord '0')
                | otherwise = 0

go :: Token' -> AlexInput -> Int -> Alex [Token]
go cstr = \ _ _ -> do emit cstr; next

goStr :: (String -> Token') -> AlexInput -> Int -> Alex [Token]
goStr cstr = \ (_,_,_,inp) len -> do emit (cstr (take len inp)); next

variable :: AlexInput -> Int -> Alex [Token]
variable (_,_,_,inp) len = do emit (VariableToken (tail (take len inp))); next

keywordOrIdent :: AlexInput -> Int -> Alex [Token]
keywordOrIdent (posn,_,_,inp) len =
  do emit (keyword (toLowerStr str)); next
    where str = (take len inp)
          toLowerStr s = map toLower s
          keyword "and"           = KeywordAnd  
          keyword "or"            = KeywordOr
          keyword "xor"           = KeywordXor
          keyword "__FILE__"      = Keyword__FILE__
          keyword "__LINE__"      = Keyword__LINE__
          keyword "__DIR__"       = Keyword__DIR__
          keyword "array"         = KeywordArray
          keyword "as"            = KeywordAs
          keyword "break"         = KeywordBreak
          keyword "case"          = KeywordCase
          keyword "class"         = KeywordClass
          keyword "const"         = KeywordConst
          keyword "continue"      = KeywordContinue
          keyword "declare"       = KeywordDeclare
          keyword "default"       = KeywordDefault
          keyword "do"            = KeywordDo
          keyword "echo"          = KeywordEcho
          keyword "else"          = KeywordElse
          keyword "elseif"        = KeywordElseif
          keyword "empty"         = KeywordEmpty
          keyword "enddeclare"    = KeywordEnddeclare
          keyword "endfor"        = KeywordEndfor
          keyword "endforeach"    = KeywordEndforeach
          keyword "endif"         = KeywordEndif
          keyword "endswitch"     = KeywordEndswitch
          keyword "endwhile"      = KeywordEndwhile
          keyword "eval"          = KeywordEval
          keyword "exit"          = KeywordExit
          keyword "die"           = KeywordDie
          keyword "extends"       = KeywordExtends
          keyword "for"           = KeywordFor
          keyword "foreach"       = KeywordForeach
          keyword "function"      = KeywordFunction
          keyword "global"        = KeywordGlobal
          keyword "if"            = KeywordIf
          keyword "include"       = KeywordInclude
          keyword "include_once"  = KeywordIncludeOnce
          keyword "instanceof"    = KeywordInstanceOf
          keyword "isset"         = KeywordIsset
          keyword "list"          = KeywordList
          keyword "new"           = KeywordNew
          keyword "print"         = KeywordPrint
          keyword "require"       = KeywordRequire
          keyword "require_once"  = KeywordRequireOnce
          keyword "return"        = KeywordReturn
          keyword "static"        = KeywordStatic
          keyword "switch"        = KeywordSwitch
          keyword "unset"         = KeywordUnset
          keyword "use"           = KeywordUse
          keyword "var"           = KeywordVar
          keyword "while"         = KeywordWhile
          keyword "__FUNCTION__"  = Keyword__FUNCTION__
          keyword "__CLASS__"     = Keyword__CLASS__
          keyword "__METHOD__"    = Keyword__METHOD__
          keyword "final"         = KeywordFinal
          keyword "interface"     = KeywordInterface
          keyword "implements"    = KeywordImplements
          keyword "public"        = KeywordPublic
          keyword "private"       = KeywordPrivate
          keyword "protected"     = KeywordProtected
          keyword "abstract"      = KeywordAbstract
          keyword "clone"         = KeywordClone
          keyword "try"           = KeywordTry
          keyword "catch"         = KeywordCatch
          keyword "throw"         = KeywordThrow
          keyword "namespace"     = KeywordNamespace
          keyword "goto"          = KeywordGoto
          keyword "finally"       = KeywordFinally
          keyword "trait"         = KeywordTrait
          keyword "callable"      = KeywordCallable
          keyword "insteadof"     = KeywordInsteadof
          keyword "yield"         = KeywordYield
          keyword "__TRAIT__"     = Keyword__TRAIT__
          keyword "__NAMESPACE__" = Keyword__NAMESPACE__
          keyword _               = IdentToken str 

quotedVariable :: AlexInput -> Int -> Alex [Token]                
quotedVariable (_,_,_,inp) len = do emit (VariableToken v); next               
                                      where (_:v) = take len inp

quotedExpression :: AlexInput -> Int -> Alex [Token]                         
quotedExpression _ _ = do emit DollarOpenCurlyBrace; next

quotedInterpolated :: AlexInput -> Int -> Alex [Token]
quotedInterpolated _ _  = do emit LBrace; next                              

quotedArrayIntIdx :: AlexInput -> Int -> Alex [Token]
quotedArrayIntIdx (_,_,_,inp) len = do emit (VariableToken ary); emit LBracket; emit (IntegerToken idx); emit RBracket; next
                                      where (_:m1)       = take len inp
                                            (ary,(_:m2)) = break (== '[') m1
                                            (idx,_)      = break (== ']') m2

quotedArrayStrIdx :: AlexInput -> Int -> Alex [Token]
quotedArrayStrIdx (_,_,_,inp) len = do emit (VariableToken ary); emit LBracket; emit (IdentToken idx); emit RBracket; next
                                    where (_:m1)       = take len inp
                                          (ary,(_:m2)) = break (== '[') m1
                                          (idx,_)      = break (== ']') m2  

quotedArrayVarIdx :: AlexInput -> Int -> Alex [Token]
quotedArrayVarIdx (_,_,_,inp) len = do emit (VariableToken ary); emit LBracket; emit (VariableToken idx); emit RBracket; next
                                    where (_:m1)         = take len inp
                                          (ary,(_:_:m2)) = break (== '[') m1
                                          (idx,_)        = break (== ']') m2

quotedMethodCall :: AlexInput -> Int -> Alex [Token]
quotedMethodCall (_,_,_,inp) len = do emit (VariableToken obj); emit OpSingleArrow; emit (IdentToken mth); next 
                                   where (_:m1)          = take len inp
                                         (obj,(_:_:mth)) = break (== '-') m1

hereDocAny :: AlexInput -> Int -> Alex [Token]                                         
hereDocAny i@(_,_,_,inp) len = do hd <- getHeredocId
                                  addToAccum ch
                                  let tailLen = atEnd hd inpTail
                                    in if (isJust tailLen)  
                                         then do str <- emitAccum; alexSetInput (skipChars (fromJust tailLen) i); alexSetStartCode php; next
                                         else next
                                 where (ch:inpTail) = inp
                                       atEnd hd tail = length <$> matchedTail
                                         where test str aff str' = if isPrefixOf s str' then Just s else Nothing
                                                                       where s = str ++ aff
                                               tailtest str str' = foldl (<|>) Nothing (map (\aff -> (test str aff str')) [";\r", ";\n", "\r", "\n"])
                                               matchedTail = tailtest hd tail

startHereDoc :: AlexInput -> Int -> Alex [Token]
startHereDoc (_,_,_,inp) len = 
  do alexSetStartCode mode; setHeredocId docId; emit StartHeredoc; next
  where (str0,tail) = splitAt len inp 
        str = takeWhile (not . (flip elem "\r\n")) str0 
        (bprefix,str') = case str of 'b':rest -> (True,rest)
                                     rest     -> (False,rest)
        docId' = dropWhile (flip elem "< \t" ) str'
        (mode',docId) = case docId' of '\'':rest -> (nowDoc, rest)
                                       '"':rest  -> (hereDoc, rest)
                                       rest      -> (hereDoc, rest)
        isEmpty = (isPrefixOf docId tail) && tailtest ttail 
        tailtest str = any (flip isPrefixOf str) [";\r", ";\n", "\r", "\n"]  
        ttail = drop (length docId) tail
        mode = if isEmpty then endHereDoc else mode'

skipChar :: AlexInput -> AlexInput                                                         
skipChar inp@(_,_,_,[]) = inp
skipChar (p,_,ps,c:s) = ((alexMove p c), c, [], s)

skipChars :: Int -> AlexInput -> AlexInput
skipChars 0 inp = inp
skipChars 1 inp = skipChar inp
skipChars n inp = skipChars (n-1) (skipChar inp)

alexEOF :: Alex [Token]
alexEOF = do emit EOF; next

lexer :: String -> [Token]
lexer input = run (initState input) 
  where 
    Alex f = alexMonadScan
    run st = case f st of Left msg                       -> [Wrapped ERROR (alex_pos st) []]
                          Right (_, t@[Wrapped EOF _ _]) -> t
                          Right (st', t)                 -> t ++ (run st')

mLexer :: (Token -> P a) -> P a
mLexer cont = P lexer'
  where lexer' (x:xs) = returnToken cont x xs  
        lexer' []     = run 
        Alex f = alexMonadScan
        run st = case f st of Left msg ->          returnToken cont (Wrapped ERROR (alex_pos st) []) [] st
                              Right (st', t:tx) -> returnToken cont t tx st'
          
returnToken :: (t -> P a) -> t -> [Token] -> AlexState -> ParseResult a          
returnToken cont tok = runP (cont tok )

data ParseResult a = Ok a | Fail String deriving (Show, Eq)
newtype P a = P ([Token] -> AlexState -> ParseResult a)

runP :: P a -> [Token] -> AlexState -> ParseResult a
runP (P f) = f

parse :: P a -> String -> ParseResult a
parse parser string = runP parser [] (initState string) 

instance Monad P where
  return m = P $ \ _ _ -> Ok m
  m >>= k =  P $ \s st -> case runP m s st of Ok a -> runP (k a) s st
                                              Fail err -> Fail err
  fail s = P $ \ _ _ -> Fail s

lexError :: P a
lexError = P $ \ _ AlexState {alex_inp=inp} -> Fail ("Parse error, remaining input: " ++ (take 20 inp))
                               
}

