module Tokenizer
where

import Text.Parsec 
import qualified Text.Parsec.Char as PC

import Data.Char (toLower, toUpper, chr)
import Control.Monad.Identity (Identity)
import Control.Monad (liftM2)

data Token = 
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
    
data TReturn = TReturn { 
    getToken:: Token, nextToken:: Tokenizer, remainingInput :: String 
    }
type Parser a = Parsec String () a
type Tokenizer = Parser TReturn

twaddle1 :: Char -> Parser Char
twaddle1 c = PC.oneOf [toLower c, toUpper c] 
twaddle :: String -> Parser String
twaddle  s = foldr (liftM2 (:)) (return "") (map twaddle1 s)

nl = many (PC.char '\r') >> many (PC.char '\n') 
ws = PC.oneOf [' ', '\t', '\n', '\r' ] 

tabsAndSpaces :: Parser ()
tabsAndSpaces = (char '\t' <|> char ' ') >> return () 

php = twaddle "php"
        
phpStart = char '<' >> PC.char '?' >> optional php

scriptStart = PC.string "<script" >> many1 ws >>
                PC.string "language" >> many ws >>
                PC.string "=" >> many ws >>
                PC.oneOf ['\'', '\"'] >> many ws >> 
                php >> many ws >>
                PC.oneOf ['\'', '\"'] >> many ws >>
                PC.string ">" >> return ()
                
start = phpStart <|> scriptStart
startEcho = PC.string "<?="
phpStop = PC.string "?>"
scriptStop = PC.string "</script" >> many ws >> PC.string ">"
stop = (phpStop <|> scriptStop) >> optional nl

phpIsAlpha c = (c >= 'a' && c <= 'z') ||
               (c >= 'A' && c <= 'Z') ||
               (c == '_') ||
               (c >= chr 127 && c <= chr 255)
phpIsAlphaNum c = phpIsAlpha c || (c >= '0' && c <= '9')

ident :: Parser String
ident = PC.satisfy phpIsAlpha >> many (PC.satisfy phpIsAlphaNum)

int :: Parser String       
int = dec <|> hex <|> oct

dec = PC.string "0" <|>
        (PC.oneOf ['1'..'9'] >> many (PC.oneOf ['0'..'9']))
hex = PC.char '0' >> PC.oneOf ['x','X'] >> 
        many1 (PC.oneOf $ ['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F'])
oct = PC.char '0' >> many (PC.oneOf ['0'..'7'])

real :: Parser String
lnum = many1 (PC.oneOf ['0'..'9'])
dnum = (many (PC.oneOf ['0'..'9']) >> PC.char '.' >> lnum) <|>
       (lnum >> PC.char '.' >> many (PC.oneOf ['0'..'9']))
exponentDnum = (lnum <|> dnum) >> PC.oneOf ['e','E'] >>
                optional (PC.oneOf ['+','-']) >> lnum
real = dnum <|> exponentDnum

castWs = PC.oneOf ['\t', ' ']
cs = PC.char '(' >> many castWs 
ce :: Parser ()
ce = many castWs >> PC.char ')' >> return ()

cInteger = twaddle "integer"
cInt = twaddle "int"
intCast = cs >> (cInteger <|> cInt) >> ce
cFloat = twaddle "float"
cReal = twaddle "real"
cDouble = twaddle "double"
realCast = cs >> (cFloat <|> cReal <|> cDouble) >> ce

cString = twaddle "string"
stringCast = cs >> cString >> ce

cArray = twaddle "array"
arrayCast = cs >> cArray >> ce

cObject = twaddle "object"
objectCast = cs >> cObject >> ce

cBool = twaddle "bool"
cBoolean = twaddle "boolean"
boolCast = cs >> (cBool <|> cBoolean) >> ce

cUnset = twaddle "unset" 
unsetCast = cs >> cUnset >> ce 

go nxt t = do 
             i <- getInput
             return $ TReturn t nxt i

token0 :: Tokenizer 
start' = (try startEcho >> go tokenPhp KeywordEcho) <|>
         (try start >> tokenPhp) <|>
         (eof >> go token0 EOF)
token0 = start' <|>
         do
            html <- manyTill PC.anyChar (lookAhead start')
            go token0 $ InlineHTML html 

tokenPhp :: Tokenizer
tokenPhp = let go' = go tokenPhp  in
    (eof         >> go' EOF) <|>
    try (intCast     >> go' CastInt) <|>
    try (realCast    >> go' CastReal) <|>
    try (stringCast  >> go' CastString) <|>
    try (arrayCast   >> go' CastArray) <|>
    try (objectCast  >> go' CastObject) <|>
    try (boolCast    >> go' CastBool) <|>
    try (unsetCast   >> go' CastUnset) <|>
    try ((PC.string "==")   >> go' OpEqEq) <|>
    try ((PC.string "===")  >> go' OpEqEqEq) <|>
    try ((PC.string "!=")   >> go' OpNotEq) <|>
    try ((PC.string "<>")   >> go' OpNotEq) <|>
    try ((PC.string "!==")  >> go' OpNotEqEq) <|>
    try ((PC.string "<=")   >> go' OpLE) <|>
    try ((PC.string ">=")   >> go' OpGE) <|>
    try ((PC.string "++")   >> go' OpInc) <|>
    try ((PC.string "--")   >> go' OpDec) <|>
    try ((PC.string "=>")   >> go' OpDoubleArrow) <|>
    try ((PC.string "->")   >> go' OpSingleArrow) <|>
    try ((PC.string "<<")   >> go' OpSL) <|>
    try ((PC.string ">>")   >> go' OpSR) <|>
    try ((PC.string "+=")   >> go' OpPlusEq) <|>
    try ((PC.string "-=")   >> go' OpMinusEq) <|>
    try ((PC.string "*=")   >> go' OpMultEq) <|>
    try ((PC.string "/=")   >> go' OpDivEq) <|>
    try ((PC.string ".=")   >> go' OpConcatEq) <|>
    try ((PC.string "%=")   >> go' OpModEq) <|>
    try ((PC.string "&=")   >> go' OpAndEq) <|>
    try ((PC.string "|=")   >> go' OpOrEq) <|>
    try ((PC.string "^=")   >> go' OpXorEq) <|>
    try ((PC.string "<<=")  >> go' OpSLEq) <|>
    try ((PC.string ">>=")  >> go' OpSREq) <|>
    ((PC.string "::")   >> go' OpColonColon) <|>
    ((PC.string "&&")   >> go' OpLogicAnd) <|>
    try ((PC.string "||")   >> go' OpLogicOr) <|>
    ((PC.string "(")    >> go' LParen) <|>
    ((PC.string ")")    >> go' RParen) <|>
    ((PC.string "{")    >> go' LBrace) <|>
    ((PC.string "}")    >> go' RBrace) <|>
    ((PC.string "[")    >> go' LBracket) <|>
    ((PC.string "]")    >> go' RBracket) <|>
    ((PC.string "+")    >> go' OpPlus) <|>
    ((PC.string "-")    >> go' OpMinus) <|>
    ((PC.string "/")    >> go' OpSlash) <|>
    ((PC.string "*")    >> go' OpStar) <|>
    ((PC.string "%")    >> go' OpPercent) <|>
    ((PC.string "^")    >> go' OpCaret) <|>
    ((PC.string "&")    >> go' OpAmpersand) <|>
    ((PC.string "|")    >> go' OpPipe) <|>
    ((PC.string "~")    >> go' OpTilde) <|>
    ((PC.string "=")    >> go' OpEq) <|>
    ((PC.string "<")    >> go' OpLt) <|>
    ((PC.string ">")    >> go' OpGt) <|>
    ((PC.string ".")    >> go' OpDot) <|>
    ((PC.string "!")    >> go' OpBang) <|>
    ((PC.string ",")    >> go' OpComma) <|>
    ((PC.string "?")    >> go' OpQuestion) <|>
    ((PC.string ":")    >> go' OpColon) <|>
    ((PC.string "@")    >> go' OpAtSign) <|>
    ((PC.string "$")    >> go' OpDollars) <|>
    ((PC.string ";")    >> go' Semicolon) <|> 
    ((PC.string "\\")   >> go' Backslash) <|>
    try ((PC.string "$") >> ident >>= variable) <|>
    try (ident  >>= keywordOrIdent) <|>
    try (int    >>= (goStr IntegerToken)) <|>
    try (real   >>= (goStr RealToken)) <|>
    try (stop   >> go token0 Semicolon) <|>
    try ((PC.char '\'') >> tokenSqStr) <|>
    try ((PC.char '`') >> go tokenBtStr Backquote) <|>
    try ((PC.char '"') >> go tokenDqStr DoubleQuote) <|>
    try (optional (PC.char 'b') >> (PC.string "???") >> tabsAndSpaces >> 
        (ident 
            <|> (between (PC.char '\'') (PC.char '\'') ident)
            <|> (between (PC.char '"') (PC.char '"') ident)) >>
            nl >> startHereDoc) <|>
    try ((PC.string "/*") >> tokenMlComm) <|>
    try (((PC.string "#") <|> (PC.string "//")) >> tokenSlComm)


variable :: String -> Tokenizer    
variable v = go tokenPhp $ VariableToken v

goStr :: (String -> Token) -> String -> Tokenizer
goStr t str = go tokenPhp (t str)

tokenSqStr = unexpected "NYI"
tokenDqStr = unexpected "NYI"
tokenBtStr = unexpected "NYI"
startHereDoc = unexpected "NYI"
tokenMlComm = unexpected "NYI"
tokenSlComm = unexpected "NYI"

keywordOrIdent :: String -> Tokenizer
keywordOrIdent _ = unexpected "NYI"

