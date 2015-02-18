module Tokenizer
where

import Text.Parsec 
import qualified Text.Parsec.Char as PC

import Data.Char (toLower, toUpper, chr, isAsciiLower, isAsciiUpper, isDigit)
import Control.Monad (liftM2)

data Token = CastInt
           | CastReal
           | CastString
           | CastArray
           | CastObject
           | CastBool
           | CastUnset
           | OpEqEq
           | OpEqEqEq
           | OpNotEq
           | OpNotEqEq
           | OpLE
           | OpGE
           | OpInc
           | OpDec
           | OpDoubleArrow
           | OpSingleArrow
           | OpSL
           | OpSR
           | OpPlusEq
           | OpMinusEq
           | OpMultEq
           | OpDivEq
           | OpConcatEq
           | OpModEq
           | OpAndEq
           | OpOrEq
           | OpXorEq
           | OpSLEq
           | OpSREq
           | OpColonColon
           | OpLogicAnd
           | OpLogicOr
           | OpPlus
           | OpMinus
           | OpSlash
           | OpStar
           | OpPercent
           | OpCaret
           | OpAmpersand
           | OpPipe
           | OpTilde
           | OpEq
           | OpLt
           | OpGt
           | OpDot
           | OpBang
           | OpComma
           | OpQuestion
           | OpColon
           | OpAtSign
           | OpDollars
           | Semicolon
           | LParen
           | RParen
           | LBrace
           | RBrace
           | LBracket
           | RBracket
           | Backslash
           | Backquote
           | DoubleQuote
           | DollarOpenCurlyBrace
           | KeywordAnd
           | KeywordOr
           | KeywordXor
           | KeywordFILE_
           | KeywordLINE_
           | KeywordDIR_
           | KeywordArray
           | KeywordAs
           | KeywordBreak
           | KeywordCase
           | KeywordClass
           | KeywordConst
           | KeywordContinue
           | KeywordDeclare
           | KeywordDefault
           | KeywordDo
           | KeywordEcho
           | KeywordElse
           | KeywordElseif
           | KeywordEmpty
           | KeywordEnddeclare
           | KeywordEndfor
           | KeywordEndforeach
           | KeywordEndif
           | KeywordEndswitch
           | KeywordEndwhile
           | KeywordEval
           | KeywordExit
           | KeywordDie
           | KeywordExtends
           | KeywordFor
           | KeywordForeach
           | KeywordFunction
           | KeywordGlobal
           | KeywordIf
           | KeywordInclude
           | KeywordIncludeOnce
           | KeywordInstanceOf
           | KeywordIsset
           | KeywordList
           | KeywordNew
           | KeywordPrint
           | KeywordRequire
           | KeywordRequireOnce
           | KeywordReturn
           | KeywordStatic
           | KeywordSwitch
           | KeywordUnset
           | KeywordUse
           | KeywordVar
           | KeywordWhile
           | KeywordFUNCTION_
           | KeywordCLASS_
           | KeywordMETHOD_
           | KeywordFinal
           | KeywordInterface
           | KeywordImplements
           | KeywordPublic
           | KeywordPrivate
           | KeywordProtected
           | KeywordAbstract
           | KeywordClone
           | KeywordTry
           | KeywordCatch
           | KeywordThrow
           | KeywordNamespace
           | KeywordGoto
           | KeywordFinally
           | KeywordTrait
           | KeywordCallable
           | KeywordInsteadof
           | KeywordYield
           | KeywordTRAIT_
           | KeywordNAMESPACE_
           | StartHeredoc
           | EndHeredoc
           | ERROR
           | EOF
           | InlineHTML String
           | VariableToken String
           | IdentToken String
           | IntegerToken String
           | RealToken String
           | StringToken String
           | VariableTokenInStr String
           | Invalid String
           deriving (Eq, Show)
    
data TReturn = TReturn { 
    getToken:: Token, nextToken:: Tokenizer, remainingInput :: String 
    }

type Parser a = Parsec String () a
type Tokenizer = Parser TReturn

comb :: Parser String -> Parser String -> Parser String
comb = liftM2 (++)

c2s :: Parser Char -> Parser String
c2s = fmap (:[]) 

twaddle1 :: Char -> Parser Char
twaddle1 c = PC.oneOf [toLower c, toUpper c] 
twaddle :: String -> Parser String
twaddle = foldr (liftM2 (:) . twaddle1) (return "")


token0 :: Tokenizer 
token0 = start' <|>
         do
            html <- manyTill PC.anyChar (lookAhead start')
            go token0 (InlineHTML html) 
  where 
    start' = (try startEcho >> go tokenPhp KeywordEcho) <|>
             (try start >> tokenPhp) <|>
             (eof >> go token0 EOF)
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
    php = twaddle "php"

nl :: Parser String
nl = many (PC.char '\r') `comb` many (PC.char '\n')

ws :: Parser String
ws = c2s $ PC.oneOf " \t\n\r"  

tabsAndSpaces :: Parser String
tabsAndSpaces = c2s (char '\t' <|> char ' ')


go :: Tokenizer -> Token ->  Parser TReturn
go nxt t = do 
                i <- getInput
                return $ TReturn t nxt i
                

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
    try (PC.string "==" >> go' OpEqEq) <|>
    try (PC.string "===" >> go' OpEqEqEq) <|>
    try (PC.string "!=" >> go' OpNotEq) <|>
    try (PC.string "<>" >> go' OpNotEq) <|>
    try (PC.string "!==" >> go' OpNotEqEq) <|>
    try (PC.string "<=" >> go' OpLE) <|>
    try (PC.string ">=" >> go' OpGE) <|>
    try (PC.string "++" >> go' OpInc) <|>
    try (PC.string "--" >> go' OpDec) <|>
    try (PC.string "=>" >> go' OpDoubleArrow) <|>
    try (PC.string "->" >> go' OpSingleArrow) <|>
    try (PC.string "<<" >> go' OpSL) <|>
    try (PC.string ">>" >> go' OpSR) <|>
    try (PC.string "+=" >> go' OpPlusEq) <|>
    try (PC.string "-=" >> go' OpMinusEq) <|>
    try (PC.string "*=" >> go' OpMultEq) <|>
    try (PC.string "/=" >> go' OpDivEq) <|>
    try (PC.string ".=" >> go' OpConcatEq) <|>
    try (PC.string "%=" >> go' OpModEq) <|>
    try (PC.string "&=" >> go' OpAndEq) <|>
    try (PC.string "|=" >> go' OpOrEq) <|>
    try (PC.string "^=" >> go' OpXorEq) <|>
    try (PC.string "<<=" >> go' OpSLEq) <|>
    try (PC.string ">>=" >> go' OpSREq) <|>
    try (PC.string "::" >> go' OpColonColon) <|>
    try (PC.string "&&" >> go' OpLogicAnd) <|>
    try (PC.string "||" >> go' OpLogicOr) <|>
    (PC.char '(' >> go' LParen) <|>
    (PC.char ')' >> go' RParen) <|>
    (PC.char '{' >> go' LBrace) <|>
    (PC.char '}' >> go' RBrace) <|>
    (PC.char '[' >> go' LBracket) <|>
    (PC.char ']' >> go' RBracket) <|>
    (PC.char '+' >> go' OpPlus) <|>
    (PC.char '-' >> go' OpMinus) <|>
    (PC.char '/' >> go' OpSlash) <|>
    (PC.char '*' >> go' OpStar) <|>
    (PC.char '%' >> go' OpPercent) <|>
    (PC.char '^' >> go' OpCaret) <|>
    (PC.char '&' >> go' OpAmpersand) <|>
    (PC.char '|' >> go' OpPipe) <|>
    (PC.char '~' >> go' OpTilde) <|>
    (PC.char '=' >> go' OpEq) <|>
    (PC.char '<' >> go' OpLt) <|>
    (PC.char '>' >> go' OpGt) <|>
    (PC.char '.' >> go' OpDot) <|>
    (PC.char '!' >> go' OpBang) <|>
    (PC.char ',' >> go' OpComma) <|>
    (PC.char '?' >> go' OpQuestion) <|>
    (PC.char ':' >> go' OpColon) <|>
    (PC.char '@' >> go' OpAtSign) <|>
    (PC.char '$' >> go' OpDollars) <|>
    (PC.char ';' >> go' Semicolon) <|> 
    (PC.char '\\' >> go' Backslash) <|>
    try (PC.char '$' >> ident >>= variable) <|>
    try (ident  >>= keywordOrIdent) <|>
    try (int >>= goStr IntegerToken) <|>
    try (real >>= goStr RealToken) <|>
    try (stop   >> go token0 Semicolon) <|>
    try (PC.char '\'' >> tokenSqStr) <|>
    try (PC.char '`' >> go tokenBtStr Backquote) <|>
    try (PC.char '"' >> go tokenDqStr DoubleQuote) <|>
    try hereDoc <|>
    try (PC.string "/*" >> tokenMlComm) <|>
    try ((PC.string "#" <|> PC.string "//") >> tokenSlComm)
  where
    phpStop = PC.string "?>"
    scriptStop = PC.string "</script" >> many ws >> PC.string ">"
    stop = (phpStop <|> scriptStop) >> optional nl
    
    phpIsAlpha c = isAsciiLower c || isAsciiUpper c || 
                        (c == '_') || (c >= chr 127 && c <= chr 255)
    phpIsAlphaNum c = phpIsAlpha c || isDigit c
    
    ident = PC.satisfy phpIsAlpha >> many (PC.satisfy phpIsAlphaNum)
    
    int = dec <|> hex <|> oct
    
    dec = PC.string "0" <|>
            (PC.oneOf ['1'..'9'] >> many (PC.oneOf ['0'..'9']))
    hex = PC.char '0' >> PC.oneOf "xX" >> 
            many1 (PC.oneOf $ ['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F'])
    oct = PC.char '0' >> many (PC.oneOf ['0'..'7'])
    
    lnum = many1 (PC.oneOf ['0'..'9'])
    dnum = (many (PC.oneOf ['0'..'9']) >> PC.char '.' >> lnum) <|>
           (lnum >> PC.char '.' >> many (PC.oneOf ['0'..'9']))
    exponentDnum = (lnum <|> dnum) >> PC.oneOf "eE" >>
                    optional (PC.oneOf "+-") >> lnum
    real = dnum <|> exponentDnum
    
    castWs = PC.oneOf "\t "
    cs = c2s (PC.char '(') `comb` many castWs 
    ce = many castWs `comb` c2s (PC.char ')')
    
    cInteger = twaddle "integer"
    cInt = twaddle "int"
    intCast = cs `comb` (cInteger <|> cInt) `comb` ce
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

    hereDoc = do
        bflag <- optionMaybe (PC.char 'b')
        _ <- PC.string ">>>"
        _ <- tabsAndSpaces
        lbl <- ident <|> 
                    between (PC.char '\'') (PC.char '\'') ident <|> 
                    between (PC.char '"') (PC.char '"') ident
        _ <- nl
        unexpected "NYI"
    

  
variable :: String -> Tokenizer    
variable v = go tokenPhp $ VariableToken v

goStr :: (String -> Token) -> String -> Tokenizer
goStr t str = go tokenPhp (t str)

tokenSqStr = unexpected "NYI"
tokenDqStr = unexpected "NYI"
tokenBtStr = unexpected "NYI"
tokenMlComm = unexpected "NYI"
tokenSlComm = unexpected "NYI"

keywordOrIdent :: String -> Tokenizer
keywordOrIdent _ = unexpected "NYI"

