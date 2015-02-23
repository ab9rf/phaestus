module Tokenizer (tokenize, Token(..))
where

import Text.Parsec 
import qualified Text.Parsec.Char as PC

import Data.Char (toLower, toUpper, chr, isAsciiLower, isAsciiUpper, isDigit)
import Control.Monad (liftM2, liftM, liftM3)


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
           | Keyword__FILE__
           | Keyword__LINE__
           | Keyword__DIR__
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
           | Keyword__FUNCTION__
           | Keyword__CLASS__
           | Keyword__METHOD__
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
           | Keyword__TRAIT__
           | Keyword__NAMESPACE__
           | StartHeredoc
           | EndHeredoc
           | InlineHTML String
           | VariableToken String
           | IdentToken String
           | IntegerToken String
           | RealToken String
           | StringToken String
           | VariableTokenInStr String
           | Invalid String
           deriving (Eq, Show)
    
type Parser a = Parsec String () a
type Tokenizer = Parser [Token]

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
             (eof >> return [])         -- EOF does not chain
    phpStart = char '<' >> PC.char '?' >> optional php
    scriptStart = PC.string "<script" >> many1 ws >>
                    PC.string "language" >> many ws >>
                    PC.string "=" >> many ws >>
                    PC.oneOf "\'\"" >> many ws >> 
                    php >> many ws >>
                    PC.oneOf "\'\"" >> many ws >>
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

-- go returns a single token and proceeds to parse another one.
go :: Tokenizer -> Token ->  Tokenizer
go nxt t = liftM (t :) nxt

tokenPhp :: Tokenizer
tokenPhp = let go' = go tokenPhp  in
    (eof         >> return []) <|>           -- EOF does not chain!
    try (stop   >> go token0 Semicolon) <|>
    try hereDoc <|>
    try mlComm <|>
    try ((PC.string "#" <|> PC.string "//") >> tokenSlComm) <|>
    try (intCast     >> go' CastInt) <|>
    try (realCast    >> go' CastReal) <|>
    try (stringCast  >> go' CastString) <|>
    try (arrayCast   >> go' CastArray) <|>
    try (objectCast  >> go' CastObject) <|>
    try (boolCast    >> go' CastBool) <|>
    try (unsetCast   >> go' CastUnset) <|>
    try (PC.string "===" >> go' OpEqEqEq) <|>
    try (PC.string "==" >> go' OpEqEq) <|>
    try (PC.string "!==" >> go' OpNotEqEq) <|>
    try (PC.string "!=" >> go' OpNotEq) <|>
    try (PC.string "<>" >> go' OpNotEq) <|>
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
    try (real >>= goStr RealToken) <|>
    try (int >>= goStr IntegerToken) <|>
    try (PC.char '\'' >> tokenSqStr) <|>
    try (PC.char '`' >> go tokenBtStr Backquote) <|>
    try (PC.char '"' >> go tokenDqStr DoubleQuote) <|>
    (many1 ws >> tokenPhp) <|>
    (PC.anyChar >>= \c -> goStr Invalid [c])
  where
    phpStop = PC.string "?>"
    scriptStop = PC.string "</script" >> many ws >> PC.string ">"
    stop = (phpStop <|> scriptStop) >> optional nl
    
    phpIsAlpha c = isAsciiLower c || isAsciiUpper c || 
                        (c == '_') || (c >= chr 127 && c <= chr 255)
    phpIsAlphaNum c = phpIsAlpha c || isDigit c
    
    ident = liftM2 (:) (PC.satisfy phpIsAlpha) (many (PC.satisfy phpIsAlphaNum))
    
    int = dec <|> hex <|> oct
    
    dec = PC.string "0" <|>
            liftM2 (:) (PC.oneOf ['1'..'9']) (many (PC.oneOf ['0'..'9']))
    hex = liftM3 (\a b c -> a:b:c) 
            (PC.char '0')
            (PC.oneOf "xX")  
            (many1 (PC.oneOf $ ['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F']))
    oct = liftM2 (:) (PC.char '0') (many (PC.oneOf ['0'..'7']))
    
    lnum = many1 (PC.oneOf ['0'..'9'])
    dnum = liftM3 (\a b c -> a ++ b:c)
                (many (PC.oneOf ['0'..'9'])) (PC.char '.') lnum 
           <|>
           liftM3 (\a b c -> a ++ b:c)
                lnum (PC.char '.') (many (PC.oneOf ['0'..'9']))
    exponentDnum = (lnum <|> dnum) >> PC.oneOf "eE" >>
                    optional (PC.oneOf "+-") >> lnum
    real = dnum <|> exponentDnum
    
    castWs = PC.oneOf "\t "
    cs = c2s (PC.char '(') `comb` many castWs 
    ce = many castWs `comb` c2s (PC.char ')')
    
    cInteger = twaddle "integer"
    cInt = twaddle "int"
    intCast = liftM3 (\a b c -> a++b++c) cs (try cInteger <|> cInt) ce
    cFloat = twaddle "float"
    cReal = twaddle "real"
    cDouble = twaddle "double"
    realCast = liftM3 (\a b c -> a++b++c) cs (cFloat <|> cReal <|> cDouble) ce
    
    cString = twaddle "string"
    stringCast = cs >> cString >> ce
    
    cArray = twaddle "array"
    arrayCast = cs >> cArray >> ce
    
    cObject = twaddle "object"
    objectCast = cs >> cObject >> ce
    
    cBool = twaddle "bool"
    cBoolean = twaddle "boolean"
    boolCast = cs >> (try cBoolean <|> cBool) >> ce
    
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

    mlComm = do
        ctext <- between (PC.string "/*") (PC.string "*/") 
                    (manyTill PC.anyChar (lookAhead (string "*/")))
        tokenPhp

  
variable :: String -> Tokenizer    
variable v = go tokenPhp $ VariableToken v

goStr :: (String -> Token) -> String -> Tokenizer
goStr t str = go tokenPhp (t str)

tokenSqStr = unexpected "NYI"
tokenDqStr = unexpected "NYI"
tokenBtStr = unexpected "NYI"
tokenSlComm = unexpected "NYI"

keywordOrIdent :: String -> Tokenizer
keywordOrIdent str = go tokenPhp (keyword (toLowerStr str))
    where toLowerStr = map toLower
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

tokenize :: String -> [Token]
tokenize s = let Right toks = parse token0 "" s in toks
