module Tokenizer (tokenize, Token(..))
where

import Text.Parsec hiding (tokens, Error)
import qualified Text.Parsec.Char as PC

import Data.Char (toLower, toUpper, chr, isAsciiLower, isAsciiUpper, isDigit)
import Control.Monad (liftM2, liftM3, liftM, void)
import Data.Maybe (maybeToList, fromJust)
import Control.Applicative ((<$>))
import qualified Control.Applicative as CA
import Data.List (elemIndex)


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
           | EndInterpolatedString
           | KeywordAnd
           | KeywordOr
           | KeywordXor
           | Keyword'FILE
           | Keyword'LINE
           | Keyword'DIR
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
           | Keyword'FUNCTION
           | Keyword'CLASS
           | Keyword'METHOD
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
           | Keyword'TRAIT
           | Keyword'NAMESPACE
           | NowDoc Bool String String
           | StartHereDoc Bool String
           | StartInterpolatedString Bool
           | EndHereDoc
           | StringFragment String
           | InterpolatedVariable String
           | InterpolatedProperty String
           | InterpolatedIndexIdent String
           | InterpolatedIndexInt String
           | InlineHTML String
           | VariableToken String
           | IdentToken String
           | IntegerToken String
           | RealToken String
           | StringToken Bool String
           | Invalid String
           | Error String
           deriving (Eq, Show)
    
type Parser a = Parsec String ParserState a
type Tokenizer = Parser [Token]

newtype ParserState = ParserState [Tokenizer]

illegal :: Tokenizer
illegal = PC.anyChar >>= \ c -> go' $ Invalid [c]

twaddle1 :: Char -> Parser Char
twaddle1 c = PC.oneOf [toLower c, toUpper c] 
twaddle :: String -> Parser String
twaddle = foldr (liftM2 (:) . twaddle1) (return "")

infixr 2 >:+> 
(>:+>) :: Parser a -> Parser [a] -> Parser [a]
l >:+> r = liftM2 (:) l r 

infixr 2 >++>
(>++>) :: Parser [a] -> Parser [a] -> Parser [a]
l >++> r = liftM2 (++) l r

infixr 2 >+:>
(>+:>) :: Parser [a] -> Parser a -> Parser [a]
l >+:> r = liftM2 (\l' r' -> l' ++ [r']) l r

token0 :: Tokenizer 
token0 = start' <|>
            (do
                html <- PC.anyChar >:+> manyTill PC.anyChar (lookAhead start')
                return [InlineHTML html]) <|>
            illegal
  where 
    start' = (try startEcho >> next tokenPhp >> return [KeywordEcho]) <|>
             (try start >> next tokenPhp >> return []) <|>
             (eof >> return [])         
    phpStart = char '<' >> PC.char '?' >> optional php
    scriptStart = PC.string "<script" >> many1 ws >>
                    PC.string "language" >> many ws >>
                    PC.string "=" >> many ws >>
                    PC.oneOf "\'\"" >> many ws >> 
                    php >> many ws >>
                    PC.oneOf "\'\"" >> many ws >>
                    PC.string ">" >> return ()
    start = try phpStart <|> scriptStart
    startEcho = PC.string "<?="
    php = twaddle "php"

lnum :: Parser String
lnum = many1 (PC.oneOf ['0'..'9'])

ident :: Parser String
ident = PC.satisfy phpIsAlpha >:+> many (PC.satisfy phpIsAlphaNum)
  where 
    phpIsAlpha c = isAsciiLower c || isAsciiUpper c || 
                    (c == '_') || (c >= chr 127 && c <= chr 255)
    phpIsAlphaNum c = phpIsAlpha c || isDigit c

nl :: Parser String
nl = try (PC.string "\r\n") <|> try (PC.string "\r") <|> try (PC.string "\n")

ws :: Parser Char
ws = PC.oneOf " \t\n\r"  

go' :: Token -> Tokenizer
go' = return . (:[])

go :: Tokenizer -> Token ->  Tokenizer
go nxt t = do next nxt; go' t
    
next :: Tokenizer -> Parser ()
next nxt = modifyState (\(ParserState (_:tt)) -> ParserState (nxt:tt))

shift :: Tokenizer -> Tokenizer
shift nxt = do next nxt; return []

tokenPhp :: Tokenizer
tokenPhp = 
    (eof         >> return []) <|>           -- EOF does not chain!
    try (stop   >> go token0 Semicolon) <|>
    try nowDoc <|>
    try hereDoc <|>
    try mlComm <|>
    try slComm <|>
    try (intCast     >> go' CastInt) <|>
    try (realCast    >> go' CastReal) <|>
    try (stringCast  >> go' CastString) <|>
    try (arrayCast   >> go' CastArray) <|>
    try (objectCast  >> go' CastObject) <|>
    try (boolCast    >> go' CastBool) <|>
    try (unsetCast   >> go' CastUnset) <|>
    try (PC.char '$' >> ident >>= \v -> go' $ VariableToken v) <|>
    try real <|>
    try int <|>
    try sqStr <|>
    try dqStr <|>
    try bqStr <|>
    try (ident  >>= keywordOrIdent) <|>
    try (PC.string "<<=" >> go' OpSLEq) <|>
    try (PC.string ">>=" >> go' OpSREq) <|>
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
    try (PC.string "::" >> go' OpColonColon) <|>
    try (PC.string "&&" >> go' OpLogicAnd) <|>
    try (PC.string "||" >> go' OpLogicOr) <|>
    (PC.char '(' >> go' LParen) <|>
    (PC.char ')' >> go' RParen) <|>
    lbrace <|>
    rbrace <|>
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
    (many1 ws >> return [] ) <|>
    illegal
  where
    lbrace = PC.char '{' >> do
        modifyState (\(ParserState s@(st:_)) -> ParserState (st:s))
        go' LBrace
    rbrace = PC.char '}' >> do
        modifyState pop
        go' RBrace
      where pop (ParserState [s]) = ParserState [s]
            pop (ParserState (_:t)) = ParserState t
            pop (ParserState []) = error "empty pop"
    phpStop = PC.string "?>"
    scriptStop = PC.string "</script" >> many ws >> PC.string ">"
    stop = (phpStop <|> scriptStop) >> optional nl
    
    int = (try bin <|> try hex <|> try oct <|> dec) >>= \s -> go' $ IntegerToken s
    
    dec = PC.string "0" <|>
            PC.oneOf ['1'..'9'] >:+> many (PC.oneOf ['0'..'9'])
    hex = PC.char '0' >:+>
            PC.oneOf "xX" >:+> 
            many1 (PC.oneOf $ ['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F'])
    bin = liftM3 (\a b c -> a:b:c) (PC.char '0') (PC.oneOf "bB") (many1 (PC.oneOf "01"))
    oct = liftM2 (:) (PC.char '0') (many (PC.oneOf ['0'..'7']))
    
    dnum = try (many (PC.oneOf ['0' .. '9']) >++> PC.char '.' >:+> lnum)
           <|>
           try (lnum >++> PC.char '.' >:+> many (PC.oneOf ['0' .. '9']))
    exponentDnum = try (dnum <|> lnum) >++> 
                        PC.oneOf "eE" >:+>
                        maybeToList <$> optionMaybe (PC.oneOf "+-") >++> 
                        lnum
    real = (try exponentDnum <|> dnum) >>= \s -> go' $ RealToken s
    
    castWs = PC.oneOf "\t "
    cs = PC.char '(' >:+> many castWs 
    ce = many castWs >+:> PC.char ')'
    
    cInteger = twaddle "integer"
    cInt = twaddle "int"
    intCast = cs >++> (try cInteger <|> cInt) >++> ce
    cFloat = twaddle "float"
    cReal = twaddle "real"
    cDouble = twaddle "double"
    realCast = cs >++> (cFloat <|> cReal <|> cDouble) >++> ce
    
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

    herenow labelP = do
        bflag <- option False (PC.char 'b' >> return True)
        _ <- PC.string "<<<"
        _ <- many (PC.oneOf " \t")
        lbl <- labelP
        _ <- try (lookAhead (end lbl)) <|> nl
        return (bflag, lbl, end lbl)
        where end lbl = nl >> PC.string lbl >> lookAhead (try (optional (PC.char ';') >> nl)) 
    
    hereDoc = do
        (bflag, lbl, end) <- herenow (try ident <|> between (PC.char '"') (PC.char '"') ident)
        go (tokenHd end) (StartHereDoc bflag lbl)
        where tokenHd end = shift (interpolated "" (try end >> go tokenPhp EndHereDoc))

    nowDoc = do
        (bflag, lbl, end) <- herenow (between (PC.char '\'') (PC.char '\'') ident)
        manyTill PC.anyChar (try end) >>= go' . NowDoc bflag lbl

    mlComm = do
        ctext <- between (PC.string "/*") (PC.string "*/") 
                    (manyTill PC.anyChar (lookAhead (try (string "*/"))))
        return [] 
        
    slComm = do 
        ctext <- between (PC.string "#" <|> PC.string "//") 
                    (void (try nl) <|> eof)
                    (manyTill PC.anyChar (void (try nl) <|> eof))
        return [] 
        
    sqStr = do 
        bflag <- option False (PC.oneOf "bB" >> return True)
        sval <- between (PC.char '\'') (PC.char '\'')
                    (many 
                        ((PC.char '\\' >> (PC.oneOf "'\\" <|> return '\\')) 
                        <|> PC.noneOf "'"))
        go' $ StringToken bflag sval         

    dqStr = do 
        bflag <- option False (PC.oneOf "bB" >> return True)
        _ <- PC.char '"'
        go tokenDq $ StartInterpolatedString bflag 
        
    bqStr = PC.char '`' >> go tokenBq Backquote

    tokenDq = shift $ interpolated "\"" (PC.char '"' >> go tokenPhp EndInterpolatedString)

    tokenBq = shift $ interpolated "`" (PC.char '`' >> go tokenPhp Backquote)

interpolated :: String -> Tokenizer -> Tokenizer
interpolated q end = try i'
            <|> (manyTill c' (try (lookAhead i')) >>= go' . StringFragment) 
            <|> illegal
    where 
        i' = (eof >> return []) 
            <|> try end 
            <|> try (between (PC.string "${") (PC.string "}") ident
                    >>= go' . InterpolatedVariable )
            <|> try (PC.char '$' >> ident
                    >>= go (interpolated' (interpolated q end)) . InterpolatedVariable)
            <|> try (PC.char '{' >> lookAhead (PC.char '$') >>
                    modifyState (\(ParserState s) -> ParserState (tokenPhp:s)) >> 
                    return [LBrace])
        c' = try (PC.char '\\' >>
                (       (PC.char 'n' >> return '\n') 
                    <|> (PC.char 'r' >> return '\r') 
                    <|> (PC.char 't' >> return '\t')
                    <|> (PC.char 'v' >> return '\v')
                    <|> (PC.char 'e' >> return (chr 27))
                    <|> (PC.char 'f' >> return '\f')
                    <|> (PC.char '\\' >> return '\\')
                    <|> (PC.char '$' >> return '$')
                    <|> PC.oneOf q
                    <|> liftM chr (octal <|> hex)))
             <|> PC.anyChar
             
        octal = intParser 8 3 
        hex = PC.char 'x' >> intParser 16 2

intParser :: Int -> Int -> Parser Int
intParser base m' = PC.oneOf digits >>= (p (pred m') . value)
    where 
        p 0 i = return i
        p m i = (try (PC.oneOf digits) >>= (\j -> p (pred m) ((i * base) + value j))) <|> return i
        digitsL = take base (['0'..'9'] ++ ['a'..'z'])
        digitsU = take base (['0'..'9'] ++ ['A'..'Z'])
        digits = digitsL ++ digitsU
        value c = fromJust (elemIndex c digitsL CA.<|> elemIndex c digitsU)
        
        
-- this parser handles interpolated variables , possibly followed by 
-- array indices or method calls    
interpolated' :: Tokenizer -> Tokenizer
interpolated' ret =
    try (PC.string "->" >> ident >>= (go' . InterpolatedProperty)) <|>
    try (between (PC.string "[") (PC.string "]") ident >>= (go' . InterpolatedIndexIdent)) <|>
    try (between (PC.string "[") (PC.string "]") lnum >>= (go' . InterpolatedIndexInt)) <|>
    shift ret
         
keywordOrIdent :: String -> Tokenizer
keywordOrIdent str = go' (keyword (toLowerStr str))
    where toLowerStr = map toLower
          keyword "and"           = KeywordAnd  
          keyword "or"            = KeywordOr
          keyword "xor"           = KeywordXor
          keyword "__file__"      = Keyword'FILE
          keyword "__line__"      = Keyword'LINE
          keyword "__dir__"       = Keyword'DIR
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
          keyword "__function__"  = Keyword'FUNCTION
          keyword "__class__"     = Keyword'CLASS
          keyword "__method__"    = Keyword'METHOD
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
          keyword "__trait__"     = Keyword'TRAIT
          keyword "__namespace__" = Keyword'NAMESPACE
          keyword _               = IdentToken str 

tokens :: Parser [Token]
tokens = do 
            ParserState (p:_) <- getState
            x <- p
            (eof >> return x) <|> (return x >++> tokens)

tokenize :: String -> [Token]
tokenize s = case runParser tokens (ParserState [token0]) "" s of
    Left e -> [Error (show e)]
    Right toks -> toks