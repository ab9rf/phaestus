module Parser (parse, Statement(..)) where

import Text.Parsec.Prim hiding (parse)
import Text.Parsec.Combinator
import qualified Tokenizer as T
import Control.Monad (liftM)
import Control.Applicative ((<*))

type ParserState = ()

type Parser a = Parsec [T.Token] ParserState a

data ParseError = ParseError String    
    deriving (Show, Eq)

parse :: [T.Token] -> Either ParseError [Statement]
parse s = case runParser statementList () "" s of
    Left e    -> Left (ParseError (show e))
    Right res -> Right res

followedBy :: Parser a -> Parser b -> Parser a
p1 `followedBy` p2 = p1 >>= (\x -> p2 >> return x)

satisfy :: (T.Token -> Bool) -> Parser T.Token
satisfy f = tokenPrim (\c -> show [c])
                        (\pos _c _cs -> pos)
                        (\c -> if f c then Just c else Nothing)

t :: T.Token -> Parser T.Token
t t' = satisfy (==t')

inlineHtml :: Parser T.Token
ident :: Parser T.Token
tString :: Parser T.Token
tVariable :: Parser T.Token
tInteger :: Parser T.Token
tReal :: Parser T.Token
tInterpolatedIndexIdent :: Parser T.Token
tInterpolatedIndexInt :: Parser T.Token
tInterpolatedProperty :: Parser T.Token
tInterpolatedVariable :: Parser T.Token
tStringFragment :: Parser T.Token
tStartInterpolatedString :: Parser T.Token
nowDoc :: Parser T.Token

inlineHtml = satisfy (\x -> case x of (T.InlineHTML _) -> True; _ -> False)
tString = satisfy (\x -> case x of (T.StringToken _ _) -> True; _ -> False)
tVariable = satisfy (\x -> case x of (T.VariableToken _) -> True; _ -> False)
tInteger = satisfy (\x -> case x of (T.IntegerToken _) -> True; _ -> False)
tReal = satisfy (\x -> case x of (T.RealToken _) -> True; _ -> False)
nowDoc = satisfy (\x -> case x of (T.NowDoc{}) -> True; _ -> False)

tInterpolatedIndexIdent = satisfy (\x -> case x of (T.InterpolatedIndexIdent _) -> True; _ -> False)
tInterpolatedIndexInt = satisfy (\x -> case x of (T.InterpolatedIndexInt _) -> True; _ -> False)
tInterpolatedProperty = satisfy (\x -> case x of (T.InterpolatedProperty _) -> True; _ -> False)
tInterpolatedVariable = satisfy (\x -> case x of (T.InterpolatedVariable _) -> True; _ -> False)
tStringFragment = satisfy (\x -> case x of (T.StringFragment _) -> True; _ -> False)

tStartInterpolatedString = satisfy (\x -> case x of (T.StartInterpolatedString _) -> True; _ -> False)

ident = satisfy (\x -> case x of (T.IdentToken _) -> True; _ -> False)

data Statement = InlineHTML T.Token
    | StmtExpression Expression
    deriving (Show, Eq)
    
data Expression = ExprConstant Constant
    | ExprUnaryOp UnaryOp Expression
    | ExprBinaryOp BinaryOp Expression Expression
    | ExprTernaryOp Expression (Maybe Expression) Expression
    deriving (Show, Eq)

data Constant = ConstantString T.Token
    | ConstantInteger T.Token
    | ConstantReal T.Token
    | ConstantFromIdentifier T.Token
    deriving (Show, Eq)

data UnaryOp = Clone | PreIncrement | PreDecrement | BinaryNegate 
    | CastInt | CastReal | CastString | CastArray | CastObject | CastBool
    | CastUnset | SuppressError | PostIncrement | PostDecrement | LogicalNot
    deriving (Show, Eq)
    
data BinaryOp = LogicalOr | LogicalXor | LogicalAnd | BinaryOr | BinaryAnd
    | Power | Assign | PlusAssign | MinusAssign | MultAssign | DivAssign
    | ConcatAssign | ModAssign | AndAssign | OrAssign | XorAssign | PowAssign
    | Equal | NotEqual | Identical | NotIdentical | Greater | Less 
    | GreaterEqual | LessEqual | ShiftLeft | ShiftRight | Add | Subtract
    | Concat | Multiply | Divide | Modulus | Subscript | InstanceOf
    deriving (Show, Eq)
    

statementList :: Parser [Statement]
statementList = many statement

statement :: Parser Statement
statement = liftM InlineHTML inlineHtml <|>
            liftM StmtExpression (expression `followedBy` t T.Semicolon)
    
expression :: Parser Expression    
expression = exp00 
    where
        exp00 :: Parser Expression
        exp00 = exp01 `chainl1` (t T.KeywordOr >> binOp LogicalOr)    
        exp01 = exp02 `chainl1` (t T.KeywordXor >> binOp LogicalXor)
        exp02 = exp03 `chainl1` (t T.KeywordAnd >> binOp LogicalAnd)
        exp03 = exp04 `chainr1` assignmentOp
        exp04 :: Parser Expression
        exp04 = exp05 `chainl1` ternaryOp 
        exp05 = exp06 `chainl1` (t T.OpLogicOr >> binOp LogicalOr)
        exp06 = exp07 `chainl1` (t T.OpLogicAnd >> binOp LogicalAnd)
        exp07 = exp08 `chainl1` (t T.OpPipe >> binOp BinaryOr)
        exp08 = exp09 `chainl1` (t T.OpAmpersand >> binOp BinaryAnd)
        exp09 = do l <- exp10
                   o <- equalityOp
                   r <- exp10
                   return (o l r)
                <|> exp10
        exp10 :: Parser Expression
        exp10 = do l <- exp11
                   o <- inequalityOp
                   r <- exp11
                   return (o l r)
                <|> exp11
        exp11 = exp12 `chainl1` shiftOp
        exp12 = exp13 `chainl1` addOp
        exp13 = exp14 `chainl1` mulOp
        exp14 = (t T.OpBang >> exp14 >>= unaryOp LogicalNot) <|> exp15
        exp15 = do l <- exp16
                   _ <- t T.KeywordInstanceOf
                   r <- exp16
                   return (ExprBinaryOp InstanceOf l r)
        exp16 = ((t T.OpInc >> exp16) >>= unaryOp PreIncrement)
            <|> ((t T.OpDec >> exp16) >>= unaryOp PreDecrement)
            <|> ((t T.OpTilde >> exp16) >>= unaryOp BinaryNegate)
            <|> ((t T.CastInt >> exp16) >>= unaryOp CastInt)
            <|> ((t T.CastReal >> exp16) >>= unaryOp CastReal)
            <|> ((t T.CastString >> exp16) >>= unaryOp CastString)
            <|> ((t T.CastArray >> exp16) >>= unaryOp CastArray)
            <|> ((t T.CastObject >> exp16) >>= unaryOp CastObject)
            <|> ((t T.CastBool >> exp16) >>= unaryOp CastBool)
            <|> ((t T.CastUnset >> exp16) >>= unaryOp CastUnset)
            <|> ((t T.OpAtSign >> exp16) >>= unaryOp SuppressError)
            <|> ((exp17 <* t T.OpInc) >>= unaryOp PostIncrement)
            <|> ((exp17 <* t T.OpDec) >>= unaryOp PostDecrement)
            <|> exp17
        exp17 :: Parser Expression
        exp17 = exp18 `chainr1` (t T.OpPow >> binOp Power)
        exp18 :: Parser Expression
        exp18 = aryIdx <|> exp19 
        exp19 = (t T.KeywordClone >> exp20) >>= unaryOp Clone
        exp20 :: Parser Expression
        exp20 = between (t T.LParen) (t T.RParen) exp00 <|>
                liftM ExprConstant constant
                    
        unaryOp u l = return (ExprUnaryOp u l)

        binOp :: BinaryOp -> Parser (Expression -> Expression -> Expression)
        binOp b =  return (ExprBinaryOp b)

        assignmentOp = (t T.OpEq >>  binOp Assign)
            <|> (t T.OpPlusEq   >>  binOp PlusAssign)
            <|> (t T.OpMinusEq  >>  binOp MinusAssign)
            <|> (t T.OpMultEq   >>  binOp MultAssign)
            <|> (t T.OpDivEq    >>  binOp DivAssign)
            <|> (t T.OpConcatEq >>  binOp ConcatAssign)
            <|> (t T.OpModEq    >>  binOp ModAssign)
            <|> (t T.OpAndEq    >>  binOp AndAssign)
            <|> (t T.OpOrEq     >>  binOp OrAssign)
            <|> (t T.OpXorEq    >>  binOp XorAssign)
            <|> (t T.OpPowEq    >>  binOp PowAssign)
            
        ternaryOp = between (t T.OpQuestion) (t T.OpColon)
            (optionMaybe exp04) >>= (\m -> return (\l r -> ExprTernaryOp l m r))
            
        equalityOp = (t T.OpEqEq >>  binOp Equal)
            <|> (t T.OpEqEqEq  >>  binOp Identical)
            <|> (t T.OpNotEq   >>  binOp NotEqual)
            <|> (t T.OpNotEqEq >>  binOp NotIdentical)

        inequalityOp = (t T.OpLt >>  binOp Less)
            <|> (t T.OpLE >>  binOp LessEqual)
            <|> (t T.OpGt >>  binOp Greater)
            <|> (t T.OpGE >>  binOp GreaterEqual)
            
        shiftOp = (t T.OpSL >>  binOp ShiftLeft)
            <|> (t T.OpSR >>  binOp ShiftRight)
            
        addOp = (t T.OpPlus >>  binOp Add)
            <|> (t T.OpMinus >>  binOp Subtract)
            <|> (t T.OpDot >>  binOp Concat)
            
        mulOp = (t T.OpStar >>  binOp Multiply)
            <|> (t T.OpSlash >>  binOp Divide)
            <|> (t T.OpPercent >>  binOp Modulus)

        aryIdx :: Parser Expression    
        aryIdx = do ary <- exp19
                    sub <- between (t T.LBracket) (t T.RBracket) exp00
                    return (ExprBinaryOp Subscript ary sub)
        

constant :: Parser Constant
constant = liftM ConstantString tString
    <|> liftM ConstantInteger tInteger
    <|> liftM ConstantReal tReal
    <|> liftM ConstantString nowDoc
    <|> liftM ConstantFromIdentifier ident
    <|> liftM ConstantString (between tStartInterpolatedString (t T.EndInterpolatedString) tStringFragment) 
       