module Parser (parse) where

import Text.Parsec.Prim hiding (parse)
import Text.Parsec.Combinator
import qualified Tokenizer as T
import Control.Monad (liftM)
import AST
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

repeated :: Parser (a -> a) -> (a -> Parser a)
repeated p = \x -> ((p >>= (\f -> repeated p (f x))) <|> return x)

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


statementList :: Parser [Statement]
statementList = many statement

statement :: Parser Statement
statement = liftM InlineHTML inlineHtml <|>
            liftM StmtExpression (expression `followedBy` t T.Semicolon)
    
expression :: Parser Expression    
expression = exp00 
    where
--        exp00 = exp01 `chainl1` (t T.KeywordOr >> binOp LogicalOr) 
--        exp01 = exp02 `chainl1` (t T.KeywordXor >> binOp LogicalXor)
--        exp02 = exp03 `chainl1` (t T.KeywordAnd >> binOp LogicalAnd)
--        exp03 = exp04 `chainr1` assignmentOp
--        exp04 = exp05 `chainl1` ternaryOp 
--        exp05 = exp06 `chainl1` (t T.OpLogicOr >> binOp LogicalOr)
--        exp06 = exp07 `chainl1` (t T.OpLogicAnd >> binOp LogicalAnd)
--        exp07 = exp08 `chainl1` (t T.OpPipe >> binOp BinaryOr)
--        exp08 = exp09 `chainl1` (t T.OpAmpersand >> binOp BinaryAnd)
--        exp09 = do l <- exp10
--                   o <- equalityOp
--                   r <- exp09
--                   return (o l r)
--                <|> exp10
--        exp10 = do l <- exp11
--                   o <- inequalityOp
--                   r <- exp10
--                   return (o l r)
--                <|> exp11
--        exp11 = exp12 `chainl1` shiftOp
--        exp12 = exp13 `chainl1` addOp
--        exp13 = exp14 `chainl1` mulOp
--        exp14 = (t T.OpBang >> exp14 >>= unaryOp LogicalNot) <|> exp15
        exp00 = exp15
        exp15 = try (do l <- exp16
                        _ <- t T.KeywordInstanceOf
                        r <- classRef
                        return (ExprInstanceOf l r))
            <|> exp16
        exp16 = ((t T.OpInc >> variable) >>= return . ExprPPID PreIncrement)
            <|> ((t T.OpDec >> variable) >>= return . ExprPPID PreDecrement)
            <|> ((t T.OpTilde >> exp16) >>= unaryOp BinaryNegate)
            <|> ((t T.CastInt >> exp16) >>= unaryOp CastInt)
            <|> ((t T.CastReal >> exp16) >>= unaryOp CastReal)
            <|> ((t T.CastString >> exp16) >>= unaryOp CastString)
            <|> ((t T.CastArray >> exp16) >>= unaryOp CastArray)
            <|> ((t T.CastObject >> exp16) >>= unaryOp CastObject)
            <|> ((t T.CastBool >> exp16) >>= unaryOp CastBool)
            <|> ((t T.CastUnset >> exp16) >>= unaryOp CastUnset)
            <|> ((t T.OpAtSign >> exp16) >>= unaryOp SuppressError)
            <|> try ((variable <* t T.OpInc >>= return . ExprPPID PostIncrement))
            <|> try ((variable <* t T.OpDec >>= return . ExprPPID PostDecrement))
            <|> exp17
        exp17 = exp19 `chainr1` (t T.OpPow >> binOp Power)
        exp19 = ((t T.KeywordClone >> exp20) >>= unaryOp Clone) <|> exp20
        exp20 = between (t T.LParen) (t T.RParen) exp00 
            <|> (variable >>= return . ExprVariable) 
            <|> liftM ExprConstant constant
        
        unaryOp :: UnaryOp -> Expression -> Parser Expression            
        unaryOp u l = return (ExprUnaryOp u l)

        binOp b =  return (ExprBinaryOp b)
--
--        assignmentOp = (t T.OpEq >>  binOp Assign)
--            <|> (t T.OpPlusEq   >>  binOp PlusAssign)
--            <|> (t T.OpMinusEq  >>  binOp MinusAssign)
--            <|> (t T.OpMultEq   >>  binOp MultAssign)
--            <|> (t T.OpDivEq    >>  binOp DivAssign)
--            <|> (t T.OpConcatEq >>  binOp ConcatAssign)
--            <|> (t T.OpModEq    >>  binOp ModAssign)
--            <|> (t T.OpAndEq    >>  binOp AndAssign)
--            <|> (t T.OpOrEq     >>  binOp OrAssign)
--            <|> (t T.OpXorEq    >>  binOp XorAssign)
--            <|> (t T.OpPowEq    >>  binOp PowAssign)
--            
--        ternaryOp = between (t T.OpQuestion) (t T.OpColon)
--            (optionMaybe exp04) >>= (\m -> return (\l r -> ExprTernaryOp l m r))
--            
--        equalityOp = (t T.OpEqEq >>  binOp Equal)
--            <|> (t T.OpEqEqEq  >>  binOp Identical)
--            <|> (t T.OpNotEq   >>  binOp NotEqual)
--            <|> (t T.OpNotEqEq >>  binOp NotIdentical)
--
--        inequalityOp = (t T.OpLt >>  binOp Less)
--            <|> (t T.OpLE >>  binOp LessEqual)
--            <|> (t T.OpGt >>  binOp Greater)
--            <|> (t T.OpGE >>  binOp GreaterEqual)
--            
--        shiftOp = (t T.OpSL >>  binOp ShiftLeft)
--            <|> (t T.OpSR >>  binOp ShiftRight)
--            
--        addOp = (t T.OpPlus >>  binOp Add)
--            <|> (t T.OpMinus >>  binOp Subtract)
--            <|> (t T.OpDot >>  binOp Concat)
--            
--        mulOp = (t T.OpStar >>  binOp Multiply)
--            <|> (t T.OpSlash >>  binOp Divide)
--            <|> (t T.OpPercent >>  binOp Modulus)
--
        

constant :: Parser Constant
constant = liftM ConstantString tString
    <|> liftM ConstantInteger tInteger
    <|> liftM ConstantReal tReal
    <|> liftM ConstantString nowDoc
    <|> liftM ConstantFromIdentifier ident
    <|> liftM ConstantString (between tStartInterpolatedString (t T.EndInterpolatedString) tStringFragment) 
       
variable :: Parser Variable
variable = 
    (liftM VariableSimple tVariable) >>= repeated aryIdx

classRef :: Parser ClassRef
classRef = (className >>= return . CRClassName)
    <|> (variable >>= return . CRDynamic)
    
className :: Parser ClassName
className = (t T.KeywordStatic >> return ClassStatic)
    <|> (do ns <- namespacePrefix
            cl <- ident
            return (ClassName ns cl))

namespacePrefix :: Parser Namespace
namespacePrefix = (try (t T.KeywordNamespace >> t T.Backslash >> return NSSelf) <|>
             try (t T.Backslash >> return NSGlobal) <|>
             return NSUnspecified)
             >>= repeated 
                    (try (ident `followedBy` t T.Backslash >>= \i -> return (\ns ->  (NS ns i))))
            

aryIdx :: Parser (Variable -> Variable)    
aryIdx = do sub <- between (t T.LBracket) (t T.RBracket) expression
            return (\a -> VariableOffset a sub)
 