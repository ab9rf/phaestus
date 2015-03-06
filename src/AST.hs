module AST (Statement(..)
    , Expression (..)
    , Constant(..)
    , UnaryOp(..)
    , BinaryOp(..) 
    ) where

import qualified Tokenizer as T

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
    
