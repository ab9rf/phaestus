module AST (Statement(..)
    , Expression (..)
    , Constant(..)
    , UnaryOp(..)
    , BinaryOp(..)
    , Variable(..)
    , PPIDType(..) 
    , ClassName(..)
    , Namespace(..)
    , ClassRef(..)
    ) where

import qualified Tokenizer as T

data Statement = InlineHTML T.Token
    | StmtExpression Expression
    deriving (Show, Eq)
    
data Expression = ExprConstant Constant
    | ExprVariable Variable
    | ExprUnaryOp UnaryOp Expression
    | ExprBinaryOp BinaryOp Expression Expression
    | ExprTernaryOp Expression (Maybe Expression) Expression
    | ExprPPID PPIDType Variable
    | ExprInstanceOf Expression ClassRef
    deriving (Show, Eq)

data Constant = ConstantString T.Token
    | ConstantInteger T.Token
    | ConstantReal T.Token
    | ConstantFromIdentifier T.Token
    deriving (Show, Eq)

data UnaryOp = Clone | BinaryNegate 
    | CastInt | CastReal | CastString | CastArray | CastObject | CastBool
    | CastUnset | SuppressError | LogicalNot
    deriving (Show, Eq)
    
data PPIDType = PreIncrement | PreDecrement | PostIncrement | PostDecrement
    deriving (Show, Eq)
    
data BinaryOp = LogicalOr | LogicalXor | LogicalAnd | BinaryOr | BinaryAnd
    | Power | Assign | PlusAssign | MinusAssign | MultAssign | DivAssign
    | ConcatAssign | ModAssign | AndAssign | OrAssign | XorAssign | PowAssign
    | Equal | NotEqual | Identical | NotIdentical | Greater | Less 
    | GreaterEqual | LessEqual | ShiftLeft | ShiftRight | Add | Subtract
    | Concat | Multiply | Divide | Modulus | Subscript | InstanceOf
    deriving (Show, Eq)
    
data Variable = VariableSimple T.Token
    | VariableOffset Variable Expression
    deriving (Show, Eq)
    
data ClassRef = CRClassName ClassName
    | CRDynamic Variable
    deriving (Show, Eq)
    
data ClassName = ClassStatic
    | ClassName Namespace T.Token
    deriving (Show, Eq)
    
data Namespace = NSGlobal | NSSelf | NSUnspecified | NS Namespace T.Token
    deriving (Show, Eq)
    