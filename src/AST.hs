{-# LANGUAGE DeriveFunctor #-}

module AST
  ( Statement(..)
  , Expression(..)
  , Constant(..)
  , UnaryOp(..)
  , BinaryOp(..)
  , Variable(..)
  , PPIDType(..)
  , ClassName(..)
  , Namespace(..)
  , ClassRef(..)
  )
where

import qualified Tokenizer                     as T

data Statement a = InlineHTML T.Token
    | StmtExpression (Expression a)
    deriving (Show, Eq, Functor)

data Expression a = ExprConstant (Constant a)
    | ExprVariable (Variable a)
    | ExprUnaryOp UnaryOp (Expression a)
    | ExprBinaryOp BinaryOp (Expression a) (Expression a)
    | ExprTernaryOp (Expression a) (Maybe (Expression a)) (Expression a)
    | ExprPPID PPIDType (Variable a)
    | ExprInstanceOf (Expression a) (ClassRef a)
    deriving (Show, Eq, Functor)

data Constant a = ConstantString T.Token
    | ConstantInteger T.Token
    | ConstantReal T.Token
    | ConstantFromIdentifier T.Token
    deriving (Show, Eq, Functor)

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

data Variable a = VariableSimple T.Token
    | VariableOffset (Variable a) (Expression a)
    deriving (Show, Eq, Functor)

data ClassRef a = CRClassName ClassName
    | CRDynamic (Variable a)
    deriving (Show, Eq, Functor)

data ClassName = ClassStatic
    | ClassName Namespace T.Token
    deriving (Show, Eq)

data Namespace = NSGlobal | NSSelf | NSUnspecified | NS Namespace T.Token
    deriving (Show, Eq)

