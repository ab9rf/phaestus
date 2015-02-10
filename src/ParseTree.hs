module ParseTree (
    PVDouble(..),
    PVIdent(..),
    PVInline(..),
    PVInteger(..),
    PVString(..),
    PVVariableName(..),
    PVVariableNameImbed(..),
    Identifier(..)
) where


data PVDouble = PVDouble String
  deriving (Show, Eq)
data PVIdent = PVIdent String
  deriving (Show, Eq)
data PVInline = PVInline String
  deriving (Show, Eq)
data PVInteger = PVInteger String
  deriving (Show, Eq)
data PVString = PVString String
  deriving (Show, Eq)
data PVVariableName = PVVariableName String
  deriving (Show, Eq)
data PVVariableNameImbed = PVVariableNameImbed String
  deriving (Show, Eq)

data Identifier = Identifier String  
  deriving (Show, Eq)