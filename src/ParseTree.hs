module ParseTree (
        PHPIdent (..),
        PHPClassNameScalar (..),
        PHPClassName (..),
        PHPIsReference (..),
        PHPQualifiedIdentifier (..),
        PHPStatement (..),
        PHPScalar (..),
        PHPStaticArrayPair (..),
        
        namespaceRelative,
        namespaceSelf,
        namespaceAbsolute
                
) where

data PHPStatement = PHPChangeNamespace [PHPIdent]
                  | PHPNamespace [PHPIdent] [PHPStatement]
                  | PHPUseDeclaration [(PHPQualifiedIdentifier,PHPIdent)] 
                  | PHPConstantDeclaration [(PHPIdent,PHPScalar)]
                  
data PHPScalar = PHPConstant PHPQualifiedIdentifier
               | PHPStaticUnaryPlus PHPScalar
               | PHPStaticUnaryMinus PHPScalar
               | PHPStaticArray [PHPStaticArrayPair]
               | PHPMagicClass                  

newtype PHPIdent = PHPIdent String

data PHPClassNameScalar = PHPClassNameScalar PHPClassName
        
data PHPClassName = PHPClassNameStatic
                  | PHPClassName PHPQualifiedIdentifier

data PHPIsReference = PHPIsReference Bool

data PHPQualifiedIdentifier = PHPQualifiedIdentifierRelative PHPIdent [PHPIdent]
                            | PHPQualifiedIdentifierSelf     PHPIdent [PHPIdent]
                            | PHPQualifiedIdentifierAbsolute PHPIdent [PHPIdent]

data PHPStaticArrayPair = PHPStaticArrayPairKV PHPScalar PHPScalar
                        | PHPStaticArrayPairV  PHPScalar
                           
namespaceRelative :: [PHPIdent] -> PHPQualifiedIdentifier
namespaceRelative (i:ns) = PHPQualifiedIdentifierRelative i ns
namespaceSelf :: [PHPIdent] -> PHPQualifiedIdentifier
namespaceSelf (i:ns)     = PHPQualifiedIdentifierSelf     i ns
namespaceAbsolute :: [PHPIdent] -> PHPQualifiedIdentifier
namespaceAbsolute (i:ns) = PHPQualifiedIdentifierAbsolute i ns

 