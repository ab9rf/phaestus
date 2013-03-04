module ParseTree (
        PHPClassNameScalar (..),
        PHPClassName (..),
        PHPIsReference (..),
        PHPQualifiedIdentifier (..),
        PHPStatement (..),
        PHPScalar (..),
        PHPStaticArrayPair (..),
        PHPExpr (..),
        PHPSwitchCase (..),
        PHPALE (..),
        PHPForeachArg (..),
        PHPExpr (..),
        PHPGlobalVarSpec (..),
        PHPLexicalVariable (..),
        PHPFormalParameter (..),
        PHPClassType (..),
        PHPStringValue (..),
        PHPVariableOffset (..),
        PHPCatch (..),
        PHPInterfaceType (..),
        PHPClassStatement (..),
        PHPActualParameter (..),
        PHPParameterType (..),
        PHPMemberModifier (..),
        PHPTraitAdaptationStatement (..),
        PHPTraitMethodIdentifier (..),
                                         
        PHPVariableToken (..),
        PHPIdent (..),
        
        namespaceRelative,
        namespaceSelf,
        namespaceAbsolute
                
) where

data PHPStatement = PHPChangeNamespace [PHPIdent]
                  | PHPNamespace [PHPIdent] [PHPStatement]
                  | PHPUseDeclaration [(PHPQualifiedIdentifier,PHPIdent)] 
                  | PHPConstantDeclaration [(PHPIdent,PHPScalar)]
                  | PHPLabelDecl PHPIdent
                  | PHPStatementGroup [PHPStatement]
                  | PHPIf [(PHPExpr,PHPStatement)] PHPStatement
                  | PHPWhile PHPExpr PHPStatement
                  | PHPDo PHPStatement PHPExpr
                  | PHPFor [PHPExpr] [PHPExpr] [PHPExpr] PHPStatement
                  | PHPSwitch PHPExpr [PHPSwitchCase]
                  | PHPBreak (Maybe PHPExpr)
                  | PHPContinue (Maybe PHPExpr)
                  | PHPReturn (Maybe PHPExpr)
                  | PHPYieldStmt PHPExpr
                  | PHPGlobalStmt [PHPGlobalVarSpec]
                  | PHPStaticStmt [(PHPVariableToken, Maybe PHPScalar)]
                  | PHPEcho [PHPExpr]
                  | PHPInline [String]
                  | PHPExprStrt [PHPExpr]
                  | PHPUnsetStmt [PHPExpr]
                  | PHPForeach PHPExpr PHPForeachArg (Maybe PHPForeachArg) PHPStatement
                  | PHPDeclare [(PHPIdent,PHPScalar)]
                  | PHPTry [PHPStatement] [PHPCatch] [PHPStatement]
                  | PHPThrow PHPExpr
                  | PHPGoto PHPIdent
                  | PHPFunctionDeclaration PHPIdent Bool [PHPFormalParameter] [PHPStatement]
                  | PHPClassDeclaration PHPIdent PHPClassType (Maybe PHPQualifiedIdentifier) [PHPQualifiedIdentifier] [PHPClassStatement]
                  | PHPInterfaceDeclaration PHPIdent PHPInterfaceType [PHPQualifiedIdentifier] [PHPClassStatement]
                  
data PHPClassType = PHPClassStandard 
                  | PHPClassAbstract 
                  | PHPClassTrait 
                  | PHPClassFinal

data PHPInterfaceType = PHPInterfaceStandard 

data PHPClassStatement = PHPClassVariableDeclaration [PHPMemberModifier] [(PHPVariableToken,Maybe PHPScalar)]
                       | PHPClassConstantDeclaration [(PHPIdent,PHPScalar)]
                       | PHPTraitUseStatement [PHPQualifiedIdentifier] [PHPTraitAdaptationStatement]
                       | PHPMethodDeclaration PHPIdent [PHPMemberModifier] Bool [PHPFormalParameter] (Maybe [PHPStatement])               

data PHPMemberModifier = PHPMemberPublic 
                       | PHPMemberProtected 
                       | PHPMemberPrivate 
                       | PHPMemberStatic 
                       | PHPMemberAbstract 
                       | PHPMemberTrait

data PHPTraitAdaptationStatement = PHPTraitPrecedence PHPTraitMethodIdentifier [PHPQualifiedIdentifier]
                                 | PHPTraitAlias PHPTraitMethodIdentifier (Maybe PHPMemberModifier) (Maybe PHPIdent)
                                 
data PHPTraitMethodIdentifier = PHPTraitMethodIdentifier PHPIdent (Maybe PHPQualifiedIdentifier)
                  
data PHPScalar = PHPConstant PHPQualifiedIdentifier
               | PHPStaticUnaryPlus PHPScalar
               | PHPStaticUnaryMinus PHPScalar
               | PHPStaticArray [PHPStaticArrayPair]
               | PHPMagicClass 
               
data PHPExpr = PHPListAssignment [PHPALE] PHPExpr
             | PHPAssignment PHPExpr PHPExpr
             | PHPRefAssignment PHPExpr PHPExpr
             | PHPRefAssignmentFromNew PHPClassName [PHPActualParameter]
             | PHPClone PHPExpr
             | PHPAddInto PHPExpr PHPExpr
             | PHPSubtractInto PHPExpr PHPExpr
             | PHPMultiplyInto PHPExpr PHPExpr
             | PHPDivideInto PHPExpr PHPExpr
             | PHPConcatInto PHPExpr PHPExpr
             | PHPModulusInto PHPExpr PHPExpr
             | PHPAndInto PHPExpr PHPExpr
             | PHPOrInto PHPExpr PHPExpr
             | PHPXorInto PHPExpr PHPExpr
             | PHPShiftLeftInto PHPExpr PHPExpr
             | PHPShiftRightInto PHPExpr PHPExpr
             | PHPPostIncrement PHPExpr
             | PHPPreIncrement PHPExpr
             | PHPPostDecrement PHPExpr
             | PHPPreDecrement PHPExpr
             | PHPBooleanOr PHPExpr PHPExpr
             | PHPBooleanAnd PHPExpr PHPExpr
             | PHPLogicalOr PHPExpr PHPExpr
             | PHPLogicalAnd PHPExpr PHPExpr
             | PHPLogicalXor PHPExpr PHPExpr
             | PHPBinaryOr PHPExpr PHPExpr
             | PHPBinaryAnd PHPExpr PHPExpr
             | PHPBinaryXor PHPExpr PHPExpr
             | PHPConcat PHPExpr PHPExpr
             | PHPAdd PHPExpr PHPExpr
             | PHPSubtract PHPExpr PHPExpr
             | PHPMultiply PHPExpr PHPExpr
             | PHPDivide PHPExpr PHPExpr
             | PHPModulus PHPExpr PHPExpr
             | PHPShiftLeft PHPExpr PHPExpr
             | PHPShiftRight PHPExpr PHPExpr
             | PHPUnaryPlus PHPExpr
             | PHPUnaryMinus PHPExpr
             | PHPLogicalNot PHPExpr
             | PHPBinaryNegation PHPExpr
             | PHPIsIdentical PHPExpr PHPExpr
             | PHPIsNotIdentical PHPExpr PHPExpr
             | PHPIsEqual PHPExpr PHPExpr
             | PHPIsNotEqual PHPExpr PHPExpr
             | PHPLessThan PHPExpr PHPExpr
             | PHPLessThanOrEqual PHPExpr PHPExpr
             | PHPGreaterThan PHPExpr PHPExpr
             | PHPGreaterThanOrEqual PHPExpr PHPExpr
             | PHPInstanceOf PHPExpr PHPClassName
             | PHPMethodCall PHPExpr PHPMethodName
             | PHPTernaryOp PHPExpr (Maybe PHPExpr) PHPExpr
             | PHPIntCast PHPExpr
             | PHPDoubleCast PHPExpr
             | PHPStringCast PHPExpr
             | PHPArrayCast PHPExpr
             | PHPObjectCast PHPExpr
             | PHPBoolCast PHPExpr
             | PHPUnsetCast PHPExpr
             | PHPExit PHPExpr
             | PHPDisableErrors PHPExpr
             | PHPScalarExpr PHPScalar
             | PHPBacktick [PHPStringValue]
             | PHPYield0
             | PHPAnonymousFunction Bool [PHPFormalParameter] [PHPLexicalVariable] [PHPStatement]
             | PHPVariable PHPVariableToken
             | PHPIndirection PHPExpr Integer
             | PHPArrayReference PHPExpr (Maybe PHPExpr)
             | PHPIndirectExpr PHPExpr
             | PHPClassStaticMember PHPQualifiedIdentifier PHPExpr
             | PHPIndirectClassStaticMember PHPExpr PHPExpr

data PHPActualParameter = PHPActualParameter PHPExpr
                        | PHPActualRefParameter PHPExpr             
             
data PHPLexicalVariable = PHPLexicalVariable PHPVariableToken
                        | PHPLexicalVariableRef PHPVariableToken              

data PHPMethodName = FIXME3

data PHPFormalParameter = PHPFormalParameter PHPVariableToken Bool (Maybe PHPParameterType) (Maybe PHPScalar)

data PHPParameterType = PHPTypeArray 
                      | PHPTypeCallable
                      | PHPTypeClass PHPQualifiedIdentifier                    

data PHPStringValue = PHPString String
                    | PHPVariableString PHPVariableToken
                    | PHPVariableOffsetString PHPVariableToken PHPVariableOffset
                    | PHPVariablePropertyString PHPVariableToken PHPIdent
                    | PHPExprString PHPExpr

data PHPVariableOffset = PHPVOIdent PHPIdent
                       | PHPVONumber PHPNumber
                       | PHPVOVariable PHPVariableToken
             
data PHPNumber = FIXME1             

data PHPSwitchCase = PHPSwitchCase PHPExpr [PHPStatement]
                   | PHPSwitchDefault [PHPStatement]

newtype PHPIdent = PHPIdent String

newtype PHPVariableToken = PHPVariableToken String

data PHPClassNameScalar = PHPClassNameScalar PHPClassName
        
data PHPClassName = PHPClassNameStatic
                  | PHPClassName PHPQualifiedIdentifier

data PHPIsReference = PHPIsReference Bool

data PHPQualifiedIdentifier = PHPQualifiedIdentifierRelative PHPIdent [PHPIdent]
                            | PHPQualifiedIdentifierSelf     PHPIdent [PHPIdent]
                            | PHPQualifiedIdentifierAbsolute PHPIdent [PHPIdent]

data PHPStaticArrayPair = PHPStaticArrayPairKV PHPScalar PHPScalar
                        | PHPStaticArrayPairV  PHPScalar
                        
data PHPGlobalVarSpec = PHPGlobalVar PHPVariableToken
                      | PHPIndirectGlobalVar PHPExpr
                      
data PHPForeachArg = PHPForeachVar PHPExpr
                   | PHPForeachRef PHPExpr
                   | PHPForeachList [PHPALE]
                   
data PHPALE = PHPALEVariable PHPExpr
            | PHPALEList [PHPALE]
            | PHPALEEmpty                   

data PHPCatch = PHPCatch PHPQualifiedIdentifier PHPExpr [PHPStatement]



data MethodOrNot = Method [PHPExpr]
                 | ArrayRef (Maybe PHPExpr) MethodOrNot
                 
                                                 
namespaceRelative :: [PHPIdent] -> PHPQualifiedIdentifier
namespaceRelative (i:ns) = PHPQualifiedIdentifierRelative i ns
namespaceSelf :: [PHPIdent] -> PHPQualifiedIdentifier
namespaceSelf (i:ns)     = PHPQualifiedIdentifierSelf     i ns
namespaceAbsolute :: [PHPIdent] -> PHPQualifiedIdentifier
namespaceAbsolute (i:ns) = PHPQualifiedIdentifierAbsolute i ns
