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
        PHPVariable (..),
        PHPArrayPair (..),

        PHPIntegerToken (..),
        PHPRealToken (..),
        PHPStringToken (..),                                         
        PHPVariableToken (..),
        PHPIdent (..),

        ZZ_BVWFC (..),
        ZZ_BV (..),
        ZZ_OP (..),
        ZZ_MON (..),
        ZZ_VP (..),
        ZZ_AFD (..),
        ZZ_FC (..),
        ZZ_RV (..),
        ZZ_SIR (..),
        ZZ_SM (..),
        ZZ_ODL (..),
        ZZ_VWO (..),
        ZZ_AMD (..),
        ZZ_VP' (..),
        ZZ_DO (..),
        ZZ_CN (..),
        ZZ_VN (..),
        ZZ_VCN (..),
        ZZ_CV (..),
        ZZ_CNR (..),
        ZZ_DCNR (..),
        ZZ_DCNVP (..),
        ZZ_DCNVP' (..),
        ZZ_CMOP (..),
        ZZ_CD (..),
        ZZ_CIC (..),
        ZZ_IC (..),
        ZZ_CSO (..),
        
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
                  | PHPIf [(PHPExpr,PHPStatement)] (Maybe PHPStatement)
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
                  | PHPInline String
                  | PHPExprStmt PHPExpr
                  | PHPUnsetStmt [PHPVariable]
                  | PHPForeach PHPExpr PHPForeachArg (Maybe PHPForeachArg) PHPStatement
                  | PHPDeclare [(PHPIdent,PHPScalar)] PHPStatement
                  | PHPTry [PHPStatement] [PHPCatch] [PHPStatement]
                  | PHPThrow PHPExpr
                  | PHPGoto PHPIdent
                  | PHPFunctionDeclaration PHPIdent Bool [PHPFormalParameter] [PHPStatement]
                  | PHPClassDeclaration PHPIdent PHPClassType (Maybe PHPQualifiedIdentifier) [PHPQualifiedIdentifier] [PHPClassStatement]
                  | PHPInterfaceDeclaration PHPIdent PHPInterfaceType [PHPQualifiedIdentifier] [PHPClassStatement]
                  | PHPEmptyStatement
  deriving (Show, Eq)
                                      
data PHPClassType = PHPClassStandard 
                  | PHPClassAbstract 
                  | PHPClassTrait 
                  | PHPClassFinal
  deriving (Show, Eq)

data PHPInterfaceType = PHPInterfaceStandard 
  deriving (Show, Eq)

data PHPClassStatement = PHPClassVariableDeclaration [PHPMemberModifier] [(PHPVariableToken,Maybe PHPScalar)]
                       | PHPClassConstantDeclaration [(PHPIdent,PHPScalar)]
                       | PHPTraitUseStatement [PHPQualifiedIdentifier] [PHPTraitAdaptationStatement]
                       | PHPMethodDeclaration PHPIdent Bool [PHPMemberModifier] [PHPFormalParameter] (Maybe [PHPStatement])               
  deriving (Show, Eq)

data PHPMemberModifier = PHPMemberPublic 
                       | PHPMemberProtected 
                       | PHPMemberPrivate 
                       | PHPMemberStatic 
                       | PHPMemberAbstract 
                       | PHPMemberFinal 
  deriving (Show, Eq)

data PHPTraitAdaptationStatement = PHPTraitPrecedence PHPTraitMethodIdentifier [PHPQualifiedIdentifier]
                                 | PHPTraitAlias PHPTraitMethodIdentifier (Maybe PHPMemberModifier) (Maybe PHPIdent)
  deriving (Show, Eq)
                                 
data PHPTraitMethodIdentifier = PHPTraitMethodIdentifier PHPIdent (Maybe PHPQualifiedIdentifier)
  deriving (Show, Eq)
                  
data PHPScalar = PHPConstant PHPQualifiedIdentifier
               | PHPStaticUnaryPlus PHPScalar
               | PHPStaticUnaryMinus PHPScalar
               | PHPStaticArray [PHPStaticArrayPair]
               | PHPMagicClass
               | PHPIntegerConstant PHPIntegerToken
               | PHPRealConstant PHPRealToken
               | PHPStringConstant PHPStringToken
               | PHPMagicLine
               | PHPMagicFile
               | PHPMagicDir
               | PHPMagicTrait
               | PHPMagicMethod
               | PHPMagicFunction
               | PHPMagicNamespace
               | PHPStaticClassConstant ZZ_CN PHPIdent
               | PHPScalarVariable PHPVariableToken
               | PHPScalarString [PHPStringValue]
               | PHPClassConstant ZZ_CN PHPIdent
               | PHPVariableClassConstant ZZ_VCN PHPIdent
               | PHPScalarClassName ZZ_CN
               | PHPScalarWithOffset ZZ_CSO
               | PHPArray [PHPArrayPair]
  deriving (Show, Eq)
               
data ZZ_CSO = ZZ_CSO_A [PHPArrayPair] ZZ_DO
            | ZZ_CSO_B ZZ_CSO ZZ_DO
            | ZZ_CSO_C PHPStringToken ZZ_DO              
  deriving (Show, Eq)
               
              
data PHPExpr = PHPListAssignment [PHPALE] PHPExpr
             | PHPAssignment PHPVariable PHPExpr
             | PHPRefAssignment PHPVariable PHPVariable
             | PHPRefAssignmentFromNew PHPVariable ZZ_CNR [PHPActualParameter]
             | PHPClone PHPExpr
             | PHPAddInto PHPVariable PHPExpr
             | PHPSubtractInto PHPVariable PHPExpr
             | PHPMultiplyInto PHPVariable PHPExpr
             | PHPDivideInto PHPVariable PHPExpr
             | PHPConcatInto PHPVariable PHPExpr
             | PHPModulusInto PHPVariable PHPExpr
             | PHPAndInto PHPVariable PHPExpr
             | PHPOrInto PHPVariable PHPExpr
             | PHPXorInto PHPVariable PHPExpr
             | PHPShiftLeftInto PHPVariable PHPExpr
             | PHPShiftRightInto PHPVariable PHPExpr
             | PHPPostIncrement PHPVariable 
             | PHPPreIncrement PHPVariable 
             | PHPPostDecrement PHPVariable 
             | PHPPreDecrement PHPVariable 
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
             | PHPInstanceOf PHPExpr ZZ_CNR
             | PHPMethodCallFromNew PHPExpr ZZ_IC
             | PHPTernaryOp PHPExpr (Maybe PHPExpr) PHPExpr
             | PHPIntCast PHPExpr
             | PHPDoubleCast PHPExpr
             | PHPStringCast PHPExpr
             | PHPArrayCast PHPExpr
             | PHPObjectCast PHPExpr
             | PHPBoolCast PHPExpr
             | PHPUnsetCast PHPExpr
             | PHPExit (Maybe PHPExpr)
             | PHPDisableErrors PHPExpr
             | PHPScalarExpr PHPScalar
             | PHPBacktick [PHPStringValue]
             | PHPYield0
             | PHPYield1 PHPExpr
             | PHPYield2 PHPExpr PHPExpr
             | PHPPrint PHPExpr
             | PHPAnonymousFunction Bool [PHPFormalParameter] [PHPLexicalVariable] [PHPStatement]
             | PHPAnonymousStaticFunction Bool [PHPFormalParameter] [PHPLexicalVariable] [PHPStatement]
             | PHPArrayReference PHPExpr (Maybe PHPExpr)
             | PHPClassStaticMember PHPQualifiedIdentifier PHPExpr
             | PHPIndirectClassStaticMember PHPExpr PHPExpr
             | PHPVariableInExpr PHPVariable
             | PHPIsSet [PHPExpr]
             | PHPEmpty PHPExpr
             | PHPInclude PHPExpr
             | PHPIncludeOnce PHPExpr
             | PHPEval PHPExpr
             | PHPRequire PHPExpr
             | PHPRequireOnce PHPExpr
             | PHPInstanceCallFromNew PHPExpr ZZ_IC
             | PHPNewExpr ZZ_CNR [PHPActualParameter]
  deriving (Show, Eq)

             

data PHPVariable = ZZ_V_A ZZ_BVWFC ZZ_OP ZZ_MON ZZ_VP
                 | ZZ_V_B ZZ_BVWFC
  deriving (Show, Eq)
                 
data ZZ_BVWFC = ZZ_BVWFC_A ZZ_BV
              | ZZ_BVWFC_B ZZ_AFD
              | ZZ_BVWFC_C ZZ_FC    
  deriving (Show, Eq)
              
data ZZ_BV = ZZ_BV_A ZZ_RV
           | ZZ_BV_B ZZ_SIR ZZ_RV
           | ZZ_BV_C ZZ_SM
  deriving (Show, Eq)
           
data ZZ_OP = ZZ_OP_A ZZ_ODL
           | ZZ_OP_B ZZ_VWO
  deriving (Show, Eq)
           
data ZZ_MON = ZZ_MON_A [PHPActualParameter]
            | ZZ_MON_B ZZ_AMD
            | ZZ_MON_C
  deriving (Show, Eq)

data ZZ_VP = ZZ_VP_A ZZ_VP ZZ_VP'
           | ZZ_VP_B 
  deriving (Show, Eq)
           
data ZZ_AFD = ZZ_AFD_A ZZ_AFD ZZ_DO
            | ZZ_AFD_B ZZ_FC ZZ_DO
  deriving (Show, Eq)

data ZZ_FC = ZZ_FC_A PHPQualifiedIdentifier [PHPActualParameter]
           | ZZ_FC_B ZZ_CN ZZ_VN [PHPActualParameter]
           | ZZ_FC_C ZZ_CN ZZ_VWO [PHPActualParameter]
           | ZZ_FC_D ZZ_VCN ZZ_VN [PHPActualParameter]
           | ZZ_FC_E ZZ_VCN ZZ_VWO [PHPActualParameter]
           | ZZ_FC_F ZZ_VWO [PHPActualParameter]
  deriving (Show, Eq)

data ZZ_RV = ZZ_RV_A ZZ_RV ZZ_DO
           | ZZ_RV_B ZZ_RV PHPExpr
           | ZZ_RV_C ZZ_CV
  deriving (Show, Eq)
           
data ZZ_SIR = ZZ_SIR_A
            | ZZ_SIR_B ZZ_SIR
  deriving (Show, Eq)

data ZZ_SM = ZZ_SM_A ZZ_CN ZZ_VWO
           | ZZ_SM_B ZZ_VCN ZZ_VWO
  deriving (Show, Eq)

data ZZ_ODL = ZZ_ODL_A ZZ_ODL ZZ_DO
            | ZZ_ODL_B ZZ_ODL PHPExpr
            | ZZ_ODL_C ZZ_VN
  deriving (Show, Eq)

data ZZ_VWO = ZZ_VWO_A ZZ_RV
            | ZZ_VWO_B ZZ_SIR ZZ_RV
  deriving (Show, Eq)
            
data ZZ_AMD = ZZ_AMD_A ZZ_AMD ZZ_DO
            | ZZ_AMD_B [PHPActualParameter] ZZ_DO
  deriving (Show, Eq)
                                                           
data ZZ_VP' = ZZ_VP'_A ZZ_OP ZZ_MON
  deriving (Show, Eq)
                                                                                     
data ZZ_DO = ZZ_DO_A 
           | ZZ_DO_B PHPExpr
  deriving (Show, Eq)

data ZZ_CN = ZZ_CN_A
           | ZZ_CN_B PHPQualifiedIdentifier
  deriving (Show, Eq)

data ZZ_VN = ZZ_VN_A PHPIdent
           | ZZ_VN_B PHPExpr         
  deriving (Show, Eq)
           
data ZZ_VCN = ZZ_VCN_A ZZ_RV          
  deriving (Show, Eq)
           
data ZZ_CV = ZZ_CV_A PHPVariableToken
           | ZZ_CV_B PHPExpr
  deriving (Show, Eq)
           
data ZZ_CNR = ZZ_CNR_A ZZ_CN
            | ZZ_CNR_B ZZ_DCNR
  deriving (Show, Eq)
            
data ZZ_DCNR = ZZ_DCNR_A ZZ_BV ZZ_OP ZZ_DCNVP
             | ZZ_DCNR_B ZZ_BV                     
  deriving (Show, Eq)

data ZZ_DCNVP = ZZ_DCNVP_A ZZ_DCNVP ZZ_DCNVP'
              | ZZ_DCNVP_B
  deriving (Show, Eq)
              
data ZZ_DCNVP' = ZZ_DCNVP'_A ZZ_OP
  deriving (Show, Eq)

data ZZ_CMOP = ZZ_CMOP_A ZZ_CMOP ZZ_VP'
             | ZZ_CMOP_B ZZ_VP'
  deriving (Show, Eq)
             
data ZZ_CD = ZZ_CD_A ZZ_CD ZZ_DO
           | ZZ_CD_B ZZ_DO
  deriving (Show, Eq)
           
data ZZ_CIC = ZZ_CIC_A ZZ_CD ZZ_CMOP
            | ZZ_CIC_B ZZ_CD                                     
            | ZZ_CIC_C ZZ_CMOP
  deriving (Show, Eq)
            
data ZZ_IC = ZZ_IC_A
           | ZZ_IC_B ZZ_CIC
  deriving (Show, Eq)
           
data PHPActualParameter = PHPActualParameter PHPExpr
                        | PHPActualRefParameter PHPVariable             
  deriving (Show, Eq)
             
data PHPLexicalVariable = PHPLexicalVariable PHPVariableToken
                        | PHPLexicalVariableRef PHPVariableToken              
  deriving (Show, Eq)

data PHPFormalParameter = PHPFormalParameter PHPVariableToken Bool (Maybe PHPParameterType) (Maybe PHPScalar)
  deriving (Show, Eq)

data PHPParameterType = PHPTypeArray 
                      | PHPTypeCallable
                      | PHPTypeClass PHPQualifiedIdentifier                    
  deriving (Show, Eq)

data PHPStringValue = PHPString String
                    | PHPVariableString PHPVariableToken
                    | PHPVariableOffsetString PHPVariableToken PHPVariableOffset
                    | PHPVariablePropertyString PHPVariableToken PHPIdent
                    | PHPVariableExprOffsetString PHPVariableToken PHPExpr
                    | PHPExprString PHPExpr
  deriving (Show, Eq)

data PHPVariableOffset = PHPVOIdent PHPIdent
                       | PHPVONumber PHPIntegerToken
                       | PHPVOVariable PHPVariableToken
  deriving (Show, Eq)
             
data PHPSwitchCase = PHPSwitchCase PHPExpr [PHPStatement]
                   | PHPSwitchDefault [PHPStatement]
  deriving (Show, Eq)

data PHPClassNameScalar = PHPClassNameScalar PHPClassName
  deriving (Show, Eq)
        
data PHPClassName = PHPClassNameStatic
                  | PHPClassName PHPQualifiedIdentifier
  deriving (Show, Eq)

data PHPIsReference = PHPIsReference Bool
  deriving (Show, Eq)

data PHPQualifiedIdentifier = PHPQualifiedIdentifierRelative PHPIdent [PHPIdent]
                            | PHPQualifiedIdentifierSelf     PHPIdent [PHPIdent]
                            | PHPQualifiedIdentifierAbsolute PHPIdent [PHPIdent]
  deriving (Show, Eq)

data PHPStaticArrayPair = PHPStaticArrayPairKV PHPScalar PHPScalar
                        | PHPStaticArrayPairV  PHPScalar
  deriving (Show, Eq)
                        
data PHPGlobalVarSpec = PHPGlobalVar PHPVariableToken
                      | PHPIndirectGlobalVar PHPExpr
  deriving (Show, Eq)
                      
data PHPForeachArg = PHPForeachVar PHPVariable
                   | PHPForeachRef PHPVariable
                   | PHPForeachList [PHPALE]
  deriving (Show, Eq)
                   
data PHPALE = PHPALEVariable PHPVariable
            | PHPALEList [PHPALE]
            | PHPALEEmpty                   
  deriving (Show, Eq)

data PHPCatch = PHPCatch PHPQualifiedIdentifier PHPVariableToken [PHPStatement]
  deriving (Show, Eq)

data PHPArrayPair = PHPArrayPairKV PHPExpr PHPExpr
                  | PHPArrayPairKR PHPExpr PHPVariable
                  | PHPArrayPairV PHPExpr
                  | PHPArrayPairR PHPVariable
  deriving (Show, Eq)

newtype PHPIdent = PHPIdent String
  deriving (Show, Eq)
newtype PHPVariableToken = PHPVariableToken String
  deriving (Show, Eq)
newtype PHPIntegerToken = PHPIntegerToken String
  deriving (Show, Eq)
newtype PHPRealToken = PHPRealToken String
  deriving (Show, Eq)
newtype PHPStringToken = PHPStringToken String
  deriving (Show, Eq)
                                                 
namespaceRelative :: [PHPIdent] -> PHPQualifiedIdentifier
namespaceRelative (i:ns) = PHPQualifiedIdentifierRelative i ns
namespaceSelf :: [PHPIdent] -> PHPQualifiedIdentifier
namespaceSelf (i:ns)     = PHPQualifiedIdentifierSelf     i ns
namespaceAbsolute :: [PHPIdent] -> PHPQualifiedIdentifier
namespaceAbsolute (i:ns) = PHPQualifiedIdentifierAbsolute i ns


