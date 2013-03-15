module ParseTree (
                  PTAdditionalCatch (..), 
                  PTAdditionalCatches (..), 
                  PTArrayFunctionDereference (..), 
                  PTArrayMethodDereference (..), 
                  PTArrayPairList (..), 
                  PTAssignmentList (..), 
                  PTAssignmentListElement (..), 
                  PTBackticksExpr (..), 
                  PTBaseVariable (..), 
                  PTBaseVariableWithFunctionCalls (..), 
                  PTCaseList (..), 
                  PTCaseSeparator (..), 
                  PTCatchStatement (..), 
                  PTChainingDereference (..), 
                  PTChainingInstanceCall (..), 
                  PTChainingMethodOrProperty (..), 
                  PTClassConstant (..), 
                  PTClassConstantDeclaration (..), 
                  PTClassDeclarationStatement (..), 
                  PTClassEntryType (..), 
                  PTClassName (..), 
                  PTClassNameReference (..), 
                  PTClassNameScalar (..), 
                  PTClassStatement (..), 
                  PTClassStatementList (..), 
                  PTClassVariableDeclaration (..), 
                  PTCombinedScalar (..), 
                  PTCombinedScalarOffset (..), 
                  PTCommonScalar (..), 
                  PTCompoundVariable (..), 
                  PTConstantDeclaration (..), 
                  PTCtorArguments (..), 
                  PTDeclareList (..), 
                  PTDeclareStatement (..), 
                  PTDimOffset (..), 
                  PTDynamicClassNameReference (..), 
                  PTDynamicClassNameVariableProperties (..), 
                  PTDynamicClassNameVariableProperty (..), 
                  PTEchoExprList (..), 
                  PTElseSingle (..), 
                  PTElseifList (..), 
                  PTEncapsList (..), 
                  PTEncapsVar (..), 
                  PTEncapsVarOffset (..), 
                  PTExitExpr (..), 
                  PTExpr (..), 
                  PTExprWithoutVariable (..), 
                  PTExtendsFrom (..), 
                  PTFinallyStatement (..), 
                  PTForExpr (..), 
                  PTForStatement (..), 
                  PTForeachOptionalArg (..), 
                  PTForeachStatement (..), 
                  PTForeachVariable (..), 
                  PTFullyQualifiedClassName (..), 
                  PTFunction (..), 
                  PTFunctionCall (..), 
                  PTFunctionCallParameterList (..), 
                  PTFunctionDeclarationStatement (..), 
                  PTGlobalVar (..), 
                  PTGlobalVarList (..), 
                  PTImplementsList (..), 
                  PTInnerStatement (..), 
                  PTInnerStatementList (..), 
                  PTInstanceCall (..), 
                  PTInterfaceEntry (..), 
                  PTInterfaceExtendsList (..), 
                  PTInterfaceList (..), 
                  PTInternalFunctionsInYacc (..), 
                  PTIsReference (..), 
                  PTIssetVariable (..), 
                  PTIssetVariables (..), 
                  PTLexicalVarList (..), 
                  PTLexicalVars (..), 
                  PTMemberModifier (..), 
                  PTMethod (..), 
                  PTMethodBody (..), 
                  PTMethodModifiers (..), 
                  PTMethodOrNot (..), 
                  PTNamespaceName (..), 
                  PTNewElseSingle (..), 
                  PTNewElseifList (..), 
                  PTNewExpr (..), 
                  PTNonEmptyAdditionalCatches (..), 
                  PTNonEmptyArrayPairList (..), 
                  PTNonEmptyForExpr (..), 
                  PTNonEmptyFunctionCallParameterList (..), 
                  PTNonEmptyMemberModifiers (..), 
                  PTNonEmptyParameterList (..), 
                  PTNonEmptyStaticArrayPairList (..), 
                  PTNonEmptyTraitAdaptationList (..), 
                  PTObjectDimList (..), 
                  PTObjectProperty (..), 
                  PTOptionalClassType (..), 
                  PTParameterList (..), 
                  PTParenthesisExpr (..), 
                  PTPossibleComma (..), 
                  PTRVariable (..), 
                  PTReferenceVariable (..), 
                  PTRwVariable (..), 
                  PTScalar (..), 
                  PTSimpleIndirectReference (..), 
                  PTStart (..), 
                  PTStatement (..), 
                  PTStaticArrayPairList (..), 
                  PTStaticClassConstant (..), 
                  PTStaticClassNameScalar (..), 
                  PTStaticMember (..), 
                  PTStaticScalar (..), 
                  PTStaticVarList (..), 
                  PTSwitchCaseList (..), 
                  PTTopStatement (..), 
                  PTTopStatementList (..), 
                  PTTraitAdaptationList (..), 
                  PTTraitAdaptationStatement (..), 
                  PTTraitAdaptations (..), 
                  PTTraitAlias (..), 
                  PTTraitList (..), 
                  PTTraitMethodReference (..), 
                  PTTraitMethodReferenceFullyQualified (..), 
                  PTTraitModifiers (..), 
                  PTTraitPrecedence (..), 
                  PTTraitReferenceList (..), 
                  PTTraitUseStatement (..), 
                  PTUnsetVariable (..), 
                  PTUnsetVariables (..), 
                  PTUntickedClassDeclarationStatement (..), 
                  PTUntickedFunctionDeclarationStatement (..), 
                  PTUntickedStatement (..), 
                  PTUseDeclaration (..), 
                  PTUseDeclarations (..), 
                  PTVariable (..), 
                  PTVariableClassName (..), 
                  PTVariableModifiers (..), 
                  PTVariableName (..), 
                  PTVariableProperties (..), 
                  PTVariableProperty (..), 
                  PTVariableWithoutObjects (..), 
                  PTWVariable (..), 
                  PTWhileStatement (..), 
                  PTYieldExpr (..), 
                  PVDouble (..), 
                  PVIdent (..), 
                  PVInline (..), 
                  PVInteger (..), 
                  PVString (..), 
                  PVVariableName (..), 
                  PVVariableNameImbed (..)
) where
data PTAdditionalCatch = PTAdditionalCatch_1 PTFullyQualifiedClassName PVVariableName PTInnerStatementList
  deriving (Show, Eq)

data PTAdditionalCatches = PTAdditionalCatches_1 PTNonEmptyAdditionalCatches
                         | PTAdditionalCatches_2 
  deriving (Show, Eq)

data PTArrayFunctionDereference = PTArrayFunctionDereference_1 PTArrayFunctionDereference PTDimOffset
                                | PTArrayFunctionDereference_2 PTFunctionCall PTDimOffset
  deriving (Show, Eq)

data PTArrayMethodDereference = PTArrayMethodDereference_1 PTArrayMethodDereference PTDimOffset
                              | PTArrayMethodDereference_2 PTMethod PTDimOffset
  deriving (Show, Eq)

data PTArrayPairList = PTArrayPairList_1 
                     | PTArrayPairList_2 PTNonEmptyArrayPairList PTPossibleComma
  deriving (Show, Eq)

data PTAssignmentList = PTAssignmentList_1 PTAssignmentList PTAssignmentListElement
                      | PTAssignmentList_2 PTAssignmentListElement
  deriving (Show, Eq)

data PTAssignmentListElement = PTAssignmentListElement_1 PTVariable
                             | PTAssignmentListElement_2 PTAssignmentList
                             | PTAssignmentListElement_3 
  deriving (Show, Eq)

data PTBackticksExpr = PTBackticksExpr_1 
                     | PTBackticksExpr_2 PVString
                     | PTBackticksExpr_3 PTEncapsList
  deriving (Show, Eq)

data PTBaseVariable = PTBaseVariable_1 PTReferenceVariable
                    | PTBaseVariable_2 PTSimpleIndirectReference PTReferenceVariable
                    | PTBaseVariable_3 PTStaticMember
  deriving (Show, Eq)

data PTBaseVariableWithFunctionCalls = PTBaseVariableWithFunctionCalls_1 PTBaseVariable
                                     | PTBaseVariableWithFunctionCalls_2 PTArrayFunctionDereference
                                     | PTBaseVariableWithFunctionCalls_3 PTFunctionCall
  deriving (Show, Eq)

data PTCaseList = PTCaseList_1 
                | PTCaseList_2 PTCaseList PTExpr PTCaseSeparator PTInnerStatementList
                | PTCaseList_3 PTCaseList PTCaseSeparator PTInnerStatementList
  deriving (Show, Eq)

data PTCaseSeparator = PTCaseSeparator_1 
                     | PTCaseSeparator_2 
  deriving (Show, Eq)

data PTCatchStatement = PTCatchStatement_1 
                      | PTCatchStatement_2 PTFullyQualifiedClassName PVVariableName PTInnerStatementList PTAdditionalCatches
  deriving (Show, Eq)

data PTChainingDereference = PTChainingDereference_1 PTChainingDereference PTDimOffset
                           | PTChainingDereference_2 PTDimOffset
  deriving (Show, Eq)

data PTChainingInstanceCall = PTChainingInstanceCall_1 PTChainingDereference PTChainingMethodOrProperty
                            | PTChainingInstanceCall_2 PTChainingDereference
                            | PTChainingInstanceCall_3 PTChainingMethodOrProperty
  deriving (Show, Eq)

data PTChainingMethodOrProperty = PTChainingMethodOrProperty_1 PTChainingMethodOrProperty PTVariableProperty
                                | PTChainingMethodOrProperty_2 PTVariableProperty
  deriving (Show, Eq)

data PTClassConstant = PTClassConstant_1 PTClassName PVIdent
                     | PTClassConstant_2 PTVariableClassName PVIdent
  deriving (Show, Eq)

data PTClassConstantDeclaration = PTClassConstantDeclaration_1 PTClassConstantDeclaration PVIdent PTStaticScalar
                                | PTClassConstantDeclaration_2 PVIdent PTStaticScalar
  deriving (Show, Eq)

data PTClassDeclarationStatement = PTClassDeclarationStatement_1 PTUntickedClassDeclarationStatement
  deriving (Show, Eq)

data PTClassEntryType = PTClassEntryType_1 
                      | PTClassEntryType_2 
                      | PTClassEntryType_3 
                      | PTClassEntryType_4 
  deriving (Show, Eq)

data PTClassName = PTClassName_1 
                 | PTClassName_2 PTNamespaceName
                 | PTClassName_3 PTNamespaceName
                 | PTClassName_4 PTNamespaceName
  deriving (Show, Eq)

data PTClassNameReference = PTClassNameReference_1 PTClassName
                          | PTClassNameReference_2 PTDynamicClassNameReference
  deriving (Show, Eq)

data PTClassNameScalar = PTClassNameScalar_1 PTClassName
  deriving (Show, Eq)

data PTClassStatement = PTClassStatement_1 PTVariableModifiers PTClassVariableDeclaration
                      | PTClassStatement_2 PTClassConstantDeclaration
                      | PTClassStatement_3 PTTraitUseStatement
                      | PTClassStatement_4 PTMethodModifiers PTFunction PTIsReference PVIdent PTParameterList PTMethodBody
  deriving (Show, Eq)

data PTClassStatementList = PTClassStatementList_1 PTClassStatementList PTClassStatement
                          | PTClassStatementList_2 
  deriving (Show, Eq)

data PTClassVariableDeclaration = PTClassVariableDeclaration_1 PTClassVariableDeclaration PVVariableName
                                | PTClassVariableDeclaration_2 PTClassVariableDeclaration PVVariableName PTStaticScalar
                                | PTClassVariableDeclaration_3 PVVariableName
                                | PTClassVariableDeclaration_4 PVVariableName PTStaticScalar
  deriving (Show, Eq)

data PTCombinedScalar = PTCombinedScalar_1 PTArrayPairList
                      | PTCombinedScalar_2 PTArrayPairList
  deriving (Show, Eq)

data PTCombinedScalarOffset = PTCombinedScalarOffset_1 PTCombinedScalar PTDimOffset
                            | PTCombinedScalarOffset_2 PTCombinedScalarOffset PTDimOffset
                            | PTCombinedScalarOffset_3 PVString PTDimOffset
  deriving (Show, Eq)

data PTCommonScalar = PTCommonScalar_1 PVInteger
                    | PTCommonScalar_10 
                    | PTCommonScalar_11 PVString
                    | PTCommonScalar_12 
                    | PTCommonScalar_2 PVDouble
                    | PTCommonScalar_3 PVString
                    | PTCommonScalar_4 
                    | PTCommonScalar_5 
                    | PTCommonScalar_6 
                    | PTCommonScalar_7 
                    | PTCommonScalar_8 
                    | PTCommonScalar_9 
  deriving (Show, Eq)

data PTCompoundVariable = PTCompoundVariable_1 PVVariableName
                        | PTCompoundVariable_2 PTExpr
  deriving (Show, Eq)

data PTConstantDeclaration = PTConstantDeclaration_1 PTConstantDeclaration PVIdent PTStaticScalar
                           | PTConstantDeclaration_2 PVIdent PTStaticScalar
  deriving (Show, Eq)

data PTCtorArguments = PTCtorArguments_1 
                     | PTCtorArguments_2 PTFunctionCallParameterList
  deriving (Show, Eq)

data PTDeclareList = PTDeclareList_1 PVIdent PTStaticScalar
                   | PTDeclareList_2 PTDeclareList PVIdent PTStaticScalar
  deriving (Show, Eq)

data PTDeclareStatement = PTDeclareStatement_1 PTStatement
                        | PTDeclareStatement_2 PTInnerStatementList
  deriving (Show, Eq)

data PTDimOffset = PTDimOffset_1 
                 | PTDimOffset_2 PTExpr
  deriving (Show, Eq)

data PTDynamicClassNameReference = PTDynamicClassNameReference_1 PTBaseVariable PTObjectProperty PTDynamicClassNameVariableProperties
                                 | PTDynamicClassNameReference_2 PTBaseVariable
  deriving (Show, Eq)

data PTDynamicClassNameVariableProperties = PTDynamicClassNameVariableProperties_1 PTDynamicClassNameVariableProperties PTDynamicClassNameVariableProperty
                                          | PTDynamicClassNameVariableProperties_2 
  deriving (Show, Eq)

data PTDynamicClassNameVariableProperty = PTDynamicClassNameVariableProperty_1 PTObjectProperty
  deriving (Show, Eq)

data PTEchoExprList = PTEchoExprList_1 PTEchoExprList PTExpr
                    | PTEchoExprList_2 PTExpr
  deriving (Show, Eq)

data PTElseSingle = PTElseSingle_1 
                  | PTElseSingle_2 PTStatement
  deriving (Show, Eq)

data PTElseifList = PTElseifList_1 
                  | PTElseifList_2 PTElseifList PTParenthesisExpr PTStatement
  deriving (Show, Eq)

data PTEncapsList = PTEncapsList_1 PTEncapsList PTEncapsVar
                  | PTEncapsList_2 PTEncapsList PVString
                  | PTEncapsList_3 PTEncapsVar
                  | PTEncapsList_4 PVString PTEncapsVar
  deriving (Show, Eq)

data PTEncapsVar = PTEncapsVar_1 PVVariableName
                 | PTEncapsVar_2 PVVariableName PTEncapsVarOffset
                 | PTEncapsVar_3 PVVariableName PVIdent
                 | PTEncapsVar_4 PTExpr
                 | PTEncapsVar_5 PVVariableNameImbed PTExpr
                 | PTEncapsVar_6 PTVariable
  deriving (Show, Eq)

data PTEncapsVarOffset = PTEncapsVarOffset_1 PVIdent
                       | PTEncapsVarOffset_2 PVInteger
                       | PTEncapsVarOffset_3 PVVariableName
  deriving (Show, Eq)

data PTExitExpr = PTExitExpr_1 
                | PTExitExpr_2 
                | PTExitExpr_3 PTParenthesisExpr
  deriving (Show, Eq)

data PTExpr = PTExpr_1 PTRVariable
            | PTExpr_2 PTExprWithoutVariable
  deriving (Show, Eq)

data PTExprWithoutVariable = PTExprWithoutVariable_1 PTAssignmentList PTExpr
                           | PTExprWithoutVariable_10 PTVariable PTExpr
                           | PTExprWithoutVariable_11 PTVariable PTExpr
                           | PTExprWithoutVariable_12 PTVariable PTExpr
                           | PTExprWithoutVariable_13 PTVariable PTExpr
                           | PTExprWithoutVariable_14 PTVariable PTExpr
                           | PTExprWithoutVariable_15 PTVariable PTExpr
                           | PTExprWithoutVariable_16 PTVariable PTExpr
                           | PTExprWithoutVariable_17 PTRwVariable
                           | PTExprWithoutVariable_18 PTRwVariable
                           | PTExprWithoutVariable_19 PTRwVariable
                           | PTExprWithoutVariable_2 PTVariable PTExpr
                           | PTExprWithoutVariable_20 PTRwVariable
                           | PTExprWithoutVariable_21 PTExpr PTExpr
                           | PTExprWithoutVariable_22 PTExpr PTExpr
                           | PTExprWithoutVariable_23 PTExpr PTExpr
                           | PTExprWithoutVariable_24 PTExpr PTExpr
                           | PTExprWithoutVariable_25 PTExpr PTExpr
                           | PTExprWithoutVariable_26 PTExpr PTExpr
                           | PTExprWithoutVariable_27 PTExpr PTExpr
                           | PTExprWithoutVariable_28 PTExpr PTExpr
                           | PTExprWithoutVariable_29 PTExpr PTExpr
                           | PTExprWithoutVariable_3 PTVariable PTVariable
                           | PTExprWithoutVariable_30 PTExpr PTExpr
                           | PTExprWithoutVariable_31 PTExpr PTExpr
                           | PTExprWithoutVariable_32 PTExpr PTExpr
                           | PTExprWithoutVariable_33 PTExpr PTExpr
                           | PTExprWithoutVariable_34 PTExpr PTExpr
                           | PTExprWithoutVariable_35 PTExpr PTExpr
                           | PTExprWithoutVariable_36 PTExpr PTExpr
                           | PTExprWithoutVariable_37 PTExpr
                           | PTExprWithoutVariable_38 PTExpr
                           | PTExprWithoutVariable_39 PTExpr
                           | PTExprWithoutVariable_4 PTVariable PTClassNameReference PTCtorArguments
                           | PTExprWithoutVariable_40 PTExpr
                           | PTExprWithoutVariable_41 PTExpr PTExpr
                           | PTExprWithoutVariable_42 PTExpr PTExpr
                           | PTExprWithoutVariable_43 PTExpr PTExpr
                           | PTExprWithoutVariable_44 PTExpr PTExpr
                           | PTExprWithoutVariable_45 PTExpr PTExpr
                           | PTExprWithoutVariable_46 PTExpr PTExpr
                           | PTExprWithoutVariable_47 PTExpr PTExpr
                           | PTExprWithoutVariable_48 PTExpr PTExpr
                           | PTExprWithoutVariable_49 PTExpr PTClassNameReference
                           | PTExprWithoutVariable_5 PTExpr
                           | PTExprWithoutVariable_50 PTParenthesisExpr
                           | PTExprWithoutVariable_51 PTNewExpr
                           | PTExprWithoutVariable_52 PTNewExpr PTInstanceCall
                           | PTExprWithoutVariable_53 PTExpr PTExpr PTExpr
                           | PTExprWithoutVariable_54 PTExpr PTExpr
                           | PTExprWithoutVariable_55 PTInternalFunctionsInYacc
                           | PTExprWithoutVariable_56 PTExpr
                           | PTExprWithoutVariable_57 PTExpr
                           | PTExprWithoutVariable_58 PTExpr
                           | PTExprWithoutVariable_59 PTExpr
                           | PTExprWithoutVariable_6 PTVariable PTExpr
                           | PTExprWithoutVariable_60 PTExpr
                           | PTExprWithoutVariable_61 PTExpr
                           | PTExprWithoutVariable_62 PTExpr
                           | PTExprWithoutVariable_63 PTExitExpr
                           | PTExprWithoutVariable_64 PTExpr
                           | PTExprWithoutVariable_65 PTScalar
                           | PTExprWithoutVariable_66 PTCombinedScalarOffset
                           | PTExprWithoutVariable_67 PTCombinedScalar
                           | PTExprWithoutVariable_68 PTBackticksExpr
                           | PTExprWithoutVariable_69 PTExpr
                           | PTExprWithoutVariable_7 PTVariable PTExpr
                           | PTExprWithoutVariable_70 
                           | PTExprWithoutVariable_71 PTFunction PTIsReference PTParameterList PTLexicalVars PTInnerStatementList
                           | PTExprWithoutVariable_72 PTFunction PTIsReference PTParameterList PTLexicalVars PTInnerStatementList
                           | PTExprWithoutVariable_8 PTVariable PTExpr
                           | PTExprWithoutVariable_9 PTVariable PTExpr
  deriving (Show, Eq)

data PTExtendsFrom = PTExtendsFrom_1 
                   | PTExtendsFrom_2 PTFullyQualifiedClassName
  deriving (Show, Eq)

data PTFinallyStatement = PTFinallyStatement_1 
                        | PTFinallyStatement_2 PTInnerStatementList
  deriving (Show, Eq)

data PTForExpr = PTForExpr_1 
               | PTForExpr_2 PTNonEmptyForExpr
  deriving (Show, Eq)

data PTForStatement = PTForStatement_1 PTStatement
                    | PTForStatement_2 PTInnerStatementList
  deriving (Show, Eq)

data PTForeachOptionalArg = PTForeachOptionalArg_1 
                          | PTForeachOptionalArg_2 PTForeachVariable
  deriving (Show, Eq)

data PTForeachStatement = PTForeachStatement_1 PTStatement
                        | PTForeachStatement_2 PTInnerStatementList
  deriving (Show, Eq)

data PTForeachVariable = PTForeachVariable_1 PTVariable
                       | PTForeachVariable_2 PTVariable
                       | PTForeachVariable_3 PTAssignmentList
  deriving (Show, Eq)

data PTFullyQualifiedClassName = PTFullyQualifiedClassName_1 PTNamespaceName
                               | PTFullyQualifiedClassName_2 PTNamespaceName
                               | PTFullyQualifiedClassName_3 PTNamespaceName
  deriving (Show, Eq)

data PTFunction = PTFunction_1 
  deriving (Show, Eq)

data PTFunctionCall = PTFunctionCall_1 PTNamespaceName PTFunctionCallParameterList
                    | PTFunctionCall_2 PTNamespaceName PTFunctionCallParameterList
                    | PTFunctionCall_3 PTNamespaceName PTFunctionCallParameterList
                    | PTFunctionCall_4 PTClassName PTVariableName PTFunctionCallParameterList
                    | PTFunctionCall_5 PTClassName PTVariableWithoutObjects PTFunctionCallParameterList
                    | PTFunctionCall_6 PTVariableClassName PTVariableName PTFunctionCallParameterList
                    | PTFunctionCall_7 PTVariableClassName PTVariableWithoutObjects PTFunctionCallParameterList
                    | PTFunctionCall_8 PTVariableWithoutObjects PTFunctionCallParameterList
  deriving (Show, Eq)

data PTFunctionCallParameterList = PTFunctionCallParameterList_1 
                                 | PTFunctionCallParameterList_2 PTNonEmptyFunctionCallParameterList
                                 | PTFunctionCallParameterList_3 PTYieldExpr
  deriving (Show, Eq)

data PTFunctionDeclarationStatement = PTFunctionDeclarationStatement_1 PTUntickedFunctionDeclarationStatement
  deriving (Show, Eq)

data PTGlobalVar = PTGlobalVar_1 PVVariableName
                 | PTGlobalVar_2 PTRVariable
                 | PTGlobalVar_3 PTExpr
  deriving (Show, Eq)

data PTGlobalVarList = PTGlobalVarList_1 PTGlobalVarList PTGlobalVar
                     | PTGlobalVarList_2 PTGlobalVar
  deriving (Show, Eq)

data PTImplementsList = PTImplementsList_1 
                      | PTImplementsList_2 PTInterfaceList
  deriving (Show, Eq)

data PTInnerStatement = PTInnerStatement_1 PTStatement
                      | PTInnerStatement_2 PTFunctionDeclarationStatement
                      | PTInnerStatement_3 PTClassDeclarationStatement
  deriving (Show, Eq)

data PTInnerStatementList = PTInnerStatementList_1 PTInnerStatementList PTInnerStatement
                          | PTInnerStatementList_2 
  deriving (Show, Eq)

data PTInstanceCall = PTInstanceCall_1 
                    | PTInstanceCall_2 PTChainingInstanceCall
  deriving (Show, Eq)

data PTInterfaceEntry = PTInterfaceEntry_1 
  deriving (Show, Eq)

data PTInterfaceExtendsList = PTInterfaceExtendsList_1 
                            | PTInterfaceExtendsList_2 PTInterfaceList
  deriving (Show, Eq)

data PTInterfaceList = PTInterfaceList_1 PTFullyQualifiedClassName
                     | PTInterfaceList_2 PTInterfaceList PTFullyQualifiedClassName
  deriving (Show, Eq)

data PTInternalFunctionsInYacc = PTInternalFunctionsInYacc_1 PTIssetVariables
                               | PTInternalFunctionsInYacc_2 PTVariable
                               | PTInternalFunctionsInYacc_3 PTExprWithoutVariable
                               | PTInternalFunctionsInYacc_4 PTExpr
                               | PTInternalFunctionsInYacc_5 PTExpr
                               | PTInternalFunctionsInYacc_6 PTExpr
                               | PTInternalFunctionsInYacc_7 PTExpr
                               | PTInternalFunctionsInYacc_8 PTExpr
  deriving (Show, Eq)

data PTIsReference = PTIsReference_1 
                   | PTIsReference_2 
  deriving (Show, Eq)

data PTIssetVariable = PTIssetVariable_1 PTVariable
                     | PTIssetVariable_2 PTExprWithoutVariable
  deriving (Show, Eq)

data PTIssetVariables = PTIssetVariables_1 PTIssetVariable
                      | PTIssetVariables_2 PTIssetVariables PTIssetVariable
  deriving (Show, Eq)

data PTLexicalVarList = PTLexicalVarList_1 PTLexicalVarList PVVariableName
                      | PTLexicalVarList_2 PTLexicalVarList PVVariableName
                      | PTLexicalVarList_3 PVVariableName
                      | PTLexicalVarList_4 PVVariableName
  deriving (Show, Eq)

data PTLexicalVars = PTLexicalVars_1 
                   | PTLexicalVars_2 PTLexicalVarList
  deriving (Show, Eq)

data PTMemberModifier = PTMemberModifier_1 
                      | PTMemberModifier_2 
                      | PTMemberModifier_3 
                      | PTMemberModifier_4 
                      | PTMemberModifier_5 
                      | PTMemberModifier_6 
  deriving (Show, Eq)

data PTMethod = PTMethod_1 PTFunctionCallParameterList
  deriving (Show, Eq)

data PTMethodBody = PTMethodBody_1 
                  | PTMethodBody_2 PTInnerStatementList
  deriving (Show, Eq)

data PTMethodModifiers = PTMethodModifiers_1 
                       | PTMethodModifiers_2 PTNonEmptyMemberModifiers
  deriving (Show, Eq)

data PTMethodOrNot = PTMethodOrNot_1 PTMethod
                   | PTMethodOrNot_2 PTArrayMethodDereference
                   | PTMethodOrNot_3 
  deriving (Show, Eq)

data PTNamespaceName = PTNamespaceName_1 PVIdent
                     | PTNamespaceName_2 PTNamespaceName PVIdent
  deriving (Show, Eq)

data PTNewElseSingle = PTNewElseSingle_1 
                     | PTNewElseSingle_2 PTInnerStatementList
  deriving (Show, Eq)

data PTNewElseifList = PTNewElseifList_1 
                     | PTNewElseifList_2 PTNewElseifList PTParenthesisExpr PTInnerStatementList
  deriving (Show, Eq)

data PTNewExpr = PTNewExpr_1 PTClassNameReference PTCtorArguments
  deriving (Show, Eq)

data PTNonEmptyAdditionalCatches = PTNonEmptyAdditionalCatches_1 PTAdditionalCatch
                                 | PTNonEmptyAdditionalCatches_2 PTNonEmptyAdditionalCatches PTAdditionalCatch
  deriving (Show, Eq)

data PTNonEmptyArrayPairList = PTNonEmptyArrayPairList_1 PTNonEmptyArrayPairList PTExpr PTExpr
                             | PTNonEmptyArrayPairList_2 PTNonEmptyArrayPairList PTExpr
                             | PTNonEmptyArrayPairList_3 PTExpr PTExpr
                             | PTNonEmptyArrayPairList_4 PTExpr
                             | PTNonEmptyArrayPairList_5 PTNonEmptyArrayPairList PTExpr PTWVariable
                             | PTNonEmptyArrayPairList_6 PTNonEmptyArrayPairList PTWVariable
                             | PTNonEmptyArrayPairList_7 PTExpr PTWVariable
                             | PTNonEmptyArrayPairList_8 PTWVariable
  deriving (Show, Eq)

data PTNonEmptyForExpr = PTNonEmptyForExpr_1 PTNonEmptyForExpr PTExpr
                       | PTNonEmptyForExpr_2 PTExpr
  deriving (Show, Eq)

data PTNonEmptyFunctionCallParameterList = PTNonEmptyFunctionCallParameterList_1 PTExprWithoutVariable
                                         | PTNonEmptyFunctionCallParameterList_2 PTVariable
                                         | PTNonEmptyFunctionCallParameterList_3 PTWVariable
                                         | PTNonEmptyFunctionCallParameterList_4 PTNonEmptyFunctionCallParameterList PTExprWithoutVariable
                                         | PTNonEmptyFunctionCallParameterList_5 PTNonEmptyFunctionCallParameterList PTVariable
                                         | PTNonEmptyFunctionCallParameterList_6 PTNonEmptyFunctionCallParameterList PTWVariable
  deriving (Show, Eq)

data PTNonEmptyMemberModifiers = PTNonEmptyMemberModifiers_1 PTMemberModifier
                               | PTNonEmptyMemberModifiers_2 PTNonEmptyMemberModifiers PTMemberModifier
  deriving (Show, Eq)

data PTNonEmptyParameterList = PTNonEmptyParameterList_1 PTOptionalClassType PVVariableName
                             | PTNonEmptyParameterList_2 PTOptionalClassType PVVariableName
                             | PTNonEmptyParameterList_3 PTOptionalClassType PVVariableName PTStaticScalar
                             | PTNonEmptyParameterList_4 PTOptionalClassType PVVariableName PTStaticScalar
                             | PTNonEmptyParameterList_5 PTNonEmptyParameterList PTOptionalClassType PVVariableName
                             | PTNonEmptyParameterList_6 PTNonEmptyParameterList PTOptionalClassType PVVariableName
                             | PTNonEmptyParameterList_7 PTNonEmptyParameterList PTOptionalClassType PVVariableName PTStaticScalar
                             | PTNonEmptyParameterList_8 PTNonEmptyParameterList PTOptionalClassType PVVariableName PTStaticScalar
  deriving (Show, Eq)

data PTNonEmptyStaticArrayPairList = PTNonEmptyStaticArrayPairList_1 PTNonEmptyStaticArrayPairList PTStaticScalar PTStaticScalar
                                   | PTNonEmptyStaticArrayPairList_2 PTNonEmptyStaticArrayPairList PTStaticScalar
                                   | PTNonEmptyStaticArrayPairList_3 PTStaticScalar PTStaticScalar
                                   | PTNonEmptyStaticArrayPairList_4 PTStaticScalar
  deriving (Show, Eq)

data PTNonEmptyTraitAdaptationList = PTNonEmptyTraitAdaptationList_1 PTTraitAdaptationStatement
                                   | PTNonEmptyTraitAdaptationList_2 PTNonEmptyTraitAdaptationList PTTraitAdaptationStatement
  deriving (Show, Eq)

data PTObjectDimList = PTObjectDimList_1 PTObjectDimList PTDimOffset
                     | PTObjectDimList_2 PTObjectDimList PTExpr
                     | PTObjectDimList_3 PTVariableName
  deriving (Show, Eq)

data PTObjectProperty = PTObjectProperty_1 PTObjectDimList
                      | PTObjectProperty_2 PTVariableWithoutObjects
  deriving (Show, Eq)

data PTOptionalClassType = PTOptionalClassType_1 
                         | PTOptionalClassType_2 
                         | PTOptionalClassType_3 
                         | PTOptionalClassType_4 PTFullyQualifiedClassName
  deriving (Show, Eq)

data PTParameterList = PTParameterList_1 PTNonEmptyParameterList
                     | PTParameterList_2 
  deriving (Show, Eq)

data PTParenthesisExpr = PTParenthesisExpr_1 PTExpr
                       | PTParenthesisExpr_2 PTYieldExpr
  deriving (Show, Eq)

data PTPossibleComma = PTPossibleComma_1 
                     | PTPossibleComma_2 
  deriving (Show, Eq)

data PTRVariable = PTRVariable_1 PTVariable
  deriving (Show, Eq)

data PTReferenceVariable = PTReferenceVariable_1 PTReferenceVariable PTDimOffset
                         | PTReferenceVariable_2 PTReferenceVariable PTExpr
                         | PTReferenceVariable_3 PTCompoundVariable
  deriving (Show, Eq)

data PTRwVariable = PTRwVariable_1 PTVariable
  deriving (Show, Eq)

data PTScalar = PTScalar_1 PVVariableNameImbed
              | PTScalar_10 PTEncapsList
              | PTScalar_11 
              | PTScalar_2 PTClassNameScalar
              | PTScalar_3 PTClassConstant
              | PTScalar_4 PTNamespaceName
              | PTScalar_5 PTNamespaceName
              | PTScalar_6 PTNamespaceName
              | PTScalar_7 PTCommonScalar
              | PTScalar_8 PTEncapsList
              | PTScalar_9 PVString
  deriving (Show, Eq)

data PTSimpleIndirectReference = PTSimpleIndirectReference_1 
                               | PTSimpleIndirectReference_2 PTSimpleIndirectReference
  deriving (Show, Eq)

data PTStart = PTStart_1 PTTopStatementList
  deriving (Show, Eq)

data PTStatement = PTStatement_1 PTUntickedStatement
                 | PTStatement_2 PVIdent
  deriving (Show, Eq)

data PTStaticArrayPairList = PTStaticArrayPairList_1 
                           | PTStaticArrayPairList_2 PTNonEmptyStaticArrayPairList PTPossibleComma
  deriving (Show, Eq)

data PTStaticClassConstant = PTStaticClassConstant_1 PTClassName PVIdent
  deriving (Show, Eq)

data PTStaticClassNameScalar = PTStaticClassNameScalar_1 PTClassName
  deriving (Show, Eq)

data PTStaticMember = PTStaticMember_1 PTClassName PTVariableWithoutObjects
                    | PTStaticMember_2 PTVariableClassName PTVariableWithoutObjects
  deriving (Show, Eq)

data PTStaticScalar = PTStaticScalar_1 PTCommonScalar
                    | PTStaticScalar_10 PTStaticClassConstant
                    | PTStaticScalar_11 
                    | PTStaticScalar_2 PTStaticClassNameScalar
                    | PTStaticScalar_3 PTNamespaceName
                    | PTStaticScalar_4 PTNamespaceName
                    | PTStaticScalar_5 PTNamespaceName
                    | PTStaticScalar_6 PTStaticScalar
                    | PTStaticScalar_7 PTStaticScalar
                    | PTStaticScalar_8 PTStaticArrayPairList
                    | PTStaticScalar_9 PTStaticArrayPairList
  deriving (Show, Eq)

data PTStaticVarList = PTStaticVarList_1 PTStaticVarList PVVariableName
                     | PTStaticVarList_2 PTStaticVarList PVVariableName PTStaticScalar
                     | PTStaticVarList_3 PVVariableName
                     | PTStaticVarList_4 PVVariableName PTStaticScalar
  deriving (Show, Eq)

data PTSwitchCaseList = PTSwitchCaseList_1 PTCaseList
                      | PTSwitchCaseList_2 PTCaseList
                      | PTSwitchCaseList_3 PTCaseList
                      | PTSwitchCaseList_4 PTCaseList
  deriving (Show, Eq)

data PTTopStatement = PTTopStatement_1 PTStatement
                    | PTTopStatement_2 PTFunctionDeclarationStatement
                    | PTTopStatement_3 PTClassDeclarationStatement
                    | PTTopStatement_4 PTNamespaceName
                    | PTTopStatement_5 PTNamespaceName PTTopStatementList
                    | PTTopStatement_6 PTTopStatementList
                    | PTTopStatement_7 PTUseDeclarations
                    | PTTopStatement_8 PTConstantDeclaration
  deriving (Show, Eq)

data PTTopStatementList = PTTopStatementList_1 PTTopStatementList PTTopStatement
                        | PTTopStatementList_2 
  deriving (Show, Eq)

data PTTraitAdaptationList = PTTraitAdaptationList_1 
                           | PTTraitAdaptationList_2 PTNonEmptyTraitAdaptationList
  deriving (Show, Eq)

data PTTraitAdaptationStatement = PTTraitAdaptationStatement_1 PTTraitPrecedence
                                | PTTraitAdaptationStatement_2 PTTraitAlias
  deriving (Show, Eq)

data PTTraitAdaptations = PTTraitAdaptations_1 
                        | PTTraitAdaptations_2 PTTraitAdaptationList
  deriving (Show, Eq)

data PTTraitAlias = PTTraitAlias_1 PTTraitMethodReference PTTraitModifiers PVIdent
                  | PTTraitAlias_2 PTTraitMethodReference PTMemberModifier
  deriving (Show, Eq)

data PTTraitList = PTTraitList_1 PTFullyQualifiedClassName
                 | PTTraitList_2 PTTraitList PTFullyQualifiedClassName
  deriving (Show, Eq)

data PTTraitMethodReference = PTTraitMethodReference_1 PVIdent
                            | PTTraitMethodReference_2 PTTraitMethodReferenceFullyQualified
  deriving (Show, Eq)

data PTTraitMethodReferenceFullyQualified = PTTraitMethodReferenceFullyQualified_1 PTFullyQualifiedClassName PVIdent
  deriving (Show, Eq)

data PTTraitModifiers = PTTraitModifiers_1 
                      | PTTraitModifiers_2 PTMemberModifier
  deriving (Show, Eq)

data PTTraitPrecedence = PTTraitPrecedence_1 PTTraitMethodReferenceFullyQualified PTTraitReferenceList
  deriving (Show, Eq)

data PTTraitReferenceList = PTTraitReferenceList_1 PTFullyQualifiedClassName
                          | PTTraitReferenceList_2 PTTraitReferenceList PTFullyQualifiedClassName
  deriving (Show, Eq)

data PTTraitUseStatement = PTTraitUseStatement_1 PTTraitList PTTraitAdaptations
  deriving (Show, Eq)

data PTUnsetVariable = PTUnsetVariable_1 PTVariable
  deriving (Show, Eq)

data PTUnsetVariables = PTUnsetVariables_1 PTUnsetVariable
                      | PTUnsetVariables_2 PTUnsetVariables PTUnsetVariable
  deriving (Show, Eq)

data PTUntickedClassDeclarationStatement = PTUntickedClassDeclarationStatement_1 PTClassEntryType PVIdent PTExtendsFrom PTImplementsList PTClassStatementList
                                         | PTUntickedClassDeclarationStatement_2 PTInterfaceEntry PVIdent PTInterfaceExtendsList PTClassStatementList
  deriving (Show, Eq)

data PTUntickedFunctionDeclarationStatement = PTUntickedFunctionDeclarationStatement_1 PTFunction PTIsReference PVIdent PTParameterList PTInnerStatementList
  deriving (Show, Eq)

data PTUntickedStatement = PTUntickedStatement_1 PTInnerStatementList
                         | PTUntickedStatement_10 
                         | PTUntickedStatement_11 PTExpr
                         | PTUntickedStatement_12 
                         | PTUntickedStatement_13 PTExprWithoutVariable
                         | PTUntickedStatement_14 PTVariable
                         | PTUntickedStatement_15 PTYieldExpr
                         | PTUntickedStatement_16 PTGlobalVarList
                         | PTUntickedStatement_17 PTStaticVarList
                         | PTUntickedStatement_18 PTEchoExprList
                         | PTUntickedStatement_19 PVInline
                         | PTUntickedStatement_2 PTParenthesisExpr PTStatement PTElseifList PTElseSingle
                         | PTUntickedStatement_20 PTExpr
                         | PTUntickedStatement_21 PTUnsetVariables
                         | PTUntickedStatement_22 PTVariable PTForeachVariable PTForeachOptionalArg PTForeachStatement
                         | PTUntickedStatement_23 PTExprWithoutVariable PTForeachVariable PTForeachOptionalArg PTForeachStatement
                         | PTUntickedStatement_24 PTDeclareList PTDeclareStatement
                         | PTUntickedStatement_25 
                         | PTUntickedStatement_26 PTInnerStatementList PTCatchStatement PTFinallyStatement
                         | PTUntickedStatement_27 PTExpr
                         | PTUntickedStatement_28 PVIdent
                         | PTUntickedStatement_3 PTParenthesisExpr PTInnerStatementList PTNewElseifList PTNewElseSingle
                         | PTUntickedStatement_4 PTParenthesisExpr PTWhileStatement
                         | PTUntickedStatement_5 PTStatement PTParenthesisExpr
                         | PTUntickedStatement_6 PTForExpr PTForExpr PTForExpr PTForStatement
                         | PTUntickedStatement_7 PTParenthesisExpr PTSwitchCaseList
                         | PTUntickedStatement_8 
                         | PTUntickedStatement_9 PTExpr
  deriving (Show, Eq)

data PTUseDeclaration = PTUseDeclaration_1 PTNamespaceName
                      | PTUseDeclaration_2 PTNamespaceName PVIdent
                      | PTUseDeclaration_3 PTNamespaceName
                      | PTUseDeclaration_4 PTNamespaceName PVIdent
  deriving (Show, Eq)

data PTUseDeclarations = PTUseDeclarations_1 PTUseDeclarations PTUseDeclaration
                       | PTUseDeclarations_2 PTUseDeclaration
  deriving (Show, Eq)

data PTVariable = PTVariable_1 PTBaseVariableWithFunctionCalls PTObjectProperty PTMethodOrNot PTVariableProperties
                | PTVariable_2 PTBaseVariableWithFunctionCalls
  deriving (Show, Eq)

data PTVariableClassName = PTVariableClassName_1 PTReferenceVariable
  deriving (Show, Eq)

data PTVariableModifiers = PTVariableModifiers_1 PTNonEmptyMemberModifiers
                         | PTVariableModifiers_2 
  deriving (Show, Eq)

data PTVariableName = PTVariableName_1 PVIdent
                    | PTVariableName_2 PTExpr
  deriving (Show, Eq)

data PTVariableProperties = PTVariableProperties_1 PTVariableProperties PTVariableProperty
                          | PTVariableProperties_2 
  deriving (Show, Eq)

data PTVariableProperty = PTVariableProperty_1 PTObjectProperty PTMethodOrNot
  deriving (Show, Eq)

data PTVariableWithoutObjects = PTVariableWithoutObjects_1 PTReferenceVariable
                              | PTVariableWithoutObjects_2 PTSimpleIndirectReference PTReferenceVariable
  deriving (Show, Eq)

data PTWVariable = PTWVariable_1 PTVariable
  deriving (Show, Eq)

data PTWhileStatement = PTWhileStatement_1 PTStatement
                      | PTWhileStatement_2 PTInnerStatementList
  deriving (Show, Eq)

data PTYieldExpr = PTYieldExpr_1 PTExprWithoutVariable
                 | PTYieldExpr_2 PTVariable
                 | PTYieldExpr_3 PTExpr PTExprWithoutVariable
                 | PTYieldExpr_4 PTExpr PTVariable
  deriving (Show, Eq)

newtype PVDouble = PVDouble String
  deriving (Show, Eq)
newtype PVIdent = PVIdent String
  deriving (Show, Eq)
newtype PVInline = PVInline String
  deriving (Show, Eq)
newtype PVInteger = PVInteger String
  deriving (Show, Eq)
newtype PVString = PVString String
  deriving (Show, Eq)
newtype PVVariableName = PVVariableName String
  deriving (Show, Eq)
newtype PVVariableNameImbed = PVVariableNameImbed String
  deriving (Show, Eq)
