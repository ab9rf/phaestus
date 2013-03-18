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
data PTAdditionalCatch = PTAdditionalCatch1 PTFullyQualifiedClassName PVVariableName PTInnerStatementList
  deriving (Show, Eq)

data PTAdditionalCatches = PTAdditionalCatches1 PTNonEmptyAdditionalCatches
                         | PTAdditionalCatches2 
  deriving (Show, Eq)

data PTArrayFunctionDereference = PTArrayFunctionDereference1 PTArrayFunctionDereference PTDimOffset
                                | PTArrayFunctionDereference2 PTFunctionCall PTDimOffset
  deriving (Show, Eq)

data PTArrayMethodDereference = PTArrayMethodDereference1 PTArrayMethodDereference PTDimOffset
                              | PTArrayMethodDereference2 PTMethod PTDimOffset
  deriving (Show, Eq)

data PTArrayPairList = PTArrayPairList1 
                     | PTArrayPairList2 PTNonEmptyArrayPairList PTPossibleComma
  deriving (Show, Eq)

data PTAssignmentList = PTAssignmentList1 PTAssignmentList PTAssignmentListElement
                      | PTAssignmentList2 PTAssignmentListElement
  deriving (Show, Eq)

data PTAssignmentListElement = PTAssignmentListElement1 PTVariable
                             | PTAssignmentListElement2 PTAssignmentList
                             | PTAssignmentListElement3 
  deriving (Show, Eq)

data PTBackticksExpr = PTBackticksExpr1 
                     | PTBackticksExpr2 PVString
                     | PTBackticksExpr3 PTEncapsList
  deriving (Show, Eq)

data PTBaseVariable = PTBaseVariable1 PTReferenceVariable
                    | PTBaseVariable2 PTSimpleIndirectReference PTReferenceVariable
                    | PTBaseVariable3 PTStaticMember
  deriving (Show, Eq)

data PTBaseVariableWithFunctionCalls = PTBaseVariableWithFunctionCalls1 PTBaseVariable
                                     | PTBaseVariableWithFunctionCalls2 PTArrayFunctionDereference
                                     | PTBaseVariableWithFunctionCalls3 PTFunctionCall
  deriving (Show, Eq)

data PTCaseList = PTCaseList1 
                | PTCaseList2 PTCaseList PTExpr PTCaseSeparator PTInnerStatementList
                | PTCaseList3 PTCaseList PTCaseSeparator PTInnerStatementList
  deriving (Show, Eq)

data PTCaseSeparator = PTCaseSeparator1 
                     | PTCaseSeparator2 
  deriving (Show, Eq)

data PTCatchStatement = PTCatchStatement1 
                      | PTCatchStatement2 PTFullyQualifiedClassName PVVariableName PTInnerStatementList PTAdditionalCatches
  deriving (Show, Eq)

data PTChainingDereference = PTChainingDereference1 PTChainingDereference PTDimOffset
                           | PTChainingDereference2 PTDimOffset
  deriving (Show, Eq)

data PTChainingInstanceCall = PTChainingInstanceCall1 PTChainingDereference PTChainingMethodOrProperty
                            | PTChainingInstanceCall2 PTChainingDereference
                            | PTChainingInstanceCall3 PTChainingMethodOrProperty
  deriving (Show, Eq)

data PTChainingMethodOrProperty = PTChainingMethodOrProperty1 PTChainingMethodOrProperty PTVariableProperty
                                | PTChainingMethodOrProperty2 PTVariableProperty
  deriving (Show, Eq)

data PTClassConstant = PTClassConstant1 PTClassName PVIdent
                     | PTClassConstant2 PTVariableClassName PVIdent
  deriving (Show, Eq)

data PTClassConstantDeclaration = PTClassConstantDeclaration1 PTClassConstantDeclaration PVIdent PTStaticScalar
                                | PTClassConstantDeclaration2 PVIdent PTStaticScalar
  deriving (Show, Eq)

data PTClassDeclarationStatement = PTClassDeclarationStatement1 PTUntickedClassDeclarationStatement
  deriving (Show, Eq)

data PTClassEntryType = PTClassEntryType1 
                      | PTClassEntryType2 
                      | PTClassEntryType3 
                      | PTClassEntryType4 
  deriving (Show, Eq)

data PTClassName = PTClassName1 
                 | PTClassName2 PTNamespaceName
                 | PTClassName3 PTNamespaceName
                 | PTClassName4 PTNamespaceName
  deriving (Show, Eq)

data PTClassNameReference = PTClassNameReference1 PTClassName
                          | PTClassNameReference2 PTDynamicClassNameReference
  deriving (Show, Eq)

data PTClassNameScalar = PTClassNameScalar1 PTClassName
  deriving (Show, Eq)

data PTClassStatement = PTClassStatement1 PTVariableModifiers PTClassVariableDeclaration
                      | PTClassStatement2 PTClassConstantDeclaration
                      | PTClassStatement3 PTTraitUseStatement
                      | PTClassStatement4 PTMethodModifiers PTFunction PTIsReference PVIdent PTParameterList PTMethodBody
  deriving (Show, Eq)

data PTClassStatementList = PTClassStatementList1 PTClassStatementList PTClassStatement
                          | PTClassStatementList2 
  deriving (Show, Eq)

data PTClassVariableDeclaration = PTClassVariableDeclaration1 PTClassVariableDeclaration PVVariableName
                                | PTClassVariableDeclaration2 PTClassVariableDeclaration PVVariableName PTStaticScalar
                                | PTClassVariableDeclaration3 PVVariableName
                                | PTClassVariableDeclaration4 PVVariableName PTStaticScalar
  deriving (Show, Eq)

data PTCombinedScalar = PTCombinedScalar1 PTArrayPairList
                      | PTCombinedScalar2 PTArrayPairList
  deriving (Show, Eq)

data PTCombinedScalarOffset = PTCombinedScalarOffset1 PTCombinedScalar PTDimOffset
                            | PTCombinedScalarOffset2 PTCombinedScalarOffset PTDimOffset
                            | PTCombinedScalarOffset3 PVString PTDimOffset
  deriving (Show, Eq)

data PTCommonScalar = PTCommonScalar1 PVInteger
                    | PTCommonScalar10 
                    | PTCommonScalar11 PVString
                    | PTCommonScalar12 
                    | PTCommonScalar2 PVDouble
                    | PTCommonScalar3 PVString
                    | PTCommonScalar4 
                    | PTCommonScalar5 
                    | PTCommonScalar6 
                    | PTCommonScalar7 
                    | PTCommonScalar8 
                    | PTCommonScalar9 
  deriving (Show, Eq)

data PTCompoundVariable = PTCompoundVariable1 PVVariableName
                        | PTCompoundVariable2 PTExpr
  deriving (Show, Eq)

data PTConstantDeclaration = PTConstantDeclaration1 PTConstantDeclaration PVIdent PTStaticScalar
                           | PTConstantDeclaration2 PVIdent PTStaticScalar
  deriving (Show, Eq)

data PTCtorArguments = PTCtorArguments1 
                     | PTCtorArguments2 PTFunctionCallParameterList
  deriving (Show, Eq)

data PTDeclareList = PTDeclareList1 PVIdent PTStaticScalar
                   | PTDeclareList2 PTDeclareList PVIdent PTStaticScalar
  deriving (Show, Eq)

data PTDeclareStatement = PTDeclareStatement1 PTStatement
                        | PTDeclareStatement2 PTInnerStatementList
  deriving (Show, Eq)

data PTDimOffset = PTDimOffset1 
                 | PTDimOffset2 PTExpr
  deriving (Show, Eq)

data PTDynamicClassNameReference = PTDynamicClassNameReference1 PTBaseVariable PTObjectProperty PTDynamicClassNameVariableProperties
                                 | PTDynamicClassNameReference2 PTBaseVariable
  deriving (Show, Eq)

data PTDynamicClassNameVariableProperties = PTDynamicClassNameVariableProperties1 PTDynamicClassNameVariableProperties PTDynamicClassNameVariableProperty
                                          | PTDynamicClassNameVariableProperties2 
  deriving (Show, Eq)

data PTDynamicClassNameVariableProperty = PTDynamicClassNameVariableProperty1 PTObjectProperty
  deriving (Show, Eq)

data PTEchoExprList = PTEchoExprList1 PTEchoExprList PTExpr
                    | PTEchoExprList2 PTExpr
  deriving (Show, Eq)

data PTElseSingle = PTElseSingle1 
                  | PTElseSingle2 PTStatement
  deriving (Show, Eq)

data PTElseifList = PTElseifList1 
                  | PTElseifList2 PTElseifList PTParenthesisExpr PTStatement
  deriving (Show, Eq)

data PTEncapsList = PTEncapsList1 PTEncapsList PTEncapsVar
                  | PTEncapsList2 PTEncapsList PVString
                  | PTEncapsList3 PTEncapsVar
                  | PTEncapsList4 PVString PTEncapsVar
  deriving (Show, Eq)

data PTEncapsVar = PTEncapsVar1 PVVariableName
                 | PTEncapsVar2 PVVariableName PTEncapsVarOffset
                 | PTEncapsVar3 PVVariableName PVIdent
                 | PTEncapsVar4 PTExpr
                 | PTEncapsVar5 PVVariableNameImbed PTExpr
                 | PTEncapsVar6 PTVariable
  deriving (Show, Eq)

data PTEncapsVarOffset = PTEncapsVarOffset1 PVIdent
                       | PTEncapsVarOffset2 PVInteger
                       | PTEncapsVarOffset3 PVVariableName
  deriving (Show, Eq)

data PTExitExpr = PTExitExpr1 
                | PTExitExpr2 
                | PTExitExpr3 PTParenthesisExpr
  deriving (Show, Eq)

data PTExpr = RvalueAsLvalue PTRVariable
           | ListAssignment PTAssignmentList PTExpr
           | PTExprWithoutVariable10 PTVariable PTExpr
           | PTExprWithoutVariable11 PTVariable PTExpr
           | PTExprWithoutVariable12 PTVariable PTExpr
           | PTExprWithoutVariable13 PTVariable PTExpr
           | PTExprWithoutVariable14 PTVariable PTExpr
           | PTExprWithoutVariable15 PTVariable PTExpr
           | PTExprWithoutVariable16 PTVariable PTExpr
           | PTExprWithoutVariable17 PTRwVariable
           | PTExprWithoutVariable18 PTRwVariable
           | PTExprWithoutVariable19 PTRwVariable
           | VariableAssignment PTVariable PTExpr
           | PTExprWithoutVariable20 PTRwVariable
           | PTExprWithoutVariable21 PTExpr PTExpr
           | PTExprWithoutVariable22 PTExpr PTExpr
           | PTExprWithoutVariable23 PTExpr PTExpr
           | PTExprWithoutVariable24 PTExpr PTExpr
           | PTExprWithoutVariable25 PTExpr PTExpr
           | PTExprWithoutVariable26 PTExpr PTExpr
           | PTExprWithoutVariable27 PTExpr PTExpr
           | PTExprWithoutVariable28 PTExpr PTExpr
           | PTExprWithoutVariable29 PTExpr PTExpr
           | ReferenceAssignment PTVariable PTVariable
           | PTExprWithoutVariable30 PTExpr PTExpr
           | PTExprWithoutVariable31 PTExpr PTExpr
           | PTExprWithoutVariable32 PTExpr PTExpr
           | PTExprWithoutVariable33 PTExpr PTExpr
           | PTExprWithoutVariable34 PTExpr PTExpr
           | PTExprWithoutVariable35 PTExpr PTExpr
           | PTExprWithoutVariable36 PTExpr PTExpr
           | PTExprWithoutVariable37 PTExpr
           | PTExprWithoutVariable38 PTExpr
           | PTExprWithoutVariable39 PTExpr
           | PTExprWithoutVariable4 PTVariable PTClassNameReference PTCtorArguments
           | PTExprWithoutVariable40 PTExpr
           | PTExprWithoutVariable41 PTExpr PTExpr
           | PTExprWithoutVariable42 PTExpr PTExpr
           | PTExprWithoutVariable43 PTExpr PTExpr
           | PTExprWithoutVariable44 PTExpr PTExpr
           | PTExprWithoutVariable45 PTExpr PTExpr
           | PTExprWithoutVariable46 PTExpr PTExpr
           | PTExprWithoutVariable47 PTExpr PTExpr
           | PTExprWithoutVariable48 PTExpr PTExpr
           | PTExprWithoutVariable49 PTExpr PTClassNameReference
           | PTExprWithoutVariable5 PTExpr
           | PTExprWithoutVariable50 PTParenthesisExpr
           | PTExprWithoutVariable51 PTNewExpr
           | PTExprWithoutVariable52 PTNewExpr PTInstanceCall
           | PTExprWithoutVariable53 PTExpr PTExpr PTExpr
           | PTExprWithoutVariable54 PTExpr PTExpr
           | PTExprWithoutVariable55 PTInternalFunctionsInYacc
           | PTExprWithoutVariable56 PTExpr
           | PTExprWithoutVariable57 PTExpr
           | PTExprWithoutVariable58 PTExpr
           | PTExprWithoutVariable59 PTExpr
           | PTExprWithoutVariable6 PTVariable PTExpr
           | PTExprWithoutVariable60 PTExpr
           | PTExprWithoutVariable61 PTExpr
           | PTExprWithoutVariable62 PTExpr
           | PTExprWithoutVariable63 PTExitExpr
           | PTExprWithoutVariable64 PTExpr
           | PTExprWithoutVariable65 PTScalar
           | PTExprWithoutVariable66 PTCombinedScalarOffset
           | PTExprWithoutVariable67 PTCombinedScalar
           | PTExprWithoutVariable68 PTBackticksExpr
           | PTExprWithoutVariable69 PTExpr
           | PTExprWithoutVariable7 PTVariable PTExpr
           | PTExprWithoutVariable70 
           | PTExprWithoutVariable71 PTFunction PTIsReference PTParameterList PTLexicalVars PTInnerStatementList
           | PTExprWithoutVariable72 PTFunction PTIsReference PTParameterList PTLexicalVars PTInnerStatementList
           | PTExprWithoutVariable8 PTVariable PTExpr
           | PTExprWithoutVariable9 PTVariable PTExpr
  deriving (Show, Eq)

data PTExtendsFrom = PTExtendsFrom1 
                   | PTExtendsFrom2 PTFullyQualifiedClassName
  deriving (Show, Eq)

data PTFinallyStatement = PTFinallyStatement1 
                        | PTFinallyStatement2 PTInnerStatementList
  deriving (Show, Eq)

data PTForExpr = PTForExpr1 
               | PTForExpr2 PTNonEmptyForExpr
  deriving (Show, Eq)

data PTForStatement = PTForStatement1 PTStatement
                    | PTForStatement2 PTInnerStatementList
  deriving (Show, Eq)

data PTForeachOptionalArg = PTForeachOptionalArg1 
                          | PTForeachOptionalArg2 PTForeachVariable
  deriving (Show, Eq)

data PTForeachStatement = PTForeachStatement1 PTStatement
                        | PTForeachStatement2 PTInnerStatementList
  deriving (Show, Eq)

data PTForeachVariable = PTForeachVariable1 PTVariable
                       | PTForeachVariable2 PTVariable
                       | PTForeachVariable3 PTAssignmentList
  deriving (Show, Eq)

data PTFullyQualifiedClassName = PTFullyQualifiedClassName1 PTNamespaceName
                               | PTFullyQualifiedClassName2 PTNamespaceName
                               | PTFullyQualifiedClassName3 PTNamespaceName
  deriving (Show, Eq)

data PTFunction = PTFunction1 
  deriving (Show, Eq)

data PTFunctionCall = PTFunctionCall1 PTNamespaceName PTFunctionCallParameterList
                    | PTFunctionCall2 PTNamespaceName PTFunctionCallParameterList
                    | PTFunctionCall3 PTNamespaceName PTFunctionCallParameterList
                    | PTFunctionCall4 PTClassName PTVariableName PTFunctionCallParameterList
                    | PTFunctionCall5 PTClassName PTVariableWithoutObjects PTFunctionCallParameterList
                    | PTFunctionCall6 PTVariableClassName PTVariableName PTFunctionCallParameterList
                    | PTFunctionCall7 PTVariableClassName PTVariableWithoutObjects PTFunctionCallParameterList
                    | PTFunctionCall8 PTVariableWithoutObjects PTFunctionCallParameterList
  deriving (Show, Eq)

data PTFunctionCallParameterList = PTFunctionCallParameterList1 
                                 | PTFunctionCallParameterList2 PTNonEmptyFunctionCallParameterList
                                 | PTFunctionCallParameterList3 PTYieldExpr
  deriving (Show, Eq)

data PTFunctionDeclarationStatement = PTFunctionDeclarationStatement1 PTUntickedFunctionDeclarationStatement
  deriving (Show, Eq)

data PTGlobalVar = PTGlobalVar1 PVVariableName
                 | PTGlobalVar2 PTRVariable
                 | PTGlobalVar3 PTExpr
  deriving (Show, Eq)

data PTGlobalVarList = PTGlobalVarList1 PTGlobalVarList PTGlobalVar
                     | PTGlobalVarList2 PTGlobalVar
  deriving (Show, Eq)

data PTImplementsList = PTImplementsList1 
                      | PTImplementsList2 PTInterfaceList
  deriving (Show, Eq)

data PTInnerStatement = PTInnerStatement1 PTStatement
                      | PTInnerStatement2 PTFunctionDeclarationStatement
                      | PTInnerStatement3 PTClassDeclarationStatement
  deriving (Show, Eq)

data PTInnerStatementList = PTInnerStatementList1 PTInnerStatementList PTInnerStatement
                          | PTInnerStatementList2 
  deriving (Show, Eq)

data PTInstanceCall = PTInstanceCall1 
                    | PTInstanceCall2 PTChainingInstanceCall
  deriving (Show, Eq)

data PTInterfaceEntry = PTInterfaceEntry1 
  deriving (Show, Eq)

data PTInterfaceExtendsList = PTInterfaceExtendsList1 
                            | PTInterfaceExtendsList2 PTInterfaceList
  deriving (Show, Eq)

data PTInterfaceList = PTInterfaceList1 PTFullyQualifiedClassName
                     | PTInterfaceList2 PTInterfaceList PTFullyQualifiedClassName
  deriving (Show, Eq)

data PTInternalFunctionsInYacc = PTInternalFunctionsInYacc1 PTIssetVariables
                               | PTInternalFunctionsInYacc2 PTVariable
                               | PTInternalFunctionsInYacc3 PTExpr
                               | PTInternalFunctionsInYacc4 PTExpr
                               | PTInternalFunctionsInYacc5 PTExpr
                               | PTInternalFunctionsInYacc6 PTExpr
                               | PTInternalFunctionsInYacc7 PTExpr
                               | PTInternalFunctionsInYacc8 PTExpr
  deriving (Show, Eq)

data PTIsReference = PTIsReference1 
                   | PTIsReference2 
  deriving (Show, Eq)

data PTIssetVariable = PTIssetVariable1 PTVariable
                     | PTIssetVariable2 PTExpr
  deriving (Show, Eq)

data PTIssetVariables = PTIssetVariables1 PTIssetVariable
                      | PTIssetVariables2 PTIssetVariables PTIssetVariable
  deriving (Show, Eq)

data PTLexicalVarList = PTLexicalVarList1 PTLexicalVarList PVVariableName
                      | PTLexicalVarList2 PTLexicalVarList PVVariableName
                      | PTLexicalVarList3 PVVariableName
                      | PTLexicalVarList4 PVVariableName
  deriving (Show, Eq)

data PTLexicalVars = PTLexicalVars1 
                   | PTLexicalVars2 PTLexicalVarList
  deriving (Show, Eq)

data PTMemberModifier = PTMemberModifier1 
                      | PTMemberModifier2 
                      | PTMemberModifier3 
                      | PTMemberModifier4 
                      | PTMemberModifier5 
                      | PTMemberModifier6 
  deriving (Show, Eq)

data PTMethod = PTMethod1 PTFunctionCallParameterList
  deriving (Show, Eq)

data PTMethodBody = PTMethodBody1 
                  | PTMethodBody2 PTInnerStatementList
  deriving (Show, Eq)

data PTMethodModifiers = PTMethodModifiers1 
                       | PTMethodModifiers2 PTNonEmptyMemberModifiers
  deriving (Show, Eq)

data PTMethodOrNot = PTMethodOrNot1 PTMethod
                   | PTMethodOrNot2 PTArrayMethodDereference
                   | PTMethodOrNot3 
  deriving (Show, Eq)

data PTNamespaceName = PTNamespaceName1 PVIdent
                     | PTNamespaceName2 PTNamespaceName PVIdent
  deriving (Show, Eq)

data PTNewElseSingle = PTNewElseSingle1 
                     | PTNewElseSingle2 PTInnerStatementList
  deriving (Show, Eq)

data PTNewElseifList = PTNewElseifList1 
                     | PTNewElseifList2 PTNewElseifList PTParenthesisExpr PTInnerStatementList
  deriving (Show, Eq)

data PTNewExpr = PTNewExpr1 PTClassNameReference PTCtorArguments
  deriving (Show, Eq)

data PTNonEmptyAdditionalCatches = PTNonEmptyAdditionalCatches1 PTAdditionalCatch
                                 | PTNonEmptyAdditionalCatches2 PTNonEmptyAdditionalCatches PTAdditionalCatch
  deriving (Show, Eq)

data PTNonEmptyArrayPairList = PTNonEmptyArrayPairList1 PTNonEmptyArrayPairList PTExpr PTExpr
                             | PTNonEmptyArrayPairList2 PTNonEmptyArrayPairList PTExpr
                             | PTNonEmptyArrayPairList3 PTExpr PTExpr
                             | PTNonEmptyArrayPairList4 PTExpr
                             | PTNonEmptyArrayPairList5 PTNonEmptyArrayPairList PTExpr PTWVariable
                             | PTNonEmptyArrayPairList6 PTNonEmptyArrayPairList PTWVariable
                             | PTNonEmptyArrayPairList7 PTExpr PTWVariable
                             | PTNonEmptyArrayPairList8 PTWVariable
  deriving (Show, Eq)

data PTNonEmptyForExpr = PTNonEmptyForExpr1 PTNonEmptyForExpr PTExpr
                       | PTNonEmptyForExpr2 PTExpr
  deriving (Show, Eq)

data PTNonEmptyFunctionCallParameterList = PTNonEmptyFunctionCallParameterList1 PTExpr
                                         | PTNonEmptyFunctionCallParameterList2 PTVariable
                                         | PTNonEmptyFunctionCallParameterList3 PTWVariable
                                         | PTNonEmptyFunctionCallParameterList4 PTNonEmptyFunctionCallParameterList PTExpr
                                         | PTNonEmptyFunctionCallParameterList5 PTNonEmptyFunctionCallParameterList PTVariable
                                         | PTNonEmptyFunctionCallParameterList6 PTNonEmptyFunctionCallParameterList PTWVariable
  deriving (Show, Eq)

data PTNonEmptyMemberModifiers = PTNonEmptyMemberModifiers1 PTMemberModifier
                               | PTNonEmptyMemberModifiers2 PTNonEmptyMemberModifiers PTMemberModifier
  deriving (Show, Eq)

data PTNonEmptyParameterList = PTNonEmptyParameterList1 PTOptionalClassType PVVariableName
                             | PTNonEmptyParameterList2 PTOptionalClassType PVVariableName
                             | PTNonEmptyParameterList3 PTOptionalClassType PVVariableName PTStaticScalar
                             | PTNonEmptyParameterList4 PTOptionalClassType PVVariableName PTStaticScalar
                             | PTNonEmptyParameterList5 PTNonEmptyParameterList PTOptionalClassType PVVariableName
                             | PTNonEmptyParameterList6 PTNonEmptyParameterList PTOptionalClassType PVVariableName
                             | PTNonEmptyParameterList7 PTNonEmptyParameterList PTOptionalClassType PVVariableName PTStaticScalar
                             | PTNonEmptyParameterList8 PTNonEmptyParameterList PTOptionalClassType PVVariableName PTStaticScalar
  deriving (Show, Eq)

data PTNonEmptyStaticArrayPairList = PTNonEmptyStaticArrayPairList1 PTNonEmptyStaticArrayPairList PTStaticScalar PTStaticScalar
                                   | PTNonEmptyStaticArrayPairList2 PTNonEmptyStaticArrayPairList PTStaticScalar
                                   | PTNonEmptyStaticArrayPairList3 PTStaticScalar PTStaticScalar
                                   | PTNonEmptyStaticArrayPairList4 PTStaticScalar
  deriving (Show, Eq)

data PTNonEmptyTraitAdaptationList = PTNonEmptyTraitAdaptationList1 PTTraitAdaptationStatement
                                   | PTNonEmptyTraitAdaptationList2 PTNonEmptyTraitAdaptationList PTTraitAdaptationStatement
  deriving (Show, Eq)

data PTObjectDimList = PTObjectDimList1 PTObjectDimList PTDimOffset
                     | PTObjectDimList2 PTObjectDimList PTExpr
                     | PTObjectDimList3 PTVariableName
  deriving (Show, Eq)

data PTObjectProperty = PTObjectProperty1 PTObjectDimList
                      | PTObjectProperty2 PTVariableWithoutObjects
  deriving (Show, Eq)

data PTOptionalClassType = PTOptionalClassType1 
                         | PTOptionalClassType2 
                         | PTOptionalClassType3 
                         | PTOptionalClassType4 PTFullyQualifiedClassName
  deriving (Show, Eq)

data PTParameterList = PTParameterList1 PTNonEmptyParameterList
                     | PTParameterList2 
  deriving (Show, Eq)

data PTParenthesisExpr = PTParenthesisExpr1 PTExpr
                       | PTParenthesisExpr2 PTYieldExpr
  deriving (Show, Eq)

data PTPossibleComma = PTPossibleComma1 
                     | PTPossibleComma2 
  deriving (Show, Eq)

data PTRVariable = PTRVariable1 PTVariable
  deriving (Show, Eq)

data PTReferenceVariable = PTReferenceVariable1 PTReferenceVariable PTDimOffset
                         | PTReferenceVariable2 PTReferenceVariable PTExpr
                         | PTReferenceVariable3 PTCompoundVariable
  deriving (Show, Eq)

data PTRwVariable = PTRwVariable1 PTVariable
  deriving (Show, Eq)

data PTScalar = PTScalar1 PVVariableNameImbed
              | PTScalar10 PTEncapsList
              | PTScalar11 
              | PTScalar2 PTClassNameScalar
              | PTScalar3 PTClassConstant
              | PTScalar4 PTNamespaceName
              | PTScalar5 PTNamespaceName
              | PTScalar6 PTNamespaceName
              | PTScalar7 PTCommonScalar
              | PTScalar8 PTEncapsList
              | PTScalar9 PVString
  deriving (Show, Eq)

data PTSimpleIndirectReference = PTSimpleIndirectReference1 
                               | PTSimpleIndirectReference2 PTSimpleIndirectReference
  deriving (Show, Eq)

data PTStart = PTStart1 PTTopStatementList
  deriving (Show, Eq)

data PTStatement = PTStatement1 PTUntickedStatement
                 | PTStatement2 PVIdent
  deriving (Show, Eq)

data PTStaticArrayPairList = PTStaticArrayPairList1 
                           | PTStaticArrayPairList2 PTNonEmptyStaticArrayPairList PTPossibleComma
  deriving (Show, Eq)

data PTStaticClassConstant = PTStaticClassConstant1 PTClassName PVIdent
  deriving (Show, Eq)

data PTStaticClassNameScalar = PTStaticClassNameScalar1 PTClassName
  deriving (Show, Eq)

data PTStaticMember = PTStaticMember1 PTClassName PTVariableWithoutObjects
                    | PTStaticMember2 PTVariableClassName PTVariableWithoutObjects
  deriving (Show, Eq)

data PTStaticScalar = PTStaticScalar1 PTCommonScalar
                    | PTStaticScalar10 PTStaticClassConstant
                    | PTStaticScalar11 
                    | PTStaticScalar2 PTStaticClassNameScalar
                    | PTStaticScalar3 PTNamespaceName
                    | PTStaticScalar4 PTNamespaceName
                    | PTStaticScalar5 PTNamespaceName
                    | PTStaticScalar6 PTStaticScalar
                    | PTStaticScalar7 PTStaticScalar
                    | PTStaticScalar8 PTStaticArrayPairList
                    | PTStaticScalar9 PTStaticArrayPairList
  deriving (Show, Eq)

data PTStaticVarList = PTStaticVarList1 PTStaticVarList PVVariableName
                     | PTStaticVarList2 PTStaticVarList PVVariableName PTStaticScalar
                     | PTStaticVarList3 PVVariableName
                     | PTStaticVarList4 PVVariableName PTStaticScalar
  deriving (Show, Eq)

data PTSwitchCaseList = PTSwitchCaseList1 PTCaseList
                      | PTSwitchCaseList2 PTCaseList
                      | PTSwitchCaseList3 PTCaseList
                      | PTSwitchCaseList4 PTCaseList
  deriving (Show, Eq)

data PTTopStatement = PTTopStatement1 PTStatement
                    | PTTopStatement2 PTFunctionDeclarationStatement
                    | PTTopStatement3 PTClassDeclarationStatement
                    | PTTopStatement4 PTNamespaceName
                    | PTTopStatement5 PTNamespaceName PTTopStatementList
                    | PTTopStatement6 PTTopStatementList
                    | PTTopStatement7 PTUseDeclarations
                    | PTTopStatement8 PTConstantDeclaration
  deriving (Show, Eq)

data PTTopStatementList = PTTopStatementList1 PTTopStatementList PTTopStatement
                        | PTTopStatementList2 
  deriving (Show, Eq)

data PTTraitAdaptationList = PTTraitAdaptationList1 
                           | PTTraitAdaptationList2 PTNonEmptyTraitAdaptationList
  deriving (Show, Eq)

data PTTraitAdaptationStatement = PTTraitAdaptationStatement1 PTTraitPrecedence
                                | PTTraitAdaptationStatement2 PTTraitAlias
  deriving (Show, Eq)

data PTTraitAdaptations = PTTraitAdaptations1 
                        | PTTraitAdaptations2 PTTraitAdaptationList
  deriving (Show, Eq)

data PTTraitAlias = PTTraitAlias1 PTTraitMethodReference PTTraitModifiers PVIdent
                  | PTTraitAlias2 PTTraitMethodReference PTMemberModifier
  deriving (Show, Eq)

data PTTraitList = PTTraitList1 PTFullyQualifiedClassName
                 | PTTraitList2 PTTraitList PTFullyQualifiedClassName
  deriving (Show, Eq)

data PTTraitMethodReference = PTTraitMethodReference1 PVIdent
                            | PTTraitMethodReference2 PTTraitMethodReferenceFullyQualified
  deriving (Show, Eq)

data PTTraitMethodReferenceFullyQualified = PTTraitMethodReferenceFullyQualified1 PTFullyQualifiedClassName PVIdent
  deriving (Show, Eq)

data PTTraitModifiers = PTTraitModifiers1 
                      | PTTraitModifiers2 PTMemberModifier
  deriving (Show, Eq)

data PTTraitPrecedence = PTTraitPrecedence1 PTTraitMethodReferenceFullyQualified PTTraitReferenceList
  deriving (Show, Eq)

data PTTraitReferenceList = PTTraitReferenceList1 PTFullyQualifiedClassName
                          | PTTraitReferenceList2 PTTraitReferenceList PTFullyQualifiedClassName
  deriving (Show, Eq)

data PTTraitUseStatement = PTTraitUseStatement1 PTTraitList PTTraitAdaptations
  deriving (Show, Eq)

data PTUnsetVariable = PTUnsetVariable1 PTVariable
  deriving (Show, Eq)

data PTUnsetVariables = PTUnsetVariables1 PTUnsetVariable
                      | PTUnsetVariables2 PTUnsetVariables PTUnsetVariable
  deriving (Show, Eq)

data PTUntickedClassDeclarationStatement = PTUntickedClassDeclarationStatement1 PTClassEntryType PVIdent PTExtendsFrom PTImplementsList PTClassStatementList
                                         | PTUntickedClassDeclarationStatement2 PTInterfaceEntry PVIdent PTInterfaceExtendsList PTClassStatementList
  deriving (Show, Eq)

data PTUntickedFunctionDeclarationStatement = PTUntickedFunctionDeclarationStatement1 PTFunction PTIsReference PVIdent PTParameterList PTInnerStatementList
  deriving (Show, Eq)

data PTUntickedStatement = PTUntickedStatement1 PTInnerStatementList
                         | PTUntickedStatement10 
                         | PTUntickedStatement11 PTExpr
                         | PTUntickedStatement12 
                         | PTUntickedStatement13 PTExpr
                         | PTUntickedStatement14 PTVariable
                         | PTUntickedStatement15 PTYieldExpr
                         | PTUntickedStatement16 PTGlobalVarList
                         | PTUntickedStatement17 PTStaticVarList
                         | PTUntickedStatement18 PTEchoExprList
                         | PTUntickedStatement19 PVInline
                         | PTUntickedStatement2 PTParenthesisExpr PTStatement PTElseifList PTElseSingle
                         | PTUntickedStatement20 PTExpr
                         | PTUntickedStatement21 PTUnsetVariables
                         | PTUntickedStatement22 PTVariable PTForeachVariable PTForeachOptionalArg PTForeachStatement
                         | PTUntickedStatement23 PTExpr PTForeachVariable PTForeachOptionalArg PTForeachStatement
                         | PTUntickedStatement24 PTDeclareList PTDeclareStatement
                         | PTUntickedStatement25 
                         | PTUntickedStatement26 PTInnerStatementList PTCatchStatement PTFinallyStatement
                         | PTUntickedStatement27 PTExpr
                         | PTUntickedStatement28 PVIdent
                         | PTUntickedStatement3 PTParenthesisExpr PTInnerStatementList PTNewElseifList PTNewElseSingle
                         | PTUntickedStatement4 PTParenthesisExpr PTWhileStatement
                         | PTUntickedStatement5 PTStatement PTParenthesisExpr
                         | PTUntickedStatement6 PTForExpr PTForExpr PTForExpr PTForStatement
                         | PTUntickedStatement7 PTParenthesisExpr PTSwitchCaseList
                         | PTUntickedStatement8 
                         | PTUntickedStatement9 PTExpr
  deriving (Show, Eq)

data PTUseDeclaration = PTUseDeclaration1 PTNamespaceName
                      | PTUseDeclaration2 PTNamespaceName PVIdent
                      | PTUseDeclaration3 PTNamespaceName
                      | PTUseDeclaration4 PTNamespaceName PVIdent
  deriving (Show, Eq)

data PTUseDeclarations = PTUseDeclarations1 PTUseDeclarations PTUseDeclaration
                       | PTUseDeclarations2 PTUseDeclaration
  deriving (Show, Eq)

data PTVariable = PTVariable1 PTBaseVariableWithFunctionCalls PTObjectProperty PTMethodOrNot PTVariableProperties
                | PTVariable2 PTBaseVariableWithFunctionCalls
  deriving (Show, Eq)

data PTVariableClassName = PTVariableClassName1 PTReferenceVariable
  deriving (Show, Eq)

data PTVariableModifiers = PTVariableModifiers1 PTNonEmptyMemberModifiers
                         | PTVariableModifiers2 
  deriving (Show, Eq)

data PTVariableName = PTVariableName1 PVIdent
                    | PTVariableName2 PTExpr
  deriving (Show, Eq)

data PTVariableProperties = PTVariableProperties1 PTVariableProperties PTVariableProperty
                          | PTVariableProperties2 
  deriving (Show, Eq)

data PTVariableProperty = PTVariableProperty1 PTObjectProperty PTMethodOrNot
  deriving (Show, Eq)

data PTVariableWithoutObjects = PTVariableWithoutObjects1 PTReferenceVariable
                              | PTVariableWithoutObjects2 PTSimpleIndirectReference PTReferenceVariable
  deriving (Show, Eq)

data PTWVariable = PTWVariable1 PTVariable
  deriving (Show, Eq)

data PTWhileStatement = PTWhileStatement1 PTStatement
                      | PTWhileStatement2 PTInnerStatementList
  deriving (Show, Eq)

data PTYieldExpr = PTYieldExpr1 PTExpr
                 | PTYieldExpr2 PTVariable
                 | PTYieldExpr3 PTExpr PTExpr
                 | PTYieldExpr4 PTExpr PTVariable
  deriving (Show, Eq)

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
