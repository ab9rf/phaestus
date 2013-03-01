module ParseTree 
where

type PTStart = PTTopStatementList

type PTTopStatementList = [PTTopStatement]

type PTNamespaceName = [PTIdent]

class PTTopStatement

instance PTInnerStatement PTTopStatement

instance PTTopStatement_0 PTTopStatement
data PTTopStatement_0 
        = NamespaceD PTNamespaceName 
        | Namespace PTNamespaceName PTTopStatementList 
        | NamespaceG PTTopStatementList 
        | UseDeclaration PTUseDeclarations
        | ConstantDeclarations PTConstantDeclarations

type PTUseDeclarations = [PTUseDeclaration]
 
data PTUseDeclaration 
        = Use PTNamespaceName
        | UseAs PTNamespaceName PTIdent
        | UseAbsolute PTNamespaceName
        | UseAbsoluteAs PTNamespaceName PTIdent

type PTConstantDeclarations = [PTConstantDeclaration] 

data PTConstantDeclaration = ConstantDeclaration PTIdent PTStaticScalar

type PTInnerStatementList = [PTInnerStatement]

class PTInnerStatement 

instance PTInnerStatement_0 PTInnerStatement
data PTInnerStatement_0 
        = FunctionDeclaration PTFunctionDeclaration
        | ClassDeclaration PTClassDeclaration
        
                
data PTStatement 
        = Label PTIdent
        | StatementGroup PTInnerStatementList
        | If PTParenthesisExpr PTStatement PTElseifList PTELseSingle
        | While PTParenthesisExpr PTWhileStatement
        | Do PTStatement PTParenthesisExprt
        | For PtForExpr PtForExpr PtForExpr PtForStatement
        | Switch PTParenthesisExpr PTSwitchCaseList
        | Break1
        | Break PTExpr
        | Continue1
        | Continue PTExpr
        | ReturnNull
        | Return PTExprWithoutVariable
        | YieldExpr PTYieldExpr
        | Global PTGlobalVarList
        | Static PTStaticVarList
        | Echo PTEchoExprList
        | Inline String
        | Expr PTExpr
        | Unset PTUnsetVariables
        | Foreach PTExpr PTForeachVariable PTForeachOptionalArg PTForeachStatement
        | Declare PTDeclareList PTDeclareStatement
        | Try PTInnerStatementList PTCatchStatement PTFinallyStatement
        | Throw PTExpr
        | Goto PTIdent
        
type PTCatchStatement = [PTCatchStatement_]

data PTCatchStatement_ = Catch PTFQClassName PTVariable PTInnerStatementList

type PTFinallyStatement = (Maybe PTInnerStatementList)

type PTUnsetVariables = [PTUnsetVariable]

type PTUnsetVariable = PTVariable

type PTIsReference = Bool

data PTFunctionDeclarationStatement = FunctionDecl PTIsReference PTIdent PTParameterList PTInnerStatementList

data PTClassDeclarationStatement 
        = ClassDecl PTClassEntryType PTIdent PTExtendsFrom PTImplementsList PTClassStatementList
        | InterfaceDecl PTInterfaceEntry PTIdent PTInterfaceExtendsList PTClassStatementList 

data PTClassEntryType = ClassStandard | ClassAbstract | ClassTrait | ClassFinal

type PTExtendsFrom = Maybe PTFQClassName

type PTInterfaceEntry = ClassInterface

type PTInterfaceExtendsList = [PTFQClassName]

type PTImplementsList = [PTFQClassName]

type PTForeachOptionalArg = Maybe PTForeachVariable

data PTForeachVariable 
        = ForeachVar PTVariable
        | ForeachRef PTVariable
        | ForeachList PTAssignmentList

type PTForStatement = PTStatement
type PTForeachStatement = PTStatement
type PTDeclareStatement = PTStatement

type PTDeclareList = [PTDeclaration]
data PTDeclaration = Declaration PTIdent PTStaticScalar

type PTSwitchCaseList = PTCaseList
type PTCaseList = [PTCase]
data PTCase 
        = Case PTExpr PTInnerStatementList
        | CaseDefault PTInnerStatementList

type PTWhileSTatement = PTStatement

type PTElseifList = [PTElseif]
data PTElseif = ElseIf PTParenthesisExpr PTStatement

type PTElseSingle = Maybe PTStatement

type PTParameterDefList = [PTParameterDef]
data PTParameterDef 
        = ParameterDef PTOptionalClassType PTVariable
        | RefParameterDef PTOptionalClassType PTVariable
        | RefParameterDefWithDefault PTOptionalClassType PTVariable PTStaticScalar
        | ParameterDefWithDefault PTOptionalClassType PTVariable PTStaticScalar

data PTOptionalClassType 
        = TypeUnspecified
        | TypeArray
        | TypeCallable
        | TypeClass PTFQClassName
              
data PTFunctionCallParameterList 
        = ParameterList [PTParameter]
        | YieldParameter PTYieldExpr

data PTParameter
        = Parameter PTExpr
        | RefParameter PTVariable

type PTGlobalVarList = [PTGlobalVar]
data PTGlobalVar 
        = GlobalVar PTVariable
        | IndirectGlobalVar PTExpr
        
type PTStaticVarList = [PTStaticVar]
data PTStaticVar
        = StaticVar PTVariable
        | StaticVarWithInitializer PTVariable PTStaticScalar               
        
type PTClassStatementList = [PTClassStatement]
data PTClassStatement 
        = ClassVariableDeclaration PTVariableModifiers PTClassVariableDeclaration
        | ClassConstantDeclaration PTClassConstantDeclaration
        | TraitUseStatement PTTraitList PTTraitAdaptions
        | MethodDeclaration PTMethodModifiers PTIdent PTIsReference PTParameterList PTMethodBody      

type PTTraitList = [PTFQClassName]

type PTTraitAdaptations = [PTTraitAdaptationStatement]
class PTTraitAdaptationStatement

instance PTTraitPrecedence PTTraitAdaptationStatement    
data PTTraitPrecedence  
        = TraitPrecedence PTTraitMethodReferenceFQ PTTraitReferenceList

type PTTraitReferenceList = [PTFQClassName]

data PTTraitMethodReference 
        = TraitMethodReference PTIdent
        | TraitMethodReferenceFQ PTFQClassName PTIdent    
        
instance PTTraitAlias PTTraitAdaptationStatement
data PTTraitAlias        
        = TraitAliasTrait PTTraitMethodReference PTTraitModifiers PTIdent
        | TraitAliasMember PTTraitMethodReference PTMemberModifier

type PTTraitModifiers = Maybe PTMemberModifier

type PTMethodMody = Maybe PTInnerStatementList

type PTVariableModifiers = [PTMemberModifier]

type PTMethodModifiers = [PTMemberModifier]

data PTMemberModifier 
        = ModifierPublic 
        | ModifierPrivate 
        | ModifierStatic 
        | ModifierAbstract
        | ModifierFinal

type PTClassVariableDeclaration = [PTClassVariableDecl]
data PTClassVariableDecl 
        = ClassVariableDecl PTVariable
        | ClassVariableDeclWithInit PTVariable PTStaticScalar

type PTClassConstantDeclaration = [PTClassConstantDecl]
data PTClassConstantDecl
        = ClassConstantDecl PTIdent PTStaticScalar
        
type PTEchoExprList = [PTExpr]

type PTForExpr = [PTExpr]

type PTChainingMethodOrProperty = [PTVariableProperty]

type PTChainingDereference = [PTDimOffset]

data PTChainingInstanceCall = ChainingInstanceCall (Maybe PTChainingDereference) PTMethodOrProperty           

type PTInstanceCall = Maybe ChainingInstanceCall

data PTNewExpr = New PtClassNameReference PtCtorArguments

class PTExprWithoutVariable

instance PTParenthesisExpr PTExprWithoutVariable
instance PTNewExpr PTExprWithoutVariable
instance PTInternalFunctionsInYacc PTExprWithoutVariable
instance PTScalar PTExprWithoutVariable
instance PTCombinedScalarOffset PTExprWithoutVariable
instance PTCombinedScalar PTExprWithoutVariable
       
instance PTExprWithoutVariable_0 PTExprWithoutVariable
data PTExprWithoutVariable_0 
        = ListAssignment PTAssignmentList PTExpr
        | Assignment PTVariable PTExpr
        | RefAssignment PTVariable PTVariable
        | RefAssignmentNew PTVariable PTClassNameReference PTCtorArguments
        | Clone PTExpr
        | AddInto PTVariable PTExpr
        | SubtractInto PTVariable PTExpr
        | MultiplyInto PTVariable PTExpr
        | DivideInto PTVariable PTExpr
        | ConcatInfo PTVariable PTExpr
        | ModulusInto PTVariable PTExpr
        | AndInto PTVariable PTExpr
        | OrInto PTVariable PTExpr
        | XorInto PTVariable PTExpr
        | ShiftLeftInto PTVariable PTExpr
        | ShiftRightInto PTVariable PTExpr
        | Postincrement PTRWVariable
        | Preincrement PTRWVariable 
        | Postdecrement PTRWVariable
        | Predecrement PTRWVariable
        | BooleanOr PTExpr PTExpr
        | BooleanAnd PTExpr PTExpr
        | LogicalOr PTExpr PTExpr
        | LogicalAnd PTExpr PTExpr
        | LogicalXor PTExpr PTExpr
        | BinaryOr PTExpr PTExpr
        | BinaryAnd PTExpr PTExpr
        | BinaryXor PTExpr PTExpr
        | Concat PTExpr PTExpr
        | Add PTExpr PTExpr
        | Subtract PTExpr PTExpr
        | Multiply PTExpr PTExpr
        | Divide PTExpr PTExpr
        | Modulus PTExpr PTExpr
        | ShiftLeft PTExpr PTExpr
        | ShiftRight PTExpr PTExpr
        | UnaryPlus PTExpr 
        | UnaryMinus PTExpr
        | LogicalNot PTExpr
        | BinaryNegate PTExpr
        | IsIdentical PTExpr PTExpr
        | IsNotIdentical PTExpr PTExpr
        | IsEqual PTExpr PTExpr
        | IsNotEqual PTExpr PTExpr
        | LessThan PTExpr PTExpr
        | LessThanOrEqual PTExpr PTExpr
        | GreaterThan PTExpr PTExpr
        | GreaterThanOrEqual PTExpr PTExpr
        | InstanceOf PTExpr PTClassNameReference
        | NewWithInstanceCAll PTNewExpr PTInstanceCall
        | IfThenElseOp PTExpr PTExpr PTExpr
        | IfElseOp PTExpr PTExpr
        | IntCast PTExpr
        | DoubleCast PTExpr
        | StringCast PTExpr
        | ArrayCast PTExpr
        | ObjectCast PTExpr
        | BoolCast PTExpr
        | UnsetCast PTExpr
        | Exit PTExpr
        | DisableErrors Expr
        | Backtick Expr
        | Print Expr
        | Yield0
        | AnonymousFunction PTIsReference PTParameterList PTLexicalVars PTInnerStatementList
        | AnonymousStaticFunction PTIsReference PTParameterList PTLexicalVars PTInnerStatementList        
        
data PTYieldExpr
        = Yield PTExpr
        | Yield2 PTExpr PTExpr

data PTCombinedScalarOffset = ScalarWithOffset PTSWO [PTDimOffset]

data SWO 
        = SWOArray PTCombinedScalar
        | SWOString PTStringConst
        
type PTCombinedScalar = PTArrayPairList

type PTLexicalVars = [PTLexicalVar]            

data PTLexicalVar 
        = LexicalVariable PTVariable
        | LexicalVariableRef PTVariable        

data PTFunctionCall 
        = FunctionCall PTFQClassName PTFunctionCallParameterList
        | ClassFunctionCall PTClassName PTVariableName PTFunctionCallParameterList
        | VariableClassFunctionCall PTVariableClassName PTVariableName PTFunctionCallParameterList
        | IndirectFunctionCall PTVariableWithoutObjects PTFunctionCallParameterList

class PTClassName
instance PTFQClassName PTClassName

instance PTClassName_0 PTClassName
data PTClassName_0
        = LateStaticBinding
         
data PTFQClassName
        = RelativeNamespace PTNamespaceName
        | SelfNamespace PTNamespaceName
        | AbsoluteNamespace PTNamespaceName
                       
class PTClassNameReference
instance PTClassName PTClassNameReference
instance PTDynamicClassNameReference PTClassNameReference

data PTDynamicClassNameReference
        = DynamicClassName3 PTBaseVariable PTObjectProperty PTDynamicClassNameVariableProperties
        | DynamicClassName1 PTBaseVariable

type PTDynamicClassNameVariableProperties = [PTDynamicClassNameVariableProperty]

type PTDynamicClassNameVariableProperty = [PTObjectProperty]

data PTExitExpr 
        = ExitEmpty
        | ExitAlmostEmpty
        | ExitNotEmpty PTParenthesisExpr

class PTBackTickExpr
instance PTStringConst PTBackTickExpr
instance PTEncapsList PTBackTickExpr

instance PTBackTickExpr_0 PTBackTicksExpr
data PTBackTickExpr_0 = BacktickEmpty

type PTCtorArguments = [PTFunctionCallParameterList]

data PTCommonScalar 
        = Constant PTString
        | MagicLine
        | MagicFile
        | MagicDir
        | MagicTrait
        | MagicMethod
        | MagicFunc
        | MagicNamespace
        
class PTStaticScalar
instance PTCommonScalar PTStaticScalar
instance PTStaticClassConstant PTStaticScalar

instance PTStaticScalar_0 PTStaticScalar
data PTStaticScalar_0
        = StaticUnaryPlus PTStaticScalar
        | StaticUnaryMinus PTStaticScalar
        | StaticArray PTStaticArrayPairList

instance PTStaticScalar_C PTStaticScalar
instance PTStaticScalar_C PTScalar
data PTStaticScalar_C        
        = NamespaceScalar PTFQClassName
        | MagicClass

data PTStaticClassConstant = StaticClassConstant PTClassName PTIdent

class PTScalar
instance PTCommonScalar PTScalar

instance PTScalar_0 PTScalar
data PTScalar_0 
        = Variable PTVariableStr
        | ClassScalar PTClassNameScalar
        | ClassConstant PTClassConstant
        | ScalarString PTEncapsList

type PTStaticArrayPairList = [PTStaticArrayPair]

data PTStaticArrayPair
        = StaticArrayPairKV PTStaticScalar PTStaticScalar
        | StaticArrayPairV PTStaticScalar
        
class PTExpr
instance PTRVariable PTExpr
instance PTExprWithoutVariable PTExpr

class PTParenthesisExpr
instance PTExpr PTParenthesisExpr
instance PTYieldExpr PTParenthesisExpr

type PTRVariable = PTVariable
type PTWVariable = PTVariable
type PTRWVariable = PTVariable

data PTVariable
        = VariableWithArrow PTBaseVariableWithFunctionCalls PTObjectProperty PTMethodOrNot PTVariableProperties
        | VariableWithoutArrow PTBaseVariableWithFunctionCalls

type PTVariableProperties = [PTVariableProperty]

data PTVariableProperty = VariableProperty PTObjectProperty PTMethodOrNot

data PTArrayMethodDereference = ArrayMethodDereference PTMethod [PTDimOffset]

type PTMethod = PTFunctionCallParameterList

data PTMethodOrNot 
        = Method PTMethod
        | ArrayMethod PTArrayMethodDereference
        | NotMethod
        
data PTVariableWithoutObjects 
        = VariableWithoutObjects Int PTReferenceVariable

data PTStaticMember 
        = StaticMember PTClassName PTVariableWithoutObjects
        | StaticMemberDynamic PTVariableClassName PTVariableWithoutObjects

type PTVariableClassName = PTReferenceVariable

data PTArrayFunctionDereference = ArrayFunctionDereference PTFunctionCall [PTDimOffset]

class PTBaseVariableWithFunctionCalls 
instance PTBaseVariable PTBaseVariableWithFunctionCalls
instance PTArrayFunctionDereference PTBaseVariableWithFunctionCalls
instance PTFunctionCall PTBaseVariableWithFunctionCalls

class PTBaseVariable
instance PTStaticMember PTBaseVariable
instance PTBaseVariable_0 PTBaseVariable

data PTBaseVariable_0 = BaseVariable Int PTReferenceVariable

data PTReferenceVariable = ReferenceVariable PTCompoundVariable PTRVOffsetOrIndex

data PTRVOffsetOrIndex = RVOffset PTDimOffset | RVIndex PTExpr


 
