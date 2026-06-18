/*
* Visual Basic 7.1 Grammar for ANTLR4
*
* Derived from the Visual Basic 7.1 language reference
* https://officeprotocoldoc.z19.web.core.windows.net/files/MS-VBAL/%5bMS-VBAL%5d.pdf
* Last compared to Protocol Revision 2.4
*/

// $antlr-format alignTrailingComments true, columnLimit 150, minEmptyLines 1, maxEmptyLinesToKeep 1, reflowComments false, useTab false
// $antlr-format allowShortRulesOnASingleLine false, allowShortBlocksOnASingleLine true, alignSemicolons hanging, alignColons hanging

parser grammar vbaParser;


options {
    caseInsensitive = true;
    tokenVocab = vbaLexer;
}

// Contexts not listed in the specification
// Everything until section 5.1 is typically machine generated code.
startRule
    : module EOF
    ;

// Added form file entry
module
    : endOfLineNoWs* (
          proceduralModule
        | classFileHeader classModule
        | formFileHeader classModule
      ) endOfLine* WS?
    ;

classFileHeader
    : classVersionIdentification classBeginBlock
    ;

classVersionIdentification
    : VERSION WS FLOATLITERAL WS CLASS
    ;

classBeginBlock
    : endOfLine+ BEGIN beginBlockConfigElement+ endOfLine+ END
    ;

beginBlockConfigElement
    : endOfLine+ (OBJECT '.')? '_'? ambiguousIdentifier WS? EQ WS? (('-'? literalExpression) | FILEOFFSET)
    | formBeginBlock
    | beginPropertyBlock
    ;

// Form entries
formFileHeader
    : formVersionIdentification (formObjectAssign)* formBeginBlock
    ;

formVersionIdentification
    : VERSION WS FLOATLITERAL
    ;
formObjectAssign
    : endOfLine+ OBJECT WS? EQ WS? STRINGLITERAL (';' WS? STRINGLITERAL)?
    ;
formBeginBlock
    : endOfLine+ BEGIN WS (GUID | (ambiguousIdentifier '.' ambiguousIdentifier)) WS ambiguousIdentifier beginBlockConfigElement+ endOfLine+ END
    ;
beginPropertyBlock
    : endOfLine+ BEGINPROPERTY WS ambiguousIdentifier (WS GUID WS?)? beginBlockConfigElement+ endOfLine+ ENDPROPERTY
    ;

//---------------------------------------------------------------------------------------
// 4.2 Modules
proceduralModule
    : proceduralModuleHeader endOfLineNoWs* proceduralModuleBody
    ;
classModule
    : classModuleHeader endOfLine* classModuleBody
    ;

// Compare STRINGLITERAL to quoted-identifier
proceduralModuleHeader
    : endOfLine* ATTRIBUTE WS? VB_NAME WS? EQ WS? STRINGLITERAL
    ;
classModuleHeader: (endOfLine+ classAttr)+ WS?;

// VBA Library Projects are allowed to have GoobalNamespace and creatable as true.
classAttr
    : ATTRIBUTE WS? VB_NAME WS? EQ WS? STRINGLITERAL
    | ATTRIBUTE WS? VB_GLOBALNAMESPACE WS? EQ WS? booleanLiteralIdentifier
    | ATTRIBUTE WS? VB_CREATABLE WS? EQ WS? booleanLiteralIdentifier
    | ATTRIBUTE WS? VB_PREDECLAREDID WS? EQ WS? booleanLiteralIdentifier
    | ATTRIBUTE WS? VB_EXPOSED WS? EQ WS? booleanLiteralIdentifier
    | ATTRIBUTE WS? VB_CUSTOMIZABLE WS? EQ WS? booleanLiteralIdentifier
    ;
//---------------------------------------------------------------------------------------
// 5.1 Module Body Structure
// Everything from here down is user generated code.
proceduralModuleBody: proceduralModuleDeclarationSection? endOfLine* proceduralModuleCode;
classModuleBody: classModuleDeclarationSection? classModuleCode;
unrestrictedName
    : reservedIdentifier
    | name
    ;

// Added markedFileNumber to fix a bug
name
    : untypedName
    | typedName
    | markedFileNumber
    ;
untypedName
    : ambiguousIdentifier
    | FOREIGN_NAME
    ;

//---------------------------------------------------------------------------------------
// 5.2 Module Declaration Section Structure
proceduralModuleDeclarationSection
    : (endOfLine+ proceduralModuleDeclarationElement)+
    | ((endOfLine+ proceduralModuleDirectiveElement)* endOfLine+ defDirective) (proceduralModuleDeclarationElement endOfLineNoWs)*
    ;
classModuleDeclarationSection
    : (classModuleDeclarationElement endOfLine+)+
    | ((classModuleDirectiveElement endOfLine+)* defDirective) (classModuleDeclarationElement endOfLine+)*
    ;
proceduralModuleDirectiveElement
    : commonOptionDirective
    | optionPrivateDirective
    | defDirective
    ;
proceduralModuleDeclarationElement
    : commonModuleDeclarationElement
    | globalVariableDeclaration
    | publicConstDeclaration
    | publicExternalProcedureDeclaration
    | globalEnumDeclaration
    | commonOptionDirective
    | optionPrivateDirective
    ;
classModuleDirectiveElement
    : commonOptionDirective
    | defDirective
    | implementsDirective
    ;
classModuleDeclarationElement
    : commonModuleDeclarationElement
    | eventDeclaration
    | commonOptionDirective
    | implementsDirective
    ;

// 5.2.1 Option Directives
commonOptionDirective
    : optionCompareDirective
    | optionBaseDirective
    | optionExplicitDirective
    | remStatement
    ;

// 5.2.1.1 Option Compare Directive
optionCompareDirective: OPTION wsc COMPARE wsc (BINARY | TEXT);

// 5.2.1.2 Option Base Directive
// INTEGER or SHORT?
optionBaseDirective: OPTION wsc BASE wsc INTEGERLITERAL;

// 5.2.1.3 Option Explicit Directive
optionExplicitDirective: OPTION wsc EXPLICIT;

// 5.2.1.4 Option Private Directive
optionPrivateDirective: OPTION wsc PRIVATE wsc MODULE;

// 5.2.2 Implicit Definition Directives
defDirective: defType WS letterSpec (WS ',' WS letterSpec)*;
letterSpec
    : singleLetter
    | universalLetterRange
    | letterRange
    ;
singleLetter: ambiguousIdentifier;
universalLetterRange: upperCaseA WS '-' WS upperCaseZ;
upperCaseA: ambiguousIdentifier;
upperCaseZ: ambiguousIdentifier;
letterRange: firstLetter WS '-' WS lastLetter;
firstLetter: ambiguousIdentifier;
lastLetter: ambiguousIdentifier;
defType
    : DEFBOOL
    | DEFBYTE
    | DEFCUR
    | DEFDATE
    | DEFDBL
    | DEFINT
    | DEFINT
    | DEFLNG
    | DEFLNGLNG
    | DEFLNGPTR
    | DEFOBJ
    | DEFSNG
    | DEFSTR
    | DEFVAR
    ;

// 5.2.3 Module Declarations
// added public-type to fix bug
commonModuleDeclarationElement
    : moduleVariableDeclaration
    | privateConstDeclaration
    | privateTypeDeclaration
    | publicTypeDeclaration
    | privateEnumDeclaration
    | publicEnumDeclaration
    | privateExternalProcedureDeclaration
    ;

// 5.2.3.1 Module Variable Declaration Lists
// Added variableHelpAttribute, not in MS-VBAL
moduleVariableDeclaration
    : publicVariableDecalation
    | privateVariableDeclaration
    | variableHelpAttribute
    ;

variableHelpAttribute
    : ATTRIBUTE WS ambiguousIdentifier '.' VB_VARHELPID WS? '=' WS? '-'? INTEGERLITERAL
    ;
globalVariableDeclaration: GLOBAL WS variableDeclarationList;
publicVariableDecalation: PUBLIC (WS SHARED)? WS moduleVariableDeclarationList;
privateVariableDeclaration: ((PRIVATE | DIM) wsc) (SHARED wsc)? moduleVariableDeclarationList;
moduleVariableDeclarationList: (witheventsVariableDcl | variableDcl) (wsc? ',' wsc? (witheventsVariableDcl | variableDcl))*;
variableDeclarationList: variableDcl (wsc? ',' wsc? variableDcl)*;

// 5.2.3.1.1 Variable Declarations
variableDcl
    : typedVariableDcl
    | untypedVariableDcl
    ;
typedVariableDcl: typedName wsc? arrayDim?;
untypedVariableDcl: ambiguousIdentifier wsc? (arrayClause | asClause)?;
arrayClause: arrayDim (wsc asClause)?;
asClause
    : asAutoObject
    | asType
    ;

// 5.2.3.1.2 WithEvents Variable Declarations
witheventsVariableDcl: WITHEVENTS wsc ambiguousIdentifier wsc AS wsc? classTypeName;
classTypeName: definedTypeExpression;

// 5.2.3.1.3 Array Dimensions and Bounds
arrayDim: '(' wsc? boundsList? wsc? ')';
boundsList: dimSpec (wsc? ',' wsc? dimSpec)*;
dimSpec: lowerBound? wsc? upperBound;
lowerBound: constantExpression wsc TO wsc;
upperBound: constantExpression;

// 5.2.3.1.4 Variable Type Declarations
asAutoObject: AS WS NEW WS classTypeName;
asType: AS WS typeSpec;
typeSpec
    : fixedLengthStringSpec
    | typeExpression
    ;
fixedLengthStringSpec: STRING WS '*' WS stringLength;
stringLength
    : INTEGERLITERAL
    | constantName
    ;
constantName: simpleNameExpression;

// 5.2.3.2 Const Declarations
publicConstDeclaration: (GLOBAL | PUBLIC) wsc moduleConstDeclaration;
privateConstDeclaration: (PRIVATE wsc)? moduleConstDeclaration;
moduleConstDeclaration: constDeclaration;
constDeclaration: CONST wsc constItemList;
constItemList: constItem (wsc? ',' wsc? constItem)*;
constItem
    : typedNameConstItem
    | untypedNameConstItem
    ;
typedNameConstItem: typedName wsc? EQ wsc? constantExpression;
untypedNameConstItem: ambiguousIdentifier (wsc constAsClause)? wsc? EQ wsc? constantExpression;
constAsClause: AS wsc builtinType;

// 5.2.3.3 User Defined Type Declarations
publicTypeDeclaration: ((GLOBAL | PUBLIC) wsc)? udtDeclaration;
privateTypeDeclaration: PRIVATE wsc udtDeclaration;
udtDeclaration: TYPE wsc untypedName endOfStatement+ udtMemberList endOfStatement+ END wsc TYPE;
udtMemberList: udtElement (endOfStatement udtElement)*;
udtElement
    : remStatement
    | udtMember
    ;
udtMember
    : reservedNameMemberDcl
    | untypedNameMemberDcl
    ;
untypedNameMemberDcl: ambiguousIdentifier optionalArrayClause;
reservedNameMemberDcl: reservedMemberName wsc asClause;
optionalArrayClause: arrayDim? wsc asClause;
reservedMemberName
    : statementKeyword
    | markerKeyword
    | operatorIdentifier
    | specialForm
    | reservedName
    | literalIdentifier
    | reservedForImplementationUse
    | futureReserved
    ;

// 5.2.3.4 Enum Declarations
globalEnumDeclaration: GLOBAL wsc  enumDeclaration;
publicEnumDeclaration: (PUBLIC wsc)? enumDeclaration;
privateEnumDeclaration: PRIVATE wsc enumDeclaration;
enumDeclaration: ENUM wsc untypedName endOfStatement+ enumMemberList endOfStatement+ END wsc ENUM ;
enumMemberList: enumElement (endOfStatement enumElement)*;
enumElement
    : remStatement
    | enumMember
    ;
enumMember: untypedName (wsc? EQ wsc? constantExpression)?;

// 5.2.3.5 External Procedure Declaration
publicExternalProcedureDeclaration: (PUBLIC wsc)? externalProcDcl;
privateExternalProcedureDeclaration: PRIVATE wsc externalProcDcl;
externalProcDcl: DECLARE wsc (PTRSAFE wsc)? (externalSub | externalFunction);
externalSub: SUB wsc subroutineName wsc libInfo (wsc procedureParameters)?;
externalFunction: FUNCTION wsc functionName wsc libInfo (wsc procedureParameters)? (wsc functionType)?;
libInfo: libClause (wsc aliasClause)?;
libClause: LIB wsc STRINGLITERAL;
aliasClause: ALIAS wsc STRINGLITERAL;

// 5.2.4 Class Module Declarations
// 5.2.4.2 Implements Directive
implementsDirective: IMPLEMENTS WS classTypeName;

// 5.2.4.3 Event Declaration
eventDeclaration: PUBLIC? wsc EVENT wsc ambiguousIdentifier eventParameterList?;
eventParameterList: '(' wsc? positionalParameters? wsc? ')';


//---------------------------------------------------------------------------------------
// 5.3 Module Code Section Structure
// removed an EOS
proceduralModuleCode: (proceduralModuleCodeElement endOfLine*)*;
classModuleCode: (classModuleCodeElement endOfLine*)*;
proceduralModuleCodeElement: commonModuleCodeElement;
classModuleCodeElement
    : commonModuleCodeElement
    | implementsDirective
    ;

// Added AttributeStatement.
commonModuleCodeElement
    : remStatement
    | procedureDeclaration
    | attributeStatement
    ;
procedureDeclaration
    : subroutineDeclaration
    | functionDeclaration
    | propertyGetDeclaration
    | propertyLhsDeclaration
    ;

// 5.3.1 Procedure Declarations
// Allow a static keyword before or after, but not both
subroutineDeclaration
    : (procedureScope wsc)? (
              ((initialStatic wsc)? SUB wsc subroutineName (wsc? procedureParameters?))
            | (SUB wsc subroutineName (wsc? procedureParameters)? wsc? trailingStatic)
        )
        procedureBody?
        endLabel? endOfStatement+ END wsc SUB procedureTail?;
functionDeclaration
    : (procedureScope wsc)? (
              (initialStatic wsc)? FUNCTION wsc functionName (wsc? procedureParameters)? (wsc? functionType)?
            | FUNCTION wsc functionName (wsc? procedureParameters)? (wsc? functionType)? wsc? trailingStatic)
        procedureBody?
        endLabel? endOfStatement+ END wsc FUNCTION procedureTail?;
  
propertyGetDeclaration
    : (procedureScope wsc)? (
              (initialStatic wsc)? PROPERTY wsc GET wsc functionName (wsc? procedureParameters)? (wsc? functionType)?
            | PROPERTY wsc GET wsc functionName procedureParameters? (wsc? functionType)? wsc? trailingStatic)
        procedureBody?
        endLabel? endOfStatement+ END wsc PROPERTY procedureTail?;
  
propertyLhsDeclaration
    : (procedureScope wsc)? (
              (initialStatic wsc)? PROPERTY wsc (LET | SET) wsc subroutineName wsc? propertyParameters
            | PROPERTY wsc (LET | SET) wsc subroutineName propertyParameters wsc? trailingStatic)
        procedureBody?
        endLabel? endOfStatement+ END wsc PROPERTY procedureTail?;
endLabel: endOfStatement* endOfLineNoWs statementLabelDefinition;
procedureTail
    : wsc? NEWLINE
    | wsc? commentBody
    | WS? ':' WS? remStatement
    ;

// 5.3.1.1 Procedure Scope
procedureScope
    : PRIVATE
    | PUBLIC
    | FRIEND
    | GLOBAL
    ;

// 5.3.1.2 Static Procedures
initialStatic: STATIC;
trailingStatic: STATIC;

// 5.3.1.3 Procedure Names
subroutineName
    : ambiguousIdentifier
    | prefixedName
    ;
functionName
    : typedName
    | ambiguousIdentifier
    | prefixedName 
    ;
prefixedName
    : eventHandlerName
    | implementedName
    | lifecycleHandlerName
    ;

// 5.3.1.4 Function Type Declarations
functionType: AS wsc typeExpression wsc? arrayDesignator?;
arrayDesignator: '(' wsc? ')';

// 5.3.1.5 Parameter Lists
procedureParameters: '(' wsc? parameterList? wsc? ')';
propertyParameters: '(' wsc? (parameterList wsc? ',' wsc?)? valueParam wsc? ')';
parameterList
    : (positionalParameters wsc? ',' wsc? optionalParameters)
    | (positionalParameters  (wsc? ',' wsc? paramArray)?)
    | optionalParameters
    | paramArray
    ;
  
positionalParameters: positionalParam (wsc? ',' wsc? positionalParam)*;
optionalParameters: optionalParam (wsc? ',' wsc? optionalParam)*;
valueParam: positionalParam;
positionalParam: (parameterMechanism wsc)? paramDcl;
optionalParam
    : optionalPrefix wsc paramDcl wsc? defaultValue?;
paramArray
    : PARAMARRAY wsc ambiguousIdentifier '(' wsc? ')' (wsc AS wsc (VARIANT | '[' VARIANT ']'))?;
paramDcl
    : untypedNameParamDcl
    | typedNameParamDcl
    ;
untypedNameParamDcl: ambiguousIdentifier parameterType?;
typedNameParamDcl: typedName arrayDesignator?;
optionalPrefix
    : OPTIONAL (wsc parameterMechanism)?
    | parameterMechanism wsc OPTIONAL
    ;
parameterMechanism
    : BYVAL
    | BYREF
    ;
parameterType: arrayDesignator? wsc AS wsc (typeExpression | ANY);
defaultValue: '=' wsc? constantExpression;

// 5.3.1.8 Event Handler Declarations
eventHandlerName: ambiguousIdentifier;

// 5.3.1.9 Implemented Name Declarations
implementedName: ambiguousIdentifier;

// 5.3.1.10 Lifecycle Handler Declarations
lifecycleHandlerName
    : CLASS_INITIALIZE
    | CLASS_TERMINATE
    ;

//---------------------------------------------------------------------------------------
// 5.4 Procedure Bodies and Statements
procedureBody: statementBlock;

// 5.4.1 Statement Blocks
// spec used *, changed to + changed all parent to call with ? to avoid empty context.
// Made EOS optional to be able to force EOL before ifStatement elements.
statementBlock
    : blockStatement+
    ;
blockStatement
    : endOfStatement* endOfLineNoWs statementLabelDefinition
    | endOfStatement+ remStatement
    | statement
    | endOfStatement* endOfLineNoWs attributeStatement
    ;
statement
    : endOfStatement+ fileStatement
    | controlStatement
    | endOfStatement+ dataManipulationStatement
    | endOfStatement+ errorHandlingStatement
    ;
    
// 5.4.1.1  Statement Labels
statementLabelDefinition
    : identifierStatementLabel ':'
    | lineNumberLabel ':'?
    ;
statementLabel
    : identifierStatementLabel
    | lineNumberLabel
    ; 
statementLabelList: statementLabel (wsc? ',' wsc? statementLabel)?;
identifierStatementLabel: ambiguousIdentifier;
lineNumberLabel: INTEGERLITERAL;

// 5.4.1.2 Rem Statement
// We have a token for this
remStatement: REMCOMMENT;

// 5.4.2 Control Statements
controlStatement
    : endOfStatement* endOfLine+ ifStatement
    | endOfStatement+ controlStatementExceptMultilineIf
    ;
controlStatementExceptMultilineIf
    : assertStatement
    | callStatement
    | whileStatement
    | forStatement
    | exitForStatement
    | doStatement
    | exitDoStatement
    | singleLineIfStatement
    | selectCaseStatement
    | stopStatement
    | gotoStatement
    | onGotoStatement
    | gosubStatement
    | returnStatement
    | onGosubStatement
    | forEachStatement
    | exitSubStatement
    | exitFunctionStatement
    | exitPropertyStatement
    | raiseeventStatement
    | withStatement
    | endStatement
    ;

// 5.4.2.1 Call Statement
callStatement
    : CALL wsc (simpleNameExpression
        | memberAccessExpression
        | indexExpression
        | withExpression)
    | (simpleNameExpression
        | memberAccessExpression
        | withExpression) (wsc argumentList)?
    ;

// 5.4.2.2 While Statement
whileStatement
    : WHILE wsc booleanExpression
        statementBlock?  endOfStatement+ WEND;

// 5.4.2.3 For Statement
forStatement
    : simpleForStatement
    | explicitForStatement
    ;
simpleForStatement: forClause statementBlock? endOfStatement+ NEXT;
explicitForStatement
    : forClause statementBlock? endOfStatement+ (NEXT | (nestedForStatement wsc? ',')) wsc boundVariableExpression;
nestedForStatement
    : explicitForStatement
    | explicitForEachStatement
    ;
forClause
    : FOR wsc boundVariableExpression wsc? EQ wsc? startValue wsc TO wsc endValue (wsc stepClause)?;
startValue: expression;
endValue: expression;
stepClause: STEP wsc stepIncrement;
stepIncrement: expression;

// 5.4.2.4 For Each Statement
forEachStatement
    : simpleForEachStatement
    | explicitForEachStatement
    ;
simpleForEachStatement
    : forEachClause statementBlock? endOfStatement+ NEXT;
  
explicitForEachStatement
    : forEachClause statementBlock? 
  endOfStatement (NEXT | (nestedForStatement wsc? ',')) wsc boundVariableExpression;
 forEachClause: FOR wsc EACH wsc? boundVariableExpression wsc? IN wsc? collection;
 collection: expression;

// 5.4.2.5 Exit For Statement
exitForStatement: EXIT wsc FOR;

// 5.4.2.6 Do Statement
doStatement
    : DO (wsc? conditionClause)? statementBlock? endOfStatement+
        LOOP (wsc? conditionClause)?;
conditionClause
    : whileClause
    | untilClause
    ;
whileClause: WHILE wsc? booleanExpression;
untilClause: UNTIL wsc? booleanExpression;

// 5.4.2.7 Exit Do Statement
exitDoStatement: EXIT wsc DO;

// 5.4.2.8 If Statement
// why is a LINE-START required before this?
ifStatement
    : IF wsc? booleanExpression wsc? THEN
        statementBlock?
    elseIfBlock*
    elseBlock? endOfStatement+
    ((END wsc IF) | ENDIF);
// Need to verify why some of the end-of-line / line-start things are set the way they are.
elseIfBlock
    : endOfStatement* endOfLine ELSEIF wsc? booleanExpression wsc? THEN endOfLine?
        statementBlock?
    | endOfStatement* ELSEIF wsc? booleanExpression wsc? THEN statementBlock?
    ;
elseBlock: endOfLine+ ELSE endOfLine? wsc? statementBlock?;

// 5.4.2.9 Single-line If Statement
singleLineIfStatement
    : ifWithNonEmptyThen
    | ifWithEmptyThen
    ;
ifWithNonEmptyThen
    : IF wsc booleanExpression wsc THEN wsc? listOrLabel (wsc singleLineElseClause)?;
ifWithEmptyThen
    : IF wsc booleanExpression wsc THEN wsc singleLineElseClause;
singleLineElseClause: ELSE wsc? listOrLabel?;
listOrLabel
    : (statementLabel (':' wsc? sameLineStatement?)*)
    | ':'? wsc? sameLineStatement (wsc? ':' wsc? sameLineStatement?)*
    ;
sameLineStatement
    : fileStatement
    | errorHandlingStatement
    | dataManipulationStatement
    | controlStatementExceptMultilineIf
    ;

// 5.4.2.10 Select Case Statement
selectCaseStatement
    : SELECT wsc CASE wsc selectExpression
        caseClause*
        caseElseClause?
    endOfStatement+ END wsc SELECT;
caseClause: endOfStatement+ CASE wsc? rangeClause (wsc? ',' wsc? rangeClause)* statementBlock?;
caseElseClause: endOfStatement+ CASE wsc ELSE statementBlock?;
rangeClause
    : expression
    | startValue wsc? TO wsc? endValue
    | IS? wsc comparisonOperator wsc? expression;
selectExpression: expression;
comparisonOperator
    : EQ
    | NEW
    | LT
    | GT
    | LEQ
    | GEQ
    ;

// 5.4.2.11 Stop Statement
stopStatement: STOP;

// 5.4.2.12 GoTo Statement
gotoStatement: (GO wsc TO | GOTO) wsc statementLabel;

// 5.4.2.13 On…GoTo Statement
onGotoStatement: ON wsc? expression GOTO wsc statementLabelList;

// 5.4.2.14 GoSub Statement
gosubStatement: ((GO wsc SUB) | GOSUB) wsc statementLabel;

// 5.4.2.15 Return Statement
returnStatement: RETURN;

// 5.4.2.16 On…GoSub Statement
onGosubStatement: ON wsc? expression wsc? GOSUB wsc statementLabelList;

// 5.4.2.17 Exit Sub Statement
exitSubStatement: EXIT wsc SUB;

// 5.4.2.18 Exit Function Statement
exitFunctionStatement: EXIT wsc FUNCTION;

// 5.4.2.19 Exit Property Statement
exitPropertyStatement: EXIT wsc PROPERTY;

// 5.4.2.20 RaiseEvent Statement
raiseeventStatement
    : RAISEEVENT wsc? ambiguousIdentifier wsc? ('(' wsc? eventArgumentList wsc? ')')?;
eventArgumentList: (eventArgument (wsc? ',' wsc? eventArgument)*)?;
eventArgument: expression;

// 5.4.2.21 With Statement
withStatement: WITH wsc? expression statementBlock? endOfStatement+ END wsc WITH;

// 5.4.2.22 End Statement
endStatement
    : END
    ;

// 5.4.2.22 Assert Statement
assertStatement
    : DEBUG '.' ASSERT wsc booleanExpression
    ;

// 5.4.3 Data Manipulation Statements
// Added eraseStatement. It is missing from the list in MsS-VBAL 1.7
dataManipulationStatement
    : localVariableDeclaration
    | staticVariableDeclaration
    | localConstDeclaration
    | redimStatement
    | eraseStatement
    | midStatement
    | rsetStatement
    | lsetStatement
    | letStatement
    | setStatement
    ;

// 5.4.3.1 Local Variable Declarations
localVariableDeclaration: DIM wsc? SHARED? wsc? variableDeclarationList;
staticVariableDeclaration: STATIC wsc variableDeclarationList;

// 5.4.3.2 Local Constant Declarations
localConstDeclaration: constDeclaration;

// 5.4.3.3 ReDim Statement
redimStatement: REDIM (wsc PRESERVE)? wsc? redimDeclarationList;
redimDeclarationList: redimVariableDcl (wsc? ',' wsc? redimVariableDcl)*;
// Had to add withExpression and memberAccess
// to match callStatement.
redimVariableDcl
    : redimTypedVariableDcl
    | redimUntypedDcl
    | withExpressionDcl
    | memberAccessExpressionDcl
    ;
redimTypedVariableDcl: typedName wsc? dynamicArrayDim;
redimUntypedDcl: untypedName wsc? dynamicArrayClause;
withExpressionDcl: withExpression wsc? dynamicArrayDim;
memberAccessExpressionDcl: memberAccessExpression wsc? dynamicArrayDim;
dynamicArrayDim: '(' wsc? dynamicBoundsList wsc? ')';
dynamicBoundsList: dynamicDimSpec (wsc? ',' wsc? dynamicDimSpec)*;
dynamicDimSpec: (dynamicLowerBound wsc)? dynamicUpperBound;
dynamicLowerBound: integerExpression wsc? TO;
dynamicUpperBound: integerExpression;
dynamicArrayClause: dynamicArrayDim wsc? asClause?;

// 5.4.3.4 Erase Statement
eraseStatement: ERASE wsc? eraseList;
eraseList: eraseElement (wsc? ',' wsc? eraseElement)*;
eraseElement: lExpression;

// 5.4.3.5 Mid/MidB/Mid$/MidB$ Statement
midStatement: modeSpecifier wsc? '(' wsc? stringArgument wsc? ',' wsc? startMid wsc? (',' wsc? length)? ')' wsc? EQ wsc? expression;
modeSpecifier
    : MID
    | MIDB
    | MID_D
    | MIDB_D
    ;
stringArgument: boundVariableExpression;
// Changed name from start to startMid due to a problem with the Dart compilation.
startMid: integerExpression;
length: integerExpression;

// 5.4.3.6 LSet Statement
lsetStatement: LSET wsc? boundVariableExpression wsc? EQ wsc? expression;

// 5.4.3.7 RSet Statement
rsetStatement: RSET wsc? boundVariableExpression wsc? EQ wsc? expression;

// 5.4.3.8 Let Statement
letStatement: (LET wsc)? lExpression wsc? EQ wsc? expression;

// 5.4.3.9 Set Statement
setStatement: SET wsc lExpression wsc? EQ wsc? expression;

// 5.4.4 Error Handling Statements
errorHandlingStatement
    : onErrorStatement
    | resumeStatement
    | errorStatement
    ;

// 5.4.4.1 On Error Statement
onErrorStatement: ON wsc ERROR wsc? errorBehavior;
errorBehavior
    : RESUME wsc NEXT
    | GOTO wsc? statementLabel
    ;

// 5.4.4.2 Resume Statement
resumeStatement: RESUME wsc? (NEXT| statementLabel)?;

// 5.4.4.3 Error Statement
errorStatement: ERROR wsc errorNumber;
errorNumber: integerExpression;

// 5.4.5 File Statements
fileStatement
    : openStatement
    | closeStatement
    | seekStatement
    | lockStatement
    | unlockStatement
    | lineInputStatement
    | widthStatement
    | printStatement
    | writeStatement
    | inputStatement
    | putStatement
    | getStatement
    ;

// 5.4.5.1 Open Statement
openStatement
    : OPEN wsc? pathName wsc? modeClause? wsc accessClause? wsc? lock? wsc? AS wsc? fileNumber wsc? lenClause?
    ;
pathName: expression;
modeClause: FOR wsc modeOpt;
modeOpt
    : APPEND
    | BINARY
    | INPUT
    | OUTPUT
    | RANDOM
    ;
accessClause: ACCESS wsc access;
access
    : READ
    | WRITE
    | READ wsc WRITE
    ;
lock
    : SHARED
    | LOCK wsc READ
    | LOCK wsc WRITE
    | LOCK wsc READ wsc WRITE
    ;
lenClause: LEN wsc EQ wsc recLength;
recLength: expression;

// 5.4.5.1.1 File Numbers
fileNumber
    : markedFileNumber
    | unmarkedFileNumber
    ;
markedFileNumber: '#' expression;
unmarkedFileNumber: expression;

// 5.4.5.2 Close and Reset Statements
closeStatement
    : RESET
    | CLOSE wsc? fileNumberList?
    ;
fileNumberList: fileNumber (wsc? ',' wsc? fileNumber)*;

// 5.4.5.3 Seek Statement
seekStatement: SEEK wsc fileNumber wsc? ',' wsc? position;
position: expression;

// 5.4.5.4 Lock Statement
lockStatement: LOCK wsc fileNumber (wsc? ',' wsc? recordRange);
recordRange
    : startRecordNumber
    | startRecordNumber? wsc TO wsc endRecordNumber
    ;
startRecordNumber: expression;
endRecordNumber: expression;

// 5.4.5.5 Unlock Statement
unlockStatement: UNLOCK wsc fileNumber (wsc? ',' wsc? recordRange)?;

// 5.4.5.6 Line Input Statement
lineInputStatement: LINE wsc INPUT wsc markedFileNumber wsc? ',' wsc? variableName;
variableName: variableExpression;

// 5.4.5.7 Width Statement
widthStatement: WIDTH wsc markedFileNumber wsc? ',' wsc? lineWidth;
lineWidth: expression;

// 5.4.5.8 Print Statement
printStatement: ((DEBUG | ME) '.')? PRINT wsc (markedFileNumber wsc? ',' wsc?)? outputList?;

// 5.4.5.8.1 Output Lists
outputList: outputItem+;
outputItem
    : outputClause charPosition?
    | charPosition;
outputClause: spcClause | tabClause| outputExpression;
charPosition: ';' | ',' | wsc;
outputExpression: expression;
spcClause: SPC wsc '(' wsc? spcNumber wsc? ')';
spcNumber: expression;
tabClause: TAB wsc '(' wsc? tabNumber wsc? ')';
tabNumber: expression;

// 5.4.5.9 Write Statement
writeStatement: WRITE wsc markedFileNumber wsc? ',' wsc? outputList?;

// 5.4.5.10 Input Statement
inputStatement: INPUT wsc markedFileNumber wsc? ',' wsc? inputList;
inputList: inputVariable (wsc? ',' wsc? inputVariable)*;
inputVariable: boundVariableExpression;

// 5.4.5.11 Put Statement
putStatement: PUT wsc fileNumber wsc? ',' wsc? recordNumber? wsc? ',' wsc? data;
recordNumber: expression;
data: expression;

// 5.4.5.12 Get Statement
getStatement: GET wsc fileNumber wsc? ',' wsc? recordNumber? wsc? ',' wsc? variable;
variable: variableExpression;

// Attribute Statement
attributeStatement
    : ATTRIBUTE WS ambiguousIdentifier '.' attributeDescName WS? EQ WS? STRINGLITERAL
    | ATTRIBUTE WS ambiguousIdentifier '.' attributeUsrName WS? EQ WS? '-'? INTEGERLITERAL
    | ATTRIBUTE WS ambiguousIdentifier '.' VB_PROCDATA '.' VB_INVOKE_FUNC WS EQ WS STRINGLITERAL
    ;

attributeDescName
    : VB_DESCRIPTION
    | VB_VARDESCRIPTION
    | VB_MEMBERFLAGS
    | VB_VARMEMBERFLAGS
    ;

attributeUsrName
    : 'VB_USERMEMID'
    | 'VB_VARUSERMEMID'
    ;

//---------------------------------------------------------------------------------------
// 5.6  Expressions
// Modifying the order will affect the order of operations
// valueExpression must be rolled up into expression due to mutual left recursion
// operatorExpression must be rolled up into expression due to mutual left recursion
// memberAccess
// DictionaryAccess
// The name 'newExpression' fails under the Go language
expression
    : literalExpression                                                            # LiteralExpress
    | parenthesizedExpression                                                      # ParenthesizedExpress
    | typeofIsExpression                                                           # TypeofIsExpress
    | NEW wsc? expression                                                          # NewExpress
    | expression wsc? POW wsc? expression                                          # ArithmeticExpression
    | MINUS wsc? expression                                                        # UnaryMinusExpression
    | expression wsc? (DIV | MULT) wsc? expression                                 # ArithmeticExpression
    | expression wsc? MOD wsc? expression                                          # ArithmeticExpression
    | expression wsc? (PLUS | MINUS) wsc? expression                               # ArithmeticExpression
    | expression wsc? AMPERSAND wsc? expression                                    # ConcatExpression
    | expression wsc? (IS | LIKE | GEQ | LEQ | GT | LT | NEQ | EQ) wsc? expression # RelationExpression
    | NOT wsc? expression                                                          # NotOperatorExpression
    | expression wsc? (AND | OR | XOR | EQV | IMP) wsc? expression                 # BooleanExpress
    | lExpression                                                                  # LExpress
    ;

// Several of the lExpression rules are rolled up due to Mutual Left Recursion
// Many are also listed separately due to their specific use elsewhere.
lExpression
    : simpleNameExpression                                                                     #simpleNameExpress
    | ME                                                                                       #instanceExpression
    | lExpression '.' wsc? unrestrictedName                                                    #memberAccessExpress
    | lExpression wsc? LINE_CONTINUATION wsc?'.' wsc? unrestrictedName                         #memberAccessExpress
    | lExpression wsc? '(' wsc? argumentList wsc? ')'                                          #indexExpress
    | lExpression  '!' unrestrictedName                                                        #dictionaryAccessExpress
    | lExpression wsc? LINE_CONTINUATION wsc? '!' unrestrictedName                             #dictionaryAccessExpress
    | lExpression wsc? LINE_CONTINUATION wsc? '!' wsc? LINE_CONTINUATION wsc? unrestrictedName #dictionaryAccessExpress
    | withExpression                                                                           #withExpress
    ;
    
// 5.6.5 Literal Expressions
// check on hex and oct
// check definition of integer and float
literalExpression
    : DATELITERAL
    | FLOATLITERAL
    | INTEGERLITERAL
    | STRINGLITERAL
    | literalIdentifier typeSuffix?
    ;

// 5.6.6 Parenthesized Expressions
parenthesizedExpression: LPAREN wsc? expression wsc? RPAREN;

// 5.6.7 TypeOf…Is Expressions
typeofIsExpression: TYPEOF wsc? expression wsc? IS wsc? typeExpression;

// 5.6.10 Simple Name Expressions
// Had to add reservedName and specialForm to allow calls to Abs() Debug. and Lbound()
simpleNameExpression
    : name
    | reservedName
    | specialForm
;

// 5.6.12  Member Access Expressions
// This expression is also rolled into lExpression.
// Changes here must be duplicated there
memberAccessExpression
    : lExpression '.' wsc? unrestrictedName
    | lExpression wsc? LINE_CONTINUATION wsc?'.' wsc? unrestrictedName
    ;

// 5.6.13 Index Expressions
// This expression is also rolled into lExpression.
// Changes here must be duplicated there
indexExpression
    : lExpression wsc? '(' wsc? argumentList wsc? ')'
    ;

// 5.6.13.1 Argument Lists
argumentList: positionalOrNamedArgumentList?;
positionalOrNamedArgumentList
    : (positionalArgument wsc? ',' wsc?)* requiredPositionalArgument
    | (positionalArgument wsc? ',' wsc?)* namedArgumentList
    ;
positionalArgument: argumentExpression?;
requiredPositionalArgument: argumentExpression;
namedArgumentList: namedArgument (wsc? ',' wsc? namedArgument)*;
namedArgument: unrestrictedName wsc? ASSIGN wsc? argumentExpression;
argumentExpression
    : (BYVAL wsc)? expression
    | addressofExpression
    ;

// 5.6.14 Dictionary Access Expressions
// This expression is also rolled into lExpression.
// Changes here must be duplicated there
dictionaryAccessExpression
    : lExpression  '!' unrestrictedName
    | lExpression wsc? LINE_CONTINUATION wsc? '!' unrestrictedName
    | lExpression wsc? LINE_CONTINUATION wsc? '!' wsc? LINE_CONTINUATION wsc? unrestrictedName
    ;

// 5.6.15 With Expressions
withExpression
    : withMemberAccessExpression
    | withDictionaryAccessExpression
    ;
withMemberAccessExpression: '.' unrestrictedName;
withDictionaryAccessExpression: '!' unrestrictedName;

// 5.6.16 Constrained Expressions
// The following Expressions have complex static requirements

// 5.6.16.1 Constant Expressions
constantExpression: expression;

// 5.6.16.2 Conditional Compilation Expressions
ccExpression: expression;

// 5.6.16.3 Boolean Expressions
booleanExpression: expression;

// 5.6.16.4 Integer Expressions
integerExpression: expression;

// 5.6.16.5
variableExpression: lExpression;

// 5.6.16.6
boundVariableExpression: lExpression;

// 5.6.16.7
typeExpression
    : builtinType
    | definedTypeExpression
    ;
definedTypeExpression
    : simpleNameExpression
    | memberAccessExpression
    ;

// 5.6.16.8
addressofExpression
    : ADDRESSOF wsc procedurePointerExpression
    ;
procedurePointerExpression
    : simpleNameExpression
    | memberAccessExpression
    ;

//---------------------------------------------------------------------------------------
// Many of the following are labeled as tokens in the standard, but are parser rules here.
// 3.3.1 Separator and Special Tokens
// In theory whitespace should be ignored, but there are a handful of cases
// where statements MUST be at the beginning of a line or where a NO-WS
// rule appears in the parser rule.
// If may make things simpler here to send all wsc to the hidden channel
// and let a linting tool highlight the couple cases where whitespace
// will cause an error.
wsc: (WS | LINE_CONTINUATION)+;
// known as EOL in MS-VBAL
endOfLine
    : wsc? (NEWLINE | commentBody | remStatement) wsc?
    ;
// We usually don't care if a line of code begins with whitespace, and the parser rules are
// cleaner if we lump that in with the EOL or EOS "token". However, for those cases where
// something MUST occur on the start of a line, use endOfLineNoWs.
endOfLineNoWs
    : wsc? (NEWLINE | commentBody | remStatement)
    ;
// known as EOS in MS-VBAL
endOfStatement
    : (endOfLine | wsc? COLON wsc?)+
    ;
endOfStatementNoWs
    : (endOfLineNoWs | wsc? COLON)+
    ;
// The COMMENT token includes the leading single quote
commentBody: COMMENT;

// 3.3.5.2 Reserved Identifiers and IDENTIFIER
reservedIdentifier
    : statementKeyword
    | markerKeyword
    | operatorIdentifier
    | specialForm
    | reservedName
    | reservedTypeIdentifier
    | literalIdentifier
    | remKeyword
    | reservedForImplementationUse
    | futureReserved
    ;
// Known as IDENTIFIER in MS-VBAL
ambiguousIdentifier
    : IDENTIFIER
    | ambiguousKeyword
    ;
statementKeyword
    : CALL
    | CASE
    | CLOSE
    | CONST
    | DECLARE
    | DEFBOOL
    | DEFBYTE
    | DEFCUR
    | DEFDATE
    | DEFDBL
    | DEFINT
    | DEFLNG
    | DEFLNGLNG
    | DEFLNGPTR
    | DEFOBJ
    | DEFSNG
    | DEFSTR
    | DEFVAR
    | DIM
    | DO
    | ELSE
    | ELSEIF
    | END
    | ENDIF
    | ENUM
    | ERASE
    | EVENT
    | EXIT
    | FOR
    | FRIEND
    | FUNCTION
    | GET
    | GLOBAL
    | GOSUB
    | GOTO
    | IF
    | IMPLEMENTS
    | INPUT
    | LET
    | LOCK
    | LOOP
    | LSET
    | NEXT
    | ON
    | OPEN
    | OPTION
    | PRINT
    | PRIVATE
    | PUBLIC
    | PUT
    | RAISEEVENT
    | REDIM
    | RESUME
    | RETURN
    | RSET
    | SEEK
    | SELECT
    | SET
    | STATIC
    | STOP
    | SUB
    | TYPE
    | UNLOCK
    | WEND
    | WHILE
    | WITH
    | WRITE
    ;
remKeyword: REM;
markerKeyword
    : ANY
    | AS
    | BYREF
    | BYVAL 
    | CASE
    | EACH
    | ELSE
    | IN 
    | NEW
    | SHARED
    | UNTIL
    | WITHEVENTS
    | WRITE
    | OPTIONAL
    | PARAMARRAY
    | PRESERVE
    | SPC
    | TAB
    | THEN
    | TO
    ;
operatorIdentifier
    : ADDRESSOF
    | AND
    | EQV
    | IMP
    | IS
    | LIKE
    | NEW
    | MOD
    | NOT
    | OR
    | TYPEOF
    | XOR
    ;
reservedName
    : ABS
    | CBOOL
    | CBYTE
    | CCUR
    | CDATE
    | CDBL
    | CDEC
    | CINT
    | CLNG
    | CLNGLNG
    | CLNGPTR
    | CSNG
    | CSTR
    | CVAR
    | CVERR
    | DATE
    | DEBUG
    | DOEVENTS
    | FIX
    | INT
    | LEN
    | LENB
    | ME
    | PSET
    | SCALE
    | SGN
    | STRING
    ;
specialForm
    : ARRAY
    | CIRCLE
    | INPUT
    | INPUTB
    | LBOUND
    | SCALE
    | UBOUND
    ;
reservedTypeIdentifier
    : BOOLEAN
    | BYTE
    | CURRENCY
    | DATE
    | DOUBLE
    | INTEGER
    | LONG
    | LONGLONG
    | LONGPTR
    | SINGLE
    | STRING
    | VARIANT
    ;

// If we did not scoop up the bracketed forms in the Lexer, they would have become
// Foreign Names.
reservedTypeIdentifierB
    : BOOLEAN_B
    | BYTE_B
    | CURRENCY_B
    | DATE_B
    | DOUBLE_B
    | INTEGER_B
    | LONG_B
    | LONGLONG_B
    | LONGPTR_B
    | SINGLE_B
    | STRING_B
    | VARIANT_B
    ;

typeableReservedName
    : DATE
    | STRING
    ;
literalIdentifier
    : booleanLiteralIdentifier
    | objectLiteralIdentifier
    | variantLiteralIdentifier
    ;
booleanLiteralIdentifier
    : TRUE
    | FALSE
    ;
objectLiteralIdentifier
    : NOTHING
    ;
variantLiteralIdentifier
    : EMPTY_X
    | NULL_
    ;
reservedForImplementationUse
    : ATTRIBUTE
    | LINEINPUT
    | VB_BASE
    | VB_CONTROL
    | VB_CREATABLE
    | VB_CUSTOMIZABLE
    | VB_DESCRIPTION
    | VB_EXPOSED
    | VB_EXT_KEY 
    | VB_GLOBALNAMESPACE
    | VB_HELPID
    | VB_INVOKE_FUNC
    | VB_INVOKE_PROPERTY 
    | VB_INVOKE_PROPERTYPUT
    | VB_INVOKE_PROPERTYPUTREF
    | VB_MEMBERFLAGS
    | VB_NAME
    | VB_PREDECLAREDID
    | VB_PROCDATA
    | VB_TEMPLATEDERIVED
    | VB_USERMEMID
    | VB_VARDESCRIPTION
    | VB_VARHELPID
    | VB_VARMEMBERFLAGS
    | VB_VARPROCDATA 
    | VB_VARUSERMEMID
    ;
futureReserved
    : CDECL
    | DECIMAL
    | DEFDEC
    ;

// 3.3.5.3  Special Identifier Forms

// known as BUILTIN-TYPE in MS-VBAL
builtinType
    : reservedTypeIdentifier
    | reservedTypeIdentifierB
    | OBJECT
    | OBJECT_B
    ;

// Known as TYPED-NAME in MS-VBAL
// This probably could be turned into a token
typedName
    : ambiguousIdentifier typeSuffix
    | typeableReservedName typeSuffix
    ;
typeSuffix
    : '&'
    | '%'
    | '#'
    | '!'
    | '@'
    | '$'
    | '^'
    ;

//---------------------------------------------------------------------------------------
// Extra Rules

// lexer keywords not in the reservedIdentifier set.
// Any that are unused within the parser rules should probably
// be removed from the lexer.
ambiguousKeyword
    : ACCESS
    | ALIAS
    | APPACTIVATE
    | APPEND
    | ASSERT
    | BASE
    | BEGIN
    | BEGINPROPERTY
    | BINARY
    | CLASS
    | CHDIR
    | CHDRIVE
    | CLASS_INITIALIZE
    | CLASS_TERMINATE
    | COLLECTION
    | COMPARE
    | DATABASE
    | DELETESETTING
    | ERROR
    | ENDPROPERTY
    | FILECOPY
    | GO
    | KILL
    | LOAD
    | LIB
    | LINE
    | MID
    | MIDB
    | MID_D
    | MIDB_D
    | MKDIR
    | MODULE
    | NAME
    | OBJECT
    | OUTPUT
    | PROPERTY
    | RANDOM
    | RANDOMIZE
    | READ
    | RESET
    | RMDIR
    | SAVEPICTURE
    | SAVESETTING
    | SENDKEYS
    | SETATTR
    | STEP
    | TEXT
    | TIME
    | UNLOAD
    | VERSION
    | WIDTH
    ;
