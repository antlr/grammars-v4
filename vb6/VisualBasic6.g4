/*
* Copyright (C) 2016, Ulrich Wolffgang <u.wol@wwu.de>
* All rights reserved.
*
* This software may be modified and distributed under the terms
* of the BSD 3-clause license. See the LICENSE file for details.
*/
/*
* Visual Basic 6.0 Grammar for ANTLR4
*
* This is an approximate grammar for Visual Basic 6.0, derived 
* from the Visual Basic 6.0 language reference 
* http://msdn.microsoft.com/en-us/library/aa338033%28v=vs.60%29.aspx 
* and tested against MSDN VB6 statement examples as well as several Visual 
* Basic 6.0 code repositories.
*
* Characteristics:
*
* 1. This grammar is line-based and takes into account whitespace, so that
*    member calls (e.g. "A.B") are distinguished from contextual object calls 
*    in WITH statements (e.g. "A .B").
*
* 2. Keywords can be used as identifiers depending on the context, enabling
*    e.g. "A.Type", but not "Type.B".
*
*
* Known limitations:
*
* 1. Preprocessor statements (#if, #else, ...) must not interfere with regular
*    statements.
*
* 2. Comments are skipped.
*
*
* Change log:
*
* v1.4
*	- erase statement fix
*	- explicit token definition
*
* v1.3
*	- call statement precedence
*
* v1.2
*	- refined call statements
*
* v1.1 
*	- precedence of operators and of ELSE in select statements
*	- optimized member calls
*
* v1.0 Initial revision
*/

grammar VisualBasic6;

// module ----------------------------------
startRule
   : module EOF
   ;

module
   : NEWLINE* (moduleHeader NEWLINE +)? moduleConfig? NEWLINE* moduleAttributes? NEWLINE* moduleOptions? NEWLINE* moduleBody? NEWLINE*
   ;

moduleHeader
   : VERSION DOUBLELITERAL CLASS
   ;

moduleConfig
   : BEGIN NEWLINE + moduleConfigElement + END NEWLINE +
   ;

moduleConfigElement
   : ambiguousIdentifier EQ literal NEWLINE
   ;

moduleAttributes
   : (attributeStmt NEWLINE +) +
   ;

moduleOptions
   : (moduleOption NEWLINE +) +
   ;

moduleOption
   : OPTION_BASE INTEGERLITERAL # optionBaseStmt
   | OPTION_COMPARE (BINARY | TEXT) # optionCompareStmt
   | OPTION_EXPLICIT # optionExplicitStmt
   | OPTION_PRIVATE_MODULE # optionPrivateModuleStmt
   ;

moduleBody
   : moduleBodyElement (NEWLINE + moduleBodyElement)*
   ;

moduleBodyElement
   : moduleBlock
   | declareStmt
   | enumerationStmt
   | eventStmt
   | functionStmt
   | macroIfThenElseStmt
   | propertyGetStmt
   | propertySetStmt
   | propertyLetStmt
   | subStmt
   | typeStmt
   ;

// block ----------------------------------
moduleBlock
   : block
   ;

attributeStmt
   : ATTRIBUTE implicitCallStmt_InStmt EQ literal (COMMA literal)*
   ;

block
   : blockStmt (NEWLINE + blockStmt)*
   ;

blockStmt
   : appactivateStmt
   | attributeStmt
   | beepStmt
   | chdirStmt
   | chdriveStmt
   | closeStmt
   | constStmt
   | dateStmt
   | deleteSettingStmt
   | deftypeStmt
   | doLoopStmt
   | endStmt
   | eraseStmt
   | errorStmt
   | exitStmt
   | explicitCallStmt
   | filecopyStmt
   | forEachStmt
   | forNextStmt
   | getStmt
   | goSubStmt
   | goToStmt
   | ifThenElseStmt
   | implementsStmt
   | inputStmt
   | killStmt
   | letStmt
   | lineInputStmt
   | lineLabel
   | loadStmt
   | lockStmt
   | lsetStmt
   | macroIfThenElseStmt
   | midStmt
   | mkdirStmt
   | nameStmt
   | onErrorStmt
   | onGoToStmt
   | onGoSubStmt
   | openStmt
   | printStmt
   | putStmt
   | raiseEventStmt
   | randomizeStmt
   | redimStmt
   | resetStmt
   | resumeStmt
   | returnStmt
   | rmdirStmt
   | rsetStmt
   | savepictureStmt
   | saveSettingStmt
   | seekStmt
   | selectCaseStmt
   | sendkeysStmt
   | setattrStmt
   | setStmt
   | stopStmt
   | timeStmt
   | unloadStmt
   | unlockStmt
   | variableStmt
   | whileWendStmt
   | widthStmt
   | withStmt
   | writeStmt
   | implicitCallStmt_InBlock
   ;

// statements ----------------------------------
appactivateStmt
   : APPACTIVATE valueStmt (COMMA valueStmt)?
   ;

beepStmt
   : BEEP
   ;

chdirStmt
   : CHDIR valueStmt
   ;

chdriveStmt
   : CHDRIVE valueStmt
   ;

closeStmt
   : CLOSE (valueStmt (COMMA valueStmt)*)?
   ;

constStmt
   : (visibility)? CONST constSubStmt (COMMA constSubStmt)*
   ;

constSubStmt
   : ambiguousIdentifier typeHint? (asTypeClause)? EQ valueStmt
   ;

dateStmt
   : DATE EQ valueStmt
   ;

declareStmt
   : (visibility)? DECLARE (FUNCTION typeHint? | SUB) ambiguousIdentifier typeHint? LIB STRINGLITERAL (ALIAS STRINGLITERAL)? (argList)? (asTypeClause)?
   ;

deftypeStmt
   : (DEFBOOL | DEFBYTE | DEFINT | DEFLNG | DEFCUR | DEFSNG | DEFDBL | DEFDEC | DEFDATE | DEFSTR | DEFOBJ | DEFVAR) letterrange (COMMA letterrange)*
   ;

deleteSettingStmt
   : DELETESETTING valueStmt COMMA valueStmt (COMMA valueStmt)?
   ;

doLoopStmt
   : DO NEWLINE + (block NEWLINE +)? LOOP
   | DO (WHILE | UNTIL) valueStmt NEWLINE + (block NEWLINE +)? LOOP
   | DO NEWLINE + (block NEWLINE +) LOOP (WHILE | UNTIL) valueStmt
   ;

endStmt
   : END
   ;

enumerationStmt
   : (visibility)? ENUM ambiguousIdentifier NEWLINE + (enumerationStmt_Constant)* END_ENUM
   ;

enumerationStmt_Constant
   : ambiguousIdentifier (EQ valueStmt)? NEWLINE +
   ;

eraseStmt
   : ERASE valueStmt (COMMA valueStmt)*
   ;

errorStmt
   : ERROR valueStmt
   ;

eventStmt
   : (visibility)? EVENT ambiguousIdentifier argList
   ;

exitStmt
   : EXIT_DO
   | EXIT_FOR
   | EXIT_FUNCTION
   | EXIT_PROPERTY
   | EXIT_SUB
   ;

filecopyStmt
   : FILECOPY valueStmt COMMA valueStmt
   ;

forEachStmt
   : FOR EACH ambiguousIdentifier typeHint? IN valueStmt NEWLINE + (block NEWLINE +)? NEXT (ambiguousIdentifier)?
   ;

forNextStmt
   : FOR ambiguousIdentifier typeHint? (asTypeClause)? EQ valueStmt TO valueStmt (STEP valueStmt)? NEWLINE + (block NEWLINE +)? NEXT (ambiguousIdentifier)?
   ;

functionStmt
   : (visibility)? (STATIC)? FUNCTION ambiguousIdentifier (argList)? (asTypeClause)? NEWLINE + (block NEWLINE +)? END_FUNCTION
   ;

getStmt
   : GET valueStmt COMMA valueStmt? COMMA valueStmt
   ;

goSubStmt
   : GOSUB valueStmt
   ;

goToStmt
   : GOTO valueStmt
   ;

ifThenElseStmt
   : IF ifConditionStmt THEN blockStmt (ELSE blockStmt)? # inlineIfThenElse
   | ifBlockStmt ifElseIfBlockStmt* ifElseBlockStmt? END_IF # blockIfThenElse
   ;

ifBlockStmt
   : IF ifConditionStmt THEN NEWLINE + (block NEWLINE +)?
   ;

ifConditionStmt
   : valueStmt
   ;

ifElseIfBlockStmt
   : ELSEIF ifConditionStmt THEN NEWLINE + (block NEWLINE +)?
   ;

ifElseBlockStmt
   : ELSE NEWLINE + (block NEWLINE +)?
   ;

implementsStmt
   : IMPLEMENTS ambiguousIdentifier
   ;

inputStmt
   : INPUT valueStmt (COMMA valueStmt) +
   ;

killStmt
   : KILL valueStmt
   ;

letStmt
   : (LET)? implicitCallStmt_InStmt (EQ | PLUS_EQ | MINUS_EQ) valueStmt
   ;

lineInputStmt
   : LINE_INPUT valueStmt COMMA valueStmt
   ;

loadStmt
   : LOAD valueStmt
   ;

lockStmt
   : LOCK valueStmt (COMMA valueStmt (TO valueStmt)?)?
   ;

lsetStmt
   : LSET implicitCallStmt_InStmt EQ valueStmt
   ;

macroIfThenElseStmt
   : macroIfBlockStmt macroElseIfBlockStmt* macroElseBlockStmt? MACRO_END_IF
   ;

macroIfBlockStmt
   : MACRO_IF ifConditionStmt THEN NEWLINE + (moduleBody NEWLINE +)?
   ;

macroElseIfBlockStmt
   : MACRO_ELSEIF ifConditionStmt THEN NEWLINE + (moduleBody NEWLINE +)?
   ;

macroElseBlockStmt
   : MACRO_ELSE NEWLINE + (moduleBody NEWLINE +)?
   ;

midStmt
   : MID LPAREN argsCall RPAREN
   ;

mkdirStmt
   : MKDIR valueStmt
   ;

nameStmt
   : NAME valueStmt AS valueStmt
   ;

onErrorStmt
   : ON_ERROR (GOTO valueStmt | RESUME NEXT)
   ;

onGoToStmt
   : ON valueStmt GOTO valueStmt (COMMA valueStmt)*
   ;

onGoSubStmt
   : ON valueStmt GOSUB valueStmt (COMMA valueStmt)*
   ;

openStmt
   : OPEN valueStmt FOR (APPEND | BINARY | INPUT | OUTPUT | RANDOM) (ACCESS (READ | WRITE | READ_WRITE))? ((SHARED | LOCK_READ | LOCK_WRITE | LOCK_READ_WRITE))? AS valueStmt (LEN EQ valueStmt)?
   ;

outputList
   : outputList_Expression ((SEMICOLON | COMMA) outputList_Expression?)*
   | outputList_Expression? ((SEMICOLON | COMMA) outputList_Expression?) +
   ;

outputList_Expression
   : valueStmt
   | (SPC | TAB) (LPAREN argsCall RPAREN)?
   ;

printStmt
   : PRINT valueStmt COMMA (outputList)?
   ;

propertyGetStmt
   : (visibility)? (STATIC)? PROPERTY_GET ambiguousIdentifier typeHint? (argList)? (asTypeClause)? NEWLINE + (block NEWLINE +)? END_PROPERTY
   ;

propertySetStmt
   : (visibility)? (STATIC)? PROPERTY_SET ambiguousIdentifier (argList)? NEWLINE + (block NEWLINE +)? END_PROPERTY
   ;

propertyLetStmt
   : (visibility)? (STATIC)? PROPERTY_LET ambiguousIdentifier (argList)? NEWLINE + (block NEWLINE +)? END_PROPERTY
   ;

putStmt
   : PUT valueStmt COMMA valueStmt? COMMA valueStmt
   ;

raiseEventStmt
   : RAISEEVENT ambiguousIdentifier (LPAREN (argsCall)? RPAREN)?
   ;

randomizeStmt
   : RANDOMIZE (valueStmt)?
   ;

redimStmt
   : REDIM (PRESERVE)? redimSubStmt (COMMA redimSubStmt)*
   ;

redimSubStmt
   : implicitCallStmt_InStmt LPAREN subscripts RPAREN (asTypeClause)?
   ;

resetStmt
   : RESET
   ;

resumeStmt
   : RESUME ((NEXT | ambiguousIdentifier))?
   ;

returnStmt
   : RETURN
   ;

rmdirStmt
   : RMDIR valueStmt
   ;

rsetStmt
   : RSET implicitCallStmt_InStmt EQ valueStmt
   ;

savepictureStmt
   : SAVEPICTURE valueStmt COMMA valueStmt
   ;

saveSettingStmt
   : SAVESETTING valueStmt COMMA valueStmt COMMA valueStmt COMMA valueStmt
   ;

seekStmt
   : SEEK valueStmt COMMA valueStmt
   ;

selectCaseStmt
   : SELECT CASE valueStmt NEWLINE + sC_Case* END_SELECT
   ;

sC_Case
   : CASE sC_Cond (COLON? NEWLINE* | NEWLINE +) (block NEWLINE +)?
   ;

// ELSE first, so that it is not interpreted as a variable call
sC_Cond
   : ELSE # caseCondElse
   | IS comparisonOperator valueStmt # caseCondIs
   | valueStmt (COMMA valueStmt)* # caseCondValue
   | INTEGERLITERAL TO valueStmt (COMMA valueStmt)* # caseCondTo
   ;

sendkeysStmt
   : SENDKEYS valueStmt (COMMA valueStmt)?
   ;

setattrStmt
   : SETATTR valueStmt COMMA valueStmt
   ;

setStmt
   : SET implicitCallStmt_InStmt EQ valueStmt
   ;

stopStmt
   : STOP
   ;

subStmt
   : (visibility)? (STATIC)? SUB ambiguousIdentifier (argList)? NEWLINE + (block NEWLINE +)? END_SUB
   ;

timeStmt
   : TIME EQ valueStmt
   ;

typeStmt
   : (visibility)? TYPE ambiguousIdentifier NEWLINE + (typeStmt_Element)* END_TYPE
   ;

typeStmt_Element
   : ambiguousIdentifier (LPAREN (subscripts)? RPAREN)? (asTypeClause)? NEWLINE +
   ;

typeOfStmt
   : TYPEOF valueStmt (IS type)?
   ;

unloadStmt
   : UNLOAD valueStmt
   ;

unlockStmt
   : UNLOCK valueStmt (COMMA valueStmt (TO valueStmt)?)?
   ;

// operator precedence is represented by rule order
valueStmt
   : literal # vsLiteral
   | implicitCallStmt_InStmt # vsICS
   | LPAREN valueStmt (COMMA valueStmt)* RPAREN # vsStruct
   | NEW valueStmt # vsNew
   | typeOfStmt # vsTypeOf
   | midStmt # vsMid
   | ADDRESSOF valueStmt # vsAddressOf
   | implicitCallStmt_InStmt ASSIGN valueStmt # vsAssign
   | valueStmt IS valueStmt # vsIs
   | valueStmt LIKE valueStmt # vsLike
   | valueStmt GEQ valueStmt # vsGeq
   | valueStmt LEQ valueStmt # vsLeq
   | valueStmt GT valueStmt # vsGt
   | valueStmt LT valueStmt # vsLt
   | valueStmt NEQ valueStmt # vsNeq
   | valueStmt EQ valueStmt # vsEq
   | valueStmt AMPERSAND valueStmt # vsAmp
   | MINUS valueStmt # vsNegation
   | PLUS valueStmt # vsPlus
   | valueStmt PLUS valueStmt # vsAdd
   | valueStmt MOD valueStmt # vsMod
   | valueStmt DIV valueStmt # vsDiv
   | valueStmt MULT valueStmt # vsMult
   | valueStmt MINUS valueStmt # vsMinus
   | valueStmt POW valueStmt # vsPow
   | valueStmt IMP valueStmt # vsImp
   | valueStmt EQV valueStmt # vsEqv
   | valueStmt XOR valueStmt # vsXor
   | valueStmt OR valueStmt # vsOr
   | valueStmt AND valueStmt # vsAnd
   | NOT valueStmt # vsNot
   ;

variableStmt
   : (DIM | STATIC | visibility) (WITHEVENTS)? variableListStmt
   ;

variableListStmt
   : variableSubStmt (COMMA variableSubStmt)*
   ;

variableSubStmt
   : ambiguousIdentifier (LPAREN (subscripts)? RPAREN)? typeHint? (asTypeClause)?
   ;

whileWendStmt
   : WHILE valueStmt NEWLINE + (block NEWLINE)* WEND
   ;

widthStmt
   : WIDTH valueStmt COMMA valueStmt
   ;

withStmt
   : WITH implicitCallStmt_InStmt NEWLINE + (block NEWLINE +)? END_WITH
   ;

writeStmt
   : WRITE valueStmt COMMA (outputList)?
   ;

// complex call statements ----------------------------------
explicitCallStmt
   : eCS_ProcedureCall
   | eCS_MemberProcedureCall
   ;

// parantheses are required in case of args -> empty parantheses are removed
eCS_ProcedureCall
   : CALL ambiguousIdentifier typeHint? (LPAREN argsCall RPAREN)?
   ;

// parantheses are required in case of args -> empty parantheses are removed
eCS_MemberProcedureCall
   : CALL implicitCallStmt_InStmt? DOT ambiguousIdentifier typeHint? (LPAREN argsCall RPAREN)?
   ;

implicitCallStmt_InBlock
   : iCS_B_ProcedureCall
   | iCS_B_MemberProcedureCall
   ;

// parantheses are forbidden in case of args
// variables cannot be called in blocks
// certainIdentifier instead of ambiguousIdentifier for preventing ambiguity with statement keywords 
iCS_B_ProcedureCall
   : certainIdentifier (argsCall)?
   ;

iCS_B_MemberProcedureCall
   : implicitCallStmt_InStmt? DOT ambiguousIdentifier typeHint? (argsCall)? dictionaryCallStmt?
   ;

// iCS_S_MembersCall first, so that member calls are not resolved as separate iCS_S_VariableOrProcedureCalls
implicitCallStmt_InStmt
   : iCS_S_MembersCall
   | iCS_S_VariableOrProcedureCall
   | iCS_S_ProcedureOrArrayCall
   | iCS_S_DictionaryCall
   ;

iCS_S_VariableOrProcedureCall
   : ambiguousIdentifier typeHint? dictionaryCallStmt?
   ;

iCS_S_ProcedureOrArrayCall
   : (ambiguousIdentifier | baseType) typeHint? LPAREN (argsCall)? RPAREN dictionaryCallStmt?
   ;

iCS_S_MembersCall
   : (iCS_S_VariableOrProcedureCall | iCS_S_ProcedureOrArrayCall)? iCS_S_MemberCall + dictionaryCallStmt?
   ;

iCS_S_MemberCall
   : DOT (iCS_S_VariableOrProcedureCall | iCS_S_ProcedureOrArrayCall)
   ;

iCS_S_DictionaryCall
   : dictionaryCallStmt
   ;

// atomic call statements ----------------------------------
argsCall
   : (argCall? (COMMA | SEMICOLON))* argCall ((COMMA | SEMICOLON) argCall?)*
   ;

argCall
   : ((BYVAL | BYREF | PARAMARRAY))? valueStmt
   ;

dictionaryCallStmt
   : EXCLAMATIONMARK ambiguousIdentifier typeHint?
   ;

// atomic rules for statements
argList
   : LPAREN (arg (COMMA arg)*)? RPAREN
   ;

arg
   : (OPTIONAL)? ((BYVAL | BYREF))? (PARAMARRAY)? ambiguousIdentifier (LPAREN RPAREN)? (asTypeClause)? (argDefaultValue)?
   ;

argDefaultValue
   : EQ (literal | ambiguousIdentifier)
   ;

subscripts
   : subscript (COMMA subscript)*
   ;

subscript
   : (valueStmt TO)? valueStmt
   ;

// atomic rules ----------------------------------
ambiguousIdentifier
   : (IDENTIFIER | ambiguousKeyword) +
   | L_SQUARE_BRACKET (IDENTIFIER | ambiguousKeyword) + R_SQUARE_BRACKET
   ;

asTypeClause
   : AS (NEW)? type (fieldLength)?
   ;

baseType
   : BOOLEAN
   | BYTE
   | COLLECTION
   | DATE
   | DOUBLE
   | INTEGER
   | LONG
   | SINGLE
   | STRING
   | VARIANT
   ;

certainIdentifier
   : IDENTIFIER (ambiguousKeyword | IDENTIFIER)*
   | ambiguousKeyword (ambiguousKeyword | IDENTIFIER) +
   ;

comparisonOperator
   : LT
   | LEQ
   | GT
   | GEQ
   | EQ
   | NEQ
   | IS
   | LIKE
   ;

complexType
   : ambiguousIdentifier (DOT ambiguousIdentifier)*
   ;

fieldLength
   : MULT (INTEGERLITERAL | ambiguousIdentifier)
   ;

letterrange
   : certainIdentifier (MINUS certainIdentifier)?
   ;

lineLabel
   : ambiguousIdentifier COLON
   ;

literal
   : COLORLITERAL
   | DATELITERAL
   | DOUBLELITERAL
   | FILENUMBER
   | INTEGERLITERAL
   | STRINGLITERAL
   | TRUE
   | FALSE
   | NOTHING
   | NULL
   ;

type
   : (baseType | complexType) (LPAREN RPAREN)?
   ;

typeHint
   : AMPERSAND
   | AT
   | DOLLAR
   | EXCLAMATIONMARK
   | HASH
   | PERCENT
   ;

visibility
   : PRIVATE
   | PUBLIC
   | FRIEND
   | GLOBAL
   ;

// ambiguous keywords
ambiguousKeyword
   : ACCESS
   | ADDRESSOF
   | ALIAS
   | AND
   | ATTRIBUTE
   | APPACTIVATE
   | APPEND
   | AS
   | BEEP
   | BEGIN
   | BINARY
   | BOOLEAN
   | BYVAL
   | BYREF
   | BYTE
   | CALL
   | CASE
   | CLASS
   | CLOSE
   | CHDIR
   | CHDRIVE
   | COLLECTION
   | CONST
   | DATE
   | DECLARE
   | DEFBOOL
   | DEFBYTE
   | DEFCUR
   | DEFDBL
   | DEFDATE
   | DEFDEC
   | DEFINT
   | DEFLNG
   | DEFOBJ
   | DEFSNG
   | DEFSTR
   | DEFVAR
   | DELETESETTING
   | DIM
   | DO
   | DOUBLE
   | EACH
   | ELSE
   | ELSEIF
   | END
   | ENUM
   | EQV
   | ERASE
   | ERROR
   | EVENT
   | FALSE
   | FILECOPY
   | FRIEND
   | FOR
   | FUNCTION
   | GET
   | GLOBAL
   | GOSUB
   | GOTO
   | IF
   | IMP
   | IMPLEMENTS
   | IN
   | INPUT
   | IS
   | INTEGER
   | KILL
   | LOAD
   | LOCK
   | LONG
   | LOOP
   | LEN
   | LET
   | LIB
   | LIKE
   | LSET
   | ME
   | MID
   | MKDIR
   | MOD
   | NAME
   | NEXT
   | NEW
   | NOT
   | NOTHING
   | NULL
   | ON
   | OPEN
   | OPTIONAL
   | OR
   | OUTPUT
   | PARAMARRAY
   | PRESERVE
   | PRINT
   | PRIVATE
   | PUBLIC
   | PUT
   | RANDOM
   | RANDOMIZE
   | RAISEEVENT
   | READ
   | REDIM
   | REM
   | RESET
   | RESUME
   | RETURN
   | RMDIR
   | RSET
   | SAVEPICTURE
   | SAVESETTING
   | SEEK
   | SELECT
   | SENDKEYS
   | SET
   | SETATTR
   | SHARED
   | SINGLE
   | SPC
   | STATIC
   | STEP
   | STOP
   | STRING
   | SUB
   | TAB
   | TEXT
   | THEN
   | TIME
   | TO
   | TRUE
   | TYPE
   | TYPEOF
   | UNLOAD
   | UNLOCK
   | UNTIL
   | VARIANT
   | VERSION
   | WEND
   | WHILE
   | WIDTH
   | WITH
   | WITHEVENTS
   | WRITE
   | XOR
   ;

// lexer rules --------------------------------------------------------------------------------
// keywords

ACCESS
   : A C C E S S
   ;


ADDRESSOF
   : A D D R E S S O F
   ;


ALIAS
   : A L I A S
   ;


AND
   : A N D
   ;


ATTRIBUTE
   : A T T R I B U T E
   ;


APPACTIVATE
   : A P P A C T I V A T E
   ;


APPEND
   : A P P E N D
   ;


AS
   : A S
   ;


BEGIN
   : B E G I N
   ;


BEEP
   : B E E P
   ;


BINARY
   : B I N A R Y
   ;


BOOLEAN
   : B O O L E A N
   ;


BYVAL
   : B Y V A L
   ;


BYREF
   : B Y R E F
   ;


BYTE
   : B Y T E
   ;


CALL
   : C A L L
   ;


CASE
   : C A S E
   ;


CHDIR
   : C H D I R
   ;


CHDRIVE
   : C H D R I V E
   ;


CLASS
   : C L A S S
   ;


CLOSE
   : C L O S E
   ;


COLLECTION
   : C O L L E C T I O N
   ;


CONST
   : C O N S T
   ;


DATE
   : D A T E
   ;


DECLARE
   : D E C L A R E
   ;


DEFBOOL
   : D E F B O O L
   ;


DEFBYTE
   : D E F B Y T E
   ;


DEFDATE
   : D E F D A T E
   ;


DEFDBL
   : D E F D B L
   ;


DEFDEC
   : D E F D E C
   ;


DEFCUR
   : D E F C U R
   ;


DEFINT
   : D E F I N T
   ;


DEFLNG
   : D E F L N G
   ;


DEFOBJ
   : D E F O B J
   ;


DEFSNG
   : D E F S N G
   ;


DEFSTR
   : D E F S T R
   ;


DEFVAR
   : D E F V A R
   ;


DELETESETTING
   : D E L E T E S E T T I N G
   ;


DIM
   : D I M
   ;


DO
   : D O
   ;


DOUBLE
   : D O U B L E
   ;


EACH
   : E A C H
   ;


ELSE
   : E L S E
   ;


ELSEIF
   : E L S E I F
   ;


END_ENUM
   : E N D ' ' E N U M
   ;


END_FUNCTION
   : E N D ' ' F U N C T I O N
   ;


END_IF
   : E N D ' ' I F
   ;


END_PROPERTY
   : E N D ' ' P R O P E R T Y
   ;


END_SELECT
   : E N D ' ' S E L E C T
   ;


END_SUB
   : E N D ' ' S U B
   ;


END_TYPE
   : E N D ' ' T Y P E
   ;


END_WITH
   : E N D ' ' W I T H
   ;


END
   : E N D
   ;


ENUM
   : E N U M
   ;


EQV
   : E Q V
   ;


ERASE
   : E R A S E
   ;


ERROR
   : E R R O R
   ;


EVENT
   : E V E N T
   ;


EXIT_DO
   : E X I T ' ' D O
   ;


EXIT_FOR
   : E X I T ' ' F O R
   ;


EXIT_FUNCTION
   : E X I T ' ' F U N C T I O N
   ;


EXIT_PROPERTY
   : E X I T ' ' P R O P E R T Y
   ;


EXIT_SUB
   : E X I T ' ' S U B
   ;


FALSE
   : F A L S E
   ;


FILECOPY
   : F I L E C O P Y
   ;


FRIEND
   : F R I E N D
   ;


FOR
   : F O R
   ;


FUNCTION
   : F U N C T I O N
   ;


GET
   : G E T
   ;


GLOBAL
   : G L O B A L
   ;


GOSUB
   : G O S U B
   ;


GOTO
   : G O T O
   ;


IF
   : I F
   ;


IMP
   : I M P
   ;


IMPLEMENTS
   : I M P L E M E N T S
   ;


IN
   : I N
   ;


INPUT
   : I N P U T
   ;


IS
   : I S
   ;


INTEGER
   : I N T E G E R
   ;


KILL
   : K I L L
   ;


LOAD
   : L O A D
   ;


LOCK
   : L O C K
   ;


LONG
   : L O N G
   ;


LOOP
   : L O O P
   ;


LEN
   : L E N
   ;


LET
   : L E T
   ;


LIB
   : L I B
   ;


LIKE
   : L I K E
   ;


LINE_INPUT
   : L I N E ' ' I N P U T
   ;


LOCK_READ
   : L O C K ' ' R E A D
   ;


LOCK_WRITE
   : L O C K ' ' W R I T E
   ;


LOCK_READ_WRITE
   : L O C K ' ' R E A D ' ' W R I T E
   ;


LSET
   : L S E T
   ;


MACRO_IF
   : HASH I F
   ;


MACRO_ELSEIF
   : HASH E L S E I F
   ;


MACRO_ELSE
   : HASH E L S E
   ;


MACRO_END_IF
   : HASH E N D ' ' I F
   ;


ME
   : M E
   ;


MID
   : M I D
   ;


MKDIR
   : M K D I R
   ;


MOD
   : M O D
   ;


NAME
   : N A M E
   ;


NEXT
   : N E X T
   ;


NEW
   : N E W
   ;


NOT
   : N O T
   ;


NOTHING
   : N O T H I N G
   ;


NULL
   : N U L L
   ;


ON
   : O N
   ;


ON_ERROR
   : O N ' ' E R R O R
   ;


OPEN
   : O P E N
   ;


OPTIONAL
   : O P T I O N A L
   ;


OPTION_BASE
   : O P T I O N ' ' B A S E
   ;


OPTION_EXPLICIT
   : O P T I O N ' ' E X P L I C I T
   ;


OPTION_COMPARE
   : O P T I O N ' ' C O M P A R E
   ;


OPTION_PRIVATE_MODULE
   : O P T I O N ' ' P R I V A T E ' ' M O D U L E
   ;


OR
   : O R
   ;


OUTPUT
   : O U T P U T
   ;


PARAMARRAY
   : P A R A M A R R A Y
   ;


PRESERVE
   : P R E S E R V E
   ;


PRINT
   : P R I N T
   ;


PRIVATE
   : P R I V A T E
   ;


PROPERTY_GET
   : P R O P E R T Y ' ' G E T
   ;


PROPERTY_LET
   : P R O P E R T Y ' ' L E T
   ;


PROPERTY_SET
   : P R O P E R T Y ' ' S E T
   ;


PUBLIC
   : P U B L I C
   ;


PUT
   : P U T
   ;


RANDOM
   : R A N D O M
   ;


RANDOMIZE
   : R A N D O M I Z E
   ;


RAISEEVENT
   : R A I S E E V E N T
   ;


READ
   : R E A D
   ;


READ_WRITE
   : R E A D ' ' W R I T E
   ;


REDIM
   : R E D I M
   ;


REM
   : R E M
   ;


RESET
   : R E S E T
   ;


RESUME
   : R E S U M E
   ;


RETURN
   : R E T U R N
   ;


RMDIR
   : R M D I R
   ;


RSET
   : R S E T
   ;


SAVEPICTURE
   : S A V E P I C T U R E
   ;


SAVESETTING
   : S A V E S E T T I N G
   ;


SEEK
   : S E E K
   ;


SELECT
   : S E L E C T
   ;


SENDKEYS
   : S E N D K E Y S
   ;


SET
   : S E T
   ;


SETATTR
   : S E T A T T R
   ;


SHARED
   : S H A R E D
   ;


SINGLE
   : S I N G L E
   ;


SPC
   : S P C
   ;


STATIC
   : S T A T I C
   ;


STEP
   : S T E P
   ;


STOP
   : S T O P
   ;


STRING
   : S T R I N G
   ;


SUB
   : S U B
   ;


TAB
   : T A B
   ;


TEXT
   : T E X T
   ;


THEN
   : T H E N
   ;


TIME
   : T I M E
   ;


TO
   : T O
   ;


TRUE
   : T R U E
   ;


TYPE
   : T Y P E
   ;


TYPEOF
   : T Y P E O F
   ;


UNLOAD
   : U N L O A D
   ;


UNLOCK
   : U N L O C K
   ;


UNTIL
   : U N T I L
   ;


VARIANT
   : V A R I A N T
   ;


VERSION
   : V E R S I O N
   ;


WEND
   : W E N D
   ;


WHILE
   : W H I L E
   ;


WIDTH
   : W I D T H
   ;


WITH
   : W I T H
   ;


WITHEVENTS
   : W I T H E V E N T S
   ;


WRITE
   : W R I T E
   ;


XOR
   : X O R
   ;

// symbols

AMPERSAND
   : '&'
   ;


ASSIGN
   : ':='
   ;


AT
   : '@'
   ;


COLON
   : ':'
   ;


COMMA
   : ','
   ;


DIV
   : '\\' | '/'
   ;


DOLLAR
   : '$'
   ;


DOT
   : '.'
   ;


EQ
   : '='
   ;


EXCLAMATIONMARK
   : '!'
   ;


GEQ
   : '>='
   ;


GT
   : '>'
   ;


HASH
   : '#'
   ;


LEQ
   : '<='
   ;


LPAREN
   : '('
   ;


LT
   : '<'
   ;


MINUS
   : '-'
   ;


MINUS_EQ
   : '-='
   ;


MULT
   : '*'
   ;


NEQ
   : '<>'
   ;


PERCENT
   : '%'
   ;


PLUS
   : '+'
   ;


PLUS_EQ
   : '+='
   ;


POW
   : '^'
   ;


RPAREN
   : ')'
   ;


SEMICOLON
   : ';'
   ;


L_SQUARE_BRACKET
   : '['
   ;


R_SQUARE_BRACKET
   : ']'
   ;

// literals

STRINGLITERAL
   : '"' (~ ["\r\n] | '""')* '"'
   ;


DATELITERAL
   : HASH (~ [#\r\n])* HASH
   ;


COLORLITERAL
   : '&H' [0-9A-F] + AMPERSAND?
   ;


INTEGERLITERAL
   : (PLUS | MINUS)? ('0' .. '9') + (('e' | 'E') INTEGERLITERAL)* (HASH | AMPERSAND)?
   ;


DOUBLELITERAL
   : (PLUS | MINUS)? ('0' .. '9')* DOT ('0' .. '9') + (('e' | 'E') (PLUS | MINUS)? ('0' .. '9') +)* (HASH | AMPERSAND)?
   ;


FILENUMBER
   : HASH LETTERORDIGIT +
   ;

// identifier

IDENTIFIER
   : LETTER LETTERORDIGIT*
   ;

// whitespace, line breaks, comments, ...

LINE_CONTINUATION
   : ' ' '_' '\r'? '\n' -> skip
   ;


NEWLINE
   : ('\r'? '\n' | COLON ' ')
   ;


COMMENT
   : ('\'' | COLON? REM ' ') (LINE_CONTINUATION | ~ ('\n' | '\r'))* -> skip
   ;


WS
   : [ \t] + -> skip
   ;

// letters

fragment LETTER
   : [a-zA-Z_ŠšŸ€…†]
   ;


fragment LETTERORDIGIT
   : [a-zA-Z0-9_ŠšŸ€…†]
   ;

// case insensitive chars

fragment A
   : ('a' | 'A')
   ;


fragment B
   : ('b' | 'B')
   ;


fragment C
   : ('c' | 'C')
   ;


fragment D
   : ('d' | 'D')
   ;


fragment E
   : ('e' | 'E')
   ;


fragment F
   : ('f' | 'F')
   ;


fragment G
   : ('g' | 'G')
   ;


fragment H
   : ('h' | 'H')
   ;


fragment I
   : ('i' | 'I')
   ;


fragment J
   : ('j' | 'J')
   ;


fragment K
   : ('k' | 'K')
   ;


fragment L
   : ('l' | 'L')
   ;


fragment M
   : ('m' | 'M')
   ;


fragment N
   : ('n' | 'N')
   ;


fragment O
   : ('o' | 'O')
   ;


fragment P
   : ('p' | 'P')
   ;


fragment Q
   : ('q' | 'Q')
   ;


fragment R
   : ('r' | 'R')
   ;


fragment S
   : ('s' | 'S')
   ;


fragment T
   : ('t' | 'T')
   ;


fragment U
   : ('u' | 'U')
   ;


fragment V
   : ('v' | 'V')
   ;


fragment W
   : ('w' | 'W')
   ;


fragment X
   : ('x' | 'X')
   ;


fragment Y
   : ('y' | 'Y')
   ;


fragment Z
   : ('z' | 'Z')
   ;
