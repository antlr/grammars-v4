/*
* Copyright (C) 2014 Ulrich Wolffgang <u.wol@wwu.de>
*
* This program is free software: you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.
* 
* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
* GNU General Public License for more details.
* 
* You should have received a copy of the GNU General Public License
* along with this program. If not, see <http://www.gnu.org/licenses/>.
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
* Change log:
*
* v1.4 Rubberduck
*   - renamed to VBA; goal is to support VBA, and a shorter name is more practical.
*   - added moduleDeclarations rule, moved moduleOptions there; options can now be
*     located anywhere in declarations section, without breaking the parser.
*   - added support for Option Compare Database.
*   - added support for VBA 7.0 PtrSafe attribute for Declare statements.
*   - implemented a fileNumber rule to locate identifier usages in file numbers.
*   - added support for anonymous declarations in With blocks (With New Something)
*   - blockStmt rules being sorted alphabetically was wrong. moved implicit call statement last.
*   - '!' in dictionary call statement rule gets picked up as a type hint; changed member call
*     to accept '!' as well as '.', but this complicates resolving the '!' shorthand syntax.
*   - added a subscripts rule in procedure calls, to avoid breaking the parser with 
*     a function call that returns an array that is immediately accessed.
*   - added missing macroConstStmt (#CONST) rule.
*   - amended selectCaseStmt rules to support all valid syntaxes.
*   - blockStmt is now illegal in declarations section.
*   - added ON_LOCAL_ERROR token, to support legacy ON LOCAL ERROR statements.
*   - added additional typeHint? token to declareStmt, to support "Declare Function Foo$".
*   - modified WS lexer rule to correctly account for line continuations;
*   - modified multi-word lexer rules to use WS lexer token instead of ' '; this makes
*     the grammar support "Option _\n Explicit" and other keywords being specified on multiple lines.
*	- modified moduleOption rules to account for WS token in corresponding lexer rules.
*   - modified NEWLINE lexer rule to properly support instructions separator (':').
*   - tightened DATELITERAL lexer rule to the format enforced by the VBE, because "#fn: Close #" 
*     in "Dim fn: fn = FreeFile: Open "filename" For Output As #fn: Close #fn" was picked up as a date literal.
*   - redefined IDENTIFIER lexer rule to support non-Latin characters (e.g. Japanese)
*   - made seekStmt, lockStmt, unlockStmt, getStmt and widthStmt accept a fileNumber (needed to support '#')
*   - fixed precompiler directives, which can now be nested. they still can't interfere with other blocks though.
*   - optional parameters can be a valueStmt.
*   - added support for Octal and Currency literals.
*   - implemented proper specs for DATELITERAL.
*   - added comments to parse tree (removes known limitation #2).
*   - macroConstStmt now allowed in blockStmt.
*   - allow type hints for parameters.
*
*======================================================================================
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

grammar vba;

// module ----------------------------------

startRule : module EOF;

module : 
	WS?
	endOfLine*
	(moduleHeader endOfLine*)?
	moduleConfig? endOfLine*
	moduleAttributes? endOfLine*
	moduleDeclarations? endOfLine*
	moduleBody? endOfLine*
	WS?
;

moduleHeader : VERSION WS DOUBLELITERAL WS CLASS;

moduleConfig :
	BEGIN endOfLine*
	moduleConfigElement+
	END
;

moduleConfigElement :
	ambiguousIdentifier WS? EQ WS? literal endOfLine*
;

moduleAttributes : (attributeStmt endOfLine+)+;

moduleDeclarations : moduleDeclarationsElement (endOfLine+ moduleDeclarationsElement)* endOfLine*;

moduleOption : 
	OPTION_BASE WS SHORTLITERAL 					# optionBaseStmt
	| OPTION_COMPARE WS (BINARY | TEXT | DATABASE) 	# optionCompareStmt
	| OPTION_EXPLICIT 								# optionExplicitStmt
	| OPTION_PRIVATE_MODULE 						# optionPrivateModuleStmt
;

moduleDeclarationsElement :
	comment
	| declareStmt
	| enumerationStmt 
	| eventStmt
	| constStmt
	| implementsStmt
	| variableStmt
	| moduleOption
	| typeStmt
	| macroStmt
;

macroStmt :
	macroConstStmt
	| macroIfThenElseStmt;

moduleBody : 
	moduleBodyElement (endOfLine+ moduleBodyElement)* endOfLine*;

moduleBodyElement : 
	functionStmt 
	| propertyGetStmt 
	| propertySetStmt 
	| propertyLetStmt 
	| subStmt 
	| macroStmt
;


// block ----------------------------------

attributeStmt : ATTRIBUTE WS implicitCallStmt_InStmt WS? EQ WS? literal (WS? ',' WS? literal)*;

block : blockStmt (endOfStatement blockStmt)* endOfStatement;

blockStmt :
	lineLabel
    | appactivateStmt
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
	| loadStmt
	| lockStmt
	| lsetStmt
	| macroStmt
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

appactivateStmt : APPACTIVATE WS valueStmt (WS? ',' WS? valueStmt)?;

beepStmt : BEEP;

chdirStmt : CHDIR WS valueStmt;

chdriveStmt : CHDRIVE WS valueStmt;

closeStmt : CLOSE (WS fileNumber (WS? ',' WS? fileNumber)*)?;

constStmt : (visibility WS)? CONST WS constSubStmt (WS? ',' WS? constSubStmt)*;

constSubStmt : ambiguousIdentifier typeHint? (WS asTypeClause)? WS? EQ WS? valueStmt;

dateStmt : DATE WS? EQ WS? valueStmt;

declareStmt : (visibility WS)? DECLARE WS (PTRSAFE WS)? ((FUNCTION typeHint?) | SUB) WS ambiguousIdentifier typeHint? WS LIB WS STRINGLITERAL (WS ALIAS WS STRINGLITERAL)? (WS? argList)? (WS asTypeClause)?;

deftypeStmt : 
	(
		DEFBOOL | DEFBYTE | DEFINT | DEFLNG | DEFCUR | 
		DEFSNG | DEFDBL | DEFDEC | DEFDATE | 
		DEFSTR | DEFOBJ | DEFVAR
	) WS
	letterrange (WS? ',' WS? letterrange)*
;

deleteSettingStmt : DELETESETTING WS valueStmt WS? ',' WS? valueStmt (WS? ',' WS? valueStmt)?;

doLoopStmt :
	DO endOfStatement 
	block?
	LOOP
	|
	DO WS (WHILE | UNTIL) WS valueStmt endOfStatement
	block?
	LOOP
	| 
	DO endOfStatement
	block
	LOOP WS (WHILE | UNTIL) WS valueStmt
;

endStmt : END;

enumerationStmt: 
	(visibility WS)? ENUM WS ambiguousIdentifier endOfStatement 
	enumerationStmt_Constant* 
	END_ENUM
;

enumerationStmt_Constant : ambiguousIdentifier (WS? EQ WS? valueStmt)? endOfStatement;

eraseStmt : ERASE WS valueStmt;

errorStmt : ERROR WS valueStmt;

eventStmt : (visibility WS)? EVENT WS ambiguousIdentifier WS? argList;

exitStmt : EXIT_DO | EXIT_FOR | EXIT_FUNCTION | EXIT_PROPERTY | EXIT_SUB;

filecopyStmt : FILECOPY WS valueStmt WS? ',' WS? valueStmt;

forEachStmt : 
	FOR WS EACH WS ambiguousIdentifier typeHint? WS IN WS valueStmt endOfStatement
	block?
	NEXT (WS ambiguousIdentifier)?
;

forNextStmt : 
	FOR WS ambiguousIdentifier typeHint? (WS asTypeClause)? WS? EQ WS? valueStmt WS TO WS valueStmt (WS STEP WS valueStmt)? endOfStatement 
	block?
	NEXT (WS ambiguousIdentifier)?
; 

functionStmt :
	(visibility WS)? (STATIC WS)? FUNCTION WS? ambiguousIdentifier typeHint? (WS? argList)? (WS? asTypeClause)? endOfStatement
	block?
	END_FUNCTION
;

getStmt : GET WS fileNumber WS? ',' WS? valueStmt? WS? ',' WS? valueStmt;

goSubStmt : GOSUB WS valueStmt;

goToStmt : GOTO WS valueStmt;

ifThenElseStmt : 
	IF WS ifConditionStmt WS THEN WS blockStmt (WS ELSE WS blockStmt)?	# inlineIfThenElse
	| ifBlockStmt ifElseIfBlockStmt* ifElseBlockStmt? END_IF			# blockIfThenElse
;

ifBlockStmt : 
	IF WS ifConditionStmt WS THEN endOfStatement 
	block?
;

ifConditionStmt : valueStmt;

ifElseIfBlockStmt : 
	ELSEIF WS ifConditionStmt WS THEN endOfStatement
	block?
;

ifElseBlockStmt : 
	ELSE endOfStatement 
	block?
;

implementsStmt : IMPLEMENTS WS ambiguousIdentifier;

inputStmt : INPUT WS fileNumber (WS? ',' WS? valueStmt)+;

killStmt : KILL WS valueStmt;

letStmt : (LET WS)? implicitCallStmt_InStmt WS? (EQ | PLUS_EQ | MINUS_EQ) WS? valueStmt;

lineInputStmt : LINE_INPUT WS fileNumber WS? ',' WS? valueStmt;

loadStmt : LOAD WS valueStmt;

lockStmt : LOCK WS valueStmt (WS? ',' WS? valueStmt (WS TO WS valueStmt)?)?;

lsetStmt : LSET WS implicitCallStmt_InStmt WS? EQ WS? valueStmt;

macroConstStmt : MACRO_CONST WS? ambiguousIdentifier WS? EQ WS? valueStmt;

macroIfThenElseStmt : macroIfBlockStmt macroElseIfBlockStmt* macroElseBlockStmt? MACRO_END_IF;

macroIfBlockStmt : 
	MACRO_IF WS? ifConditionStmt WS THEN endOfStatement
	(moduleDeclarations | moduleBody | block)*
;

macroElseIfBlockStmt : 
	MACRO_ELSEIF WS? ifConditionStmt WS THEN endOfStatement
	(moduleDeclarations | moduleBody | block)*
;

macroElseBlockStmt : 
	MACRO_ELSE endOfStatement
	(moduleDeclarations | moduleBody | block)*
;

midStmt : MID WS? LPAREN WS? argsCall WS? RPAREN;

mkdirStmt : MKDIR WS valueStmt;

nameStmt : NAME WS valueStmt WS AS WS valueStmt;

onErrorStmt : (ON_ERROR | ON_LOCAL_ERROR) WS (GOTO WS valueStmt | RESUME WS NEXT);

onGoToStmt : ON WS valueStmt WS GOTO WS valueStmt (WS? ',' WS? valueStmt)*;

onGoSubStmt : ON WS valueStmt WS GOSUB WS valueStmt (WS? ',' WS? valueStmt)*;

openStmt : 
	OPEN WS valueStmt WS FOR WS (APPEND | BINARY | INPUT | OUTPUT | RANDOM) 
	(WS ACCESS WS (READ | WRITE | READ_WRITE))?
	(WS (SHARED | LOCK_READ | LOCK_WRITE | LOCK_READ_WRITE))?
	WS AS WS fileNumber
	(WS LEN WS? EQ WS? valueStmt)?
;

outputList :
	outputList_Expression (WS? (';' | ',') WS? outputList_Expression?)*
	| outputList_Expression? (WS? (';' | ',') WS? outputList_Expression?)+
;

outputList_Expression : 
	valueStmt
	| (SPC | TAB) (WS? LPAREN WS? argsCall WS? RPAREN)?
;

printStmt : PRINT WS fileNumber WS? ',' (WS? outputList)?;

propertyGetStmt : 
	(visibility WS)? (STATIC WS)? PROPERTY_GET WS ambiguousIdentifier typeHint? (WS? argList)? (WS asTypeClause)? endOfStatement 
	block? 
	END_PROPERTY
;

propertySetStmt : 
	(visibility WS)? (STATIC WS)? PROPERTY_SET WS ambiguousIdentifier (WS? argList)? endOfStatement 
	block? 
	END_PROPERTY
;

propertyLetStmt : 
	(visibility WS)? (STATIC WS)? PROPERTY_LET WS ambiguousIdentifier (WS? argList)? endOfStatement 
	block? 
	END_PROPERTY
;

putStmt : PUT WS fileNumber WS? ',' WS? valueStmt? WS? ',' WS? valueStmt;

raiseEventStmt : RAISEEVENT WS ambiguousIdentifier (WS? LPAREN WS? (argsCall WS?)? RPAREN)?;

randomizeStmt : RANDOMIZE (WS valueStmt)?;

redimStmt : REDIM WS (PRESERVE WS)? redimSubStmt (WS?',' WS? redimSubStmt)*;

redimSubStmt : implicitCallStmt_InStmt WS? LPAREN WS? subscripts WS? RPAREN (WS asTypeClause)?;

resetStmt : RESET;

resumeStmt : RESUME (WS (NEXT | ambiguousIdentifier))?;

returnStmt : RETURN;

rmdirStmt : RMDIR WS valueStmt;

rsetStmt : RSET WS implicitCallStmt_InStmt WS? EQ WS? valueStmt;

savepictureStmt : SAVEPICTURE WS valueStmt WS? ',' WS? valueStmt;

saveSettingStmt : SAVESETTING WS valueStmt WS? ',' WS? valueStmt WS? ',' WS? valueStmt WS? ',' WS? valueStmt;

seekStmt : SEEK WS fileNumber WS? ',' WS? valueStmt;

selectCaseStmt : 
	SELECT WS CASE WS valueStmt endOfStatement 
	sC_Case*
	END_SELECT
;

sC_Selection :
    IS WS? comparisonOperator WS? valueStmt                       # caseCondIs
    | valueStmt WS TO WS valueStmt                                # caseCondTo
    | valueStmt                                                   # caseCondValue
;

sC_Case : 
	CASE WS sC_Cond endOfStatement
	block?
;

// ELSE first, so that it is not interpreted as a variable call
sC_Cond :
    ELSE                                                            # caseCondElse
    | sC_Selection (WS? ',' WS? sC_Selection)*                      # caseCondSelection
;

sendkeysStmt : SENDKEYS WS valueStmt (WS? ',' WS? valueStmt)?;

setattrStmt : SETATTR WS valueStmt WS? ',' WS? valueStmt;

setStmt : SET WS implicitCallStmt_InStmt WS? EQ WS? valueStmt;

stopStmt : STOP;

subStmt : 
	(visibility WS)? (STATIC WS)? SUB WS? ambiguousIdentifier (WS? argList)? endOfStatement
	block? 
	END_SUB
;

timeStmt : TIME WS? EQ WS? valueStmt;

typeStmt : 
	(visibility WS)? TYPE WS ambiguousIdentifier endOfStatement
	typeStmt_Element*
	END_TYPE
;

typeStmt_Element : ambiguousIdentifier (WS? LPAREN (WS? subscripts)? WS? RPAREN)? (WS asTypeClause)? endOfStatement;

typeOfStmt : TYPEOF WS valueStmt (WS IS WS type)?;

unloadStmt : UNLOAD WS valueStmt;

unlockStmt : UNLOCK WS fileNumber (WS? ',' WS? valueStmt (WS TO WS valueStmt)?)?;

// operator precedence is represented by rule order
valueStmt : 
	literal 												# vsLiteral
	| implicitCallStmt_InStmt 								# vsICS
	| LPAREN WS? valueStmt (WS? ',' WS? valueStmt)* RPAREN 	# vsStruct
	| NEW WS? valueStmt 										# vsNew
	| typeOfStmt 											# vsTypeOf
	| midStmt 												# vsMid
	| ADDRESSOF WS? valueStmt 								# vsAddressOf
	| implicitCallStmt_InStmt WS? ASSIGN WS? valueStmt 		# vsAssign
	
	| valueStmt WS? IS WS? valueStmt 							# vsIs
	| valueStmt WS? LIKE WS? valueStmt 						# vsLike
	| valueStmt WS? GEQ WS? valueStmt 						# vsGeq
	| valueStmt WS? LEQ WS? valueStmt 						# vsLeq
	| valueStmt WS? GT WS? valueStmt 						# vsGt
	| valueStmt WS? LT WS? valueStmt 						# vsLt
	| valueStmt WS? NEQ WS? valueStmt 						# vsNeq
	| valueStmt WS? EQ WS? valueStmt 						# vsEq

	| valueStmt WS? POW WS? valueStmt 						# vsPow
	| MINUS WS? valueStmt 									# vsNegation
	| PLUS WS? valueStmt 									# vsPlus
	| valueStmt WS? DIV WS? valueStmt 						# vsDiv
	| valueStmt WS? MULT WS? valueStmt 						# vsMult
	| valueStmt WS? MOD WS? valueStmt 						# vsMod
	| valueStmt WS? PLUS WS? valueStmt 						# vsAdd
	| valueStmt WS? MINUS WS? valueStmt 					# vsMinus
	| valueStmt WS? AMPERSAND WS? valueStmt 					# vsAmp

	| valueStmt WS? IMP WS? valueStmt 						# vsImp
	| valueStmt WS? EQV WS? valueStmt 						# vsEqv
	| valueStmt WS? XOR WS? valueStmt 						# vsXor
	| valueStmt WS? OR WS? valueStmt 						# vsOr
	| valueStmt WS? AND WS? valueStmt 						# vsAnd
	| NOT WS? valueStmt 										# vsNot
;

variableStmt : (DIM | STATIC | visibility) WS (WITHEVENTS WS)? variableListStmt;

variableListStmt : variableSubStmt (WS? ',' WS? variableSubStmt)*;

variableSubStmt : ambiguousIdentifier (WS? LPAREN WS? (subscripts WS?)? RPAREN WS?)? typeHint? (WS asTypeClause)?;

whileWendStmt : 
	WHILE WS valueStmt endOfStatement 
	block?
	WEND
;

widthStmt : WIDTH WS fileNumber WS? ',' WS? valueStmt;

withStmt : 
	WITH WS (implicitCallStmt_InStmt | (NEW WS type)) endOfStatement 
	block? 
	END_WITH
;

writeStmt : WRITE WS fileNumber WS? ',' (WS? outputList)?;


fileNumber : '#'? valueStmt;


// complex call statements ----------------------------------

explicitCallStmt : 
	eCS_ProcedureCall 
	| eCS_MemberProcedureCall 
;

// parantheses are required in case of args -> empty parantheses are removed
eCS_ProcedureCall : CALL WS ambiguousIdentifier typeHint? (WS? LPAREN WS? argsCall WS? RPAREN)? (WS? LPAREN subscripts RPAREN)*;



// parantheses are required in case of args -> empty parantheses are removed
eCS_MemberProcedureCall : CALL WS implicitCallStmt_InStmt? '.' ambiguousIdentifier typeHint? (WS? LPAREN WS? argsCall WS? RPAREN)? (WS? LPAREN subscripts RPAREN)*;


implicitCallStmt_InBlock :
	iCS_B_MemberProcedureCall 
	| iCS_B_ProcedureCall
;

iCS_B_MemberProcedureCall : implicitCallStmt_InStmt? '.' ambiguousIdentifier typeHint? (WS argsCall)? dictionaryCallStmt? (WS? LPAREN subscripts RPAREN)*;

// parantheses are forbidden in case of args
// variables cannot be called in blocks
// certainIdentifier instead of ambiguousIdentifier for preventing ambiguity with statement keywords 
iCS_B_ProcedureCall : certainIdentifier (WS argsCall)? (WS? LPAREN subscripts RPAREN)*;


// iCS_S_MembersCall first, so that member calls are not resolved as separate iCS_S_VariableOrProcedureCalls
implicitCallStmt_InStmt :
	iCS_S_MembersCall
	| iCS_S_VariableOrProcedureCall
	| iCS_S_ProcedureOrArrayCall
	| iCS_S_DictionaryCall
;

iCS_S_VariableOrProcedureCall : ambiguousIdentifier typeHint? dictionaryCallStmt? (WS? LPAREN subscripts RPAREN)*;

iCS_S_ProcedureOrArrayCall : (ambiguousIdentifier | baseType) typeHint? WS? LPAREN WS? (argsCall WS?)? RPAREN dictionaryCallStmt? (WS? LPAREN subscripts RPAREN)*;

iCS_S_MembersCall : (iCS_S_VariableOrProcedureCall | iCS_S_ProcedureOrArrayCall)? iCS_S_MemberCall+ dictionaryCallStmt? (WS? LPAREN subscripts RPAREN)*;

iCS_S_MemberCall : ('.' | '!') (iCS_S_VariableOrProcedureCall | iCS_S_ProcedureOrArrayCall);

iCS_S_DictionaryCall : dictionaryCallStmt;


// atomic call statements ----------------------------------

argsCall : (argCall? WS? (',' | ';') WS?)* argCall (WS? (',' | ';') WS? argCall?)*;

argCall : LPAREN? ((BYVAL | BYREF | PARAMARRAY) WS)? RPAREN? valueStmt;

dictionaryCallStmt : '!' ambiguousIdentifier typeHint?;


// atomic rules for statements

argList : LPAREN (WS? arg (WS? ',' WS? arg)*)? WS? RPAREN;

arg : (OPTIONAL WS)? ((BYVAL | BYREF) WS)? (PARAMARRAY WS)? ambiguousIdentifier typeHint? (WS? LPAREN WS? RPAREN)? (WS? asTypeClause)? (WS? argDefaultValue)?;

argDefaultValue : EQ WS? valueStmt;

subscripts : subscript (WS? ',' WS? subscript)*;

subscript : (valueStmt WS TO WS)? valueStmt;


// atomic rules ----------------------------------

ambiguousIdentifier : 
	(IDENTIFIER | ambiguousKeyword)+
;

asTypeClause : AS WS? (NEW WS)? type (WS? fieldLength)?;

baseType : BOOLEAN | BYTE | COLLECTION | DATE | DOUBLE | INTEGER | LONG | SINGLE | STRING | VARIANT;

certainIdentifier : 
	IDENTIFIER (ambiguousKeyword | IDENTIFIER)*
	| ambiguousKeyword (ambiguousKeyword | IDENTIFIER)+
;

comparisonOperator : LT | LEQ | GT | GEQ | EQ | NEQ | IS | LIKE;

complexType : ambiguousIdentifier (('.' | '!') ambiguousIdentifier)*;

fieldLength : MULT WS? (INTEGERLITERAL | ambiguousIdentifier);

letterrange : certainIdentifier (WS? MINUS WS? certainIdentifier)?;

lineLabel : ambiguousIdentifier ':';

literal : HEXLITERAL | OCTLITERAL | DATELITERAL | DOUBLELITERAL | INTEGERLITERAL | SHORTLITERAL | STRINGLITERAL | TRUE | FALSE | NOTHING | NULL;

type : (baseType | complexType) (WS? LPAREN WS? RPAREN)?;

typeHint : '&' | '%' | '#' | '!' | '@' | '$';

visibility : PRIVATE | PUBLIC | FRIEND | GLOBAL;

// ambiguous keywords
ambiguousKeyword : 
	ACCESS | ADDRESSOF | ALIAS | AND | ATTRIBUTE | APPACTIVATE | APPEND | AS |
	BEEP | BEGIN | BINARY | BOOLEAN | BYVAL | BYREF | BYTE | 
	CALL | CASE | CLASS | CLOSE | CHDIR | CHDRIVE | COLLECTION | CONST | 
	DATABASE | DATE | DECLARE | DEFBOOL | DEFBYTE | DEFCUR | DEFDBL | DEFDATE | DEFDEC | DEFINT | DEFLNG | DEFOBJ | DEFSNG | DEFSTR | DEFVAR | DELETESETTING | DIM | DO | DOUBLE | 
	EACH | ELSE | ELSEIF | END | ENUM | EQV | ERASE | ERROR | EVENT | 
	FALSE | FILECOPY | FRIEND | FOR | FUNCTION | 
	GET | GLOBAL | GOSUB | GOTO | 
	IF | IMP | IMPLEMENTS | IN | INPUT | IS | INTEGER |
	KILL | 
	LOAD | LOCK | LONG | LOOP | LEN | LET | LIB | LIKE | LSET |
	ME | MID | MKDIR | MOD | 
	NAME | NEXT | NEW | NOT | NOTHING | NULL | 
	ON | OPEN | OPTIONAL | OR | OUTPUT | 
	PARAMARRAY | PRESERVE | PRINT | PRIVATE | PUBLIC | PUT |
	RANDOM | RANDOMIZE | RAISEEVENT | READ | REDIM | REM | RESET | RESUME | RETURN | RMDIR | RSET |
	SAVEPICTURE | SAVESETTING | SEEK | SELECT | SENDKEYS | SET | SETATTR | SHARED | SINGLE | SPC | STATIC | STEP | STOP | STRING | SUB | 
	TAB | TEXT | THEN | TIME | TO | TRUE | TYPE | TYPEOF | 
	UNLOAD | UNLOCK | UNTIL | 
	VARIANT | VERSION | 
	WEND | WHILE | WIDTH | WITH | WITHEVENTS | WRITE |
	XOR
;

remComment : REMCOMMENT;

comment : COMMENT;

endOfLine : WS? (NEWLINE | comment | remComment) WS?;

endOfStatement : (endOfLine | WS? COLON WS?)*;


// lexer rules --------------------------------------------------------------------------------


// keywords
ACCESS : A C C E S S;
ADDRESSOF : A D D R E S S O F;
ALIAS : A L I A S;
AND : A N D;
ATTRIBUTE : A T T R I B U T E;
APPACTIVATE : A P P A C T I V A T E;
APPEND : A P P E N D;
AS : A S;
BEGIN : B E G I N;
BEEP : B E E P;
BINARY : B I N A R Y;
BOOLEAN : B O O L E A N;
BYVAL : B Y V A L;
BYREF : B Y R E F;
BYTE : B Y T E;
CALL : C A L L;
CASE : C A S E;
CHDIR : C H D I R;
CHDRIVE : C H D R I V E;
CLASS : C L A S S;
CLOSE : C L O S E;
COLLECTION : C O L L E C T I O N;
CONST : C O N S T;
DATABASE : D A T A B A S E;
DATE : D A T E;
DECLARE : D E C L A R E;
DEFBOOL : D E F B O O L; 
DEFBYTE : D E F B Y T E;
DEFDATE : D E F D A T E;
DEFDBL : D E F D B L;
DEFDEC : D E F D E C;
DEFCUR : D E F C U R;
DEFINT : D E F I N T;
DEFLNG : D E F L N G;
DEFOBJ : D E F O B J;
DEFSNG : D E F S N G;
DEFSTR : D E F S T R;
DEFVAR : D E F V A R;
DELETESETTING : D E L E T E S E T T I N G;
DIM : D I M;
DO : D O;
DOUBLE : D O U B L E;
EACH : E A C H;
ELSE : E L S E;
ELSEIF : E L S E I F;
END_ENUM : E N D WS E N U M;
END_FUNCTION : E N D WS F U N C T I O N;
END_IF : E N D WS I F;
END_PROPERTY : E N D WS P R O P E R T Y;
END_SELECT : E N D WS S E L E C T;
END_SUB : E N D WS S U B;
END_TYPE : E N D WS T Y P E;
END_WITH : E N D WS W I T H;
END : E N D;
ENUM : E N U M;
EQV : E Q V;
ERASE : E R A S E;
ERROR : E R R O R;
EVENT : E V E N T;
EXIT_DO : E X I T WS D O;
EXIT_FOR : E X I T WS F O R;
EXIT_FUNCTION : E X I T WS F U N C T I O N;
EXIT_PROPERTY : E X I T WS P R O P E R T Y;
EXIT_SUB : E X I T WS S U B;
FALSE : F A L S E;
FILECOPY : F I L E C O P Y;
FRIEND : F R I E N D;
FOR : F O R;
FUNCTION : F U N C T I O N;
GET : G E T;
GLOBAL : G L O B A L;
GOSUB : G O S U B;
GOTO : G O T O;
IF : I F;
IMP : I M P;
IMPLEMENTS : I M P L E M E N T S;
IN : I N;
INPUT : I N P U T;
IS : I S;
INTEGER : I N T E G E R;
KILL: K I L L;
LOAD : L O A D;
LOCK : L O C K;
LONG : L O N G;
LOOP : L O O P;
LEN : L E N;
LET : L E T;
LIB : L I B;
LIKE : L I K E;
LINE_INPUT : L I N E WS I N P U T;
LOCK_READ : L O C K WS R E A D;
LOCK_WRITE : L O C K WS W R I T E;
LOCK_READ_WRITE : L O C K WS R E A D WS W R I T E;
LSET : L S E T;
MACRO_CONST : '#' C O N S T;
MACRO_IF : '#' I F;
MACRO_ELSEIF : '#' E L S E I F;
MACRO_ELSE : '#' E L S E;
MACRO_END_IF : '#' E N D WS? I F;
ME : M E;
MID : M I D;
MKDIR : M K D I R;
MOD : M O D;
NAME : N A M E;
NEXT : N E X T;
NEW : N E W;
NOT : N O T;
NOTHING : N O T H I N G;
NULL : N U L L;
ON : O N;
ON_ERROR : O N WS E R R O R;
ON_LOCAL_ERROR : O N WS L O C A L WS E R R O R;
OPEN : O P E N;
OPTIONAL : O P T I O N A L;
OPTION_BASE : O P T I O N WS B A S E;
OPTION_EXPLICIT : O P T I O N WS E X P L I C I T;
OPTION_COMPARE : O P T I O N WS C O M P A R E;
OPTION_PRIVATE_MODULE : O P T I O N WS P R I V A T E WS M O D U L E;
OR : O R;
OUTPUT : O U T P U T;
PARAMARRAY : P A R A M A R R A Y;
PRESERVE : P R E S E R V E;
PRINT : P R I N T;
PRIVATE : P R I V A T E;
PROPERTY_GET : P R O P E R T Y WS G E T;
PROPERTY_LET : P R O P E R T Y WS L E T;
PROPERTY_SET : P R O P E R T Y WS S E T;
PTRSAFE : P T R S A F E;
PUBLIC : P U B L I C;
PUT : P U T;
RANDOM : R A N D O M;
RANDOMIZE : R A N D O M I Z E;
RAISEEVENT : R A I S E E V E N T;
READ : R E A D;
READ_WRITE : R E A D WS W R I T E;
REDIM : R E D I M;
REM : R E M;
RESET : R E S E T;
RESUME : R E S U M E;
RETURN : R E T U R N;
RMDIR : R M D I R;
RSET : R S E T;
SAVEPICTURE : S A V E P I C T U R E;
SAVESETTING : S A V E S E T T I N G;
SEEK : S E E K;
SELECT : S E L E C T;
SENDKEYS : S E N D K E Y S;
SET : S E T;
SETATTR : S E T A T T R;
SHARED : S H A R E D;
SINGLE : S I N G L E;
SPC : S P C;
STATIC : S T A T I C;
STEP : S T E P;
STOP : S T O P;
STRING : S T R I N G;
SUB : S U B;
TAB : T A B;
TEXT : T E X T;
THEN : T H E N;
TIME : T I M E;
TO : T O;
TRUE : T R U E;
TYPE : T Y P E;
TYPEOF : T Y P E O F;
UNLOAD : U N L O A D;
UNLOCK : U N L O C K;
UNTIL : U N T I L;
VARIANT : V A R I A N T;
VERSION : V E R S I O N;
WEND : W E N D;
WHILE : W H I L E;
WIDTH : W I D T H;
WITH : W I T H;
WITHEVENTS : W I T H E V E N T S;
WRITE : W R I T E;
XOR : X O R;


// symbols
AMPERSAND : '&';
ASSIGN : ':=';
DIV : '\\' | '/';
EQ : '=';
GEQ : '>=';
GT : '>';
LEQ : '<=';
LPAREN : '(';
LT : '<';
MINUS : '-';
MINUS_EQ : '-=';
MULT : '*';
NEQ : '<>';
PLUS : '+';
PLUS_EQ : '+=';
POW : '^';
RPAREN : ')';
L_SQUARE_BRACKET : '[';
R_SQUARE_BRACKET : ']';


// literals
STRINGLITERAL : '"' (~["\r\n] | '""')* '"';
OCTLITERAL : '&O' [0-8]+ '&'?;
HEXLITERAL : '&H' [0-9A-F]+ '&'?;
SHORTLITERAL : (PLUS|MINUS)? DIGIT+ ('#' | '&' | '@')?;
INTEGERLITERAL : SHORTLITERAL (E SHORTLITERAL)?;
DOUBLELITERAL : (PLUS|MINUS)? DIGIT* '.' DIGIT+ (E SHORTLITERAL)?;

DATELITERAL : '#' DATEORTIME '#';
fragment DATEORTIME : DATEVALUE WS? TIMEVALUE | DATEVALUE | TIMEVALUE;
fragment DATEVALUE : DATEVALUEPART DATESEPARATOR DATEVALUEPART (DATESEPARATOR DATEVALUEPART)?;
fragment DATEVALUEPART : DIGIT+ | MONTHNAME;
fragment DATESEPARATOR : WS? [/,-]? WS?;
fragment MONTHNAME : ENGLISHMONTHNAME | ENGLISHMONTHABBREVIATION;
fragment ENGLISHMONTHNAME : J A N U A R Y | F E B R U A R Y | M A R C H | A P R I L | M A Y | J U N E  | A U G U S T | S E P T E M B E R | O C T O B E R | N O V E M B E R | D E C E M B E R;
fragment ENGLISHMONTHABBREVIATION : J A N | F E B | M A R | A P R | J U N | J U L | A U G | S E P |  O C T | N O V | D E C;
fragment TIMEVALUE : DIGIT+ AMPM | DIGIT+ TIMESEPARATOR DIGIT+ (TIMESEPARATOR DIGIT+)? AMPM?;
fragment TIMESEPARATOR : WS? (':' | '.') WS?;
fragment AMPM : WS? (A M | P M | A | P);

// whitespace, line breaks, comments, ...
LINE_CONTINUATION : [ \t]+ UNDERSCORE '\r'? '\n' -> skip;
NEWLINE : [\r\n\u2028\u2029]+;
REMCOMMENT : COLON? REM WS (LINE_CONTINUATION | ~[\r\n\u2028\u2029])*;
COMMENT : SINGLEQUOTE (LINE_CONTINUATION | ~[\r\n\u2028\u2029])*;
SINGLEQUOTE : '\'';
COLON : ':';
UNDERSCORE : '_';
WS : ([ \t] | LINE_CONTINUATION)+;

// identifier
IDENTIFIER :  ~[\]()\r\n\t.,'"|!@#$%^&*\-+:=; ]+ | L_SQUARE_BRACKET (~[!\]\r\n])+ R_SQUARE_BRACKET;


// letters
fragment LETTER : [a-zA-Z_äöüÄÖÜ];
fragment DIGIT : [0-9];
fragment LETTERORDIGIT : [a-zA-Z0-9_äöüÄÖÜ];

// case insensitive chars
fragment A:('a'|'A');
fragment B:('b'|'B');
fragment C:('c'|'C');
fragment D:('d'|'D');
fragment E:('e'|'E');
fragment F:('f'|'F');
fragment G:('g'|'G');
fragment H:('h'|'H');
fragment I:('i'|'I');
fragment J:('j'|'J');
fragment K:('k'|'K');
fragment L:('l'|'L');
fragment M:('m'|'M');
fragment N:('n'|'N');
fragment O:('o'|'O');
fragment P:('p'|'P');
fragment Q:('q'|'Q');
fragment R:('r'|'R');
fragment S:('s'|'S');
fragment T:('t'|'T');
fragment U:('u'|'U');
fragment V:('v'|'V');
fragment W:('w'|'W');
fragment X:('x'|'X');
fragment Y:('y'|'Y');
fragment Z:('z'|'Z');
