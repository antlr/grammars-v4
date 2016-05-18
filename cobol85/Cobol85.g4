/*
* Copyright (C) 2016, Ulrich Wolffgang <u.wol@wwu.de>
* All rights reserved.
*
* This software may be modified and distributed under the terms
* of the BSD 3-clause license. See the LICENSE file for details.
*/

/*
* Cobol 85 Grammar for ANTLR4
*
* This is an approximate grammar for Cobol 85. It is akin but neither
* copied from nor identical to Cobol.jj, Cobol.kg and VS COBOL II grammars.
* Tested against the NIST test suite.
*
* Characteristics:
*
* 1. Passes the NIST tests.
*
* 2. To be used in conjunction with the provided preprocessor, which executes
*    COPY and REPLACE statements.
*
*
* Known limitations (work under progress):
*
* 1. Picture strings are parsed as (groups of) terminal symbols.
*
* 2. Comments are skipped.
*
*
* Change log:
*
* v1.3
*	- data description fixes
*	- picture string fixes
*
* v1.2
*	- fixes
*
* v1.1
*	- DFHRESP and DFHVALUE
*	- CALL BY VALUE
*	- REMARKS paragraph
*
* v1.0
*	- fixes
*	- compiler options
*
* v0.9 Initial revision
*/

grammar Cobol85;

options
{
	language = Java;
}

startRule : compilationUnit EOF;

compilationUnit :
	programUnit+
;

programUnit :
	compilerOptions?
	identificationDivision
	environmentDivision?
	dataDivision?
	procedureDivision?
	programUnit*
	endProgramStatement?
;

endProgramStatement :
	END PROGRAM programName DOT_FS
;

// --- compiler options --------------------------------------------------------------------

compilerOptions :
	(PROCESS compilerOption+)+
;

compilerOption :
	APOST
	| ARITH LPARENCHAR EXTEND RPARENCHAR
	| CODEPAGE LPARENCHAR literal RPARENCHAR
	| DBCS
	| LIB
	| NOSEQ
	| NOSTDTRUNC
	| OPTIMIZE LPARENCHAR FULL RPARENCHAR
	| XOPTS LPARENCHAR compilerSubOption+ RPARENCHAR
;

compilerSubOption :
	SP
	| APOST
;

// --- identification division --------------------------------------------------------------------

identificationDivision :
	(IDENTIFICATION | ID) DIVISION DOT_FS
	programIdParagraph
	identificationDivisionBody*
;

identificationDivisionBody :
	authorParagraph
	| installationParagraph
	| dateWrittenParagraph
	| dateCompiledParagraph
	| securityParagraph
	| remarksParagraph
;

// - program id paragraph ----------------------------------

programIdParagraph :
	PROGRAM_ID DOT_FS programName (IS? (COMMON | INITIAL | LIBRARY | DEFINITION) PROGRAM?)? DOT_FS
;

// - author paragraph ----------------------------------

authorParagraph :
	AUTHOR DOT_FS commentEntry?
;

// - installation paragraph ----------------------------------

installationParagraph :
	INSTALLATION DOT_FS commentEntry?
;

// - date written paragraph ----------------------------------

dateWrittenParagraph :
	DATE_WRITTEN DOT_FS commentEntry?
;

// - date compiled paragraph ----------------------------------

dateCompiledParagraph :
	DATE_COMPILED DOT_FS commentEntry?
;

// - security paragraph ----------------------------------

securityParagraph :
	SECURITY DOT_FS commentEntry?
;

// - remarks paragraph ----------------------------------

remarksParagraph :
	REMARKS DOT_FS commentEntry?
;

// --- environment division --------------------------------------------------------------------

environmentDivision :
	ENVIRONMENT DIVISION DOT_FS
	environmentDivisionBody*
;

environmentDivisionBody :
	configurationSection
	| specialNamesParagraph
	| inputOutputSection
;

// -- configuration section ----------------------------------

configurationSection :
	CONFIGURATION SECTION DOT_FS
	configurationSectionParagraph*
;

// - configuration section paragraph ----------------------------------

configurationSectionParagraph :
	sourceComputerParagraph
	| objectComputerParagraph
;

// - source computer paragraph ----------------------------------

sourceComputerParagraph :
	SOURCE_COMPUTER DOT_FS computerName (WITH? DEBUGGING MODE)? DOT_FS
;

// - object computer paragraph ----------------------------------

objectComputerParagraph :
	OBJECT_COMPUTER DOT_FS
	computerName objectComputerClause* DOT_FS
;

objectComputerClause :
	memorySizeClause
	| diskSizeClause
	| collatingSequenceClause
	| segmentLimitClause
	| characterSetClause
;

memorySizeClause :
	MEMORY SIZE? (integerLiteral | cobolWord) (WORDS | CHARACTERS | MODULES)?
;

diskSizeClause :
	DISK SIZE? IS? (integerLiteral | cobolWord) (WORDS | MODULES)?
;

collatingSequenceClause :
	PROGRAM? COLLATING? SEQUENCE IS? alphabetName+
;

segmentLimitClause :
	SEGMENT_LIMIT IS? integerLiteral
;

characterSetClause :
	CHARACTER SET commentEntry?
;

// - special names paragraph ----------------------------------

specialNamesParagraph :
	SPECIAL_NAMES DOT_FS
	(specialNameClause+ DOT_FS)?
;

specialNameClause :
	channelClause
	| odtClause
	| alphabetClause
	| classClause
	| currencySignClause
	| decimalPointClause
	| symbolicCharactersClause
	| environmentSwitchNameClause
;

channelClause :
	CHANNEL integerLiteral IS? mnemonicName
;

odtClause :
	ODT IS? mnemonicName
;

alphabetClause :
	ALPHABET alphabetName (FOR ALPHANUMERIC)? IS?
	(
		EBCDIC
		| ASCII
		| STANDARD_1
		| STANDARD_2
		| NATIVE
		| cobolWord
		| (literal
			(
				(
					(THROUGH | THRU) literal | (ALSO literal)+
				)
			)?
		)+
	)
;

classClause :
	CLASS className (FOR? (ALPHANUMERIC | NATIONAL))? IS?
	(
		(identifier | literal) ((THROUGH | THRU) (identifier | literal))?
	)+
;

currencySignClause :
	CURRENCY SIGN? IS? literal
;

decimalPointClause :
	DECIMAL_POINT IS? COMMA
;

symbolicCharactersClause :
	SYMBOLIC CHARACTERS? (FOR? (ALPHANUMERIC | NATIONAL))? (symbolicCharacter+ (IS | ARE)? integerLiteral+)+ (IN alphabetName)?
;

environmentSwitchNameClause :
	environmentName IS? mnemonicName specialNamesStatusPhrase?
	| specialNamesStatusPhrase
;

specialNamesStatusPhrase :
	ON STATUS? IS? condition (OFF STATUS? IS? condition)?
	| OFF STATUS? IS? condition (ON STATUS? IS? condition)?
;

// -- input output section ----------------------------------

inputOutputSection :
	INPUT_OUTPUT SECTION DOT_FS
	inputOutputSectionParagraph*
;

// - input output section paragraph ----------------------------------

inputOutputSectionParagraph :
	fileControlParagraph
	| ioControlParagraph
;

// - file control paragraph ----------------------------------

fileControlParagraph :
	FILE_CONTROL (DOT_FS? fileControlEntry)+ DOT_FS
;

fileControlEntry :
	selectClause
	fileControlClause*
;

fileControlClause :
	assignClause
	| reserveClause
	| organizationClause
	| paddingCharacterClause
	| recordDelimiterClause
	| accessModeClause
	| recordKeyClause
	| alternateRecordKeyClause
	| fileStatusClause
	| passwordClause
	| relativeKeyClause
;

assignClause :
	ASSIGN TO? (assignmentName | literal)
;

selectClause :
	SELECT OPTIONAL? fileName
;

reserveClause :
	RESERVE (integerLiteral | NO) ALTERNATE? (AREA | AREAS)?
;

organizationClause :
	(ORGANIZATION IS?)?
	(LINE | RECORD BINARY | RECORD | BINARY)?
	(SEQUENTIAL | RELATIVE | INDEXED)
;

paddingCharacterClause :
	PADDING CHARACTER? IS? (qualifiedDataName | literal)
;

recordDelimiterClause :
	RECORD DELIMITER IS? (STANDARD_1 | IMPLICIT | assignmentName)
;

accessModeClause :
	ACCESS MODE? IS?
	(
		SEQUENTIAL
		| RANDOM
		| DYNAMIC
		| EXCLUSIVE
	)
;

recordKeyClause :
	RECORD KEY? IS? qualifiedDataName passwordClause? (WITH? DUPLICATES)?
;

alternateRecordKeyClause :
	ALTERNATE RECORD KEY? IS? qualifiedDataName passwordClause? (WITH? DUPLICATES)?
;

passwordClause :
	PASSWORD IS? dataName
;

fileStatusClause :
	FILE? STATUS IS? qualifiedDataName qualifiedDataName?
;

relativeKeyClause :
	RELATIVE KEY? IS? qualifiedDataName
;

// - io control paragraph ----------------------------------

ioControlParagraph :
	I_O_CONTROL DOT_FS
	(fileName DOT_FS)?
	(ioControlClause (DOT_FS? ioControlClause)* DOT_FS)?
;

ioControlClause :
	rerunClause
	| sameAreaClause
	| multipleFileClause
	| commitmentControlClause
;

rerunClause :
	RERUN
	(
		ON (assignmentName | fileName)
	)?
	EVERY (rerunEveryRecords | rerunEveryOf | rerunEveryClock)
;

rerunEveryRecords :
	integerLiteral RECORDS
;

rerunEveryOf :
	END? OF? (REEL | UNIT) OF fileName
;

rerunEveryClock :
	integerLiteral CLOCK_UNITS?
;

sameAreaClause :
	SAME (RECORD | SORT | SORT_MERGE)? AREA? FOR? fileName+
;

multipleFileClause :
	MULTIPLE FILE TAPE? CONTAINS? (fileName POSITION? integerLiteral?)+
;

commitmentControlClause :
	COMMITMENT CONTROL FOR? fileName
;

// --- data division --------------------------------------------------------------------

dataDivision :
	DATA DIVISION DOT_FS
	dataDivisionBody*
;

dataDivisionBody :
	fileSection
	| workingStorageSection
	| linkageSection
	| communicationSection
	| screenSection
	| reportSection
;

// -- file section ----------------------------------

fileSection :
	FILE SECTION DOT_FS
	(fileAndSortDescriptionEntry dataDescriptionEntry*)*
;

fileAndSortDescriptionEntry :
	(FD | SD) fileName (DOT_FS? fileAndSortDescriptionEntryClause)* DOT_FS
;

fileAndSortDescriptionEntryClause :
	externalClause
	| globalClause
	| blockContainsClause
	| recordContainsClause
	| labelRecordsClause
	| valueOfClause
	| dataRecordClause
	| linageClause
	| codeSetClause
	| reportClause
	| recordingModeClause
;

externalClause :
	IS? EXTERNAL
;

globalClause :
	IS? GLOBAL
;

blockContainsClause :
	BLOCK CONTAINS? (integerLiteral TO)? integerLiteral (RECORDS | CHARACTERS)?
;

recordContainsClause :
	RECORD CONTAINS?
	(
		(integerLiteral TO)? integerLiteral CHARACTERS?
		|
		IS? VARYING IN? SIZE?
		(FROM? integerLiteral (TO integerLiteral)? CHARACTERS?)?
		(DEPENDING ON? qualifiedDataName)?
	)
;

labelRecordsClause :
	LABEL (RECORD IS? | RECORDS ARE?) (OMITTED | STANDARD | dataName+)
;

valueOfClause :
	VALUE OF (systemName IS? (qualifiedDataName | literal))+
;

dataRecordClause :
	DATA (RECORD IS? | RECORDS ARE?) dataName+
;

linageClause :
	LINAGE IS? (dataName | integerLiteral) LINES?
	(
		WITH? FOOTING AT? (dataName | integerLiteral)
		| LINES? AT? TOP (dataName | integerLiteral)
		| LINES? AT? BOTTOM (dataName | integerLiteral)
	)*
;

recordingModeClause :
	RECORDING MODE? IS? modeStatement
;

modeStatement :
	cobolWord
;

codeSetClause :
	CODE_SET IS? alphabetName
;

reportClause :
	(REPORT IS? | REPORTS ARE?) reportName+
;

dataDescriptionEntry :
	dataDescriptionEntryFormat1
	| dataDescriptionEntryFormat2
	| dataDescriptionEntryFormat3
;

dataDescriptionEntryFormat1 :
	(INTEGERLITERAL | LEVEL_NUMBER_77)
	(dataName | FILLER)?
	dataExternalClause?
	dataGlobalClause?
	dataRedefinesClause?
	(
		dataPictureClause
		| dataValueClause
		| dataUsageClause
		| dataOccursClause
		| dataSignClause
		| dataSynchronizedClause
		| dataJustifiedClause
		| dataBlankWhenZeroClause
	)*
	DOT_FS
;

dataDescriptionEntryFormat2 :
	LEVEL_NUMBER_66 dataName renamesClause DOT_FS
;

dataDescriptionEntryFormat3 :
	LEVEL_NUMBER_88 conditionName dataValueClause DOT_FS
;

dataRedefinesClause :
	REDEFINES dataName
;

dataBlankWhenZeroClause :
	BLANK WHEN? (ZERO | ZEROS | ZEROES)
;

dataJustifiedClause :
	(JUSTIFIED | JUST) RIGHT?
;

dataOccursClause :
	OCCURS (integerLiteral TO)? integerLiteral TIMES?
	(DEPENDING ON? qualifiedDataName)?
	(
		(ASCENDING | DESCENDING) KEY? IS? qualifiedDataName+
	)*
	(INDEXED BY? indexName+)?
;

dataPictureClause :
	(PICTURE | PIC) IS? pictureString
;

pictureString :
	(pictureChars+ pictureCardinality?)+
;

pictureChars :
	DOLLARCHAR
	| IDENTIFIER
	| integerLiteral
	| pictureCharsKeyword
	| NUMERICLITERAL
	| SLASHCHAR
	| COMMACHAR
	| DOT
	| COLONCHAR
	| ASTERISKCHAR
	| DOUBLEASTERISKCHAR
	| LPARENCHAR
	| RPARENCHAR
	| PLUSCHAR
	| MINUSCHAR
	| LESSTHANCHAR
	| MORETHANCHAR
;

pictureCardinality :
	LPARENCHAR integerLiteral RPARENCHAR
;

dataExternalClause :
	IS? EXTERNAL
;

dataGlobalClause :
	IS? GLOBAL
;

dataUsageClause :
	(USAGE IS?)?
	(
		BINARY (TRUNCATED | EXTENDED)?
		| COMP
		| COMP_1
		| COMP_2
		| COMP_3
		| COMP_4
		| COMPUTATIONAL
		| COMPUTATIONAL_1
		| COMPUTATIONAL_2
		| COMPUTATIONAL_3
		| COMPUTATIONAL_4
		| DATE
		| DISPLAY
		| DISPLAY_1
		| FUNCTION_POINTER
		| INDEX
		| NATIONAL
		| PACKED_DECIMAL
		| POINTER
		| PROCEDURE_POINTER
	)
;

dataSignClause :
	(SIGN IS?)? (LEADING | TRAILING) (SEPARATE CHARACTER?)?
;

dataSynchronizedClause :
	(SYNCHRONIZED | SYNC) (LEFT | RIGHT)?
;

dataValueClause :
	(VALUE IS? | VALUES ARE?)?
	(
		(literal | cobolWord)
		(
			(THROUGH | THRU) literal
		)?
	)+
;

renamesClause :
	RENAMES qualifiedDataName
	(
		(THROUGH | THRU) qualifiedDataName
	)?
;

// -- working storage section ----------------------------------

workingStorageSection :
	WORKING_STORAGE SECTION DOT_FS
	dataDescriptionEntry*
;

// -- linkage section ----------------------------------

linkageSection :
	LINKAGE SECTION DOT_FS
	dataDescriptionEntry*
;

// -- communication section ----------------------------------

communicationSection :
	COMMUNICATION SECTION DOT_FS
	(communicationDescriptionEntry | dataDescriptionEntry)*
;

communicationDescriptionEntry :
	communicationDescriptionEntryFormat1
	| communicationDescriptionEntryFormat2
	| communicationDescriptionEntryFormat3
;

communicationDescriptionEntryFormat1 :
	CD cdName FOR? INITIAL? INPUT
	(
		(
			SYMBOLIC? QUEUE IS? dataDescName
			| SYMBOLIC? SUB_QUEUE_1 IS? dataDescName
			| SYMBOLIC? SUB_QUEUE_2 IS? dataDescName
			| SYMBOLIC? SUB_QUEUE_3 IS? dataDescName
			| MESSAGE DATE IS? dataDescName
			| MESSAGE TIME IS? dataDescName
			| SYMBOLIC? SOURCE IS? dataDescName
			| TEXT LENGTH IS? dataDescName
			| END KEY IS? dataDescName
			| STATUS KEY IS? dataDescName
			| MESSAGE? COUNT IS? dataDescName
		)
		| dataDescName
	)*
	DOT_FS
;

communicationDescriptionEntryFormat2 :
	CD cdName FOR? OUTPUT
	(
		DESTINATION COUNT IS? dataDescName
		| TEXT LENGTH IS? dataDescName
		| STATUS KEY IS? dataDescName
		| DESTINATION TABLE OCCURS integerLiteral TIMES (INDEXED BY indexName+)?
		| ERROR KEY IS? dataDescName
		| SYMBOLIC? DESTINATION IS? dataDescName
	)*
	DOT_FS
;

communicationDescriptionEntryFormat3 :
	CD cdName FOR? INITIAL I_O
	( 	
		(
			MESSAGE DATE IS? dataDescName
			| MESSAGE TIME IS? dataDescName
			| SYMBOLIC? TERMINAL IS? dataDescName
			| TEXT LENGTH IS? dataDescName
			| END KEY IS? dataDescName
			| STATUS KEY IS? dataDescName
		)
		| dataDescName+
	)*
	DOT_FS
;

// -- screen section ----------------------------------

screenSection :
	SCREEN SECTION DOT_FS
;

// -- report section ----------------------------------

reportSection :
	REPORT SECTION DOT_FS
	(
		reportDescriptionEntry reportGroupDescriptionEntry+
	)*
;

reportDescriptionEntry :
	RD reportName
	(IS? GLOBAL)?
	(
		PAGE (LIMIT IS? | LIMITS ARE?)? integerLiteral (LINE | LINES)?
		(HEADING integerLiteral)?
		(FIRST DETAIL integerLiteral)?
		(LAST DETAIL integerLiteral)?
		(FOOTING integerLiteral)?
	)?
	DOT_FS
;

reportGroupDescriptionEntry :
	reportGroupDescriptionEntryFormat1
	| reportGroupDescriptionEntryFormat2
	| reportGroupDescriptionEntryFormat3
;

reportGroupDescriptionEntryFormat1 :
	integerLiteral dataName
	(
		LINE NUMBER? IS?
		(
			integerLiteral (ON? NEXT PAGE)?
			| PLUS integerLiteral
			| NEXT PAGE
		)
	)?
	
	(
		NEXT GROUP IS?
		(
			integerLiteral | PLUS integerLiteral | NEXT PAGE
		)
	)?
	
	TYPE IS? (
		(REPORT HEADING | RH)
		| (PAGE HEADING | PH)
		| (CONTROL HEADING | CH) (dataName | FINAL)
		| (DETAIL | DE)
		| (CONTROL FOOTING | CF) (dataName | FINAL)
		| (PAGE FOOTING | PF)
		| (REPORT FOOTING | RF)
	)
	
	(
		USAGE IS? (DISPLAY | DISPLAY_1)
	)?
	DOT_FS
;

reportGroupDescriptionEntryFormat2 :
	integerLiteral dataName?
	(
		LINE NUMBER? IS?
		(
			integerLiteral (ON? NEXT PAGE)?
			| PLUS integerLiteral
			| NEXT PAGE
		)
	)?
	(
		USAGE IS? (DISPLAY | DISPLAY_1)	
	)
	DOT_FS
;

reportGroupDescriptionEntryFormat3 :
	integerLiteral dataName?
	(
		dataPictureClause
		| (USAGE IS? (DISPLAY | DISPLAY_1))
		| (SIGN IS? (LEADING | TRAILING) SEPARATE CHARACTER?)
		| ((JUSTIFIED | JUST) RIGHT?)
		| (BLANK WHEN? ZERO)
		| (
			LINE? NUMBER? IS? (
		 		integerLiteral (ON? NEXT PAGE)?
				| PLUS integerLiteral
			)
		)
		| (COLUMN NUMBER? IS? integerLiteral)
		| (
			SOURCE IS? identifier
			| VALUE IS? literal
			| (
				SUM identifier (COMMACHAR identifier)+
				(UPON dataName (COMMACHAR dataName)+)?
			)
			| (RESET ON? (FINAL | dataName))
		)
		| (GROUP INDICATE?)
	)*
	DOT_FS
;

// --- procedure division --------------------------------------------------------------------

procedureDivision :
	PROCEDURE DIVISION (USING dataName+)? (GIVING dataName)? DOT_FS
	procedureDeclaratives?
	procedureDivisionBody
;

procedureDeclaratives :
	DECLARATIVES DOT_FS
	(
		procedureSectionHeader DOT_FS
		useStatement DOT_FS
		paragraphs
	)+
	END DECLARATIVES DOT_FS
;

procedureDivisionBody :
	paragraphs procedureSection*
;

procedureSectionHeader :
	sectionName SECTION integerLiteral?
;

// -- procedure section ----------------------------------

procedureSection :
	procedureSectionHeader DOT_FS paragraphs
;

paragraphs :
	sentence* paragraph*
;

paragraph :
	paragraphName DOT_FS
	(
		exitStatement
		| alteredGoTo
		| sentence*
	)
;

sentence :
	statements DOT_FS
;

statements :
	statement*
;

statement :
	acceptStatement
	| addStatement
	| alterStatement
	| callStatement
	| cancelStatement
	| closeStatement
	| computeStatement
	| continueStatement
	| deleteStatement
	| disableStatement
	| displayStatement
	| divideStatement
	| enableStatement
	| entryStatement
	| evaluateStatement
	| exitProgramStatement
	| exitStatement
	| generateStatement
	| gobackStatement
	| goToStatement
	| ifStatement
	| initializeStatement
	| initiateStatement
	| inspectStatement
	| mergeStatement
	| moveStatement
	| multiplyStatement
	| openStatement
	| performStatement
	| purgeStatement
	| readStatement
	| receiveStatement
	| releaseStatement
	| returnStatement
	| rewriteStatement
	| searchStatement
	| sendStatement
	| setStatement
	| sortStatement
	| startStatement
	| stopStatement
	| stringStatement
	| subtractStatement
	| terminateStatement
	| unstringStatement
	| writeStatement
;

// accept statement

acceptStatement :
	acceptFromDate
	| acceptFromMnemonic
	| acceptMessageCount
;

acceptFromDate :
	ACCEPT identifier FROM
	(
		DATE YYYYMMDD?
		| DAY YYYYDDD?
		| DAY_OF_WEEK
		| TIME
		| TIMER
		| YEAR
		| YYYYMMDD
		| YYYYDDD
	)
;

acceptFromMnemonic :
	ACCEPT identifier (FROM mnemonicName)?
;

acceptMessageCount :
	ACCEPT identifier MESSAGE? COUNT
;

// add statement

addStatement :
	ADD
	(
		addToStatement
		| addToGivingStatement
		| addCorrespondingStatement
	)
	(ON? SIZE ERROR statements)?
	(NOT ON? SIZE ERROR statements)?
	END_ADD?
;

addToStatement :
	(identifier | literal)+ TO (identifier ROUNDED?)+
;

addToGivingStatement :
	(identifier | literal)+ (TO (identifier)+)? GIVING (identifier ROUNDED?)+
;

addCorrespondingStatement :
	(CORRESPONDING | CORR) identifier TO identifier ROUNDED?
;

// altered go to statement

alteredGoTo :
	GO TO? DOT_FS
;

// alter statement

alterStatement :
	ALTER (procedureName TO (PROCEED TO)? procedureName)+
;

// call statement

callStatement :
	CALL (identifier | literal)
	(
		USING (callByReferenceStatement | callByValueStatement | callByContentStatement)+
	)?
	(ON? OVERFLOW statements)?
	onExceptionClause?
	notOnExceptionClause?
	END_CALL?
;

callByReferenceStatement :
	(BY? REFERENCE)? (identifier | ADDRESS OF identifier | fileName)+
;

callByValueStatement :
	BY? VALUE (identifier | literal)+
;

callByContentStatement :
	BY? CONTENT ((LENGTH OF)? identifier | ADDRESS OF identifier | literal)+
;

// cancel statement

cancelStatement :
	CANCEL (identifier | literal)+
;

// close statement

closeStatement :
	CLOSE
	(
		fileName (closeReelUnitStatement | closeWithStatement)?
	)+
;

closeReelUnitStatement :
	(REEL | UNIT) (FOR? REMOVAL | WITH? NO REWIND)?
;

closeWithStatement :
	WITH? (NO REWIND | LOCK)
;

// compute statement

computeStatement :
	COMPUTE (identifier ROUNDED?)+
	(EQUALCHAR | EQUAL)
	arithmeticExpression
	(ON? SIZE ERROR statements)?
	(NOT ON? SIZE ERROR statements)?
	END_COMPUTE?
;

// continue statement

continueStatement :
	CONTINUE
;

// delete statement

deleteStatement :
	DELETE fileName RECORD?
	(INVALID KEY? statements)?
	(NOT INVALID KEY? statements)?
	END_DELETE?
;

// disable statement

disableStatement :
	DISABLE
	(INPUT TERMINAL? | I_O TERMINAL | OUTPUT)
	cdName WITH? KEY (identifier | literal)
;

// display statement

displayStatement :
	DISPLAY (identifier | literal)+
	(UPON (mnemonicName | environmentName))?
	(WITH? NO ADVANCING)?
;

// divide statement

divideStatement :
	DIVIDE (identifier | literal)
	(
		divideIntoStatement
		| divideIntoGivingStatement
		| divideIntoByGivingStatement
	)
	(REMAINDER identifier)?
	(ON? SIZE ERROR statements)?
	(NOT ON? SIZE ERROR statements)?
	END_DIVIDE?
;

divideIntoStatement :
	INTO (identifier | literal) (GIVING (identifier ROUNDED?)+)?
;

divideIntoGivingStatement :
	INTO (identifier ROUNDED?)+
;

divideIntoByGivingStatement :
	BY (identifier | literal) (GIVING (identifier ROUNDED?)+)?
;

// enable statement

enableStatement :
	ENABLE (INPUT TERMINAL? | I_O TERMINAL | OUTPUT)
	cdName WITH? KEY (literal | identifier)
;

// entry statement

entryStatement :
	ENTRY literal (USING identifier+)?
;

// evaluate statement

evaluateStatement :
	EVALUATE evaluateValue
	(ALSO evaluateValue)*
	(
		(WHEN evaluatePhrase (ALSO evaluatePhrase)*)+
		statements
	)+
	(WHEN OTHER statements)?
	END_EVALUATE?
;

evaluateValue :
	arithmeticExpression
	| identifier
	| literal
	| condition
;

evaluatePhrase :
	ANY
	| condition
	| booleanLiteral
	| NOT? (identifier | literal | arithmeticExpression) (
		(THROUGH | THRU) (identifier | literal | arithmeticExpression)
	)?
;

// exit program statement

exitProgramStatement :
	EXIT PROGRAM
;

// exit statement

exitStatement :
	EXIT
;

// generate statement

generateStatement :
	GENERATE (dataName | reportName)
;

// goback statement

gobackStatement :
	GOBACK
;

// goto statement

goToStatement :
	GO TO?
	(
		procedureName+ (DEPENDING ON? identifier)?
		| MORE_LABELS
	)
;

// if statement

ifStatement :
	IF condition THEN? (statement* | NEXT SENTENCE)
	(
		ELSE (statement* | NEXT SENTENCE)
	)?
	END_IF?
;

// initialize statement

initializeStatement :
	INITIALIZE identifier+
	(
		REPLACING
		(
			(
				ALPHABETIC
				| ALPHANUMERIC
				| NUMERIC
				| ALPHANUMERIC_EDITED
				| NUMERIC_EDITED
				| DBCS
				| EGCS
			)
			DATA? BY (identifier | literal)
		)+
	)?
;

// initiate statement

initiateStatement :
	INITIATE reportName+
;

// inspect statement

inspectStatement :
	INSPECT identifier
	(
		inspectTallyingPhrase
		| inspectConvertingPhrase
		| inspectReplacingPhrase
	)
;

inspectTallyingPhrase :
	TALLYING
	(
		identifier FOR
		(
			CHARACTERS inspectBeforeAfterPhrase*
			|
			(ALL | LEADING)
			(
				(identifier | literal) inspectBeforeAfterPhrase*
			)+
		)+
	)+
	inspectReplacingPhrase?
;

inspectConvertingPhrase :
	CONVERTING (identifier | literal)
	TO (identifier | literal) inspectBeforeAfterPhrase*
;

inspectReplacingPhrase :
	REPLACING
	(
		CHARACTERS BY (identifier | literal) inspectBeforeAfterPhrase*
		| (ALL | LEADING | FIRST)
		(
			(identifier | literal) BY (identifier | literal)
			inspectBeforeAfterPhrase*
		)+
	)+
;

inspectBeforeAfterPhrase :
	(BEFORE | AFTER) INITIAL? (identifier | literal)
;

// merge statement

mergeStatement :
	MERGE fileName
	(ON? (ASCENDING | DESCENDING) KEY? qualifiedDataName+)+
	(COLLATING? SEQUENCE IS? alphabetName)? USING fileName fileName+
	(
		OUTPUT PROCEDURE IS? procedureName (
			(THROUGH | THRU) procedureName
		)?
		| GIVING fileName+
	)
;

// move statement

moveStatement :
	MOVE
	(
		moveToStatement
		| moveCorrespondingToStatement
	)
;

moveToStatement :
	(identifier | literal | moveToKeyword) TO identifier+
;

moveCorrespondingToStatement :
	(CORRESPONDING | CORR) qualifiedDataName TO identifier+
;

// multiply statement

multiplyStatement :
	(
		multiplyRegular
		| multiplyGiving
	)
	(ON? SIZE ERROR statements)?
	(NOT ON? SIZE ERROR statements)?
	END_MULTIPLY?
;

multiplyRegular :
	MULTIPLY (identifier | literal) BY (identifier ROUNDED?)+
;

multiplyGiving :
	MULTIPLY (identifier | literal) BY (identifier | literal)
	GIVING (identifier ROUNDED?)+
;

// open statement

openStatement :
	OPEN
	(
		openInputStatement
		| openOutputStatement
		| openIOStatement
		| openExtendStatement
	)+
;

openInputStatement :
	INPUT (fileName (REVERSED | WITH? NO REWIND)?)+
;

openOutputStatement :
	OUTPUT (fileName (WITH? NO REWIND)?)+
;

openIOStatement :
	I_O fileName+
;

openExtendStatement :
	EXTEND fileName+
;

// perform statement

performStatement :
	performInlineStatement
	| performProcedureStatement
;

performInlineStatement :
	PERFORM performType? statement+ END_PERFORM
;

performProcedureStatement :
	PERFORM procedureName ((THROUGH | THRU) procedureName)? performType?
;

performType :
	performTimes
	| performUntil
	| performVarying
;

performTimes :
	(identifier | integerLiteral) TIMES
;

performUntil :
	performTestClause? UNTIL condition
;

performVarying :
	performTestClause performVaryingClause
	| performVaryingClause performTestClause?
;

performVaryingClause :
	VARYING (identifier | literal)
	FROM (identifier | literal | arithmeticExpression)
	BY (identifier | literal | arithmeticExpression)
	performUntil
	(
		AFTER (identifier)
		FROM (identifier | literal | arithmeticExpression)
		BY (identifier | literal | arithmeticExpression)
		performUntil
	)*
	(statement+ END_PERFORM)?
;

performTestClause :
	WITH? TEST (BEFORE | AFTER)
;

// purge statement

purgeStatement :
	PURGE cdName+
;

// read statement

readStatement :
	READ fileName
	NEXT? RECORD?
	(INTO identifier)?
	(
		WITH?
		(
			(KEPT | NO) LOCK
			| WAIT
		)
	)?
	(KEY IS? qualifiedDataName)?
	(INVALID KEY? statements)?
	(NOT INVALID KEY? statements)?
	(AT? END statements)?
	(NOT AT? END statements)?
	END_READ?
;

// receive statement

receiveStatement :
	RECEIVE
	(
		dataName FROM (THREAD dataName | LAST THREAD | ANY THREAD)
		(
			BEFORE TIME? (numericLiteral | identifier)
			| WITH? NO WAIT
			| THREAD IN? dataName
			| SIZE IN? (numericLiteral | identifier)
			| STATUS IN? (identifier)
			| onExceptionClause
			| notOnExceptionClause
		)
		| cdName (MESSAGE | SEGMENT) INTO? identifier (NO DATA statements)? (WITH DATA statements)?
	)
	END_RECEIVE?
;

// release statement

releaseStatement :
	RELEASE recordName (FROM qualifiedDataName)?
;

// return statement

returnStatement :
	RETURN fileName RECORD? (INTO qualifiedDataName)?
	AT? END statements
	(NOT AT? END statements)?
	END_RETURN?
;

// rewrite statement

rewriteStatement :
	REWRITE recordName (FROM identifier)?
	(INVALID KEY? statements)?
	(NOT INVALID KEY? statements)?
	END_REWRITE?
;

// search statement

searchStatement :
	SEARCH ALL? qualifiedDataName
	(VARYING qualifiedDataName)?
	(AT? END statements)?
	(WHEN condition (statements | NEXT SENTENCE))+
	END_SEARCH?
;

// send statement

sendStatement :
	sendStatementSync | sendStatementAsync
;

sendStatementSync :
	SEND (identifier | literal) (FROM identifier)?
	(
		WITH (identifier | EGI | EMI | ESI)
	)?
	(REPLACING LINE?)?
	sendAdvancingPhrase?
	onExceptionClause?
	notOnExceptionClause?
;

sendStatementAsync :
	SEND TO (TOP | BOTTOM) identifier
	onExceptionClause?
	notOnExceptionClause?
;

sendAdvancingPhrase :
	(BEFORE | AFTER) ADVANCING?
	(
		PAGE
		| (identifier | literal) (LINE | LINES)?
		| mnemonicName
	)
;

// set statement

setStatement :
	setToStatement
	| setUpDownByStatement
;

setToStatement :
	SET
	(
		identifier+ TO (identifier | ON | OFF | literal)
	)+
;

setUpDownByStatement :
	SET identifier+ (UP | DOWN) BY? (identifier | literal)
;

// sort statement

sortStatement :
	SORT fileName
	(ON? (ASCENDING | DESCENDING) KEY? qualifiedDataName+)+
	(WITH? DUPLICATES IN? ORDER?)?
	(COLLATING? SEQUENCE IS? alphabetName)?
	(USING fileName+ | INPUT PROCEDURE IS? procedureName ((THROUGH | THRU) procedureName)?)
	(GIVING fileName+ | OUTPUT PROCEDURE IS? procedureName ((THROUGH | THRU) procedureName)?)
;

// start statement

startStatement :
	START fileName
	(KEY IS? (
		EQUAL TO?
		| EQUALCHAR
		| GREATER THAN?
		| MORETHANCHAR
		| NOT LESS THAN?
		| NOT LESSTHANCHAR
		| GREATER THAN? OR EQUAL TO?
		| MORETHANOREQUAL
	) qualifiedDataName)?
	(INVALID KEY? statements)?
	(NOT INVALID KEY? statements)?
	END_START?
;

// stop statement

stopStatement :
	STOP (RUN | literal)
;

// string statement

stringStatement :
	STRING
	(
		(tableCall | literal)+
		DELIMITED BY? (identifier | literal | SIZE)
	)+
	INTO identifier
	(WITH? POINTER qualifiedDataName)?
	(ON? OVERFLOW statements)?
	(NOT ON? OVERFLOW statements)?
	END_STRING?
;

// subtract statement

subtractStatement :
	SUBTRACT
	(
		subtractFromStatement
		| subtractFromGivingStatement
		| subtractCorrespondingStatement
 	)
	(ON? SIZE ERROR statements)?
	(NOT ON? SIZE ERROR statements)?
	END_SUBTRACT?
;

subtractFromStatement :
	(identifier | literal)+ FROM (identifier ROUNDED?)+
;

subtractFromGivingStatement :
	(identifier | literal)+ FROM (identifier | literal) ROUNDED? GIVING (identifier ROUNDED?)+
;

subtractCorrespondingStatement :
	(CORRESPONDING | CORR) qualifiedDataName FROM qualifiedDataName ROUNDED?
;

// terminate statement

terminateStatement :
	TERMINATE reportName
;

// unstring statement

unstringStatement :
	UNSTRING qualifiedDataName
	(
		DELIMITED BY? ALL? (identifier | literal) (OR ALL? (identifier | literal))*
	)?
	INTO
	(
		identifier (DELIMITER IN? identifier)? (COUNT IN? identifier)?
	)+
	(WITH? POINTER qualifiedDataName)?
	(TALLYING IN? qualifiedDataName)?
	(ON? OVERFLOW statements)?
	(NOT ON? OVERFLOW statements)?
	END_UNSTRING?
;

// use statement

useStatement :
	USE (
		useProcedureClause
		| useProcedureDebugClause
	)
;

useProcedureClause :
		GLOBAL? AFTER STANDARD?
		(
			(EXCEPTION | ERROR)
			| (BEGINNING | ENDING)? (FILE | REEL | UNIT)? LABEL
		)
		PROCEDURE ON? (fileName+ | INPUT | OUTPUT | I_O | EXTEND)
		(GIVING dataName+)?
;

useProcedureDebugClause :
	FOR? DEBUGGING ON?
	(
		ALL PROCEDURES
		| ALL REFERENCES? OF? identifier
		| procedureName
		| fileName
	)+
;

// write statement

writeStatement :
	WRITE recordName (FROM (identifier | literal))?
	writeAdvancingPhrase?
	(AT? (END_OF_PAGE | EOP) statements)?
	(NOT AT? (END_OF_PAGE | EOP) statements)?
	(INVALID KEY? statements)?
	(NOT INVALID KEY? statements)?
	END_WRITE?
;

writeAdvancingPhrase :
	(BEFORE | AFTER) ADVANCING?
	(
		PAGE
		| (identifier | literal) (LINE | LINES)?
		| mnemonicName
	)
;

// statement clauses ----------------------------------

onExceptionClause :
	ON? EXCEPTION statements
;

notOnExceptionClause :
	NOT ON? EXCEPTION statements
;

// arithmetic expression ----------------------------------

arithmeticExpression :
	timesDiv ((PLUSCHAR | MINUSCHAR) timesDiv)*
;

timesDiv :
	power ((ASTERISKCHAR | SLASHCHAR) power)*
;

power :
	(PLUSCHAR | MINUSCHAR)? basis (DOUBLEASTERISKCHAR basis)*
;

basis :
	(identifier | literal | basisKeyword | LPARENCHAR arithmeticExpression RPARENCHAR)
;

commentEntry :
	(
		commentEntryText | commentEntryAmbiguousKeyword
	)+
;

commentEntryText :
	literal |
	cobolWord |
	AMPCHAR |
	ASTERISKCHAR |
	COLONCHAR |
	COMMACHAR |
	COMMENTTAG |
	DOLLARCHAR |
	DOUBLEQUOTE |
	DOT |
	DOT_FS |
	EQUALCHAR |
	LESSTHANCHAR |
	LESSTHANOREQUAL |
	LPARENCHAR |
	MINUSCHAR |
	MORETHANCHAR |
	MORETHANOREQUAL |
	PLUSCHAR |
	RPARENCHAR |
	SINGLEQUOTE |
	SLASHCHAR
;


// logical expressions ----------------------------------

condition :
	combinableCondition (
		(AND | OR) (combinableCondition | abbreviationRest)
	)*
;

combinableCondition : NOT? simpleCondition;

simpleCondition :
	LPARENCHAR condition RPARENCHAR
	| relationCondition
	| classCondition
	| conditionNameCondition
;

classCondition :
	identifier IS? NOT?
	(
		NUMERIC
		| ALPHABETIC
		| ALPHABETIC_LOWER
		| ALPHABETIC_UPPER
		| className
		| DBCS
		| KANJI
	)
;

conditionNameCondition :
	conditionNameReference
;

conditionNameReference :
	conditionName
	(
		((IN | OF) dataName)*
		((IN | OF) fileName)?
		(LPARENCHAR subscript RPARENCHAR)*
		|
		((IN | OF) mnemonicName)*
	)
;

relationCondition :
	arithmeticExpression (relationalOperator arithmeticExpression | signCondition)
;

signCondition :
	IS? NOT? (POSITIVE | NEGATIVE | ZERO)
;

relationalOperator :
	(IS | ARE)?
	(
		NOT? (
			GREATER THAN?
			| MORETHANCHAR
			| LESS THAN?
			| LESSTHANCHAR
			| EQUAL TO?
			| EQUALCHAR
		)
		| GREATER THAN? OR EQUAL TO?
		| MORETHANOREQUAL
		| LESS THAN? OR EQUAL TO?
		| LESSTHANOREQUAL
	)
;

abbreviationRest :
	(NOT? relationalOperator? abbreviationLeaf)+
;

abbreviationLeaf :
	arithmeticExpression
	| LPARENCHAR arithmeticExpression abbreviationRest RPARENCHAR
;


// names ----------------------------------

alphabetName :
	cobolWord
;

assignmentName :
	systemName
;

basisName :
	programName
;

cdName :
	cobolWord
;

className :
	cobolWord
;

computerName :
	systemName
;

conditionName :
	cobolWord
;

dataName :
	cobolWord
;

dataDescName :
	FILLER | CURSOR | dataName
;

environmentName :
	systemName
;

fileName :
	cobolWord
;

functionName :
	cobolWord
	| LENGTH
	| RANDOM
	| SUM
	| WHEN_COMPILED
;

indexName :
	cobolWord
;

languageName :
	systemName
;

libraryName :
	cobolWord
;

mnemonicName :
	cobolWord
;

paragraphName :
	cobolWord | integerLiteral
;

procedureName :
	paragraphName ((IN | OF) sectionName)?
	| sectionName
;

programName :
	cobolWord | NONNUMERICLITERAL
;

qualifiedDataName :
	dataName
	((IN | OF) (dataName | tableCall))*
	((IN | OF) fileName)?
	|
	specialRegister
;

recordName :
	qualifiedDataName
;

reportName :
	qualifiedDataName
;

routineName :
	cobolWord
;

sectionName :
	cobolWord | integerLiteral
;

systemName :
	cobolWord
;

symbolicCharacter :
	cobolWord
;


// identifier ----------------------------------

identifier :
	qualifiedDataName
	| tableCall
	| functionCall
	| linageCounterCall
;

// array access
tableCall :
	qualifiedDataName (LPARENCHAR subscript RPARENCHAR)* (LPARENCHAR characterPosition COLONCHAR length? RPARENCHAR)?
;

functionCall :
	FUNCTION functionName (LPARENCHAR argument RPARENCHAR)* (LPARENCHAR characterPosition COLONCHAR length? RPARENCHAR)?
;

linageCounterCall :
	LINAGE_COUNTER ((IN | OF) fileName)?
;

length :
	arithmeticExpression
;

characterPosition :
	arithmeticExpression
;

subscript :
	(
		integerLiteral
		| qualifiedDataName integerLiteral?
		| indexName integerLiteral?
		| arithmeticExpression
		| ALL
	)+
;

argument :
	(
		literal
		| identifier
		| qualifiedDataName integerLiteral?
		| indexName integerLiteral?
		| arithmeticExpression
	)+
;

// literal ----------------------------------

cobolWord : IDENTIFIER;

literal : NONNUMERICLITERAL | numericLiteral | booleanLiteral | figurativeConstant | cicsDfhRespLiteral | cicsDfhValueLiteral;

booleanLiteral : TRUE | FALSE;

numericLiteral : NUMERICLITERAL | integerLiteral | ZERO;

integerLiteral : INTEGERLITERAL | LEVEL_NUMBER_66 | LEVEL_NUMBER_77 | LEVEL_NUMBER_88;

cicsDfhRespLiteral :
	DFHRESP LPARENCHAR (cobolWord | literal | cicsDfhRespKeyword) RPARENCHAR
;

cicsDfhValueLiteral :
	DFHVALUE LPARENCHAR (cobolWord | literal | cicsDfhValueKeyword) RPARENCHAR
;


// contextual keywords -----------------------

basisKeyword :
	otherKeyword
;

cicsDfhRespKeyword :
	otherKeyword
;

cicsDfhValueKeyword :
	otherKeyword
;

commentEntryAmbiguousKeyword :
	connective | divisionKeyword | figurativeConstant | otherKeyword | sectionKeyword | specialRegister
;

moveToKeyword :
	otherKeyword
;

pictureCharsKeyword :
	SP
;


// keywords ----------------------------------

// keyword :
//	connective
//	| figurativeConstant
//	| specialRegister
//	| divisionKeyword
//	| paragraphKeyword
//	| sectionKeyword
//	| otherKeyword
// ;

connective :
	AND |
	IN |
	OF | OR
;

figurativeConstant :
	ALL literal
	| HIGH_VALUE | HIGH_VALUES
	| LOW_VALUE | LOW_VALUES
	| NULL | NULLS
	| QUOTE | QUOTES
	| SPACE | SPACES
	| ZERO | ZEROS | ZEROES
;

specialRegister :
	ADDRESS OF dataName
	| DATE | DAY | DAY_OF_WEEK | DEBUG_ITEM
	| LENGTH OF identifier | LINAGE_COUNTER | LINE_COUNTER
	| PAGE_COUNTER
	| RETURN_CODE
	| SHIFT_OUT | SHIFT_IN | SORT_CONTROL | SORT_CORE_SIZE | SORT_FILE_SIZE | SORT_MESSAGE | SORT_MODE_SIZE | SORT_RETURN
	| TALLY | TIME
	| WHEN_COMPILED
;

divisionKeyword :
	DATA
	| ENVIRONMENT
	| IDENTIFICATION
	| PROCEDURE
;

paragraphKeyword :
	AUTHOR |
	DATE_COMPILED | DATE_WRITTEN |
	FILE_CONTROL |
	INSTALLATION | I_O_CONTROL |
	OBJECT_COMPUTER |
	PROGRAM_ID |
	REMARKS |
	SECURITY | SOURCE_COMPUTER | SPECIAL_NAMES
;

sectionKeyword :
	COMMUNICATION |	CONFIGURATION |
	FILE |
	INPUT_OUTPUT |
	LINKAGE |
	REPORT |
	SCREEN |
	WORKING_STORAGE
;

otherKeyword :
	ACCEPT | ACCESS | ADD | ADDRESS | ADVANCING | AFTER | ALL | ALPHABET | ALPHABETIC | ALPHABETIC_LOWER | ALPHABETIC_UPPER | ALPHANUMERIC | ALPHANUMERIC_EDITED | ALSO | ALTER | ALTERNATE | ANY | APOST | APPROXIMATE | ARE | AREA | AREAS | ARITH | AS | ASCENDING | ASCII | ASSIGN | AT
	| BEFORE | BEGINNING | BINARY | BLANK | BLOCK | BOTTOM | BY
	| CALL | CANCEL | CD | CF | CH | CHANNEL | CHARACTER | CHARACTERS | CLASS | CLOCK_UNITS | CLOSE | COBOL | CODE | CODEPAGE | CODE_SET | COLLATING | COLUMN | COMMA | COMMITMENT | COMMON | COMP | COMP_1 | COMP_2 | COMP_3 | COMP_4 | COMPUTATIONAL | COMPUTATIONAL_1 | COMPUTATIONAL_2 | COMPUTATIONAL_3 | COMPUTATIONAL_4 | COMPUTE | CONTAINS | CONTENT | CONTINUE | CONTROL | CONTROLS | CONVERTING | COPY | CORR | CORRESPONDING | COUNT | CURRENCY | CURSOR
	| DBCS | DE | DEBUG_CONTENTS | DEBUG_LINE | DEBUG_NAME | DEBUG_SUB_1 | DEBUG_SUB_2 | DEBUG_SUB_3 | DEBUGGING | DECIMAL_POINT | DECLARATIVES | DEFINITION | DELETE | DELIMITED | DELIMITER | DEPENDING | DESCENDING | DESTINATION | DETAIL | DFHRESP | DFHVALUE | DISABLE | DISK | DISPLAY | DISPLAY_1 | DIVIDE | DIVISION | DOWN | DUPLICATES | DYNAMIC
	| EBCDIC | EGCS | EGI | ELSE | EMI | ENABLE | END | END_ADD | END_CALL | END_COMPUTE | END_DELETE | END_DIVIDE | END_EVALUATE | END_IF | END_MULTIPLY | END_OF_PAGE | END_PERFORM | END_READ | END_RECEIVE | END_RETURN | END_REWRITE | END_SEARCH | END_START | END_STRING | END_SUBTRACT | END_UNSTRING | END_WRITE | ENDING | ENTER | ENTRY | EOP | EQUAL | ERROR | ESI | EVALUATE | EVERY | EXCEPTION | EXCLUSIVE | EXIT | EXTEND | EXTENDED | EXTERNAL
	| FALSE | FD | FILLER | FINAL | FIRST | FOOTING | FOR | FROM | FULL | FUNCTION | FUNCTION_POINTER
	| GENERATE | GOBACK | GENERIC | GIVING | GLOBAL | GO | GREATER | GROUP
	| HEADING
	| I_O | ID | IF | IMPLICIT | INDEX | INDEXED | INDICATE | INITIAL | INITIALIZE | INITIATE | INPUT | INSPECT | INTO | INVALID | IS
	| JUST | JUSTIFIED | JUSTIFY
	| KANJI | KEPT | KEY
	| LABEL | LAST | LEADING | LEFT | LENGTH | LESS | LIB | LIBRARY | LIMIT | LIMITS | LINAGE | LINE | LINES | LOCK | LOCKFILE
	| MEMORY | MERGE | MESSAGE | MODE | MODULES | MORE_LABELS | MOVE | MULTIPLE | MULTIPLY
	| NATIONAL | NATIVE | NEGATIVE | NEXT | NO | NOSEQ | NOT | NUMBER | NUMERIC | NUMERIC_EDITED
	| OCCURS | ODT | OFF | OMITTED | ON | OPEN | OPTIMIZE | OPTIONAL | ORDER | ORGANIZATION | OTHER | OUTPUT | OVERFLOW
	| PACKED_DECIMAL | PADDING | PAGE | PASSWORD | PERFORM | PF | PH | PIC | PICTURE | PLUS | POINTER | POSITION | POSITIVE | PRINTING | PROCEDURE_POINTER| PROCEDURES | PROCEED | PROCESS | PROGRAM | PROGRAM_STATUS | PROMPT | PROTECTED | PURGE
	| QUEUE
	| RANDOM | RD | READ | RECEIVE | RECEIVE_CONTROL | RECORD | RECORDING | RECORDS | REDEFINES | REEL | REFERENCE | REFERENCES | RELATIVE | RELEASE | REMAINDER | REMOVAL | RENAMES | REPLACE | REPLACING | REPLY | REPORTING | REPORTS | RERUN | RESERVE | RESET | RETURN | RETURNED | REVERSED | REWIND | REWRITE | RF | RH | RIGHT | ROUNDED | RUN
	| SAME | SD | SEARCH | SECTION | SEGMENT | SEGMENT_LIMIT | SELECT | SEND | SENTENCE | SEPARATE | SEQUENCE | SEQUENTIAL | SET | SHARED | SIGN | SIZE | SORT | SORT_MERGE | SOURCE | SP | STANDARD | STANDARD_1 | STANDARD_2 | START | STATUS | STOP | STRING | SUB_QUEUE_1 | SUB_QUEUE_2 | SUB_QUEUE_3 | SUBTRACT | SUM | SUPPRESS | SYMBOLIC | SYNC | SYNCHRONIZED
	| TABLE | TALLYING | TAPE | TERMINAL | TERMINATE | TEST | TEXT | THAN | THEN | THREAD | THROUGH | THRU | TIMER | TIMES | TO | TOP | TRAILING | TRUE | TRUNCATED | TYPE
	| UNIT | UNLOCK | UNLOCKFILE | UNLOCKRECORD | UNSTRING | UNTIL | UP | UPON | USAGE | USE | USING
	| VALUE | VALUES | VARYING
	| WAIT | WHEN | WITH | WORDS | WRITE
	| XOPTS
	| YEAR | YYYYMMDD | YYYYDDD
;


// lexer rules --------------------------------------------------------------------------------

// keywords
ACCEPT : A C C E P T;
ACCESS : A C C E S S;
ADD : A D D;
ADDRESS : A D D R E S S;
ADVANCING : A D V A N C I N G;
AFTER : A F T E R;
ALL : A L L;
ALPHABET : A L P H A B E T;
ALPHABETIC : A L P H A B E T I C;
ALPHABETIC_LOWER : A L P H A B E T I C MINUSCHAR L O W E R;
ALPHABETIC_UPPER : A L P H A B E T I C MINUSCHAR U P P E R;
ALPHANUMERIC : A L P H A N U M E R I C;
ALPHANUMERIC_EDITED : A L P H A N U M E R I C MINUSCHAR E D I T E D;
ALSO : A L S O;
ALTER : A L T E R;
ALTERNATE : A L T E R N A T E;
AND : A N D;
ANY : A N Y;
APOST : A P O S T;
APPROXIMATE : A P P R O X I M A T E;
ARE : A R E;
AREA : A R E A;
AREAS : A R E A S;
ARITH : A R I T H;
AS : A S;
ASCENDING : A S C E N D I N G;
ASCII : A S C I I;
ASSIGN : A S S I G N;
AT : A T;
AUTHOR : A U T H O R;
BEFORE : B E F O R E;
BEGINNING : B E G I N N I N G;
BINARY : B I N A R Y;
BLANK : B L A N K;
BLOCK : B L O C K;
BOTTOM : B O T T O M;
BY : B Y;
CALL : C A L L;
CANCEL : C A N C E L;
CD : C D;
CF : C F;
CH : C H;
CHANNEL : C H A N N E L;
CHARACTER : C H A R A C T E R;
CHARACTERS : C H A R A C T E R S;
CLASS : C L A S S;
CLOCK_UNITS : C L O C K MINUSCHAR U N I T S;
CLOSE : C L O S E;
COBOL : C O B O L;
CODE : C O D E;
CODEPAGE : C O D E P A G E;
CODE_SET : C O D E MINUSCHAR S E T;
COLLATING : C O L L A T I N G;
COLUMN : C O L U M N;
COMMA : C O M M A;
COMMITMENT : C O M M I T M E N T;
COMMON : C O M M O N;
COMMUNICATION : C O M M U N I C A T I O N;
COMP : C O M P;
COMP_1 : C O M P MINUSCHAR '1';
COMP_2 : C O M P MINUSCHAR '2';
COMP_3 : C O M P MINUSCHAR '3';
COMP_4 : C O M P MINUSCHAR '4';
COMPUTATIONAL : C O M P U T A T I O N A L;
COMPUTATIONAL_1 : C O M P U T A T I O N A L MINUSCHAR '1';
COMPUTATIONAL_2 : C O M P U T A T I O N A L MINUSCHAR '2';
COMPUTATIONAL_3 : C O M P U T A T I O N A L MINUSCHAR '3';
COMPUTATIONAL_4 : C O M P U T A T I O N A L MINUSCHAR '4';
COMPUTE : C O M P U T E;
CONFIGURATION : C O N F I G U R A T I O N;
CONTAINS : C O N T A I N S;
CONTENT : C O N T E N T;
CONTINUE : C O N T I N U E;
CONTROL : C O N T R O L;
CONTROLS : C O N T R O L S;
CONVERTING : C O N V E R T I N G;
COPY : C O P Y;
CORR : C O R R;
CORRESPONDING : C O R R E S P O N D I N G;
COUNT : C O U N T;
CURRENCY : C U R R E N C Y;
CURSOR : C U R S O R;
DATA : D A T A;
DATE : D A T E;
DATE_COMPILED : D A T E MINUSCHAR C O M P I L E D;
DATE_WRITTEN : D A T E MINUSCHAR W R I T T E N;
DAY : D A Y;
DAY_OF_WEEK : D A Y MINUSCHAR O F MINUSCHAR W E E K;
DBCS : D B C S;
DE : D E;
DEBUG_CONTENTS : D E B U G MINUSCHAR C O N T E N T S;
DEBUG_ITEM : D E B U G MINUSCHAR I T E M;
DEBUG_LINE : D E B U G MINUSCHAR L I N E;
DEBUG_NAME : D E B U G MINUSCHAR N A M E;
DEBUG_SUB_1 : D E B U G MINUSCHAR S U B MINUSCHAR '1';
DEBUG_SUB_2 : D E B U G MINUSCHAR S U B MINUSCHAR '2';
DEBUG_SUB_3 : D E B U G MINUSCHAR S U B MINUSCHAR '3';
DEBUGGING : D E B U G G I N G;
DECIMAL_POINT : D E C I M A L MINUSCHAR P O I N T;
DECLARATIVES : D E C L A R A T I V E S;
DEFINITION : D E F I N I T I O N;
DELETE : D E L E T E;
DELIMITED : D E L I M I T E D;
DELIMITER : D E L I M I T E R;
DEPENDING : D E P E N D I N G;
DESCENDING : D E S C E N D I N G;
DESTINATION : D E S T I N A T I O N;
DETAIL : D E T A I L;
DFHRESP : D F H R E S P;
DFHVALUE : D F H V A L U E;
DISABLE : D I S A B L E;
DISK : D I S K;
DISPLAY : D I S P L A Y;
DISPLAY_1 : D I S P L A Y MINUSCHAR '1';
DIVIDE : D I V I D E;
DIVISION : D I V I S I O N;
DOWN : D O W N;
DUPLICATES : D U P L I C A T E S;
DYNAMIC : D Y N A M I C;
EBCDIC : E B C D I C;
EGCS : E G C S; // E X T E N S I O N
EGI : E G I;
ELSE : E L S E;
EMI : E M I;
ENABLE : E N A B L E;
END : E N D;
END_ADD : E N D MINUSCHAR A D D;
END_CALL : E N D MINUSCHAR C A L L;
END_COMPUTE : E N D MINUSCHAR C O M P U T E;
END_DELETE : E N D MINUSCHAR D E L E T E;
END_DIVIDE : E N D MINUSCHAR D I V I D E;
END_EVALUATE : E N D MINUSCHAR E V A L U A T E;
END_IF : E N D MINUSCHAR I F;
END_MULTIPLY : E N D MINUSCHAR M U L T I P L Y;
END_OF_PAGE : E N D MINUSCHAR O F MINUSCHAR P A G E;
END_PERFORM : E N D MINUSCHAR P E R F O R M;
END_READ : E N D MINUSCHAR R E A D;
END_RECEIVE : E N D MINUSCHAR R E C E I V E;
END_RETURN : E N D MINUSCHAR R E T U R N;
END_REWRITE : E N D MINUSCHAR R E W R I T E;
END_SEARCH : E N D MINUSCHAR S E A R C H;
END_START : E N D MINUSCHAR S T A R T;
END_STRING : E N D MINUSCHAR S T R I N G;
END_SUBTRACT : E N D MINUSCHAR S U B T R A C T;
END_UNSTRING : E N D MINUSCHAR U N S T R I N G;
END_WRITE : E N D MINUSCHAR W R I T E;
ENDING : E N D I N F;
ENTER : E N T E R;
ENTRY : E N T R Y;
ENVIRONMENT : E N V I R O N M E N T;
EOP : E O P;
EQUAL : E Q U A L;
ERROR : E R R O R;
ESI : E S I;
EVALUATE : E V A L U A T E;
EVERY : E V E R Y;
EXCEPTION : E X C E P T I O N;
EXCLUSIVE : E X C L U S I V E;
EXIT : E X I T;
EXTEND : E X T E N D;
EXTENDED : E X T E N D E D;
EXTERNAL : E X T E R N A L;
FALSE : F A L S E;
FD : F D;
FILE : F I L E;
FILE_CONTROL : F I L E MINUSCHAR C O N T R O L;
FILLER : F I L L E R;
FINAL : F I N A L;
FIRST : F I R S T;
FOOTING : F O O T I N G;
FOR : F O R;
FROM : F R O M;
FULL : F U L L;
FUNCTION : F U N C T I O N;
FUNCTION_POINTER : F U N C T I O N MINUSCHAR P O I N T E R;
GENERATE : G E N E R A T E;
GOBACK : G O B A C K;
GENERIC : G E N E R I C;
GIVING : G I V I N G;
GLOBAL : G L O B A L;
GO : G O;
GREATER : G R E A T E R;
GROUP : G R O U P;
HEADING : H E A D I N G;
HIGH_VALUE : H I G H MINUSCHAR V A L U E;
HIGH_VALUES : H I G H MINUSCHAR V A L U E S;
I_O : I MINUSCHAR O;
I_O_CONTROL : I MINUSCHAR O MINUSCHAR C O N T R O L;
ID : I D;
IDENTIFICATION : I D E N T I F I C A T I O N;
IF : I F;
IMPLICIT : I M P L I C I T;
IN : I N;
INDEX : I N D E X;
INDEXED : I N D E X E D;
INDICATE : I N D I C A T E;
INITIAL : I N I T I A L;
INITIALIZE : I N I T I A L I Z E;
INITIATE : I N I T I A T E;
INPUT : I N P U T;
INPUT_OUTPUT : I N P U T MINUSCHAR O U T P U T;
INSPECT : I N S P E C T;
INSTALLATION : I N S T A L L A T I O N;
INTO : I N T O;
INVALID : I N V A L I D;
IS : I S;
JUST : J U S T;
JUSTIFIED : J U S T I F I E D;
JUSTIFY : J U S T I F Y;
KANJI : K A N J I;
KEPT : K E P T;
KEY : K E Y;
LABEL : L A B E L;
LAST : L A S T;
LEADING : L E A D I N G;
LEFT : L E F T;
LENGTH : L E N G T H;
LESS : L E S S;
LIB : L I B;
LIBRARY : L I B R A R Y;
LIMIT : L I M I T;
LIMITS : L I M I T S;
LINAGE : L I N A G E;
LINAGE_COUNTER : L I N A G E '_' C O U N T E R;
LINE : L I N E;
LINES : L I N E S;
LINE_COUNTER : L I N E MINUSCHAR C O U N T E R;
LINKAGE : L I N K A G E;
LOCK : L O C K;
LOCKFILE : L O C K F I L E;
LOW_VALUE : L O W MINUSCHAR V A L U E;
LOW_VALUES : L O W MINUSCHAR V A L U E S;
MEMORY : M E M O R Y;
MERGE : M E R G E;
MESSAGE : M E S S A G E;
MODE : M O D E;
MODULES : M O D U L E S;
MORE_LABELS : M O R E MINUSCHAR L A B E L S;
MOVE : M O V E;
MULTIPLE : M U L T I P L E;
MULTIPLY : M U L T I P L Y;
NATIONAL : N A T I O N A L;
NATIVE : N A T I V E;
NEGATIVE : N E G A T I V E;
NEXT : N E X T;
NO : N O;
NOSEQ : N O S E Q;
NOSTDTRUNC : N O S T D T R U N C;
NOT : N O T;
NULL : N U L L;
NULLS : N U L L S;
NUMBER : N U M B E R;
NUMERIC : N U M E R I C;
NUMERIC_EDITED : N U M E R I C MINUSCHAR E D I T E D;
OBJECT_COMPUTER : O B J E C T MINUSCHAR C O M P U T E R;
OCCURS : O C C U R S;
ODT : O D T;
OF : O F;
OFF : O F F;
OMITTED : O M I T T E D;
ON : O N;
OPEN : O P E N;
OPTIMIZE : O P T I M I Z E;
OPTIONAL : O P T I O N A L;
OR : O R;
ORDER : O R D E R;
ORGANIZATION : O R G A N I Z A T I O N;
OTHER : O T H E R;
OUTPUT : O U T P U T;
OVERFLOW : O V E R F L O W;
PACKED_DECIMAL : P A C K E D MINUSCHAR D E C I M A L;
PADDING : P A D D I N G;
PAGE : P A G E;
PAGE_COUNTER : P A G E MINUSCHAR C O U N T E R;
PASSWORD : P A S S W O R D;
PERFORM : P E R F O R M;
PF : P F;
PH : P H;
PIC : P I C;
PICTURE : P I C T U R E;
PLUS : P L U S;
POINTER : P O I N T E R;
POSITION : P O S I T I O N;
POSITIVE : P O S I T I V E;
PRINTING : P R I N T I N G;
PROCEDURE : P R O C E D U R E;
PROCEDURE_POINTER : P R O C E D U R E MINUSCHAR P O I N T E R;
PROCEDURES : P R O C E D U R E S;
PROCEED : P R O C E E D;
PROCESS : P R O C E S S;
PROGRAM : P R O G R A M;
PROGRAM_ID : P R O G R A M MINUSCHAR I D;
PROGRAM_STATUS : P R O G R A M MINUSCHAR S T A T U S;
PROMPT : P R O M P T;
PROTECTED : P R O T E C T E D;
PURGE : P U R G E;
QUEUE : Q U E U E;
QUOTE : Q U O T E;
QUOTES : Q U O T E S;
RANDOM : R A N D O M;
RD : R D;
READ : R E A D;
RECEIVE : R E C E I V E;
RECEIVE_CONTROL : R E C E I V E MINUSCHAR C O N T R O L;
RECORD : R E C O R D;
RECORDING : R E C O R D I N G;
RECORDS : R E C O R D S;
REDEFINES : R E D E F I N E S;
REEL : R E E L;
REFERENCE : R E F E R E N C E;
REFERENCES : R E F E R E N C E S;
RELATIVE : R E L A T I V E;
RELEASE : R E L E A S E;
REMAINDER : R E M A I N D E R;
REMARKS : R E M A R K S;
REMOVAL : R E M O V A L;
RENAMES : R E N A M E S;
REPLACE : R E P L A C E;
REPLACING : R E P L A C I N G;
REPLY : R E P L Y; // TANDEM EXTENSION
REPORT : R E P O R T;
REPORTING : R E P O R T I N G;
REPORTS : R E P O R T S;
RERUN : R E R U N;
RESERVE : R E S E R V E;
RESET : R E S E T;
RETURN : R E T U R N;
RETURN_CODE : R E T U R N MINUSCHAR C O D E;
RETURNED : R E T U R N E D;
REVERSED : R E V E R S E D;
REWIND : R E W I N D;
REWRITE : R E W R I T E;
RF : R F;
RH : R H;
RIGHT : R I G H T;
ROUNDED : R O U N D E D;
RUN : R U N;
SAME : S A M E;
SCREEN : S C R E E N;
SD : S D;
SEARCH : S E A R C H;
SECTION : S E C T I O N;
SECURITY : S E C U R I T Y;
SEGMENT : S E G M E N T;
SEGMENT_LIMIT : S E G M E N T MINUSCHAR L I M I T;
SELECT : S E L E C T;
SEND : S E N D;
SENTENCE : S E N T E N C E;
SEPARATE : S E P A R A T E;
SEQUENCE : S E Q U E N C E;
SEQUENTIAL : S E Q U E N T I A L;
SET : S E T;
SHARED : S H A R E D;
SHIFT_IN : S H I F T MINUSCHAR I N;
SHIFT_OUT : S H I F T MINUSCHAR O U T;
SIGN : S I G N;
SIZE : S I Z E;
SORT : S O R T;
SORT_CONTROL : S O R T MINUSCHAR C O N T R O L;
SORT_CORE_SIZE : S O R T MINUSCHAR C O R E MINUSCHAR S I Z E;
SORT_FILE_SIZE : S O R T MINUSCHAR F I L E MINUSCHAR S I Z E;
SORT_MERGE : S O R T MINUSCHAR M E R G E;
SORT_MESSAGE : S O R T MINUSCHAR M E S S A G E;
SORT_MODE_SIZE : S O R T MINUSCHAR M O D E MINUSCHAR S I Z E;
SORT_RETURN : S O R T MINUSCHAR R E T U R N;
SOURCE : S O U R C E;
SOURCE_COMPUTER : S O U R C E MINUSCHAR C O M P U T E R;
SP : S P;
SPACE : S P A C E;
SPACES : S P A C E S;
SPECIAL_NAMES : S P E C I A L MINUSCHAR N A M E S;
STANDARD : S T A N D A R D;
STANDARD_1 : S T A N D A R D MINUSCHAR '1';
STANDARD_2 : S T A N D A R D MINUSCHAR '2';
START : S T A R T;
STATUS : S T A T U S;
STOP : S T O P;
STRING : S T R I N G;
SUB_QUEUE_1 : S U B MINUSCHAR Q U E U E MINUSCHAR '1';
SUB_QUEUE_2 : S U B MINUSCHAR Q U E U E MINUSCHAR '2';
SUB_QUEUE_3 : S U B MINUSCHAR Q U E U E MINUSCHAR '3';
SUBTRACT : S U B T R A C T;
SUM : S U M;
SUPPRESS : S U P P R E S S;
SYMBOLIC : S Y M B O L I C;
SYNC : S Y N C;
SYNCHRONIZED : S Y N C H R O N I Z E D;
TABLE : T A B L E;
TALLY : T A L L Y;
TALLYING : T A L L Y I N G;
TAPE : T A P E;
TERMINAL : T E R M I N A L;
TERMINATE : T E R M I N A T E;
TEST : T E S T;
TEXT : T E X T;
THAN : T H A N;
THEN : T H E N;
THREAD : T H R E A D;
THROUGH : T H R O U G H;
THRU : T H R U;
TIME : T I M E;
TIMER : T I M E R;
TIMES : T I M E S;
TO : T O;
TOP : T O P;
TRAILING : T R A I L I N G;
TRUE : T R U E;
TRUNCATED : T R U N C A T E D;
TYPE : T Y P E;
UNIT : U N I T;
UNLOCK : U N L O C K;
UNLOCKFILE : U N L O C K F I L E;
UNLOCKRECORD : U N L O C K R E C O R D;
UNSTRING : U N S T R I N G;
UNTIL : U N T I L;
UP : U P;
UPON : U P O N;
USAGE : U S A G E;
USE : U S E;
USING : U S I N G;
VALUE : V A L U E;
VALUES : V A L U E S;
VARYING : V A R Y I N G;
WAIT : W A I T;
WHEN : W H E N;
WHEN_COMPILED : W H E N MINUSCHAR C O M P I L E D;
WITH : W I T H;
WORDS : W O R D S;
WORKING_STORAGE : W O R K I N G MINUSCHAR S T O R A G E;
WRITE : W R I T E;
XOPTS: X O P T S;
YEAR : Y E A R;
YYYYMMDD : Y Y Y Y M M D D;
YYYYDDD : Y Y Y Y D D D;
ZERO : Z E R O;
ZEROS : Z E R O S;
ZEROES : Z E R O E S;


// symbols
AMPCHAR : '&';
ASTERISKCHAR : '*';
DOUBLEASTERISKCHAR : '**';
COLONCHAR : ':';
COMMACHAR : ',';
COMMENTTAG : '>*';
DOLLARCHAR : '$';
DOUBLEQUOTE : '"';
// period full stop
DOT_FS : '.' ('\r' | '\n' | '\f' | '\t' | ' ')+ | '.' EOF;
DOT : '.';
EQUALCHAR : '=';
LESSTHANCHAR : '<';
LESSTHANOREQUAL : '<=';
LPARENCHAR : '(';
MINUSCHAR : '-';
MORETHANCHAR : '>';
MORETHANOREQUAL : '>=';
PLUSCHAR : '+';
SINGLEQUOTE : '\'';
RPARENCHAR : ')';
SLASHCHAR : '/';


// literals
NONNUMERICLITERAL : STRINGLITERAL | DBCSLITERAL | HEXNUMBER;

fragment HEXNUMBER :
	X '"' [0-9A-F]+ '"'
	| X '\'' [0-9A-F]+ '\''
;

fragment STRINGLITERAL :
	'"' (~["\n\r] | '""' | '\'')* '"'
	| '\'' (~['\n\r] | '\'\'' | '"')* '\''
;

fragment DBCSLITERAL :
	[GN] '"' (~["\n\r] | '""' | '\'')* '"'
	| [GN] '\'' (~['\n\r] | '\'\'' | '"')* '\''
;

LEVEL_NUMBER_66 : '66';
LEVEL_NUMBER_77 : '77';
LEVEL_NUMBER_88 : '88';

INTEGERLITERAL : (PLUSCHAR | MINUSCHAR)? [0-9]+;

NUMERICLITERAL : (PLUSCHAR | MINUSCHAR)? [0-9]* (DOT | COMMACHAR) [0-9]+ (('e' | 'E') (PLUSCHAR | MINUSCHAR)? [0-9]+)?;

IDENTIFIER : [a-zA-Z0-9]+ ([-_]+ [a-zA-Z0-9]+)*;

// whitespace, line breaks, comments, ...
NEWLINE : '\r'? '\n' -> skip;
COMMENTLINE : COMMENTTAG ~('\n' | '\r')* -> skip;
WS : [ \t\f;]+ -> skip;
SEPARATOR : ', ' -> skip;



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