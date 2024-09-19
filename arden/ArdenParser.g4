/*
BSD License

Copyright (c) 2023, Christian Weich
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. Neither the name of Tom Everett nor the names of its contributors
   may be used to endorse or promote products derived from this software
   without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

parser grammar ArdenParser;

options { tokenVocab = ArdenLexer; }

mlm

   : maintenanceCategory libraryCategory knowledgeCategory resourcesCategory END COLON EOF
   ;

maintenanceCategory
   : MAINTENANCE COLON titleSlot mlmNameSlot ardenVersionSlot versionSlot institutionSlot authorSlot specialistSlot dateSlot validationSlot
   ;

libraryCategory
   : LIBRARY COLON purposeSlot explanationSlot keywordsSlot citationsSlot? linksSlot?
   ;

knowledgeCategory
   : KNOWLEDGE COLON typeSlot dataSlot prioritySlot? evokeSlot logicSlot actionSlot urgencySlot?
   ;

resourcesCategory
   : RESOURCES COLON defaultSlot languageSlot+
   ;

titleSlot
   : TITLE TEXTMODECOLON slotText ADSC
   ;

slotText
   : (UTEXT | TEXTMODECOLON | TEXT)+
   ;

mlmNameSlot
   : (MLMNAME | FILENAME) MlMCOLON MLMID DSC
   ;

ardenVersionSlot
   : ARDEN COLON ARDEN_VERSION DSC
   ;

versionSlot
   : VERSION TEXTMODECOLON slotText ADSC
   ;

institutionSlot
   : INSTITUTION_SLOT slotText ADSC
   ;

authorSlot
   : AUTHOR TEXTMODECOLON slotText ADSC
   ;

specialistSlot
   : SPECIALIST TEXTMODECOLON slotText ADSC
   ;

dateSlot
   : DATE COLON (ISO_DATE | ISO_DATE_TIME) DSC
   ;

validationSlot
   : VALIDATION COLON VALIDATION_CODE DSC
   ;

purposeSlot
   : PURPOSE TEXTMODECOLON slotText ADSC
   ;

explanationSlot
   : EXPLANATION TEXTMODECOLON slotText ADSC
   ;

keywordsSlot
   : KEYWORDS TEXTMODECOLON slotText ADSC
   ;

citationsSlot
   : CITATIONS TEXTMODECOLON slotText ADSC
   ;

linksSlot
   : LINKS TEXTMODECOLON slotText ADSC
   ;

typeSlot
   : TYPE COLON TYPE_CODE DSC
   ;

dataSlot
   : DATA COLON dataStatement? (SC dataStatement)* SC? DSC
   ;

prioritySlot
   : PRIORITY COLON NUMBER DSC
   ;

evokeSlot
   : EVOKE COLON evokeStatement? (SC evokeStatement)* SC? DSC
   ;

logicSlot
   : LOGIC COLON logicStatement? (SC logicStatement)* SC? DSC
   ;

actionSlot
   : ACTION COLON actionStatement? (SC actionStatement)* SC? DSC
   ;

urgencySlot
   : URGENCY COLON (NUMBER | identifierOrObjectRef) DSC
   ;

defaultSlot
   : DEFAULT_SLOT TWOCHARCODE DSC
   ;

languageSlot
   : LANGUAGE TWOCHARCOLON TWOCHARCODE (TERM COLON STRING)? (SC TERM COLON STRING)* SC? DSC
   ;

dataStatement
   : identifierBecomes (expr | newObjectPhrase | callPhrase | fuzzySetPhrase)
   | (IDENTIFIER ASSIGN | LET IDENTIFIER BE) dataIdentifierAssign
   | (LPAREN dataVarList RPAREN ASSIGN | LET LPAREN dataVarList RPAREN BE) (READ (AS IDENTIFIER)? readPhrase | ARGUMENT | callPhrase)
   | (fhirObject ASSIGN | LET fhirObject BE) READ AS LATEST? fhirObject fhirReadPhrase?
   | IF expr THEN (dataStatement SC)* dataElseIf AGGREGATE?
   | FOR identifierOrObjectRef IN expr DO (dataStatement SC)* ENDDO
   | WHILE expr DO (dataStatement SC)* ENDDO
   | SWITCH identifierOrObjectRef dataSwitchCases ENDSWITCH AGGREGATE?
   | (timeBecomes | applicabilityBecomes) expr
   | BREAKLOOP
   | INCLUDE IDENTIFIER
   ;

dataElseIf
   : ENDIF
   | ELSE (dataStatement SC)* ENDIF
   | ELSEIF expr THEN (dataStatement SC)* dataElseIf
   ;

dataSwitchCases
   : CASE exprFactor (dataStatement SC)* dataSwitchCases?
   | DEFAULT (dataStatement SC)*
   ;

dataIdentifierAssign
   : READ (AS IDENTIFIER)? readPhrase
   | MLM (TERM (FROM INSTITUTION STRING)? | MLM_SELF)
   | (INTERFACE | EVENT | MESSAGE | DESTINATION) mappingFactor
   | (MESSAGE | DESTINATION) AS IDENTIFIER mappingFactor?
   | ARGUMENT
   | (OBJECT | LINGUISTIC VARIABLE) objectDefinition
   ;

identifierBecomes
   : identifierOrObjectRef ASSIGN
   | LET identifierOrObjectRef BE
   ;

timeBecomes
   : TIME OF? identifierOrObjectRef ASSIGN
   | LET TIME OF? identifierOrObjectRef BE
   ;

applicabilityBecomes
   : APPLICABILITY OF? identifierOrObjectRef ASSIGN
   | LET APPLICABILITY OF? identifierOrObjectRef BE
   ;

dataVarList
   : identifierOrObjectRef (COMMA dataVarList)?
   ;

identifierOrObjectRef
   : IDENTIFIER
   | identifierOrObjectRef LBRACK expr RBRACK
   | identifierOrObjectRef DOT IDENTIFIER
   ;

newObjectPhrase
   : NEW IDENTIFIER (WITH (expr | LBRACK objectInitList RBRACK | expr WITH LBRACK objectInitList RBRACK))?
   ;

objectInitList
   : objectInitElement (COMMA objectInitElement)*
   ;

objectInitElement
   : (IDENTIFIER ASSIGN | LET IDENTIFIER BE) expr
   ;

fuzzySetPhrase
   : FUZZY SET fuzzySetInitElement (COMMA fuzzySetInitElement)*
   | exprNegation FUZZIFIED BY exprNegation
   ;

fuzzySetInitElement
   : LPAREN exprNegation COMMA exprNegation RPAREN
   ;

readPhrase
   : (ofReadFuncOp OF?)? mappingFactor (WHERE exprOr)?
   | fromOfFuncOp OF? mappingFactor (WHERE exprOr)?
   | fromOfFuncOp exprFactor FROM mappingFactor (WHERE exprOr)?
   ;

mappingFactor
   : LBRACE DATA_MAPPING RBRACE
   ;

callPhrase
   : CALL IDENTIFIER (WITH expr)?
   ;

fhirObject
   : IDENTIFIER (LBRACK dataVarList RBRACK)?
   ;

fhirReadPhrase
   : WHERE fhirAccessPhrase (AND fhirAccessPhrase)*
   ;

fhirAccessPhrase
   : IT (DOT IDENTIFIER)+ fhirCompOp exprFactor
   | FIND exprFactor IN IT (DOT IDENTIFIER)+
   ;

fhirCompOp
   : simpleCompOp
   | NOT? EQUAL
   | LESS THAN (OR EQUAL)?
   | GREATER THAN (OR EQUAL)?
   | IS (IN | NOT)
   ;

objectDefinition
   : LBRACK IDENTIFIER (COMMA IDENTIFIER)* RBRACK
   ;

evokeStatement
   : simpleTrigger
   | delayedEventTrigger
   | constantTimeTrigger
   | EVERY evokeDuration FOR evokeDuration STARTING startingDelay (UNTIL expr)?
   | EVERY evokeDuration FOR evokeDuration STARTING constantTimeTrigger (UNTIL expr)?
   ;

simpleTrigger
   : eventOr
   | ANY OF? LPAREN eventList RPAREN
   ;

delayedEventTrigger
   : evokeTimeOr AFTER TIME OF? IDENTIFIER
   ;

constantTimeTrigger
   : evokeTimeOr
   | evokeDuration AFTER evokeTime
   ;

eventOr
   : IDENTIFIER (OR IDENTIFIER)*
   ;

eventList
   : IDENTIFIER (COMMA IDENTIFIER)*
   ;

evokeTimeOr
   : evokeTimeExpr (OR evokeTimeExpr)*
   ;

evokeTimeExpr
   : evokeTime
   | evokeDuration
   ;

evokeTime
   : (ISO_DATE_TIME | ISO_DATE)
   | (TODAY | TOMORROW | WEEKDAYLITERAL) ATTIME TIME_OF_DAY
   ;

evokeDuration
   : NUMBER durationOp
   ;

startingDelay
   : delayedEventTrigger
   | TIME OF? IDENTIFIER
   ;

logicStatement
   : (identifierBecomes | timeBecomes | applicabilityBecomes) expr
   | identifierBecomes (newObjectPhrase | callPhrase | fuzzySetPhrase)
   | (LPAREN dataVarList RPAREN ASSIGN | LET LPAREN dataVarList RPAREN BE) callPhrase
   | IF expr THEN (logicStatement SC)* logicElseIf AGGREGATE?
   | FOR identifierOrObjectRef IN expr DO (logicStatement SC)* ENDDO
   | WHILE expr DO (logicStatement SC)* ENDDO
   | SWITCH identifierOrObjectRef logicSwitchCases ENDSWITCH AGGREGATE?
   | BREAKLOOP
   | CONCLUDE expr
   ;

logicElseIf
   : ENDIF
   | ELSE (logicStatement SC)* ENDIF
   | ELSEIF expr THEN (logicStatement SC)* logicElseIf
   ;

logicSwitchCases
   : CASE exprFactor (logicStatement SC)* logicSwitchCases?
   | DEFAULT (logicStatement SC)*
   ;

actionStatement
   : (identifierBecomes | applicabilityBecomes) (expr | CONCLUDE)
   | IF expr THEN (actionStatement SC)* actionElseIf AGGREGATE?
   | FOR identifierOrObjectRef IN expr DO (actionStatement SC)* ENDDO
   | WHILE expr DO (actionStatement SC)* ENDDO
   | SWITCH identifierOrObjectRef actionSwitchCases ENDSWITCH AGGREGATE?
   | timeBecomes expr
   | identifierBecomes newObjectPhrase
   | callPhrase (DELAY expr)?
   | WRITE expr (AT IDENTIFIER)?
   | RETURN expr
   | BREAKLOOP
   ;

actionElseIf
   : ENDIF
   | ELSE (actionStatement SC)* ENDIF
   | ELSEIF expr THEN (actionStatement SC)* actionElseIf
   ;

actionSwitchCases
   : CASE exprFactor (actionStatement SC)* actionSwitchCases?
   | DEFAULT (actionStatement SC)*
   ;

expr
   : COMMA? exprMerge
   | expr COMMA exprMerge
   ;

exprMerge
   : exprSort
   | exprMerge MERGE exprSort
   ;

exprSort
   : (SORT (DATA | TIME | APPLICABILITY)?)? exprAddList (USING exprFunction)?
   ;

exprAddList
   : ADD? exprRemoveList (TO exprRemoveList (AT exprRemoveList)?)?
   ;

exprRemoveList
   : (REMOVE exprWhere FROM)? exprWhere
   ;

exprWhere
   : exprOr (WHERE exprOr)?
   ;

exprOr
   : exprAnd
   | exprOr OR exprAnd
   ;

exprAnd
   : exprRange
   | exprAnd AND exprRange
   ;

exprRange
   : exprNot (SEQTO exprNot)?
   ;

exprNot
   : NOT? exprComparison
   ;

exprComparison
   : exprString (simpleCompOp exprString)?
   | FIND exprString IN? STRINGOP exprComparison (STARTING AT exprString)?
   | exprString IS NOT? mainCompOp
   | exprString NOT? IN exprString
   | exprString OCCUR NOT? (binaryCompOpOccur exprString | ternaryCompOp | AT exprString)
   | exprString MATCHES PATTERN exprString
   ;

exprString
   : exprPlus
   | exprString (DOR | FORMATTED WITH) exprPlus
   | TRIM (LEFT | RIGHT)? exprString
   | (UPPERCASE | LOWERCASE) exprString
   | SUBSTRING exprPlus CHARACTERS (STARTING AT exprPlus)? FROM exprString
   ;

exprPlus
   : exprMul ((PLUS | MINUS) exprMul)*
   ;

exprMul
   : exprPower ((MUL | DIV) exprPower)*
   ;

exprPower
   : exprAtTime (POWER exprAtTime)?
   ;

exprAtTime
   : exprBefore (ATTIME exprAtTime)?
   ;

exprBefore
   : exprAgo
   | exprAgo (BEFORE | AFTER | FROM) exprAgo
   ;

exprAgo
   : exprFunction AGO?
   ;

exprFunction
   : exprNegation (AS (NUMBEROP | TIME | STRINGOP | TRUTHVALUE))?
   | (ofReadFuncOp | ofNoReadFuncOp) OF? exprFunction
   | fromOfFuncOp OF? exprFunction (USING exprFunction)?
   | REPLACE timePart OF? exprNegation WITH exprFunction
   | fromOfFuncOp exprNegation FROM exprFunction (USING exprFunction)?
   | NEAREST exprNegation FROM exprFunction
   | INDEX indexFromOfFuncOp OF? exprFunction
   | INDEX (MINIMUM | MAXIMUM) OF? exprNegation FROM exprFunction
   | AT (LEAST | MOST) exprNegation OF? ((ISTRUE | ARETRUE)? FROM)? exprFunction
   | INDEX OF? exprNegation FROM exprFunction
   | INDEX NEAREST exprNegation FROM exprFunction
   | ATTRIBUTE exprNegation FROM exprFunction
   | SUBLIST exprNegation ELEMENTS (STARTING AT exprNegation)? FROM exprFunction
   ;

exprNegation
   : (PLUS | MINUS)? exprDuration
   ;

exprDuration
   : exprFactor durationOp?
   ;

exprFactor
   : exprAtom
   | exprFactor LBRACK expr RBRACK
   | exprFactor DOT IDENTIFIER
   ;

exprAtom
   : (IDENTIFIER | NUMBER | NULL | IT)
   | (STRING | (LOCALIZED TERM (BY exprAtom)?))
   | (NOW | ISO_DATE_TIME | ISO_DATE | EVENTTIME | TRIGGERTIME | CURRENTTIME | TIME_OF_DAY | TODAY | TOMORROW | WEEKDAYLITERAL)
   | ((TRUTHVALUE? (TRUE | FALSE)) | TRUTHVALUE NUMBER)
   | LPAREN (expr | fuzzySetPhrase)? RPAREN
   ;

simpleCompOp
   : (EQ | LT | GT | LE | GE | NE)
   ;

mainCompOp
   : unaryCompOp
   | binaryCompOp exprString
   | binaryCompOpOccur exprString
   | ternaryCompOp
   ;

unaryCompOp
   : (PRESENT | NULL | BOOLEAN | TRUTHVALUE | NUMBEROP | STRINGOP | TIME | DURATION | LIST | OBJECT | IDENTIFIER | FUZZY | CRISP)
   | TIME OF DAY
   | LINGUISTIC VARIABLE
   ;

binaryCompOp
   : LESS THAN (OR EQUAL)?
   | GREATER THAN (OR EQUAL)?
   | IN
   ;

binaryCompOpOccur
   : (EQUAL | BEFORE | AFTER)
   | WITHIN (PAST | SAME DAY AS)
   ;

ternaryCompOp
   : WITHIN exprString (TO | PRECEDING | FOLLOWING | SURROUNDING) exprString
   ;

ofReadFuncOp
   : (AVERAGE | COUNT | EXIST | SUM | MEDIAN)
   ;

ofNoReadFuncOp
   : (SLOPE | STDDEV | VARIANCE | INTERVAL | ARCCOS | ARCSIN | ARCTAN | COSINE | SINE | TANGENT | EXP | FLOOR | INT | ROUND | CEILING | TRUNCATE | LOG | LOG10 | ABS | SQRT | STRINGOP | REVERSE | LENGTH | CLONE | APPLICABILITY | DEFUZZIFIED)
   | ANY ISTRUE?
   | ALL ARETRUE?
   | NO ISTRUE?
   | PERCENT? (INCREASE | DECREASE)
   | TIME (OF DAY)?
   | DAY OF WEEK
   | EXTRACT (YEAR | MONTH | DAY | HOUR | MINUTE | SECOND | CHARACTERS | ATTRIBUTE NAMES)
   ;

fromOfFuncOp
   : (MINIMUM | MAXIMUM | LAST | FIRST | EARLIEST | LATEST)
   ;

indexFromOfFuncOp
   : (MINIMUM | MAXIMUM | EARLIEST | LATEST)
   ;

durationOp
   : (YEAR | MONTH | WEEK | DAY | HOUR | MINUTE | SECOND)
   ;

timePart
   : (YEAR | MONTH | DAY | HOUR | MINUTE | SECOND)
   ;