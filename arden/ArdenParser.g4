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
   : identifierBecomes (expr | newObjectPhrase | callPhrase | fuzzySetPhrase) # DataIdentifierOrObjectAssign
   | (IDENTIFIER ASSIGN | LET IDENTIFIER BE) dataIdentifierAssign # DataIdentifierAssignPhrase
   | (LPAREN dataVarList RPAREN ASSIGN | LET LPAREN dataVarList RPAREN BE) (READ (AS IDENTIFIER)? readPhrase | ARGUMENT | callPhrase) # DataDataVarList
   | (fhirObject ASSIGN | LET fhirObject BE) READ AS LATEST? fhirObject fhirReadPhrase? # DataFhir
   | IF expr THEN (dataStatement SC)* dataElseIf AGGREGATE? # DataIf
   | FOR identifierOrObjectRef IN expr DO (dataStatement SC)* ENDDO # DataFor
   | WHILE expr DO (dataStatement SC)* ENDDO # DataWhile
   | SWITCH identifierOrObjectRef dataSwitchCases ENDSWITCH AGGREGATE? # DataSwitch
   | (timeBecomes | applicabilityBecomes) expr # DataTimeOrApplicabilityAssign
   | BREAKLOOP # DataBreakLoop
   | INCLUDE IDENTIFIER # DataInclude
   ;

dataElseIf
   : ENDIF # DataEndIf
   | ELSE (dataStatement SC)* ENDIF # DataElse
   | ELSEIF expr THEN (dataStatement SC)* dataElseIf # DataElseIfRecursive
   ;

dataSwitchCases
   : CASE exprFactor (dataStatement SC)* dataSwitchCases? # DataSwitchCase
   | DEFAULT (dataStatement SC)* # DataSwitchDefault
   ;

dataIdentifierAssign
   : READ (AS IDENTIFIER)? readPhrase # DataRead
   | MLM (TERM (FROM INSTITUTION STRING)? | MLM_SELF) # DataMlm
   | (INTERFACE | EVENT | MESSAGE | DESTINATION) mappingFactor # DataInterface
   | (MESSAGE | DESTINATION) AS IDENTIFIER mappingFactor? # DataMessage
   | ARGUMENT # DataArgument
   | (OBJECT | LINGUISTIC VARIABLE) objectDefinition # DataObject
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
   : IDENTIFIER # IdentifierRefIdentifier
   | identifierOrObjectRef LBRACK expr RBRACK # IdentifierRefBrack
   | identifierOrObjectRef DOT IDENTIFIER # IdentifierRefObject
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
   : FUZZY SET fuzzySetInitElement (COMMA fuzzySetInitElement)* # FuzzySet
   | exprNegation FUZZIFIED BY exprNegation # FuzzifiedBy
   ;

fuzzySetInitElement
   : LPAREN exprNegation COMMA exprNegation RPAREN
   ;

readPhrase
   : (ofReadFuncOp OF?)? mappingFactor (WHERE exprOr)? # OfReadFuncOpReadWhere
   | fromOfFuncOp OF? mappingFactor (WHERE exprOr)? # FromOfFuncOpReadWhere
   | fromOfFuncOp exprFactor FROM mappingFactor (WHERE exprOr)? # FromOfFuncOpFromReadWhere
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
   : IT (DOT IDENTIFIER)+ fhirCompOp exprFactor # FhirCompOpAccess
   | FIND exprFactor IN IT (DOT IDENTIFIER)+ # FhirFindInAccess
   ;

fhirCompOp
   : simpleCompOp # FhirCompOpSimple
   | NOT? EQUAL # FhirCompOpNot
   | LESS THAN (OR EQUAL)? # FhirCompOpLess
   | GREATER THAN (OR EQUAL)? # FhirCompOpGreater
   | IS (IN | NOT) # FhirCompOpIs
   ;

objectDefinition
   : LBRACK IDENTIFIER (COMMA IDENTIFIER)* RBRACK
   ;

evokeStatement
   : simpleTrigger # EvokeSimple
   | delayedEventTrigger # EvokeDelayedEvent
   | constantTimeTrigger # EvokeConstantTime
   | EVERY evokeDuration FOR evokeDuration STARTING startingDelay (UNTIL expr)? # EvokePeriodicEvent
   | EVERY evokeDuration FOR evokeDuration STARTING constantTimeTrigger (UNTIL expr)? # EvokePeriodicConstantTime
   ;

simpleTrigger
   : eventOr # SimpleEventOr
   | ANY OF? LPAREN eventList RPAREN # SimpleEventList
   ;

delayedEventTrigger
   : evokeTimeOr AFTER TIME OF? IDENTIFIER
   ;

constantTimeTrigger
   : evokeTimeOr # ConstantTimeOr
   | evokeDuration AFTER evokeTime # ConstantAfter
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
   : evokeTime # EvokeExprTime
   | evokeDuration # EvokeExprDuration
   ;

evokeTime
   : (ISO_DATE_TIME | ISO_DATE) # EvokeIso
   | (TODAY | TOMORROW | WEEKDAYLITERAL) ATTIME TIME_OF_DAY # EvokeRelative
   ;

evokeDuration
   : NUMBER durationOp
   ;

startingDelay
   : delayedEventTrigger # StartingDelayed
   | TIME OF? IDENTIFIER # StartingEvent
   ;

logicStatement
   : (identifierBecomes | timeBecomes | applicabilityBecomes) expr # LogicIdentifierAssignSimple
   | identifierBecomes (newObjectPhrase | callPhrase | fuzzySetPhrase) # LogicIdentifierAssignComplex
   | (LPAREN dataVarList RPAREN ASSIGN | LET LPAREN dataVarList RPAREN BE) callPhrase # LogicDataVarList
   | IF expr THEN (logicStatement SC)* logicElseIf AGGREGATE? # LogicIf
   | FOR identifierOrObjectRef IN expr DO (logicStatement SC)* ENDDO # LogicFor
   | WHILE expr DO (logicStatement SC)* ENDDO # LogicWhile
   | SWITCH identifierOrObjectRef logicSwitchCases ENDSWITCH AGGREGATE? # LogicSwitch
   | BREAKLOOP # LogicBreakLoop
   | CONCLUDE expr # LogicConclude
   ;

logicElseIf
   : ENDIF # LogicEndIf
   | ELSE (logicStatement SC)* ENDIF # LogicElse
   | ELSEIF expr THEN (logicStatement SC)* logicElseIf # LogicElseIfRecursive
   ;

logicSwitchCases
   : CASE exprFactor (logicStatement SC)* logicSwitchCases? # LogicSwitchCase
   | DEFAULT (logicStatement SC)* # LogicSwitchDefault
   ;

actionStatement
   : (identifierBecomes | applicabilityBecomes) (expr | CONCLUDE) # ActionIdentifierAssign
   | IF expr THEN (actionStatement SC)* actionElseIf AGGREGATE? # ActionIf
   | FOR identifierOrObjectRef IN expr DO (actionStatement SC)* ENDDO # ActionFor
   | WHILE expr DO (actionStatement SC)* ENDDO # ActionWhile
   | SWITCH identifierOrObjectRef actionSwitchCases ENDSWITCH AGGREGATE? # ActionSwitch
   | timeBecomes expr # ActionTimeAssign
   | identifierBecomes newObjectPhrase # ActionIdentifierAssignObject
   | callPhrase (DELAY expr)? # ActionCallPhrase
   | WRITE expr (AT IDENTIFIER)? # ActionWrite
   | RETURN expr # ActionReturn
   | BREAKLOOP # ActionBreakLoop
   ;

actionElseIf
   : ENDIF # ActionEndIf
   | ELSE (actionStatement SC)* ENDIF # ActionElse
   | ELSEIF expr THEN (actionStatement SC)* actionElseIf # ActionElseIfRecursive
   ;

actionSwitchCases
   : CASE exprFactor (actionStatement SC)* actionSwitchCases? # ActionSwitchCase
   | DEFAULT (actionStatement SC)* # ActionSwitchDefault
   ;

expr
   : COMMA? exprMerge # ListUnary
   | expr COMMA exprMerge # ListBinary
   ;

exprMerge
   : exprSort # ToExprSort
   | exprMerge MERGE exprSort # MergeBinary
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
   : exprAnd # ToAnd
   | exprOr OR exprAnd # OrBinary
   ;

exprAnd
   : exprRange # ToRange
   | exprAnd AND exprRange # AndBinary
   ;

exprRange
   : exprNot (SEQTO exprNot)?
   ;

exprNot
   : NOT? exprComparison
   ;

exprComparison
   : exprString (simpleCompOp exprString)? # SimpleComparisonBinary
   | FIND exprString IN? STRINGOP exprComparison (STARTING AT exprString)? # FindInStringTernary
   | exprString IS NOT? mainCompOp # ComparisonIsMainCompOp
   | exprString NOT? IN exprString # InBinary
   | exprString OCCUR NOT? (binaryCompOpOccur exprString | ternaryCompOp | AT exprString) # ComparisonOccur
   | exprString MATCHES PATTERN exprString # MatchesPatternBinary
   ;

exprString
   : exprPlus # ToExprPlus
   | exprString (DOR | FORMATTED WITH) exprPlus # DorFormattedWithBinary
   | TRIM (LEFT | RIGHT)? exprString # TrimUnary
   | (UPPERCASE | LOWERCASE) exprString # StringCaseUnary
   | SUBSTRING exprPlus CHARACTERS (STARTING AT exprPlus)? FROM exprString # SubstringCharactersTernary
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
   : exprAgo # ToExprAgo
   | exprAgo (BEFORE | AFTER | FROM) exprAgo # BeforeAfterFrom
   ;

exprAgo
   : exprFunction AGO?
   ;

exprFunction
   : exprNegation (AS (NUMBEROP | TIME | STRINGOP | TRUTHVALUE))? # AsUnary
   | (ofReadFuncOp | ofNoReadFuncOp) OF? exprFunction # FuncOpsUnary
   | fromOfFuncOp OF? exprFunction (USING exprFunction)? # FromOfFuncOpUnary
   | REPLACE timePart OF? exprNegation WITH exprFunction # ReplaceWithBinary
   | fromOfFuncOp exprNegation FROM exprFunction (USING exprFunction)? # FromOfFuncOpBinary
   | NEAREST exprNegation FROM exprFunction # NearestFromBinary
   | INDEX indexFromOfFuncOp OF? exprFunction # IndexUnary
   | INDEX (MINIMUM | MAXIMUM) OF? exprNegation FROM exprFunction # IndexFromMinMax
   | AT (LEAST | MOST) exprNegation OF? ((ISTRUE | ARETRUE)? FROM)? exprFunction # AtLeastMostBinary
   | INDEX OF? exprNegation FROM exprFunction # IndexFromBinary
   | INDEX NEAREST exprNegation FROM exprFunction # IndexNearestBinary
   | ATTRIBUTE exprNegation FROM exprFunction # AttributeFromBinary
   | SUBLIST exprNegation ELEMENTS (STARTING AT exprNegation)? FROM exprFunction # SublistTernary
   ;

exprNegation
   : (PLUS | MINUS)? exprDuration
   ;

exprDuration
   : exprFactor durationOp?
   ;

exprFactor
   : exprAtom # ToAtom
   | exprFactor LBRACK expr RBRACK # FactorBrack
   | exprFactor DOT IDENTIFIER # FactorDot
   ;

exprAtom
   : (IDENTIFIER | NUMBER | NULL | IT) # AtomMisc
   | (STRING | (LOCALIZED TERM (BY exprAtom)?)) # AtomString
   | (NOW | ISO_DATE_TIME | ISO_DATE | EVENTTIME | TRIGGERTIME | CURRENTTIME | TIME_OF_DAY | TODAY | TOMORROW | WEEKDAYLITERAL) # AtomTime
   | ((TRUTHVALUE? (TRUE | FALSE)) | TRUTHVALUE NUMBER) # AtomBoolean
   | LPAREN (expr | fuzzySetPhrase)? RPAREN # AtomRecursive
   ;

simpleCompOp
   : (EQ | LT | GT | LE | GE | NE)
   ;

mainCompOp
   : unaryCompOp # ToUnaryCompOp
   | binaryCompOp exprString # ToBinaryCompOp
   | binaryCompOpOccur exprString # ToBinaryCompOpOccur
   | ternaryCompOp # ToTernaryCompOp
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