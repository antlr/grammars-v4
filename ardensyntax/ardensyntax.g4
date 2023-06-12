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

parser grammar Arden;

options { tokenVocab = ArdenLexer; }

mlms
   : mlms mlm
   | mlm
   | EOF
   ;

mlm
   : maintenanceCategory libraryCategory knowledgeCategory resourcesCategory END
   ;

maintenanceCategory
   : MAINTENANCE titleSlot mlmNameSlot ardenVersionSlot versionSlot institutionSlot authorSlot specialistSlot dateSlot validationSlot
   ;

libraryCategory
   : LIBRARY purposeSlot explanationSlot keywordsSlot citationsSlot linksSlot
   ;

knowledgeCategory
   : KNOWLEDGE typeSlot dataSlot prioritySlot evokeSlot logicSlot actionSlot urgencySlot
   ;

resourcesCategory
   : (RESOURCES defaultSlot languageSlot)?
   ;

titleSlot
   : TITLE TEXT
   ;

mlmNameSlot
   : MLMNAME MLMID DSC
   | FILENAME MLMID DSC
   ;

ardenVersionSlot
   : (ARDEN ARDEN_VERSION DSC)?
   ;

versionSlot
   : VERSION TEXT
   ;

institutionSlot
   : INSTITUTION TEXT
   ;

authorSlot
   : AUTHOR TEXT
   ;

specialistSlot
   : SPECIALIST TEXT
   ;

dateSlot
   : DATE (ISO_DATE | ISO_DATE_TIME) DSC
   ;

validationSlot
   : VALIDATION VALIDATION_CODE DSC
   ;
   
purposeSlot
   : PURPOSE TEXT
   ;

explanationSlot
   : EXPLANATION TEXT
   ;

keywordsSlot
   : KEYWORDS TEXT
   ;

citationsSlot
   : (CITATIONS citationList DSC)?
   ;

citationList
   : singleCitation?
   | singleCitation SC citationList
   ;

singleCitation
   : CITATION? STRING
   ;

linksSlot
   : (LINKS linkList DSC)?
   ;

linkList
   : singleLink?
   | linkList SC singleLink
   ;

singleLink
   : LINK_TYPE? TERM? STRING
   ;
   
typeSlot
   : TYPE TYPE_CODE DSC
   ;

dataSlot
   : DATA dataBlock DSC
   ;

prioritySlot
   : (PRIORITY NUMBER DSC)?
   ;

evokeSlot
   : EVOKE evokeBlock DSC
   ;

logicSlot
   : LOGIC logicBlock DSC
   ;

actionSlot
   : ACTION actionBlock DSC
   ;

urgencySlot
   : (URGENCY (NUMBER | IDENTIFIER) DSC)?
   ;

defaultSlot
   : DEFAULTCO TWOCHARCODE DSC
   ;

languageSlot
   : languageSlot singleLanguageCode
   | singleLanguageCode
   ;

singleLanguageCode
   : LANGUAGE TWOCHARCODE resourceTerms DSC
   ;

resourceTerms
   : resourceTerms SC TERM COLON STRING
   | (TERM COLON STRING)?
   ;

logicBlock
   : logicBlock SC logicStatement
   | logicStatement
   ;

logicStatement
   : logicAssignment?
   | IF logicIfThenElse
   | FOR IDENTIFIER IN expr DO logicBlock SC ENDDO
   | WHILE expr DO logicBlock SC ENDDO
   | logicSwitch
   | BREAKLOOP
   | CONCLUDE expr
   ;

logicIfThenElse
   : expr THEN logicBlock SC logicElseIf
   ;

logicElseIf
   : ENDIF AGGREGATE?
   | ELSE logicBlock SC ENDIF AGGREGATE?
   | ELSEIF logicIfThenElse
   ;

logicAssignment
   : identifierBecomes expr
   | timeBecomes expr
   | applicabilityBecomes expr
   | identifierBecomes callPhrase
   | LPAREN dataVarList RPAREN ASSIGN callPhrase
   | LET LPAREN dataVarList RPAREN BE callPhrase
   | identifierBecomes newObjectPhrase
   | identifierBecomes fuzzySetPhrase
   ;

exprFuzzySet
   : fuzzySetPhrase
   | expr
   ;

identifierBecomes
   : identifierOrObjectRef ASSIGN
   | LET identifierOrObjectRef BE
   | NOW ASSIGN
   ;

logicSwitch
   : SWITCH IDENTIFIER COLON logicSwitchCases ENDSWITCH AGGREGATE?
   ;

logicSwitchCases
   : (CASE exprFactor logicBlock logicSwitchCases)?
   | DEFAULT logicBlock
   ;

identifierOrObjectRef
   : IDENTIFIER
   | identifierOrObjectRef LBRACK expr RBRACK
   | identifierOrObjectRef DOT identifierOrObjectRef
   ;

timeBecomes
   : TIME OF? IDENTIFIER ASSIGN
   | LET TIME OF? IDENTIFIER BE
   ;

applicabilityBecomes
   : APPLICABILITY OF? IDENTIFIER ASSIGN
   | LET APPLICABILITY OF? IDENTIFIER BE
   ;

callPhrase
   : CALL IDENTIFIER (WITH expr)?
   ;
   
expr
   : exprSort
   | expr COMMA exprSort
   | COMMA exprSort
   ;

exprSort
   : exprAddList (MERGE exprSort)?
   | SORT sortOption exprSort
   | exprAddList MERGE exprSort USING exprFunction
   | SORT sortOption exprSort USING exprFunction
   ;

sortOption
   : DATAWC?
   | TIME
   | APPLICABILITY
   ;

exprAddList
   : exprRemoveList
   | ADD exprWhere TO exprWhere (AT exprWhere)?
   ;

exprRemoveList
   : exprWhere
   | REMOVE exprWhere FROM exprWhere
   ;

exprWhere
   : exprRange
   | exprRange WHERE exprRange
   ;

exprRange
   : exprOr
   | exprOr SEQTO exprOr
   ;

exprOr
   : exprOr OR exprAnd
   | exprAnd
   ;

exprAnd
   : exprAnd AND exprNot
   | exprNot
   ;

exprNot
   : NOT exprComparison
   | exprComparison
   ;

exprComparison
   : exprString
   | exprFindString
   | exprString singleCompOp exprString
   | exprString IS NOT? mainCompOp
   | exprString NOT? inCompOp
   | exprString OCCUR NOT? temporalCompOp
   | exprString OCCUR NOT? rangeCompOp
   | exprString MATCHES PATTERN exprString
   ;

exprFindString
   : FIND exprString IN? STRINGOP exprString stringSearchStart
   ;

exprString
   : exprPlus
   | exprString '||' exprPlus
   | exprString FORMATTED WITH exprPlus
   | TRIM trimOption exprString
   | caseOption exprString
   | SUBSTRING exprPlus CHARACTERS stringSearchStart FROM exprString
   ;

trimOption
   : LEFT?
   | RIGHT
   ;

caseOption
   : UPPERCASE
   | LOWERCASE
   ;

stringSearchStart
   : (STARTING AT exprPlus)?
   ;

exprPlus
   : exprTimes
   | exprPlus PLUS exprTimes
   | exprPlus MINUS exprTimes
   | PLUS exprTimes
   | MINUS exprTimes
   ;

exprTimes
   : exprPower
   | exprTimes MUL exprPower
   | exprTimes DIV exprPower
   ;

exprPower
   : exprAtTime
   | exprFunction POWER exprFunction
   ;

exprAtTime
   : exprBefore
   | exprBefore ATTIME exprAtTime
   ;

exprBefore
   : exprAgo
   | exprDuration BEFORE exprAgo
   | exprDuration AFTER exprAgo
   | exprDuration FROM exprAgo
   ;

exprAgo
   : exprDuration AGO?
   | exprFunction AGO?
   ;

exprDuration
   : exprFunction
   | exprFunction durationOp
   ;

exprFunction
   : exprFactor
   | ofFuncOp OF? exprFunction
   | fromOfFuncOp OF? exprFunction
   | fromOfFuncOp exprFactor FROM exprFunction
   | REPLACE timePart OF? exprFunction WITH exprFactor
   | fromOfFuncOp OF? exprFunction USING exprFunction
   | fromOfFuncOp exprFactor FROM exprFunction USING exprFunction
   | fromFuncOp exprFactor FROM exprFunction
   | indexFromOfFuncOp OF? exprFunction
   | indexFromOfFuncOp exprFactor FROM exprFunction
   | atLeastMostOp exprFactor FROM exprFunction
   | atLeastMostOp exprFactor ISTRUE FROM exprFunction
   | atLeastMostOp exprFactor ARETRUE FROM exprFunction
   | INDEX OF exprFactor FROM exprFunction
   | indexFromFuncOp exprFactor FROM exprFunction
   | exprFactor AS asFuncOp
   | exprAttributeFrom
   | exprSubListFrom
   ;

exprAttributeFrom
   : ATTRIBUTE exprFactor FROM exprFactor
   ;

exprSubListFrom
   : SUBLIST exprFactor FROM exprFactor
   | SUBLIST exprFactor STARTING AT exprFactor FROM exprFactor
   ;

exprFactor
   : exprFactorAtom
   | exprFactorAtom LBRACK expr RBRACK
   | exprFactor DOT IDENTIFIER
   ;

exprFactorAtom
   : IDENTIFIER
   | NUMBER
   | string
   | timeValue
   | booleanValue
   | WEEKDAYLITERAL
   | TODAY
   | TOMORROW
   | NULL
   | CONCLUDE
   | IT
   | LPAREN RPAREN
   | LPAREN expr RPAREN
   | LPAREN exprFuzzySet RPAREN
   ;

singleCompOp
   : EQ
   | LT
   | GT
   | LE
   | GE
   | NE
   ;

mainCompOp
   : temporalCompOp
   | rangeCompOp
   | unaryCompOp
   | binaryCompOp exprString
   ;

rangeCompOp
   : WITHIN exprString TO exprString
   ;

temporalCompOp
   : WITHIN exprString PRECEDING exprString
   | WITHIN exprString FOLLOWING exprString
   | WITHIN exprString SURROUNDING exprString
   | WITHIN PAST exprString
   | WITHIN SAME DAY AS exprString
   | BEFORE exprString
   | AFTER exprString
   | EQUAL exprString
   | AT exprString
   ;

unaryCompOp
   : PRESENT
   | NULL
   | BOOLEAN
   | TRUTHVALUE
   | CRISP
   | FUZZY
   | NUMBEROP
   | TIME
   | DURATION
   | STRINGOP
   | LIST
   | OBJECT
   | LINGUISTIC VARIABLE
   | IDENTIFIER
   | TIME OF DAY
   ;

binaryCompOp
   : LESS THAN
   | GREATER THAN
   | GREATER THAN OR EQUAL
   | LESS THAN OR EQUAL
   | IN
   ;

ofFuncOp
   : ofReadFuncOp
   | ofNoreadFuncOp
   ;

inCompOp
   : IN exprString
   ;

ofReadFuncOp
   : AVERAGE
   | COUNT
   | EXIST
   | SUM
   | MEDIAN
   ;

ofNoreadFuncOp
   : ANY
   | ANY ISTRUE
   | ALL
   | ALL ARETRUE
   | NO
   | NO ISTRUE
   | SLOPE
   | STDDEV
   | VARIANCE
   | INCREASE
   | PERCENT INCREASE
   | DECREASE
   | PERCENT DECREASE
   | INTERVAL
   | TIME
   | TIME OF DAY
   | DAY OF WEEK
   | ARCCOS
   | ARCSIN
   | ARCTAN
   | COSINE
   | SINE
   | TANGENT
   | EXP
   | FLOOR
   | INT
   | ROUND
   | CEILING
   | TRUNCATE
   | LOG
   | LOG10
   | ABS
   | SQRT
   | EXTRACT YEAR
   | EXTRACT MONTH
   | EXTRACT DAY
   | EXTRACT HOUR
   | EXTRACT MINUTE
   | EXTRACT SECOND
   | EXTRACT TIME OF DAY
   | STRINGOP
   | EXTRACT CHARACTERS
   | REVERSE
   | LENGTH
   | CLONE
   | EXTRACT ATTRIBUTE NAMES
   | APPLICABILITY
   | DEFUZZIFIED
   ;

fromFuncOp
   : NEAREST
   ;

indexFromFuncOp
   : INDEX NEAREST
   ;

fromOfFuncOp
   : MINIMUM
   | MAXIMUM
   | LAST
   | FIRST
   | EARLIEST
   | LATEST
   ;

indexFromOfFuncOp
   : INDEX MINIMUM
   | INDEX MAXIMUM
   | INDEX EARLIEST
   | INDEX LATEST
   ;

asFuncOp
   : NUMBEROP
   | TIME
   | STRINGOP
   | TRUTHVALUE
   ;

atLeastMostOp
   : AT LEAST
   | AT MOST
   ;

durationOp
   : YEAR
   | MONTH
   | WEEK
   | DAY
   | HOUR
   | MINUTE
   | SECOND
   ;

timePart
   : YEAR
   | MONTH
   | DAY
   | HOUR
   | MINUTE
   | SECOND
   ;

string
   : STRING
   | LOCALIZED TERM localizeOption
   ;

localizeOption
   : (BY STRING)?
   | BY IDENTIFIER
   ;

booleanValue
   : TRUE
   | FALSE
   | TRUTHVALUE NUMBER
   | TRUTHVALUE TRUE
   | TRUTHVALUE FALSE
   ;

timeValue
   : NOW
   | ISO_DATE_TIME
   | ISO_DATE
   | EVENTTIME
   | TRIGGERTIME
   | CURRENTTIME
   | TIMEOFDAY
   ;

dataBlock
   : dataBlock SC dataStatement
   | dataStatement
   ;

dataStatement
   : dataAssignment?
   | IF dataIfThenElse
   | FOR IDENTIFIER IN expr DO dataBlock SC ENDDO
   | WHILE expr DO dataBlock SC ENDDO
   | dataSwitch
   | BREAKLOOP
   | INCLUDE IDENTIFIER
   ;

dataIfThenElse
   : expr THEN dataBlock SC dataElseIf
   ;

dataElseIf
   : ENDIF AGGREGATE?
   | ELSE dataBlock SC ENDIF AGGREGATE?
   | ELSEIF dataIfThenElse
   ;

dataSwitch
   : SWITCH IDENTIFIER COLON dataSwitchCases ENDSWITCH AGGREGATE?
   ;

dataSwitchCases
   : (CASE exprFactor dataBlock dataSwitchCases)?
   | DEFAULT dataBlock
   ;

dataAssignment
   : identifierBecomes dataAssignPhrase
   | timeBecomes expr
   | applicabilityBecomes expr
   | LPAREN dataVarList RPAREN ASSIGN READ readPhrase
   | LET LPAREN dataVarList RPAREN BE READ readPhrase
   | LPAREN dataVarList RPAREN ASSIGN READ AS IDENTIFIER readPhrase
   | LET LPAREN dataVarList RPAREN BE READ AS IDENTIFIER readPhrase
   | LPAREN dataVarList RPAREN ASSIGN ARGUMENT
   | LET LPAREN dataVarList RPAREN BE ARGUMENT
   ;

dataVarList
   : IDENTIFIER
   | IDENTIFIER COMMA dataVarList
   ;

dataAssignPhrase
   : READ readPhrase
   | READ AS IDENTIFIER readPhrase
   | MLM TERM
   | MLM TERM FROM INSTITUTIONWC string
   | MLM MLM_SELF
   | INTERFACE mappingFactor
   | EVENT mappingFactor
   | MESSAGE mappingFactor
   | MESSAGE AS IDENTIFIER mappingFactor?
   | DESTINATION mappingFactor
   | DESTINATION AS IDENTIFIER mappingFactor?
   | ARGUMENT
   | OBJECT objectDefinition
   | LINGUISTIC VARIABLE objectDefinition
   | callPhrase
   | newObjectPhrase
   | fuzzySetPhrase
   | expr
   ;

fuzzySetPhrase
   : FUZZY SET fuzzySetInitList
   | exprDuration FUZZIFIED BY exprDuration
   | exprFactor FUZZIFIED BY exprFactor
   ;

fuzzySetInitList
   : fuzzySetInitElement
   | fuzzySetInitList COMMA fuzzySetInitElement
   ;

fuzzySetInitElement
   : LPAREN fuzzySetInitFactor COMMA exprFactor RPAREN
   ;

fuzzySetInitFactor
   : exprFactor
   | NUMBER durationOp
   ;

readPhrase
   : readWhere
   | ofReadFuncOp OF? readWhere
   | fromOfFuncOp OF? readWhere
   | fromOfFuncOp exprFactor FROM readWhere
   ;

readWhere
   : mappingFactor
   | mappingFactor WHERE IT OCCUR temporalCompOp
   | mappingFactor WHERE IT OCCUR NOT temporalCompOp
   | mappingFactor WHERE IT OCCUR rangeCompOp
   | mappingFactor WHERE IT OCCUR NOT rangeCompOp
   | LPAREN readWhere RPAREN
   ;

mappingFactor
   : LBRACE DATA_MAPPING RBRACE
   ;

objectDefinition
   : LBRACK objectAttributeList RBRACK
   ;

objectAttributeList
   : IDENTIFIER
   | IDENTIFIER COMMA objectAttributeList
   ;

newObjectPhrase
   : NEW IDENTIFIER (WITH expr)?
   | NEW IDENTIFIER WITH LBRACK objectInitList RBRACK
   | NEW IDENTIFIER WITH expr WITH LBRACK objectInitList RBRACK
   ;

objectInitList
   : objectInitElement
   | objectInitList COMMA objectInitElement
   ;

objectInitElement
   : IDENTIFIER ASSIGN expr
   ;

evokeBlock
   : evokeStatement
   | evokeBlock SC evokeStatement
   ;

evokeStatement
   : (eventOr)?
   | evokeTime
   | delayedEvoke
   | qualifiedEvokeCycle
   | CALL
   ;

eventList
   : eventOr
   | eventList COMMA eventOr
   ;

eventOr
   : eventOr OR eventAny
   | eventAny
   ;

eventAny
   : ANY OF? LPAREN eventList RPAREN
   | ANY OF? IDENTIFIER
   | eventFactor
   ;

eventFactor
   : LPAREN eventOr RPAREN
   | IDENTIFIER
   ;

delayedEvoke
   : evokeTimeExprOr (AFTER eventTime)?
   | evokeDuration AFTER evokeTimeOr
   ;

eventTime
   : TIME OF? eventAny
   ;

evokeTimeOr
   : evokeTime
   | evokeTime OR evokeTimeOr
   ;

evokeTimeExprOr
   : evokeTimeExpr
   | evokeTimeExpr OR evokeTimeExprOr
   ;

evokeTimeExpr
   : evokeDuration
   | evokeTime
   ;

evokeTime
   : ISO_DATE_TIME
   | ISO_DATE
   | relativeEvokeTimeExpr
   ;

evokeDuration
   : NUMBER durationOp
   ;

relativeEvokeTimeExpr
   : TODAY ATTIME TIMEOFDAY
   | TOMORROW ATTIME TIMEOFDAY
   | WEEKDAYLITERAL ATTIME TIMEOFDAY
   ;

qualifiedEvokeCycle
   : simpleEvokeCycle
   | simpleEvokeCycle UNTIL expr
   ;

simpleEvokeCycle
   : EVERY evokeDuration FOR evokeDuration STARTING startingDelay
   ;

startingDelay
   : eventTime
   | delayedEvoke
   ;

actionBlock
   : actionStatement
   | actionBlock SC actionStatement
   ;

actionStatement
   : (IF actionIfThenElse)?
   | FOR IDENTIFIER IN expr DO actionBlock SC ENDDO
   | WHILE expr DO actionBlock SC ENDDO
   | actionSwitch
   | BREAKLOOP
   | callPhrase
   | callPhrase DELAY expr
   | WRITE expr
   | WRITE expr AT IDENTIFIER
   | RETURN expr
   | identifierBecomes expr
   | timeBecomes expr
   | applicabilityBecomes expr
   | identifierBecomes newObjectPhrase
   ;

actionIfThenElse
   : expr THEN actionBlock SC actionElseIf
   ;

actionElseIf
   : ENDIF AGGREGATE?
   | ELSE actionBlock SC ENDIF AGGREGATE?
   | ELSEIF actionIfThenElse
   ;

actionSwitch
   : SWITCH IDENTIFIER COLON actionSwitchCases ENDSWITCH AGGREGATE?
   ;

actionSwitchCases
   : (CASE exprFactor actionBlock actionSwitchCases)?
   | DEFAULT actionBlock
   ;

