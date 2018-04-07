
grammar krl;

/*
    This file is the grammar for the KUKA Robot Language.
    Copyright (C) 2010-2011  Jan Schlößin
    
    This grammar is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/
/*
Antlr4 port by Tom Everett, 2016
*/
module
   : (moduleData | moduleRoutines) EOF
   ;

moduleRoutines
   : mainRoutine (subRoutine | NEWLINE)*
   ;

mainRoutine
   : procedureDefinition
   | functionDefinition
   ;

subRoutine
   : procedureDefinition
   | functionDefinition
   ;

procedureDefinition
   : GLOBAL? DEF procedureName formalParameters NEWLINE routineBody END
   ;

procedureName
   : IDENTIFIER
   ;

functionDefinition
   : GLOBAL? DEFFCT type functionName formalParameters NEWLINE routineBody ENDFCT
   ;

functionName
   : IDENTIFIER
   ;

moduleData
   : DEFDAT moduleName PUBLIC? NEWLINE dataList ENDDAT NEWLINE*
   ;

moduleName
   : IDENTIFIER
   ;

dataList
   : (NEWLINE | forwardDeclaration NEWLINE | typeDeclaration NEWLINE | variableDeclarationInDataList NEWLINE | arrayInitialisation NEWLINE | importStatement NEWLINE)*
   ;

arrayInitialisation
   : IDENTIFIER arrayVariableSuffix '=' unaryPlusMinuxExpression
   ;

typeDeclaration
   : structureDefinition
   | enumDefinition
   ;

structureDefinition
   : GLOBAL? STRUC typeName type variableName variableListRest (',' type variableName variableListRest)*
   ;

enumDefinition
   : GLOBAL? ENUM typeName enumValue (',' enumValue)*
   ;

enumValue
   : IDENTIFIER
   ;

variableDeclaration
   : DECL? (type variableName variableListRest | signalDeclaration)
   ;

signalDeclaration
   : SIGNAL IDENTIFIER primary (TO primary)?
   ;

variableDeclarationInDataList
   : DECL? GLOBAL? CONST? (type variableName (variableListRest | variableInitialisation) | signalDeclaration)
   ;

variableListRest
   : (',' variableName)*
   ;

variableInitialisation
   : '=' unaryPlusMinuxExpression
   ;

structLiteral
   : '{' (typeName ':')? structElementList '}'
   ;

structElementList
   : structElement (',' structElement)*
   ;

structElement
   : variableName unaryPlusMinuxExpression
   ;

formalParameters
   : '(' (parameter (',' parameter)*)? ')'
   ;

parameter
   : variableName (parameterCallType)?
   ;

routineBody
   : routineDataSection routineImplementationSection
   ;

routineDataSection
   : (forwardDeclaration NEWLINE | variableDeclaration NEWLINE | (NEWLINE) NEWLINE | importStatement NEWLINE)*
   ;

forwardDeclaration
   : EXT procedureName formalParametersWithType
   | EXTFCT type functionName formalParametersWithType
   ;

formalParametersWithType
   : '(' (parameterWithType (',' parameterWithType)*)? ')'
   ;

parameterWithType
   : type (parameterCallType)?
   ;

parameterCallType
   : ':' IDENTIFIER
   ;

importStatement
   : IMPORT type variableName IS '/R1/' moduleName '..' variableName
   ;

variableName
   : IDENTIFIER (arrayVariableSuffix)?
   ;

// expression in arrays are optional: a string literal can be assigned to a char array as a whole
arrayVariableSuffix
   : '[' (expression (',' (expression (',' expression?)?)?)?)? ']'
   ;

routineImplementationSection
   : statementList
   ;

statementList
   : statement*
   ;

statement
   : CONTINUE NEWLINE
   | EXIT NEWLINE
   | FOR IDENTIFIER '=' expression TO expression (IDENTIFIER expression)? NEWLINE statementList ENDFOR
   | GOTO IDENTIFIER NEWLINE
   | HALT NEWLINE
   | IF expression THEN NEWLINE statementList (ELSE NEWLINE statementList)? ENDIF NEWLINE
   | LOOP NEWLINE statementList ENDLOOP NEWLINE
   | REPEAT NEWLINE statementList UNTIL expression NEWLINE
   | SWITCH expression NEWLINE switchBlockStatementGroups ENDSWITCH NEWLINE
   | WAIT FOR expression NEWLINE
   | WAIT SEC expression NEWLINE
   | WHILE expression NEWLINE statementList ENDWHILE NEWLINE
   | RETURN (assignmentExpression)? NEWLINE
   | BRAKE (IDENTIFIER)? NEWLINE
   | assignmentExpression NEWLINE
   | IDENTIFIER ':' NEWLINE
   | NEWLINE
   | GLOBAL? INTERRUPT DECL primary WHEN expression DO assignmentExpression NEWLINE
   | INTERRUPT IDENTIFIER primary? NEWLINE
   | (PTP | PTP_REL) geometricExpression (C_PTP (C_DIS | C_ORI | C_VEL)?)? NEWLINE
   | LIN geometricExpression (C_DIS | C_ORI | C_VEL)? NEWLINE
   | LIN_REL geometricExpression (C_DIS | C_ORI | C_VEL)? enumElement? NEWLINE
   | (CIRC | CIRC_REL) geometricExpression ',' geometricExpression (',' IDENTIFIER primary)? (C_DIS | C_ORI | C_VEL)? NEWLINE
   | TRIGGER WHEN (IDENTIFIER) '=' expression DELAY '=' expression DO assignmentExpression (PRIO '=' expression)? NEWLINE
   | analogInputStatement NEWLINE
   | analogOutputStatement NEWLINE
   ;

analogOutputStatement
   : ANOUT (IDENTIFIER assignmentExpression (IDENTIFIER '=' literal)* | IDENTIFIER IDENTIFIER)
   ;

analogInputStatement
   : ANIN (IDENTIFIER assignmentExpression | IDENTIFIER IDENTIFIER)
   ;

switchBlockStatementGroups
   : NEWLINE* (caseLabel statementList) + (defaultLabel statementList)?
   ;

caseLabel
   : CASE expression (',' expression)* NEWLINE
   ;

defaultLabel
   : DEFAULT NEWLINE
   ;

expressionList
   : assignmentExpression (',' assignmentExpression)*
   ;

assignmentExpression
   : expression ('=' expression)*
   ;

expression
   : conditionalOrExpression (relationalOp conditionalOrExpression)*
   ;

relationalOp
   : '=='
   | '<>'
   | '<='
   | '>='
   | '<'
   | '>'
   ;

conditionalOrExpression
   : exclusiveOrExpression ((OR | B_OR) exclusiveOrExpression)*
   ;

exclusiveOrExpression
   : conditionalAndExpression ((EXOR | B_EXOR) conditionalAndExpression)*
   ;

conditionalAndExpression
   : additiveExpression ((AND | B_AND) additiveExpression)*
   ;

additiveExpression
   : multiplicativeExpression (('+' | '-') multiplicativeExpression)*
   ;

multiplicativeExpression
   : geometricExpression (('*' | '/') geometricExpression)*
   ;

geometricExpression
   : unaryNotExpression (':' unaryNotExpression)*
   ;

unaryNotExpression
   : NOT unaryNotExpression
   | B_NOT unaryNotExpression
   | unaryPlusMinuxExpression
   ;

unaryPlusMinuxExpression
   : '+' unaryPlusMinuxExpression
   | '-' unaryPlusMinuxExpression
   | primary
   ;

primary
   : parExpression
   | variableName ('.' variableName)* (arguments)?
   | literal
   ;

parExpression
   : '(' assignmentExpression ')'
   ;

type
   : primitiveType ('[' (INTLITERAL)? ']')?
   | typeName ('[' (INTLITERAL)? ']')?
   ;

typeName
   : IDENTIFIER
   ;

primitiveType
   : BOOL
   | CHAR
   | INT
   | REAL
   ;

arguments
   : '(' (expressionList)? ')'
   ;

literal
   : INTLITERAL
   | FLOATLITERAL
   | CHARLITERAL
   | STRINGLITERAL
   | structLiteral
   | TRUE
   | FALSE
   | enumElement
   ;

enumElement
   : '#' IDENTIFIER
   ;


AND
   : A N D
   ;


ANIN
   : A N I N
   ;


ANOUT
   : A N O U T
   ;


B_AND
   : B '_' A N D
   ;


B_NOT
   : B '_' N O T
   ;


B_OR
   : B '_' O R
   ;


B_EXOR
   : B '_' E X O R
   ;


BOOL
   : B O O L
   ;


BRAKE
   : B R A K E
   ;


C_DIS
   : C '_' D I S
   ;


C_ORI
   : C '_' O R I
   ;


C_PTP
   : C '_' P T P
   ;


C_VEL
   : C '_' V E L
   ;


CASE
   : C A S E
   ;


CAST_FROM
   : C A S T '_' F R O M
   ;


CAST_TO
   : C A S T '_' T O
   ;


CHAR
   : C H A R
   ;


CIRC_REL
   : C I R C '_' R E L
   ;


CIRC
   : C I R C
   ;


CONST
   : C O N S T
   ;


CONTINUE
   : C O N T I N U E
   ;


DELAY
   : D E L A Y
   ;


DECL
   : D E C L
   ;


DEF
   : D E F
   ;


DEFAULT
   : D E F A U L T
   ;


DEFDAT
   : D E F D A T
   ;


DEFFCT
   : D E F F C T
   ;


DO
   : D O
   ;


ELSE
   : E L S E
   ;


END
   : E N D
   ;


ENDDAT
   : E N D D A T
   ;


ENDFCT
   : E N D F C T
   ;


ENDFOR
   : E N D F O R
   ;


ENDIF
   : E N D I F
   ;


ENDLOOP
   : E N D L O O P
   ;


ENDSWITCH
   : E N D S W I T C H
   ;


ENDWHILE
   : E N D W H I L E
   ;


ENUM
   : E N U M
   ;


EXIT
   : E X I T
   ;


EXT
   : E X T
   ;


EXTFCT
   : E X T F C T
   ;


FALSE
   : F A L S E
   ;


FOR
   : F O R
   ;


GLOBAL
   : G L O B A L
   ;


GOTO
   : G O T O
   ;


HALT
   : H A L T
   ;


IF
   : I F
   ;


IMPORT
   : I M P O R T
   ;


INTERRUPT
   : I N T E R R U P T
   ;


INT
   : I N T
   ;


IS
   : I S
   ;


LIN_REL
   : L I N '_' R E L
   ;


LIN
   : L I N
   ;


LOOP
   : L O O P
   ;


MAXIMUM
   : M A X I M U M
   ;


MINIMUM
   : M I N I M U M
   ;


NOT
   : N O T
   ;


OR
   : O R
   ;


PRIO
   : P R I O
   ;


PTP_REL
   : P T P '_' R E L
   ;


PTP
   : P T P
   ;


PUBLIC
   : P U B L I C
   ;


REAL
   : R E A L
   ;


REPEAT
   : R E P E A T
   ;


RETURN
   : R E T U R N
   ;


SEC
   : S E C
   ;


SIGNAL
   : S I G N A L
   ;


STRUC
   : S T R U C
   ;


SWITCH
   : S W I T C H
   ;


THEN
   : T H E N
   ;


TO
   : T O
   ;


TRIGGER
   : T R I G G E R
   ;


TRUE
   : T R U E
   ;


UNTIL
   : U N T I L
   ;


WAIT
   : W A I T
   ;


WHEN
   : W H E N
   ;


WHILE
   : W H I L E
   ;


EXOR
   : E X O R
   ;


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


HEADERLINE
   : '&' ~ ('\n' | '\r')* ('\r\n' | '\r' | '\n' | EOF) -> skip
   ;


WS
   : (' ' | '\t' | '\u000C') -> skip
   ;


NEWLINE
   : '\r'? '\n'
   ;


LINE_COMMENT
   : ';' ~ ('\n' | '\r')* -> skip
   ;


CHARLITERAL
   : '\'' (EscapeSequence | ~ ('\'' | '\\' | '\r' | '\n')) '\''
   ;


STRINGLITERAL
   : '"' (EscapeSequence | ~ ('\\' | '"' | '\r' | '\n'))* '"'
   ;


fragment EscapeSequence
   : '\\' ('b' | 't' | 'n' | 'f' | 'r' | '"' | '\'' | '\\' | ('0' .. '3') ('0' .. '7') ('0' .. '7') | ('0' .. '7') ('0' .. '7') | ('0' .. '7'))
   ;


FLOATLITERAL
   : ('0' .. '9') + '.' ('0' .. '9')* Exponent? | '.' ('0' .. '9') + Exponent? | ('0' .. '9') + Exponent
   ;


fragment Exponent
   : E ('+' | '-')? ('0' .. '9') +
   ;


INTLITERAL
   : ('0' .. '9') + | HexPrefix HexDigit + HexSuffix | BinPrefix BinDigit + BinSuffix
   ;


fragment HexPrefix
   : '\'' H
   ;


fragment HexDigit
   : ('0' .. '9' | 'a' .. 'f' | 'A' .. 'F')
   ;


fragment HexSuffix
   : '\''
   ;


fragment BinPrefix
   : '\'' B
   ;


fragment BinDigit
   : ('0' | '1')
   ;


fragment BinSuffix
   : '\''
   ;


IDENTIFIER
   : IdentifierStart IdentifierPart*
   ;


fragment IdentifierStart
   : 'a' .. 'z' | 'A' .. 'Z' | '_' | '$'
   ;


fragment IdentifierPart
   : IdentifierStart | '0' .. '9'
   ;
