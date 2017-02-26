/*
BSD License
Copyright (c) 2013, Tom Everett
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
/*
Adapted from m2pim4_LL1.g by Benjamin Kowarsch
*/

grammar m2pim4;

ident
   : IDENT
   ;

number
   : INTEGER
   | REAL
   ;

integer
   : INTEGER
   ;

real
   : REAL
   ;

scaleFactor
   : SCALE_FACTOR
   ;

hexDigit
   : HEX_DIGIT
   ;

digit
   : DIGIT
   ;

octalDigit
   : OCTAL_DIGIT
   ;

string
   : STRING
   ;

qualident
   : ident ('.' ident)*
   ;

constantDeclaration
   : ident '=' constExpression
   ;

constExpression
   : simpleConstExpr (relation simpleConstExpr)?
   ;

relation
   : '='
   | '#'
   | '<>'
   | '<'
   | '<='
   | '>'
   | '>='
   | 'IN' {       }
   ;

simpleConstExpr
   : ('+' | '-' {       })? constTerm (addOperator constTerm)*
   ;

addOperator
   : '+'
   | '-'
   | OR
   ;

constTerm
   : constFactor (mulOperator constFactor)*
   ;

mulOperator
   : '*'
   | '/'
   | DIV
   | MOD
   | AND
   | '&'
   ;

constFactor
   : number
   | string
   | setOrQualident
   | '(' constExpression ')'
   | (NOT | '~' {       }) constFactor
   ;

setOrQualident
   : set
   | qualident set?
   ;

set
   : '{' (element (',' element)*)? '}'
   ;

element
   : constExpression ('..' constExpression)?
   ;

typeDeclaration
   : ident '=' type
   ;

type
   : simpleType
   | arrayType
   | recordType
   | setType
   | pointerType
   | procedureType
   ;

simpleType
   : qualident
   | enumeration
   | subrangeType
   ;

enumeration
   : '(' identList ')'
   ;

identList
   : ident (',' ident)*
   ;

subrangeType
   : '[' constExpression '..' constExpression ']'
   ;

arrayType
   : ARRAY simpleType (',' simpleType)* OF type
   ;

recordType
   : RECORD fieldListSequence END
   ;

fieldListSequence
   : fieldList (';' fieldList)*
   ;

fieldList
   : (identList ':' type | CASE ident ((':' | '.' {       }) qualident)? OF variant ('|' variant)* (ELSE fieldListSequence)? END)?
   ;

variant
   : caseLabelList ':' fieldListSequence
   ;

caseLabelList
   : caseLabels (',' caseLabels)*
   ;

caseLabels
   : constExpression ('..' constExpression)?
   ;

setType
   : SET OF simpleType
   ;

pointerType
   : POINTER TO type
   ;

procedureType
   : PROCEDURE formalTypeList?
   ;

formalTypeList
   : '(' (VAR? formalType (',' VAR? formalType)*)? ')' (':' qualident)?
   ;

variableDeclaration
   : identList ':' type
   ;

designator
   : qualident (designatorTail)?
   ;

designatorTail
   : (('[' expList ']' | '^') ('.' ident)*) +
   ;

expList
   : expression (',' expression)*
   ;

expression
   : simpleExpression (relation simpleExpression)?
   ;

simpleExpression
   : ('+' | '-' {       })? term (addOperator term)*
   ;

term
   : factor (mulOperator factor)*
   ;

factor
   : number
   | string
   | setOrDesignatorOrProcCall
   | '(' expression ')'
   | (NOT | '~' {       }) factor
   ;

setOrDesignatorOrProcCall
   : set
   | qualident (set | designatorTail? actualParameters?)
   ;

actualParameters
   : '(' expList? ')'
   ;

statement
   : (assignmentOrProcCall | ifStatement | caseStatement | whileStatement | repeatStatement | loopStatement | forStatement | withStatement | EXIT | RETURN expression?)?
   ;

assignmentOrProcCall
   : designator (':=' expression | actualParameters?)
   ;

statementSequence
   : statement (';' statement)*
   ;

ifStatement
   : IF expression THEN statementSequence (ELSIF expression THEN statementSequence)* (ELSE statementSequence)? END
   ;

caseStatement
   : CASE expression OF ccase ('|' ccase)* (ELSE statementSequence)? END
   ;

ccase
   : caseLabelList ':' statementSequence
   ;

whileStatement
   : WHILE expression DO statementSequence END
   ;

repeatStatement
   : REPEAT statementSequence UNTIL expression
   ;

forStatement
   : FOR ident ':=' expression TO expression (BY constExpression)? DO statementSequence END
   ;

loopStatement
   : LOOP statementSequence END
   ;

withStatement
   : WITH designator DO statementSequence END
   ;

procedureDeclaration
   : procedureHeading ';' block ident
   ;

procedureHeading
   : PROCEDURE ident formalParameters?
   ;

block
   : declaration* (BEGIN statementSequence)? END
   ;

declaration
   : CONST (constantDeclaration ';')*
   | TYPE (typeDeclaration ';')*
   | VAR (variableDeclaration ';')*
   | procedureDeclaration ';'
   | moduleDeclaration ';'
   ;

formalParameters
   : '(' (fpSection (';' fpSection)*)? ')' (':' qualident)?
   ;

fpSection
   : VAR? identList ':' formalType
   ;

formalType
   : (ARRAY OF)? qualident
   ;

moduleDeclaration
   : MODULE ident priority? ';' importList* exportList? block ident
   ;

priority
   : '[' constExpression ']'
   ;

exportList
   : EXPORT QUALIFIED? identList ';'
   ;

importList
   : (FROM ident)? IMPORT identList ';'
   ;

definitionModule
   : DEFINITION MODULE ident ';' importList* exportList? definition* END ident '.'
   ;

definition
   : CONST (constantDeclaration ';')*
   | TYPE (ident ('=' type)? ';')*
   | VAR (variableDeclaration ';')*
   | procedureHeading ';'
   ;

programModule
   : MODULE ident priority? ';' importList* block ident '.'
   ;

compilationUnit
   : definitionModule
   | IMPLEMENTATION? programModule
   ;


AND
   : 'AND'
   ;


ARRAY
   : 'ARRAY'
   ;


BEGIN
   : 'BEGIN'
   ;


BY
   : 'BY'
   ;


CASE
   : 'CASE'
   ;


CONST
   : 'CONST'
   ;


DEFINITION
   : 'DEFINITION'
   ;


DIV
   : 'DIV'
   ;


DO
   : 'DO'
   ;


ELSE
   : 'ELSE'
   ;


ELSIF
   : 'ELSIF'
   ;


END
   : 'END'
   ;


EXIT
   : 'EXIT'
   ;


EXPORT
   : 'EXPORT'
   ;


FOR
   : 'FOR'
   ;


FROM
   : 'FROM'
   ;


IF
   : 'IF'
   ;


IMPLEMENTATION
   : 'IMPLEMENTATION'
   ;


IMPORT
   : 'IMPORT'
   ;


IN
   : 'IN'
   ;


LOOP
   : 'LOOP'
   ;


MOD
   : 'MOD'
   ;


MODULE
   : 'MODULE'
   ;


NOT
   : 'NOT'
   ;


OF
   : 'OF'
   ;


OR
   : 'OR'
   ;


POINTER
   : 'POINTER'
   ;


PROCEDURE
   : 'PROCEDURE'
   ;


QUALIFIED
   : 'QUALIFIED'
   ;


RECORD
   : 'RECORD'
   ;


REPEAT
   : 'REPEAT'
   ;


RETURN
   : 'RETURN'
   ;


SET
   : 'SET'
   ;


THEN
   : 'THEN'
   ;


TO
   : 'TO'
   ;


TYPE
   : 'TYPE'
   ;


UNTIL
   : 'UNTIL'
   ;


VAR
   : 'VAR'
   ;


WHILE
   : 'WHILE'
   ;


WITH
   : 'WITH'
   ;


IDENT
   : LETTER (LETTER | DIGIT)*
   ;


INTEGER
   : DIGIT + | OCTAL_DIGIT + ('B' | 'C') | DIGIT (HEX_DIGIT)* 'H'
   ;


REAL
   : DIGIT + '.' DIGIT* SCALE_FACTOR?
   ;


STRING
   : '\'' (CHARACTER | '"')* '\'' | '"' (CHARACTER | '\'')* '"'
   ;


fragment LETTER
   : 'A' .. 'Z' | 'a' .. 'z'
   ;


DIGIT
   : OCTAL_DIGIT | '8' | '9'
   ;


OCTAL_DIGIT
   : '0' .. '7'
   ;


HEX_DIGIT
   : DIGIT | 'A' | 'B' | 'C' | 'D' | 'E' | 'F'
   ;


SCALE_FACTOR
   : 'E' ('+' | '-')? DIGIT +
   ;


fragment CHARACTER
   : DIGIT | LETTER | ' ' | '!' | '#' | '$' | '%' | '&' | '(' | ')' | '*' | '+' | ',' | '-' | '.' | ':' | ';' | '<' | '=' | '>' | '?' | '@' | '[' | '\\' | ']' | '^' | '_' | '`' | '{' | '|' | '}' | '~'
   ;


COMMENT
   : '(*' .*? '*)' -> skip
   ;


WS
   : [ \t\r\n] -> skip
   ;
