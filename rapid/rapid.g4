/*
BSD License

Copyright (c) 2026, Tom Everett
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
* Grammar for the RAPID language (ABB industrial robot programming language).
*/
grammar rapid;

rapid
   : module+ EOF
   ;

module
   : MODULE IDENT moduleAttributeList? moduleMember* ENDMODULE
   ;

moduleAttributeList
   : '(' IDENT (',' IDENT)* ')'
   ;

moduleMember
   : dataDeclaration
   | routineDeclaration
   ;

dataDeclaration
   : LOCAL? (VAR | PERS | CONST) dataType IDENT dim? (':=' expression)? ';'
   ;

dim
   : '{' expression (',' expression)* '}'
   ;

dataType
   : IDENT
   ;

routineDeclaration
   : procDeclaration
   | funcDeclaration
   | trapDeclaration
   ;

procDeclaration
   : LOCAL? PROC IDENT '(' paramList? ')' dataDeclaration* statement* ENDPROC
   ;

funcDeclaration
   : LOCAL? FUNC dataType IDENT '(' paramList? ')' dataDeclaration* statement* ENDFUNC
   ;

trapDeclaration
   : LOCAL? TRAP IDENT dataDeclaration* statement* ENDTRAP
   ;

paramList
   : param (',' param)*
   ;

param
   : paramAlternative ('|' paramAlternative)*
   ;

paramAlternative
   : '\\'? (INOUT | VAR | PERS)? dataType IDENT arrayMarker?
   ;

arrayMarker
   : '{' '*' (',' '*')* '}'
   ;

statement
   : assignmentStatement
   | procCallStatement
   | ifStatement
   | forStatement
   | whileStatement
   | testStatement
   | returnStatement
   | gotoStatement
   | labelStatement
   ;

assignmentStatement
   : expression ':=' expression ';'
   ;

procCallStatement
   : IDENT (argumentElement (',' argumentElement)*)? ';'
   ;

argumentElement
   : expression switchSuffix*
   | switchSuffix+
   ;

switchSuffix
   : '\\' IDENT (':=' expression)?
   ;

ifStatement
   : IF expression THEN statement* (ELSEIF expression THEN statement*)* (ELSE statement*)? ENDIF
   ;

forStatement
   : FOR IDENT FROM expression TO expression (STEP expression)? DO statement* ENDFOR
   ;

whileStatement
   : WHILE expression DO statement* ENDWHILE
   ;

testStatement
   : TEST expression (CASE expression (',' expression)* ':' statement*)* (DEFAULT ':' statement*)? ENDTEST
   ;

returnStatement
   : RETURN expression? ';'
   ;

gotoStatement
   : GOTO IDENT ';'
   ;

labelStatement
   : IDENT ':'
   ;

expression
   : orExpression
   ;

orExpression
   : andExpression ((OR | XOR) andExpression)*
   ;

andExpression
   : notExpression (AND notExpression)*
   ;

notExpression
   : NOT? relationalExpression
   ;

relationalExpression
   : additiveExpression ((EQ | NEQ | LT | GT | LE | GE) additiveExpression)?
   ;

additiveExpression
   : multiplicativeExpression (('+' | '-') multiplicativeExpression)*
   ;

multiplicativeExpression
   : unaryExpression (('*' | '/' | DIV | MOD) unaryExpression)*
   ;

unaryExpression
   : '-' unaryExpression
   | primaryExpression
   ;

primaryExpression
   : literal
   | functionCall
   | variableAccess
   | '(' expression ')'
   ;

functionCall
   : IDENT '(' (argumentElement (',' argumentElement)*)? ')'
   ;

variableAccess
   : IDENT component*
   ;

component
   : '{' expression (',' expression)* '}'
   | '.' IDENT
   ;

literal
   : NUMBER
   | STRING
   | TRUE
   | FALSE
   ;

MODULE
   : 'MODULE'
   ;

ENDMODULE
   : 'ENDMODULE'
   ;

LOCAL
   : 'LOCAL'
   ;

VAR
   : 'VAR'
   ;

PERS
   : 'PERS'
   ;

CONST
   : 'CONST'
   ;

INOUT
   : 'INOUT'
   ;

PROC
   : 'PROC'
   ;

ENDPROC
   : 'ENDPROC'
   ;

FUNC
   : 'FUNC'
   ;

ENDFUNC
   : 'ENDFUNC'
   ;

TRAP
   : 'TRAP'
   ;

ENDTRAP
   : 'ENDTRAP'
   ;

IF
   : 'IF'
   ;

THEN
   : 'THEN'
   ;

ELSEIF
   : 'ELSEIF'
   ;

ELSE
   : 'ELSE'
   ;

ENDIF
   : 'ENDIF'
   ;

FOR
   : 'FOR'
   ;

FROM
   : 'FROM'
   ;

TO
   : 'TO'
   ;

STEP
   : 'STEP'
   ;

DO
   : 'DO'
   ;

ENDFOR
   : 'ENDFOR'
   ;

WHILE
   : 'WHILE'
   ;

ENDWHILE
   : 'ENDWHILE'
   ;

TEST
   : 'TEST'
   ;

CASE
   : 'CASE'
   ;

DEFAULT
   : 'DEFAULT'
   ;

ENDTEST
   : 'ENDTEST'
   ;

RETURN
   : 'RETURN'
   ;

GOTO
   : 'GOTO'
   ;

AND
   : 'AND'
   ;

OR
   : 'OR'
   ;

XOR
   : 'XOR'
   ;

NOT
   : 'NOT'
   ;

DIV
   : 'DIV'
   ;

MOD
   : 'MOD'
   ;

TRUE
   : 'TRUE'
   ;

FALSE
   : 'FALSE'
   ;

EQ
   : '='
   ;

NEQ
   : '<>'
   ;

LE
   : '<='
   ;

GE
   : '>='
   ;

LT
   : '<'
   ;

GT
   : '>'
   ;

NUMBER
   : [0-9]+ ('.' [0-9]+)? ([eE] [+-]? [0-9]+)?
   ;

STRING
   : '"' (~["\r\n])* '"'
   ;

IDENT
   : [a-zA-Z_] [a-zA-Z_0-9]*
   ;

COMMENT
   : '!' ~[\r\n]* -> skip
   ;

WS
   : [ \t\r\n]+ -> skip
   ;
