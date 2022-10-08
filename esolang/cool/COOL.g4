/*
The MIT License (MIT)

Copyright (c) 2017 Linonetwo

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.

COOL grammar derived from:

http://sist.shanghaitech.edu.cn/faculty/songfu/course/spring2017/cs131/COOL/COOLAid.pdf

*/
grammar COOL;

program
   : programBlocks EOF
   ;

programBlocks
   : classDefine ';' programBlocks # classes
   | EOF # eof
   ;

classDefine
   : CLASS TYPEID (INHERITS TYPEID)? '{' (feature ';')* '}'
   ;

feature
   : OBJECTID '(' (formal (',' formal)*)? ')' ':' TYPEID '{' expression '}' # method
   | OBJECTID ':' TYPEID (ASSIGNMENT expression)? # property
   ;

formal
   : OBJECTID ':' TYPEID
   ;
/* method argument */
   
   
expression
   : expression ('@' TYPEID)? '.' OBJECTID '(' (expression (',' expression)*)? ')' # methodCall
   | OBJECTID '(' (expression (',' expression)*)? ')' # ownMethodCall
   | IF expression THEN expression ELSE expression FI # if
   | WHILE expression LOOP expression POOL # while
   | '{' (expression ';')+ '}' # block
   | LET OBJECTID ':' TYPEID (ASSIGNMENT expression)? (',' OBJECTID ':' TYPEID (ASSIGNMENT expression)?)* IN expression # letIn
   | CASE expression OF (OBJECTID ':' TYPEID CASE_ARROW expression ';')+ ESAC # case
   | NEW TYPEID # new
   | INTEGER_NEGATIVE expression # negative
   | ISVOID expression # isvoid
   | expression MULTIPLY expression # multiply
   | expression DIVISION expression # division
   | expression ADD expression # add
   | expression MINUS expression # minus
   | expression LESS_THAN expression # lessThan
   | expression LESS_EQUAL expression # lessEqual
   | expression EQUAL expression # equal
   | NOT expression # boolNot
   | '(' expression ')' # parentheses
   | OBJECTID # id
   | INT # int
   | STRING # string
   | TRUE # true
   | FALSE # false
   | OBJECTID ASSIGNMENT expression # assignment
   ;
   // key words
   
CLASS
   : C L A S S
   ;

ELSE
   : E L S E
   ;

FALSE
   : 'f' A L S E
   ;

FI
   : F I
   ;

IF
   : I F
   ;

IN
   : I N
   ;

INHERITS
   : I N H E R I T S
   ;

ISVOID
   : I S V O I D
   ;

LET
   : L E T
   ;

LOOP
   : L O O P
   ;

POOL
   : P O O L
   ;

THEN
   : T H E N
   ;

WHILE
   : W H I L E
   ;

CASE
   : C A S E
   ;

ESAC
   : E S A C
   ;

NEW
   : N E W
   ;

OF
   : O F
   ;

NOT
   : N O T
   ;

TRUE
   : 't' R U E
   ;
   // primitives
   
STRING
   : '"' (ESC | ~ ["\\])* '"'
   ;

INT
   : [0-9]+
   ;

TYPEID
   : [A-Z] [_0-9A-Za-z]*
   ;

OBJECTID
   : [a-z] [_0-9A-Za-z]*
   ;

ASSIGNMENT
   : '<-'
   ;

CASE_ARROW
   : '=>'
   ;

ADD
   : '+'
   ;

MINUS
   : '-'
   ;

MULTIPLY
   : '*'
   ;

DIVISION
   : '/'
   ;

LESS_THAN
   : '<'
   ;

LESS_EQUAL
   : '<='
   ;

EQUAL
   : '='
   ;

INTEGER_NEGATIVE
   : '~'
   ;

fragment A
   : [aA]
   ;

fragment C
   : [cC]
   ;

fragment D
   : [dD]
   ;

fragment E
   : [eE]
   ;

fragment F
   : [fF]
   ;

fragment H
   : [hH]
   ;

fragment I
   : [iI]
   ;

fragment L
   : [lL]
   ;

fragment N
   : [nN]
   ;

fragment O
   : [oO]
   ;

fragment P
   : [pP]
   ;

fragment R
   : [rR]
   ;

fragment S
   : [sS]
   ;

fragment T
   : [tT]
   ;

fragment U
   : [uU]
   ;

fragment V
   : [vV]
   ;

fragment W
   : [wW]
   ;

fragment ESC
   : '\\' (["\\/bfnrt] | UNICODE)
   ;

fragment UNICODE
   : 'u' HEX HEX HEX HEX
   ;

fragment HEX
   : [0-9a-fA-F]
   ;
   // comments
   
OPEN_COMMENT
   : '(*'
   ;

CLOSE_COMMENT
   : '*)'
   ;

COMMENT
   : OPEN_COMMENT (COMMENT | .)*? CLOSE_COMMENT -> skip
   ;

ONE_LINE_COMMENT
   : '--' (~ '\n')* '\n'? -> skip
   ;
   // skip spaces, tabs, newlines, note that \v is not suppoted in antlr
   
WHITESPACE
   : [ \t\r\n\f]+ -> skip
   ;

