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

program: programBlocks;
programBlocks
  : classDefine ';' programBlocks #class
  | EOF #eof
  ;
classDefine: CLASS TYPEID (INHERITS TYPEID)? '{' (feature ';')* '}';
feature
  : OBJECTID '(' (formal (',' formal)*)* ')' ':' TYPEID '{' expression '}' #method
  | OBJECTID ':' TYPEID (ASSIGNMENT expression)? /* class member variable */ #classProperty
  ;
formal: OBJECTID ':' TYPEID; /* method argument */
expression
  : OBJECTID ASSIGNMENT expression #assignment
  | expression ('@' TYPEID)? '.' OBJECTID '(' (expression (',' expression)*)* ')' /* call super class method */ #superClassMethod
  | OBJECTID '(' (expression (',' expression)*)* ')' /* call function that refered by variable */ #functionCall
  | IF expression THEN expression ELSE expression FI #if
  | WHILE expression LOOP expression POOL #whild
  | '{' (expression ';')+ '}' #multipleExpression
  | LET OBJECTID ':' TYPEID (ASSIGNMENT expression)? (',' OBJECTID ':' TYPEID (ASSIGNMENT expression)?)* IN expression /* let num : Int <- num_cells() in */ #letIn
  | CASE expression OF (OBJECTID ':' TYPEID CASE_ARROW expression ';')+ ESAC #case
  | NEW TYPEID #newType
  | ISVOID expression #isvoid
  | expression ADD expression #add
  | expression MINUS expression #minus
  | expression MULTIPLY expression #multiply
  | expression DIVISION expression #division
  | INTEGER_COMPLEMENT expression #integerComplement
  | expression LESS_THAN expression #lessThan
  | expression LESS_EQUAL expression #lessEqual
  | expression EQUAL expression #equal
  | NOT expression #boolNot
  | '(' expression ')' #parentheses
  | OBJECTID #id
  | INT #int
  | STRING #string
  | TRUE #true
  | FALSE #false
  ;


// skip spaces, tabs, newlines, note that \v is not suppoted in antlr
WHITESPACE: [ \t\r\n\f]+ -> skip; 

// comments
OPEN_COMMENT: '(*';
CLOSE_COMMENT: '*)';
COMMENT: OPEN_COMMENT (COMMENT|.)*? CLOSE_COMMENT -> channel(HIDDEN);
ONE_LINE_COMMENT: '--' .*? '\n' -> channel(HIDDEN);

// key words
CLASS: ('C'|'c')('L'|'l')('A'|'a')('S'|'s')('S'|'s');
ELSE: ('E'|'e')('L'|'l')('S'|'s')('E'|'e');
FALSE: 'f'('A'|'a')('L'|'l')('S'|'s')('E'|'e');
FI: ('F'|'f')('I'|'i');
IF: ('I'|'i')('F'|'f');
IN: ('I'|'i')('N'|'n');
INHERITS: ('I'|'i')('N'|'n')('H'|'h')('E'|'e')('R'|'r')('I'|'i')('T'|'t')('S'|'s');
ISVOID: ('I'|'i')('S'|'s')('V'|'v')('O'|'o')('I'|'i')('D'|'d');
LET: ('L'|'l')('E'|'e')('T'|'t');
LOOP: ('L'|'l')('O'|'o')('O'|'o')('P'|'p');
POOL: ('P'|'p')('O'|'o')('O'|'o')('L'|'l');
THEN: ('T'|'t')('H'|'h')('E'|'e')('N'|'n');
WHILE: ('W'|'w')('H'|'h')('I'|'i')('L'|'l')('E'|'e');
CASE: ('C'|'c')('A'|'a')('S'|'s')('E'|'e');
ESAC: ('E'|'e')('S'|'s')('A'|'a')('C'|'c');
NEW: ('N'|'n')('E'|'e')('W'|'w');
OF: ('O'|'o')('F'|'f');
NOT: ('N'|'n')('O'|'o')('T'|'t');
TRUE: 't'('R'|'r')('U'|'u')('E'|'e');

// premitives
STRING: '"' (ESC | ~ ["\\])* '"'; // non-greedy matching one line string
INT: [0-9]+;
TYPEID: [A-Z][_0-9A-Za-z]*;
OBJECTID: [a-z][_0-9A-Za-z]*;
ASSIGNMENT: '<-';
CASE_ARROW: '=>';
ADD: '+';
MINUS: '-';
MULTIPLY: '*';
DIVISION: '/';
LESS_THAN: '<';
LESS_EQUAL: '<=';
EQUAL: '=';
INTEGER_COMPLEMENT: '~';

fragment ESC: '\\' (["\\/bfnrt] | UNICODE);
fragment UNICODE: 'u' HEX HEX HEX HEX;
fragment HEX: [0-9a-fA-F];