/*
Ada 2012 grammar.
The MIT License (MIT).

Copyright (c) 2022, Michał Lorek.

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
*/

lexer grammar AdaLexer;

options { caseInsensitive = true; }

ABORT             : 'abort';
ABS               : 'abs';
ABSTRACT          : 'abstract';
ACCEPT_           : 'accept';
ACCESS            : 'access';
ALIASED           : 'aliased';
ALL               : 'all';
AND               : 'and';
ARRAY             : 'array';
AT                : 'at';

BEGIN             : 'begin';
BODY_             : 'body';

CASE              : 'case';
CONSTANT          : 'constant';

DECLARE           : 'declare';
DELAY             : 'delay';
DELTA             : 'delta';
DIGITS            : 'digits';
DO                : 'do';

ELSE              : 'else';
ELSIF             : 'elsif';
END               : 'end';
ENTRY             : 'entry';
EXCEPTION         : 'exception';
EXIT              : 'exit';

FOR               : 'for';
FUNCTION          : 'function';

GENERIC           : 'generic';
GOTO              : 'goto';

IF                : 'if';
IN                : 'in';
INTERFACE         : 'interface';
IS                : 'is';

LIMITED           : 'limited';
LOOP              : 'loop';

MOD               : 'mod';

NEW               : 'new';
NOT               : 'not';
NULL_             : 'null';

OF                : 'of';
OR                : 'or';
OTHERS            : 'others';
OUT               : 'out';
OVERRIDING        : 'overriding';
PACKAGE           : 'package';
PRAGMA            : 'pragma';
PRIVATE           : 'private';
PROCEDURE         : 'procedure';
PROTECTED         : 'protected';

RAISE             : 'raise';
RANGE_            : 'range';
RECORD            : 'record';
REM               : 'rem';
RENAMES           : 'renames';
REQUEUE           : 'requeue';
RETURN            : 'return';
REVERSE           : 'reverse';

SELECT            : 'select';
SEPARATE          : 'separate';
SOME              : 'some';
SUBTYPE           : 'subtype';
SYNCHRONIZED      : 'synchronized';

TAGGED            : 'tagged';
TASK              : 'task';
TERMINATE         : 'terminate';
THEN              : 'then';
TYPE              : 'type';

UNTIL             : 'until';
USE               : 'use';

WHEN              : 'when';
WHILE             : 'while';
WITH              : 'with';

XOR               : 'xor';

CLASS__    options { caseInsensitive = false; }         : 'Class';
ACCESS__   options { caseInsensitive = false; }         : 'Access';
DELTA__    options { caseInsensitive = false; }         : 'Delta';
DIGITS__   options { caseInsensitive = false; }         : 'Digits';
MOD__      options { caseInsensitive = false; }         : 'Mod';

WHITESPACE        : [ \t\r\n]+               -> channel(HIDDEN);
LINE_COMMENT      : '--' ~[\r\n]*            -> channel(HIDDEN);

IDENTIFIER_       : LETTER+ [A-Z_0-9]*;
NUMERIC_LITERAL_  : DECIMAL_LITERAL_ | BASED_LITERAL;
DECIMAL_LITERAL_  : NUMERAL ('.' NUMERAL)? EXPONENT?;
NUMERAL           : DIGIT+ ('_'? DIGIT)*;
EXPONENT          : 'E' '+'? NUMERAL | 'E' '-' NUMERAL;
BASED_LITERAL     : BASE '#' BASED_NUMERAL ('.' BASED_NUMERAL)? '#' EXPONENT?;
BASED_NUMERAL     : EXTENDED_DIGIT ('_'? EXTENDED_DIGIT)*;
EXTENDED_DIGIT    : DIGIT | [A-F];
BASE              : NUMERAL;

CHARACTER_LITERAL_: '\'' ~['\\\r\n] '\'';
STRING_LITERAL_   : '"' ('""' | ~'"' )* '"';

fragment LETTER   : [A-Z];
fragment DIGIT    : [0-9];

HASH              : '#';
AMPERSAND         : '&';
LP                : '(';
RP                : ')';
MULT              : '*';
PLUS              : '+';
COMMA             : ',';
MINUS             : '-';
DOT               : '.';
COLON             : ':';
SEMI              : ';';
LT                : '<';
EQ                : '=';
GT                : '>';
US                : '_';
VL                : '|';
DIV               : '/';
EP                : '!';
PS                : '%';
ARROW             : '=>';
DOTDOT            : '..';
EXPON             : '**';
ASSIGN            : ':=';
NE                : '/=';
GE                : '>=';
LE                : '<=';
LLB               : '<<';
RLB               : '>>';
BOX               : '<>';
SQ                : '\'';
