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

// $antlr-format alignTrailingComments true, columnLimit 150, maxEmptyLinesToKeep 1, reflowComments false, useTab false
// $antlr-format allowShortRulesOnASingleLine true, allowShortBlocksOnASingleLine true, minEmptyLines 0, alignSemicolons ownLine
// $antlr-format alignColons trailing, singleLineOverrulesHangingColon true, alignLexerCommands true, alignLabels true, alignTrailers true

lexer grammar AdaLexer;

options {
    caseInsensitive = true;
    superClass = AdaLexerBase;
}

channels { PRAGMA_CHANNEL }

// Insert here @header for lexer.

ABORT    : 'abort';
ABS      : 'abs';
ABSTRACT : 'abstract';
ACCEPT_  : 'accept';
ACCESS   : 'access';
ALIASED  : 'aliased';
ALL      : 'all';
AND      : 'and';
ARRAY    : 'array';
AT       : 'at';

BEGIN : 'begin';
BODY_ : 'body';

CASE     : 'case';
CONSTANT : 'constant';

DECLARE : 'declare';
DELAY   : 'delay';
DELTA   : 'delta';
DIGITS  : 'digits';
DO      : 'do';

ELSE      : 'else';
ELSIF     : 'elsif';
END       : 'end';
ENTRY     : 'entry';
EXCEPTION : 'exception';
EXIT      : 'exit';

FOR      : 'for';
FUNCTION : 'function';

GENERIC : 'generic';
GOTO    : 'goto';

IF        : 'if';
IN        : 'in';
INTERFACE : 'interface';
IS        : 'is';

LIMITED : 'limited';
LOOP    : 'loop';

MOD: 'mod';

NEW   : 'new';
NOT   : 'not';
NULL_ : 'null';

OF         : 'of';
OR         : 'or';
OTHERS     : 'others';
OUT        : 'out';
OVERRIDING : 'overriding';
PACKAGE    : 'package';
PRAGMA     : 'pragma' -> channel(PRAGMA_CHANNEL), mode(PRAGMA_MODE);
PRIVATE    : 'private';
PROCEDURE  : 'procedure';
PROTECTED  : 'protected';

RAISE   : 'raise';
RANGE_  : 'range';
RECORD  : 'record';
REM     : 'rem';
RENAMES : 'renames';
REQUEUE : 'requeue';
RETURN  : 'return';
REVERSE : 'reverse';

SELECT       : 'select';
SEPARATE     : 'separate';
SOME         : 'some';
SUBTYPE      : 'subtype';
SYNCHRONIZED : 'synchronized';

TAGGED    : 'tagged';
TASK      : 'task';
TERMINATE : 'terminate';
THEN      : 'then';
TYPE      : 'type';

UNTIL : 'until';
USE   : 'use';

WHEN  : 'when';
WHILE : 'while';
WITH  : 'with';

XOR: 'xor';

CLASS__ options {
    caseInsensitive = false;
}: 'Class';
ACCESS__ options {
    caseInsensitive = false;
}: 'Access';
DELTA__ options {
    caseInsensitive = false;
}: 'Delta';
DIGITS__ options {
    caseInsensitive = false;
}: 'Digits';
MOD__ options {
    caseInsensitive = false;
}: 'Mod';

WHITESPACE   : [\u0009\u000A\u000B\u000C\u000D\u0020]+ -> channel(HIDDEN);
LINE_COMMENT : '--' ~[\r\n]* -> channel(HIDDEN);

IDENTIFIER_      : LETTER+ [A-Z_0-9]*;
NUMERIC_LITERAL_ : DECIMAL_LITERAL_ | BASED_LITERAL;
DECIMAL_LITERAL_ : NUMERAL ('.' NUMERAL)? EXPONENT?;
NUMERAL          : DIGIT+ ('_'? DIGIT)*;
EXPONENT         : 'E' '+'? NUMERAL | 'E' '-' NUMERAL;
BASED_LITERAL    : BASE '#' BASED_NUMERAL ('.' BASED_NUMERAL)? '#' EXPONENT?;
BASED_NUMERAL    : EXTENDED_DIGIT ('_'? EXTENDED_DIGIT)*;
EXTENDED_DIGIT   : DIGIT | [A-F];
BASE             : NUMERAL;

CHARACTER_LITERAL_ : {this.IsCharLiteralAllowed()}? '\'' ~['\\\r\n] '\'';
STRING_LITERAL_    : '"' ('""' | ~'"')* '"';

fragment LETTER : [A-Z];
fragment DIGIT  : [0-9];

HASH      : '#';
AMPERSAND : '&';
LP        : '(';
RP        : ')';
MULT      : '*';
PLUS      : '+';
COMMA     : ',';
MINUS     : '-';
DOT       : '.';
COLON     : ':';
SEMI      : ';';
LT        : '<';
EQ        : '=';
GT        : '>';
US        : '_';
VL        : '|' | '!';
DIV       : '/';
PS        : '%';
ARROW     : '=>';
DOTDOT    : '..';
EXPON     : '**';
ASSIGN    : ':=';
NE        : '/=';
GE        : '>=';
LE        : '<=';
LLB       : '<<';
RLB       : '>>';
BOX       : '<>';
SQ        : '\'';

mode PRAGMA_MODE;
PRAGMA_WHITESPACE      : [\u0009\u000A\u000B\u000C\u000D\u0020]+ -> channel(HIDDEN);
PRAGMA_LINE_COMMENT    : '--' ~[\r\n]*                            -> channel(HIDDEN);
PRAGMA_IDENTIFIER      : [A-Za-z]+ [A-Za-z_0-9]*   -> channel(PRAGMA_CHANNEL), type(IDENTIFIER_);
PRAGMA_STRING_LITERAL  : '"' ('""' | ~'"')* '"'     -> channel(PRAGMA_CHANNEL), type(STRING_LITERAL_);
PRAGMA_CHAR_LITERAL    : '\'' ~['\\\r\n] '\''       -> channel(PRAGMA_CHANNEL), type(CHARACTER_LITERAL_);
PRAGMA_NUMERIC_LITERAL : [0-9]+ ('_'? [0-9])* (('.' [0-9]+ ('_'? [0-9])*)? ([Ee] [+-]? [0-9]+ ('_'? [0-9])*)? | '#' [0-9A-Fa-f]+ ('_'? [0-9A-Fa-f])* ('.' [0-9A-Fa-f]+ ('_'? [0-9A-Fa-f])*)? '#' ([Ee] [+-]? [0-9]+ ('_'? [0-9])*)?) -> channel(PRAGMA_CHANNEL), type(NUMERIC_LITERAL_);
PRAGMA_ARROW           : '=>' -> channel(PRAGMA_CHANNEL), type(ARROW);
PRAGMA_DOTDOT          : '..' -> channel(PRAGMA_CHANNEL), type(DOTDOT);
PRAGMA_EXPON           : '**' -> channel(PRAGMA_CHANNEL), type(EXPON);
PRAGMA_NE              : '/=' -> channel(PRAGMA_CHANNEL), type(NE);
PRAGMA_GE              : '>=' -> channel(PRAGMA_CHANNEL), type(GE);
PRAGMA_LE              : '<=' -> channel(PRAGMA_CHANNEL), type(LE);
PRAGMA_LP              : '('  -> channel(PRAGMA_CHANNEL), type(LP);
PRAGMA_RP              : ')'  -> channel(PRAGMA_CHANNEL), type(RP);
PRAGMA_COMMA           : ','  -> channel(PRAGMA_CHANNEL), type(COMMA);
PRAGMA_DOT             : '.'  -> channel(PRAGMA_CHANNEL), type(DOT);
PRAGMA_SQ              : '\'' -> channel(PRAGMA_CHANNEL), type(SQ);
PRAGMA_SEMI            : ';'  -> channel(PRAGMA_CHANNEL), type(SEMI), mode(DEFAULT_MODE);
PRAGMA_PLUS            : '+'  -> channel(PRAGMA_CHANNEL), type(PLUS);
PRAGMA_MINUS           : '-'  -> channel(PRAGMA_CHANNEL), type(MINUS);
PRAGMA_MULT            : '*'  -> channel(PRAGMA_CHANNEL), type(MULT);
PRAGMA_DIV             : '/'  -> channel(PRAGMA_CHANNEL), type(DIV);
PRAGMA_EQ              : '='  -> channel(PRAGMA_CHANNEL), type(EQ);
PRAGMA_LT              : '<'  -> channel(PRAGMA_CHANNEL), type(LT);
PRAGMA_GT              : '>'  -> channel(PRAGMA_CHANNEL), type(GT);
PRAGMA_AMPERSAND       : '&'  -> channel(PRAGMA_CHANNEL), type(AMPERSAND);
PRAGMA_VL              : '|'  -> channel(PRAGMA_CHANNEL), type(VL);