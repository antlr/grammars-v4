/*
ArangoDB grammar.
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

lexer grammar ArangoDbLexer;

options { caseInsensitive = true; }

//keywrods
COLLECT:                  'COLLECT';
FILTER:                   'FILTER';
FOR:                      'FOR';
INSERT:                   'INSERT';
LET:                      'LET';
LIMIT:                    'LIMIT';
REMOVE:                   'REMOVE';
REPLACE:                  'REPLACE';
RETURN:                   'RETURN';
SEARCH:                   'SEARCH';
SORT:                     'SORT';
UPDATE:                   'UPDATE';
UPSERT:                   'UPSERT';
WINDOW:                   'WINDOW';
WITH:                     'WITH';

AGGREGATE:                'AGGREGATE';
ALL:                      'ALL';
AND:                      'AND';
ANY:                      'ANY';
ASC:                      'ASC';
DESC:                     'DESC';
DISTINCT:                 'DISTINCT';
FALSE:                    'FALSE';
GRAPH:                    'GRAPH';
IN:                       'IN';
INBOUND:                  'INBOUND';
INTO:                     'INTO';
K_PATHS:                  'K_PATHS';
K_SHORTEST_PATHS:         'K_SHORTEST_PATHS';
LIKE:                     'LIKE';
NONE:                     'NONE';
NOT:                      'NOT';
NULL_:                    'NULL';
OR:                       'OR';
OUTBOUND:                 'OUTBOUND';
SHORTEST_PATH:            'SHORTEST_PATH';
TRUE:                     'TRUE';

KEEP: 'KEEP';
COUNT: 'COUNT';
OPTIONS: 'OPTIONS';
PRUNE: 'PRUNE';
TO: 'TO';

//case-sensitive:
//CURRENT – available in array inline expressions
//NEW     – available after INSERT / UPDATE / REPLACE / UPSERT operation
//OLD     – available after UPDATE / REPLACE / UPSERT / REMOVE operation
CURRENT options { caseInsensitive = false; }: 'CURRENT';
NEW     options { caseInsensitive = false; }: 'NEW';
OLD     options { caseInsensitive = false; }: 'OLD';

DOCUMENT: 'DOCUMENT';


SEMI: ';';
L_AND: '&&';
L_OR: '||';
L_NOT: '!';

EQ: '==';
NE: '!=';
LT: '<';
LE: '<=';
GT: '>';
GE: '>=';

PLUS: '+';
MINUS: '-';
STAR: '*';
DIV: '/';
MOD: '%';

QM: '?';
COLON: ':';
RANGE: '..';

ASSIGN: '=';
COMMA: ',';
SCOPE: '::';
ACCESS: '.';

REGEX_MATCH: '=~';
REGEX_NON_MATCH: '!~';

LRB: '(';
RRB: ')';
LSB: '[';
RSB: ']';
LCB: '{';
RCB: '}';

WHITE_SPACE                   : [ \t\r\n]+                   -> channel(HIDDEN);

SQL_COMMENT                   : '/*' (SQL_COMMENT | .)*? '*/' -> channel(HIDDEN);
LINE_COMMENT                  : '//' ~[\r\n]*                 -> channel(HIDDEN);

ID                            : [A-Z_] [A-Z0-9_]*;
BIND_PARAMETER                : '@' [A-Z0-9][A-Z0-9_]*;
BIND_PARAMETER_COLL           : '@@' [A-Z0-9][A-Z0-9_]*;

STRING_LITERAL                : '\'' (~'\'' | '\'\'')* '\'';
DOUBLE_QUOTED_STRING_LITERAL  : '"' ~'"'+ '"';
BACKSTICK_STRING_LITERAL      : '`' ~'`'+ '`';

DECIMAL_LITERAL               : DEC_DIGIT+;
FLOAT_LITERAL                 : DEC_DOT_DEC;
REAL_LITERAL                  : (DECIMAL_LITERAL | DEC_DOT_DEC) 'E' [+-]? DEC_DIGIT+;


fragment HexDigit             : [0-9a-f];
fragment LETTER               : [A-Z_];
fragment DEC_DOT_DEC          : DEC_DIGIT+ '.' DEC_DIGIT+ |  '.' DEC_DIGIT+;
fragment DEC_DIGIT            : [0-9];
