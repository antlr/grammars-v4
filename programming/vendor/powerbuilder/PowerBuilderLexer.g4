/*
BSD License
Copyright (c) 2018, Tom Everett
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

lexer grammar PowerBuilderLexer;

options {
    caseInsensitive = true;
}

// Keywords
ANY: 'ANY';
BLOB: 'BLOB';
BOOLEAN: 'BOOLEAN';
BYTE: 'BYTE';
CHARACTER: 'CHARACTER';
CHAR: 'CHAR';
DATE_TYPE: 'DATE';
DATETIME: 'DATETIME';
DECIMAL: 'DECIMAL';
DEC: 'DEC';
DOUBLE: 'DOUBLE';
INTEGER: 'INTEGER';
INT: 'INT';
LONG: 'LONG';
LONGLONG: 'LONGLONG';
REAL: 'REAL';
STRING: 'STRING';
TIME_TYPE: 'TIME';
UNSIGNEDINTEGER: 'UNSIGNEDINTEGER';
UINT: 'UINT';
UNSIGNEDLONG: 'UNSIGNEDLONG';
ULONG: 'ULONG';
WINDOW: 'WINDOW';
TRUE: 'TRUE';
FALSE: 'FALSE';
GLOBAL: 'GLOBAL';
SHARED: 'SHARED';
END: 'END';
INDIRECT: 'INDIRECT';
VARIABLES: 'VARIABLES';
FORWARD: 'FORWARD';
PUBLIC: 'PUBLIC';
PRIVATE: 'PRIVATE';
FUNCTION: 'FUNCTION';
SUBROUTINE: 'SUBROUTINE';
READONLY: 'READONLY';
PROTOTYPES: 'PROTOTYPES';
TYPE: 'TYPE';
ON: 'ON';
TO: 'TO';
FROM: 'FROM';
REF: 'REF';
NULL_: 'NULL';
UPDATE: 'UPDATE';
CASE: 'CASE';
DYNAMIC: 'DYNAMIC';
WITHIN: 'WITHIN';
PRIVATEWRITE: 'PRIVATEWRITE';
PROTECTED: 'PROTECTED';
PRIVATEREAD: 'PRIVATEREAD';
PROTECTEDREAD: 'PROTECTEDREAD';
PROTECTEDWRITE: 'PROTECTEDWRITE';
LOCAL: 'LOCAL';
EVENT: 'EVENT';
OPEN: 'OPEN';
GOTO: 'GOTO';
ELSE: 'ELSE';
IF: 'IF';
THEN: 'THEN';
ELSEIF: 'ELSEIF';
TRY: 'TRY';
EXIT: 'EXIT';
CHOOSE: 'CHOOSE';
IS: 'IS';
CONTINUE: 'CONTINUE';
DO: 'DO';
WHILE: 'WHILE';
FOR: 'FOR';
CLOSE: 'CLOSE';
NEXT: 'NEXT';
LOOP: 'LOOP';
UNTIL: 'UNTIL';
STEP: 'STEP';
CATCH: 'CATCH';
FINALLY: 'FINALLY';
THROW: 'THROW';
RELEASE: 'RELEASE';
CREATE: 'CREATE';
DESTROY: 'DESTROY';
USING: 'USING';
POST: 'POST';
TRIGGER: 'TRIGGER';
SELECT: 'SELECT';
DELETE: 'DELETE';
INSERT: 'INSERT';
DESCRIBE: 'DESCRIBE';
RETURN: 'RETURN';
OR: 'OR';
AND: 'AND';
NOT: 'NOT';
CALL: 'CALL';
HALT: 'HALT';
SUPER: 'SUPER';
LIBRARY: 'LIBRARY';
SYSTEM: 'SYSTEM';
RPCFUNC: 'RPCFUNC';
ALIAS: 'ALIAS';
THROWS: 'THROWS';
AUTOINSTANTIATE: 'AUTOINSTANTIATE';
DESCRIPTOR: 'DESCRIPTOR';
SQLCA: 'SQLCA';
IMMEDIATE: 'IMMEDIATE';
EXECUTE: 'EXECUTE';
DECLARE: 'DECLARE';
PROCEDURE: 'PROCEDURE';
INTO: 'INTO';
VALUES: 'VALUES';
WHERE: 'WHERE';
COMMIT: 'COMMIT';
CURSOR: 'CURSOR';
PREPARE: 'PREPARE';
FETCH: 'FETCH';
SET: 'SET';
CONNECT: 'CONNECT';
DISCONNECT: 'DISCONNECT';
CONSTANT: 'CONSTANT';
SELECTBLOB: 'SELECTBLOB';
UPDATEBLOB: 'UPDATEBLOB';
ROLLBACK: 'ROLLBACK';
// Operators

EQ:                 '=';
GT:                 '>';
GTE:                '>=';
LT:                 '<';
LTE:                '<=';
GTLT:               '<>';
PLUS:               '+';
MINUS:              '-';
PLUSEQ:             '+=';
MINUSEQ:            '-=';
COLONCOLON:         '::';
MULT:               '*';
DIV:                '/';
MULTEQ:             '*=';
DIVEQ:              '/=';
CARAT:              '^';
LCURLY:             '{';
RCURLY:             '}';
LBRACE:             '[';
RBRACE:             ']';
TICK:               '`';
DQUOTED_STRING:     '"' ('~~' | ~'"' | '~"')* '"';
QUOTED_STRING:      '\'' (~'\'' | '~\'')* '\'';
COMMA:              ',';
SEMI:               ';';
LPAREN:             '(';
RPAREN:             ')';
COLON:              ':';
DQUOTE:             '"';
TQ:                 '???';
DOUBLE_PIPE:        '||';
DOTDOTDOT:          '...';
AT: '@';
UNDERSCORE: '_';
// Literals

NUMBER:             (NUM '.' NUM | '.' NUM | NUM) ('E' ('+' | '-')? NUM)? ('D' | 'F')?;
DOT:                '.';
DATE:               DIGIT DIGIT DIGIT DIGIT '-' DIGIT DIGIT '-' DIGIT DIGIT;
TIME:               DIGIT DIGIT ':' DIGIT DIGIT ':' DIGIT DIGIT ('.' DIGIT DIGIT DIGIT DIGIT DIGIT DIGIT)?;

ENUM:               ID_PARTS '!';
ID:                 ID_PARTS;

// Hidden

EXPORT_HEADER:      'HA'? '$' ~[\r\n]* -> channel(HIDDEN);
LINE_CONTINUATION:  '&' WS* [\r\n] -> channel(HIDDEN);
SL_COMMENT:         '//' ~ [\r\n]* -> channel(HIDDEN);
ML_COMMENT:         '/*' .*? '*/'  -> channel(HIDDEN);
WS:                 [ \t\r\n]+     -> channel(HIDDEN);

// Fragments

fragment ID_PARTS
   : [A-Z] ([A-Z] | DIGIT | '-' | '$' | '#' | '%' | '_')*
   ;

fragment NUM
   : DIGIT+
   ;

fragment DIGIT
   : '0' .. '9'
   ;

fragment LETTER
   : 'A' .. 'Z'
   ;