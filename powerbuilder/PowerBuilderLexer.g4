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

// Keywords
ANY: A N Y ;
BLOB: B L O B ;
BOOLEAN: B O O L E A N ;
BYTE: B Y T E ;
CHARACTER: C H A R A C T E R ;
CHAR: C H A R ;
DATE_TYPE: D A T E ;
DATETIME: D A T E T I M E ;
DECIMAL: D E C I M A L ;
DEC: D E C ;
DOUBLE: D O U B L E ;
INTEGER: I N T E G E R ;
INT: I N T ;
LONG: L O N G ;
LONGLONG: L O N G L O N G ;
REAL: R E A L ;
STRING: S T R I N G ;
TIME_TYPE: T I M E ;
UNSIGNEDINTEGER: U N S I G N E D I N T E G E R ;
UINT: U I N T ;
UNSIGNEDLONG: U N S I G N E D L O N G ;
ULONG: U L O N G ;
WINDOW: W I N D O W ;
TRUE: T R U E ;
FALSE: F A L S E ;
GLOBAL: G L O B A L ;
SHARED: S H A R E D ;
END: E N D ;
INDIRECT : I N D I R E C T ;
VARIABLES: V A R I A B L E S ;
FORWARD: F O R W A R D ;
PUBLIC: P U B L I C ;
PRIVATE: P R I V A T E ;
FUNCTION: F U N C T I O N ;
SUBROUTINE: S U B R O U T I N E ;
READONLY: R E A D O N L Y ;
PROTOTYPES: P R O T O T Y P E S ;
TYPE: T Y P E ;
ON: O N ;
TO: T O ;
FROM: F R O M ;
REF: R E F ;
NULL_: N U L L ;
UPDATE: U P D A T E ;
CASE: C A S E ;
DYNAMIC: D Y N A M I C ;
WITHIN: W I T H I N ;
PRIVATEWRITE: P R I V A T E W R I T E ;
PROTECTED: P R O T E C T E D ;
PRIVATEREAD: P R I V A T E R E A D ;
PROTECTEDREAD: P R O T E C T E D R E A D ;
PROTECTEDWRITE: P R O T E C T E D W R I T E ;
LOCAL: L O C A L ;
EVENT: E V E N T ;
OPEN: O P E N ;
GOTO: G O T O ;
ELSE: E L S E ;
IF: I F ;
THEN: T H E N ;
ELSEIF: E L S E I F ;
TRY: T R Y ;
EXIT: E X I T ;
CHOOSE: C H O O S E ;
IS: I S ;
CONTINUE: C O N T I N U E ;
DO: D O ;
WHILE: W H I L E ;
FOR: F O R ;
CLOSE: C L O S E ;
NEXT: N E X T ;
LOOP: L O O P ;
UNTIL: U N T I L ;
STEP: S T E P ;
CATCH: C A T C H ;
FINALLY: F I N A L L Y ;
THROW: T H R O W ;
RELEASE: R E L E A S E ;
CREATE: C R E A T E ;
DESTROY: D E S T R O Y ;
USING: U S I N G ;
POST: P O S T ;
TRIGGER: T R I G G E R ;
SELECT: S E L E C T ;
DELETE: D E L E T E ;
INSERT: I N S E R T ;
DESCRIBE: D E S C R I B E ;
RETURN: R E T U R N ;
OR: O R ;
AND: A N D ;
NOT: N O T ;
CALL: C A L L ;
HALT: H A L T ;
SUPER: S U P E R ;
LIBRARY: L I B R A R Y ;
SYSTEM: S Y S T E M ;
RPCFUNC: R P C F U N C ;
ALIAS: A L I A S ;
THROWS: T H R O W S ;
AUTOINSTANTIATE: A U T O I N S T A N T I A T E ;
DESCRIPTOR: D E S C R I P T O R ;
SQLCA: S Q L C A;
IMMEDIATE: I M M E D I A T E;
EXECUTE: E X E C U T E;
DECLARE: D E C L A R E;
PROCEDURE: P R O C E D U R E;
INTO: I N T O;
VALUES: V A L U E S;
WHERE: W H E R E;
COMMIT: C O M M I T;
CURSOR: C U R S O R;
PREPARE: P R E P A R E;
FETCH: F E T C H;
SET: S E T;
CONNECT: C O N N E C T;
DISCONNECT: D I S C O N N E C T;
CONSTANT: C O N S T A N T;
SELECTBLOB: S E L E C T B L O B;
UPDATEBLOB: U P D A T E B L O B;
ROLLBACK: R O L L B A C K;
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

EXPORT_HEADER:      ('HA')? '$' ~[\r\n]* -> channel(HIDDEN);
LINE_CONTINUATION:  '&' WS* [\r\n] -> channel(HIDDEN);
SL_COMMENT:         '//' ~ [\r\n]* -> channel(HIDDEN);
ML_COMMENT:         '/*' .*? '*/'  -> channel(HIDDEN);
WS:                 [ \t\r\n]+     -> channel(HIDDEN);

// Fragments

fragment ID_PARTS
   : [A-Za-z] ([A-Za-z] | DIGIT | '-' | '$' | '#' | '%' | '_')*
   ;

fragment NUM
   : DIGIT+
   ;

fragment DIGIT
   : '0' .. '9'
   ;

fragment LETTER
   : 'A' .. 'Z' | 'a' .. 'z'
   ;

/* case insensitive lexer matching */
fragment A:('a'|'A');
fragment B:('b'|'B');
fragment C:('c'|'C');
fragment D:('d'|'D');
fragment E:('e'|'E');
fragment F:('f'|'F');
fragment G:('g'|'G');
fragment H:('h'|'H');
fragment I:('i'|'I');
fragment J:('j'|'J');
fragment K:('k'|'K');
fragment L:('l'|'L');
fragment M:('m'|'M');
fragment N:('n'|'N');
fragment O:('o'|'O');
fragment P:('p'|'P');
fragment Q:('q'|'Q');
fragment R:('r'|'R');
fragment S:('s'|'S');
fragment T:('t'|'T');
fragment U:('u'|'U');
fragment V:('v'|'V');
fragment W:('w'|'W');
fragment X:('x'|'X');
fragment Y:('y'|'Y');
fragment Z:('z'|'Z');