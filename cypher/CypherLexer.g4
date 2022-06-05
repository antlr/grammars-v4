/*
 [The "BSD licence"]
 Copyright (c) 2022 Boris Zhguchev
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:
 1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.
 3. The name of the author may not be used to endorse or promote products
    derived from this software without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
lexer grammar CypherLexer;
ASSIGN: '=';
ADD_ASSIGN: '+=';
LE: '<=';
GE: '>=';
GT: '>';
LT: '<';
NOT_EQUAL: '<>';
RANGE: '..';
SEMI: ';';
DOT: '.';
COMMA: ',';
LPAREN: '(';
RPAREN: ')';
LBRACE: '{';
RBRACE: '}';
LBRACK: '[';
RBRACK: ']';
SUB: '-';
PLUS:'+';
DIV: '/';
MOD: '%';
CARET: '^';
MULT: '*';
ESC_ID: ESC ANY_BUT_ESC ESC;
ESC: '`';
COLON: ':';
STICK: '|';
DOLLAR: '$';

CALL:C A L L;
YIELD:Y I E L D;
FILTER: F I L T E R;
EXTRACT: E X T R A C T;
COUNT: C O U N T;
ANY: A N Y ;
NONE: N O N E;
SINGLE:S I N G L E;
ALL: A L L;
ASC: A S C;
ASCENDING:A S C E N D I N G;
BY: B Y ;
CREATE:C R E A T E;
DELETE:D E L E T E;
DESC:D E S C;
DESCENDING:D E S C E N D I N G;
DETACH: D E T A C H;
EXISTS: E X I S T S;
LIMIT:L I M I T;
MATCH:M A T C H;
MERGE: M E R G E;
ON: O N;
OPTIONAL:O P T I O N A L;
ORDER: O R D E R;
REMOVE: R E M O V E;
RETURN: R E T U R N;
SET: S E T;
SKIP_W: S K I P;
WHERE: W H E R E;
WITH: W I T H;
UNION: U N I O N;
UNWIND: U N W I N D;
AND: A N D;
AS: A S ;
CONTAINS: C O N T A I N S;
DISTINCT: D I S T I N C T;
ENDS: E N D S;
IN: I N;
IS: I S;
NOT: N O T;
OR: O R;
STARTS: S T A R T S;
XOR: X O R;
FALSE: F A L S E;
TRUE: T R U E;
NULL: N U L L;
CONSTRAINT: C O N S T R A I N T;
DO: D O;
FOR: F O R;
REQUIRE: R E Q U I R E;
UNIQUE: U N I Q U E;
CASE: C A S E;
WHEN: W H E N;
THEN: T H E N;
ELSE: E L S E;
END: E N D;
MANDATORY: M A N D A T O R Y;
SCALAR: S C A L A R;
OF: O F;
ADD: A D D;
DROP: D R O P;

CHAR_LITERAL:       '\'' (~['\\\r\n] | EscapeSequence) '\'';
STRING_LITERAL:     '"' (~["\\\r\n] | EscapeSequence)* '"';
ANY_BUT_ESC : ~[`];

ID: LetterOrDigit;
DIGIT: SUB (HexDigit | OctalDigit | Digits);

WS: [ \t\r\n\u000C]+ -> channel(HIDDEN);
COMMENT: '/*' .*? '*/' -> channel(2);
LINE_COMMENT: '//' ~[\r\n]* -> channel(2);
ERRCHAR: . -> channel(HIDDEN);

fragment EscapeSequence
    : '\\' [btnfr"'\\]
    | '\\' ([0-3]? [0-7])? [0-7]
    | '\\' 'u'+ HexDigit HexDigit HexDigit HexDigit
    ;

fragment ExponentPart: [eE] [+-]? Digits;

fragment HexDigits: '0x' HexDigit ((HexDigit | '_')* HexDigit)?;
fragment HexDigit : [0-9a-fA-F] ;
fragment OctalDigit: '0' Digits;
fragment Digits : [1-9] ([0-9_]* [0-9])? ;

fragment LetterOrDigit: Letter | [0-9];

Letter:
	[a-zA-Z_]
	| ~[\u0000-\u007F\uD800-\uDBFF] // covers all characters above 0x7F which are not a surrogate
	| [\uD800-\uDBFF] [\uDC00-\uDFFF]; // covers UTF-16 surrogate pairs encodings for U+10000 to U+10FFFF

fragment A : [aA];
fragment B : [bB];
fragment C : [cC];
fragment D : [dD];
fragment E : [eE];
fragment F : [fF];
fragment G : [gG];
fragment H : [hH];
fragment I : [iI];
fragment J : [jJ];
fragment K : [kK];
fragment L : [lL];
fragment M : [mM];
fragment N : [nN];
fragment O : [oO];
fragment P : [pP];
fragment Q : [qQ];
fragment R : [rR];
fragment S : [sS];
fragment T : [tT];
fragment U : [uU];
fragment V : [vV];
fragment W : [wW];
fragment X : [xX];
fragment Y : [yY];
fragment Z : [zZ];