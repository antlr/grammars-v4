/*
 * The MIT License (MIT)
 * 
 * Copyright (c) 2020 by Martin Mirchev
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 * 
 * Project : sqlite-parser; an ANTLR4 grammar for SQLite https://github.com/bkiers/sqlite-parser
 * Developed by : Bart Kiers, bart@big-o.nl
 */
lexer grammar SQLiteLexer;

SCOL: ';';
DOT: '.';
OPEN_PAR: '(';
CLOSE_PAR: ')';
COMMA: ',';
ASSIGN: '=';
STAR: '*';
PLUS: '+';
MINUS: '-';
TILDE: '~';
PIPE2: '||';
DIV: '/';
MOD: '%';
LT2: '<<';
GT2: '>>';
AMP: '&';
PIPE: '|';
LT: '<';
LT_EQ: '<=';
GT: '>';
GT_EQ: '>=';
EQ: '==';
NOT_EQ1: '!=';
NOT_EQ2: '<>';

// http://www.sqlite.org/lang_keywords.html
ABORT: A B O R T;
ACTION: A C T I O N;
ADD: A D D;
AFTER: A F T E R;
ALL: A L L;
ALTER: A L T E R;
ANALYZE: A N A L Y Z E;
AND: A N D;
AS: A S;
ASC: A S C;
ATTACH: A T T A C H;
AUTOINCREMENT: A U T O I N C R E M E N T;
BEFORE: B E F O R E;
BEGIN: B E G I N;
BETWEEN: B E T W E E N;
BY: B Y;
CASCADE: C A S C A D E;
CASE: C A S E;
CAST: C A S T;
CHECK: C H E C K;
COLLATE: C O L L A T E;
COLUMN: C O L U M N;
COMMIT: C O M M I T;
CONFLICT: C O N F L I C T;
CONSTRAINT: C O N S T R A I N T;
CREATE: C R E A T E;
CROSS: C R O S S;
CURRENT_DATE: C U R R E N T '_' D A T E;
CURRENT_TIME: C U R R E N T '_' T I M E;
CURRENT_TIMESTAMP: C U R R E N T '_' T I M E S T A M P;
DATABASE: D A T A B A S E;
DEFAULT: D E F A U L T;
DEFERRABLE: D E F E R R A B L E;
DEFERRED: D E F E R R E D;
DELETE: D E L E T E;
DESC: D E S C;
DETACH: D E T A C H;
DISTINCT: D I S T I N C T;
DROP: D R O P;
EACH: E A C H;
ELSE: E L S E;
END: E N D;
ESCAPE: E S C A P E;
EXCEPT: E X C E P T;
EXCLUSIVE: E X C L U S I V E;
EXISTS: E X I S T S;
EXPLAIN: E X P L A I N;
FAIL: F A I L;
FOR: F O R;
FOREIGN: F O R E I G N;
FROM: F R O M;
FULL: F U L L;
GLOB: G L O B;
GROUP: G R O U P;
HAVING: H A V I N G;
IF: I F;
IGNORE: I G N O R E;
IMMEDIATE: I M M E D I A T E;
IN: I N;
INDEX: I N D E X;
INDEXED: I N D E X E D;
INITIALLY: I N I T I A L L Y;
INNER: I N N E R;
INSERT: I N S E R T;
INSTEAD: I N S T E A D;
INTERSECT: I N T E R S E C T;
INTO: I N T O;
IS: I S;
ISNULL: I S N U L L;
JOIN: J O I N;
KEY: K E Y;
LEFT: L E F T;
LIKE: L I K E;
LIMIT: L I M I T;
MATCH: M A T C H;
NATURAL: N A T U R A L;
NO: N O;
NOT: N O T;
NOTNULL: N O T N U L L;
NULL: N U L L;
OF: O F;
OFFSET: O F F S E T;
ON: O N;
OR: O R;
ORDER: O R D E R;
OUTER: O U T E R;
PLAN: P L A N;
PRAGMA: P R A G M A;
PRIMARY: P R I M A R Y;
QUERY: Q U E R Y;
RAISE: R A I S E;
RECURSIVE: R E C U R S I V E;
REFERENCES: R E F E R E N C E S;
REGEXP: R E G E X P;
REINDEX: R E I N D E X;
RELEASE: R E L E A S E;
RENAME: R E N A M E;
REPLACE: R E P L A C E;
RESTRICT: R E S T R I C T;
RIGHT: R I G H T;
ROLLBACK: R O L L B A C K;
ROW: R O W;
ROWS: R O W S;
SAVEPOINT: S A V E P O I N T;
SELECT: S E L E C T;
SET: S E T;
TABLE: T A B L E;
TEMP: T E M P;
TEMPORARY: T E M P O R A R Y;
THEN: T H E N;
TO: T O;
TRANSACTION: T R A N S A C T I O N;
TRIGGER: T R I G G E R;
UNION: U N I O N;
UNIQUE: U N I Q U E;
UPDATE: U P D A T E;
USING: U S I N G;
VACUUM: V A C U U M;
VALUES: V A L U E S;
VIEW: V I E W;
VIRTUAL: V I R T U A L;
WHEN: W H E N;
WHERE: W H E R E;
WITH: W I T H;
WITHOUT: W I T H O U T;
FIRST_VALUE: F I R S T '_' V A L U E;
OVER: O V E R;
PARTITION: P A R T I T I O N;
RANGE: R A N G E;
PRECEDING: P R E C E D I N G;
UNBOUNDED: U N B O U N D E D;
CURRENT: C U R R E N T;
FOLLOWING: F O L L O W I N G;
CUME_DIST: C U M E '_' D I S T;
DENSE_RANK: D E N S E '_' R A N K;
LAG: L A G;
LAST_VALUE: L A S T '_' V A L U E;
LEAD: L E A D;
NTH_VALUE: N T H '_' V A L U E;
NTILE: N T I L E;
PERCENT_RANK: P E R C E N T '_' R A N K;
RANK: R A N K;
ROW_NUMBER: R O W '_' N U M B E R;
GENERATED: G E N E R A T E D;
ALWAYS: A L W A Y S;
STORED: S T O R E D;
TRUE: T R U E;
FALSE: F A L S E;
WINDOW: W I N D O W;
NULLS: N U L L S;
FIRST: F I R S T;
LAST: L A S T;
FILTER: F I L T E R;
GROUPS: G R O U P S;
EXCLUDE: E X C L U D E;
TIES: T I E S;
OTHERS: O T H E R S;
DO: D O;
NOTHING: N O T H I N G;

IDENTIFIER:
	'"' (~'"' | '""')* '"'
	| '`' (~'`' | '``')* '`'
	| '[' ~']'* ']'
	| [a-zA-Z_] [a-zA-Z_0-9]*; // TODO check: needs more chars in set

NUMERIC_LITERAL:
	((DIGIT+ ('.' DIGIT*)?) | ('.' DIGIT+)) (E [-+]? DIGIT+)?
	| '0x' HEX_DIGIT+;

BIND_PARAMETER: '?' DIGIT* | [:@$] IDENTIFIER;

STRING_LITERAL: '\'' ( ~'\'' | '\'\'')* '\'';

BLOB_LITERAL: X STRING_LITERAL;

SINGLE_LINE_COMMENT:
	'--' ~[\r\n]* (('\r'? '\n') | EOF) -> channel(HIDDEN);

MULTILINE_COMMENT: '/*' .*? ( '*/' | EOF) -> channel(HIDDEN);

SPACES: [ \u000B\t\r\n] -> channel(HIDDEN);

UNEXPECTED_CHAR: .;

fragment HEX_DIGIT: [0-9a-fA-F];
fragment DIGIT: [0-9];

fragment A: [aA];
fragment B: [bB];
fragment C: [cC];
fragment D: [dD];
fragment E: [eE];
fragment F: [fF];
fragment G: [gG];
fragment H: [hH];
fragment I: [iI];
fragment J: [jJ];
fragment K: [kK];
fragment L: [lL];
fragment M: [mM];
fragment N: [nN];
fragment O: [oO];
fragment P: [pP];
fragment Q: [qQ];
fragment R: [rR];
fragment S: [sS];
fragment T: [tT];
fragment U: [uU];
fragment V: [vV];
fragment W: [wW];
fragment X: [xX];
fragment Y: [yY];
fragment Z: [zZ];
