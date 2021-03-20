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

// $antlr-format alignTrailingComments on, columnLimit 150, maxEmptyLinesToKeep 1, reflowComments off, useTab off
// $antlr-format allowShortRulesOnASingleLine on, alignSemicolons ownLine

lexer grammar SQLiteLexer;

SCOL:      ';';
DOT:       '.';
OPEN_PAR:  '(';
CLOSE_PAR: ')';
COMMA:     ',';
ASSIGN:    '=';
STAR:      '*';
PLUS:      '+';
MINUS:     '-';
TILDE:     '~';
PIPE2:     '||';
DIV:       '/';
MOD:       '%';
LT2:       '<<';
GT2:       '>>';
AMP:       '&';
PIPE:      '|';
LT:        '<';
LT_EQ:     '<=';
GT:        '>';
GT_EQ:     '>=';
EQ:        '==';
NOT_EQ1:   '!=';
NOT_EQ2:   '<>';

// http://www.sqlite.org/lang_keywords.html
ABORT_SYM:             A B O R T;
ACTION_SYM:            A C T I O N;
ADD_SYM:               A D D;
AFTER_SYM:             A F T E R;
ALL_SYM:               A L L;
ALTER_SYM:             A L T E R;
ANALYZE_SYM:           A N A L Y Z E;
AND_SYM:               A N D;
AS_SYM:                A S;
ASC_SYM:               A S C;
ATTACH_SYM:            A T T A C H;
AUTOINCREMENT_SYM:     A U T O I N C R E M E N T;
BEFORE_SYM:            B E F O R E;
BEGIN_SYM:             B E G I N;
BETWEEN_SYM:           B E T W E E N;
BY_SYM:                B Y;
CASCADE_SYM:           C A S C A D E;
CASE_SYM:              C A S E;
CAST_SYM:              C A S T;
CHECK_SYM:             C H E C K;
COLLATE_SYM:           C O L L A T E;
COLUMN_SYM:            C O L U M N;
COMMIT_SYM:            C O M M I T;
CONFLICT_SYM:          C O N F L I C T;
CONSTRAINT_SYM:        C O N S T R A I N T;
CREATE_SYM:            C R E A T E;
CROSS_SYM:             C R O S S;
CURRENT_DATE_SYM:      C U R R E N T '_' D A T E;
CURRENT_TIME_SYM:      C U R R E N T '_' T I M E;
CURRENT_TIMESTAMP_SYM: C U R R E N T '_' T I M E S T A M P;
DATABASE_SYM:          D A T A B A S E;
DEFAULT_SYM:           D E F A U L T;
DEFERRABLE_SYM:        D E F E R R A B L E;
DEFERRED_SYM:          D E F E R R E D;
DELETE_SYM:            D E L E T E;
DESC_SYM:              D E S C;
DETACH_SYM:            D E T A C H;
DISTINCT_SYM:          D I S T I N C T;
DROP_SYM:              D R O P;
EACH_SYM:              E A C H;
ELSE_SYM:              E L S E;
END_SYM:               E N D;
ESCAPE_SYM:            E S C A P E;
EXCEPT_SYM:            E X C E P T;
EXCLUSIVE_SYM:         E X C L U S I V E;
EXISTS_SYM:            E X I S T S;
EXPLAIN_SYM:           E X P L A I N;
FAIL_SYM:              F A I L;
FOR_SYM:               F O R;
FOREIGN_SYM:           F O R E I G N;
FROM_SYM:              F R O M;
FULL_SYM:              F U L L;
GLOB_SYM:              G L O B;
GROUP_SYM:             G R O U P;
HAVING_SYM:            H A V I N G;
IF_SYM:                I F;
IGNORE_SYM:            I G N O R E;
IMMEDIATE_SYM:         I M M E D I A T E;
IN_SYM:                I N;
INDEX_SYM:             I N D E X;
INDEXED_SYM:           I N D E X E D;
INITIALLY_SYM:         I N I T I A L L Y;
INNER_SYM:             I N N E R;
INSERT_SYM:            I N S E R T;
INSTEAD_SYM:           I N S T E A D;
INTERSECT_SYM:         I N T E R S E C T;
INTO_SYM:              I N T O;
IS_SYM:                I S;
ISNULL_SYM:            I S N U L L;
JOIN_SYM:              J O I N;
KEY_SYM:               K E Y;
LEFT_SYM:              L E F T;
LIKE_SYM:              L I K E;
LIMIT_SYM:             L I M I T;
MATCH_SYM:             M A T C H;
NATURAL_SYM:           N A T U R A L;
NO_SYM:                N O;
NOT_SYM:               N O T;
NOTNULL_SYM:           N O T N U L L;
NULL_SYM:              N U L L;
OF_SYM:                O F;
OFFSET_SYM:            O F F S E T;
ON_SYM:                O N;
OR_SYM:                O R;
ORDER_SYM:             O R D E R;
OUTER_SYM:             O U T E R;
PLAN_SYM:              P L A N;
PRAGMA_SYM:            P R A G M A;
PRIMARY_SYM:           P R I M A R Y;
QUERY_SYM:             Q U E R Y;
RAISE_SYM:             R A I S E;
RECURSIVE_SYM:         R E C U R S I V E;
REFERENCES_SYM:        R E F E R E N C E S;
REGEXP_SYM:            R E G E X P;
REINDEX_SYM:           R E I N D E X;
RELEASE_SYM:           R E L E A S E;
RENAME_SYM:            R E N A M E;
REPLACE_SYM:           R E P L A C E;
RESTRICT_SYM:          R E S T R I C T;
RIGHT_SYM:             R I G H T;
ROLLBACK_SYM:          R O L L B A C K;
ROW_SYM:               R O W;
ROWS_SYM:              R O W S;
SAVEPOINT_SYM:         S A V E P O I N T;
SELECT_SYM:            S E L E C T;
SET_SYM:               S E T;
TABLE_SYM:             T A B L E;
TEMP_SYM:              T E M P;
TEMPORARY_SYM:         T E M P O R A R Y;
THEN_SYM:              T H E N;
TO_SYM:                T O;
TRANSACTION_SYM:       T R A N S A C T I O N;
TRIGGER_SYM:           T R I G G E R;
UNION_SYM:             U N I O N;
UNIQUE_SYM:            U N I Q U E;
UPDATE_SYM:            U P D A T E;
USING_SYM:             U S I N G;
VACUUM_SYM:            V A C U U M;
VALUES_SYM:            V A L U E S;
VIEW_SYM:              V I E W;
VIRTUAL_SYM:           V I R T U A L;
WHEN_SYM:              W H E N;
WHERE_SYM:             W H E R E;
WITH_SYM:              W I T H;
WITHOUT_SYM:           W I T H O U T;
FIRST_VALUE_SYM:       F I R S T '_' V A L U E;
OVER_SYM:              O V E R;
PARTITION_SYM:         P A R T I T I O N;
RANGE_SYM:             R A N G E;
PRECEDING_SYM:         P R E C E D I N G;
UNBOUNDED_SYM:         U N B O U N D E D;
CURRENT_SYM:           C U R R E N T;
FOLLOWING_SYM:         F O L L O W I N G;
CUME_DIST_SYM:         C U M E '_' D I S T;
DENSE_RANK_SYM:        D E N S E '_' R A N K;
LAG_SYM:               L A G;
LAST_VALUE_SYM:        L A S T '_' V A L U E;
LEAD_SYM:              L E A D;
NTH_VALUE_SYM:         N T H '_' V A L U E;
NTILE_SYM:             N T I L E;
PERCENT_RANK_SYM:      P E R C E N T '_' R A N K;
RANK_SYM:              R A N K;
ROW_NUMBER_SYM:        R O W '_' N U M B E R;
GENERATED_SYM:         G E N E R A T E D;
ALWAYS_SYM:            A L W A Y S;
STORED_SYM:            S T O R E D;
TRUE_SYM:              T R U E;
FALSE_SYM:             F A L S E;
WINDOW_SYM:            W I N D O W;
NULLS_SYM:             N U L L S;
FIRST_SYM:             F I R S T;
LAST_SYM:              L A S T;
FILTER_SYM:            F I L T E R;
GROUPS_SYM:            G R O U P S;
EXCLUDE_SYM:           E X C L U D E;
TIES_SYM:              T I E S;
OTHERS_SYM:            O T H E R S;
DO_SYM:                D O;
NOTHING_SYM:           N O T H I N G;

IDENTIFIER:
    '"' (~'"' | '""')* '"'
    | '`' (~'`' | '``')* '`'
    | '[' ~']'* ']'
    | [a-zA-Z_] [a-zA-Z_0-9]*
; // TODO check: needs more chars in set

NUMERIC_LITERAL: ((DIGIT+ ('.' DIGIT*)?) | ('.' DIGIT+)) (E [-+]? DIGIT+)? | '0x' HEX_DIGIT+;

BIND_PARAMETER: '?' DIGIT* | [:@$] IDENTIFIER;

STRING_LITERAL: '\'' ( ~'\'' | '\'\'')* '\'';

BLOB_LITERAL: X STRING_LITERAL;

SINGLE_LINE_COMMENT: '--' ~[\r\n]* (('\r'? '\n') | EOF) -> channel(HIDDEN);

MULTILINE_COMMENT: '/*' .*? '*/' -> channel(HIDDEN);

SPACES: [ \u000B\t\r\n] -> channel(HIDDEN);

UNEXPECTED_CHAR: .;

fragment HEX_DIGIT: [0-9a-fA-F];
fragment DIGIT:     [0-9];

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
