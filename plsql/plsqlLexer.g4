/**
 * Oracle(c) PL/SQL 11g Parser  
 *
 * Copyright (c) 2009-2011 Alexandre Porcelli <alexandre.porcelli@gmail.com>
 * Copyright (c) 2015-2017 Ivan Kochurkin (KvanTTT, kvanttt@gmail.com, Positive Technologies).
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

lexer grammar plsqlLexer;

A_LETTER:                     A;
ADD:                          A D D;
AFTER:                        A F T E R;
AGENT:                        A G E N T;
AGGREGATE:                    A G G R E G A T E;
ALL:                          A L L;
ALTER:                        A L T E R;
ANALYZE:                      A N A L Y Z E;
AND:                          A N D;
ANY:                          A N Y;
ARRAY:                        A R R A Y;
AS:                           A S;
ASC:                          A S C;
ASSOCIATE:                    A S S O C I A T E;
AT:                           A T;
ATTRIBUTE:                    A T T R I B U T E;
AUDIT:                        A U D I T;
AUTHID:                       A U T H I D;
AUTO:                         A U T O;
AUTOMATIC:                    A U T O M A T I C;
AUTONOMOUS_TRANSACTION:       A U T O N O M O U S '_' T R A N S A C T I O N;
BATCH:                        B A T C H;
BEFORE:                       B E F O R E;
BEGIN:                        B E G I N;
BETWEEN:                      B E T W E E N;
BFILE:                        B F I L E;
BINARY_DOUBLE:                B I N A R Y '_' D O U B L E;
BINARY_FLOAT:                 B I N A R Y '_' F L O A T;
BINARY_INTEGER:               B I N A R Y '_' I N T E G E R;
BLOB:                         B L O B;
BLOCK:                        B L O C K;
BODY:                         B O D Y;
BOOLEAN:                      B O O L E A N;
BOTH:                         B O T H;
BREADTH:                      B R E A D T H;
BULK:                         B U L K;
BY:                           B Y;
BYTE:                         B Y T E;
C_LETTER:                     C;
CACHE:                        C A C H E;
CALL:                         C A L L;
CANONICAL:                    C A N O N I C A L;
CASCADE:                      C A S C A D E;
CASE:                         C A S E;
CAST:                         C A S T;
CHAR:                         C H A R;
CHAR_CS:                      C H A R '_' C S;
CHARACTER:                    C H A R A C T E R;
CHECK:                        C H E C K;
CHR:                          C H R;
CLOB:                         C L O B;
CLOSE:                        C L O S E;
CLUSTER:                      C L U S T E R;
COALESCE:                     C O A L E S C E;
COLLECT:                      C O L L E C T;
COLUMNS:                      C O L U M N S;
COMMENT:                      C O M M E N T;
COMMIT:                       C O M M I T;
COMMITTED:                    C O M M I T T E D;
COMPATIBILITY:                C O M P A T I B I L I T Y;
COMPILE:                      C O M P I L E;
COMPOUND:                     C O M P O U N D;
CONNECT:                      C O N N E C T;
CONNECT_BY_ROOT:              C O N N E C T '_' B Y '_' R O O T;
CONSTANT:                     C O N S T A N T;
CONSTRAINT:                   C O N S T R A I N T;
CONSTRAINTS:                  C O N S T R A I N T S;
CONSTRUCTOR:                  C O N S T R U C T O R;
CONTENT:                      C O N T E N T;
CONTEXT:                      C O N T E X T;
CONTINUE:                     C O N T I N U E;
CONVERT:                      C O N V E R T;
CORRUPT_XID:                  C O R R U P T '_' X I D;
CORRUPT_XID_ALL:              C O R R U P T '_' X I D '_' A L L;
COST:                         C O S T;
COUNT:                        C O U N T;
CREATE:                       C R E A T E;
CROSS:                        C R O S S;
CUBE:                         C U B E;
CURRENT:                      C U R R E N T;
CURRENT_USER:                 C U R R E N T '_' U S E R;
CURSOR:                       C U R S O R;
CUSTOMDATUM:                  C U S T O M D A T U M;
CYCLE:                        C Y C L E;
DATA:                         D A T A;
DATABASE:                     D A T A B A S E;
DATE:                         D A T E;
DAY:                          D A Y;
DB_ROLE_CHANGE:               D B '_' R O L E '_' C H A N G E;
DBTIMEZONE:                   D B T I M E Z O N E;
DDL:                          D D L;
DEBUG:                        D E B U G;
DEC:                          D E C;
DECIMAL:                      D E C I M A L;
DECLARE:                      D E C L A R E;
DECOMPOSE:                    D E C O M P O S E;
DECREMENT:                    D E C R E M E N T;
DEFAULT:                      D E F A U L T;
DEFAULTS:                     D E F A U L T S;
DEFERRED:                     D E F E R R E D;
DEFINER:                      D E F I N E R;
DELETE:                       D E L E T E;
DEPTH:                        D E P T H;
DESC:                         D E S C;
DETERMINISTIC:                D E T E R M I N I S T I C;
DIMENSION:                    D I M E N S I O N;
DISABLE:                      D I S A B L E;
DISASSOCIATE:                 D I S A S S O C I A T E;
DISTINCT:                     D I S T I N C T;
DOCUMENT:                     D O C U M E N T;
DOUBLE:                       D O U B L E;
DROP:                         D R O P;
DSINTERVAL_UNCONSTRAINED:     D S I N T E R V A L '_' U N C O N S T R A I N E D;
EACH:                         E A C H;
ELEMENT:                      E L E M E N T;
ELSE:                         E L S E;
ELSIF:                        E L S I F;
EMPTY:                        E M P T Y;
ENABLE:                       E N A B L E;
ENCODING:                     E N C O D I N G;
END:                          E N D;
ENTITYESCAPING:               E N T I T Y E S C A P I N G;
ERR:                          E R R;
ERRORS:                       E R R O R S;
ESCAPE:                       E S C A P E;
EVALNAME:                     E V A L N A M E;
EXCEPTION:                    E X C E P T I O N;
EXCEPTION_INIT:               E X C E P T I O N '_' I N I T;
EXCEPTIONS:                   E X C E P T I O N S;
EXCLUDE:                      E X C L U D E;
EXCLUSIVE:                    E X C L U S I V E;
EXECUTE:                      E X E C U T E;
EXISTS:                       E X I S T S;
EXIT:                         E X I T;
EXPLAIN:                      E X P L A I N;
EXTERNAL:                     E X T E R N A L;
EXTRACT:                      E X T R A C T;
FAILURE:                      F A I L U R E;
FALSE:                        F A L S E;
FETCH:                        F E T C H;
FINAL:                        F I N A L;
FIRST:                        F I R S T;
FIRST_VALUE:                  F I R S T '_' V A L U E;
FLOAT:                        F L O A T;
FOLLOWING:                    F O L L O W I N G;
FOLLOWS:                      F O L L O W S;
FOR:                          F O R;
FORALL:                       F O R A L L;
FORCE:                        F O R C E;
FROM:                         F R O M;
FULL:                         F U L L;
FUNCTION:                     F U N C T I O N;
GOTO:                         G O T O;
GRANT:                        G R A N T;
GROUP:                        G R O U P;
GROUPING:                     G R O U P I N G;
HASH:                         H A S H;
HAVING:                       H A V I N G;
HIDE:                         H I D E;
HOUR:                         H O U R;
IF:                           I F;
IGNORE:                       I G N O R E;
IMMEDIATE:                    I M M E D I A T E;
IN:                           I N;
INCLUDE:                      I N C L U D E;
INCLUDING:                    I N C L U D I N G;
INCREMENT:                    I N C R E M E N T;
INDENT:                       I N D E N T;
INDEX:                        I N D E X;
INDEXED:                      I N D E X E D;
INDICATOR:                    I N D I C A T O R;
INDICES:                      I N D I C E S;
INFINITE:                     I N F I N I T E;
INLINE:                       I N L I N E;
INNER:                        I N N E R;
INOUT:                        I N O U T;
INSERT:                       I N S E R T;
INSTANTIABLE:                 I N S T A N T I A B L E;
INSTEAD:                      I N S T E A D;
INT:                          I N T;
INTEGER:                      I N T E G E R;
INTERSECT:                    I N T E R S E C T;
INTERVAL:                     I N T E R V A L;
INTO:                         I N T O;
INVALIDATE:                   I N V A L I D A T E;
IS:                           I S;
ISOLATION:                    I S O L A T I O N;
ITERATE:                      I T E R A T E;
JAVA:                         J A V A;
JOIN:                         J O I N;
KEEP:                         K E E P;
LANGUAGE:                     L A N G U A G E;
LAST:                         L A S T;
LAST_VALUE:                   L A S T '_' V A L U E;
LEADING:                      L E A D I N G;
LEFT:                         L E F T;
LEVEL:                        L E V E L;
LIBRARY:                      L I B R A R Y;
LIKE:                         L I K E;
LIKE2:                        L I K E '2';
LIKE4:                        L I K E '4';
LIKEC:                        L I K E C;
LIMIT:                        L I M I T;
LOCAL:                        L O C A L;
LOCK:                         L O C K;
LOCKED:                       L O C K E D;
LOG:                          L O G;
LOGOFF:                       L O G O F F;
LOGON:                        L O G O N;
LONG:                         L O N G;
LOOP:                         L O O P;
MAIN:                         M A I N;
MAP:                          M A P;
MATCHED:                      M A T C H E D;
MAXVALUE:                     M A X V A L U E;
MEASURES:                     M E A S U R E S;
MEMBER:                       M E M B E R;
MERGE:                        M E R G E;
MINUS:                        M I N U S;
MINUTE:                       M I N U T E;
MINVALUE:                     M I N V A L U E;
MLSLABEL:                     M L S L A B E L;
MODE:                         M O D E;
MODEL:                        M O D E L;
MODIFY:                       M O D I F Y;
MONTH:                        M O N T H;
MULTISET:                     M U L T I S E T;
NAME:                         N A M E;
NAN:                          N A N;
NATURAL:                      N A T U R A L;
NATURALN:                     N A T U R A L N;
NAV:                          N A V;
NCHAR:                        N C H A R;
NCHAR_CS:                     N C H A R '_' C S;
NCLOB:                        N C L O B;
NESTED:                       N E S T E D;
NEW:                          N E W;
NO:                           N O;
NOAUDIT:                      N O A U D I T;
NOCACHE:                      N O C A C H E;
NOCOPY:                       N O C O P Y;
NOCYCLE:                      N O C Y C L E;
NOENTITYESCAPING:             N O E N T I T Y E S C A P I N G;
NOMAXVALUE:                   N O M A X V A L U E;
NOMINVALUE:                   N O M I N V A L U E;
NONE:                         N O N E;
NOORDER:                      N O O R D E R;
NOSCHEMACHECK:                N O S C H E M A C H E C K;
NOT:                          N O T;
NOWAIT:                       N O W A I T;
NULL:                         N U L L;
NULLS:                        N U L L S;
NUMBER:                       N U M B E R;
NUMERIC:                      N U M E R I C;
NVARCHAR2:                    N V A R C H A R '2';
OBJECT:                       O B J E C T;
OF:                           O F;
OFF:                          O F F;
OID:                          O I D;
OLD:                          O L D;
ON:                           O N;
ONLY:                         O N L Y;
OPEN:                         O P E N;
OPTION:                       O P T I O N;
OR:                           O R;
ORADATA:                      O R A D A T A;
ORDER:                        O R D E R;
ORDINALITY:                   O R D I N A L I T Y;
OSERROR:                      O S E R R O R;
OUT:                          O U T;
OUTER:                        O U T E R;
OVER:                         O V E R;
OVERRIDING:                   O V E R R I D I N G;
PACKAGE:                      P A C K A G E;
PARALLEL_ENABLE:              P A R A L L E L '_' E N A B L E;
PARAMETERS:                   P A R A M E T E R S;
PARENT:                       P A R E N T;
PARTITION:                    P A R T I T I O N;
PASSING:                      P A S S I N G;
PATH:                         P A T H;
PERCENT_ISOPEN:               '%' I S O P E N;
PERCENT_FOUND:                '%' F O U N D;
PERCENT_NOTFOUND:             '%' N O T F O U N D;
PERCENT_ROWCOUNT:             '%' R O W C O U N T;
PERCENT_ROWTYPE:              '%' R O W T Y P E;
PERCENT_TYPE:                 '%' T Y P E;
PIPELINED:                    P I P E L I N E D;
PIVOT:                        P I V O T;
PLAN:                         P L A N;
PLS_INTEGER:                  P L S '_' I N T E G E R;
POSITIVE:                     P O S I T I V E;
POSITIVEN:                    P O S I T I V E N;
PRAGMA:                       P R A G M A;
PRECEDING:                    P R E C E D I N G;
PRECISION:                    P R E C I S I O N;
PRESENT:                      P R E S E N T;
PRIOR:                        P R I O R;
PROCEDURE:                    P R O C E D U R E;
RAISE:                        R A I S E;
RANGE:                        R A N G E;
RAW:                          R A W;
READ:                         R E A D;
REAL:                         R E A L;
RECORD:                       R E C O R D;
REF:                          R E F;
REFERENCE:                    R E F E R E N C E;
REFERENCING:                  R E F E R E N C I N G;
REJECT:                       R E J E C T;
RELIES_ON:                    R E L I E S '_' O N;
RENAME:                       R E N A M E;
REPLACE:                      R E P L A C E;
RESPECT:                      R E S P E C T;
RESTRICT_REFERENCES:          R E S T R I C T '_' R E F E R E N C E S;
RESULT:                       R E S U L T;
RESULT_CACHE:                 R E S U L T '_' C A C H E;
RETURN:                       R E T U R N;
RETURNING:                    R E T U R N I N G;
REUSE:                        R E U S E;
REVERSE:                      R E V E R S E;
REVOKE:                       R E V O K E;
RIGHT:                        R I G H T;
ROLLBACK:                     R O L L B A C K;
ROLLUP:                       R O L L U P;
ROW:                          R O W;
ROWID:                        R O W I D;
ROWS:                         R O W S;
RULES:                        R U L E S;
SAMPLE:                       S A M P L E;
SAVE:                         S A V E;
SAVEPOINT:                    S A V E P O I N T;
SCHEMA:                       S C H E M A;
SCHEMACHECK:                  S C H E M A C H E C K;
SCN:                          S C N;
SEARCH:                       S E A R C H;
SECOND:                       S E C O N D;
SEED:                         S E E D;
SEGMENT:                      S E G M E N T;
SELECT:                       S E L E C T;
SELF:                         S E L F;
SEQUENCE:                     S E Q U E N C E;
SEQUENTIAL:                   S E Q U E N T I A L;
SERIALIZABLE:                 S E R I A L I Z A B L E;
SERIALLY_REUSABLE:            S E R I A L L Y '_' R E U S A B L E;
SERVERERROR:                  S E R V E R E R R O R;
SESSIONTIMEZONE:              S E S S I O N T I M E Z O N E;
SET:                          S E T;
SETS:                         S E T S;
SETTINGS:                     S E T T I N G S;
SHARE:                        S H A R E;
SHOW:                         S H O W;
SHUTDOWN:                     S H U T D O W N;
SIBLINGS:                     S I B L I N G S;
SIGNTYPE:                     S I G N T Y P E;
SIMPLE_INTEGER:               S I M P L E '_' I N T E G E R;
SINGLE:                       S I N G L E;
SIZE:                         S I Z E;
SKIP_:                        S K I P;
SMALLINT:                     S M A L L I N T;
SNAPSHOT:                     S N A P S H O T;
SOME:                         S O M E;
SPECIFICATION:                S P E C I F I C A T I O N;
SQLDATA:                      S Q L D A T A;
SQLERROR:                     S Q L E R R O R;
STANDALONE:                   S T A N D A L O N E;
START:                        S T A R T;
STARTUP:                      S T A R T U P;
STATEMENT:                    S T A T E M E N T;
STATEMENT_ID:                 S T A T E M E N T '_' I D;
STATIC:                       S T A T I C;
STATISTICS:                   S T A T I S T I C S;
STRING:                       S T R I N G;
SUBMULTISET:                  S U B M U L T I S E T;
SUBPARTITION:                 S U B P A R T I T I O N;
SUBSTITUTABLE:                S U B S T I T U T A B L E;
SUBTYPE:                      S U B T Y P E;
SUCCESS:                      S U C C E S S;
SUSPEND:                      S U S P E N D;
TABLE:                        T A B L E;
THE:                          T H E;
THEN:                         T H E N;
TIME:                         T I M E;
TIMESTAMP:                    T I M E S T A M P;
TIMESTAMP_LTZ_UNCONSTRAINED:  T I M E S T A M P '_' L T Z '_' U N C O N S T R A I N E D;
TIMESTAMP_TZ_UNCONSTRAINED:   T I M E S T A M P '_' T Z '_' U N C O N S T R A I N E D;
TIMESTAMP_UNCONSTRAINED:      T I M E S T A M P '_' U N C O N S T R A I N E D;
TIMEZONE_ABBR:                T I M E Z O N E '_' A B B R;
TIMEZONE_HOUR:                T I M E Z O N E '_' H O U R;
TIMEZONE_MINUTE:              T I M E Z O N E '_' M I N U T E;
TIMEZONE_REGION:              T I M E Z O N E '_' R E G I O N;
TO:                           T O;
TRAILING:                     T R A I L I N G;
TRANSACTION:                  T R A N S A C T I O N;
TRANSLATE:                    T R A N S L A T E;
TREAT:                        T R E A T;
TRIGGER:                      T R I G G E R;
TRUE:                         T R U E;
TRUNCATE:                     T R U N C A T E;
TYPE:                         T Y P E;
UNBOUNDED:                    U N B O U N D E D;
UNDER:                        U N D E R;
UNION:                        U N I O N;
UNIQUE:                       U N I Q U E;
UNLIMITED:                    U N L I M I T E D;
UNPIVOT:                      U N P I V O T;
UNTIL:                        U N T I L;
UPDATE:                       U P D A T E;
UPDATED:                      U P D A T E D;
UPSERT:                       U P S E R T;
UROWID:                       U R O W I D;
USE:                          U S E;
USING:                        U S I N G;
VALIDATE:                     V A L I D A T E;
VALUE:                        V A L U E;
VALUES:                       V A L U E S;
VARCHAR:                      V A R C H A R;
VARCHAR2:                     V A R C H A R '2';
VARIABLE:                     V A R I A B L E;
VARRAY:                       V A R R A Y;
VARYING:                      V A R Y I N G;
VERSION:                      V E R S I O N;
VERSIONS:                     V E R S I O N S;
WAIT:                         W A I T;
WARNING:                      W A R N I N G;
WELLFORMED:                   W E L L F O R M E D;
WHEN:                         W H E N;
WHENEVER:                     W H E N E V E R;
WHERE:                        W H E R E;
WHILE:                        W H I L E;
WITH:                         W I T H;
WITHIN:                       W I T H I N;
WORK:                         W O R K;
WRITE:                        W R I T E;
XML:                          X M L;
XMLAGG:                       X M L A G G;
XMLATTRIBUTES:                X M L A T T R I B U T E S;
XMLCAST:                      X M L C A S T;
XMLCOLATTVAL:                 X M L C O L A T T V A L;
XMLELEMENT:                   X M L E L E M E N T;
XMLEXISTS:                    X M L E X I S T S;
XMLFOREST:                    X M L F O R E S T;
XMLNAMESPACES:                X M L N A M E S P A C E S;
XMLPARSE:                     X M L P A R S E;
XMLPI:                        X M L P I;
XMLQUERY:                     X M L Q U E R Y;
XMLROOT:                      X M L R O O T;
XMLSERIALIZE:                 X M L S E R I A L I Z E;
XMLTABLE:                     X M L T A B L E;
YEAR:                         Y E A R;
YES:                          Y E S;
YMINTERVAL_UNCONSTRAINED:     Y M I N T E R V A L '_' U N C O N S T R A I N E D;
ZONE:                         Z O N E;

PREDICTION:                   P R E D I C T I O N;
PREDICTION_BOUNDS:            P R E D I C T I O N '_' B O U N D S;
PREDICTION_COST:              P R E D I C T I O N '_' C O S T;
PREDICTION_DETAILS:           P R E D I C T I O N '_' D E T A I L S;
PREDICTION_PROBABILITY:       P R E D I C T I O N '_' P R O B A B I L I T Y;
PREDICTION_SET:               P R E D I C T I O N '_' S E T;
                              
CUME_DIST:                    C U M E '_' D I S T;
DENSE_RANK:                   D E N S E '_' R A N K;
LISTAGG:                      L I S T A G G;
PERCENT_RANK:                 P E R C E N T '_' R A N K;
PERCENTILE_CONT:              P E R C E N T I L E '_' C O N T;
PERCENTILE_DISC:              P E R C E N T I L E '_' D I S C;
RANK:                         R A N K;
                              
AVG:                          A V G;
CORR:                         C O R R;
COVAR_:                       C O V A R '_';
DECODE:                       D E C O D E;
LAG:                          L A G;
LEAD:                         L E A D;
MAX:                          M A X;
MEDIAN:                       M E D I A N;
MIN:                          M I N;
NTILE:                        N T I L E;
NVL:                          N V L;
RATIO_TO_REPORT:              R A T I O '_' T O '_' R  E P O R T;
REGR_:                        R E G R '_';
ROUND:                        R O U N D;
ROW_NUMBER:                   R O W '_' N U M B E R;
SUBSTR:                       S U B S T R;
TO_CHAR:                      T O '_' C H A R;
TRIM:                         T R I M;
SUM:                          S U M;
STDDEV:                       S T D D E V;
VAR_:                         V A R '_';
VARIANCE:                     V A R I A N C E;

// Rule #358 <NATIONAL_CHAR_STRING_LIT> - subtoken typecast in <REGULAR_ID>, it also incorporates <character_representation>
//  Lowercase 'n' is a usual addition to the standard
NATIONAL_CHAR_STRING_LIT: N '\'' (~('\'' | '\r' | '\n' ) | '\'' '\'' | NEWLINE)* '\'';

//  Rule #040 <BIT_STRING_LIT> - subtoken typecast in <REGULAR_ID>
//  Lowercase 'b' is a usual addition to the standard
BIT_STRING_LIT: B ('\'' ('0' | '1')* '\'' /*SEPARATOR?*/ )+;

//  Rule #284 <HEX_STRING_LIT> - subtoken typecast in <REGULAR_ID>
//  Lowercase 'x' is a usual addition to the standard
HEX_STRING_LIT: X ('\'' ('a'..'f' | 'A'..'F' | '0'..'9')* '\'' /*SEPARATOR?*/ )+;
DOUBLE_PERIOD: '..';
PERIOD:        '.';

//{ Rule #238 <EXACT_NUM_LIT>
//  This rule is a bit tricky - it resolves the ambiguity with <PERIOD> 
//  It also incorporates <mantisa> and <exponent> for the <APPROXIMATE_NUM_LIT>
//  Rule #501 <signed_integer> was incorporated directly in the token <APPROXIMATE_NUM_LIT>
//  See also the rule #617 <unsigned_num_lit>
/*
    : (
            UNSIGNED_INTEGER
            ( '.' UNSIGNED_INTEGER
            | {$type = UNSIGNED_INTEGER;}
            ) ( E ('+' | '-')? UNSIGNED_INTEGER {$type = APPROXIMATE_NUM_LIT;} )?
    | '.' UNSIGNED_INTEGER ( E ('+' | '-')? UNSIGNED_INTEGER {$type = APPROXIMATE_NUM_LIT;} )?
    )
    (D | F)?
    ;*/

UNSIGNED_INTEGER: UNSIGNED_INTEGER_FRAGMENT;
APPROXIMATE_NUM_LIT: FLOAT_FRAGMENT (('e'|'E') ('+'|'-')? (FLOAT_FRAGMENT | UNSIGNED_INTEGER_FRAGMENT))? (D | F)?;

// Rule #--- <CHAR_STRING> is a base for Rule #065 <char_string_lit> , it incorporates <character_representation>
// and a superfluous subtoken typecasting of the "QUOTE"
CHAR_STRING: '\'' (~('\'' | '\r' | '\n') | '\'' '\'' | NEWLINE)* '\'';

// Perl-style quoted string, see Oracle SQL reference, chapter String Literals
CHAR_STRING_PERL    : Q ( QS_ANGLE | QS_BRACE | QS_BRACK | QS_PAREN) -> type(CHAR_STRING);
fragment QUOTE      : '\'' ;
fragment QS_ANGLE   : QUOTE '<' .*? '>' QUOTE ;
fragment QS_BRACE   : QUOTE '{' .*? '}' QUOTE ;
fragment QS_BRACK   : QUOTE '[' .*? ']' QUOTE ;
fragment QS_PAREN   : QUOTE '(' .*? ')' QUOTE ;
fragment QS_OTHER_CH: ~('<' | '{' | '[' | '(' | ' ' | '\t' | '\n' | '\r');

// Rule #163 <DELIMITED_ID>
DELIMITED_ID: '"' (~('"' | '\r' | '\n') | '"' '"')+ '"' ;

// Rule #546 <SQL_SPECIAL_CHAR> was split into single rules
PERCENT: '%';
AMPERSAND: '&';
LEFT_PAREN: '(';
RIGHT_PAREN: ')';
DOUBLE_ASTERISK: '**';
ASTERISK: '*';
PLUS_SIGN: '+';
MINUS_SIGN: '-';
COMMA: ',';
SOLIDUS: '/';
AT_SIGN: '@';
ASSIGN_OP: ':=';
    
// See OCI reference for more information about this
BINDVAR
    : ':' SIMPLE_LETTER  (SIMPLE_LETTER | '0' .. '9' | '_')*
    | ':' DELIMITED_ID  // not used in SQL but spotted in v$sqltext when using cursor_sharing
    | ':' UNSIGNED_INTEGER
    | QUESTION_MARK // not in SQL, not in Oracle, not in OCI, use this for JDBC
    ;

COLON: ':';
SEMICOLON: ';';
LESS_THAN_OR_EQUALS_OP: '<=';
LESS_THAN_OP: '<';
GREATER_THAN_OR_EQUALS_OP: '>=';
NOT_EQUAL_OP: '!='| '<>'| '^='| '~=';
CARRET_OPERATOR_PART: '^';
TILDE_OPERATOR_PART: '~';
EXCLAMATION_OPERATOR_PART: '!';
GREATER_THAN_OP: '>';

fragment
QUESTION_MARK: '?';

// protected UNDERSCORE : '_' SEPARATOR ; // subtoken typecast within <INTRODUCER>
CONCATENATION_OP: '||';
VERTICAL_BAR: '|';
EQUALS_OP: '=';

// Rule #532 <SQL_EMBDD_LANGUAGE_CHAR> was split into single rules:
LEFT_BRACKET: '[';
RIGHT_BRACKET: ']';

//{ Rule #319 <INTRODUCER>
INTRODUCER
    : '_' //(SEPARATOR {$type = UNDERSCORE;})?
    ;

//{ Rule #479 <SEPARATOR>
//  It was originally a protected rule set to be filtered out but the <COMMENT> and <'-'> clashed. 
/*SEPARATOR
    : '-' -> type('-')
    | COMMENT -> channel(HIDDEN)
    | (SPACE | NEWLINE)+ -> channel(HIDDEN)
    ;*/
//}

SPACES: [ \t\r\n]+ -> skip;
    
//{ Rule #504 <SIMPLE_LETTER> - simple_latin _letter was generalised into SIMPLE_LETTER
//  Unicode is yet to be implemented - see NSF0
fragment
SIMPLE_LETTER
    : 'a'..'z'
    | 'A'..'Z'
    ;
//}

//  Rule #176 <DIGIT> was incorporated by <UNSIGNED_INTEGER> 
//{ Rule #615 <UNSIGNED_INTEGER> - subtoken typecast in <EXACT_NUM_LIT> 
fragment
UNSIGNED_INTEGER_FRAGMENT: ('0'..'9')+ ;

fragment
FLOAT_FRAGMENT
    : UNSIGNED_INTEGER* '.'? UNSIGNED_INTEGER+
    ;

//{ Rule #097 <COMMENT>
SINGLE_LINE_COMMENT: '--' ( ~('\r' | '\n') )* (NEWLINE|EOF) -> channel(HIDDEN);
MULTI_LINE_COMMENT: '/*' .*? '*/'                           -> channel(HIDDEN);

// SQL*Plus prompt
// TODO should be grammar rule, but tricky to implement
PROMPT
    : 'prompt' SPACE ( ~('\r' | '\n') )* (NEWLINE|EOF)
    ;

//{ Rule #360 <NEWLINE>
fragment
NEWLINE: '\r'? '\n';
    
fragment
SPACE: [ \t];

//{ Rule #442 <REGULAR_ID> additionally encapsulates a few STRING_LITs.
//  Within testLiterals all reserved and non-reserved words are being resolved

// PLSQL keywords:

/*PLSQL_NON_RESERVED_COLUMNS: 'columns';
CLUSTERS: 'clusters';
COLAUTH: 'colauth';
COMPRESS: 'compress';
PLSQL_NON_RESERVED_CONNECT_BY_ROOT: 'connect_by_root';
CRASH: 'crash';
EXCLUSIVE: 'exclusive';
IDENTIFIED: 'identified';
INDEX: 'index';
INDEXES: 'indexes';
LOCK: 'lock';
MINUS: 'minus';
MODE: 'mode';
NOCOMPRESS: 'nocompress';
NOWAIT: 'nowait';
RESOURCE: 'resource';
SHARE: 'share';*/

// SQL92 keywords:

/*ALL: 'all';
ALTER: 'alter';
AND: 'and';

ANY: 'any';
AS: 'as';
ASC: 'asc';
BEGIN: 'begin';
BETWEEN: 'between';
BY: 'by';
CASE: 'case';
CHECK: 'check';
CONNECT: 'connect';
CREATE: 'create';
CURRENT: 'current';
CURSOR: 'cursor';
DATE: 'date';
DECLARE: 'declare';
DEFAULT: 'default';
DELETE: 'delete';
DESC: 'desc';
DISTINCT: 'distinct';
DROP: 'drop';
ELSE: 'else';
END: 'end';
//TODO "exception" is a keyword only withing the contex of the PL/SQL language
//while it can be an identifier(column name, table name) in SQL
//"exception" is a keyword if and only it is followed by "when"
EXCEPTION: 'exception';
EXISTS: 'exists';
FALSE: 'false';
FETCH: 'fetch';
FOR: 'for';
FROM: 'from';
GOTO: 'goto';
GRANT: 'grant';
GROUP: 'group';
HAVING: 'having';
IN: 'in';
INSERT: 'insert';
INTERSECT: 'intersect';
INTO: 'into';
IS: 'is';
LIKE: 'like';
NOT: 'not';
NULL: 'null';
OF: 'of';
ON: 'on';
OPTION: 'option';
OR: 'or';
ORDER: 'order';
OVERLAPS: 'overlaps';
PRIOR: 'prior';
PROCEDURE: 'procedure';
PUBLIC: 'public';
REVOKE: 'revoke';
SELECT: 'select';
SIZE: 'size';
START: 'start';
TABAUTH: 'tabauth';
TABLE: 'table';
THE: 'the';
THEN: 'then';
TO: 'to';
TRUE: 'true';
UNION: 'union';
UNIQUE: 'unique';
UPDATE: 'update';
VALUES: 'values';
VIEW: 'view';
VIEWS: 'views';
WHEN: 'when';
WHERE: 'where';
WITH: 'with';
USING: 'using';*/

REGULAR_ID: SIMPLE_LETTER (SIMPLE_LETTER | '$' | '_' | '#' | '0'..'9')*;
ZV: '@!' -> channel(HIDDEN);

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