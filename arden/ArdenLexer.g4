lexer grammar ArdenLexer;

// Keywords
ABS:            [Aa][Bb][Ss];
ACTION:         [Aa][Cc][Tt][Ii][Oo][Nn][:];
ADD:            [Aa][Dd][Dd];
AFTER:          [Aa][Ff][Tt][Ee][Rr];
AGGREGATE:      [Aa][Gg][Gg][Rr][Ee][Gg][Aa][Tt][Ee];
AGO:            [Aa][Gg][Oo];
ALL:            [Aa][Ll][Ll];
AND:            [Aa][Nn][Dd];
ANY:            [Aa][Nn][Yy];
APPLICABILITY:  [Aa][Pp][Pp][Ll][Ii][Cc][Aa][Bb][Ii][Ll][Ii][Tt][Yy];
ARCCOS:         [Aa][Rr][Cc][Cc][Oo][Ss];
ARCSIN:         [Aa][Rr][Cc][Ss][Ii][Nn];
ARCTAN:         [Aa][Rr][Cc][Tt][Aa][Nn];
ARDEN:          [Aa][Rr][Dd][Ee][Nn][:];
ARDEN_VERSION:  [Vv][Ee][Rr][Ss][Ii][Oo][Nn][ ](('2' (DOT ( '1' | '2' | '5' | '6' | '7' | '8' | '9' | '10'))?) | [3]);
ARETRUE:        [Aa][Rr][Ee][Tt][Rr][Uu][Ee];
ARGUMENT:       [Aa][Rr][Gg][Uu][Mm][Ee][Nn][Tt];
AS:             [Aa][Ss];
AT:             [Aa][Tt];
ATTIME:         [Aa][Tt][Tt][Ii][Mm][Ee];
ATTRIBUTE:      [Aa][Tt][Tt][Rr][Ii][Bb][Uu][Tt][Ee];
AUTHOR:         [Aa][Uu][Tt][Hh][Oo][Rr][:] -> mode(TextMode);
AVERAGE:        ([Aa][Vv][Ee][Rr][Aa][Gg][Ee] | [Aa][Vv][Gg]);
BE:             [Bb][Ee];
BEFORE:         [Bb][Ee][Ff][Oo][Rr][Ee];
BOOLEAN:        [Bb][Oo][Oo][Ll][Ee][Aa][Nn];
BREAKLOOP:      [Bb][Rr][Ee][Aa][Kk][Ll][Oo][Oo][Pp];
BY:             [Bb][Yy];
CALL:           [Cc][Aa][Ll][Ll];
CASE:           [Cc][Aa][Ss][Ee];
CEILING:        [Cc][Ee][Ii][Ll][Ii][Nn][Gg];
CHARACTERS:     [Cc][Hh][Aa][Rr][Aa][Cc][Tt][Ee][Rr][Ss];
CITATIONS:      [Cc][Ii][Tt][Aa][Tt][Ii][Oo][Nn][Ss][:];
CLONE:          [Cc][Ll][Oo][Nn][Ee];
CONCLUDE:       [Cc][Oo][Nn][Cc][Ll][Uu][Dd][Ee];
COSINE:         ([Cc][Oo][Ss][Ii][Nn][Ee] | [Cc][Oo][Ss]);
COUNT:          [Cc][Oo][Uu][Nn][Tt];
CRISP:          [Cc][Rr][Ii][Ss][Pp];
CURRENTTIME:    [Cc][Uu][Rr][Rr][Ee][Nn][Tt][Tt][Ii][Mm][Ee];
DATA:           [Dd][Aa][Tt][Aa][:];
DATAWC:         [Dd][Aa][Tt][Aa];
DATE:           [Dd][Aa][Tt][Ee][:];
DAY:            ([Dd][Aa][Yy] | [Dd][Aa][Yy][Ss]);
DECREASE:       [Dd][Ee][Cc][Rr][Ee][Aa][Ss][Ee];
DEFAULT:        [Dd][Ee][Ff][Aa][Uu][Ll][Tt];
DEFAULTCO:      [Dd][Ee][Ff][Aa][Uu][Ll][Tt][:] -> mode(TwoCharMode);
DEFUZZIFIED:    [Dd][Ee][Ff][Uu][Zz][Zz][Ii][Ff][Ii][Ee][Dd];
DELAY:          [Dd][Ee][Ll][Aa][Yy];
DESTINATION:    [Dd][Ee][Ss][Tt][Ii][Nn][Aa][Tt][Ii][Oo][Nn];
DO:             [Dd][Oo];
DURATION:       [Dd][Uu][Rr][Aa][Tt][Ii][Oo][Nn];
EARLIEST:       [Ee][Aa][Rr][Ll][Ii][Ee][Ss][Tt];
ELEMENTS:       [Ee][Ll][Ee][Mm][Ee][Nn][Tt][Ss];
ELSE:           [Ee][Ll][Ss][Ee];
ELSEIF:         [Ee][Ll][Ss][Ee][Ii][Ff];
END:            [Ee][Nn][Dd][:];
ENDDO:          [Ee][Nn][Dd][Dd][Oo];
ENDIF:          [Ee][Nn][Dd][Ii][Ff];
ENDSWITCH:      [Ee][Nn][Dd][Ss][Ww][Ii][Tt][Cc][Hh];
EQUAL:          [Ee][Qq][Uu][Aa][Ll];
EVENT:          [Ee][Vv][Ee][Nn][Tt];
EVENTTIME:      [Ee][Vv][Ee][Nn][Tt][Tt][Ii][Mm][Ee];
EVERY:          [Ee][Vv][Ee][Rr][Yy];
EVOKE:          [Ee][Vv][Oo][Kk][Ee][:];
EXIST:          ([Ee][Xx][Ii][Ss][Tt] | [Ee][Xx][Ii][Ss][Tt][Ss]);
EXP:            [Ee][Xx][Pp];
EXPLANATION:    [Ee][Xx][Pp][Ll][Aa][Nn][Aa][Tt][Ii][Oo][Nn][:] -> mode(TextMode);
EXTRACT:        [Ee][Xx][Tt][Rr][Aa][Cc][Tt];
FALSE:          [Ff][Aa][Ll][Ss][Ee];
FILENAME:       [Ff][Ii][Ll][Ee][Nn][Aa][Mm][Ee][:] -> mode(MlmName);
FIND:           [Ff][Ii][Nn][Dd];
FIRST:          [Ff][Ii][Rr][Ss][Tt];
FLOOR:          [Ff][Ll][Oo][Oo][Rr];
FOLLOWING:      [Ff][Oo][Ll][Ll][Oo][Ww][Ii][Nn][Gg];
FOR:            [Ff][Oo][Rr];
FORMATTED:      [Ff][Oo][Rr][Mm][Aa][Tt][Tt][Ee][Dd];
FROM:           [Ff][Rr][Oo][Mm];
FUZZIFIED:      [Ff][Uu][Zz][Zz][Ii][Ff][Ii][Ee][Dd];
FUZZY:          [Ff][Uu][Zz][Zz][Yy];
GREATER:        [Gg][Rr][Ee][Aa][Tt][Ee][Rr];
HOUR:           ([Hh][Oo][Uu][Rr] | [Hh][Oo][Uu][Rr][Ss]);
IF:             [Ii][Ff];
IN:             [Ii][Nn];
INCLUDE:        [Ii][Nn][Cc][Ll][Uu][Dd][Ee];
INCREASE:       [Ii][Nn][Cc][Rr][Ee][Aa][Ss][Ee];
INDEX:          [Ii][Nn][Dd][Ee][Xx];
INSTITUTION:    [Ii][Nn][Ss][Tt][Ii][Tt][Uu][Tt][Ii][Oo][Nn][:] -> mode(TextMode);
INSTITUTIONWC:  [Ii][Nn][Ss][Tt][Ii][Tt][Uu][Tt][Ii][Oo][Nn];
INT:            [Ii][Nn][Tt];
INTERFACE:      [Ii][Nn][Tt][Ee][Rr][Ff][Aa][Cc][Ee];
INTERVAL:       [Ii][Nn][Tt][Ee][Rr][Vv][Aa][Ll];
IS:             ([Ii][Ss] | [Aa][Rr][Ee] | [Ww][Aa][Ss] | [Ww][Ee][Rr][Ee]);
ISTRUE:         [Ii][Ss][Tt][Rr][Uu][Ee];
IT:             ([Ii][Tt] | [Tt][Hh][Ee][Yy]);
KEYWORDS:       [Kk][Ee][Yy][Ww][Oo][Rr][Dd][Ss][:] -> mode(TextMode);
KNOWLEDGE:      [Kk][Nn][Oo][Ww][Ll][Ee][Dd][Gg][Ee][:];
LANGUAGE:       [Ll][Aa][Nn][Gg][Uu][Aa][Gg][Ee][:] -> mode(TwoCharMode);
LAST:           [Ll][Aa][Ss][Tt];
LATEST:         [Ll][Aa][Tt][Ee][Ss][Tt];
LEAST:          [Ll][Ee][Aa][Ss][Tt];
LEFT:           [Ll][Ee][Ff][Tt];
LENGTH:         [Ll][Ee][Nn][Gg][Tt][Hh];
LESS:           [Ll][Ee][Ss][Ss];
LET:            [Ll][Ee][Tt];
LIBRARY:        [Ll][Ii][Bb][Rr][Aa][Rr][Yy][:];
LINGUISTIC:     [Ll][Ii][Nn][Gg][Uu][Ii][Ss][Tt][Ii][Cc];
LINKS:          [Ll][Ii][Nn][Kk][Ss][:];
LINK_TYPE:      ([Uu][Rr][Ll] | [Mm][Ee][Ss][Hh] | [Oo][Tt][Hh][Ee][Rr] | [Ee][Xx][Ee]);
LIST:           [Ll][Ii][Ss][Tt];
LOCALIZED:      [Ll][Oo][Cc][Aa][Ll][Ii][Zz][Ee][Dd];
LOG10:          [Ll][Oo][Gg][1][0];
LOG:            [Ll][Oo][Gg];
LOGIC:          [Ll][Oo][Gg][Ii][Cc][:];
LOWERCASE:      [Ll][Oo][Ww][Ee][Rr][Cc][Aa][Ss][Ee];
MAINTENANCE:    [Mm][Aa][Ii][Nn][Tt][Ee][Nn][Aa][Nn][Cc][Ee][:];
MATCHES:        [Mm][Aa][Tt][Cc][Hh][Ee][Ss];
MAXIMUM:        ([Mm][Aa][Xx][Ii][Mm][Uu][Mm] | [Mm][Aa][Xx]);
MEDIAN:         [Mm][Ee][Dd][Ii][Aa][Nn];
MERGE:          [Mm][Ee][Rr][Gg][Ee];
MESSAGE:        [Mm][Ee][Ss][Ss][Aa][Gg][Ee];
MINIMUM:        ([Mm][Ii][Nn][Ii][Mm][Uu][Mm] | [Mm][Ii][Nn]);
MINUTE:         ([Mm][Ii][Nn][Uu][Tt][Ee] | [Mm][Ii][Nn][Uu][Tt][Ee][Ss]);
MLM:            [Mm][Ll][Mm];
MLMNAME:        [Mm][Ll][Mm][Nn][Aa][Mm][Ee][:] -> mode(MlmName);
MLM_SELF:       [Mm][Ll][Mm][_-][Ss][Ee][Ll][Ff];
MONTH:          ([Mm][Oo][Nn][Tt][Hh] | [Mm][Oo][Nn][Tt][Hh][Ss]);
MOST:           [Mm][Oo][Ss][Tt];
NAMES:          [Nn][Aa][Mm][Ee][Ss];
NEAREST:        [Nn][Ee][Aa][Rr][Ee][Ss][Tt];
NEW:            [Nn][Ee][Ww];
NO:             [Nn][Oo];
NOT:            [Nn][Oo][Tt];
NOW:            [Nn][Oo][Ww];
NULL:           [Nn][Uu][Ll][Ll];
NUMBEROP:       [Nn][Uu][Mm][Bb][Ee][Rr];
OBJECT:         [Oo][Bb][Jj][Ee][Cc][Tt];
OCCUR:          ([Oo][Cc][Cc][Uu][Rr] | [Oo][Cc][Cc][Uu][Rr][Ss] | [Oo][Cc][Cc][Uu][Rr][Rr][Ee][Dd]);
OF:             [Oo][Ff];
OR:             [Oo][Rr];
PAST:           [Pp][Aa][Ss][Tt];
PATTERN:        [Pp][Aa][Tt][Tt][Ee][Rr][Nn];
PERCENT:        ([Pp][Ee][Rr][Cc][Ee][Nn][Tt] | '%');
PRECEDING:      [Pp][Rr][Ee][Cc][Ee][Dd][Ii][Nn][Gg];
PRESENT:        [Pp][Rr][Ee][Ss][Ee][Nn][Tt];
PRIORITY:       [Pp][Rr][Ii][Oo][Rr][Ii][Tt][Yy][:];
PURPOSE:        [Pp][Uu][Rr][Pp][Oo][Ss][Ee][:] -> mode(TextMode);
READ:           [Rr][Ee][Aa][Dd];
REMOVE:         [Rr][Ee][Mm][Oo][Vv][Ee];
REPLACE:        [Rr][Ee][Pp][Ll][Aa][Cc][Ee];
RESOURCES:      [Rr][Ee][Ss][Oo][Uu][Rr][Cc][Ee][Ss][:];
RETURN:         [Rr][Ee][Tt][Uu][Rr][Nn];
REVERSE:        [Rr][Ee][Vv][Ee][Rr][Ss][Ee];
RIGHT:          [Rr][Ii][Gg][Hh][Tt];
ROUND:          [Rr][Oo][Uu][Nn][Dd];
SAME:           [Ss][Aa][Mm][Ee];
SECOND:         ([Ss][Ee][Cc][Oo][Nn][Dd] | [Ss][Ee][Cc][Oo][Nn][Dd][Ss]);
SEQTO:          [Ss][Ee][Qq][Tt][Oo];
SET:            [Ss][Ee][Tt];
SINE:           ([Ss][Ii][Nn][Ee] | [Ss][Ii][Nn]);
SLOPE:          [Ss][Ll][Oo][Pp][Ee];
SORT:           [Ss][Oo][Rr][Tt];
SPECIALIST:     [Ss][Pp][Ee][Cc][Ii][Aa][Ll][Ii][Ss][Tt][:] -> mode(TextMode);
SQRT:           [Ss][Qq][Rr][Tt];
STARTING:       [Ss][Tt][Aa][Rr][Tt][Ii][Nn][Gg];
STDDEV:         [Ss][Tt][Dd][Dd][Ee][Vv];
STRINGOP:       [Ss][Tt][Rr][Ii][Nn][Gg];
SUBLIST:        [Ss][Uu][Bb][Ll][Ii][Ss][Tt];
SUBSTRING:      [Ss][Uu][Bb][Ss][Tt][Rr][Ii][Nn][Gg];
SUM:            [Ss][Uu][Mm];
SURROUNDING:    [Ss][Uu][Rr][Rr][Oo][Uu][Nn][Dd][Ii][Nn][Gg];
SWITCH:         [Ss][Ww][Ii][Tt][Cc][Hh];
TANGENT:        ([Tt][Aa][Nn][Gg][Ee][Nn][Tt] | [Tt][Aa][Nn]);
THAN:           [Tt][Hh][Aa][Nn];
THE:            [Tt][Hh][Ee] -> channel(HIDDEN);
THEN:           [Tt][Hh][Ee][Nn];
TIME:           [Tt][Ii][Mm][Ee];
TITLE:          [Tt][Ii][Tt][Ll][Ee][:] -> mode(TextMode);
TO:             [Tt][Oo];
TODAY:          [Tt][Oo][Dd][Aa][Yy];
TOMORROW:       [Tt][Oo][Mm][Oo][Rr][Rr][Oo][Ww];
TRIGGERTIME:    [Tt][Rr][Ii][Gg][Gg][Ee][Rr][Tt][Ii][Mm][Ee];
TRIM:           [Tt][Rr][Ii][Mm];
TRUE:           [Tt][Rr][Uu][Ee];
TRUNCATE:       [Tt][Rr][Uu][Nn][Cc][Aa][Tt][Ee];
TRUTHVALUE:     [Tt][Rr][Uu][Tt][Hh][ ][Vv][Aa][Ll][Uu][Ee];
TYPE:           [Tt][Yy][Pp][Ee][:];
TYPE_CODE:      [Dd][Aa][Tt][Aa][_-][Dd][Rr][Ii][Vv][Ee][Nn];
UNTIL:          [Uu][Nn][Tt][Ii][Ll];
UPPERCASE:      [Uu][Pp][Pp][Ee][Rr][Cc][Aa][Ss][Ee];
URGENCY:        [Uu][Rr][Gg][Ee][Nn][Cc][Yy][:];
USING:          [Uu][Ss][Ii][Nn][Gg];
VALIDATION:     [Vv][Aa][Ll][Ii][Dd][Aa][Tt][Ii][Oo][Nn][:];
VALIDATION_CODE:([Pp][Rr][Oo][Dd][Uu][Cc][Tt][Ii][Oo][Nn] | [Rr][Ee][Ss][Ee][Aa][Rr][Cc][Hh] | [Tt][Ee][Ss][Tt][Ii][Nn][Gg] | [Ee][Xx][Pp][Ii][Rr][Ee][Dd]);
VARIABLE:       [Vv][Aa][Rr][Ii][Aa][Bb][Ll][Ee];
VARIANCE:       [Vv][Aa][Rr][Ii][Aa][Nn][Cc][Ee];
VERSION:        [Vv][Ee][Rr][Ss][Ii][Oo][Nn][:] -> mode(TextMode);
WEEK:           ([Ww][Ee][Ee][Kk] | [Ww][Ee][Ee][Kk][Ss]);
WEEKDAYLITERAL: ([Ss][Uu][Nn][Dd][Aa][Yy] | [Mm][Oo][Nn][Dd][Aa][Yy] | [Tt][Uu][Ee][Ss][Dd][Aa][Yy] | [Ww][Ee][Dd][Nn][Ee][Ss][Dd][Aa][Yy] | [Tt][Hh][Uu][Rr][Ss][Dd][Aa][Yy] | [Ff][Rr][Ii][Dd][Aa][Yy] | [Ss][Aa][Tt][Uu][Rr][Dd][Aa][Yy]);
WHERE:          [Ww][Hh][Ee][Rr][Ee];
WHILE:          [Ww][Hh][Ii][Ll][Ee];
WITH:           [Ww][Ii][Tt][Hh];
WITHFS:         [Ww][Ii][Tt][Hh];
WITHIN:         [Ww][Ii][Tt][Hh][Ii][Nn];
WRITE:          [Ww][Rr][Ii][Tt][Ee];
YEAR:           ([Yy][Ee][Aa][Rr] | [Yy][Ee][Aa][Rr][Ss]);

// Seperators
LPAREN:         '(';
RPAREN:         ')';
LBRACE:         '{' -> mode(DataMapping);
RBRACE:         '}';
LBRACK:         '[';
RBRACK:         ']';
SC:             ';';
DSC:            ';;';
COLON:          ':';
DOT:            '.';
COMMA:          ',';

// Operators
ASSIGN:         ':=';
PLUS:           '+';
MINUS:          '-';
MUL:            '*';
DIV:            '/';
POWER:          '**';
EQ:             ('=' | [Ee][Qq]);
LT:             ('<' | [Ll][Tt]);
GT:             ('>' | [Gg][Tt]);
LE:             ('<=' | [Ll][Ee]);
GE:             ('>=' | [Gg][Ee]);
NE:             ('<>' | [Nn][Ee]);
DOR:            '||';

// Digit constructs
NUMBER
    :           Digit+ DOT? (Digit+)? Exponent
    |           DOT Digit+ Exponent
    ;

// Date constructs
TIMEOFDAY:      Digit Digit COLON Digit Digit Seconds TimeZone;

ISO_DATE:       Digit Digit Digit Digit MINUS Digit Digit MINUS Digit Digit;

ISO_DATE_TIME:  Digit Digit Digit Digit MINUS Digit Digit MINUS Digit Digit ('T' | 't') Digit Digit COLON Digit Digit COLON Digit Digit FractionalDigit TimeZone;

// String constructs
CITATION:       Digit+ DOT ' ' ([Ss][Uu][Pp][Pp][Oo][Rr][Tt] | [Rr][Ee][Ff][Uu][Tt][Ee])?;

TERM:           '\'' ( .)*? '\'';

STRING:         '"' ( '""' | ~["] )* '"';

// Creates an identifier with max of 80 chars
IDENTIFIER
    :           StartIdentifier RestIdentifier RestIdentifier RestIdentifier RestIdentifier RestIdentifier RestIdentifier RestIdentifier
    ;

// Comment
COMMENT:        '/*' .*? '*/' -> channel(HIDDEN);

LINE_COMMENT:   '//' ~[\r\n]* -> channel(HIDDEN);

// Whitespace
WS:             [ \t\r\n]+ -> channel(HIDDEN);

// Fragment rules
// Digit fragments
fragment Digit
    :           [0-9]
    ;

fragment FractionalDigit
    :          (DOT Digit+)?
    ;

fragment Exponent
    :           ([Ee] [+-]? Digit+)?
    ;

// Date fragments
fragment Seconds
    :           (COLON Digit Digit FractionalDigit)?
    ;

fragment TimeZone
    :           ('Z' | 'z')?
    |           (PLUS | MINUS) Digit Digit COLON Digit Digit
    ;

// String fragments
fragment Letter
    :           [a-zA-Z]
    ;

fragment LittleLetter
    :           [a-z]
    ;

fragment BigLetter
    :           [A-Z]
    ;

// Start of the identifier 10 chars, starting with a letter
fragment StartIdentifier
    :           Letter ID? ID? ID? ID? ID? ID? ID? ID? ID?
    ;

// Rest of the identifier 10 chars
fragment RestIdentifier
    :           ID? ID? ID? ID? ID? ID? ID? ID? ID? ID?
    ;

fragment ID
    :           (Letter | [0-9] | '_' )
    ;

fragment MlmID
    :           (ID | DOT | MINUS)
    ;

// Lexer modes
mode TextMode;
TEXT:           .+? ';;' -> mode(DEFAULT_MODE);

mode DataMapping;
DATA_MAPPING:   (~[}] | '\\' (.))+ -> mode(DEFAULT_MODE);

mode MlmName;
MLMID:          MlmIDStart MlmIDRest MlmIDRest MlmIDRest MlmIDRest MlmIDRest MlmIDRest MlmIDRest -> mode(DEFAULT_MODE);

WS_ID:          ([ \t\r\n]+) -> channel(HIDDEN);

fragment MlmIDStart
    :           Letter MlmID? MlmID? MlmID? MlmID? MlmID? MlmID? MlmID? MlmID? MlmID?
    ;
fragment MlmIDRest
    :           MlmID? MlmID? MlmID? MlmID? MlmID? MlmID? MlmID? MlmID? MlmID? MlmID?
    ;

mode TwoCharMode;
TWOCHARCODE:    LittleLetter LittleLetter ('_' BigLetter BigLetter)? -> mode(DEFAULT_MODE);

WS_TCM:         WS -> channel(HIDDEN);




