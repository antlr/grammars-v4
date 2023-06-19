lexer grammar ArdenLexer;
options { caseInsensitive = true; }

// Keywords
ABS:            'abs';
ACTION:         'action:';
ADD:            'add';
AFTER:          'after';
AGGREGATE:      'aggregate';
AGO:            'ago';
ALL:            'all';
AND:            'and';
ANY:            'any';
APPLICABILITY:  'applicability';
ARCCOS:         'arccos';
ARCSIN:         'arcsin';
ARCTAN:         'arctan';
ARDEN:          'arden:';
ARDEN_VERSION:  'version ' ('2' ('.' ( '1' | '2' | '5' | '6' | '7' | '8' | '9' | '10'))? | '3');
ARETRUE:        'aretrue';
ARGUMENT:       'argument';
AS:             'as';
AT:             'at';
ATTIME:         'attime';
ATTRIBUTE:      'attribute';
AUTHOR:         'author:' -> mode(TextMode);
AVERAGE:        'average' | 'avg';
BE:             'be';
BEFORE:         'before';
BOOLEAN:        'boolean';
BREAKLOOP:      'breakloop';
BY:             'by';
CALL:           'call';
CASE:           'case';
CEILING:        'ceiling';
CHARACTERS:     'characters';
CITATIONS:      'citations:';
CLONE:          'clone';
CONCLUDE:       'conclude';
COSINE:         'cos' 'ine'?;
COUNT:          'count';
CRISP:          'crisp';
CURRENTTIME:    'currenttime';
DATA:           'data:';
DATAWC:         'data';
DATE:           'date:';
DAY:            'day' 's'?;
DECREASE:       'decrease';
DEFAULT:        'default';
DEFAULTCO:      'default:' -> mode(TwoCharMode);
DEFUZZIFIED:    'defuzzified';
DELAY:          'delay';
DESTINATION:    'destination';
DO:             'do';
DURATION:       'duration';
EARLIEST:       'earliest';
ELEMENTS:       'elements';
ELSE:           'else';
ELSEIF:         'elseif';
END:            'end:';
ENDDO:          'enddo';
ENDIF:          'endif';
ENDSWITCH:      'endswitch';
EQUAL:          'equal';
EVENT:          'event';
EVENTTIME:      'eventtime';
EVERY:          'every';
EVOKE:          'evoke:';
EXIST:          'exist' 's'?;
EXP:            'exp';
EXPLANATION:    'explanation:' -> mode(TextMode);
EXTRACT:        'extract';
FALSE:          'false';
FILENAME:       'filename:' -> mode(MlmName);
FIND:           'find';
FIRST:          'first';
FLOOR:          'floor';
FOLLOWING:      'following';
FOR:            'for';
FORMATTED:      'formatted';
FROM:           'from';
FUZZIFIED:      'fuzzified';
FUZZY:          'fuzzy';
GREATER:        'greater';
HOUR:           'hour' 's'?;
IF:             'if';
IN:             'in';
INCLUDE:        'include';
INCREASE:       'increase';
INDEX:          'index';
INSTITUTION:    'institution:' -> mode(TextMode);
INSTITUTIONWC:  'institution';
INT:            'int';
INTERFACE:      'interface';
INTERVAL:       'interval';
IS:             'is' | 'are' | 'was' | 'were';
ISTRUE:         'istrue';
IT:             'it' | 'they';
KEYWORDS:       'keywords' -> mode(TextMode);
KNOWLEDGE:      'knowledge:';
LANGUAGE:       'language:' -> mode(TwoCharMode);
LAST:           'last';
LATEST:         'latest';
LEAST:          'least';
LEFT:           'left';
LENGTH:         'length';
LESS:           'less';
LET:            'let';
LIBRARY:        'library:';
LINGUISTIC:     'linguistic';
LINKS:          'links:';
LINK_TYPE:      'url_link' | 'mesh_link' | 'other_link' | 'exe_link';
LIST:           'list';
LOCALIZED:      'localized';
LOG10:          'log10';
LOG:            'log';
LOGIC:          'logic:';
LOWERCASE:      'lowercase';
MAINTENANCE:    'maintenance:';
MATCHES:        'matches';
MAXIMUM:        'max' 'imum'?;
MEDIAN:         'median';
MERGE:          'merge';
MESSAGE:        'message';
MINIMUM:        'min' 'imum'?;
MINUTE:         'minute' 's'?;
MLM:            'mlm';
MLMNAME:        'mlmname:' -> mode(MlmName);
MLM_SELF:       'mlm'[_-]'self';
MONTH:          'month' 's'?;
MOST:           'most';
NAMES:          'names';
NEAREST:        'nearest';
NEW:            'new';
NO:             'no';
NOT:            'not';
NOW:            'now';
NULL:           'null';
NUMBEROP:       'number';
OBJECT:         'object';
OCCUR:          'occur' ('s' | 'red')?;
OF:             'of';
OR:             'or';
PAST:           'past';
PATTERN:        'pattern';
PERCENT:        'percent' | '%';
PRECEDING:      'preceding';
PRESENT:        'present';
PRIORITY:       'priority:';
PURPOSE:        'purpose:' -> mode(TextMode);
READ:           'read';
REMOVE:         'remove';
REPLACE:        'replace';
RESOURCES:      'resources:';
RETURN:         'return';
REVERSE:        'reverse';
RIGHT:          'right';
ROUND:          'round';
SAME:           'same';
SECOND:         'second' 's'?;
SEQTO:          'seqto';
SET:            'set';
SINE:           'sin' 'e'?;
SLOPE:          'slope';
SORT:           'sort';
SPECIALIST:     'specialist:' -> mode(TextMode);
SQRT:           'sqrt';
STARTING:       'starting';
STDDEV:         'stddev';
STRINGOP:       'string';
SUBLIST:        'sublist';
SUBSTRING:      'substring';
SUM:            'sum';
SURROUNDING:    'surrounding';
SWITCH:         'switch';
TANGENT:        'tan' 'gent'?;
THAN:           'than';
THE:            'the' -> channel(HIDDEN);
THEN:           'then';
TIME:           'time';
TITLE:          'title:' -> mode(TextMode);
TO:             'to';
TODAY:          'today';
TOMORROW:       'tomorrow';
TRIGGERTIME:    'triggertime';
TRIM:           'trim';
TRUE:           'true';
TRUNCATE:       'truncate';
TRUTHVALUE:     'truth value';
TYPE:           'type:';
TYPE_CODE:      'data'[_-]'driven';
UNTIL:          'until';
UPPERCASE:      'uppercase';
URGENCY:        'urgency:';
USING:          'using';
VALIDATION:     'validation:';
VALIDATION_CODE:'production' | 'research' | 'testing' | 'expired';
VARIABLE:       'variable';
VARIANCE:       'variance';
VERSION:        'version:' -> mode(TextMode);
WEEK:           'week' 's'?;
WEEKDAYLITERAL: 'sunday' | 'monday' | 'tuesday' | 'wednesday' | 'thursday' | 'friday' | 'saturday';
WHERE:          'where';
WHILE:          'while';
WITH:           'with';
WITHIN:         'within';
WRITE:          'write';
YEAR:           'year' 's'?;

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
EQ:             '=' | 'eq';
LT:             '<' | 'lt';
GT:             '>' | 'gt';
LE:             '<=' | 'le';
GE:             '>=' | 'ge';
NE:             '<>' | 'ne';
DOR:            '||';

// Digit constructs
NUMBER
    :           Digit+ DOT? Digit* Exponent
    |           DOT Digit+ Exponent
    ;

// Date constructs
TIMEOFDAY:      Digit Digit COLON Digit Digit Seconds TimeZone;

ISO_DATE:       Digit Digit Digit Digit MINUS Digit Digit MINUS Digit Digit;

ISO_DATE_TIME:  Digit Digit Digit Digit MINUS Digit Digit MINUS Digit Digit 't' Digit Digit COLON Digit Digit COLON Digit Digit FractionalDigit TimeZone;

// String constructs
CITATION:       Digit+ DOT ' ' ('support' | 'refute')?;

TERM:           '\''  .*? '\'';

STRING:         '"' ( '""' | ~'"' )* '"';

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
    :           ('e' [+-]? Digit+)?
    ;

// Date fragments
fragment Seconds
    :           (COLON Digit Digit FractionalDigit)?
    ;

fragment TimeZone
    :           'z'?
    |           (PLUS | MINUS) Digit Digit COLON Digit Digit
    ;

// String fragments
fragment Letter
    :           [a-z]
    ;

// Start of the identifier 10 chars, starting with a letter
fragment StartIdentifier
    :           Letter (ID (ID (ID (ID (ID (ID (ID (ID (ID?)?)?)?)?)?)?)?)?)?
    ;

// Rest of the identifier 10 chars
fragment RestIdentifier
    :           (ID (ID (ID (ID (ID (ID (ID (ID(ID (ID?)?)?)?)?)?)?)?)?)?)?
    ;

fragment ID
    :           Letter | [0-9] | '_'
    ;

fragment MlmID
    :           ID | DOT | MINUS
    ;

// Lexer modes
mode TextMode;
TEXT:           .+? ';;' -> mode(DEFAULT_MODE);

mode DataMapping;
DATA_MAPPING:   (~'}' | '\\' .)+ -> mode(DEFAULT_MODE);

mode MlmName;
MLMID:          MlmIDStart MlmIDRest MlmIDRest MlmIDRest MlmIDRest MlmIDRest MlmIDRest MlmIDRest -> mode(DEFAULT_MODE);

WS_ID:          [ \t\r\n]+ -> channel(HIDDEN);

fragment MlmIDStart
    :           Letter (MlmID (MlmID (MlmID (MlmID (MlmID (MlmID (MlmID (MlmID (MlmID?)?)?)?)?)?)?)?)?)?
    ;
fragment MlmIDRest
    :           (MlmID (MlmID (MlmID (MlmID (MlmID (MlmID (MlmID (MlmID (MlmID (MlmID?)?)?)?)?)?)?)?)?)?)?
    ;

mode TwoCharMode;
TWOCHARCODE    options {caseInsensitive = false; }: Letter Letter ('_' [A-Z] [A-Z])? -> mode(DEFAULT_MODE);

WS_TCM:         WS -> channel(HIDDEN);