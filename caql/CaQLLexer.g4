lexer grammar CaQLLexer;

fragment NUMERAL: [0-9]+ ('.' [0-9]+)?;

fragment SCIENTIFIC_NUMBER
   : NUMERAL ('e' ('-' | '+')? NUMERAL)?
   ;

NUMBER
    : NUMERAL
    | SCIENTIFIC_NUMBER;
STRING
    : '\'' (~('\'' | '\\') | '\\' .)* '\''
    | '"' (~('"' | '\\') | '\\' .)* '"'
    ;

// Binary operators

ADD:  '+';
SUB:  '-';
MULT: '*';
DIV:  '/';
MOD:  '%';
POW:  '^';

AND:    'and';
OR:     'or';
UNLESS: 'unless';

// Comparison operators

EQ:  '=';
DEQ: '==';
NE:  '!=';
GT:  '>';
LT:  '<';
GE:  '>=';
LE:  '<=';
RE:  '=~';
NRE: '!~';

// Aggregation modifiers

BY:      'by';
WITHOUT: 'without';

// Join modifiers

ON:          'on';
IGNORING:    'ignoring';
GROUP_LEFT:  'group_left';
GROUP_RIGHT: 'group_right';

OFFSET: 'offset';

BOOL: 'bool';
AGGREGATION_OPERATOR
    : 'stats:sum'
    | 'label'
    | 'top'
    | 'count'
    | 'stats:max'
    | 'stats:percentile'
    | 'histogram'
    | 'histogram:percentile'
    ;

FUNCTION
    : 'find'
    | 'find:counter'
    | 'find:average'
    | 'op:prod'
    | 'op:div'
    ;

LEFT_BRACE:  '{';
RIGHT_BRACE: '}';

LEFT_PAREN:  '(';
RIGHT_PAREN: ')';

LEFT_BRACKET:  '[';
RIGHT_BRACKET: ']';

COMMA: ',';

LINE_: '|';

COLON: ':';


SUBQUERY_RANGE
     : LEFT_BRACKET DURATION ':' DURATION? RIGHT_BRACKET;

TIME_RANGE
    : LEFT_BRACKET DURATION RIGHT_BRACKET;

DURATION: [0-9]+ ('s' | 'm' | 'h' | 'd' | 'w' | 'y');

METRIC_NAME: [a-z_:] [a-z0-9_:]*;
LABEL_NAME:  [a-z_] [a-z0-9_]*;
WS: [\r\t\n ]+ -> skip;
LIMIT: 'limit=';
