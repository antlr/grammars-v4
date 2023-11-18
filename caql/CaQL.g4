grammar CaQL;

start_: expression EOF;

expression: vectorOperation;

vectorOperation
    : <assoc=right> vectorOperation (powOp vectorOperation | subqueryOp)
    | unaryOp vectorOperation
    | vectorOperation multOp vectorOperation
    | vectorOperation addOp vectorOperation
    | vectorOperation compareOp vectorOperation
    | vectorOperation andUnlessOp vectorOperation
    | vectorOperation orOp vectorOperation
    | vectorOperation vectorMatchOp vectorOperation
    | name LEFT_BRACE vectorOperation (COMMA vectorOperation)*  RIGHT_BRACE
    | LEFT_PAREN vectorOperation RIGHT_PAREN
    | name LEFT_PAREN (STRING|NUMBER|UUID) (COMMA LIMIT? (STRING|NUMBER|UUID))* RIGHT_PAREN metricsAggregation*
    | vectorOperation metricsAggregation
    | NUMBER
    | STRING
    | UUID
    ;

// Operators


unaryOp:        ADD | SUB;
powOp:          POW grouping?;
multOp:         (MULT | DIV | MOD) grouping?;
addOp:          (ADD | SUB) grouping?;
compareOp:      (DEQ | NE | GT | LT | GE | LE) BOOL? grouping?;
andUnlessOp:    (AND | UNLESS) grouping?;
orOp:           OR grouping?;
vectorMatchOp:  (ON | UNLESS) grouping?;
subqueryOp:     SUBQUERY_RANGE offsetOp?;
offsetOp:       OFFSET DURATION;

// Functions

parameter:     literal | vectorOperation;
parameterList: LEFT_PAREN (parameter (COMMA parameter)*)? RIGHT_PAREN;
metricsAggregation: LINE_ name parameterList;
multMetrics: COMMA vectorOperation;
by:      BY labelNameList;
without: WITHOUT labelNameList;

// Vector one-to-one/one-to-many joins

grouping:   (on_ | ignoring) (groupLeft | groupRight)?;
on_:         ON labelNameList;
ignoring:   IGNORING labelNameList;
groupLeft:  GROUP_LEFT labelNameList?;
groupRight: GROUP_RIGHT labelNameList?;

// Label names

name:     keyword | NAME;

labelNameList: LEFT_PAREN (name (COMMA name)*)? RIGHT_PAREN;

keyword
    : AND
    | OR
    | UNLESS
    | BY
    | WITHOUT
    | ON
    | IGNORING
    | GROUP_LEFT
    | GROUP_RIGHT
    | OFFSET
    | BOOL
    ;

literal: NUMBER | STRING;

fragment NUMERAL: [0-9]+ ('.' [0-9]+)?;

fragment SCIENTIFIC_NUMBER
   : NUMERAL ('e' ('-' | '+')? NUMERAL)?
   ;

NUMBER: NUMERAL | SCIENTIFIC_NUMBER;
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

LEFT_BRACE:  '{';
RIGHT_BRACE: '}';
LEFT_PAREN:  '(';
RIGHT_PAREN: ')';
LEFT_BRACKET:  '[';
RIGHT_BRACKET: ']';
COMMA: ',';
LINE_: '|';
COLON: ':';

SUBQUERY_RANGE : LEFT_BRACKET DURATION ':' DURATION? RIGHT_BRACKET;
TIME_RANGE : LEFT_BRACKET DURATION RIGHT_BRACKET;
DURATION: [0-9]+ ('s' | 'm' | 'h' | 'd' | 'w' | 'y');
NAME: [a-z_:] [a-z0-9_:]*;
LIMIT: 'limit=';
UUID: Time_low '-' Time_mid '-'	Time_high_and_version '-' Clock_seq_and_reserved Clock_seq_low '-' Node	;
fragment Time_low: FourHexOctet;
fragment Time_mid: TwoHexOctet;
fragment Time_high_and_version: TwoHexOctet;
fragment Clock_seq_and_reserved: HexOctet;
fragment Clock_seq_low: HexOctet;
fragment Node: SixHexOctet;
fragment HexOctet: HexDigit HexDigit;
fragment HexDigit: [0-9a-fA-F];
fragment SixHexOctet: HexDigit HexDigit HexDigit HexDigit HexDigit HexDigit HexDigit HexDigit HexDigit HexDigit HexDigit HexDigit;
fragment FourHexOctet: HexDigit HexDigit HexDigit HexDigit HexDigit HexDigit HexDigit HexDigit;
fragment TwoHexOctet: HexDigit HexDigit HexDigit HexDigit ;

WS: [\r\t\n ]+ -> channel(HIDDEN);
