grammar PICT;

options { caseInsensitive = true; }

/// https://github.com/Microsoft/pict/blob/main/doc/pict.md
input
 : parameter_definitions sub_models constraints EOF
 ;

parameter_definitions
 : parameter_definition*
 ;

parameter_definition
 : parameter COLON parameter_values
 ;

parameter
 : VALUE+
 ;

parameter_values
 : parameter_value ( COMMA parameter_value )*
 ;

parameter_value
 : parameter_value OPAR parameter_value CPAR
 | TILDE parameter_value
 | VALUE+ ( PIPE VALUE+ )?
 | LT VALUE+ GT
 | NUMBER
 ;

sub_models
 : sub_model*
 ;

sub_model
 : OBRACE parameter ( COMMA parameter )* CBRACE AT NUMBER
 ;

/// Constraints   :: =
///    Constraint
///  | Constraint Constraints
constraints
 : constraint*
 ;

/// Constraint    :: =
///    IF Predicate THEN Predicate ELSE Predicate;
///  | Predicate;
constraint
 : IF predicate THEN predicate ( ELSE predicate )? SCOLON
 | predicate SCOLON
 ;

/// Predicate     :: =
///    Clause
///  | Clause LogicalOperator Predicate
predicate
 : clause ( logical_operator predicate )?
 ;

/// Clause :: =
///    Term
///  | ( Predicate )
///  | NOT Predicate
clause
 : term
 | OPAR predicate CPAR
 | NOT predicate
 ;

/// Term :: =
///    ParameterName Relation Value
///  | ParameterName LIKE PatternString
///  | ParameterName IN { ValueSet }
///  | ParameterName Relation ParameterName
term
 : parameter_name relation value
 | parameter_name LIKE STRING
 | parameter_name IN OBRACE value_set CBRACE
 | parameter_name relation parameter_name
 ;

/// ValueSet :: =
///    Value
///  | Value, ValueSet
value_set
 : value ( COMMA value )*
 ;

/// LogicalOperator ::=
///    AND
///  | OR
logical_operator
 : AND
 | OR
 ;

/// Relation :: =
///    =
///  | <>
///  | >
///  | >=
///  | <
///  | <=
relation
 : EQ
 | NEQ
 | GT
 | GTE
 | LT
 | LTE
 ;

/// ParameterName ::= [String]
parameter_name
 : OBRACK VALUE+ CBRACK
 ;

/// Value :: =
///    "String"
///  | Number
value
 : STRING
 | NUMBER
 ;

IF     : 'IF';
THEN   : 'THEN';
ELSE   : 'ELSE';
NOT    : 'NOT';
AND    : 'AND';
OR     : 'OR';
LIKE   : 'LIKE';
IN     : 'IN';

EQ     : '=';
NEQ    : '<>';
LT     : '<';
LTE    : '<=';
GT     : '>';
GTE    : '>=';
OBRACK : '[';
CBRACK : ']';
OPAR   : '(';
CPAR   : ')';
OBRACE : '{';
CBRACE : '}';
AT     : '@';
COLON  : ':';
SCOLON : ';';
COMMA  : ',';
PIPE   : '|';
TILDE  : '~';

/// String :: = whatever is typically regarded as a string of characters
///
/// PatternString ::= string with embedded special characters (wildcards):
///                   * a series of characters of any length (can be zero)
///                   ? any one character
STRING
 : '"' .*? '"'
 ;

/// Number :: = whatever is typically regarded as a number
NUMBER
 : '-'? [0-9]+ ( '.' [0-9]+ )?
 ;

VALUE
 : ~[#: \t\r\n,=<>[\]()";{}@|~]+
 ;

COMMENT
 : '#' ~[\r\n]* -> skip
 ;

SPACES
 : [ \t\r\n]+ -> skip
 ;

OTHER
 : .
 ;