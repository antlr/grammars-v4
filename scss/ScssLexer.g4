lexer grammar ScssLexer;

NULL              : 'null';

// Whitespace -- ignored
fragment Whitespace
  : (' '|'\t'|'\n'|'\r'|'\r\n')+
  ;

WS
  : Whitespace -> skip
  ;


fragment MeasurementUnit
  : ('%'|'px'|'cm'|'mm'|'in'|'pt'|'pc'|'em'|'ex'|'deg'|'rad'|'grad'|'ms'|'s'|'hz'|'khz')
  ;

Unit: MeasurementUnit;

ArgumentsStart
  : '(' -> pushMode(ARGS_STARTED)
  ;

//Separators
LPAREN          : '(';
RPAREN          : ')';
BlockStart      : '{';
BlockEnd        : '}';
LBRACK          : '[';
RBRACK          : ']';
GT              : '>';
TIL             : '~';

LT              : '<';
COLON           : ':';
SEMI            : ';';
COMMA           : ',';
DOT             : '.';
DOLLAR          : '$';
AT              : '@';
AND             : '&';
HASH            : '#';
COLONCOLON      : '::';
PLUS            : '+';
TIMES           : '*';
DIV             : '/';
MINUS           : '-';
PERC            : '%';



COMPARISON      : '==' | LT | GT | '!=';
COMBINE_COMPARE : '&&' | '||';



EQ              : '=';
PIPE_EQ         : '|=';
TILD_EQ         : '~=';



MIXIN           : '@mixin';
FUNCTION        : '@function';
AT_ELSE         : '@else';
IF              : 'if';
AT_IF           : '@if';
AT_FOR          : '@for';
AT_WHILE        : '@while';

INCLUDE         : '@include';
IMPORT          : '@import';
RETURN          : '@return';
NEW_LINE        : '\n';
CHAR_FEED       : '\r';

FROM            : 'from';
THROUGH         : 'through';
POUND_DEFAULT   : '!default';







Identifier
	:	('_' | 'a'..'z'| 'A'..'Z' | '\u0100'..'\ufffe' )
		('_' | '-' | 'a'..'z'| 'A'..'Z' | '\u0100'..'\ufffe' | '0'..'9')*
	|	'-' ('_' | 'a'..'z'| 'A'..'Z' | '\u0100'..'\ufffe' )
		('_' | '-' | 'a'..'z'| 'A'..'Z' | '\u0100'..'\ufffe' | '0'..'9')*
	;



fragment STRING
  	:	'"' (~('"'|'\n'|'\r'))* '"'
  	|	'\'' (~('\''|'\n'|'\r'))* '\''
  	;

// string literals
StringLiteral
	:	STRING
	;

Number
	:	'-' (('0'..'9')* '.')? ('0'..'9')+
	|	(('0'..'9')* '.')? ('0'..'9')+
	;

Color
	:	'#' ('0'..'9'|'a'..'f'|'A'..'F')+
	;


UrlStart
  : 'url' '(' -> pushMode(URL_STARTED)
  ;



// Single-line comments
SL_COMMENT
	:	'//'
		(~('\n'|'\r'))* ('\n'|'\r'('\n')?) -> skip
	;


// multiple-line comments
COMMENT
	:	'/*' .*? '*/' -> skip
	;

mode ARGS_STARTED;

UrlArgStart       : 'url' '(' -> pushMode(URL_STARTED);
ArgumentNumber    : Number;
ArgumentUnit      : MeasurementUnit;
Ellipsis          : '...';
ParamName         : '$' Identifier;

Expression        : Identifier | Color | StringLiteral | UrlStart;

MathChar          : '/' | '+' | '-' | '*' | '%';
VAR_VALUE_START   : ':';
VAR_VALUE_SEPER   : ',';
ArgumentsEnd      : RPAREN -> popMode;
ArgumentsReStart  : '(' -> pushMode(ARGS_STARTED);
ARGS_WS           : Whitespace -> skip;

InterpolateStart  : '#' '{';
InterpolateEnd    : '}';



mode URL_STARTED;
UrlEnd                 : RPAREN -> popMode;
Url                     :	STRING | (~(')' | '\n' | '\r' | ';'))+;



