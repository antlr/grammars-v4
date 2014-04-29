lexer grammar ScssLexer;

NULL              : 'null';

// Whitespace -- ignored
fragment Whitespace
  : (' '|'\t'|'\n'|'\r'|'\r\n')+
  ;

WS
  : Whitespace -> skip
  ;

IN              : 'in';

fragment MeasurementUnit
  : ('%'|'px'|'cm'|'mm'|'in'|'pt'|'pc'|'em'|'ex'|'deg'|'rad'|'grad'|'ms'|'s'|'hz'|'khz')
  ;


Unit: MeasurementUnit;

/*
ArgumentsStart
  : '(' -> pushMode(ARGS_STARTED)
  ;
 */
COMBINE_COMPARE : '&&' | '||';


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


EQEQ            : '==';
NOTEQ           : '!=';



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
AT_EACH         : '@each';



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

/*
mode ARGS_STARTED;

UrlArgStart       : 'url' '(' -> pushMode(URL_STARTED);
ArgumentNumber    : Number -> type(Number);
ArgumentUnit      : MeasurementUnit -> type(Unit);
Ellipsis          : '...';
ParamIdentifier   : Identifier -> type(Identifier);
Color_Arg         : Color -> type(Color);
StringLiteral_Arg : StringLiteral -> type(StringLiteral);
UrlStart_Arg      : UrlStart -> type(UrlStart);



DOLLAR_Arg        : DOLLAR -> type(DOLLAR);
MathChar          : '/' | '+' | '-' | '*' | '%';
VAR_VALUE_START   : COLON -> type(COLON);
VAR_VALUE_SEPER   : COMMA -> type(COMMA);
ArgumentsEnd      : RPAREN -> popMode;
ArgumentsReStart  : '(' -> pushMode(ARGS_STARTED), type(ArgumentsStart);
ARGS_WS           : Whitespace -> skip;

HASH_Args         : HASH -> type(HASH);
BlockStart_ARGS   : BlockStart -> type(BlockStart);
BlockEnd_ARGS     : BlockEnd -> type(BlockEnd);
 */


mode URL_STARTED;
UrlEnd                 : RPAREN -> popMode;
Url                     :	STRING | (~(')' | '\n' | '\r' | ';'))+;



