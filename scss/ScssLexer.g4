lexer grammar ScssLexer;

NULL              : 'null';

// Whitespace -- ignored
WS
  : (' '|'\t'|'\n'|'\r'|'\r\n')+ -> skip
  ;

//Separators
LPAREN          : '(';
RPAREN          : ')';
LBRACE          : '{';
RBRACE          : '}';
LBRACK          : '[';
RBRACK          : ']';
GT              : '>';
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


//MATH
PLUS            : '+';
MINUS           : '-';
MULT            : '*';
DIV             : '/';
PERC            : '%';


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
ELIPSIS         : '...';
NEW_LINE        : '\n';
CHAR_FEED       : '\r';

FROM            : 'from';
THROUGH         : 'through';


UNIT
  : ('%'|'px'|'cm'|'mm'|'in'|'pt'|'pc'|'em'|'ex'|'deg'|'rad'|'grad'|'ms'|'s'|'hz'|'khz')
  ;




Identifier
	:	('_' | 'a'..'z'| 'A'..'Z' | '\u0100'..'\ufffe' )
		('_' | '-' | 'a'..'z'| 'A'..'Z' | '\u0100'..'\ufffe' | '0'..'9')*
	|	'-' ('_' | 'a'..'z'| 'A'..'Z' | '\u0100'..'\ufffe' )
		('_' | '-' | 'a'..'z'| 'A'..'Z' | '\u0100'..'\ufffe' | '0'..'9')*
	;

MATH_CHAR         :  '*' | '+' | '/' | '-';
COMPARISON        : '==' | '<' | '>' | '!=';
COMBINE_COMPARE   : '&&' | '||';

fragment STRING
  	:	'"' (~('"'|'\n'|'\r'))* '"'
  	|	'\'' (~('\''|'\n'|'\r'))* '\''
  	;

// string literals
StringLiteral
	:	STRING
	;

NUM
	:	'-' (('0'..'9')* '.')? ('0'..'9')+
	|	(('0'..'9')* '.')? ('0'..'9')+
	;

COLOR
	:	'#' ('0'..'9'|'a'..'f'|'A'..'F')+
	;



ARGS_START
  : '(' -> pushMode(ARGS_STARTED)
  ;

URL_START
  : 'url' LPAREN -> pushMode(URL_STARTED)
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
VAR_START         : '$';
VariableName      : Identifier;
Expr              : NUM UNIT? | Identifier | COLOR | StringLiteral;
VAR_VALUE_START   : ':';
VAR_VALUE_SEPER   : ',';
ARGS_END          : ')' -> popMode;


mode URL_STARTED;
URL_END                 : ')' -> popMode;
Url                     :	STRING | (~(')' | '\n' | '\r' | ';'))+;



