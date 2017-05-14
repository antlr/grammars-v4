grammar css3;

stylesheet
	: (namespace | charset | importRule | nested | ruleset)+ 
	;
	
namespace
	: '@namespace' IDENT* STRING 
	;
	
charset
	:'@charset' STRING 
	;
	
importRule
	: ('@import' | '@include')  (STRING|URI) (IDENT|',' IDENT)* 
	;

nested
 	: '@' nest '{' properties? nested* '}' 
	;

nest
	: IDENT IDENT* pseudo* 
	;
	
ruleset
 	: selectors '{' properties* '}'
	;
	
selectors
	: selector (',' selector)*
	;
	
selector
	: elem selectorOperation* attrib* pseudo? 
	;

selectorOperation
	: selectop? elem
	;

selectop
	: '>' 
        | '+'  
	;

properties
	: declaration (';' declaration?)*
	;
	
elem
	:(IDENT)+ 
	|(idSelector)+ 
	|(classSelector)+ 
	;

idSelector
	:'#' IDENT
	;
	
classSelector
	:'.' IDENT
	;

pseudo
	: (':'|'::') IDENT
	| (':'|'::') function 
	;

attrib
	: '[' IDENT (attribRelate (STRING | IDENT))? ']' 
	;
	
attribRelate
	: '=' 
	| '~='
	| '|=' 
	;	
  
declaration
	: IDENT ':' args 
	;

args
	: expr (','? expr)*
	;

expr
	: (NUM unit?)
	| IDENT
	| COLOR
	| STRING
	| function
	;

unit
	: ('%'|'px'|'cm'|'mm'|'in'|'pt'|'pc'|'em'|'ex'|'deg'|'rad'|'grad'|'ms'|'s'|'hz'|'khz')
	;
	
function
	: IDENT '(' args? ')'
	;

IDENT
	:	('_' | 'a'..'z'| 'A'..'Z' | '\u0100'..'\ufffe' ) 
		('_' | '-' | 'a'..'z'| 'A'..'Z' | '\u0100'..'\ufffe' | '0'..'9')*
	|	'-' ('_' | 'a'..'z'| 'A'..'Z' | '\u0100'..'\ufffe' ) 
		('_' | '-' | 'a'..'z'| 'A'..'Z' | '\u0100'..'\ufffe' | '0'..'9')*
	;

STRING
	:	'"' (~('"'|'\n'|'\r'))* '"'
	|	'\'' (~('\''|'\n'|'\r'))* '\''
	;

NUM
	:	'-' (('0'..'9')* '.')? ('0'..'9')+
	|	(('0'..'9')* '.')? ('0'..'9')+
	;

COLOR
	: '#' ('0'..'9'|'a'..'f'|'A'..'F')+
	;

SL_COMMENT
	: '//' ~[\r\n]* -> skip
	;
	
COMMENT
	: '/*' .* '*/' ->skip
	;

WS	: ( ' ' | '\t' | '\r' | '\n' | '\f' )+ ->skip
	;
	
URI 	: 'url(' (~('\n'|'\r'))* ')'? 
	;
SEMICOLON : ';' -> skip;
