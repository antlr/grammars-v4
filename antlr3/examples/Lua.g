/*
 * Lua 5.1 grammar
 * 
 * Nicolai Mainiero
 * May 2007
 * 
 * This is a Lua (http://www.lua.org) grammar for the version 5.1 for ANTLR 3.
 * I tested it with basic and extended examples and it worked fine. It is also used
 * for LunarEclipse (http://lunareclipse.sf.net) a Lua editor based on Eclipse.
 * 
 * Thanks to Johannes Luber and Gavin Lambert who helped me with some mutually left recursion.
 *  
 */

grammar Lua;

options {
  backtrack=true;
}

chunk : (stat (';')?)* (laststat (';')?)?;

block : chunk;

stat :  varlist1 '=' explist1 | 
	functioncall | 
	'do' block 'end' | 
	'while' exp 'do' block 'end' | 
	'repeat' block 'until' exp | 
	'if' exp 'then' block ('elseif' exp 'then' block)* ('else' block)? 'end' | 
	'for' NAME '=' exp ',' exp (',' exp)? 'do' block 'end' | 
	'for' namelist 'in' explist1 'do' block 'end' | 
	'function' funcname funcbody | 
	'local' 'function' NAME funcbody | 
	'local' namelist ('=' explist1)? ;

laststat : 'return' (explist1)? | 'break';

funcname : NAME ('.' NAME)* (':' NAME)? ;

varlist1 : var (',' var)*;


namelist : NAME (',' NAME)*;

explist1 : (exp ',')* exp;

exp :  ('nil' | 'false' | 'true' | number | string | '...' | function | prefixexp | tableconstructor | unop exp) (binop exp)* ;

var: (NAME | '(' exp ')' varSuffix) varSuffix*;

prefixexp: varOrExp nameAndArgs*;

functioncall: varOrExp nameAndArgs+;

/*
var :  NAME | prefixexp '[' exp ']' | prefixexp '.' NAME; 

prefixexp : var | functioncall | '(' exp ')';

functioncall :  prefixexp args | prefixexp ':' NAME args ;
*/

varOrExp: var | '(' exp ')';

nameAndArgs: (':' NAME)? args;

varSuffix: nameAndArgs* ('[' exp ']' | '.' NAME);

args :  '(' (explist1)? ')' | tableconstructor | string ;

function : 'function' funcbody;

funcbody : '(' (parlist1)? ')' block 'end';

parlist1 : namelist (',' '...')? | '...';

tableconstructor : '{' (fieldlist)? '}';

fieldlist : field (fieldsep field)* (fieldsep)?;

field : '[' exp ']' '=' exp | NAME '=' exp | exp;

fieldsep : ',' | ';';

binop : '+' | '-' | '*' | '/' | '^' | '%' | '..' | 
		 '<' | '<=' | '>' | '>=' | '==' | '~=' | 
		 'and' | 'or';

unop : '-' | 'not' | '#';

number : INT | FLOAT | EXP | HEX;

string	: NORMALSTRING | CHARSTRING | LONGSTRING;


// LEXER

NAME	:('a'..'z'|'A'..'Z'|'_')(options{greedy=true;}:	'a'..'z'|'A'..'Z'|'_'|'0'..'9')*
	;

INT	: ('0'..'9')+;

FLOAT 	:INT '.' INT ;

EXP	: (INT| FLOAT) ('E'|'e') ('-')? INT;

HEX	:'0x' ('0'..'9'| 'a'..'f')+ ;

	

NORMALSTRING
    :  '"' ( EscapeSequence | ~('\\'|'"') )* '"' 
    ;

CHARSTRING
   :	'\'' ( EscapeSequence | ~('\''|'\\') )* '\''
   ;

LONGSTRING
	:	'['('=')*'[' ( EscapeSequence | ~('\\'|']') )* ']'('=')*']'
	;

fragment
EscapeSequence
    :   '\\' ('b'|'t'|'n'|'f'|'r'|'\"'|'\''|'\\')
    |   UnicodeEscape
    |   OctalEscape
    ;
    
fragment
OctalEscape
    :   '\\' ('0'..'3') ('0'..'7') ('0'..'7')
    |   '\\' ('0'..'7') ('0'..'7')
    |   '\\' ('0'..'7')
    ;
    
fragment
UnicodeEscape
    :   '\\' 'u' HexDigit HexDigit HexDigit HexDigit
    ;
    
fragment
HexDigit : ('0'..'9'|'a'..'f'|'A'..'F') ;


COMMENT
    :   '--[[' ( options {greedy=false;} : . )* ']]' {skip();}
    ;
    
LINE_COMMENT
    : '--' ~('\n'|'\r')* '\r'? '\n' {skip();}
    ;
    
    
WS  :  (' '|'\t'|'\u000C') {skip();}
    ;
    
NEWLINE	: ('\r')? '\n' {skip();}
	;
