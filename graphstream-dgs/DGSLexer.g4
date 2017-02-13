/*
 * Graphstream DGS grammar.
 *
 * Adapted from http://graphstream-project.org/doc/Advanced-Concepts/The-DGS-File-Format/
 *
 */

lexer grammar DGSLexer;

MAGIC : 'DGS004' | 'DGS003';

AN : 'an';
CN : 'cn';
DN : 'dn';
AE : 'ae';
CE : 'ce';
DE : 'de';
CG : 'cg';
ST : 'st';
CL : 'cl';

INT : ('+'|'-')? ( '0' | ( [1-9] ([0-9])* ) );
REAL : INT ( '.' [0-9]*)? ( [Ee] ('+'|'-')? [0-9]*[1-9] )?;

PLUS : '+';
MINUS : '-';
COMMA : ',';
LBRACE : '{';
RBRACE : '}';
LBRACK : '[';
RBRACK : ']';
DOT : '.';
LANGLE : '<';
RANGLE : '>';
EQUALS : '=';
COLON : ':';

EOL : '\r'|'\n'|'\r\n';
WORD : ( 'a'..'z' | 'A'..'Z' ) ( 'a'..'z' | 'A'..'Z' | '0'..'9' | '-' | '_' )* ;
STRING : SQSTRING | DQSTRING;
fragment DQSTRING : '"' (DQESC|.)*? '"';
fragment DQESC : '\\"' | '\\\\' ;
fragment SQSTRING : '\'' (SQESC|.)*? '\'';
fragment SQESC : '\\\'' | '\\\\' ;

fragment HEXBYTE : ([0-9a-fA-F]) ([0-9a-fA-F]) ;
COLOR : '#' HEXBYTE HEXBYTE HEXBYTE HEXBYTE? ;

START_COMMENT : '#' -> pushMode(CMT), skip;

WS : [ \t]+ -> skip;

// identifier : STRING | INT | WORD ( '.' WORD )* ;

mode CMT;

COMMENT: .*? EOL -> popMode;

