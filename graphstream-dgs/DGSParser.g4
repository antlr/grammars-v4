/*
 * Graphstream DGS grammar.
 *
 * Adapted from http://graphstream-project.org/doc/Advanced-Concepts/The-DGS-File-Format/
 *
 */

parser grammar DGSParser;

options { tokenVocab=DGSLexer; }

dgs : header ( event | COMMENT | EOL )* ;
header : MAGIC EOL identifier INT INT EOL;
event : ( an | cn | dn | ae | ce | de | cg | st | cl ) ( COMMENT | EOL ) ;

an : AN identifier attributes;
cn : CN identifier attributes;
dn : DN identifier;
ae : AE identifier identifier direction? identifier attributes;
ce : CE identifier attributes;
de : DE identifier;
cg : CG attributes;
st : ST REAL;
cl : CL;


attributes : attribute*;
attribute : (PLUS|MINUS)? identifier ( assign value ( COMMA value )* )? ;

value : STRING | INT| REAL | COLOR | array | a_map | identifier;
// 'string' and #334455 colors
// identifier

array : LBRACE ( value ( COMMA value )* )? RBRACE;
a_map : LBRACK ( mapping ( COMMA mapping )* )? RBRACK;
mapping : identifier assign value;
direction : LANGLE | RANGLE ;
assign : EQUALS | COLON ;
identifier : STRING | INT | WORD ( DOT WORD )* ;

