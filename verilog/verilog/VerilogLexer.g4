/*
MIT License

Copyright (c) 2022 Mustafa Said Ağca

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

lexer grammar VerilogLexer;
channels { COMMENTS, DIRECTIVES }

ALWAYS : 'always' ;
AM : '&' ;
AMAM : '&&' ;
AMAMAM : '&&&' ;
AND : 'and' ;
AP : '\'' ;
AS : '*' ;
ASAS : '**' ;
ASGT : '*>' ;
ASSIGN : 'assign' ;
ASSL : '*/' ;
AT : '@' ;
AUTOMATIC : 'automatic' ;
BEGIN : 'begin' ;
BUF : 'buf' ;
BUFIFONE : 'bufif1' ;
BUFIFZERO : 'bufif0' ;
CA : '^' ;
CASE : 'case' ;
CASEX : 'casex' ;
CASEZ : 'casez' ;
CATI : '^~' ;
CELL : 'cell' ;
CL : ':' ;
CMOS : 'cmos' ;
CO : ',' ;
CONFIG : 'config' ;
DEASSIGN : 'deassign' ;
DEFAULT : 'default' ;
DEFPARAM : 'defparam' ;
DESIGN : 'design' ;
DISABLE : 'disable' ;
DL : '$' ;
DLFULLSKEW : '$fullskew' ;
DLHOLD : '$hold' ;
DLNOCHANGE : '$nochange' ;
DLPERIOD : '$period' ;
DLRECOVERY : '$recovery' ;
DLRECREM : '$recrem' ;
DLREMOVAL : '$removal' ;
DLSETUP : '$setup' ;
DLSETUPHOLD : '$setuphold' ;
DLSKEW : '$skew' ;
DLTIMESKEW : '$timeskew' ;
DLWIDTH : '$width' ;
DQ : '"' ;
DT : '.' ;
EDGE : 'edge' -> mode(EDGE_MODE) ;
ELSE : 'else' ;
EM : '!' ;
EMEQ : '!=' ;
EMEQEQ : '!==' ;
END : 'end' ;
ENDCASE : 'endcase' ;
ENDCONFIG : 'endconfig' ;
ENDFUNCTION : 'endfunction' ;
ENDGENERATE : 'endgenerate' ;
ENDMODULE : 'endmodule' ;
ENDPRIMITIVE : 'endprimitive' ;
ENDSPECIFY : 'endspecify' ;
ENDTABLE : 'endtable' ;
ENDTASK : 'endtask' ;
EQ : '=' ;
EQEQ : '==' ;
EQEQEQ : '===' ;
EQGT : '=>' ;
EVENT : 'event' ;
FOR : 'for' ;
FORCE : 'force' ;
FOREVER : 'forever' ;
FORK : 'fork' ;
FUNCTION : 'function' ;
GA : '`' -> channel(DIRECTIVES), pushMode(DIRECTIVE_MODE) ;
GENERATE : 'generate' ;
GENVAR : 'genvar' ;
GT : '>' ;
GTEQ : '>=' ;
GTGT : '>>' ;
GTGTGT : '>>>' ;
HA : '#' ;
HIGHZONE : 'highz1' ;
HIGHZZERO : 'highz0' ;
IF : 'if' ;
IFNONE : 'ifnone' ;
INCLUDE : 'include' -> mode(LIBRARY_MODE) ;
INITIAL : 'initial' ;
INOUT : 'inout' ;
INPUT : 'input' ;
INSTANCE : 'instance' ;
INTEGER : 'integer' ;
JOIN : 'join' ;
LARGE : 'large' ;
LB : '[' ;
LC : '{' ;
LIBLIST : 'liblist' ;
LIBRARY : 'library' -> mode(LIBRARY_MODE) ;
LOCALPARAM : 'localparam' ;
LP : '(' ;
LT : '<' ;
LTEQ : '<=' ;
LTLT : '<<' ;
LTLTLT : '<<<' ;
MACROMODULE : 'macromodule' ;
MEDIUM : 'medium' ;
MI : '-' ;
MICL : '-:' ;
MIGT : '->' ;
MIINCDIR : '-incdir' ;
MO : '%' ;
MODULE : 'module' ;
NAND : 'nand' ;
NEGEDGE : 'negedge' ;
NMOS : 'nmos' ;
NOR : 'nor' ;
NOSHOWCANCELLED : 'noshowcancelled' ;
NOT : 'not' ;
NOTIFONE : 'notif1' ;
NOTIFZERO : 'notif0' ;
OR : 'or' ;
OUTPUT : 'output' ;
PARAMETER : 'parameter' ;
PATHPULSEDL : 'PATHPULSE$' ;
PL : '+' ;
PLCL : '+:' ;
PMOS : 'pmos' ;
POSEDGE : 'posedge' ;
PRIMITIVE : 'primitive' ;
PULLDOWN : 'pulldown' ;
PULLONE : 'pull1' ;
PULLUP : 'pullup' ;
PULLZERO : 'pull0' ;
PULSESTYLE_ONDETECT : 'pulsestyle_ondetect' ;
PULSESTYLE_ONEVENT : 'pulsestyle_onevent' ;
QM : '?' ;
RB : ']' ;
RC : '}' ;
RCMOS : 'rcmos' ;
REAL : 'real' ;
REALTIME : 'realtime' ;
REG : 'reg' ;
RELEASE : 'release' ;
REPEAT : 'repeat' ;
RNMOS : 'rnmos' ;
RP : ')' ;
RPMOS : 'rpmos' ;
RTRAN : 'rtran' ;
RTRANIFONE : 'rtranif1' ;
RTRANIFZERO : 'rtranif0' ;
SC : ';' ;
SCALARED : 'scalared' ;
SHOWCANCELLED : 'showcancelled' ;
SIGNED : 'signed' ;
SL : '/' ;
SLAS : '/*' ;
SLSL : '//' ;
SMALL : 'small' ;
SPECIFY : 'specify' ;
SPECPARAM : 'specparam' ;
STRONGONE : 'strong1' ;
STRONGZERO : 'strong0' ;
SUPPLYONE : 'supply1' ;
SUPPLYZERO : 'supply0' ;
TABLE : 'table' -> mode(UDP_MODE) ;
TASK : 'task' ;
TI : '~' ;
TIAM : '~&' ;
TICA : '~^' ;
TIME : 'time' ;
TIVL : '~|' ;
TRAN : 'tran' ;
TRANIFONE : 'tranif1' ;
TRANIFZERO : 'tranif0' ;
TRI : 'tri' ;
TRIAND : 'triand' ;
TRIONE : 'tri1' ;
TRIOR : 'trior' ;
TRIREG : 'trireg' ;
TRIZERO : 'tri0' ;
USE : 'use' ;
UWIRE : 'uwire' ;
VECTORED : 'vectored' ;
VL : '|' ;
VLVL : '||' ;
WAIT : 'wait' ;
WAND : 'wand' ;
WEAKONE : 'weak1' ;
WEAKZERO : 'weak0' ;
WHILE : 'while' ;
WIRE : 'wire' ;
WOR : 'wor' ;
XNOR : 'xnor' ;
XOR : 'xor' ;

BINARY_NUMBER : ( SIZE WHITE_SPACE? )? BINARY_BASE WHITE_SPACE? BINARY_VALUE ;
COMMENT : ( ONE_LINE_COMMENT | BLOCK_COMMENT ) -> channel(COMMENTS) ;
DECIMAL_NUMBER : ( ( SIZE WHITE_SPACE? )? DECIMAL_BASE WHITE_SPACE? )? UNSIGNED_NUMBER | ( SIZE WHITE_SPACE? )? DECIMAL_BASE WHITE_SPACE? ( X_DIGIT | Z_DIGIT ) '_'* ;
ESCAPED_IDENTIFIER : '\\' ASCII_PRINTABLE_EXCEPT_SPACE* WHITE_SPACE ;
HEX_NUMBER : ( SIZE WHITE_SPACE? )? HEX_BASE WHITE_SPACE? HEX_VALUE ;
OCTAL_NUMBER : ( SIZE WHITE_SPACE? )? OCTAL_BASE WHITE_SPACE? OCTAL_VALUE ;
REAL_NUMBER : UNSIGNED_NUMBER '.' UNSIGNED_NUMBER | UNSIGNED_NUMBER ( '.' UNSIGNED_NUMBER )? EXP SIGN? UNSIGNED_NUMBER ;
SIMPLE_IDENTIFIER : [a-zA-Z_] [a-zA-Z0-9_$]* ;
STRING : '"' ( ~["\\] | ESC_SEQ )* '"' ;
SYSTEM_TF_IDENTIFIER : '$' [a-zA-Z0-9_$] [a-zA-Z0-9_$]* ;
WHITE_SPACE : [ \t\r\n]+ -> channel(HIDDEN) ;

mode LIBRARY_MODE;
LIBRARY_COMMENT : COMMENT -> channel(COMMENTS), type(COMMENT) ;
LIBRARY_ESCAPED_IDENTIFIER : ESCAPED_IDENTIFIER -> type(ESCAPED_IDENTIFIER) ;
LIBRARY_MIINCDIR : MIINCDIR -> type(MIINCDIR) ;
LIBRARY_SIMPLE_IDENTIFIER : SIMPLE_IDENTIFIER -> type(SIMPLE_IDENTIFIER) ;
LIBRARY_WHITE_SPACE : WHITE_SPACE -> channel(HIDDEN), type(WHITE_SPACE) ;
LMCO : CO -> type(CO) ;
LMGA : GA -> channel(DIRECTIVES), type(GA), pushMode(DIRECTIVE_MODE) ;
LMSC : SC -> type(SC), mode(DEFAULT_MODE) ;
FILE_PATH_SPEC : ( [a-zA-Z0-9_./] | ESC_SEQ )+ | STRING ;

mode UDP_MODE;
EDGE_SYMBOL : [rRfFpPnN*] ;
LEVEL_ONLY_SYMBOL : [?bB] ;
OUTPUT_OR_LEVEL_SYMBOL : [01xX] ;
UDP_COMMENT : COMMENT -> channel(COMMENTS), type(COMMENT) ;
UDP_ENDTABLE : ENDTABLE -> type(ENDTABLE), mode(DEFAULT_MODE) ;
UDP_WHITE_SPACE : WHITE_SPACE -> channel(HIDDEN), type(WHITE_SPACE) ;
UMCL : CL -> type(CL) ;
UMGA : GA -> channel(DIRECTIVES), type(GA), pushMode(DIRECTIVE_MODE) ;
UMLP : LP -> type(LP) ;
UMMI : MI -> type(MI) ;
UMRP : RP -> type(RP) ;
UMSC : SC -> type(SC) ;

mode EDGE_MODE;
EDGE_COMMENT : COMMENT -> channel(COMMENTS), type(COMMENT) ;
EDGE_DESCRIPTOR : '01' | '10' | [zZxX] [01] | [01] [zZxX] ;
EDGE_WHITE_SPACE : WHITE_SPACE -> channel(HIDDEN), type(WHITE_SPACE) ;
EMCO : CO -> type(CO) ;
EMGA : GA -> channel(DIRECTIVES), type(GA), pushMode(DIRECTIVE_MODE) ;
EMLB : LB -> type(LB) ;
EMRB : RB -> type(RB), mode(DEFAULT_MODE) ;

mode DIRECTIVE_MODE;
BEGIN_KEYWORDS_DIRECTIVE : 'begin_keywords' -> channel(DIRECTIVES), mode(DIRECTIVE_TEXT_MODE) ;
CELLDEFINE_DIRECTIVE : 'celldefine' -> channel(DIRECTIVES), popMode ;
DEFAULT_NETTYPE_DIRECTIVE : 'default_nettype' -> channel(DIRECTIVES), mode(DIRECTIVE_TEXT_MODE) ;
DEFINE_DIRECTIVE : 'define' -> channel(DIRECTIVES), mode(DEFINE_DIRECTIVE_MODE) ;
ELSE_DIRECTIVE : 'else' -> channel(DIRECTIVES), popMode, mode(SOURCE_TEXT_MODE) ;
ELSIF_DIRECTIVE : 'elsif' -> channel(DIRECTIVES), popMode, mode(ELSIF_DIRECTIVE_MODE) ;
END_KEYWORDS_DIRECTIVE : 'end_keywords' -> channel(DIRECTIVES), popMode ;
ENDCELLDEFINE_DIRECTIVE : 'endcelldefine' -> channel(DIRECTIVES), popMode ;
ENDIF_DIRECTIVE : 'endif' -> channel(DIRECTIVES), popMode, popMode, popMode ;
IFDEF_DIRECTIVE : 'ifdef' -> channel(DIRECTIVES), mode(IFDEF_DIRECTIVE_MODE) ;
IFNDEF_DIRECTIVE : 'ifndef' -> channel(DIRECTIVES), mode(IFDEF_DIRECTIVE_MODE) ;
INCLUDE_DIRECTIVE : 'include' -> channel(DIRECTIVES), mode(INCLUDE_DIRECTIVE_MODE) ;
LINE_DIRECTIVE : 'line' -> channel(DIRECTIVES), mode(DIRECTIVE_TEXT_MODE) ;
NOUNCONNECTED_DRIVE_DIRECTIVE : 'nounconnected_drive' -> channel(DIRECTIVES), popMode ;
PRAGMA_DIRECTIVE : 'pragma' -> channel(DIRECTIVES), mode(DIRECTIVE_TEXT_MODE) ;
RESETALL_DIRECTIVE : 'resetall' -> channel(DIRECTIVES), popMode ;
TIMESCALE_DIRECTIVE : 'timescale' -> channel(DIRECTIVES), mode(DIRECTIVE_TEXT_MODE) ;
UNCONNECTED_DRIVE_DIRECTIVE : 'unconnected_drive' -> channel(DIRECTIVES), mode(DIRECTIVE_TEXT_MODE) ;
UNDEF_DIRECTIVE : 'undef' -> channel(DIRECTIVES), mode(UNDEF_DIRECTIVE_MODE) ;
MACRO_USAGE : DIRECTIVE_ID ( [ \t\r\n]* MACRO_ARGS )? -> channel(DIRECTIVES), popMode ;

mode DIRECTIVE_TEXT_MODE;
DIRECTIVE_TEXT : COMMENT_TEXT NEWLINE -> channel(DIRECTIVES), popMode ;

mode INCLUDE_DIRECTIVE_MODE;
FILENAME : ( '<' ~[<>]+ '>' | STRING ) -> channel(DIRECTIVES), popMode ;
INCLUDE_COMMENT : COMMENT -> channel(COMMENTS), type(DIRECTIVE_COMMENT) ;
INCLUDE_WHITE_SPACE : WHITE_SPACE -> channel(HIDDEN), type(DIRECTIVE_WHITE_SPACE) ;

mode DEFINE_DIRECTIVE_MODE;
DIRECTIVE_COMMENT : COMMENT -> channel(COMMENTS) ;
DIRECTIVE_WHITE_SPACE : WHITE_SPACE -> channel(HIDDEN) ;
MACRO_NAME : DIRECTIVE_ID ( [ \t\r\n]* MACRO_ARGS )? -> channel(DIRECTIVES), mode(MACRO_TEXT_MODE) ;

mode MACRO_TEXT_MODE;
MACRO_ESC_NEWLINE : '\\' NEWLINE -> channel(DIRECTIVES) ;
MACRO_ESC_SEQ : ESC_SEQ -> channel(DIRECTIVES), type(MACRO_TEXT) ;
MACRO_NEWLINE : NEWLINE -> channel(HIDDEN), type(DIRECTIVE_WHITE_SPACE), popMode ;
MACRO_TEXT : ~[\r\n\\]+ -> channel(DIRECTIVES) ;

mode UNDEF_DIRECTIVE_MODE;
DIRECTIVE_IDENTIFIER : DIRECTIVE_ID -> channel(DIRECTIVES), popMode ;
UNDEF_COMMENT : COMMENT -> channel(COMMENTS), type(DIRECTIVE_COMMENT) ;
UNDEF_WHITE_SPACE : WHITE_SPACE -> channel(HIDDEN), type(DIRECTIVE_WHITE_SPACE) ;

mode IFDEF_DIRECTIVE_MODE;
IFDEF_COMMENT : COMMENT -> channel(COMMENTS), type(DIRECTIVE_COMMENT) ;
IFDEF_IDENTIFIER : DIRECTIVE_ID -> channel(DIRECTIVES), type(DIRECTIVE_IDENTIFIER), pushMode(SOURCE_TEXT_MODE) ;
IFDEF_WHITE_SPACE : WHITE_SPACE -> channel(HIDDEN), type(DIRECTIVE_WHITE_SPACE) ;

mode ELSIF_DIRECTIVE_MODE;
ELSIF_COMMENT : COMMENT -> channel(COMMENTS), type(DIRECTIVE_COMMENT) ;
ELSIF_IDENTIFIER : DIRECTIVE_ID -> channel(DIRECTIVES), type(DIRECTIVE_IDENTIFIER), mode(SOURCE_TEXT_MODE) ;
ELSIF_WHITE_SPACE : WHITE_SPACE -> channel(HIDDEN), type(DIRECTIVE_WHITE_SPACE) ;

mode SOURCE_TEXT_MODE;
SOURCE_TEXT : ~[`/]+ -> channel(DIRECTIVES) ;
SOURCE_TEXT_COMMENT : COMMENT -> channel(COMMENTS), type(DIRECTIVE_COMMENT) ;
STGA : GA -> channel(DIRECTIVES), type(GA), pushMode(DIRECTIVE_MODE) ;
STSL : SL -> more ;

fragment ASCII_ANY : [\u0000-\u007f] ;
fragment ASCII_PRINTABLE : [\u0020-\u007e] ;
fragment ASCII_PRINTABLE_EXCEPT_SPACE : [\u0021-\u007e] ;
fragment BINARY_BASE : '\'' [sS]? [bB] ;
fragment BINARY_DIGIT : [01] | X_DIGIT | Z_DIGIT ;
fragment BINARY_VALUE : BINARY_DIGIT ( '_' | BINARY_DIGIT )* ;
fragment BLOCK_COMMENT : '/*' COMMENT_TEXT '*/' ;
fragment COMMENT_TEXT : ASCII_ANY*? ;
fragment DECIMAL_BASE : '\'' [sS]? [dD] ;
fragment DECIMAL_DIGIT : [0-9] ;
fragment DIRECTIVE_ID : ESCAPED_IDENTIFIER | SIMPLE_IDENTIFIER ;
fragment ESC_SEQ : '\\' . ;
fragment EXP : [eE] ;
fragment HEX_BASE : '\'' [sS]? [hH] ;
fragment HEX_DIGIT : [0-9a-fA-F] | X_DIGIT | Z_DIGIT ;
fragment HEX_VALUE : HEX_DIGIT ( '_' | HEX_DIGIT )* ;
fragment MACRO_ARGS : '(' ( MACRO_ARGS | ~[()] )* ')' ;
fragment NEWLINE : '\r'? '\n' ;
fragment NON_ZERO_DECIMAL_DIGIT : [1-9] ;
fragment NON_ZERO_UNSIGNED_NUMBER : NON_ZERO_DECIMAL_DIGIT ( '_' | DECIMAL_DIGIT )* ;
fragment OCTAL_BASE : '\'' [sS]? [oO] ;
fragment OCTAL_DIGIT : [0-7] | X_DIGIT | Z_DIGIT ;
fragment OCTAL_VALUE : OCTAL_DIGIT ( '_' | OCTAL_DIGIT )* ;
fragment ONE_LINE_COMMENT : '//' COMMENT_TEXT NEWLINE ;
fragment SIGN : [+\-] ;
fragment SIZE : NON_ZERO_UNSIGNED_NUMBER ;
fragment UNSIGNED_NUMBER : DECIMAL_DIGIT ( '_' | DECIMAL_DIGIT )* ;
fragment X_DIGIT : [xX] ;
fragment Z_DIGIT : [zZ?] ;
