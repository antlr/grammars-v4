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
ESCAPED_IDENTIFIER : '\\' ASCII_PRINTABLE_NO_SPACE* WHITE_SPACE ;
HEX_NUMBER : ( SIZE WHITE_SPACE? )? HEX_BASE WHITE_SPACE? HEX_VALUE ;
OCTAL_NUMBER : ( SIZE WHITE_SPACE? )? OCTAL_BASE WHITE_SPACE? OCTAL_VALUE ;
REAL_NUMBER : UNSIGNED_NUMBER '.' UNSIGNED_NUMBER | UNSIGNED_NUMBER ( '.' UNSIGNED_NUMBER )? EXP SIGN? UNSIGNED_NUMBER ;
SIMPLE_IDENTIFIER : [a-zA-Z_] [a-zA-Z0-9_$]* ;
STRING : '"' ( ASCII_NO_NEWLINE_QUOTATION_MARK_BACKSLASH | ESC_SPECIAL_CHAR )* '"' ;
SYSTEM_TF_IDENTIFIER : '$' [a-zA-Z0-9_$] [a-zA-Z0-9_$]* ;
WHITE_SPACE : [ \t\r\n]+ -> channel(HIDDEN) ;

mode LIBRARY_MODE;
CO_0 : CO -> type(CO) ;
COMMENT_0 : COMMENT -> channel(COMMENTS), type(COMMENT) ;
ESCAPED_IDENTIFIER_0 : ESCAPED_IDENTIFIER -> type(ESCAPED_IDENTIFIER) ;
GA_0 : GA -> channel(DIRECTIVES), type(GA), pushMode(DIRECTIVE_MODE) ;
MIINCDIR_0 : MIINCDIR -> type(MIINCDIR) ;
SC_0 : SC -> type(SC), mode(DEFAULT_MODE) ;
SIMPLE_IDENTIFIER_0 : SIMPLE_IDENTIFIER -> type(SIMPLE_IDENTIFIER) ;
WHITE_SPACE_0 : WHITE_SPACE -> channel(HIDDEN), type(WHITE_SPACE) ;
FILE_PATH_SPEC : ( [a-zA-Z0-9_./] | ESC_ASCII_PRINTABLE )+ | FILENAME_IN_DOUBLE_QUOTES ;

mode UDP_MODE;
CL_0 : CL -> type(CL) ;
COMMENT_1 : COMMENT -> channel(COMMENTS), type(COMMENT) ;
EDGE_SYMBOL : [rRfFpPnN*] ;
ENDTABLE_0 : ENDTABLE -> type(ENDTABLE), mode(DEFAULT_MODE) ;
GA_1 : GA -> channel(DIRECTIVES), type(GA), pushMode(DIRECTIVE_MODE) ;
LEVEL_ONLY_SYMBOL : [?bB] ;
LP_0 : LP -> type(LP) ;
MI_0 : MI -> type(MI) ;
OUTPUT_OR_LEVEL_SYMBOL : [01xX] ;
RP_0 : RP -> type(RP) ;
SC_1 : SC -> type(SC) ;
WHITE_SPACE_1 : WHITE_SPACE -> channel(HIDDEN), type(WHITE_SPACE) ;

mode EDGE_MODE;
CO_2 : CO -> type(CO) ;
COMMENT_12 : COMMENT -> channel(COMMENTS), type(COMMENT) ;
EDGE_DESCRIPTOR : '01' | '10' | [zZxX] [01] | [01] [zZxX] ;
EDGE_WHITE_SPACE : WHITE_SPACE -> channel(HIDDEN), type(WHITE_SPACE) ;
GA_5 : GA -> channel(DIRECTIVES), type(GA), pushMode(DIRECTIVE_MODE) ;
LB_0 : LB -> type(LB) ;
RB_0 : RB -> type(RB), mode(DEFAULT_MODE) ;

mode DIRECTIVE_MODE;
BEGIN_KEYWORDS_DIRECTIVE : 'begin_keywords' -> channel(DIRECTIVES), mode(BEGIN_KEYWORDS_DIRECTIVE_MODE) ;
CELLDEFINE_DIRECTIVE : 'celldefine' -> channel(DIRECTIVES), popMode ;
DEFAULT_NETTYPE_DIRECTIVE : 'default_nettype' -> channel(DIRECTIVES), mode(DEFAULT_NETTYPE_DIRECTIVE_MODE) ;
DEFINE_DIRECTIVE : 'define' -> channel(DIRECTIVES), mode(DEFINE_DIRECTIVE_MODE) ;
ELSE_DIRECTIVE : 'else' -> channel(DIRECTIVES), popMode, mode(SOURCE_TEXT_MODE) ;
ELSIF_DIRECTIVE : 'elsif' -> channel(DIRECTIVES), popMode, mode(ELSIF_DIRECTIVE_MODE) ;
END_KEYWORDS_DIRECTIVE : 'end_keywords' -> channel(DIRECTIVES), popMode ;
ENDCELLDEFINE_DIRECTIVE : 'endcelldefine' -> channel(DIRECTIVES), popMode ;
ENDIF_DIRECTIVE : 'endif' -> channel(DIRECTIVES), popMode, popMode, popMode ;
IFDEF_DIRECTIVE : 'ifdef' -> channel(DIRECTIVES), mode(IFDEF_DIRECTIVE_MODE) ;
IFNDEF_DIRECTIVE : 'ifndef' -> channel(DIRECTIVES), mode(IFDEF_DIRECTIVE_MODE) ;
INCLUDE_DIRECTIVE : 'include' -> channel(DIRECTIVES), mode(INCLUDE_DIRECTIVE_MODE) ;
LINE_DIRECTIVE : 'line' -> channel(DIRECTIVES), mode(LINE_DIRECTIVE_MODE) ;
NOUNCONNECTED_DRIVE_DIRECTIVE : 'nounconnected_drive' -> channel(DIRECTIVES), popMode ;
PRAGMA_DIRECTIVE : 'pragma' -> channel(DIRECTIVES), mode(PRAGMA_DIRECTIVE_MODE) ;
RESETALL_DIRECTIVE : 'resetall' -> channel(DIRECTIVES), popMode ;
TIMESCALE_DIRECTIVE : 'timescale' -> channel(DIRECTIVES), mode(TIMESCALE_DIRECTIVE_MODE) ;
UNCONNECTED_DRIVE_DIRECTIVE : 'unconnected_drive' -> channel(DIRECTIVES), mode(UNCONNECTED_DRIVE_DIRECTIVE_MODE) ;
UNDEF_DIRECTIVE : 'undef' -> channel(DIRECTIVES), mode(UNDEF_DIRECTIVE_MODE) ;
MACRO_USAGE : IDENTIFIER ( [ \t\r\n]* MACRO_ARGS )? -> channel(DIRECTIVES), popMode ;

mode BEGIN_KEYWORDS_DIRECTIVE_MODE;
COMMENT_2 : COMMENT -> channel(COMMENTS), type(DIRECTIVE_COMMENT) ;
NEWLINE_0 : NEWLINE -> channel(HIDDEN), type(DIRECTIVE_WHITE_SPACE), popMode ;
SPACE_TAB_0 : SPACE_TAB -> channel(HIDDEN), type(DIRECTIVE_WHITE_SPACE) ;
VERSION_SPECIFIER : '"' ( '1364-1995' | '1364-2001' | '1364-2001-noconfig' | '1364-2005' ) '"' -> channel(DIRECTIVES), popMode ;

mode DEFAULT_NETTYPE_DIRECTIVE_MODE;
COMMENT_3 : COMMENT -> channel(COMMENTS), type(DIRECTIVE_COMMENT) ;
DEFAULT_NETTYPE_VALUE : ( 'wire' | 'tri' | 'tri0' | 'tri1' | 'wand' | 'triand' | 'wor' | 'trior' | 'trireg' | 'uwire' | 'none' ) -> channel(DIRECTIVES), popMode ;
NEWLINE_1 : NEWLINE -> channel(HIDDEN), type(DIRECTIVE_WHITE_SPACE), popMode ;
SPACE_TAB_1 : SPACE_TAB -> channel(HIDDEN), type(DIRECTIVE_WHITE_SPACE) ;

mode DEFINE_DIRECTIVE_MODE;
DIRECTIVE_COMMENT : COMMENT -> channel(COMMENTS) ;
DIRECTIVE_WHITE_SPACE : WHITE_SPACE -> channel(HIDDEN) ;
MACRO_NAME : IDENTIFIER ( [ \t\r\n]* MACRO_ARGS )? -> channel(DIRECTIVES), mode(MACRO_TEXT_MODE) ;

mode ELSIF_DIRECTIVE_MODE;
COMMENT_4 : COMMENT -> channel(COMMENTS), type(DIRECTIVE_COMMENT) ;
IDENTIFIER_0 : IDENTIFIER -> channel(DIRECTIVES), type(DIRECTIVE_IDENTIFIER), mode(SOURCE_TEXT_MODE) ;
WHITE_SPACE_2 : WHITE_SPACE -> channel(HIDDEN), type(DIRECTIVE_WHITE_SPACE) ;

mode IFDEF_DIRECTIVE_MODE;
COMMENT_5 : COMMENT -> channel(COMMENTS), type(DIRECTIVE_COMMENT) ;
IDENTIFIER_1 : IDENTIFIER -> channel(DIRECTIVES), type(DIRECTIVE_IDENTIFIER), pushMode(SOURCE_TEXT_MODE) ;
WHITE_SPACE_3 : WHITE_SPACE -> channel(HIDDEN), type(DIRECTIVE_WHITE_SPACE) ;

mode INCLUDE_DIRECTIVE_MODE;
COMMENT_6 : COMMENT -> channel(COMMENTS), type(DIRECTIVE_COMMENT) ;
FILENAME : FILENAME_IN_DOUBLE_QUOTES -> channel(DIRECTIVES), popMode ;
NEWLINE_2 : NEWLINE -> channel(HIDDEN), type(DIRECTIVE_WHITE_SPACE), popMode ;
SPACE_TAB_2 : SPACE_TAB -> channel(HIDDEN), type(DIRECTIVE_WHITE_SPACE) ;

mode LINE_DIRECTIVE_MODE;
FILENAME_0 : FILENAME -> channel(DIRECTIVES), type(FILENAME) ;
LINE_NUMBER : UNSIGNED_NUMBER -> channel(DIRECTIVES) ;
NEWLINE_3 : NEWLINE -> channel(HIDDEN), type(DIRECTIVE_WHITE_SPACE), popMode ;
SPACE_TAB_3 : SPACE_TAB -> channel(HIDDEN), type(DIRECTIVE_WHITE_SPACE) ;

mode MACRO_TEXT_MODE;
GA_4 : GA -> channel(DIRECTIVES), type(MACRO_TEXT) ;
MACRO_DELIMITER : '``' -> channel(DIRECTIVES) ;
MACRO_ESC_NEWLINE : ESC_NEWLINE -> channel(DIRECTIVES) ;
MACRO_ESC_QUOTATION_MARK : '`\\`"' -> channel(DIRECTIVES) ;
MACRO_ESC_SEQ : ESC_ASCII_NO_NEWLINE -> channel(DIRECTIVES), type(MACRO_TEXT) ;
MACRO_QUOTATION_MARK : '`"' -> channel(DIRECTIVES) ;
MACRO_STRING : STRING -> channel(DIRECTIVES) ;
MACRO_TEXT : ASCII_NO_NEWLINE_QUOTATION_MARK_BACKSLASH_GRAVE_ACCENT+ -> channel(DIRECTIVES) ;
NEWLINE_4 : NEWLINE -> channel(HIDDEN), type(DIRECTIVE_WHITE_SPACE), popMode ;

mode PRAGMA_DIRECTIVE_MODE;
CO_1 : CO -> channel(DIRECTIVES), type(CO) ;
COMMENT_7 : COMMENT -> channel(COMMENTS), type(DIRECTIVE_COMMENT) ;
EQ_0 : EQ -> channel(DIRECTIVES), type(EQ) ;
LP_1 : LP -> channel(DIRECTIVES), type(LP) ;
NEWLINE_5 : NEWLINE -> channel(HIDDEN), type(DIRECTIVE_WHITE_SPACE), popMode ;
PRAGMA_IDENTIFIER : SIMPLE_IDENTIFIER -> channel(DIRECTIVES) ;
PRAGMA_NUMBER : ( DECIMAL_NUMBER | BINARY_NUMBER | OCTAL_NUMBER | HEX_NUMBER | REAL_NUMBER ) -> channel(DIRECTIVES) ;
PRAGMA_STRING : STRING -> channel(DIRECTIVES) ;
RP_1 : RP -> channel(DIRECTIVES), type(RP) ;
SPACE_TAB_4 : SPACE_TAB -> channel(HIDDEN), type(DIRECTIVE_WHITE_SPACE) ;

mode SOURCE_TEXT_MODE;
GA_3 : GA -> channel(DIRECTIVES), type(GA), pushMode(DIRECTIVE_MODE) ;
SL_0 : SL -> more ;
SOURCE_TEXT : ASCII_NO_SLASH_GRAVE_ACCENT+ -> channel(DIRECTIVES) ;
SOURCE_TEXT_COMMENT : COMMENT -> channel(COMMENTS), type(DIRECTIVE_COMMENT) ;

mode TIMESCALE_DIRECTIVE_MODE;
COMMENT_8 : COMMENT -> channel(COMMENTS), type(DIRECTIVE_COMMENT) ;
NEWLINE_6 : NEWLINE -> channel(HIDDEN), type(DIRECTIVE_WHITE_SPACE), popMode ;
SL_1 : SL -> channel(DIRECTIVES), type(SL) ;
SPACE_TAB_5 : SPACE_TAB -> channel(HIDDEN), type(DIRECTIVE_WHITE_SPACE) ;
TIMESCALE_UNIT : TIME_UNIT -> channel(DIRECTIVES) ;
TIMESCALE_VALUE : ( '1' | '10' | '100' ) -> channel(DIRECTIVES) ;

mode UNCONNECTED_DRIVE_DIRECTIVE_MODE;
COMMENT_9 : COMMENT -> channel(COMMENTS), type(DIRECTIVE_COMMENT) ;
NEWLINE_7 : NEWLINE -> channel(HIDDEN), type(DIRECTIVE_WHITE_SPACE), popMode ;
SPACE_TAB_6 : SPACE_TAB -> channel(HIDDEN), type(DIRECTIVE_WHITE_SPACE) ;
UNCONNECTED_DRIVE_VALUE : ( 'pull0' | 'pull1' ) -> channel(DIRECTIVES), popMode ;

mode UNDEF_DIRECTIVE_MODE;
COMMENT_10 : COMMENT -> channel(COMMENTS), type(DIRECTIVE_COMMENT) ;
DIRECTIVE_IDENTIFIER : IDENTIFIER -> channel(DIRECTIVES), popMode ;
WHITE_SPACE_4 : WHITE_SPACE -> channel(HIDDEN), type(DIRECTIVE_WHITE_SPACE) ;

fragment ASCII_ANY : [\u0000-\u007f] ;
fragment ASCII_NO_NEWLINE : [\u0000-\u0009\u000b-\u000c\u000e-\u007f] ;
fragment ASCII_NO_NEWLINE_QUOTATION_MARK_BACKSLASH : [\u0000-\u0009\u000b-\u000c\u000e-\u0021\u0023-\u005b\u005d-\u007f] ;
fragment ASCII_NO_NEWLINE_QUOTATION_MARK_BACKSLASH_GRAVE_ACCENT : [\u0000-\u0009\u000b-\u000c\u000e-\u0021\u0023-\u005b\u005d-\u005f\u0061-\u007f] ;
fragment ASCII_NO_PARENTHESES : [\u0000-\u0027\u002a-\u007f] ;
fragment ASCII_NO_SLASH_GRAVE_ACCENT : [\u0000-\u002e\u0030-\u005f\u0061-\u007e] ;
fragment ASCII_PRINTABLE : [\u0020-\u007e] ;
fragment ASCII_PRINTABLE_NO_QUOTATION_MARK_BACKSLASH : [\u0020-\u0021\u0023-\u005b\u005d-\u007e] ;
fragment ASCII_PRINTABLE_NO_SPACE : [\u0021-\u007e] ;
fragment BINARY_BASE : '\'' [sS]? [bB] ;
fragment BINARY_DIGIT : [01] | X_DIGIT | Z_DIGIT ;
fragment BINARY_VALUE : BINARY_DIGIT ( '_' | BINARY_DIGIT )* ;
fragment BLOCK_COMMENT : '/*' ASCII_ANY*? '*/' ;
fragment CHAR_OCTAL : OCTAL_DIGIT_NO_XZ OCTAL_DIGIT_NO_XZ? OCTAL_DIGIT_NO_XZ? ;
fragment DECIMAL_BASE : '\'' [sS]? [dD] ;
fragment DECIMAL_DIGIT : [0-9] ;
fragment ESC_ASCII_NO_NEWLINE : '\\' ASCII_NO_NEWLINE ;
fragment ESC_ASCII_PRINTABLE : '\\' ASCII_PRINTABLE ;
fragment ESC_NEWLINE : '\\' NEWLINE ;
fragment ESC_SPECIAL_CHAR : '\\' ( [nt\\"] | CHAR_OCTAL ) ;
fragment EXP : [eE] ;
fragment FILENAME_IN_DOUBLE_QUOTES : '"' ( ASCII_PRINTABLE_NO_QUOTATION_MARK_BACKSLASH | ESC_ASCII_PRINTABLE )* '"' ;
fragment HEX_BASE : '\'' [sS]? [hH] ;
fragment HEX_DIGIT : [0-9a-fA-F] | X_DIGIT | Z_DIGIT ;
fragment HEX_VALUE : HEX_DIGIT ( '_' | HEX_DIGIT )* ;
fragment IDENTIFIER : ESCAPED_IDENTIFIER | SIMPLE_IDENTIFIER ;
fragment MACRO_ARGS : '(' ( MACRO_ARGS | ASCII_NO_PARENTHESES )* ')' ;
fragment NEWLINE : '\r'? '\n' ;
fragment NON_ZERO_DECIMAL_DIGIT : [1-9] ;
fragment NON_ZERO_UNSIGNED_NUMBER : NON_ZERO_DECIMAL_DIGIT ( '_' | DECIMAL_DIGIT )* ;
fragment OCTAL_BASE : '\'' [sS]? [oO] ;
fragment OCTAL_DIGIT : [0-7] | X_DIGIT | Z_DIGIT ;
fragment OCTAL_DIGIT_NO_XZ : [0-7] ;
fragment OCTAL_VALUE : OCTAL_DIGIT ( '_' | OCTAL_DIGIT )* ;
fragment ONE_LINE_COMMENT : '//' ASCII_NO_NEWLINE* ( NEWLINE | EOF ) ;
fragment SIGN : [+\-] ;
fragment SIZE : NON_ZERO_UNSIGNED_NUMBER ;
fragment SPACE_TAB : [ \t] ;
fragment TIME_UNIT : [munpf]? 's' ;
fragment UNSIGNED_NUMBER : DECIMAL_DIGIT ( '_' | DECIMAL_DIGIT )* ;
fragment X_DIGIT : [xX] ;
fragment Z_DIGIT : [zZ?] ;
