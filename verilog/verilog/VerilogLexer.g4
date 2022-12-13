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
AS : '*' ;
ASAS : '**' ;
ASGT : '*>' ;
ASSIGN : 'assign' ;
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
EDGE : 'edge' -> pushMode(EDGE_MODE) ;
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
INCLUDE : 'include' -> pushMode(LIBRARY_MODE) ;
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
LIBRARY : 'library' -> pushMode(LIBRARY_MODE) ;
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
SMALL : 'small' ;
SPECIFY : 'specify' ;
SPECPARAM : 'specparam' ;
STRONGONE : 'strong1' ;
STRONGZERO : 'strong0' ;
SUPPLYONE : 'supply1' ;
SUPPLYZERO : 'supply0' ;
TABLE : 'table' -> pushMode(TABLE_MODE) ;
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

BINARY_BASE : '\'' [sS]? [bB] -> pushMode(BINARY_NUMBER_MODE) ;
COMMENT : ( ONE_LINE_COMMENT | BLOCK_COMMENT ) -> channel(COMMENTS) ;
DECIMAL_BASE : '\'' [sS]? [dD] -> pushMode(DECIMAL_NUMBER_MODE) ;
ESCAPED_IDENTIFIER : '\\' ASCII_PRINTABLE_NO_SPACE* [ \t\r\n] ;
EXPONENTIAL_NUMBER : UNSIGNED_NUMBER ( '.' UNSIGNED_NUMBER )? [eE] [+\-]? UNSIGNED_NUMBER ;
FIXED_POINT_NUMBER : UNSIGNED_NUMBER '.' UNSIGNED_NUMBER ;
HEX_BASE : '\'' [sS]? [hH] -> pushMode(HEX_NUMBER_MODE) ;
OCTAL_BASE : '\'' [sS]? [oO] -> pushMode(OCTAL_NUMBER_MODE) ;
SIMPLE_IDENTIFIER : [a-zA-Z_] [a-zA-Z0-9_$]* ;
STRING : '"' ( ASCII_NO_NEWLINE_QUOTE_BACKSLASH | ESC_SPECIAL_CHAR )* '"' ;
SYSTEM_TF_IDENTIFIER : '$' [a-zA-Z0-9_$] [a-zA-Z0-9_$]* ;
UNSIGNED_NUMBER : [0-9] [0-9_]* ;
WHITE_SPACE : [ \t\r\n]+ -> channel(HIDDEN) ;

mode BINARY_NUMBER_MODE;
BINARY_VALUE : [01xXzZ?] [01xXzZ?_]* -> popMode ;
WHITE_SPACE_0 : WHITE_SPACE -> channel(HIDDEN), type(WHITE_SPACE) ;

mode DECIMAL_NUMBER_MODE;
UNSIGNED_NUMBER_0 : UNSIGNED_NUMBER -> type(UNSIGNED_NUMBER), popMode ;
WHITE_SPACE_1 : WHITE_SPACE -> channel(HIDDEN), type(WHITE_SPACE) ;
X_OR_Z_UNDERSCORE : [xXzZ?] '_'* -> popMode ;

mode EDGE_MODE;
CO_0 : CO -> type(CO) ;
COMMENT_0 : COMMENT -> channel(COMMENTS), type(COMMENT) ;
EDGE_DESCRIPTOR : '01' | '10' | [xXzZ] [01] | [01] [xXzZ] ;
GA_0 : GA -> channel(DIRECTIVES), type(GA), pushMode(DIRECTIVE_MODE) ;
LB_0 : LB -> type(LB) ;
RB_0 : RB -> type(RB), popMode ;
WHITE_SPACE_2 : WHITE_SPACE -> channel(HIDDEN), type(WHITE_SPACE) ;

mode HEX_NUMBER_MODE;
HEX_VALUE : [0-9a-fA-FxXzZ?] [0-9a-fA-FxXzZ?_]* -> popMode ;
WHITE_SPACE_3 : WHITE_SPACE -> channel(HIDDEN), type(WHITE_SPACE) ;

mode LIBRARY_MODE;
CO_1 : CO -> type(CO) ;
COMMENT_1 : COMMENT -> channel(COMMENTS), type(COMMENT) ;
ESCAPED_IDENTIFIER_0 : ESCAPED_IDENTIFIER -> type(ESCAPED_IDENTIFIER) ;
GA_1 : GA -> channel(DIRECTIVES), type(GA), pushMode(DIRECTIVE_MODE) ;
MIINCDIR_0 : MIINCDIR -> type(MIINCDIR) ;
SC_0 : SC -> type(SC), popMode ;
SIMPLE_IDENTIFIER_0 : SIMPLE_IDENTIFIER -> type(SIMPLE_IDENTIFIER) ;
WHITE_SPACE_4 : WHITE_SPACE -> channel(HIDDEN), type(WHITE_SPACE) ;
FILE_PATH_SPEC : ( [a-zA-Z0-9_./] | ESC_ASCII_PRINTABLE )+ | STRING ;

mode OCTAL_NUMBER_MODE;
OCTAL_VALUE : [0-7xXzZ?] [0-7xXzZ?_]* -> popMode ;
WHITE_SPACE_5 : WHITE_SPACE -> channel(HIDDEN), type(WHITE_SPACE) ;

mode TABLE_MODE;
CL_0 : CL -> type(CL) ;
COMMENT_2 : COMMENT -> channel(COMMENTS), type(COMMENT) ;
EDGE_SYMBOL : [rRfFpPnN*] ;
ENDTABLE_0 : ENDTABLE -> type(ENDTABLE), popMode ;
GA_2 : GA -> channel(DIRECTIVES), type(GA), pushMode(DIRECTIVE_MODE) ;
LEVEL_ONLY_SYMBOL : [?bB] ;
LP_0 : LP -> type(LP) ;
MI_0 : MI -> type(MI) ;
OUTPUT_OR_LEVEL_SYMBOL : [01xX] ;
RP_0 : RP -> type(RP) ;
SC_1 : SC -> type(SC) ;
WHITE_SPACE_6 : WHITE_SPACE -> channel(HIDDEN), type(WHITE_SPACE) ;

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
COMMENT_3 : COMMENT -> channel(COMMENTS), type(COMMENT) ;
DQ_0 : DQ -> channel(DIRECTIVES), type(DQ) ;
NEWLINE_0 : NEWLINE -> channel(HIDDEN), type(WHITE_SPACE), popMode ;
SPACE_TAB_0 : SPACE_TAB -> channel(HIDDEN), type(WHITE_SPACE) ;
VERSION_SPECIFIER : ( '1364-2005' | '1364-2001' | '1364-2001-noconfig' | '1364-1995' ) -> channel(DIRECTIVES) ;

mode DEFAULT_NETTYPE_DIRECTIVE_MODE;
COMMENT_4 : COMMENT -> channel(COMMENTS), type(COMMENT) ;
DEFAULT_NETTYPE_VALUE : ( 'wire' | 'tri' | 'tri0' | 'tri1' | 'wand' | 'triand' | 'wor' | 'trior' | 'trireg' | 'uwire' | 'none' ) -> channel(DIRECTIVES), popMode ;
NEWLINE_1 : NEWLINE -> channel(HIDDEN), type(WHITE_SPACE), popMode ;
SPACE_TAB_1 : SPACE_TAB -> channel(HIDDEN), type(WHITE_SPACE) ;

mode DEFINE_DIRECTIVE_MODE;
COMMENT_5 : COMMENT -> channel(COMMENTS) ;
MACRO_NAME : IDENTIFIER ( [ \t\r\n]* MACRO_ARGS )? -> channel(DIRECTIVES), mode(MACRO_TEXT_MODE) ;
WHITE_SPACE_7 : WHITE_SPACE -> channel(HIDDEN) ;

mode ELSIF_DIRECTIVE_MODE;
COMMENT_6 : COMMENT -> channel(COMMENTS), type(COMMENT) ;
IDENTIFIER_0 : IDENTIFIER -> channel(DIRECTIVES), type(MACRO_IDENTIFIER), mode(SOURCE_TEXT_MODE) ;
WHITE_SPACE_8 : WHITE_SPACE -> channel(HIDDEN), type(WHITE_SPACE) ;

mode FILENAME_MODE;
DQ_1 : DQ -> channel(DIRECTIVES), type(DQ), popMode ;
FILENAME : ( ASCII_PRINTABLE_NO_QUOTE_BACKSLASH | ESC_ASCII_PRINTABLE )+ -> channel(DIRECTIVES) ;

mode IFDEF_DIRECTIVE_MODE;
COMMENT_7 : COMMENT -> channel(COMMENTS), type(COMMENT) ;
IDENTIFIER_1 : IDENTIFIER -> channel(DIRECTIVES), type(MACRO_IDENTIFIER), pushMode(SOURCE_TEXT_MODE) ;
WHITE_SPACE_9 : WHITE_SPACE -> channel(HIDDEN), type(WHITE_SPACE) ;

mode INCLUDE_DIRECTIVE_MODE;
COMMENT_8 : COMMENT -> channel(COMMENTS), type(COMMENT) ;
DQ_2 : DQ -> channel(DIRECTIVES), type(DQ), pushMode(FILENAME_MODE) ;
NEWLINE_2 : NEWLINE -> channel(HIDDEN), type(WHITE_SPACE), popMode ;
SPACE_TAB_2 : SPACE_TAB -> channel(HIDDEN), type(WHITE_SPACE) ;

mode LINE_DIRECTIVE_MODE;
DQ_3 : DQ -> channel(DIRECTIVES), type(DQ), pushMode(FILENAME_MODE) ;
NEWLINE_3 : NEWLINE -> channel(HIDDEN), type(WHITE_SPACE), popMode ;
SPACE_TAB_3 : SPACE_TAB -> channel(HIDDEN), type(WHITE_SPACE) ;
UNSIGNED_NUMBER_1 : UNSIGNED_NUMBER -> channel(DIRECTIVES), type(UNSIGNED_NUMBER) ;

mode MACRO_TEXT_MODE;
GA_3 : GA -> channel(DIRECTIVES), type(MACRO_TEXT) ;
MACRO_DELIMITER : '``' -> channel(DIRECTIVES) ;
MACRO_ESC_NEWLINE : ESC_NEWLINE -> channel(DIRECTIVES) ;
MACRO_ESC_QUOTE : '`\\`"' -> channel(DIRECTIVES) ;
MACRO_ESC_SEQ : ESC_ASCII_NO_NEWLINE -> channel(DIRECTIVES), type(MACRO_TEXT) ;
MACRO_QUOTE : '`"' -> channel(DIRECTIVES) ;
MACRO_TEXT : ASCII_NO_NEWLINE_QUOTE_BACKSLASH_GRAVE_ACCENT+ -> channel(DIRECTIVES) ;
NEWLINE_4 : NEWLINE -> channel(HIDDEN), type(WHITE_SPACE), popMode ;
STRING_0 : STRING -> channel(DIRECTIVES), type(STRING) ;

mode PRAGMA_DIRECTIVE_MODE;
CO_2 : CO -> channel(DIRECTIVES), type(CO) ;
COMMENT_9 : COMMENT -> channel(COMMENTS), type(COMMENT) ;
EQ_0 : EQ -> channel(DIRECTIVES), type(EQ) ;
LP_1 : LP -> channel(DIRECTIVES), type(LP) ;
NEWLINE_5 : NEWLINE -> channel(HIDDEN), type(WHITE_SPACE), popMode ;
RP_1 : RP -> channel(DIRECTIVES), type(RP) ;
SIMPLE_IDENTIFIER_1 : SIMPLE_IDENTIFIER -> channel(DIRECTIVES), type(SIMPLE_IDENTIFIER) ;
SPACE_TAB_4 : SPACE_TAB -> channel(HIDDEN), type(WHITE_SPACE) ;
STRING_1 : STRING -> channel(DIRECTIVES), type(STRING) ;
UNSIGNED_NUMBER_2 : UNSIGNED_NUMBER -> channel(DIRECTIVES), type(UNSIGNED_NUMBER) ;

mode SOURCE_TEXT_MODE;
COMMENT_10 : COMMENT -> channel(COMMENTS), type(COMMENT) ;
GA_4 : GA -> channel(DIRECTIVES), type(GA), pushMode(DIRECTIVE_MODE) ;
SL_0 : SL -> more ;
SOURCE_TEXT : ASCII_NO_SLASH_GRAVE_ACCENT+ -> channel(DIRECTIVES) ;

mode TIMESCALE_DIRECTIVE_MODE;
COMMENT_11 : COMMENT -> channel(COMMENTS), type(COMMENT) ;
NEWLINE_6 : NEWLINE -> channel(HIDDEN), type(WHITE_SPACE), popMode ;
SL_1 : SL -> channel(DIRECTIVES), type(SL) ;
SPACE_TAB_5 : SPACE_TAB -> channel(HIDDEN), type(WHITE_SPACE) ;
TIME_UNIT : [munpf]? 's' -> channel(DIRECTIVES) ;
TIME_VALUE : ( '1' | '10' | '100' ) -> channel(DIRECTIVES) ;

mode UNCONNECTED_DRIVE_DIRECTIVE_MODE;
COMMENT_12 : COMMENT -> channel(COMMENTS), type(COMMENT) ;
NEWLINE_7 : NEWLINE -> channel(HIDDEN), type(WHITE_SPACE), popMode ;
SPACE_TAB_6 : SPACE_TAB -> channel(HIDDEN), type(WHITE_SPACE) ;
UNCONNECTED_DRIVE_VALUE : ( 'pull0' | 'pull1' ) -> channel(DIRECTIVES), popMode ;

mode UNDEF_DIRECTIVE_MODE;
COMMENT_13 : COMMENT -> channel(COMMENTS), type(COMMENT) ;
MACRO_IDENTIFIER : IDENTIFIER -> channel(DIRECTIVES), popMode ;
WHITE_SPACE_10 : WHITE_SPACE -> channel(HIDDEN), type(WHITE_SPACE) ;

fragment ASCII_ANY : [\u0000-\u007f] ;
fragment ASCII_NO_NEWLINE : [\u0000-\u0009\u000b-\u000c\u000e-\u007f] ;
fragment ASCII_NO_NEWLINE_QUOTE_BACKSLASH : [\u0000-\u0009\u000b-\u000c\u000e-\u0021\u0023-\u005b\u005d-\u007f] ;
fragment ASCII_NO_NEWLINE_QUOTE_BACKSLASH_GRAVE_ACCENT : [\u0000-\u0009\u000b-\u000c\u000e-\u0021\u0023-\u005b\u005d-\u005f\u0061-\u007f] ;
fragment ASCII_NO_PARENTHESES : [\u0000-\u0027\u002a-\u007f] ;
fragment ASCII_NO_SLASH_GRAVE_ACCENT : [\u0000-\u002e\u0030-\u005f\u0061-\u007f] ;
fragment ASCII_PRINTABLE : [\u0020-\u007e] ;
fragment ASCII_PRINTABLE_NO_QUOTE_BACKSLASH : [\u0020-\u0021\u0023-\u005b\u005d-\u007e] ;
fragment ASCII_PRINTABLE_NO_SPACE : [\u0021-\u007e] ;
fragment BLOCK_COMMENT : '/*' ASCII_ANY*? '*/' ;
fragment CHAR_OCTAL : [0-7] [0-7]? [0-7]? ;
fragment ESC_ASCII_NO_NEWLINE : '\\' ASCII_NO_NEWLINE ;
fragment ESC_ASCII_PRINTABLE : '\\' ASCII_PRINTABLE ;
fragment ESC_NEWLINE : '\\' NEWLINE ;
fragment ESC_SPECIAL_CHAR : '\\' ( [nt\\"] | CHAR_OCTAL ) ;
fragment IDENTIFIER : ESCAPED_IDENTIFIER | SIMPLE_IDENTIFIER ;
fragment MACRO_ARGS : '(' ( MACRO_ARGS | ASCII_NO_PARENTHESES )* ')' ;
fragment NEWLINE : '\r'? '\n' ;
fragment ONE_LINE_COMMENT : '//' ASCII_NO_NEWLINE* ;
fragment SPACE_TAB : [ \t] ;
