// Author: Mustafa Said Ağca
// License: MIT

lexer grammar VerilogLexer;

// Keywords

ALWAYS : 'always' ;
AND : 'and' ;
ASSIGN : 'assign' ;
AUTOMATIC : 'automatic' ;
BEGIN : 'begin' ;
BUF : 'buf' ;
BUFIF0 : 'bufif0' ;
BUFIF1 : 'bufif1' ;
CASE : 'case' ;
CASEX : 'casex' ;
CASEZ : 'casez' ;
CELL : 'cell' ;
CMOS : 'cmos' ;
CONFIG : 'config' ;
DEASSIGN : 'deassign' ;
DEFAULT : 'default' ;
DEFPARAM : 'defparam' ;
DESIGN : 'design' ;
DISABLE : 'disable' ;
EDGE : 'edge' ;
ELSE : 'else' ;
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
EVENT : 'event' ;
FOR : 'for' ;
FORCE : 'force' ;
FOREVER : 'forever' ;
FORK : 'fork' ;
FUNCTION : 'function' ;
GENERATE : 'generate' ;
GENVAR : 'genvar' ;
HIGHZ0 : 'highz0' ;
HIGHZ1 : 'highz1' ;
IF : 'if' ;
IFNONE : 'ifnone' ;
INCDIR : 'incdir' ;
INCLUDE : 'include' ;
INITIAL : 'initial' ;
INOUT : 'inout' ;
INPUT : 'input' ;
INSTANCE : 'instance' ;
INTEGER : 'integer' ;
JOIN : 'join' ;
LARGE : 'large' ;
LIBLIST : 'liblist' ;
LIBRARY : 'library' ;
LOCALPARAM : 'localparam' ;
MACROMODULE : 'macromodule' ;
MEDIUM : 'medium' ;
MODULE : 'module' ;
NAND : 'nand' ;
NEGEDGE : 'negedge' ;
NMOS : 'nmos' ;
NOR : 'nor' ;
NOSHOWCANCELLED : 'noshowcancelled' ;
NOT : 'not' ;
NOTIF0 : 'notif0' ;
NOTIF1 : 'notif1' ;
OR : 'or' ;
OUTPUT : 'output' ;
PARAMETER : 'parameter' ;
PMOS : 'pmos' ;
POSEDGE : 'posedge' ;
PRIMITIVE : 'primitive' ;
PULL0 : 'pull0' ;
PULL1 : 'pull1' ;
PULLDOWN : 'pulldown' ;
PULLUP : 'pullup' ;
PULSESTYLE_ONEVENT : 'pulsestyle_onevent' ;
PULSESTYLE_ONDETECT : 'pulsestyle_ondetect' ;
RCMOS : 'rcmos' ;
REAL : 'real' ;
REALTIME : 'realtime' ;
REG : 'reg' ;
RELEASE : 'release' ;
REPEAT : 'repeat' ;
RNMOS : 'rnmos' ;
RPMOS : 'rpmos' ;
RTRAN : 'rtran' ;
RTRANIF0 : 'rtranif0' ;
RTRANIF1 : 'rtranif1' ;
SCALARED : 'scalared' ;
SHOWCANCELLED : 'showcancelled' ;
SIGNED : 'signed' ;
SMALL : 'small' ;
SPECIFY : 'specify' ;
SPECPARAM : 'specparam' ;
STRONG0 : 'strong0' ;
STRONG1 : 'strong1' ;
SUPPLY0 : 'supply0' ;
SUPPLY1 : 'supply1' ;
TABLE : 'table' ;
TASK : 'task' ;
TIME : 'time' ;
TRAN : 'tran' ;
TRANIF0 : 'tranif0' ;
TRANIF1 : 'tranif1' ;
TRI : 'tri' ;
TRI0 : 'tri0' ;
TRI1 : 'tri1' ;
TRIAND : 'triand' ;
TRIOR : 'trior' ;
TRIREG : 'trireg' ;
UNSIGNED : 'unsigned' ;
USE : 'use' ;
UWIRE : 'uwire' ;
VECTORED : 'vectored' ;
WAIT : 'wait' ;
WAND : 'wand' ;
WEAK0 : 'weak0' ;
WEAK1 : 'weak1' ;
WHILE : 'while' ;
WIRE : 'wire' ;
WOR : 'wor' ;
XNOR : 'xnor' ;
XOR : 'xor' ;

// System tasks and functions

DISPLAY : '$display' ;
DISPLAYB : '$displayb' ;
DISPLAYH : '$displayh' ;
DISPLAYO : '$displayo' ;
WRITE : '$write' ;
WRITEB : '$writeb' ;
WRITEH : '$writeh' ;
WRITEO : '$writeo' ;
STROBE : '$strobe' ;
STROBEB : '$strobeb' ;
STROBEH : '$strobeh' ;
STROBEO : '$strobeo' ;
MONITOR : '$monitor' ;
MONITORB : '$monitorb' ;
MONITORH : '$monitorh' ;
MONITORO : '$monitoro' ;
MONITOROFF : '$monitoroff' ;
MONITORON : '$monitoron' ;
FCLOSE : '$fclose' ;
FDISPLAY : '$fdisplay' ;
FDISPLAYB : '$fdisplayb' ;
FDISPLAYH : '$fdisplayh' ;
FDISPLAYO : '$fdisplayo' ;
FSTROBE : '$fstrobe' ;
FSTROBEB : '$fstrobeb' ;
FSTROBEH : '$fstrobeh' ;
FSTROBEO : '$fstrobeo' ;
SWRITE : '$swrite' ;
SWRITEB : '$swriteb' ;
SWRITEH : '$swriteh' ;
SWRITEO : '$swriteo' ;
FSCANF : '$fscanf' ;
FREAD : '$fread' ;
FSEEK : '$fseek' ;
FFLUSH : '$fflush' ;
FEOF : '$feof' ;
SDF_ANNOTATE : '$sdf_annotate' ;
FOPEN : '$fopen' ;
FWRITE : '$fwrite' ;
FWRITEB : '$fwriteb' ;
FWRITEH : '$fwriteh' ;
FWRITEO : '$fwriteo' ;
FMONITOR : '$fmonitor' ;
FMONITORB : '$fmonitorb' ;
FMONITORH : '$fmonitorh' ;
FMONITORO : '$fmonitoro' ;
SFORMAT : '$sformat' ;
FGETC : '$fgetc' ;
UNGETC : '$ungetc' ;
FGETS : '$fgets' ;
SSCANF : '$sscanf' ;
REWIND : '$rewind' ;
FTELL : '$ftell' ;
FERROR : '$ferror' ;
READMEMB : '$readmemb' ;
READMEMH : '$readmemh' ;
PRINTTIMESCALE : '$printtimescale' ;
TIMEFORMAT : '$timeformat' ;
FINISH : '$finish' ;
STOP : '$stop' ;
ASYNC_AND_ARRAY : '$async$and$array' ;
ASYNC_NAND_ARRAY : '$async$nand$array' ;
ASYNC_OR_ARRAY : '$async$or$array' ;
ASYNC_NOR_ARRAY : '$async$nor$array' ;
SYNC_AND_ARRAY : '$sync$and$array' ;
SYNC_NAND_ARRAY : '$sync$nand$array' ;
SYNC_OR_ARRAY : '$sync$or$array' ;
SYNC_NOR_ARRAY : '$sync$nor$array' ;
ASYNC_AND_PLANE : '$async$and$plane' ;
ASYNC_NAND_PLANE : '$async$nand$plane' ;
ASYNC_OR_PLANE : '$async$or$plane' ;
ASYNC_NOR_PLANE : '$async$nor$plane' ;
SYNC_AND_PLANE : '$sync$and$plane' ;
SYNC_NAND_PLANE : '$sync$nand$plane' ;
SYNC_OR_PLANE : '$sync$or$plane' ;
SYNC_NOR_PLANE : '$sync$nor$plane' ;
Q_INITIALIZE : '$q_initialize' ;
Q_REMOVE : '$q_remove' ;
Q_EXAM : '$q_exam' ;
Q_ADD : '$q_add' ;
Q_FULL : '$q_full' ;
REALTIME : '$realtime' ;
TIME : '$time' ;
STIME : '$stime' ;
BITSTOREAL : '$bitstoreal' ;
ITOR : '$itor' ;
SIGNED : '$signed' ;
REALTOBITS : '$realtobits' ;
RTOI : '$rtoi' ;
UNSIGNED : '$unsigned' ;
RANDOM : '$random' ;
DIST_ERLANG : '$dist_erlang' ;
DIST_NORMAL : '$dist_normal' ;
DIST_T : '$dist_t' ;
DIST_CHI_SQUARE : '$dist_chi_square' ;
DIST_EXPONENTIAL : '$dist_exponential' ;
DIST_POISSON : '$dist_poisson' ;
DIST_UNIFORM : '$dist_uniform' ;
TEST_PLUSARGS : '$test$plusargs' ;
VALUE_PLUSARGS : '$value$plusargs' ;
CLOG2 : '$clog2' ;
LN : '$ln' ;
LOG10 : '$log10' ;
EXP : '$exp' ;
SQRT : '$sqrt' ;
POW : '$pow' ;
FLOOR : '$floor' ;
CEIL : '$ceil' ;
SIN : '$sin' ;
COS : '$cos' ;
TAN : '$tan' ;
ASIN : '$asin' ;
ACOS : '$acos' ;
ATAN : '$atan' ;
ATAN2 : '$atan2' ;
HYPOT : '$hypot' ;
SINH : '$sinh' ;
COSH : '$cosh' ;
TANH : '$tanh' ;
ASINH : '$asinh' ;
ACOSH : '$acosh' ;
ATANH : '$atanh' ;

// System timing check commands

SETUP : '$setup' ;
HOLD : '$hold' ;
SETUPHOLD : '$setuphold' ;
RECOVERY : '$recovery' ;
REMOVAL : '$removal' ;
RECREM : '$recrem' ;
SKEW : '$skew' ;
TIMESKEW : '$timeskew' ;
FULLSKEW : '$fullskew' ;
PERIOD : '$period' ;
WIDTH : '$width' ;
NOCHANGE : '$nochange' ;

// Compiler directives

BEGIN_KEYWORDS : '`begin_keywords' ;
CELLDEFINE : '`celldefine' ;
DEFAULT_NETTYPE : '`default_nettype' ;
DEFINE : '`define' ;
ELSE : '`else' ;
ELSIF : '`elsif' ;
END_KEYWORDS : '`end_keywords' ;
ENDCELLDEFINE : '`endcelldefine' ;
ENDIF : '`endif' ;
IFDEF : '`ifdef' ;
IFNDEF : '`ifndef' ;
INCLUDE : '`include' ;
LINE : '`line' ;
NOUNCONNECTED_DRIVE : '`nounconnected_drive' ;
PRAGMA : '`pragma' ;
RESETALL : '`resetall' ;
TIMESCALE : '`timescale' ;
UNCONNECTED_DRIVE : '`unconnected_drive' ;
UNDEF : '`undef' ;

// Numbers

EXP : E ;
SIGN : PLUS | MINUS ;
SIZE : NON_ZERO_UNSIGNED_NUMBER ;
fragment NON_ZERO_UNSIGNED_NUMBER : NON_ZERO_DECIMAL_DIGIT (UNDERSCORE | DECIMAL_DIGIT)* ;
UNSIGNED_NUMBER : DECIMAL_DIGIT (UNDERSCORE | DECIMAL_DIGIT)* ;
DECIMAL_VALUE : UNSIGNED_NUMBER | (X_DIGIT | Z_DIGIT) UNDERSCORE* ;
BINARY_VALUE : BINARY_DIGIT (UNDERSCORE | BINARY_DIGIT)* ;
OCTAL_VALUE : OCTAL_DIGIT (UNDERSCORE | OCTAL_DIGIT)* ;
HEX_VALUE : HEX_DIGIT (UNDERSCORE | HEX_DIGIT)* ;
DECIMAL_BASE : APOSTROPHE S? D ;
BINARY_BASE : APOSTROPHE S? B ;
OCTAL_BASE : APOSTROPHE S? O ;
HEX_BASE : APOSTROPHE S? H ;
fragment NON_ZERO_DECIMAL_DIGIT : [1-9] ;
fragment DECIMAL_DIGIT : [0-9] ;
fragment BINARY_DIGIT : X_DIGIT | Z_DIGIT | [01] ;
fragment OCTAL_DIGIT : X_DIGIT | Z_DIGIT | [0-7] ;
fragment HEX_DIGIT : X_DIGIT | Z_DIGIT | [0-9a-fA-F] ;
fragment X_DIGIT : X ;
fragment Z_DIGIT : [zZ?] ;
fragment APOSTROPHE : '\'' ;

// Strings

STRING : DOUBLE_QUOTE ~["\r\n]* DOUBLE_QUOTE ;
DOUBLE_QUOTE : '"' ;

// Comments

ONE_LINE_COMMENT : DOUBLE_SLASH MATCH_EVERYTHING_NON_GREEDY CARRIAGE_RETURN? LINE_FEED -> channel(HIDDEN) ;
BLOCK_COMMENT : SLASH_ASTERISK MATCH_EVERYTHING_NON_GREEDY ASTERISK_SLASH -> channel(HIDDEN) ;
fragment DOUBLE_SLASH : '//' ;
fragment SLASH_ASTERISK : '/*' ;
fragment ASTERISK_SLASH : '*/' ;

// Identifiers

ESCAPED_IDENTIFIER : BACKSLASH ('\u0021'..'\u007E')+ ~[ \t\r\n]* ;
SIMPLE_IDENTIFIER : (LETTER | UNDERSCORE) (LETTER | UNDERSCORE | DECIMAL_DIGIT | DOLLAR_SIGN)* ;
SYSTEM_TF_IDENTIFIER : DOLLAR_SIGN (LETTER | UNDERSCORE | DECIMAL_DIGIT | DOLLAR_SIGN) (LETTER | UNDERSCORE | DECIMAL_DIGIT | DOLLAR_SIGN)* ;
UNDERSCORE : '_' ;
DOLLAR_SIGN : '$' ;

// White space

WHITE_SPACE_REGION : WHITE_SPACE+ -> channel(HIDDEN) ;
fragment WHITE_SPACE : SPACE | TAB | CARRIAGE_RETURN | LINE_FEED ;

// Separators

LEFT_PARENTHESIS : '(' ;
RIGHT_PARENTHESIS : '(' ;
LEFT_BRACKET : '[' ;
RIGHT_BRACKET : ']' ;
LEFT_BRACE : '{' ;
RIGHT_BRACE : '}' ;

// Operator symbols

PLUS : '+' ;
MINUS : '-' ;
EXCLAMATION_MARK : '!' ;
TILDE : '~' ;
AMPERSAND : '&' ;
TILDE_AMPERSAND : '~&' ;
VERTICAL_BAR : '|' ;
TILDE_VERTICAL_BAR : '~|' ;
CARET : '^' ;
TILDE_CARET : '~^' ;
CARET_TILDE : '^~' ;
ASTERISK : '*' ;
PERCENT : '%' ;
DOUBLE_EQUAL : '==' ;
EXCLAMATION_MARK_EQUAL : '!=' ;
TRIPLE_EQUAL : '===' ;
EXCLAMATION_MARK_DOUBLE_EQUAL : '!==' ;
DOUBLE_AMPERSAND : '&&' ;
DOUBLE_VERTICAL_BAR : '||' ;
DOUBLE_ASTERISK : '**' ;
LESS_THAN : '<' ;
LESS_THAN_EQUAL : '<=' ;
GREATER_THAN : '>' ;
GREATER_THAN_EQUAL : '>=' ;
DOUBLE_GREATER_THAN : '>>' ;
DOUBLE_LESS_THAN : '<<' ;
TRIPLE_GREATER_THAN : '>>>' ;
TRIPLE_LESS_THAN : '<<<' ;

// Other symbols

DOT : '.' ;
COMMA : ',' ;
COLON : ':' ;
SEMICOLON : ';' ;
SLASH : '/' ;
EQUAL : '=' ;
QUESTION_MARK : '?' ;
HASH : '#' ;
GRAVE_ACCENT : '`' ;
PLUS_COLON : '+:' ;
MINUS_COLON : '-:' ;
MINUS_GREATER_THAN : '->' ;
EQUAL_GREATER_THAN : '=>' ;
ASTERISK_GREATER_THAN : '*>' ;
TRIPLE_AMPERSAND : '&&&' ;

// Context-sensitive rules (special lexical modes are required to handle these exceptions)

FILE_PATH_SPEC : ~[ \t\r\n]+ ;
FILE_NAME : ~[/\\?%':|"<>,;=]+ ;
MTM_SPEC : 'MAXIMUM' | 'MINIMUM' | 'TOOL_CONTROL' | 'TYPICAL' ;
SCALE_TYPE : 'FROM_MAXIMUM' | 'FROM_MINIMUM' | 'FROM_MTM' | 'FROM_TYPICAL' ;
UNITS_NUMBER : '0' | MINUS [1-15] ;
FINISH_NUMBER : [0-2] ;
NONE : 'none' ;
TIME_LITERAL : UNSIGNED_NUMBER TIME_UNIT ;
fragment TIME_UNIT : [mnpf]? 's' ;
TEXT : MATCH_EVERYTHING_NON_GREEDY ~'\\' NEWLINE ;
TYPE : 'r' | 'rb' | 'w' | 'wb' | 'a' | 'ab' | 'r+' | 'r+b' | 'rb+' | 'w+' | 'w+b' | 'wb+' | 'a+' | 'a+b' | 'ab+' ;
LEVEL : [0-2] ;
VERSION_SPECIFIER : '1364-1995' | '1364-2001' | '1364-2001-noconfig' | '1364-2005' ;
MINUS_INCDIR : '-incdir' ;
COLON_CONFIG : ':config' ;
PATHPULSE : 'PATHPULSE$' ;
INIT_VAL : '1' APOSTROPHE B (ZERO_OR_ONE | X) | ZERO_OR_ONE ;
OUTPUT_SYMBOL : [01xX] ;
LEVEL_SYMBOL : [01xX?bB] ;
EDGE_SYMBOL : [rRfFpPnN*] ;
EDGE_DESCRIPTOR : '01' | '10' | Z_OR_X ZERO_OR_ONE | ZERO_OR_ONE Z_OR_X ;
fragment ZERO_OR_ONE : [01] ;
fragment Z_OR_X : X | Z ;
SCALAR_CONSTANT : '1' APOSTROPHE B ZERO_OR_ONE | ZERO_OR_ONE ;

// Escape sequences

fragment NEWLINE : CARRIAGE_RETURN? LINE_FEED ;
fragment SPACE : ' ' ;
fragment TAB : '\t' ;
fragment CARRIAGE_RETURN : '\r' ;
fragment LINE_FEED : '\n' ;
fragment BACKSLASH : '\\' ;

// Letters

fragment LETTER : [a-zA-Z] ;
fragment A : [aA] ;
fragment B : [bB] ;
fragment C : [cC] ;
fragment D : [dD] ;
fragment E : [eE] ;
fragment F : [fF] ;
fragment G : [gG] ;
fragment H : [hH] ;
fragment I : [iI] ;
fragment J : [jJ] ;
fragment K : [kK] ;
fragment L : [lL] ;
fragment M : [mM] ;
fragment N : [nN] ;
fragment O : [oO] ;
fragment P : [pP] ;
fragment Q : [qQ] ;
fragment R : [rR] ;
fragment S : [sS] ;
fragment T : [tT] ;
fragment U : [uU] ;
fragment V : [vV] ;
fragment W : [wW] ;
fragment X : [xX] ;
fragment Y : [yY] ;
fragment Z : [zZ] ;

// Other fragments

fragment MATCH_EVERYTHING_NON_GREEDY : .*? ;
