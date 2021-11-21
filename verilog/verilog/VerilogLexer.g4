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
EDGE : 'edge' -> mode(EDGE_CONTROL_SPECIFIER_MODE) ;
ELSE : 'else' ;
END : 'end' ;
ENDCASE : 'endcase' ;
ENDCONFIG : 'endconfig' ;
ENDFUNCTION : 'endfunction' ;
ENDGENERATE : 'endgenerate' ;
ENDMODULE : 'endmodule' ;
ENDPRIMITIVE : 'endprimitive' -> mode(DEFAULT_MODE) ;
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
INCLUDE : 'include' -> mode(LIBRARY_SOURCE_TEXT_MODE) ;
INITIAL : 'initial' ;
INOUT : 'inout' ;
INPUT : 'input' ;
INSTANCE : 'instance' ;
INTEGER : 'integer' ;
JOIN : 'join' ;
LARGE : 'large' ;
LIBLIST : 'liblist' ;
LIBRARY : 'library' -> mode(LIBRARY_SOURCE_TEXT_MODE) ;
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
PRIMITIVE : 'primitive' -> mode(UDP_MODE) ;
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
// Display tasks
DOLLAR_DISPLAY : '$display' ;
DOLLAR_DISPLAYB : '$displayb' ;
DOLLAR_DISPLAYH : '$displayh' ;
DOLLAR_DISPLAYO : '$displayo' ;
DOLLAR_WRITE : '$write' ;
DOLLAR_WRITEB : '$writeb' ;
DOLLAR_WRITEH : '$writeh' ;
DOLLAR_WRITEO : '$writeo' ;
DOLLAR_STROBE : '$strobe' ;
DOLLAR_STROBEB : '$strobeb' ;
DOLLAR_STROBEH : '$strobeh' ;
DOLLAR_STROBEO : '$strobeo' ;
DOLLAR_MONITOR : '$monitor' ;
DOLLAR_MONITORB : '$monitorb' ;
DOLLAR_MONITORH : '$monitorh' ;
DOLLAR_MONITORO : '$monitoro' ;
DOLLAR_MONITOROFF : '$monitoroff' ;
DOLLAR_MONITORON : '$monitoron' ;
// File I/O tasks
DOLLAR_FCLOSE : '$fclose' ;
DOLLAR_FDISPLAY : '$fdisplay' ;
DOLLAR_FDISPLAYB : '$fdisplayb' ;
DOLLAR_FDISPLAYH : '$fdisplayh' ;
DOLLAR_FDISPLAYO : '$fdisplayo' ;
DOLLAR_FSTROBE : '$fstrobe' ;
DOLLAR_FSTROBEB : '$fstrobeb' ;
DOLLAR_FSTROBEH : '$fstrobeh' ;
DOLLAR_FSTROBEO : '$fstrobeo' ;
DOLLAR_SWRITE : '$swrite' ;
DOLLAR_SWRITEB : '$swriteb' ;
DOLLAR_SWRITEH : '$swriteh' ;
DOLLAR_SWRITEO : '$swriteo' ;
DOLLAR_FSCANF : '$fscanf' ;
DOLLAR_FREAD : '$fread' ;
DOLLAR_FSEEK : '$fseek' ;
DOLLAR_FFLUSH : '$fflush' ;
DOLLAR_FEOF : '$feof' ;
DOLLAR_SDF_ANNOTATE : '$sdf_annotate' ;
DOLLAR_FOPEN : '$fopen' -> mode(FOPEN_TASK_MODE) ;
DOLLAR_FWRITE : '$fwrite' ;
DOLLAR_FWRITEB : '$fwriteb' ;
DOLLAR_FWRITEH : '$fwriteh' ;
DOLLAR_FWRITEO : '$fwriteo' ;
DOLLAR_FMONITOR : '$fmonitor' ;
DOLLAR_FMONITORB : '$fmonitorb' ;
DOLLAR_FMONITORH : '$fmonitorh' ;
DOLLAR_FMONITORO : '$fmonitoro' ;
DOLLAR_SFORMAT : '$sformat' ;
DOLLAR_FGETC : '$fgetc' ;
DOLLAR_UNGETC : '$ungetc' ;
DOLLAR_FGETS : '$fgets' ;
DOLLAR_SSCANF : '$sscanf' ;
DOLLAR_REWIND : '$rewind' ;
DOLLAR_FTELL : '$ftell' ;
DOLLAR_FERROR : '$ferror' ;
DOLLAR_READMEMB : '$readmemb' ;
DOLLAR_READMEMH : '$readmemh' ;
// Timescale tasks
DOLLAR_PRINTTIMESCALE : '$printtimescale' ;
DOLLAR_TIMEFORMAT : '$timeformat' ;
// Simulation control tasks
DOLLAR_FINISH : '$finish' -> mode(SIMULATION_CONTROL_TASK_MODE) ;
DOLLAR_STOP : '$stop' -> mode(SIMULATION_CONTROL_TASK_MODE) ;
// PLA modeling tasks
DOLLAR_ASYNC_AND_ARRAY : '$async$and$array' ;
DOLLAR_ASYNC_NAND_ARRAY : '$async$nand$array' ;
DOLLAR_ASYNC_OR_ARRAY : '$async$or$array' ;
DOLLAR_ASYNC_NOR_ARRAY : '$async$nor$array' ;
DOLLAR_SYNC_AND_ARRAY : '$sync$and$array' ;
DOLLAR_SYNC_NAND_ARRAY : '$sync$nand$array' ;
DOLLAR_SYNC_OR_ARRAY : '$sync$or$array' ;
DOLLAR_SYNC_NOR_ARRAY : '$sync$nor$array' ;
DOLLAR_ASYNC_AND_PLANE : '$async$and$plane' ;
DOLLAR_ASYNC_NAND_PLANE : '$async$nand$plane' ;
DOLLAR_ASYNC_OR_PLANE : '$async$or$plane' ;
DOLLAR_ASYNC_NOR_PLANE : '$async$nor$plane' ;
DOLLAR_SYNC_AND_PLANE : '$sync$and$plane' ;
DOLLAR_SYNC_NAND_PLANE : '$sync$nand$plane' ;
DOLLAR_SYNC_OR_PLANE : '$sync$or$plane' ;
DOLLAR_SYNC_NOR_PLANE : '$sync$nor$plane' ;
// Stochastic analysis tasks
DOLLAR_Q_INITIALIZE : '$q_initialize' ;
DOLLAR_Q_REMOVE : '$q_remove' ;
DOLLAR_Q_EXAM : '$q_exam' ;
DOLLAR_Q_ADD : '$q_add' ;
DOLLAR_Q_FULL : '$q_full' ;
// Simulation time functions
DOLLAR_REALTIME : '$realtime' ;
DOLLAR_TIME : '$time' ;
DOLLAR_STIME : '$stime' ;
// Conversion functions
DOLLAR_BITSTOREAL : '$bitstoreal' ;
DOLLAR_ITOR : '$itor' ;
DOLLAR_SIGNED : '$signed' ;
DOLLAR_REALTOBITS : '$realtobits' ;
DOLLAR_RTOI : '$rtoi' ;
DOLLAR_UNSIGNED : '$unsigned' ;
// Probabilistic distribution functions
DOLLAR_RANDOM : '$random' ;
DOLLAR_DIST_ERLANG : '$dist_erlang' ;
DOLLAR_DIST_NORMAL : '$dist_normal' ;
DOLLAR_DIST_T : '$dist_t' ;
DOLLAR_DIST_CHI_SQUARE : '$dist_chi_square' ;
DOLLAR_DIST_EXPONENTIAL : '$dist_exponential' ;
DOLLAR_DIST_POISSON : '$dist_poisson' ;
DOLLAR_DIST_UNIFORM : '$dist_uniform' ;
// Command line input
DOLLAR_TEST_PLUSARGS : '$test$plusargs' ;
DOLLAR_VALUE_PLUSARGS : '$value$plusargs' ;
// Math functions
DOLLAR_CLOG2 : '$clog2' ;
DOLLAR_LN : '$ln' ;
DOLLAR_LOG10 : '$log10' ;
DOLLAR_EXP : '$exp' ;
DOLLAR_SQRT : '$sqrt' ;
DOLLAR_POW : '$pow' ;
DOLLAR_FLOOR : '$floor' ;
DOLLAR_CEIL : '$ceil' ;
DOLLAR_SIN : '$sin' ;
DOLLAR_COS : '$cos' ;
DOLLAR_TAN : '$tan' ;
DOLLAR_ASIN : '$asin' ;
DOLLAR_ACOS : '$acos' ;
DOLLAR_ATAN : '$atan' ;
DOLLAR_ATAN2 : '$atan2' ;
DOLLAR_HYPOT : '$hypot' ;
DOLLAR_SINH : '$sinh' ;
DOLLAR_COSH : '$cosh' ;
DOLLAR_TANH : '$tanh' ;
DOLLAR_ASINH : '$asinh' ;
DOLLAR_ACOSH : '$acosh' ;
DOLLAR_ATANH : '$atanh' ;
// Value change dump (VCD) functions
DOLLAR_DUMPFILE : '$dumpfile' ;
DOLLAR_DUMPVARS : '$dumpvars' ;
DOLLAR_DUMPOFF : '$dumpoff' ;
DOLLAR_DUMPON : '$dumpon' ;
DOLLAR_DUMPALL : '$dumpall' ;
DOLLAR_DUMPLIMIT : '$dumplimit' ;
DOLLAR_DUMPFLUSH : '$dumpflush' ;
DOLLAR_END : '$end' -> mode(DEFAULT_MODE) ;
DOLLAR_COMMENT : '$comment' -> mode(VCD_MODE) ;
DOLLAR_DATE : '$date' -> mode(VCD_MODE) ;
DOLLAR_ENDDEFINITIONS : '$enddefinitions' -> mode(VCD_MODE) ;
DOLLAR_SCOPE : '$scope' -> mode(VCD_MODE) ;
DOLLAR_TIMESCALE : '$timescale' -> mode(VCD_MODE) ;
DOLLAR_UPSCOPE : '$upscope' -> mode(VCD_MODE) ;
DOLLAR_VAR : '$var' -> mode(VCD_MODE) ;
DOLLAR_VERSION : '$version' -> mode(VCD_MODE) ;
DOLLAR_DUMPPORTS : '$dumpports' -> mode(VCD_MODE) ;
DOLLAR_DUMPPORTSOFF : '$dumpportsoff' -> mode(VCD_MODE) ;
DOLLAR_DUMPPORTSON : '$dumpportson' -> mode(VCD_MODE) ;
DOLLAR_DUMPPORTSALL : '$dumpportsall' -> mode(VCD_MODE) ;
DOLLAR_DUMPPORTSLIMIT : '$dumpportslimit' ;
DOLLAR_DUMPPORTSFLUSH : '$dumpportsflush' ;
DOLLAR_VCDCLOSE : '$vcdclose' -> mode(VCD_MODE) ;

// System timing check commands

DOLLAR_SETUP : '$setup' ;
DOLLAR_HOLD : '$hold' ;
DOLLAR_SETUPHOLD : '$setuphold' ;
DOLLAR_RECOVERY : '$recovery' ;
DOLLAR_REMOVAL : '$removal' ;
DOLLAR_RECREM : '$recrem' ;
DOLLAR_SKEW : '$skew' ;
DOLLAR_TIMESKEW : '$timeskew' ;
DOLLAR_FULLSKEW : '$fullskew' ;
DOLLAR_PERIOD : '$period' ;
DOLLAR_WIDTH : '$width' ;
DOLLAR_NOCHANGE : '$nochange' ;

// Compiler directives

GRAVE_BEGIN_KEYWORDS : '`begin_keywords' ;
GRAVE_CELLDEFINE : '`celldefine' ;
GRAVE_DEFAULT_NETTYPE : '`default_nettype' ;
GRAVE_DEFINE : '`define' -> mode(DEFINE_DIRECTIVE_MODE) ;
GRAVE_ELSE : '`else' -> mode(CONDITIONAL_DIRECTIVE_MODE) ;
GRAVE_ELSIF : '`elsif' -> mode(CONDITIONAL_DIRECTIVE_MODE) ;
GRAVE_END_KEYWORDS : '`end_keywords' ;
GRAVE_ENDCELLDEFINE : '`endcelldefine' ;
GRAVE_ENDIF : '`endif' -> mode(DEFAULT_MODE) ;
GRAVE_IFDEF : '`ifdef' -> mode(CONDITIONAL_DIRECTIVE_MODE) ;
GRAVE_IFNDEF : '`ifndef' -> mode(CONDITIONAL_DIRECTIVE_MODE) ;
GRAVE_INCLUDE : '`include' ;
GRAVE_LINE : '`line' -> mode(LINE_DIRECTIVE_MODE) ;
GRAVE_NOUNCONNECTED_DRIVE : '`nounconnected_drive' ;
GRAVE_PRAGMA : '`pragma' ;
GRAVE_RESETALL : '`resetall' ;
GRAVE_TIMESCALE : '`timescale' ;
GRAVE_UNCONNECTED_DRIVE : '`unconnected_drive' ;
GRAVE_UNDEF : '`undef' ;

// Numbers

REAL_NUMBER : UNSIGNED_NUMBER DOT UNSIGNED_NUMBER | UNSIGNED_NUMBER (DOT UNSIGNED_NUMBER)? EXP SIGN? UNSIGNED_NUMBER ;
fragment EXP : [eE] ;
DECIMAL_NUMBER : UNSIGNED_NUMBER | SIZE? DECIMAL_BASE DECIMAL_VALUE ;
BINARY_NUMBER : SIZE? BINARY_BASE BINARY_VALUE ;
OCTAL_NUMBER : SIZE? OCTAL_BASE OCTAL_VALUE ;
HEX_NUMBER : SIZE? HEX_BASE HEX_VALUE ;
fragment SIGN : PLUS | MINUS ;
fragment SIZE : NON_ZERO_UNSIGNED_NUMBER ;
fragment NON_ZERO_UNSIGNED_NUMBER : NON_ZERO_DECIMAL_DIGIT (UNDERSCORE | DECIMAL_DIGIT)* ;
UNSIGNED_NUMBER : DECIMAL_DIGIT (UNDERSCORE | DECIMAL_DIGIT)* ;
fragment DECIMAL_VALUE : UNSIGNED_NUMBER | (X_DIGIT | Z_DIGIT) UNDERSCORE* ;
fragment BINARY_VALUE : BINARY_DIGIT (UNDERSCORE | BINARY_DIGIT)* ;
fragment OCTAL_VALUE : OCTAL_DIGIT (UNDERSCORE | OCTAL_DIGIT)* ;
fragment HEX_VALUE : HEX_DIGIT (UNDERSCORE | HEX_DIGIT)* ;
fragment DECIMAL_BASE : APOSTROPHE [sS]? [dD] ;
fragment BINARY_BASE : APOSTROPHE [sS]? [bB] ;
fragment OCTAL_BASE : APOSTROPHE [sS]? [oO] ;
fragment HEX_BASE : APOSTROPHE [sS]? [hH] ;
fragment NON_ZERO_DECIMAL_DIGIT : [1-9] ;
fragment DECIMAL_DIGIT : [0-9] ;
fragment BINARY_DIGIT : X_DIGIT | Z_DIGIT | [01] ;
fragment OCTAL_DIGIT : X_DIGIT | Z_DIGIT | [0-7] ;
fragment HEX_DIGIT : X_DIGIT | Z_DIGIT | [0-9a-fA-F] ;
fragment X_DIGIT : [xX] ;
fragment Z_DIGIT : [zZ?] ;
fragment APOSTROPHE : '\'' ;

// Strings

STRING : DOUBLE_QUOTE ~["\r\n]* DOUBLE_QUOTE ;
DOUBLE_QUOTE : '"' ;

// Comments

ONE_LINE_COMMENT : DOUBLE_SLASH TEXT NEWLINE -> channel(HIDDEN) ;
BLOCK_COMMENT : SLASH_ASTERISK TEXT ASTERISK_SLASH -> channel(HIDDEN) ;
fragment TEXT : .*? ;
fragment DOUBLE_SLASH : '//' ;
fragment SLASH_ASTERISK : '/*' ;
fragment ASTERISK_SLASH : '*/' ;
fragment NEWLINE : CARRIAGE_RETURN? LINE_FEED ;

// Identifiers

ESCAPED_IDENTIFIER : BACKSLASH ASCII_PRINTABLE_EXCEPT_SPACE+ WHITE_SPACE ;
SIMPLE_IDENTIFIER : (LETTER | UNDERSCORE) (LETTER | UNDERSCORE | DECIMAL_DIGIT | DOLLAR_SIGN)* ;
SYSTEM_TF_IDENTIFIER : DOLLAR_SIGN (LETTER | UNDERSCORE | DECIMAL_DIGIT | DOLLAR_SIGN) (LETTER | UNDERSCORE | DECIMAL_DIGIT | DOLLAR_SIGN)* ;
fragment ASCII_PRINTABLE_EXCEPT_SPACE : [\u0021-\u007e] ;
fragment UNDERSCORE : '_' ;
fragment DOLLAR_SIGN : '$' ;
fragment BACKSLASH : '\\' ;
fragment LETTER : [a-zA-Z] ;

// White space

WHITE_SPACE_REGION : WHITE_SPACE+ -> channel(HIDDEN) ;
fragment WHITE_SPACE : SPACE | TAB | CARRIAGE_RETURN | LINE_FEED ;
fragment SPACE : ' ' ;
fragment TAB : '\t' ;
fragment CARRIAGE_RETURN : '\r' ;
fragment LINE_FEED : '\n' ;

// Separators

LEFT_PARENTHESIS : '(' ;
RIGHT_PARENTHESIS : ')' ;
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
SEMICOLON : ';' -> mode(DEFAULT_MODE) ;
SLASH : '/' ;
EQUAL : '=' ;
QUESTION_MARK : '?' ;
AT : '@' ;
HASH : '#' ;
GRAVE_ACCENT : '`' -> mode(MACRO_USAGE_MODE) ;
PLUS_COLON : '+:' ;
MINUS_COLON : '-:' ;
MINUS_GREATER_THAN : '->' ;
EQUAL_GREATER_THAN : '=>' ;
ASTERISK_GREATER_THAN : '*>' ;
TRIPLE_AMPERSAND : '&&&' ;

// 19.2 `default_nettype
NONE : 'none' ;
// 19.8 `timescale
TIME_LITERAL : TIME_NUMBER TIME_UNIT ;
// 19.11 `begin_keywords, `end_keywords
VERSION_SPECIFIER : VERSION_IDENTIFIER ;
// A.1.1 Library source text
MINUS_INCDIR : '-incdir' ;
// A.2.4 Declaration assignments
PATHPULSE_DOLLAR : 'PATHPULSE$' ;

mode FOPEN_TASK_MODE;
// 17.2.1 Opening and closing files
TYPE : DOUBLE_QUOTE ('r' | 'rb' | 'w' | 'wb' | 'a' | 'ab' | 'r+' | 'r+b' | 'rb+' | 'w+' | 'w+b' | 'wb+' | 'a+' | 'a+b' | 'ab+') DOUBLE_QUOTE ;

mode SIMULATION_CONTROL_TASK_MODE;
// 17.4 Simulation control system tasks
FINISH_NUMBER : [0-2] ;

mode VCD_MODE;
// 18.4 Format of extended VCD file
COMMAND_TEXT : COMMENT_TEXT | CLOSE_TEXT | DATE_SECTION | SCOPE_SECTION | TIMESCALE_SECTION | VAR_SECTION | VERSION_SECTION ;
fragment SIMULATION_TIME : HASH DECIMAL_NUMBER ;
VALUE_CHANGE : VALUE IDENTIFIER_CODE ;
fragment VALUE : PORT_VALUE | STRENGTH_COMPONENT ;
fragment PORT_VALUE : INPUT_VALUE | OUTPUT_VALUE | UNKNOWN_DIRECTION_VALUE ;
fragment INPUT_VALUE : [dDNuUZ] ;
fragment OUTPUT_VALUE : [HlLTX] ;
fragment UNKNOWN_DIRECTION_VALUE : [01?aAbBcCfF] ;
fragment STRENGTH_COMPONENT : [0-7] ;
fragment IDENTIFIER_CODE : IDENTIFIER ;
fragment COMMENT_TEXT : ASCII_ANY+ ;
fragment ASCII_ANY : [\u0000-\u007f] ;
fragment CLOSE_TEXT : SIMULATION_TIME ;
fragment DATE_SECTION : DATE_TEXT ;
fragment DATE_TEXT : (DAY MONTH COMMA? YEAR | YEAR COMMA? MONTH DAY | MONTH DAY COMMA? YEAR) HOUR COLON MINUTE COLON SECOND ;
fragment DAY : DECIMAL_DIGIT DECIMAL_DIGIT? ;
fragment MONTH : MONTH_FULL | MONTH_ABBREVIATION ;
fragment MONTH_FULL : 'January' | 'February' | 'March' | 'April' | 'May' | 'June' | 'July' | 'August' | 'September' | 'October' | 'November' | 'December';
fragment MONTH_ABBREVIATION : 'Jan' | 'Feb' | 'Mar' | 'Apr' | 'May' | 'Jun' | 'Jul' | 'Aug' | 'Sep' | 'Oct' | 'Nov' | 'Dec' ;
fragment YEAR : DECIMAL_DIGIT DECIMAL_DIGIT DECIMAL_DIGIT DECIMAL_DIGIT ;
fragment HOUR : DECIMAL_DIGIT DECIMAL_DIGIT ;
fragment MINUTE : DECIMAL_DIGIT DECIMAL_DIGIT ;
fragment SECOND : DECIMAL_DIGIT DECIMAL_DIGIT ;
fragment SCOPE_SECTION : SCOPE_TYPE SCOPE_IDENTIFIER ;
fragment SCOPE_TYPE : MODULE ;
fragment SCOPE_IDENTIFIER : IDENTIFIER ;
fragment TIMESCALE_SECTION : TIME_NUMBER TIME_UNIT ;
fragment TIME_NUMBER : '1' | '10' | '100' ;
fragment TIME_UNIT : [mnpf]? 's' ;
fragment VAR_SECTION : VAR_TYPE VAR_SIZE IDENTIFIER_CODE REFERENCE ;
fragment VAR_TYPE : 'port' ;
fragment VAR_SIZE : '1' | VECTOR_INDEX ;
fragment VECTOR_INDEX : LEFT_BRACKET INDEX COLON INDEX RIGHT_BRACKET ;
fragment INDEX : DECIMAL_NUMBER ;
fragment REFERENCE : IDENTIFIER ;
fragment IDENTIFIER : ASCII_PRINTABLE_EXCEPT_SPACE+ ;
fragment VERSION_SECTION : VERSION_TEXT ;
fragment VERSION_TEXT : VERSION_IDENTIFIER ;
fragment VERSION_IDENTIFIER : '1364-1995' | '1364-2001' | '1364-2001-noconfig' | '1364-2005' ;

mode DEFINE_DIRECTIVE_MODE;
// 19.3.1 `define
MACRO_TEXT : TEXT ~'\\' NEWLINE -> mode(DEFAULT_MODE) ;

mode MACRO_USAGE_MODE;
// 19.3.1 `define
LIST_OF_ACTUAL_ARGUMENTS : ACTUAL_ARGUMENT (COMMA ACTUAL_ARGUMENT)* -> mode(DEFAULT_MODE) ;
fragment ACTUAL_ARGUMENT : TEXT ;

mode CONDITIONAL_DIRECTIVE_MODE;
// 19.4 `ifdef, `else, `elsif, `endif , `ifndef
GROUP_OF_LINES : TEXT -> mode(DEFAULT_MODE) ;

mode LINE_DIRECTIVE_MODE;
// 19.7 `line
LEVEL : [0-2] -> mode(DEFAULT_MODE) ;

mode LIBRARY_SOURCE_TEXT_MODE;
// A.1.1 Library source text
FILE_PATH_SPEC : ~[ \t\r\n]+ ;

mode UDP_MODE;
// A.5.3 UDP body
INIT_VAL : '1' APOSTROPHE [bB] [01xX] | [01] ;
OUTPUT_SYMBOL : [01xX] ;
LEVEL_SYMBOL : [01xX?bB] ;
EDGE_SYMBOL : [rRfFpPnN*] ;

mode EDGE_CONTROL_SPECIFIER_MODE;
// A.7.5.3 System timing check event definitions
EDGE_DESCRIPTOR : '01' | '10' | [xXzZ] [01] | [01] [xXzZ] ;
SCALAR_CONSTANT : '1' APOSTROPHE [bB] [01] | [01] ;
