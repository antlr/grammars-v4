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
INCLUDE : 'include' -> mode(FILE_PATH_MODE) ;
INITIAL : 'initial' ;
INOUT : 'inout' ;
INPUT : 'input' ;
INSTANCE : 'instance' ;
INTEGER : 'integer' ;
JOIN : 'join' ;
LARGE : 'large' ;
LIBLIST : 'liblist' ;
LIBRARY : 'library' -> mode(FILE_PATH_MODE) ;
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
SPECPARAM : 'specparam' -> mode(SPECPARAM_MODE) ;
STRONG0 : 'strong0' ;
STRONG1 : 'strong1' ;
SUPPLY0 : 'supply0' ;
SUPPLY1 : 'supply1' ;
TABLE : 'table' -> mode(TABLE_MODE) ;
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
USE : 'use' -> mode(USE_CLAUSE_MODE) ;
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
DOLLAR_SDF_ANNOTATE : '$sdf_annotate' -> mode(FILE_READ_MODE) ;
DOLLAR_FOPEN : '$fopen' -> mode(FILE_READ_MODE);
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
DOLLAR_READMEMB : '$readmemb' -> mode(FILE_READ_MODE) ;
DOLLAR_READMEMH : '$readmemh' -> mode(FILE_READ_MODE) ;
DOLLAR_PRINTTIMESCALE : '$printtimescale' ;
DOLLAR_TIMEFORMAT : '$timeformat' -> mode(TIME_FORMAT_MODE) ;
DOLLAR_FINISH : '$finish' -> mode(SIMULATION_CONTROL_MODE) ;
DOLLAR_STOP : '$stop' -> mode(SIMULATION_CONTROL_MODE) ;
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
DOLLAR_Q_INITIALIZE : '$q_initialize' ;
DOLLAR_Q_REMOVE : '$q_remove' ;
DOLLAR_Q_EXAM : '$q_exam' ;
DOLLAR_Q_ADD : '$q_add' ;
DOLLAR_Q_FULL : '$q_full' ;
DOLLAR_REALTIME : '$realtime' ;
DOLLAR_TIME : '$time' ;
DOLLAR_STIME : '$stime' ;
DOLLAR_BITSTOREAL : '$bitstoreal' ;
DOLLAR_ITOR : '$itor' ;
DOLLAR_SIGNED : '$signed' ;
DOLLAR_REALTOBITS : '$realtobits' ;
DOLLAR_RTOI : '$rtoi' ;
DOLLAR_UNSIGNED : '$unsigned' ;
DOLLAR_RANDOM : '$random' ;
DOLLAR_DIST_ERLANG : '$dist_erlang' ;
DOLLAR_DIST_NORMAL : '$dist_normal' ;
DOLLAR_DIST_T : '$dist_t' ;
DOLLAR_DIST_CHI_SQUARE : '$dist_chi_square' ;
DOLLAR_DIST_EXPONENTIAL : '$dist_exponential' ;
DOLLAR_DIST_POISSON : '$dist_poisson' ;
DOLLAR_DIST_UNIFORM : '$dist_uniform' ;
DOLLAR_TEST_PLUSARGS : '$test$plusargs' ;
DOLLAR_VALUE_PLUSARGS : '$value$plusargs' ;
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

// System timing check commands

DOLLAR_SETUP : '$setup' -> mode(TIMING_CHECK_MODE) ;
DOLLAR_HOLD : '$hold' -> mode(TIMING_CHECK_MODE) ;
DOLLAR_SETUPHOLD : '$setuphold' -> mode(TIMING_CHECK_MODE) ;
DOLLAR_RECOVERY : '$recovery' -> mode(TIMING_CHECK_MODE) ;
DOLLAR_REMOVAL : '$removal' -> mode(TIMING_CHECK_MODE) ;
DOLLAR_RECREM : '$recrem' -> mode(TIMING_CHECK_MODE) ;
DOLLAR_SKEW : '$skew' -> mode(TIMING_CHECK_MODE) ;
DOLLAR_TIMESKEW : '$timeskew' -> mode(TIMING_CHECK_MODE) ;
DOLLAR_FULLSKEW : '$fullskew' -> mode(TIMING_CHECK_MODE) ;
DOLLAR_PERIOD : '$period' -> mode(TIMING_CHECK_MODE) ;
DOLLAR_WIDTH : '$width' -> mode(TIMING_CHECK_MODE) ;
DOLLAR_NOCHANGE : '$nochange' -> mode(TIMING_CHECK_MODE) ;

// Compiler directives

GRAVE_BEGIN_KEYWORDS : '`begin_keywords' -> mode(BEGIN_KEYWORDS_MODE) ;
GRAVE_CELLDEFINE : '`celldefine' ;
GRAVE_DEFAULT_NETTYPE : '`default_nettype' -> mode(DEFAULT_NETTYPE_MODE) ;
GRAVE_DEFINE : '`define' -> mode(DEFINE_MODE) ;
GRAVE_ELSE : '`else' ;
GRAVE_ELSIF : '`elsif' ;
GRAVE_END_KEYWORDS : '`end_keywords' ;
GRAVE_ENDCELLDEFINE : '`endcelldefine' ;
GRAVE_ENDIF : '`endif' ;
GRAVE_IFDEF : '`ifdef' ;
GRAVE_IFNDEF : '`ifndef' ;
GRAVE_INCLUDE : '`include' -> mode(FILE_READ_MODE) ;
GRAVE_LINE : '`line' -> mode(LINE_MODE) ;
GRAVE_NOUNCONNECTED_DRIVE : '`nounconnected_drive' ;
GRAVE_PRAGMA : '`pragma' ;
GRAVE_RESETALL : '`resetall' ;
GRAVE_TIMESCALE : '`timescale' -> mode(TIMESCALE_MODE) ;
GRAVE_UNCONNECTED_DRIVE : '`unconnected_drive' ;
GRAVE_UNDEF : '`undef' ;

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

ONE_LINE_COMMENT : DOUBLE_SLASH MATCH_EVERYTHING_NON_GREEDY NEWLINE -> channel(HIDDEN) ;
BLOCK_COMMENT : SLASH_ASTERISK MATCH_EVERYTHING_NON_GREEDY ASTERISK_SLASH -> channel(HIDDEN) ;
fragment DOUBLE_SLASH : '//' ;
fragment SLASH_ASTERISK : '/*' ;
fragment ASTERISK_SLASH : '*/' ;
fragment NEWLINE : CARRIAGE_RETURN? LINE_FEED ;

// Identifiers

ESCAPED_IDENTIFIER : BACKSLASH ('\u0021'..'\u007E')+ ~[ \t\r\n]* ;
SIMPLE_IDENTIFIER : (LETTER | UNDERSCORE) (LETTER | UNDERSCORE | DECIMAL_DIGIT | DOLLAR_SIGN)* ;
SYSTEM_TF_IDENTIFIER : DOLLAR_SIGN (LETTER | UNDERSCORE | DECIMAL_DIGIT | DOLLAR_SIGN) (LETTER | UNDERSCORE | DECIMAL_DIGIT | DOLLAR_SIGN)* ;
UNDERSCORE : '_' ;
DOLLAR_SIGN : '$' ;
fragment BACKSLASH : '\\' ;

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

// Context-sensitive rules

mode FILE_READ_MODE;
// 17.2 File input-output system tasks and functions | 19.5 `include
FILE_NAME : ~[/\\?%':|"<>,;=]+ -> mode(DEFAULT_MODE) ;
// 17.2.1 Opening and closing files
TYPE : ('r' | 'rb' | 'w' | 'wb' | 'a' | 'ab' | 'r+' | 'r+b' | 'rb+' | 'w+' | 'w+b' | 'wb+' | 'a+' | 'a+b' | 'ab+') -> mode(DEFAULT_MODE) ;
// 17.2.10 Loading timing data from an SDF file
MTM_SPEC : ('MAXIMUM' | 'MINIMUM' | 'TOOL_CONTROL' | 'TYPICAL') -> mode(DEFAULT_MODE) ;
SCALE_TYPE : ('FROM_MAXIMUM' | 'FROM_MINIMUM' | 'FROM_MTM' | 'FROM_TYPICAL') -> mode(DEFAULT_MODE) ;

mode TIME_FORMAT_MODE;
// 17.3.2 $timeformat
UNITS_NUMBER : ('0' | MINUS [1-15]) -> mode(DEFAULT_MODE) ;

mode SIMULATION_CONTROL_MODE;
// 17.4 Simulation control system tasks
FINISH_NUMBER : [0-2] -> mode(DEFAULT_MODE) ;

mode DEFAULT_NETTYPE_MODE;
// 19.2 `default_nettype
NONE : 'none' -> mode(DEFAULT_MODE) ;

mode DEFINE_MODE;
// 19.3 `define and `undef
TEXT : MATCH_EVERYTHING_NON_GREEDY ~'\\' NEWLINE -> mode(DEFAULT_MODE) ;

mode LINE_MODE;
// 19.7 `line
LEVEL : [0-2] -> mode(DEFAULT_MODE) ;

mode TIMESCALE_MODE;
// 19.8 `timescale
TIME_LITERAL : UNSIGNED_NUMBER TIME_UNIT -> mode(DEFAULT_MODE) ;
fragment TIME_UNIT : [mnpf]? 's' ;

mode BEGIN_KEYWORDS_MODE;
// 19.11 `begin_keywords, `end_keywords
VERSION_SPECIFIER : ('1364-1995' | '1364-2001' | '1364-2001-noconfig' | '1364-2005') -> mode(DEFAULT_MODE) ;

mode FILE_PATH_MODE;
// A.1.1 Library source text
FILE_PATH_SPEC : ~[ \t\r\n]+ -> mode(DEFAULT_MODE) ;
MINUS_INCDIR : '-incdir' -> mode(DEFAULT_MODE) ;

mode USE_CLAUSE_MODE;
// A.1.5 Configuration source text
COLON_CONFIG : ':config' -> mode(DEFAULT_MODE) ;

mode SPECPARAM_MODE;
// A.2.4 Declaration assignments
PATHPULSE : 'PATHPULSE$' -> mode(DEFAULT_MODE) ;

mode TABLE_MODE;
// A.5.3 UDP body
INIT_VAL : ('1' APOSTROPHE B (ZERO_OR_ONE | X) | ZERO_OR_ONE) -> mode(DEFAULT_MODE) ;
OUTPUT_SYMBOL : [01xX] -> mode(DEFAULT_MODE) ;
LEVEL_SYMBOL : [01xX?bB] -> mode(DEFAULT_MODE) ;
EDGE_SYMBOL : [rRfFpPnN*] -> mode(DEFAULT_MODE) ;

mode TIMING_CHECK_MODE;
// A.7.5.3 System timing check event definitions
EDGE_DESCRIPTOR : ('01' | '10' | Z_OR_X ZERO_OR_ONE | ZERO_OR_ONE Z_OR_X) -> mode(DEFAULT_MODE) ;
fragment ZERO_OR_ONE : [01] ;
fragment Z_OR_X : X | Z ;
SCALAR_CONSTANT : ('1' APOSTROPHE B ZERO_OR_ONE | ZERO_OR_ONE) -> mode(DEFAULT_MODE) ;

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
