// Author: Mustafa Said Ağca
// License: MIT

lexer grammar VerilogLexer;

channels { COMMENTS, DIRECTIVES }

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
DOLLAR_FOPEN : '$fopen' -> mode(FILE_OPEN_FUNCTION_MODE) ;
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
DOLLAR_END : '$end' ;
DOLLAR_COMMENT : '$comment' ;
DOLLAR_DATE : '$date' ;
DOLLAR_ENDDEFINITIONS : '$enddefinitions' ;
DOLLAR_SCOPE : '$scope' ;
DOLLAR_TIMESCALE : '$timescale' ;
DOLLAR_UPSCOPE : '$upscope' ;
DOLLAR_VAR : '$var' ;
DOLLAR_VERSION : '$version' ;
DOLLAR_DUMPPORTS : '$dumpports' ;
DOLLAR_DUMPPORTSOFF : '$dumpportsoff' ;
DOLLAR_DUMPPORTSON : '$dumpportson' ;
DOLLAR_DUMPPORTSALL : '$dumpportsall' ;
DOLLAR_DUMPPORTSLIMIT : '$dumpportslimit' ;
DOLLAR_DUMPPORTSFLUSH : '$dumpportsflush' ;
DOLLAR_VCDCLOSE : '$vcdclose' ;

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
fragment UNSIGNED_NUMBER : DECIMAL_DIGIT (UNDERSCORE | DECIMAL_DIGIT)* ;
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

ONE_LINE_COMMENT : DOUBLE_SLASH TEXT NEWLINE -> channel(COMMENTS) ;
BLOCK_COMMENT : SLASH_ASTERISK TEXT ASTERISK_SLASH -> channel(COMMENTS) ;
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
SEMICOLON : ';' ;
SLASH : '/' ;
EQUAL : '=' ;
QUESTION_MARK : '?' ;
AT : '@' ;
HASH : '#' ;
GRAVE_ACCENT : '`' -> channel(DIRECTIVES), mode(DIRECTIVE_MODE) ;
PLUS_COLON : '+:' ;
MINUS_COLON : '-:' ;
MINUS_GREATER_THAN : '->' ;
EQUAL_GREATER_THAN : '=>' ;
ASTERISK_GREATER_THAN : '*>' ;
TRIPLE_AMPERSAND : '&&&' ;

// A.2.4 Declaration assignments
PATHPULSE_DOLLAR : 'PATHPULSE$' ;

// Context-specific rules
/*
// A.5.3 UDP body
INIT_VAL : '1' APOSTROPHE [bB] [01xX] | [01] ;
OUTPUT_SYMBOL : [01xX] ;
LEVEL_SYMBOL : [01xX?bB] ;
EDGE_SYMBOL : [rRfFpPnN*] ;
// A.7.5.3 System timing check event definitions
EDGE_DESCRIPTOR : '01' | '10' | [xXzZ] [01] | [01] [xXzZ] ;
SCALAR_CONSTANT : '1' APOSTROPHE [bB] [01] | [01] ;
*/

mode LIBRARY_SOURCE_TEXT_MODE;
LIBRARY_IDENTIFIER : (LETTER | UNDERSCORE) (LETTER | UNDERSCORE | DECIMAL_DIGIT | DOLLAR_SIGN)* ;
FILE_PATH_SPEC : ~[ \t\r\n,;]+ ;
MINUS_INCDIR : '-incdir' ;
LIBRARY_WHITE_SPACE : WHITE_SPACE+ -> channel(HIDDEN) ;
LIBRARY_COMMA : ',' ;
LIBRARY_SEMICOLON : ';' -> mode(DEFAULT_MODE) ;

mode FILE_OPEN_FUNCTION_MODE;
TYPE : 'r' | 'rb' | 'w' | 'wb' | 'a' | 'ab' | 'r+' | 'r+b' | 'rb+' | 'w+' | 'w+b' | 'wb+' | 'a+' | 'a+b' | 'ab+' ;
FILE_OPEN_STRING : '"' ~["\r\n]* '"' ;
FILE_OPEN_WHITE_SPACE : WHITE_SPACE+ -> channel(HIDDEN) ;
FILE_OPEN_LEFT_PARENTHESIS : '(' ;
FILE_OPEN_RIGHT_PARENTHESIS : ')' ;
FILE_OPEN_COMMA : ',' ;
FILE_OPEN_SEMICOLON : ';' -> mode(DEFAULT_MODE) ;

mode SIMULATION_CONTROL_TASK_MODE;
FINISH_NUMBER : [0-2] ;
SIMULATION_CONTROL_WHITE_SPACE : WHITE_SPACE+ -> channel(HIDDEN) ;
SIMULATION_CONTROL_LEFT_PARENTHESIS : '(' ;
SIMULATION_CONTROL_RIGHT_PARENTHESIS : ')' ;
SIMULATION_CONTROL_SEMICOLON : ';' -> mode(DEFAULT_MODE) ;

mode DIRECTIVE_MODE;
// Compiler directives
DIRECTIVE_BEGIN_KEYWORDS : 'begin_keywords' [ \t]+ -> channel(DIRECTIVES), mode(BEGIN_KEYWORDS_DIRECTIVE_MODE) ;
DIRECTIVE_CELLDEFINE : 'celldefine' -> channel(DIRECTIVES), mode(DEFAULT_MODE) ;
DIRECTIVE_DEFAULT_NETTYPE : 'default_nettype' [ \t]+ -> channel(DIRECTIVES), mode(DEFAULT_NETTYPE_DIRECTIVE_MODE) ;
DIRECTIVE_DEFINE : 'define' [ \t]+ -> channel(DIRECTIVES), mode(DEFINE_DIRECTIVE_MODE) ;
DIRECTIVE_ELSE : 'else' [ \t]+ -> channel(DIRECTIVES), mode(VERILOG_TEXT_MODE) ;
DIRECTIVE_ELSIF : 'elsif' [ \t]+ -> channel(DIRECTIVES), mode(CONDITIONAL_DIRECTIVE_MODE)  ;
DIRECTIVE_END_KEYWORDS : 'end_keywords' -> channel(DIRECTIVES), mode(DEFAULT_MODE) ;
DIRECTIVE_ENDCELLDEFINE : 'endcelldefine' -> channel(DIRECTIVES), mode(DEFAULT_MODE) ;
DIRECTIVE_ENDIF : 'endif' -> channel(DIRECTIVES), mode(DEFAULT_MODE) ;
DIRECTIVE_IFDEF : 'ifdef' [ \t]+ -> channel(DIRECTIVES), mode(CONDITIONAL_DIRECTIVE_MODE) ;
DIRECTIVE_IFNDEF : 'ifndef' [ \t]+ -> channel(DIRECTIVES), mode(CONDITIONAL_DIRECTIVE_MODE) ;
DIRECTIVE_INCLUDE : 'include' [ \t]+ -> channel(DIRECTIVES), mode(INCLUDE_DIRECTIVE_MODE) ;
DIRECTIVE_LINE : 'line' [ \t]+ -> channel(DIRECTIVES), mode(LINE_DIRECTIVE_MODE) ;
DIRECTIVE_NOUNCONNECTED_DRIVE : 'nounconnected_drive' -> channel(DIRECTIVES), mode(DEFAULT_MODE) ;
DIRECTIVE_PRAGMA : 'pragma' [ \t]+ -> channel(DIRECTIVES), mode(PRAGMA_MODE) ;
DIRECTIVE_RESETALL : 'resetall' -> channel(DIRECTIVES), mode(DEFAULT_MODE) ;
DIRECTIVE_TIMESCALE : 'timescale' [ \t]+ -> channel(DIRECTIVES), mode(TIMESCALE_DIRECTIVE_MODE) ;
DIRECTIVE_UNCONNECTED_DRIVE : 'unconnected_drive' [ \t]+ -> channel(DIRECTIVES), mode(UNCONNECTED_DRIVE_DIRECTIVE_MODE) ;
DIRECTIVE_UNDEF : 'undef' [ \t]+ -> channel(DIRECTIVES), mode(UNDEF_DIRECTIVE_MODE) ;
// Other symbols
DIRECTIVE_IDENTIFIER : (LETTER | UNDERSCORE) (LETTER | UNDERSCORE | DECIMAL_DIGIT | DOLLAR_SIGN)* -> channel(DIRECTIVES) ;
DIRECTIVE_NEWLINE : NEWLINE -> channel(HIDDEN), mode(DEFAULT_MODE) ;
TEXT_MACRO_USAGE : [ \t]* '(' [ \t]* MACRO_LIST_OF_ACTUAL_ARGUMENTS [ \t]* ')' -> channel(DIRECTIVES), mode(DEFAULT_MODE) ;
fragment MACRO_LIST_OF_ACTUAL_ARGUMENTS : MACRO_ACTUAL_ARGUMENT ([ \t]* ',' [ \t]* MACRO_ACTUAL_ARGUMENT)* ;
fragment MACRO_ACTUAL_ARGUMENT : ~[,"\r\n]+ ;

mode DEFAULT_NETTYPE_DIRECTIVE_MODE;
DEFAULT_NETTYPE_VALUE : ('wire' | 'tri' | 'tri0' | 'tri1' | 'wand' | 'triand' | 'wor' | 'trior' | 'trireg' | 'uwire' | 'none') -> channel(DIRECTIVES), mode(DEFAULT_MODE) ;

mode DEFINE_DIRECTIVE_MODE;
TEXT_MACRO_NAME : TEXT_MACRO_IDENTIFIER [ \t]* ('(' [ \t]* MACRO_LIST_OF_FORMAL_ARGUMENTS [ \t]* ')')? -> channel(DIRECTIVES), mode(MACRO_TEXT_MODE) ;
fragment TEXT_MACRO_IDENTIFIER : (LETTER | UNDERSCORE) (LETTER | UNDERSCORE | DECIMAL_DIGIT | DOLLAR_SIGN)* ;
fragment MACRO_LIST_OF_FORMAL_ARGUMENTS : MACRO_FORMAL_ARGUMENT_IDENTIFIER ([ \t]* ',' [ \t]* MACRO_FORMAL_ARGUMENT_IDENTIFIER)* ;
fragment MACRO_FORMAL_ARGUMENT_IDENTIFIER : (LETTER | UNDERSCORE) (LETTER | UNDERSCORE | DECIMAL_DIGIT | DOLLAR_SIGN)* ;

mode UNDEF_DIRECTIVE_MODE;
UNDEF_DIRECTIVE_IDENTIFIER : (LETTER | UNDERSCORE) (LETTER | UNDERSCORE | DECIMAL_DIGIT | DOLLAR_SIGN)* -> channel(DIRECTIVES), mode(DEFAULT_MODE) ;

mode CONDITIONAL_DIRECTIVE_MODE;
CONDITIONAL_DIRECTIVE_IDENTIFIER : (LETTER | UNDERSCORE) (LETTER | UNDERSCORE | DECIMAL_DIGIT | DOLLAR_SIGN)* -> channel(DIRECTIVES), mode(VERILOG_TEXT_MODE) ;

mode INCLUDE_DIRECTIVE_MODE;
FILENAME_STRING : '"' ~["\r\n]+ '"' -> channel(DIRECTIVES), mode(DEFAULT_MODE) ;

mode LINE_DIRECTIVE_MODE;
LINE_ARGUMENTS : LINE_NUMBER [ \t]+ LINE_FILENAME [ \t]+ LINE_LEVEL -> channel(DIRECTIVES), mode(DEFAULT_MODE) ;
fragment LINE_NUMBER : NON_ZERO_UNSIGNED_NUMBER ;
fragment LINE_FILENAME : '"' ~["\r\n]+ '"' ;
fragment LINE_LEVEL : [0-2] ;

mode TIMESCALE_DIRECTIVE_MODE;
TIMESCALE_ARGUMENTS : TIME_LITERAL [ \t]* '/' [ \t]* TIME_LITERAL -> channel(DIRECTIVES), mode(DEFAULT_MODE) ;
fragment TIME_LITERAL : TIME_NUMBER [ \t]* TIME_UNIT ;
fragment TIME_NUMBER : ('1' | '10' | '100') ;
fragment TIME_UNIT : [mnpf]? 's' ;

mode UNCONNECTED_DRIVE_DIRECTIVE_MODE;
UNCONNECTED_DRIVE_VALUE : ('pull1' | 'pull0') -> channel(DIRECTIVES), mode(DEFAULT_MODE) ;

mode PRAGMA_MODE;
PRAGMA_ARGUMENTS : PRAGMA_NAME [ \t]* ('(' [ \t]* PRAGMA_EXPRESSION ([ \t]* ',' [ \t]* PRAGMA_EXPRESSION)* ')')? -> channel(DIRECTIVES), mode(DEFAULT_MODE) ;
fragment PRAGMA_NAME : PRAGMA_IDENTIFIER ;
fragment PRAGMA_EXPRESSION : (PRAGMA_KEYWORD | PRAGMA_VALUE | PRAGMA_KEYWORD '=' PRAGMA_VALUE ) ;
fragment PRAGMA_VALUE : ('(' [ \t]* PRAGMA_EXPRESSION ([ \t]* ',' [ \t]* PRAGMA_EXPRESSION)* ')' | PRAGMA_NUMBER | PRAGMA_STRING | PRAGMA_IDENTIFIER ) ;
fragment PRAGMA_KEYWORD : PRAGMA_IDENTIFIER ;
fragment PRAGMA_NUMBER : DECIMAL_DIGIT (UNDERSCORE | DECIMAL_DIGIT)* ;
fragment PRAGMA_STRING : '"' ~["\r\n]+ '"' ;
fragment PRAGMA_IDENTIFIER : (LETTER | UNDERSCORE) (LETTER | UNDERSCORE | DECIMAL_DIGIT | DOLLAR_SIGN)* ;

mode BEGIN_KEYWORDS_DIRECTIVE_MODE;
VERSION_SPECIFIER : '"' ('1364-1995' | '1364-2001' | '1364-2001-noconfig' | '1364-2005') '"' -> channel(DIRECTIVES), mode(DEFAULT_MODE) ;

mode MACRO_TEXT_MODE;
MACRO_TEXT : ~[\r\n\\]+ -> channel(DIRECTIVES) ;
MACRO_TEXT_ONE_LINE_COMMENT : DOUBLE_SLASH TEXT NEWLINE -> channel(COMMENTS) ;
MACRO_TEXT_BLOCK_COMMENT : SLASH_ASTERISK TEXT ASTERISK_SLASH -> channel(COMMENTS) ;
MACRO_TEXT_BACKSLASH_NEWLINE : BACKSLASH NEWLINE -> channel(DIRECTIVES) ;
MACRO_TEXT_NEWLINE : NEWLINE -> channel(HIDDEN), mode(DEFAULT_MODE) ;

mode VERILOG_TEXT_MODE;
VERILOG_TEXT : ~[`\r\n\\]+ -> channel(DIRECTIVES) ;
VERILOG_TEXT_ONE_LINE_COMMENT : DOUBLE_SLASH TEXT NEWLINE -> channel(COMMENTS) ;
VERILOG_TEXT_BLOCK_COMMENT : SLASH_ASTERISK TEXT ASTERISK_SLASH -> channel(COMMENTS) ;
VERILOG_TEXT_GRAVE_ACCENT : '`' -> channel(DIRECTIVES), mode(DIRECTIVE_MODE) ;
