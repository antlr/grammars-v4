// Author: Mustafa Said Ağca
// License: MIT

lexer grammar SystemVerilogLexerRules;

/******************
	LEXER RULES
******************/

// 20. Utility system tasks and system functions
// 21. Input/output system tasks and system functions

/*SYSTEM_TASK_FUNCTION
	: '$acos'
	| '$acosh'
	| '$asin'
	| '$asinh'
	| '$assertcontrol'
	| '$assertfailoff'
	| '$assertfailon'
	| '$assertkill'
	| '$assertnonvacuouson'
	| '$assertoff'
	| '$asserton'
	| '$assertpassoff'
	| '$assertpasson'
	| '$assertvacuousoff'
	| '$async$and$array'
	| '$async$and$plane'
	| '$async$nand$array'
	| '$async$nand$plane'
	| '$async$nor$array'
	| '$async$nor$plane'
	| '$async$or$array'
	| '$async$or$plane'
	| '$atan'
	| '$atan2'
	| '$atanh'
	| '$bits'
	| '$bitstoreal'
	| '$bitstoshortreal'
	| '$cast'
	| '$ceil'
	| '$changed'
	| '$changed_gclk'
	| '$changing_gclk'
	| '$clog2'
	| '$cos'
	| '$cosh'
	| '$countbits'
	| '$countones'
	| '$coverage_control'
	| '$coverage_get'
	| '$coverage_get_max'
	| '$coverage_merge'
	| '$coverage_save'
	| '$dimensions'
	| '$display'
	| '$displayb'
	| '$displayh'
	| '$displayo'
	| '$dist_chi_square'
	| '$dist_erlang'
	| '$dist_exponential'
	| '$dist_normal'
	| '$dist_poisson'
	| '$dist_t'
	| '$dist_uniform'
	| '$dumpall'
	| '$dumpfile'
	| '$dumpflush'
	| '$dumplimit'
	| '$dumpoff'
	| '$dumpon'
	| '$dumpports'
	| '$dumpportsall'
	| '$dumpportsflush'
	| '$dumpportslimit'
	| '$dumpportsoff'
	| '$dumpportson'
	| '$dumpvars'
	| '$error'
	| '$exit'
	| '$exp'
	| '$falling_gclk'
	| '$fatal'
	| '$fclose'
	| '$fdisplay'
	| '$fdisplayb'
	| '$fdisplayh'
	| '$fdisplayo'
	| '$fell'
	| '$fell_gclk'
	| '$feof'
	| '$ferror'
	| '$fflush'
	| '$fgetc'
	| '$fgets'
	| '$finish'
	| '$floor'
	| '$fmonitor'
	| '$fmonitorb'
	| '$fmonitorh'
	| '$fmonitoro'
	| '$fopen'
	| '$fread'
	| '$fscanf'
	| '$fseek'
	| '$fstrobe'
	| '$fstrobeb'
	| '$fstrobeh'
	| '$fstrobeo'
	| '$ftell'
	| '$future_gclk'
	| '$fwrite'
	| '$fwriteb'
	| '$fwriteh'
	| '$fwriteo'
	| '$get_coverage'
	| '$high'
	| '$hypot'
	| '$increment'
	| '$info'
	| '$isunbounded'
	| '$isunknown'
	| '$itor'
	| '$left'
	| '$ln'
	| '$load_coverage_db'
	| '$log10'
	| '$low'
	| '$monitor'
	| '$monitorb'
	| '$monitorh'
	| '$monitoro'
	| '$monitoroff'
	| '$monitoron'
	| '$onehot'
	| '$onehot0'
	| '$past'
	| '$past_gclk'
	| '$pow'
	| '$printtimescale'
	| '$q_add'
	| '$q_exam'
	| '$q_full'
	| '$q_initialize'
	| '$q_remove'
	| '$random'
	| '$readmemb'
	| '$readmemh'
	| '$realtime'
	| '$realtobits'
	| '$rewind'
	| '$right'
	| '$rising_gclk'
	| '$rose'
	| '$rose_gclk'
	| '$rtoi'
	| '$sampled'
	| '$set_coverage_db_name'
	| '$sformat'
	| '$sformatf'
	| '$shortrealtobits'
	| '$signed'
	| '$sin'
	| '$sinh'
	| '$size'
	| '$sqrt'
	| '$sscanf'
	| '$stable'
	| '$stable_gclk'
	| '$steady_gclk'
	| '$stime'
	| '$stop'
	| '$strobe'
	| '$strobeb'
	| '$strobeh'
	| '$strobeo'
	| '$swrite'
	| '$swriteb'
	| '$swriteh'
	| '$swriteo'
	| '$sync$and$array'
	| '$sync$and$plane'
	| '$sync$nand$array'
	| '$sync$nand$plane'
	| '$sync$nor$array'
	| '$sync$nor$plane'
	| '$sync$or$array'
	| '$sync$or$plane'
	| '$system'
	| '$tan'
	| '$tanh'
	| '$test$plusargs'
	| '$time'
	| '$timeformat'
	| '$typename'
	| '$ungetc'
	| '$unpacked_dimensions'
	| '$unsigned'
	| '$value$plusargs'
	| '$warning'
	| '$write'
	| '$writeb'
	| '$writeh'
	| '$writememb'
	| '$writememh'
	| '$writeo'
	;
*/
// 22. Compiler directives

fragment
COMPILER_DIRECTIVE
	: '`__FILE__'
	| '`__LINE__'
	| '`begin_keywords'
	| '`celldefine'
	| '`default_nettype'
	| '`define'
	| '`else'
	| '`elsif'
	| '`end_keywords'
	| '`endcelldefine'
	| '`endif'
	| '`ifdef'
	| '`ifndef'
	| '`include'
	| '`line'
	| '`nounconnected_drive'
	| '`pragma'
	| '`resetall'
	| '`timescale'
	| '`unconnected_drive'
	| '`undef'
	| '`undefineall'
	;

// ignore the lines starting with a compiler directive
PREPROCESSOR
	: COMPILER_DIRECTIVE .*? '\r'? '\n' -> skip
	;

// 33.3.1 Specifying libraries—the library map file

/*FILE_PATH_SPEC
	: ([/~] | './') ~[ \r\t\n]*?
	;
*/
// A.7.5.3 System timing check event definitions

/*EDGE_DESCRIPTOR
	: '01'
	| '10'
	| Z_OR_X ZERO_OR_ONE
	| ZERO_OR_ONE Z_OR_X
	;

fragment
ZERO_OR_ONE
	: [01]
	;

fragment
Z_OR_X
	: [xXzZ]
	;
*/
// A.8.4 Primaries

TIME_LITERAL
	: UNSIGNED_NUMBER TIME_UNIT
	| FIXED_POINT_NUMBER TIME_UNIT
	;

fragment
TIME_UNIT
	: [mnpf]? 's'
	;

// A.8.7 Numbers

INTEGRAL_NUMBER
	: DECIMAL_NUMBER
	| OCTAL_NUMBER
	| BINARY_NUMBER
	| HEX_NUMBER
	;

fragment
DECIMAL_NUMBER
	: UNSIGNED_NUMBER
	| (SIZE)? DECIMAL_BASE UNSIGNED_NUMBER
	| (SIZE)? DECIMAL_BASE X_DIGIT ('_')*
	| (SIZE)? DECIMAL_BASE Z_DIGIT ('_')*
	;

fragment
BINARY_NUMBER
	: (SIZE)? BINARY_BASE BINARY_VALUE
	;

fragment
OCTAL_NUMBER
	: (SIZE)? OCTAL_BASE OCTAL_VALUE
	;

fragment
HEX_NUMBER
	: (SIZE)? HEX_BASE HEX_VALUE
	;

fragment
SIGN
	: [+-]
	;

fragment
SIZE
	: NON_ZERO_UNSIGNED_NUMBER
	;

fragment
NON_ZERO_UNSIGNED_NUMBER
	: NON_ZERO_DECIMAL_DIGIT ('_' | DECIMAL_DIGIT)*
	;

REAL_NUMBER
	: FIXED_POINT_NUMBER
	;

fragment
FIXED_POINT_NUMBER
	: UNSIGNED_NUMBER '.' UNSIGNED_NUMBER
	;

fragment
EXP
	: [eE]
	;

UNSIGNED_NUMBER
	: DECIMAL_DIGIT ('_' | DECIMAL_DIGIT)*
	;

fragment
BINARY_VALUE
	: BINARY_DIGIT ('_' | BINARY_DIGIT)*
	;

fragment
OCTAL_VALUE
	: OCTAL_DIGIT ('_' | OCTAL_DIGIT)*
	;

fragment
HEX_VALUE
	: HEX_DIGIT ('_' | HEX_DIGIT)*
	;

fragment
DECIMAL_BASE
	: '\'' [sS]? [dD]
	;

fragment
BINARY_BASE
	: '\'' [sS]? [bB]
	;

fragment
OCTAL_BASE
	: '\'' [sS]? [oO]
	;

fragment
HEX_BASE
	: '\'' [sS]? [hH]
	;

fragment
NON_ZERO_DECIMAL_DIGIT
	: [1-9]
	;

fragment
DECIMAL_DIGIT
	: [0-9]
	;

fragment
BINARY_DIGIT
	: X_DIGIT
	| Z_DIGIT
	| [01]
	;

fragment
OCTAL_DIGIT
	: X_DIGIT
	| Z_DIGIT
	| [0-7]
	;

fragment
HEX_DIGIT
	: X_DIGIT
	| Z_DIGIT
	| [0-9a-fA-F]
	;

fragment
X_DIGIT
	: [xX]
	;

fragment
Z_DIGIT
	: [zZ?]
	;

/*UNBASED_UNSIZED_LITERAL
	: '\'0'
	| '\'1'
	| '\'' Z_OR_X
	;
*/
// A.8.8 Strings

STRING_LITERAL
	: '"' ~[\n\r]* '"'
	;

// A.9.2 Comments

// commented out the following rule so that no token is generated for comments
/*COMMENT
	: ONE_LINE_COMMENT
	| BLOCK_COMMENT
	;
*/
ONE_LINE_COMMENT
	: '//' .*? '\r'? '\n' -> skip
	;

BLOCK_COMMENT
	: '/*' .*? '*/' -> skip
	;

// A.9.3 Identifiers

SIMPLE_IDENTIFIER
	: [a-zA-Z_] [a-zA-Z0-9_$]*
	;

/*ESCAPED_IDENTIFIER
	: '\\' ('\u0021'..'\u007E')+ ~[ \r\t\n]*
	;
*/
SYSTEM_TF_IDENTIFIER
	: '$' [a-zA-Z0-9_$] [a-zA-Z0-9_$]*
	;

// A.9.4 White space

WHITE_SPACE
	: [ \t\n\r]+ -> skip
	;

// B Keywords

/*KEYWORD
	: 'accept_on'
	| 'alias'
	| 'always'
	| 'always_comb'
	| 'always_ff'
	| 'always_latch'
	| 'and'
	| 'assert'
	| 'assign'
	| 'assume'
	| 'automatic'
	| 'before'
	| 'begin'
	| 'bind'
	| 'bins'
	| 'binsof'
	| 'bit'
	| 'break'
	| 'buf'
	| 'bufif0'
	| 'bufif1'
	| 'byte'
	| 'case'
	| 'casex'
	| 'casez'
	| 'cell'
	| 'chandle'
	| 'checker'
	| 'class'
	| 'clocking'
	| 'cmos'
	| 'config'
	| 'const'
	| 'constraint'
	| 'context'
	| 'continue'
	| 'cover'
	| 'covergroup'
	| 'coverpoint'
	| 'cross'
	| 'deassign'
	| 'default'
	| 'defparam'
	| 'design'
	| 'disable'
	| 'dist'
	| 'do'
	| 'edge'
	| 'else'
	| 'end'
	| 'endcase'
	| 'endchecker'
	| 'endclass'
	| 'endclocking'
	| 'endconfig'
	| 'endfunction'
	| 'endgenerate'
	| 'endgroup'
	| 'endinterface'
	| 'endmodule'
	| 'endpackage'
	| 'endprimitive'
	| 'endprogram'
	| 'endproperty'
	| 'endspecify'
	| 'endsequence'
	| 'endtable'
	| 'endtask'
	| 'enum'
	| 'event'
	| 'eventually'
	| 'expect'
	| 'export'
	| 'extends'
	| 'extern'
	| 'final'
	| 'first_match'
	| 'for'
	| 'force'
	| 'foreach'
	| 'forever'
	| 'fork'
	| 'forkjoin'
	| 'function'
	| 'generate'
	| 'genvar'
	| 'global'
	| 'highz0'
	| 'highz1'
	| 'if'
	| 'iff'
	| 'ifnone'
	| 'ignore_bins'
	| 'illegal_bins'
	| 'implements'
	| 'implies'
	| 'import'
	| 'incdir'
	| 'include'
	| 'initial'
	| 'inout'
	| 'input'
	| 'inside'
	| 'instance'
	| 'int'
	| 'integer'
	| 'interconnect'
	| 'interface'
	| 'intersect'
	| 'join'
	| 'join_any'
	| 'join_none'
	| 'large'
	| 'let'
	| 'liblist'
	| 'library'
	| 'local'
	| 'localparam'
	| 'logic'
	| 'longint'
	| 'macromodule'
	| 'matches'
	| 'medium'
	| 'modport'
	| 'module'
	| 'nand'
	| 'negedge'
	| 'nettype'
	| 'new'
	| 'nexttime'
	| 'nmos'
	| 'nor'
	| 'noshowcancelled'
	| 'not'
	| 'notif0'
	| 'notif1'
	| 'null'
	| 'or'
	| 'output'
	| 'package'
	| 'packed'
	| 'parameter'
	| 'pmos'
	| 'posedge'
	| 'primitive'
	| 'priority'
	| 'program'
	| 'property'
	| 'protected'
	| 'pull0'
	| 'pull1'
	| 'pulldown'
	| 'pullup'
	| 'pulsestyle_ondetect'
	| 'pulsestyle_onevent'
	| 'pure'
	| 'rand'
	| 'randc'
	| 'randcase'
	| 'randsequence'
	| 'rcmos'
	| 'real'
	| 'realtime'
	| 'ref'
	| 'reg'
	| 'reject_on'
	| 'release'
	| 'repeat'
	| 'restrict'
	| 'return'
	| 'rnmos'
	| 'rpmos'
	| 'rtran'
	| 'rtranif0'
	| 'rtranif1'
	| 's_always'
	| 's_eventually'
	| 's_nexttime'
	| 's_until'
	| 's_until_with'
	| 'scalared'
	| 'sequence'
	| 'shortint'
	| 'shortreal'
	| 'showcancelled'
	| 'signed'
	| 'small'
	| 'soft'
	| 'solve'
	| 'specify'
	| 'specparam'
	| 'static'
	| 'string'
	| 'strong'
	| 'strong0'
	| 'strong1'
	| 'struct'
	| 'super'
	| 'supply0'
	| 'supply1'
	| 'sync_accept_on'
	| 'sync_reject_on'
	| 'table'
	| 'tagged'
	| 'task'
	| 'this'
	| 'throughout'
	| 'time'
	| 'timeprecision'
	| 'timeunit'
	| 'tran'
	| 'tranif0'
	| 'tranif1'
	| 'tri'
	| 'tri0'
	| 'tri1'
	| 'triand'
	| 'trior'
	| 'trireg'
	| 'type'
	| 'typedef'
	| 'union'
	| 'unique'
	| 'unique0'
	| 'unsigned'
	| 'until'
	| 'until_with'
	| 'untyped'
	| 'use'
	| 'uwire'
	| 'var'
	| 'vectored'
	| 'virtual'
	| 'void'
	| 'wait'
	| 'wait_order'
	| 'wand'
	| 'weak'
	| 'weak0'
	| 'weak1'
	| 'while'
	| 'wildcard'
	| 'wire'
	| 'with'
	| 'within'
	| 'wor'
	| 'xnor'
	| 'xor'
	;
*/
