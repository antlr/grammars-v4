// Author: Mustafa Said Ağca
// License: MIT

lexer grammar VerilogLexer;

// A.8.7 Numbers

REAL_NUMBER
	: UNSIGNED_NUMBER '.' UNSIGNED_NUMBER
	| UNSIGNED_NUMBER ('.' UNSIGNED_NUMBER)? EXP SIGN? UNSIGNED_NUMBER
	;

fragment
EXP
	: [eE]
	;

DECIMAL_NUMBER
	: UNSIGNED_NUMBER
	| SIZE? DECIMAL_BASE UNSIGNED_NUMBER
	| SIZE? DECIMAL_BASE X_DIGIT '_'*
	| SIZE? DECIMAL_BASE Z_DIGIT '_'*
	;

BINARY_NUMBER
	: SIZE? BINARY_BASE BINARY_VALUE
	;

OCTAL_NUMBER
	: SIZE? OCTAL_BASE OCTAL_VALUE
	;

HEX_NUMBER
	: SIZE? HEX_BASE HEX_VALUE
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
	: X_DIGIT | Z_DIGIT | [01]
	;

fragment
OCTAL_DIGIT
	: X_DIGIT | Z_DIGIT | [0-7]
	;

fragment
HEX_DIGIT
	: X_DIGIT | Z_DIGIT | [0-9a-fA-F]
	;

fragment
X_DIGIT
	: [xX]
	;

fragment
Z_DIGIT
	: [zZ?]
	;

// A.8.8 Strings

STRING
	: '"' ~["\n\r]* '"'
	;

// A.9.2 Comments

ONE_LINE_COMMENT
	: '//' .*? '\r'? '\n' -> channel (HIDDEN)
	;

BLOCK_COMMENT
	: '/*' .*? '*/' -> channel (HIDDEN)
	;

// A.9.3 Identifiers

ESCAPED_IDENTIFIER
	: '\\' ('\u0021'..'\u007E')+ ~[ \r\t\n]*
	;

SIMPLE_IDENTIFIER
	: [a-zA-Z_] [a-zA-Z0-9_$]*
	;

SYSTEM_TF_IDENTIFIER
	: '$' [a-zA-Z0-9_$] [a-zA-Z0-9_$]*
	;

// A.9.5 White space

WHITE_SPACE
	: [ \t\n\r]+ -> channel (HIDDEN)
	;

// 13.2.1 Specifying libraries—the library map file

FILE_PATH_SPEC
	: ([/~] | './') ~[ \r\t\n]*?
	;

// 19. Compiler directives

GRAVE_ACCENT
	: '`' -> pushMode(COMPILER_DIRECTIVE);
	;

mode COMPILER_DIRECTIVE;

// 19.1 `celldefine and `endcelldefine

CELLDEFINE_COMPILER_DIRECTIVE
	: '`celldefine'
	;

ENDCELLDEFINE_COMPILER_DIRECTIVE
	: '`endcelldefine'
	;

// 19.2 `default_nettype

DEFAULT_NETTYPE_COMPILER_DIRECTIVE
	: '`default_nettype' DEFAULT_NETTYPE_VALUE
	;

DEFAULT_NETTYPE_VALUE
	: 'wire'
	| 'tri'
	| 'tri0'
	| 'tri1'
	| 'wand'
	| 'triand'
	| 'wor'
	| 'trior'
	| 'trireg'
	| 'uwire'
	| 'none'
	;

// 19.3 `define and `undef
// 19.3.1 `define

TEXT_MACRO_DEFINITION
	: '`define' TEXT_MACRO_NAME MACRO_TEXT
	;

fragment
TEXT_MACRO_NAME
	: TEXT_MACRO_IDENTIFIER ('(' LIST_OF_FORMAL_ARGUMENTS ')')?
	;

fragment
MACRO_TEXT
	: 
	;

fragment
LIST_OF_FORMAL_ARGUMENTS
	: FORMAL_ARGUMENT_IDENTIFIER (',' FORMAL_ARGUMENT_IDENTIFIER)*
	;

fragment
FORMAL_ARGUMENT_IDENTIFIER
	: SIMPLE_IDENTIFIER
	;

TEXT_MACRO_USAGE
	: '`' TEXT_MACRO_IDENTIFIER ('(' LIST_OF_ACTUAL_ARGUMENTS ')')?
	;

fragment
LIST_OF_ACTUAL_ARGUMENTS
	: ACTUAL_ARGUMENT (',' ACTUAL_ARGUMENT)*
	;

fragment
ACTUAL_ARGUMENT
	: 
	;

fragment
TEXT_MACRO_IDENTIFIER
	: SIMPLE_IDENTIFIER
	| ESCAPED_IDENTIFIER
	;

// 19.3.2 `undef

UNDEFINE_COMPILER_DIRECTIVE
	: '`undef' TEXT_MACRO_IDENTIFIER
	;

// 19.4 `ifdef, `else, `elsif, `endif , `ifndef

IFDEF_DIRECTIVE
	: '`ifdef' TEXT_MACRO_IDENTIFIER IFDEF_GROUP_OF_LINES ('`elsif' TEXT_MACRO_IDENTIFIER ELSIF_GROUP_OF_LINES)* ('`else' ELSE_GROUP_OF_LINES)? '`endif'
	;

IFNDEF_DIRECTIVE
	: '`ifndef' TEXT_MACRO_IDENTIFIER IFNDEF_GROUP_OF_LINES ('`elsif' TEXT_MACRO_IDENTIFIER ELSIF_GROUP_OF_LINES)* ('`else' ELSE_GROUP_OF_LINES)? '`endif'
	;

fragment
IFDEF_GROUP_OF_LINES
	: 
	;

fragment
IFNDEF_GROUP_OF_LINES
	: 
	;

fragment
ELSIF_GROUP_OF_LINES
	: 
	;

fragment
ELSE_GROUP_OF_LINES
	: 
	;

// 19.5 `include

INCLUDE_COMPILER_DIRECTIVE
	: '`include' FILE_PATH_SPEC
	;

// 19.6 `resetall

RESETALL_COMPILER_DIRECTIVE
	: '`resetall'
	;

// 19.7 `line

LINE_COMPILER_DIRECTIVE
	: '`line' (DECIMAL_NUMBER | OCTAL_NUMBER | BINARY_NUMBER | HEX_NUMBER | REAL_NUMBER STRING) ('0' | '1' | '2')
	;

// 19.8 `timescale

TIMESCALE_COMPILER_DIRECTIVE
	: '`timescale' TIME_LITERAL '/' TIME_LITERAL
	;

fragment
TIME_LITERAL
	: UNSIGNED_NUMBER TIME_UNIT
	;

fragment
TIME_UNIT
	: [mnpf]? 's'
	;

// 19.9 `unconnected_drive and `nounconnected_drive

UNCONNECTED_DRIVE_COMPILER_DIRECTIVE
	: '`unconnected_drive' ('pull0' | 'pull1')
	;

NOUNCONNECTED_DRIVE_COMPILER_DIRECTIVE
	: '`nounconnected_drive'
	;

// 19.10 `pragma

PRAGMA
	: '`pragma' PRAGMA_NAME (PRAGMA_EXPRESSION (',' PRAGMA_EXPRESSION )*)?
	;

fragment
PRAGMA_NAME
	: SIMPLE_IDENTIFIER
	;

fragment
PRAGMA_EXPRESSION
	: PRAGMA_KEYWORD
	| PRAGMA_KEYWORD '=' PRAGMA_VALUE
	| PRAGMA_VALUE
	;

fragment
PRAGMA_VALUE
	: '(' PRAGMA_EXPRESSION (',' PRAGMA_EXPRESSION)* ')'
	| DECIMAL_NUMBER
	| OCTAL_NUMBER
	| BINARY_NUMBER
	| HEX_NUMBER
	| REAL_NUMBER
	| STRING
	| SIMPLE_IDENTIFIER
	| ESCAPED_IDENTIFIER
	;

fragment
PRAGMA_KEYWORD
	: SIMPLE_IDENTIFIER
	;

// 19.11 `begin_keywords, `end_keywords

KEYWORDS_DIRECTIVE
	: '`begin_keywords' '"' VERSION_SPECIFIER '"'
	;

fragment
VERSION_SPECIFIER
	: '1364-1995'
	| '1364-2001'
	| '1364-2001-noconfig'
	| '1364-2005'
	;

fragment
ENDKEYWORDS_DIRECTIVE
	: '`end_keywords'
	;
