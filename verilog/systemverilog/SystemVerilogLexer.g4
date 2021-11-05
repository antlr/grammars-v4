// Author: Mustafa Said Ağca
// License: MIT

lexer grammar SystemVerilogLexer;

// 22. Compiler directives

COMPILER_DIRECTIVE
	: '`' .*? '\r'? '\n' -> skip
	;

// 33.3.1 Specifying libraries—the library map file

FILE_PATH_SPEC
	: ([/~] | './') ~[ \r\t\n]*?
	;

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

DECIMAL_NUMBER
	: SIZE? DECIMAL_BASE (UNSIGNED_NUMBER | (X_DIGIT | Z_DIGIT) '_'*)
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
	: '"' ~["\n\r]* '"'
	;

// A.9.2 Comments

ONE_LINE_COMMENT
	: '//' .*? '\r'? '\n' -> channel(HIDDEN)
	;

BLOCK_COMMENT
	: '/*' .*? '*/' -> channel(HIDDEN)
	;

// A.9.3 Identifiers

SIMPLE_IDENTIFIER
	: [a-zA-Z_] [a-zA-Z0-9_$]*
	;

SYSTEM_TF_IDENTIFIER
	: '$' [a-zA-Z0-9_$] [a-zA-Z0-9_$]*
	;

// A.9.4 White space

WHITE_SPACE
	: [ \t\n\r]+ -> channel(HIDDEN)
	;
