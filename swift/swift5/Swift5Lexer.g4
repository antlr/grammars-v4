lexer grammar Swift5Lexer;
// Insert here @header for C++ lexer.

options {
	superClass = SwiftSupportLexer;
}
AS: 'as';
ALPHA: 'alpha';
BREAK: 'break';
CASE: 'case';
CATCH: 'catch';
CLASS: 'class';
CONTINUE: 'continue';
DEFAULT: 'default';
DEFER: 'defer';
DO: 'do';
GUARD: 'guard';
ELSE: 'else';
ENUM: 'enum';
FOR: 'for';
FALLTHROUGH: 'fallthrough';
FUNC: 'func';
IN: 'in';
IF: 'if';
IMPORT: 'import';
INTERNAL: 'internal';
FINAL: 'final';
OPEN: 'open';
PRIVATE: 'private';
PUBLIC: 'public';
WHERE: 'where';
WHILE: 'while';
LET: 'let';
VAR: 'var';
PROTOCOL: 'protocol';
GET: 'get';
SET: 'set';
WILL_SET: 'willSet';
DID_SET: 'didSet';
REPEAT: 'repeat';
SWITCH: 'switch';
STRUCT: 'struct';
RETURN: 'return';
THROW: 'throw';
THROWS: 'throws';
RETHROWS: 'rethrows';
INDIRECT: 'indirect';
INIT: 'init';
DEINIT: 'deinit';
ASSOCIATED_TYPE: 'associatedtype';
EXTENSION: 'extension';
SUBSCRIPT: 'subscript';
PREFIX: 'prefix';
INFIX: 'infix';
LEFT: 'left';
RIGHT: 'right';
NONE: 'none';
PRECEDENCE_GROUP: 'precedencegroup';
HIGHER_THAN: 'higherThan';
LOWER_THAN: 'lowerThan';
ASSIGNMENT: 'assignment';
ASSOCIATIVITY: 'associativity';
POSTFIX: 'postfix';
OPERATOR: 'operator';
TYPEALIAS: 'typealias';
OS: 'os';
ARCH: 'arch';
SWIFT: 'swift';
COMPILER: 'compiler';
CAN_IMPORT: 'canImport';
TARGET_ENVIRONMENT: 'targetEnvironment';
CONVENIENCE: 'convenience';
DYNAMIC: 'dynamic';
LAZY: 'lazy';
OPTIONAL: 'optional';
OVERRIDE: 'override';
REQUIRED: 'required';
STATIC: 'static';
WEAK: 'weak';
UNOWNED: 'unowned';
SAFE: 'safe';
UNSAFE: 'unsafe';
MUTATING: 'mutating';
NONMUTATING: 'nonmutating';
FILE_PRIVATE: 'fileprivate';
IS: 'is';
TRY: 'try';
SUPER: 'super';
ANY: 'Any';
FALSE: 'false';
RED: 'red';
BLUE: 'blue';
GREEN: 'green';
RESOURCE_NAME: 'resourceName';
TRUE: 'true';
NIL: 'nil';
INOUT: 'inout';
SOME: 'some';
TYPE: 'Type';
PRECEDENCE: 'precedence';
SELF: 'self';
SELF_BIG: 'Self';

MAC_OS: 'macOS';
I_OS: 'iOS';
OSX: 'OSX';
WATCH_OS: 'watchOS';
TV_OS: 'tvOS';
LINUX: 'Linux';
WINDOWS: 'Windows';

I386: 'i386';
X86_64: 'x86_64';
ARM: 'arm';
ARM64: 'arm64';

SIMULATOR: 'simulator';
MAC_CATALYST: 'macCatalyst';

I_OS_APPLICATION_EXTENSION: 'iOSApplicationExtension';
MAC_CATALYST_APPLICATION_EXTENSION:
	'macCatalystApplicationExtension';
MAC_OS_APPLICATION_EXTENSION: 'macOSApplicationExtension';
SOURCE_LOCATION: '#sourceLocation';

FILE: 'file';
LINE: 'line';
ERROR: '#error';
WARNING: '#warning';
AVAILABLE: '#available';

HASH_IF: '#if';
HASH_ELSEIF: '#elseif';
HASH_ELSE: '#else';
HASH_ENDIF: '#endif';
HASH_FILE: '#file';
HASH_FILE_ID: '#fileID';
HASH_FILE_PATH: '#filePath';
HASH_LINE: '#line';
HASH_COLUMN: '#column';
HASH_FUNCTION: '#function';
HASH_DSO_HANDLE: '#dsohandle';
HASH_SELECTOR: '#selector';
HASH_KEYPATH: '#keyPath';
HASH_COLOR_LITERAL: '#colorLiteral';
HASH_FILE_LITERAL: '#fileLiteral';
HASH_IMAGE_LITERAL: '#imageLiteral';
GETTER: 'getter';
SETTER: 'setter';

Identifier:
	Identifier_head Identifier_characters?
	| Implicit_parameter_name
	| Property_wrapper_projection;

fragment Identifier_head:
	[a-zA-Z]
	| '_'
	| '\u00A8'
	| '\u00AA'
	| '\u00AD'
	| '\u00AF'
	| [\u00B2-\u00B5]
	| [\u00B7-\u00BA]
	| [\u00BC-\u00BE]
	| [\u00C0-\u00D6]
	| [\u00D8-\u00F6]
	| [\u00F8-\u00FF]
	| [\u0100-\u02FF]
	| [\u0370-\u167F]
	| [\u1681-\u180D]
	| [\u180F-\u1DBF]
	| [\u1E00-\u1FFF]
	| [\u200B-\u200D]
	| [\u202A-\u202E]
	| [\u203F-\u2040]
	| '\u2054'
	| [\u2060-\u206F]
	| [\u2070-\u20CF]
	| [\u2100-\u218F]
	| [\u2460-\u24FF]
	| [\u2776-\u2793]
	| [\u2C00-\u2DFF]
	| [\u2E80-\u2FFF]
	| [\u3004-\u3007]
	| [\u3021-\u302F]
	| [\u3031-\u303F]
	| [\u3040-\uD7FF]
	| [\uF900-\uFD3D]
	| [\uFD40-\uFDCF]
	| [\uFDF0-\uFE1F]
	| [\uFE30-\uFE44]
	| [\uFE47-\uFFFD]
	| [\u{10000}-\u{1FFFD}]
	| [\u{20000}-\u{2FFFD}]
	| [\u{30000}-\u{3FFFD}]
	| [\u{40000}-\u{4FFFD}]
	| [\u{50000}-\u{5FFFD}]
	| [\u{60000}-\u{6FFFD}]
	| [\u{70000}-\u{7FFFD}]
	| [\u{80000}-\u{8FFFD}]
	| [\u{90000}-\u{9FFFD}]
	| [\u{A0000}-\u{AFFFD}]
	| [\u{B0000}-\u{BFFFD}]
	| [\u{C0000}-\u{CFFFD}]
	| [\u{D0000}-\u{DFFFD}]
	| [\u{E0000}-\u{EFFFD}];

fragment Identifier_character:
	[0-9]
	| [\u0300-\u036F]
	| [\u1DC0-\u1DFF]
	| [\u20D0-\u20FF]
	| [\uFE20-\uFE2F]
	| Identifier_head;

fragment Identifier_characters: Identifier_character+;

fragment Implicit_parameter_name: '$' Decimal_digits;

fragment Property_wrapper_projection: '$' Identifier_characters;

DOT: '.';
LCURLY: '{';
LPAREN:
	'(' { if(!parenthesis.isEmpty()) parenthesis.push(parenthesis.pop()+1);};
LBRACK: '[';
RCURLY: '}';
RPAREN:
	')' { if(!parenthesis.isEmpty())
		{
			parenthesis.push(parenthesis.pop()-1);
			if(parenthesis.peek() == 0)
			{
				parenthesis.pop();
				popMode();
			}
		}
		};
RBRACK: ']';
COMMA: ',';
COLON: ':';
SEMI: ';';
LT: '<';
GT: '>';
UNDERSCORE: '_';
BANG: '!';
QUESTION: '?';
AT: '@';
AND: '&';
SUB: '-';
EQUAL: '=';
OR: '|';
DIV: '/';
ADD: '+';
MUL: '*';
MOD: '%';
CARET: '^';
TILDE: '~';
HASH: '#';
BACKTICK: '`';
DOLLAR: '$';
BACKSLASH: '\\';

Operator_head_other: // valid operator chars not used by Swift itself
	[\u00A1-\u00A7]
	| [\u00A9\u00AB]
	| [\u00AC\u00AE]
	| [\u00B0-\u00B1\u00B6\u00BB\u00BF\u00D7\u00F7]
	| [\u2016-\u2017\u2020-\u2027]
	| [\u2030-\u203E]
	| [\u2041-\u2053]
	| [\u2055-\u205E]
	| [\u2190-\u23FF]
	| [\u2500-\u2775]
	| [\u2794-\u2BFF]
	| [\u2E00-\u2E7F]
	| [\u3001-\u3003]
	| [\u3008-\u3020\u3030];

Operator_following_character:
	[\u0300-\u036F]
	| [\u1DC0-\u1DFF]
	| [\u20D0-\u20FF]
	| [\uFE00-\uFE0F]
	| [\uFE20-\uFE2F]
	| [\u{E0100}-\u{E01EF}];

Binary_literal: '0b' Binary_digit Binary_literal_characters?;
fragment Binary_digit: [01];
fragment Binary_literal_character: Binary_digit | '_';
fragment Binary_literal_characters: Binary_literal_character+;

Octal_literal: '0o' Octal_digit Octal_literal_characters?;
fragment Octal_digit: [0-7];
fragment Octal_literal_character: Octal_digit | '_';
fragment Octal_literal_characters: Octal_literal_character+;

Decimal_digits: Decimal_digit+;
Decimal_literal: Decimal_digit Decimal_literal_characters?;
fragment Decimal_digit: [0-9];
fragment Decimal_literal_character: Decimal_digit | '_';
fragment Decimal_literal_characters: Decimal_literal_character+;

Hexadecimal_literal:
	'0x' Hexadecimal_digit Hexadecimal_literal_characters?;
fragment Hexadecimal_digit: [0-9a-fA-F];
fragment Hexadecimal_literal_character: Hexadecimal_digit | '_';
fragment Hexadecimal_literal_characters:
	Hexadecimal_literal_character+;

// Floating-Point Literals
Floating_point_literal:
	Decimal_literal Decimal_fraction? Decimal_exponent?
	| Hexadecimal_literal Hexadecimal_fraction? Hexadecimal_exponent;
fragment Decimal_fraction: '.' Decimal_literal;
fragment Decimal_exponent:
	Floating_point_e Sign? Decimal_literal;
fragment Hexadecimal_fraction:
	'.' Hexadecimal_digit Hexadecimal_literal_characters?;
fragment Hexadecimal_exponent:
	Floating_point_p Sign? Decimal_literal;
fragment Floating_point_e: [eE];
fragment Floating_point_p: [pP];
fragment Sign: [+-];

WS: [ \n\r\t\u000B\u000C\u0000]+ -> channel(HIDDEN);

HASHBANG: '#!' .*? [\r\n]+ -> channel(HIDDEN);

Block_comment:
	'/*' (Block_comment | .)*? '*/' -> channel(HIDDEN);

Line_comment: '//' .*? ('\n' | EOF) -> channel(HIDDEN);

Multi_line_extended_string_open:
	'#'+ '"""' -> pushMode(MultiLineExtended);

Single_line_extended_string_open:
	'#'+ '"' -> pushMode(SingleLineExtended);

Multi_line_string_open: '"""' -> pushMode(MultiLine);

Single_line_string_open: '"' -> pushMode(SingleLine);

mode SingleLine;

Interpolataion_single_line:
	'\\(' { parenthesis.push(1);} -> pushMode(DEFAULT_MODE);

Single_line_string_close: '"' -> popMode;

Quoted_single_line_text: Quoted_text;

mode MultiLine;

Interpolataion_multi_line:
	'\\(' {parenthesis.push(1); } -> pushMode(DEFAULT_MODE);

Multi_line_string_close: '"""' -> popMode;

Quoted_multi_line_text: Multiline_quoted_text;

mode SingleLineExtended;

Single_line_extended_string_close: '"' '#'+ -> popMode;

Quoted_single_line_extended_text: ~[\r\n"]+;

mode MultiLineExtended;

Multi_line_extended_string_close: '"""' '#'+ -> popMode;

Quoted_multi_line_extended_text: ~["]+ | '"' '"'?;

fragment Quoted_text: Quoted_text_item+;

fragment Quoted_text_item: Escaped_character | ~["\n\r\\];

fragment Multiline_quoted_text:
	Escaped_character
	| ~[\\"]+
	| '"' '"'?
	| Escaped_newline;

fragment Escape_sequence: '\\' '#'*;

fragment Escaped_character:
	Escape_sequence (
		[0\\tnr"'\u201c]
		| 'u' '{' Unicode_scalar_digits '}'
	);

//Between one and eight hexadecimal digits
fragment Unicode_scalar_digits:
	Hexadecimal_digit Hexadecimal_digit? Hexadecimal_digit? Hexadecimal_digit? Hexadecimal_digit?
		Hexadecimal_digit? Hexadecimal_digit? Hexadecimal_digit?;

fragment Escaped_newline:
	Escape_sequence Inline_spaces? Line_break;

fragment Inline_spaces: [\u0009\u0020];

fragment Line_break: [\u000A\u000D]| '\u000D' '\u000A';
