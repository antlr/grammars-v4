lexer grammar CPP14Lexer;


MultiLineMacro:
	'#' (~[\n]*? '\\' '\r'? '\n')+ ~ [\n]+ -> channel (HIDDEN);

Directive: '#' ~ [\n]* -> channel (HIDDEN);
/*Keywords*/

Alignas: 'alignas';

Alignof: 'alignof';

Asm: 'asm';

Auto: 'auto';

Bool: 'bool';

Break: 'break';

Case: 'case';

Catch: 'catch';

Char: 'char';

Char16: 'char16_t';

Char32: 'char32_t';

Class: 'class';

Const: 'const';

Constexpr: 'constexpr';

Const_cast: 'const_cast';

Continue: 'continue';

Decltype: 'decltype';

Default: 'default';

Delete: 'delete';

Do: 'do';

Double: 'double';

Dynamic_cast: 'dynamic_cast';

Else: 'else';

Enum: 'enum';

Explicit: 'explicit';

Export: 'export';

Extern: 'extern';

//DO NOT RENAME - PYTHON NEEDS True and False
False_: 'false';

Final: 'final';

Float: 'float';

For: 'for';

Friend: 'friend';

Goto: 'goto';

If: 'if';

Inline: 'inline';

Int: 'int';

Long: 'long';

Mutable: 'mutable';

Namespace: 'namespace';

New: 'new';

Noexcept: 'noexcept';

Nullptr: 'nullptr';

Operator: 'operator';

Override: 'override';

Private: 'private';

Protected: 'protected';

Public: 'public';

Register: 'register';

Reinterpret_cast: 'reinterpret_cast';

Return: 'return';

Short: 'short';

Signed: 'signed';

Sizeof: 'sizeof';

Static: 'static';

Static_assert: 'static_assert';

Static_cast: 'static_cast';

Struct: 'struct';

Switch: 'switch';

Template: 'template';

This: 'this';

Thread_local: 'thread_local';

Throw: 'throw';

//DO NOT RENAME - PYTHON NEEDS True and False
True_: 'true';

Try: 'try';

Typedef: 'typedef';

Typeid_: 'typeid';

Typename_: 'typename';

Union: 'union';

Unsigned: 'unsigned';

Using: 'using';

Virtual: 'virtual';

Void: 'void';

Volatile: 'volatile';

Wchar: 'wchar_t';

While: 'while';
/*Operators*/

LeftParen: '(';

RightParen: ')';

LeftBracket: '[';

RightBracket: ']';

LeftBrace: '{';

RightBrace: '}';

Plus: '+';

Minus: '-';

Star: '*';

Div: '/';

Mod: '%';

Caret: '^';

And: '&';

Or: '|';

Tilde: '~';

Not: '!' | 'not';

Assign: '=';

Less: '<';

Greater: '>';

PlusAssign: '+=';

MinusAssign: '-=';

StarAssign: '*=';

DivAssign: '/=';

ModAssign: '%=';

XorAssign: '^=';

AndAssign: '&=';

OrAssign: '|=';

LeftShiftAssign: '<' '<' '=';

RightShiftAssign: '>' '>' '=';

Equal: '==';

NotEqual: '!=';

LessEqual: '<' '=';

GreaterEqual: '>' '=';

AndAnd: AndOperator | AndKeyword;

AndOperator:'&&';
AndKeyword: 'and';

OrOr: '||' | 'or';

PlusPlus: '++';

MinusMinus: '--';

Comma: ',';

ArrowStar: '->*';

Arrow: '->';

Question: '?';

Colon: ':';

Doublecolon: '::';

Semi: ';';

Dot: '.';

DotStar: '.*';

Ellipsis: '...';

fragment HexQuad:
	HexadecimalDigit HexadecimalDigit HexadecimalDigit HexadecimalDigit;

fragment UniversalCharacterName:
	'\\u' HexQuad
	| '\\U' HexQuad HexQuad;

Identifier:
	/*
	 Identifiernondigit | Identifier Identifiernondigit | Identifier DIGIT
	 */
	Identifiernondigit (Identifiernondigit | Digit)*;

fragment Identifiernondigit: NonDigit | UniversalCharacterName;

fragment NonDigit: [a-zA-Z_];

fragment Digit: [0-9];

IntegerLiteral:
	(
		DecimalLiteral
		| OctalLiteral
		| HexadecimalLiteral
		| BinaryLiteral
	) IntegerSuffix?;

DecimalLiteral: NonZeroDigit ('\''? Digit)*;

OctalLiteral: '0' ('\''? OctalDigit)*;

HexadecimalLiteral: ('0x' | '0X') HexadecimalDigit (
		'\''? HexadecimalDigit
	)*;

BinaryLiteral: ('0b' | '0B') BinaryDigit ('\''? BinaryDigit)*;

fragment NonZeroDigit: [1-9];

fragment OctalDigit: [0-7];

fragment HexadecimalDigit: [0-9a-fA-F];

fragment BinaryDigit: [01];

IntegerSuffix:
	UnsignedSuffix LongSuffix?
	| UnsignedSuffix LongLongSuffix?
	| LongSuffix UnsignedSuffix?
	| LongLongSuffix UnsignedSuffix?;

fragment UnsignedSuffix: [uU];

fragment LongSuffix: [lL];

fragment LongLongSuffix: 'll' | 'LL';

CharacterLiteral: EncodingPrefix? '\'' Cchar+ '\'';
fragment Cchar:
	~ ['\\\r\n]
	| EscapeSequence
	| UniversalCharacterName;

fragment EscapeSequence:
	SimpleEscapeSequence
	| OctalEscapeSequence
	| HexadecimalEscapeSequence;

fragment SimpleEscapeSequence: '\\' ['"?\\abfnrtv];

fragment OctalEscapeSequence:
	'\\' OctalDigit OctalDigit? OctalDigit?;

fragment HexadecimalEscapeSequence: '\\x' HexadecimalDigit+;

FloatingLiteral:
	(
		FractionalConstant ExponentPart?
		| DigitSequence ExponentPart
	) FloatingSuffix?;

fragment FractionalConstant:
	DigitSequence? '.' DigitSequence
	| DigitSequence '.';

fragment ExponentPart: [eE] SIGN? DigitSequence;

fragment SIGN: [+-];

fragment DigitSequence: Digit ('\''? Digit)*;

fragment FloatingSuffix: [flFL];

StringLiteral: EncodingPrefix? ('"' Schar* '"' | 'R' Rawstring);

fragment EncodingPrefix: 'u8' | 'u' | 'U' | 'L';

fragment Schar:
	~ ["\\\r\n]
	| EscapeSequence
	| UniversalCharacterName;

fragment Rawstring:
	'"' (~[\\\t\r\n])*? '(' .*? ')' (~[\\\t\r\n])*? '"';

BooleanLiteral: False_ | True_;

PointerLiteral: Nullptr;

UserDefinedLiteral:
	UserDefinedIntegerLiteral
	| UserDefinedFloatingLiteral
	| UserDefinedStringLiteral
	| UserDefinedCharacterLiteral;

UserDefinedIntegerLiteral:
	(
		DecimalLiteral
		| OctalLiteral
		| HexadecimalLiteral
		| BinaryLiteral
	) UdSuffix;

UserDefinedFloatingLiteral:
	(
		FractionalConstant ExponentPart?
		| DigitSequence ExponentPart
	) UdSuffix;

UserDefinedStringLiteral: StringLiteral UdSuffix;

UserDefinedCharacterLiteral: CharacterLiteral UdSuffix;

fragment UdSuffix: Identifier;

Whitespace: [ \t]+ -> skip;

Newline: ('\r' '\n'? | '\n') -> skip;

BlockComment: '/*' .*? '*/' -> skip;

LineComment: '//' ~ [\r\n]* -> skip;