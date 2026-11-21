/*******************************************************************************
 * The MIT License (MIT)
 *
 * Copyright (c) 2015 Camilo Sanchez (Camiloasc1) 2020 Martin Mirchev (Marti2203) 2026 Pavel Bondar (bondar-p)
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 * ****************************************************************************
 */


/** C++23 grammar built from the C++23 Spec */


// $antlr-format alignTrailingComments true, columnLimit 150, maxEmptyLinesToKeep 1, reflowComments false, useTab false
// $antlr-format allowShortRulesOnASingleLine true, allowShortBlocksOnASingleLine true, minEmptyLines 0, alignSemicolons ownLine
// $antlr-format alignColons trailing, singleLineOverrulesHangingColon true, alignLexerCommands true, alignLabels true, alignTrailers true

lexer grammar CPP23Lexer;


IntegerLiteral:
    BinaryLiteral Integersuffix?
	| OctalLiteral Integersuffix?
	| DecimalLiteral Integersuffix?
    | HexadecimalLiteral Integersuffix?
;

CharacterLiteral: ('u8' | 'u' | 'U' | 'L')? '\'' Cchar+ '\'';

FloatingPointLiteral:
    Fractionalconstant Exponentpart? Floatingsuffix?
    | Digitsequence Exponentpart Floatingsuffix?
    | Hexadecimalprefix Hexadecimalfractionalconstant Binaryexponentpart Floatingsuffix?
    | Hexadecimalprefix Hexadigitsequence Binaryexponentpart Floatingsuffix?
;

fragment Hexadecimalprefix: '0x' | '0X';

fragment Hexadecimalfractionalconstant:
    Hexadigitsequence? '.' Hexadigitsequence
    | Hexadigitsequence '.'
;

fragment Binaryexponentpart: [pP] SIGN? Digitsequence;

StringLiteral: Encodingprefix? (Rawstring | '"' Schar* '"');

BooleanLiteral: False_ | True_;

PointerLiteral: Nullptr;

UserDefinedLiteral:
    UserDefinedIntegerLiteral
    | UserDefinedFloatingLiteral
    | UserDefinedStringLiteral
    | UserDefinedCharacterLiteral
;

MultiLineMacro: '#' (~[\n]*? '\\' '\r'? '\n')+ ~ [\n]+ -> channel (HIDDEN);

Directive: '#' ~ [\n]* -> channel (HIDDEN);

// Qt-specific token
Q_Arg: 'Q_ARG' -> mode(DEFAULT_MODE), type(Identifier);


/* A.2 Keywords */

Alignas: 'alignas';

Alignof: 'alignof';

Asm: 'asm';

Atomic_cancel: 'atomic_cancel';

Atomic_commit: 'atomic_commit';

Atomic_noexcept: 'atomic_noexcept';

Auto: 'auto';

Bool: 'bool';

Break: 'break';

Case: 'case';

Catch: 'catch';

Char: 'char';

Char8: 'char8_t';

Char16: 'char16_t';

Char32: 'char32_t';

Class: 'class';

Concept: 'concept';

Const: 'const';

Consteval: 'consteval';

Constexpr: 'constexpr';

Constinit: 'constinit';

Const_cast: 'const_cast';

Continue: 'continue';

Co_await: 'co_await';

Co_return: 'co_return';

Co_yield: 'co_yield';

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

// DO NOT RENAME - PYTHON NEEDS True and False
False_: 'false';

Final: 'final';

Float: 'float';

For: 'for';

Friend: 'friend';

Goto: 'goto';

If: 'if';

Import: 'import';

Inline: 'inline';

Int: 'int';

Long: 'long';

Module: 'module';

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

Requires: 'requires';

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

// DO NOT RENAME - PYTHON NEEDS True and False
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


/* Operators */

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

Caret: '^' | 'xor';

And: '&' | 'bitand';

Or: '|' | 'bitor';

Tilde: '~' | 'compl';

Not: '!' | 'not';

Assign: '=';

Less: '<';

Greater: '>';

PlusAssign: '+=';

MinusAssign: '-=';

StarAssign: '*=';

DivAssign: '/=';

ModAssign: '%=';

XorAssign: '^=' | 'xor_eq';

AndAssign: '&=' | 'and_eq';

OrAssign: '|=' | 'or_eq';

LeftShiftAssign: '<<=';

RightShiftAssign: '>>=';

Equal: '==';

NotEqual: '!=' | 'not_eq';

LessEqual: '<=';

GreaterEqual: '>=';

LessEqualGreater: '<=>';

AndAnd: '&&' | 'and';

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


/* Preprocessor's keywords */
/*
Sharp: '#';

Include: '#include';

Define: '#define';

Undef: '#undef';

Line_: '#line';

Error_: '#error';

Pragma: '#pragma';

Ifdef: '#ifdef';

Ifndef: '#ifndef';

Elif: '#elif';

Endif: '#endif';

Warning:  '#warning';

Elifdef:  '#elifdef';

Elifndef: '#elifndef';
*/

Quotes: '"';

/*
Defined: '#defined';

Has_include: '__has_include';

Has_cpp_attribute: '__has_cpp_attribute';

Va_opt: '__VA_OPT__';
*/

// Qt macros
QML_Named_Element: 'QML_NAMED_ELEMENT' ~ [\n]* -> channel (HIDDEN);

QML_Singleton: 'QML_SINGLETON' ~ [\n]* -> channel (HIDDEN);

Q_Disable_Copy_Move: 'Q_DISABLE_COPY_MOVE' ~ [\n]* -> channel (HIDDEN);

Q_Declare_Interface: 'Q_DECLARE_INTERFACE' ~ [\n]* -> channel (HIDDEN);

Q_Declare_Metatype: 'Q_DECLARE_METATYPE' ~ [\n]* -> channel (HIDDEN);

Q_Object: 'Q_OBJECT' -> channel (HIDDEN);

Q_Property: 'Q_PROPERTY' ~ [\n]* -> channel (HIDDEN);

Q_Invokable: 'Q_INVOKABLE' ~ [\n]* -> channel (HIDDEN);

Q_Classinfo: 'Q_CLASSINFO' ~ [\n]* -> channel (HIDDEN);

Qax: 'QAX' ~ [\n]* -> channel (HIDDEN);

/* A.3 Lexical conventions */

fragment Hexquad: HEXADECIMALDIGIT HEXADECIMALDIGIT HEXADECIMALDIGIT HEXADECIMALDIGIT;

fragment Universalcharactername:
    '\\u' Hexquad
    | '\\U' Hexquad Hexquad
    | '\\u{' Simplehexadecimaldigitsequence '}'
    | Nameduniversalcharacter
;

Identifier:
	Identifiernondigit (Identifiernondigit | DIGIT)*
;

fragment Identifiernondigit: NONDIGIT | Universalcharactername;

fragment NONDIGIT: [a-zA-Z_];

fragment DIGIT: [0-9];

DecimalLiteral: NONZERODIGIT ('\''? DIGIT)*;

OctalLiteral: '0' ('\''? OCTALDIGIT)*;

HexadecimalLiteral: ('0x' | '0X') HEXADECIMALDIGIT ( '\''? HEXADECIMALDIGIT)*;

BinaryLiteral: ('0b' | '0B') BINARYDIGIT ('\''? BINARYDIGIT)*;

fragment NONZERODIGIT: [1-9];

fragment OCTALDIGIT: [0-7];

fragment HEXADECIMALDIGIT: [0-9a-fA-F];

fragment BINARYDIGIT: [01];

fragment Sizesuffix: [zZ];

Integersuffix:
    Unsignedsuffix Longsuffix?
    | Unsignedsuffix Longlongsuffix?
    | Unsignedsuffix Sizesuffix?
    | Longsuffix Unsignedsuffix?
    | Longlongsuffix Unsignedsuffix?
    | Sizesuffix Unsignedsuffix?
;

fragment Unsignedsuffix: [uU];

fragment Longsuffix: [lL];

fragment Longlongsuffix: 'll' | 'LL';

fragment Cchar: ~ ['\\\r\n] | Escapesequence | Universalcharactername;

fragment Escapesequence: Simpleescapesequence | Octalescapesequence | Hexadecimalescapesequence;

fragment Simpleescapesequence:
    '\\\''
    | '\\"'
    | '\\?'
    | '\\\\'
    | '\\a'
    | '\\b'
    | '\\f'
    | '\\n'
    | '\\r'
    | '\\' ('\r' '\n'? | '\n')
    | '\\t'
    | '\\v'
;

// named universal character \N{...}
fragment Nchar: ~[}\r\n];
fragment Ncharsequence: Nchar+;
fragment Nameduniversalcharacter: '\\N{' Ncharsequence '}';

// delimited escape sequences \o{...}, \x{...}, \u{...}
fragment Simpleoctaldigitsequence: OCTALDIGIT+;
fragment Simplehexadecimaldigitsequence: HEXADECIMALDIGIT+;

fragment Hexadigitsequence:
    HEXADECIMALDIGIT ('\''? HEXADECIMALDIGIT)*
;

fragment Octalescapesequence:
    '\\' OCTALDIGIT
    | '\\' OCTALDIGIT OCTALDIGIT
    | '\\' OCTALDIGIT OCTALDIGIT OCTALDIGIT
	| '\\o{' Simpleoctaldigitsequence '}'
;

fragment Hexadecimalescapesequence:
    '\\x' HEXADECIMALDIGIT+
    | '\\x{' Simplehexadecimaldigitsequence '}'
    ;

fragment Fractionalconstant: Digitsequence? '.' Digitsequence | Digitsequence '.';

fragment Exponentpart: 'e' SIGN? Digitsequence | 'E' SIGN? Digitsequence;

fragment SIGN: [+-];

fragment Digitsequence: DIGIT ('\''? DIGIT)*;

fragment Floatingsuffix:
    [flFL]
    | 'f16' | 'f32' | 'f64' | 'f128' | 'bf16'
    | 'F16' | 'F32' | 'F64' | 'F128' | 'BF16'
;

fragment Encodingprefix: 'u8' | 'u' | 'U' | 'L';

// any member of the basic source character set except the single-quote ’,
// backslash \, or new-line character
fragment Schar: ~ ["\\\r\n] | Escapesequence | Universalcharactername;

fragment Rawstring: 'R"' ( '\\' ["()] | ~[\r\n (])*? '(' ~[)]*? ')' ( '\\' ["()] | ~[\r\n "])*? '"';

UserDefinedIntegerLiteral:
    DecimalLiteral Udsuffix
    | OctalLiteral Udsuffix
    | HexadecimalLiteral Udsuffix
    | BinaryLiteral Udsuffix
;

UserDefinedFloatingLiteral:
    Fractionalconstant Exponentpart? Udsuffix
    | Digitsequence Exponentpart Udsuffix
    | Hexadecimalprefix Hexadecimalfractionalconstant Binaryexponentpart Udsuffix
    | Hexadecimalprefix Hexadigitsequence Binaryexponentpart Udsuffix
;

UserDefinedStringLiteral: StringLiteral Udsuffix;

UserDefinedCharacterLiteral: CharacterLiteral Udsuffix;

fragment Udsuffix: Identifier;

Whitespace: [ \t]+ -> skip;

Newline: ('\r' '\n'? | '\n') -> skip;

BlockComment: '/*' .*? '*/' -> skip;

LineComment: '//' ~ [\r\n]* -> skip;
