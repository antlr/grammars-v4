// Copyright (c) 2017, the Dart project authors.  Please see the AUTHORS file
// for details. All rights reserved. Use of this source code is governed by a
// BSD-style license that can be found in the LICENSE file.

// CHANGES:
//
// v0.18 Add support for enhanced `enum` declarations.
//
// v0.17 (58d917e7573c359580ade43845004dbbc62220d5) Correct `uri` to allow
// multi-line strings (raw and non-raw).
//
// v0.16 (284695f1937c262523a9a11b9084213f889c83e0) Correct instance variable
// declaration syntax such that `covariant late final` is allowed.
//
// v0.15 (6facd6dfdafa2953e8523348220d3129ea884678) Add support for
// constructor tearoffs and explicitly instantiated function tearoffs and
// type literals.
//
// v0.14 (f65c20124edd9e04f7b3a6f014f40c16f51052f6) Correct `partHeader`
// to allow uri syntax in a `PART OF` directive.
//
// v0.13 (bb5cb79a2fd57d6a480b922bc650d5cd15948753) Introduce non-terminals
// `builtinIdentifier` and `reservedWord`; update `typeAlias` to enable
// non-function type aliases; add missing `metadata` to formal parameter
// declarations; correct `symbolLiteral` to allow `VOID`;

// v0.12 (82403371ac00ddf004be60fa7b705474d2864509) Cf. language issue #1341:
// correct `metadata`. Change `qualifiedName` such that it only includes the
// cases with a '.'; the remaining case is added where `qualifiedName` is used.
//
// v0.11 (67c703063d5b68c9e132edbaf34dfe375851f5a6) Corrections, mainly:
// `fieldFormalParameter` now allows `?` on the parameter type; cascade was
// reorganized in the spec, it is now reorganized similarly here; `?` was
// removed from argumentPart (null-aware invocation was never added).
//
// v0.10 (8ccdb9ae796d543e4ad8f339c847c02b09018d2d) Simplify grammar by making
// `constructorInvocation` an alternative in `primary`.
//
// v0.9 (f4d7951a88e1b738e22b768c3bc72bf1a1062365) Introduce abstract and
// external variables.
//
// v0.8 (a9ea9365ad8a3e3b59115bd889a55b6aa2c5a5fa) Change null-aware
// invocations of `operator []` and `operator []=` to not have a period.
//
// v0.7 (6826faf583f6a543b1a0e2e85bd6a8042607ce00) Introduce extension and
// mixin declarations. Revise rules about string literals and string
// interpolation. Reorganize "keywords" (built-in identifiers, reserved words,
// other words that are specified in the grammar and not parsed as IDENTIFIER)
// into explicitly marked groups. Change the cascade syntax to be
// compositional.
//
// v0.6 (a58052974ec2b4b334922c5227b043ed2b9c2cc5) Introduce syntax associated
// with null safety.
//
// v0.5 (56793b3d4714d4818d855a72074d5295489aef3f) Stop treating `ASYNC` as a
// conditional reserved word (only `AWAIT` and `YIELD` get this treatment).
//
// v0.4 Added support for 'unified collections' (spreads and control flow
// in collection literals).
//
// v0.3 Updated to use ANTLR v4 rather than antlr3.
//
// v0.2 Changed top level variable declarations to avoid redundant and
// misleading occurrence of (FINAL|CONST).
//
// v0.1 First version available in the SDK github repository. Covers the
// Dart language as specified in the language specification based on the
// many grammar rule snippets. That grammar was then adjusted to remove
// known issues (e.g., misplaced metadata) and to resolve ambiguities.

lexer grammar DartLexer;

options { superClass = DartLexerBase; }

// ---------------------------------------- Lexer rules.

AA: '&&';
AND: '&';
ANDE: '&=';
AT: '@';
B: '|';
BANG: '!';
BB: '||';
BE: '|=';
CB: ']';
CO: ':';
COM: ',';
CP: ')';
DD: '..';
DDD: '...';
DDDQ: '...?';
DOT: '.';
E: '=';
EE: '==';
EGT: '=>';
GT: '>';
LE: '<=';
LL: '<<';
LLE: '<<=';
LT: '<';
ME: '-=';
MIN: '-';
MINMIN: '--';
MOD: '%';
MODE: '%=';
MUL: '*';
MULE: '*=';
NE: '!=';
OB: '[';
OP: '(';
P: '+';
PE: '+=';
POUND: '#';
PP: '++';
Q: '?';
QD: '?.';
QDD: '?..';
QQ: '??';
QQE: '??=';
SCO: ';';
SL: '/';
SLE: '/=';
SQ: '~';
SQE: '~/';
SQSE: '~/=';
TE: '^=';
TIL: '^';

fragment
LETTER
    :    'a' .. 'z'
    |    'A' .. 'Z'
    ;

fragment
DIGIT
    :    '0' .. '9'
    ;

fragment
EXPONENT
    :    ('e' | 'E') ('+' | '-')? DIGIT+
    ;

fragment
HEX_DIGIT
    :    ('a' | 'b' | 'c' | 'd' | 'e' | 'f')
    |    ('A' | 'B' | 'C' | 'D' | 'E' | 'F')
    |    DIGIT
    ;

// Reserved words.

ASSERT
    :    'assert'
    ;

BREAK
    :    'break'
    ;

CASE
    :    'case'
    ;

CATCH
    :    'catch'
    ;

CLASS
    :    'class'
    ;

CONST
    :    'const'
    ;

CONTINUE
    :    'continue'
    ;

DEFAULT
    :    'default'
    ;

DO
    :    'do'
    ;

ELSE
    :    'else'
    ;

ENUM
    :    'enum'
    ;

EXTENDS
    :    'extends'
    ;

FALSE
    :    'false'
    ;

FINAL
    :    'final'
    ;

FINALLY
    :    'finally'
    ;

FOR
    :    'for'
    ;

IF
    :    'if'
    ;

IN
    :    'in'
    ;

IS
    :    'is'
    ;

NEW
    :    'new'
    ;

NULL
    :    'null'
    ;

RETHROW
    :    'rethrow'
    ;

RETURN
    :    'return'
    ;

SUPER
    :    'super'
    ;

SWITCH
    :    'switch'
    ;

THIS
    :    'this'
    ;

THROW
    :    'throw'
    ;

TRUE
    :    'true'
    ;

TRY
    :    'try'
    ;

VAR
    :    'var'
    ;

VOID
    :    'void'
    ;

WHILE
    :    'while'
    ;

WITH
    :    'with'
    ;

// Built-in identifiers.

ABSTRACT
    :    'abstract'
    ;

AS
    :    'as'
    ;

COVARIANT
    :    'covariant'
    ;

DEFERRED
    :    'deferred'
    ;

DYNAMIC
    :    'dynamic'
    ;

EXPORT
    :    'export'
    ;

EXTENSION
    :    'extension'
    ;

EXTERNAL
    :    'external'
    ;

FACTORY
    :    'factory'
    ;

FUNCTION
    :    'Function'
    ;

GET
    :    'get'
    ;

IMPLEMENTS
    :    'implements'
    ;

IMPORT
    :    'import'
    ;

INTERFACE
    :    'interface'
    ;

LATE
    :    'late'
    ;

LIBRARY
    :    'library'
    ;

OPERATOR
    :    'operator'
    ;

MIXIN
    :    'mixin'
    ;

PART
    :    'part'
    ;

REQUIRED
    :    'required'
    ;

SET
    :    'set'
    ;

STATIC
    :    'static'
    ;

TYPEDEF
    :    'typedef'
    ;

// "Contextual keywords".

AWAIT
    :    'await'
    ;

YIELD
    :    'yield'
    ;

// Other words used in the grammar.

ASYNC
    :    'async'
    ;

HIDE
    :    'hide'
    ;

OF
    :    'of'
    ;

ON
    :    'on'
    ;

SHOW
    :    'show'
    ;

SYNC
    :    'sync'
    ;

// Lexical tokens that are not words.

NUMBER
    :    DIGIT+ '.' DIGIT+ EXPONENT?
    |    DIGIT+ EXPONENT?
    |    '.' DIGIT+ EXPONENT?
    ;

HEX_NUMBER
    :    '0x' HEX_DIGIT+
    |    '0X' HEX_DIGIT+
    ;

RAW_SINGLE_LINE_STRING
    :    'r' '\'' (~('\'' | '\r' | '\n'))* '\''
    |    'r' '"' (~('"' | '\r' | '\n'))* '"'
    ;

RAW_MULTI_LINE_STRING
    :    'r' '"""' (.)*? '"""'
    |    'r' '\'\'\'' (.)*? '\'\'\''
    ;

fragment
SIMPLE_STRING_INTERPOLATION
    :    '$' IDENTIFIER_NO_DOLLAR
    ;

fragment
ESCAPE_SEQUENCE
    :    '\\n'
    |    '\\r'
    |    '\\b'
    |    '\\t'
    |    '\\v'
    |    '\\x' HEX_DIGIT HEX_DIGIT
    |    '\\u' HEX_DIGIT HEX_DIGIT HEX_DIGIT HEX_DIGIT
    |    '\\u{' HEX_DIGIT_SEQUENCE '}'
    ;

fragment
HEX_DIGIT_SEQUENCE
    :    HEX_DIGIT HEX_DIGIT? HEX_DIGIT?
         HEX_DIGIT? HEX_DIGIT? HEX_DIGIT?
    ;

fragment
STRING_CONTENT_COMMON
    :    ~('\\' | '\'' | '"' | '$' | '\r' | '\n')
    |    ESCAPE_SEQUENCE
    |    '\\' ~('n' | 'r' | 'b' | 't' | 'v' | 'x' | 'u' | '\r' | '\n')
    |    SIMPLE_STRING_INTERPOLATION
    ;

fragment
STRING_CONTENT_SQ
    :    STRING_CONTENT_COMMON
    |    '"'
    ;

SINGLE_LINE_STRING_SQ_BEGIN_END
    :    '\'' STRING_CONTENT_SQ* '\''
    ;

SINGLE_LINE_STRING_SQ_BEGIN_MID
    :    '\'' STRING_CONTENT_SQ* '${' { enterBraceSingleQuote(); }
    ;

SINGLE_LINE_STRING_SQ_MID_MID
    :    { braceLevelSingle() }?
         { exitBrace(); } '}' STRING_CONTENT_SQ* '${'
         { enterBraceSingleQuote(); }
    ;

SINGLE_LINE_STRING_SQ_MID_END
    :    { braceLevelSingle() }?
         { exitBrace(); } '}' STRING_CONTENT_SQ* '\''
    ;

fragment
STRING_CONTENT_DQ
    :    STRING_CONTENT_COMMON
    |    '\''
    ;

SINGLE_LINE_STRING_DQ_BEGIN_END
    :    '"' STRING_CONTENT_DQ* '"'
    ;

SINGLE_LINE_STRING_DQ_BEGIN_MID
    :    '"' STRING_CONTENT_DQ* '${' { enterBraceDoubleQuote(); }
    ;

SINGLE_LINE_STRING_DQ_MID_MID
    :    { braceLevelDouble() }?
         { exitBrace(); } '}' STRING_CONTENT_DQ* '${'
         { enterBraceDoubleQuote(); }
    ;

SINGLE_LINE_STRING_DQ_MID_END
    :    { braceLevelDouble() }?
         { exitBrace(); } '}' STRING_CONTENT_DQ* '"'
    ;

fragment
QUOTES_SQ
    :
    |    '\''
    |    '\'\''
    ;

// Read string contents, which may be almost anything, but stop when seeing
// '\'\'\'' and when seeing '${'. We do this by allowing all other
// possibilities including escapes, simple interpolation, and fewer than
// three '\''.
fragment
STRING_CONTENT_TSQ
    :    QUOTES_SQ
         (STRING_CONTENT_COMMON | '"' | '\r' | '\n' | '\\\r' | '\\\n')
    ;

MULTI_LINE_STRING_SQ_BEGIN_END
    :    '\'\'\'' STRING_CONTENT_TSQ* '\'\'\''
    ;

MULTI_LINE_STRING_SQ_BEGIN_MID
    :    '\'\'\'' STRING_CONTENT_TSQ* QUOTES_SQ '${'
         { enterBraceThreeSingleQuotes(); }
    ;

MULTI_LINE_STRING_SQ_MID_MID
    :    { braceLevelTripleSingle() }?
         { exitBrace(); } '}' STRING_CONTENT_TSQ* QUOTES_SQ '${'
         { enterBraceThreeSingleQuotes(); }
    ;

MULTI_LINE_STRING_SQ_MID_END
    :    { braceLevelTripleSingle() }?
         { exitBrace(); } '}' STRING_CONTENT_TSQ* '\'\'\''
    ;

fragment
QUOTES_DQ
    :
    |    '"'
    |    '""'
    ;

// Read string contents, which may be almost anything, but stop when seeing
// '"""' and when seeing '${'. We do this by allowing all other possibilities
// including escapes, simple interpolation, and fewer-than-three '"'.
fragment
STRING_CONTENT_TDQ
    :    QUOTES_DQ
         (STRING_CONTENT_COMMON | '\'' | '\r' | '\n' | '\\\r' | '\\\n')
    ;

MULTI_LINE_STRING_DQ_BEGIN_END
    :    '"""' STRING_CONTENT_TDQ* '"""'
    ;

MULTI_LINE_STRING_DQ_BEGIN_MID
    :    '"""' STRING_CONTENT_TDQ* QUOTES_DQ '${'
         { enterBraceThreeDoubleQuotes(); }
    ;

MULTI_LINE_STRING_DQ_MID_MID
    :    { braceLevelTriple() }?
         { exitBrace(); } '}' STRING_CONTENT_TDQ* QUOTES_DQ '${'
         { enterBraceThreeDoubleQuotes(); }
    ;

MULTI_LINE_STRING_DQ_MID_END
    :    { braceLevelTriple() }?
         { exitBrace(); } '}' STRING_CONTENT_TDQ* '"""'
    ;

LBRACE
    :    '{' { enterBrace(); }
    ;

RBRACE
    :    { braceLevelNormal() }? { exitBrace(); } '}'
    ;

fragment
IDENTIFIER_START_NO_DOLLAR
    :    LETTER
    |    '_'
    ;

fragment
IDENTIFIER_PART_NO_DOLLAR
    :    IDENTIFIER_START_NO_DOLLAR
    |    DIGIT
    ;

fragment
IDENTIFIER_NO_DOLLAR
    :    IDENTIFIER_START_NO_DOLLAR IDENTIFIER_PART_NO_DOLLAR*
    ;

fragment
IDENTIFIER_START
    :    IDENTIFIER_START_NO_DOLLAR
    |    '$'
    ;

fragment
IDENTIFIER_PART
    :    IDENTIFIER_START
    |    DIGIT
    ;

SCRIPT_TAG
    :    '#!' (~('\r' | '\n'))* NEWLINE
    ;

IDENTIFIER
    :    IDENTIFIER_START IDENTIFIER_PART*
    ;

SINGLE_LINE_COMMENT
    :    '//' (~('\r' | '\n'))* NEWLINE?
         { skip(); }
    ;

MULTI_LINE_COMMENT
    :    '/*' (MULTI_LINE_COMMENT | .)*? '*/'
         { skip(); }
    ;

fragment
NEWLINE
    :    ('\r' | '\n' | '\r\n')
    ;

FEFF
    :    '\uFEFF'
    ;

WS
    :    (' ' | '\t' | '\r' | '\n')+
         { skip(); }
    ;
