/*
BSD License

Lobster grammar for ANTLR4.
Lobster is a statically typed, lexically scoped, garbage collected, embeddable
scripting language, written in C++. Source: https://github.com/aardappel/lobster
Language reference used for this grammar: https://aardappel.github.io/lobster/language_reference.html

Lobster uses significant indentation (like Python) to delimit the bodies of
functions, classes, and control-flow statements rather than braces or 'end'
keywords, so this lexer follows the well known INDENT/DEDENT technique used
by the Python3 / GDScript grammars elsewhere in this project: a hand written
base lexer class (see Java/LobsterLexerBase.java and CSharp/LobsterLexerBase.cs)
synthesizes INDENT and DEDENT tokens whenever the indentation level changes.
*/

lexer grammar LobsterLexer;

tokens {
    INDENT,
    DEDENT
}

options {
    superClass = LobsterLexerBase;
}

// ---------------------------------------------------------------------------
// Keywords
// ---------------------------------------------------------------------------

NIL            : 'nil';
RETURN         : 'return';
CLASS          : 'class';
STRUCT         : 'struct';
IMPORT         : 'import';
KW_INT         : 'int';
KW_FLOAT       : 'float';
KW_STRING      : 'string';
KW_ANY         : 'any';
KW_VOID        : 'void';
DEF            : 'def';
FN             : 'fn';
IS             : 'is';
FROM           : 'from';
PROGRAM        : 'program';
PRIVATE        : 'private';
RESOURCE       : 'resource';
ENUM_FLAGS     : 'enum_flags';
ENUM           : 'enum';
TYPEOF         : 'typeof';
VAR            : 'var';
LET            : 'let';
PAKFILE        : 'pakfile';
SWITCH         : 'switch';
CASE           : 'case';
DEFAULT        : 'default';
OUT_OF_RANGE   : 'out_of_range';
NAMESPACE      : 'namespace';
NOT            : 'not';
AND            : 'and';
OR             : 'or';
ATTRIBUTE      : 'attribute';
IF             : 'if';
ELIF           : 'elif';
ELSE           : 'else';
FOR            : 'for';
WHILE          : 'while';
SUPER          : 'super';
CONSTRUCTOR    : 'constructor';
GUARD          : 'guard';
ABSTRACT       : 'abstract';
UNION          : 'union';
MEMBER_FRAME   : 'member_frame';
MEMBER         : 'member';
STATIC_FRAME   : 'static_frame';
STATIC         : 'static';
OPERATOR       : 'operator';
DO             : 'do';
BREAK          : 'break';
CONTINUE       : 'continue';
ASSERT         : 'assert';
IN             : 'in';
TRUE           : 'true';
FALSE          : 'false';

// ---------------------------------------------------------------------------
// Line breaks / indentation (handled by LobsterLexerBase)
// ---------------------------------------------------------------------------

NEWLINE: ({atStartOfInput()}? SPACE | ('\r'? '\n' | '\r' | '\f') SPACE?) {onNewLine();};

// ---------------------------------------------------------------------------
// Identifiers
// ---------------------------------------------------------------------------

IDENTIFIER: [a-zA-Z_] [a-zA-Z_0-9]*;

// ---------------------------------------------------------------------------
// Literals
// ---------------------------------------------------------------------------

FLOAT_LIT        : DEC+ '.' DEC* | '.' DEC+;
INT_LIT          : '0' [xX] HEXD+ | DEC+;
fragment DEC     : [0-9];
fragment HEXD    : [0-9a-fA-F];

TRIPLE_STRING_LIT: '"""' .*? '"""';
STRING_LIT       : '"' (ESC | ~["\\\r\n])* '"';
CHAR_LIT         : '\'' (ESC | ~['\\\r\n]) '\'';
fragment ESC     : '\\' ([ntr"'\\] | 'x' HEXD HEXD);

// ---------------------------------------------------------------------------
// Punctuation / operators
// ---------------------------------------------------------------------------

LPAREN      : '(' {openBrace();};
RPAREN      : ')' {closeBrace();};
LBRACK      : '[' {openBrace();};
RBRACK      : ']' {closeBrace();};
LBRACE      : '{' {openBrace();};
RBRACE      : '}' {closeBrace();};

DOUBLECOLON : '::';
COLON       : ':';
COMMA       : ',';

INC         : '++';
PLUS_ASSIGN : '+=';
PLUS        : '+';
DEC_OP      : '--';
MINUS_ASSIGN: '-=';
ARROW       : '->';
MINUS       : '-';
STAR_ASSIGN : '*=';
STAR        : '*';
SLASH_ASSIGN: '/=';
SLASH       : '/';
PERCENT_ASSIGN: '%=';
PERCENT     : '%';

EQ          : '==';
NEQ         : '!=';
LE          : '<=';
GE          : '>=';
SHL         : '<<';
SHR         : '>>';
LARROW      : '<-';
LT          : '<';
GT          : '>';

ASSIGN      : '=';
QUESTION    : '?';
RANGE       : '..';
DOT         : '.';
AMP         : '&';
PIPE        : '|';
CARET       : '^';
TILDE       : '~';

// ---------------------------------------------------------------------------
// Comments & whitespace
// ---------------------------------------------------------------------------

LINE_COMMENT : '//' ~[\r\n]* -> skip;
BLOCK_COMMENT: '/*' (BLOCK_COMMENT | .)*? '*/' -> skip;

WS: SPACE -> skip;

fragment SPACE: [ \t]+;
