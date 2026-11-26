/*
BSD License

PostScript grammar for ANTLR4.
PostScript (https://en.wikipedia.org/wiki/PostScript) is Adobe's
stack-based page description / programming language (see the "PostScript
Language Reference Manual", 3rd edition, a.k.a. the "Red Book").

PostScript's syntax is unusually simple and uniform: a program is just a
sequence of objects (numbers, strings, names, procedures delimited by
'{' '}', and arrays delimited by '[' ']'). Operators such as "add", "dup",
"if", "ifelse", "for", and "def" are not reserved words at all - they are
ordinary executable names, resolved against the current dictionary stack
at run time - so no keyword list is needed in the grammar; see
PostScriptParser.g4.
*/

lexer grammar PostScriptLexer;

// ---------------------------------------------------------------------------
// Delimiters
// ---------------------------------------------------------------------------

LBRACE  : '{';
RBRACE  : '}';
LBRACKET: '[';
RBRACKET: ']';

// ---------------------------------------------------------------------------
// Numbers (checked before EXECUTABLE_NAME so that, e.g., "42" lexes as a
// number rather than a name; EXECUTABLE_NAME's own character class also
// matches digits, so that something like "3d" - not a valid number - still
// lexes as one name token, exactly as the real PostScript tokenizer does)
// ---------------------------------------------------------------------------

REAL   : SIGN? (DIGIT+ '.' DIGIT* | '.' DIGIT+) ([eE] SIGN? DIGIT+)?
       | SIGN? DIGIT+ [eE] SIGN? DIGIT+
       ;
INTEGER: SIGN? DIGIT+;
RADIX  : DIGIT+ '#' [0-9a-zA-Z]+;

fragment SIGN : [+\-];
fragment DIGIT: [0-9];

// ---------------------------------------------------------------------------
// Strings
// ---------------------------------------------------------------------------

// Literal strings: balanced, nestable parentheses with backslash escapes.
STRING: '(' (ESC_SEQ | STRING | ~[()\\])* ')';
fragment ESC_SEQ
    : '\\' ( [nrtbf\\()]
           | [0-7] [0-7]? [0-7]?
           | '\r\n'
           | '\r'
           | '\n'
           )
    ;

// Hex strings: hex digit pairs, whitespace between them is ignored.
HEXSTRING: '<' (HEXD | [ \t\r\n\f])* '>';
fragment HEXD: [0-9a-fA-F];

// ASCII-85 encoded strings (rare, but part of the language).
BASE85STRING: '<~' .*? '~>';

// ---------------------------------------------------------------------------
// Names
// ---------------------------------------------------------------------------

// An immediately-evaluated name, "//name" (Level 2); checked before
// LITERAL_NAME, though the two can never actually tie: LITERAL_NAME
// requires at least one non-delimiter character after its '/', and '/'
// itself is a delimiter, so "//..." can only ever match here.
IMMEDIATE_NAME: '//' REGULAR_CHAR*;

// A literal name, "/name" (a bare "/" is the valid empty-string name).
LITERAL_NAME: '/' REGULAR_CHAR*;

// Anything else that isn't whitespace, a delimiter, or part of one of the
// tokens above is an executable name - this is how every PostScript
// operator ("add", "dup", "if", "def", ...) and every user-defined
// procedure or variable reference gets lexed.
EXECUTABLE_NAME: REGULAR_CHAR+;

fragment REGULAR_CHAR: ~[ \t\r\n\f\u0000()<>[\]{}/%];

// ---------------------------------------------------------------------------
// Comments & whitespace
// ---------------------------------------------------------------------------

// Includes document-structuring comments ("%!PS-Adobe-3.0", "%%EOF", ...)
// and ordinary "% ..." end-of-line comments alike; neither carries any
// syntactic weight in the object-sequence grammar below.
COMMENT: '%' ~[\r\n]* -> skip;

WS: [ \t\r\n\f\u0000]+ -> skip;
