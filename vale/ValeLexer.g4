/*
BSD License

Vale grammar for ANTLR4.
Vale (https://vale.dev/, https://github.com/ValeLang/Vale) is a statically
typed, compiled systems language built around region-based memory management
(single ownership, borrow references, and generational/weak references)
instead of a garbage collector or a borrow checker.

Unlike many of the other scripting-language grammars in this project, Vale
uses C-family syntax: braces delimit blocks and statements end with ';', so
no indentation-sensitive lexing is required here.

Built from the (archived, "Vale 0.2 alpha") language guide at
https://vale.dev/guide/introduction and the surrounding guide pages
(structs, interfaces, generics, references, functions, patterns,
collections, modules, regions).
*/

lexer grammar ValeLexer;

// ---------------------------------------------------------------------------
// Keywords
// ---------------------------------------------------------------------------

IMPORT     : 'import';
EXPORTED   : 'exported';
FUNC       : 'func';
STRUCT     : 'struct';
INTERFACE  : 'interface';
IMPL       : 'impl';
FOR        : 'for';
SEALED     : 'sealed';
WEAKABLE   : 'weakable';
IMM        : 'imm';
VIRTUAL    : 'virtual';
SELF       : 'self';
THIS       : 'this';
SET        : 'set';
RETURN     : 'return';
IF         : 'if';
ELSE       : 'else';
WHILE      : 'while';
FOREACH    : 'foreach';
IN         : 'in';
BREAK      : 'break';
CONTINUE   : 'continue';
MATCH      : 'match';
DESTRUCT   : 'destruct';
PURE       : 'pure';
BLOCK      : 'block';
BUMP       : 'bump';
NOT        : 'not';
AND        : 'and';
OR         : 'or';
VOID       : 'void';
KW_INT     : 'int';
KW_STR     : 'str';
KW_BOOL    : 'bool';
KW_FLOAT   : 'float';
TRUE       : 'true';
FALSE      : 'false';

// ---------------------------------------------------------------------------
// Identifiers
// ---------------------------------------------------------------------------

IDENTIFIER: [a-zA-Z_] [a-zA-Z_0-9]*;

// ---------------------------------------------------------------------------
// Literals
// ---------------------------------------------------------------------------

FLOAT_LIT       : DEC+ '.' DEC+;
INT_LIT         : DEC+;
fragment DEC    : [0-9];

STRING_LIT      : '"' (ESC | ~["\\\r\n])* '"';
fragment ESC    : '\\' .;

// ---------------------------------------------------------------------------
// Punctuation / operators
// ---------------------------------------------------------------------------

LPAREN   : '(';
RPAREN   : ')';
LBRACE   : '{';
RBRACE   : '}';
LBRACK   : '[';
RBRACK   : ']';

HASHBANG : '#!';
HASH     : '#';

ARROW2   : '=>';
LE       : '<=';
GE       : '>=';
EQ       : '==';
NEQ      : '!=';
LT       : '<';
GT       : '>';
ASSIGN   : '=';

PLUS     : '+';
MINUS    : '-';
STAR     : '*';
SLASH    : '/';
PERCENT  : '%';

DOT      : '.';
COMMA    : ',';
SEMI     : ';';
COLON    : ':';
AMP      : '&';
CARET    : '^';
QUESTION : '?';
PIPE     : '|';
QUOTE    : '\'';

// ---------------------------------------------------------------------------
// Comments & whitespace
// ---------------------------------------------------------------------------

LINE_COMMENT : '//' ~[\r\n]* -> skip;
BLOCK_COMMENT: '/*' (BLOCK_COMMENT | .)*? '*/' -> skip;

WS: [ \t\r\n]+ -> skip;
