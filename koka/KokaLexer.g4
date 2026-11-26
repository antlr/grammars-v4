/*
BSD License

Koka grammar for ANTLR4.
Koka (https://koka-lang.github.io/koka/doc/index.html,
https://github.com/koka-lang/koka) is a function-oriented language with
an effect-type-and-handler system as its signature feature.

Koka's real surface syntax is layout-sensitive (Haskell/Python-style: an
indented line implicitly opens a '{', aligned lines get an implicit ';',
and dedenting closes with an implicit '}' and the ';' that is always
inserted before a '}'), but every one of those implicit tokens has an
always-valid explicit spelling ('{', '}', ';') - the compiler's own lexer
(doc/spec/grammar/lexer.l) runs the very same grammar either way. This
grammar targets that explicit, non-layout-sensitive form, since replicating
Koka's layout algorithm exactly (its "continuation" and "ending" token
lists control exactly when a new indented line opens a block) is well
beyond what a static ANTLR grammar can usefully encode. See README.md.

Built from doc/spec/grammar/lexer.l and doc/spec/grammar/parser.y in the
koka-lang/koka repository, and the project's "book" and whatsnew.md.
*/

lexer grammar KokaLexer;

// ---------------------------------------------------------------------------
// Keywords (reserved identifiers)
// ---------------------------------------------------------------------------

MODULE    : 'module';
IMPORT    : 'import';
PUB       : 'pub';
AS        : 'as';
INFIX     : 'infix';
INFIXL    : 'infixl';
INFIXR    : 'infixr';
TYPE      : 'type';
ALIAS     : 'alias';
STRUCT    : 'struct';
EFFECT    : 'effect';
CON       : 'con';
FORALL    : 'forall';
FUN       : 'fun';
FN        : 'fn';
VAL       : 'val';
VAR       : 'var';
IF        : 'if';
THEN      : 'then';
ELSE      : 'else';
ELIF      : 'elif';
MATCH     : 'match';
RETURN    : 'return';
WITH      : 'with';
IN        : 'in';
HANDLE    : 'handle';
HANDLER   : 'handler';
CTL       : 'ctl';
FINAL     : 'final';
RAW       : 'raw';
MASK      : 'mask';
NAMED     : 'named';

// ---------------------------------------------------------------------------
// Identifiers
// ---------------------------------------------------------------------------

fragment ID_CHAR: [a-zA-Z0-9_];

// lower-case identifiers; a '-' is allowed inside a name only when directly
// followed by a letter (so "fold-right" is one identifier, but "n-1" lexes
// as IDENTIFIER("n") MINUS INT(1), not as a single identifier).
IDENTIFIER : [a-z] ID_CHAR* ('-' [a-zA-Z] ID_CHAR*)* '\''*;
CONID      : [A-Z] ID_CHAR* ('-' [a-zA-Z] ID_CHAR*)* '\''*;
WILDCARD   : '_' ID_CHAR*;
IMPLICIT_ID: '?' IDENTIFIER;

// ---------------------------------------------------------------------------
// Literals
// ---------------------------------------------------------------------------

FLOAT_LIT: DEC ('_'? DEC)* '.' DEC ('_'? DEC)* (EXP)?
         | DEC ('_'? DEC)* EXP
         | '0' [xX] HEXD ('_'? HEXD)* '.' HEXD ('_'? HEXD)* [pP] [+-]? DEC+
         ;
fragment EXP: [eE] [+-]? DEC+;

INT_LIT  : DEC ('_'? DEC)*
         | '0' [xX] HEXD ('_'? HEXD)*
         ;
fragment DEC : [0-9];
fragment HEXD: [0-9a-fA-F];

CHAR_LIT  : '\'' (ESC | ~['\\\r\n]) '\'';
STRING_LIT: '"' (ESC | ~["\\\r\n])* '"';
RAW_STRING_LIT: 'r' '"' .*? '"';
fragment ESC: '\\' ([nrtvab0\\'"] | 'x' HEXD HEXD | 'u' HEXD HEXD HEXD HEXD | 'U' HEXD HEXD HEXD HEXD HEXD HEXD);

// ---------------------------------------------------------------------------
// Punctuation / reserved operators (checked before the generic OP catch-all
// below so a lone '=', '.', ':', etc. keeps its distinct token identity)
// ---------------------------------------------------------------------------

LPAREN: '(';
RPAREN: ')';
LBRACK: '[';
RBRACK: ']';
LBRACE: '{';
RBRACE: '}';

ARROW    : '->';
LARROW   : '<-';
DCOLON   : '::';
ASSIGN_OP: ':=';
EQ       : '=';
DOT      : '.';
COLON    : ':';
PIPE     : '|';
LT       : '<';
GT       : '>';
SLASH    : '/';

COMMA: ',';
SEMI : ';';

// Generic run of operator-symbol characters: user-defined operators in Koka
// get their fixity (precedence/associativity) from "infix"/"infixl"/"infixr"
// declarations rather than from the grammar, so (unlike a fixed-precedence
// C-family grammar) every such run is lexed uniformly as one OP token and
// combined at a single, flat, left-associative level in the parser grammar.
OP: [$%&*+~!\\^#=./:\-<>|]+;

// ---------------------------------------------------------------------------
// Comments & whitespace
// ---------------------------------------------------------------------------

LINE_COMMENT : '//' ~[\r\n]* -> skip;
BLOCK_COMMENT: '/*' (BLOCK_COMMENT | .)*? '*/' -> skip;

WS: [ \t\r\n]+ -> skip;
