/*
Lexer grammar for ABC music notation.
References:
 - https://en.wikipedia.org/wiki/ABC_notation
 - https://abcnotation.com/
*/
lexer grammar abcLexer;

// ---------------------------------------------------------------------
// Default mode: tune header. Each header field is "<letter>:<value>"
// on its own line. The K: (key) field is always the last header field
// and switches the lexer into the tune body.
// ---------------------------------------------------------------------

COMMENT
   : '%' ~ [\r\n]* -> skip
   ;

WS
   : [ \t\r\n]+ -> skip
   ;

// X: (reference number) starts a new tune, in either mode.
XFIELD
   : 'X' ':' ~ [\r\n]*
   ;

// K: (key) ends the header and starts the tune body.
KFIELD
   : 'K' ':' ~ [\r\n]* -> mode(BODY)
   ;

FIELD
   : [A-Za-z] ':' ~ [\r\n]*
   ;

// ---------------------------------------------------------------------
// Tune body: the music itself.
// ---------------------------------------------------------------------

mode BODY;

BCOMMENT
   : '%' ~ [\r\n]* -> skip
   ;

BWS
   : [ \t\r\n]+ -> skip
   ;

// X: appearing in the body means a new tune is starting.
XFIELD2
   : 'X' ':' ~ [\r\n]* -> mode(DEFAULT_MODE), type(XFIELD)
   ;

// An inline field, e.g. [K:Dm] or [r:allegro], changes a setting
// without leaving the tune body.
INLINE_FIELD
   : '[' [A-Za-z] ':' ~ [\]\r\n]* ']'
   ;

// A whole-line field inside the body, e.g. a lyrics line (w:...),
// a voice line (V:...), or a key/meter/tempo change.
BFIELD
   : [A-Za-z] ':' ~ [\r\n]* -> type(FIELD)
   ;

ACCIDENTAL
   : '^^'
   | '__'
   | '^'
   | '_'
   | '='
   ;

PITCH
   : [A-Ga-g]
   ;

OCTAVE
   : ('\'' | ',')+
   ;

NUMBER
   : [0-9]+
   ;

SLASH
   : '/'
   ;

REST
   : [xzXZ]
   ;

BROKEN
   : '>'
   | '<'
   ;

TIE
   : '-'
   ;

GRACE_OPEN
   : '{'
   ;

GRACE_CLOSE
   : '}'
   ;

TUPLET
   : '(' [0-9] (':' [0-9] (':' [0-9])?)?
   ;

SLUR_OPEN
   : '('
   ;

SLUR_CLOSE
   : ')'
   ;

CHORD_SYMBOL
   : '"' ~ ["\r\n]* '"'
   ;

DECORATION
   : '!' ~ [!\r\n]* '!'
   ;

STACCATO
   : '.'
   ;

AMPERSAND
   : '&'
   ;

BARLINE
   : '::'
   | ':|:'
   | ':|'
   | '|:'
   | '|]'
   | '[|'
   | '||'
   | '|'
   ;

LBRACKET
   : '['
   ;

RBRACKET
   : ']'
   ;
