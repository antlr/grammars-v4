/*
Parser grammar for ABC music notation.
References:
 - https://en.wikipedia.org/wiki/ABC_notation
 - https://abcnotation.com/
*/
parser grammar abcParser;

options {
   tokenVocab = abcLexer;
}

abc
   : tune+ EOF
   ;

tune
   : header body
   ;

header
   : XFIELD FIELD* KFIELD
   ;

body
   : element*
   ;

element
   : FIELD
   | inlineField
   | barline
   | note
   | rest
   | chord
   | tie
   | slur
   | broken
   | grace
   | tuplet
   | chordSymbol
   | decoration
   | staccato
   | AMPERSAND
   ;

inlineField
   : INLINE_FIELD
   ;

barline
   : BARLINE NUMBER?
   ;

note
   : ACCIDENTAL? PITCH OCTAVE? noteLength?
   ;

rest
   : REST noteLength?
   ;

noteLength
   : NUMBER (SLASH NUMBER?)*
   | SLASH+ NUMBER?
   ;

chord
   : LBRACKET note+ RBRACKET noteLength?
   ;

tie
   : TIE
   ;

slur
   : SLUR_OPEN
   | SLUR_CLOSE
   ;

broken
   : BROKEN
   ;

grace
   : GRACE_OPEN (note | rest)+ GRACE_CLOSE
   ;

tuplet
   : TUPLET
   ;

chordSymbol
   : CHORD_SYMBOL
   ;

decoration
   : DECORATION
   ;

staccato
   : STACCATO
   ;
