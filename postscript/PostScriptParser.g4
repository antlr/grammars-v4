/*
BSD License

Parser grammar for PostScript. See PostScriptLexer.g4 for background.

A PostScript program is simply a sequence of objects executed in order;
procedures ('{' ... '}') and arrays ('[' ... ']') are themselves just
objects containing a nested sequence of objects. Control structures
(if/ifelse/for/repeat/loop/forall), definitions (def), and every operator
are not part of the grammar at all - they are ordinary executable names
that operate on the stack and dictionaries at run time, so this grammar
has no keyword list and no statement forms beyond "object".

Entry rule: program
*/

parser grammar PostScriptParser;

options {
    tokenVocab = PostScriptLexer;
}

program
    : object* EOF
    ;

object
    : INTEGER
    | REAL
    | RADIX
    | STRING
    | HEXSTRING
    | BASE85STRING
    | LITERAL_NAME
    | IMMEDIATE_NAME
    | EXECUTABLE_NAME
    | procedure
    | array
    ;

procedure
    : LBRACE object* RBRACE
    ;

array
    : LBRACKET object* RBRACKET
    ;
