/*
 MAML - Minimal Abstract Markup Language.

 "JSON with comments, unquoted keys, optional commas, and multiline strings."

 Derived from the MAML v0.1 specification: https://maml.dev/spec/v0.1
 See also https://maml.dev/ and ../json5/JSON5.g4 for prior art.

 Note on separators: per the spec, members and items are separated by a comma
 OR a newline (separator = "," / newline). A comma is "optional" only in the
 sense that a newline may stand in for it - two values on the SAME line with
 only spaces between them are NOT valid. Newlines are therefore significant
 tokens here, not skipped whitespace.
 */

// $antlr-format alignTrailingComments true, columnLimit 150, minEmptyLines 1, maxEmptyLinesToKeep 1, reflowComments false, useTab false
// $antlr-format allowShortRulesOnASingleLine false, allowShortBlocksOnASingleLine true, alignSemicolons hanging, alignColons hanging

grammar MAML;

// maml = ws-comment-newline value ws-comment-newline
maml
    : NL* value NL* EOF
    ;

value
    : object
    | array
    | STRING
    | RAW_STRING
    | NUMBER
    | TRUE
    | FALSE
    | NULL
    ;

// object = "{" [ members ] ws-comment-newline "}"
object
    : '{' NL* (member (separator member)* separator?)? '}'
    ;

// key-value = key ws-comment-newline ":" ws-comment-newline value
member
    : key NL* ':' NL* value
    ;

key
    : IDENTIFIER
    | STRING
    | NUMBER // digits-only keys are allowed (interpreted as strings)
    | TRUE
    | FALSE
    | NULL
    ;

// array = "[" [ items ] ws-comment-newline "]"
array
    : '[' NL* (value (separator value)* separator?)? ']'
    ;

// separator = "," / newline. A comma may be followed by further newlines
// (insignificant filler), but a newline-separator may not be followed by a
// comma, and two commas in a row are not allowed.
separator
    : ',' NL*
    | NL+
    ;

// Lexer

TRUE
    : 'true'
    ;

FALSE
    : 'false'
    ;

NULL
    : 'null'
    ;

// A hash marks the rest of the line as a comment, except inside a string. The
// terminating newline is left for NL so it can still act as a separator.
COMMENT
    : '#' ~[\r\n]* -> channel(HIDDEN)
    ;

// Raw strings are bounded by """ and preserve their contents literally. Listed
// before STRING so the """ delimiter is never read as an empty "" plus a quote.
// At least one character is required: empty single-line raw strings are not
// supported by the spec (use "" instead).
RAW_STRING
    : '"""' . .*? '"""'
    ;

STRING
    : '"' CHAR* '"'
    ;

// Any char but ", backslash, or a control char; tab is allowed.
fragment CHAR
    : ESCAPE
    | ~["\\\u0000-\u0008\u000A-\u001F]
    ;

fragment ESCAPE
    : '\\' (["\\tnr] | 'u{' HEX HEX? HEX? HEX? HEX? HEX? '}')
    ;

NUMBER
    : '-'? INT ('.' [0-9]+)? EXP?
    ;

fragment INT
    : '0'
    | [1-9] [0-9]*
    ;

fragment EXP
    : [eE] [+-]? [0-9]+
    ;

fragment HEX
    : [0-9a-fA-F]
    ;

// Identifier keys contain only A-Z, a-z, 0-9, _ and -.
IDENTIFIER
    : [A-Za-z0-9_-]+
    ;

// Newline (LF or CRLF) is significant: it separates members and items.
NL
    : ('\r'? '\n')+
    ;

// Spaces and tabs are insignificant.
WS
    : [ \t]+ -> channel(HIDDEN)
    ;
