lexer grammar BibTeXLexer;
import CaseInsensitiveFragments;

// Keywords
ARTICLE:
    AT A R T I C L E
    ;

BOOK:
    AT B O O K
    ;

BOOKLET:
    AT B O O K L E T
    ;

INBOOK:
    AT I N B O O K
    ;

INCOLLECTION:
    AT I N C O L L E C T I O N
    ;

INPROCEEDINGS:
    AT I N P R O C E E D I N G S
    ;

PROCEEDINGS:
    AT P R O C E E D I N G S
    ;

MANUAL:
    AT M A N U A L
    ;

MASTERTHESIS:
    AT M A S T E R T H E S I S
    ;

PHDTHESIS:
    AT P H D T H E S I S
    ;

MISC:
    AT M I S C
    ;

TECHREPORT:
    AT T E C H R E P O R T
    ;

UNPUBLISHED:
    AT U N P U B L I S H E D
    ;


// Identifiers

IDENTIFIER
    : [a-zA-Z_] [a-zA-Z_0-9-]*
    ;

// Operators

EQ
    : '='
    ;


// Punctuation

COMMA
    : ','
    ;

DQUOTE
    : '"'
    ;

// Delimiters

LPAREN
    : '('
    ;
RPAREN
    : ')'
    ;
LBRACE
    : '{'
    ;
RBRACE
    : '}'
    ;

// Symbols

AT
    : '@'
    ;


// Literals

STRING_LITERAL
    : LBRACE (ESC | BRACE_ENCLOSED_SAFECODEPOINT)* RBRACE
    | DQUOTE (ESC | QUOTE_ENCLOSED_SAFECODEPOINT)* DQUOTE
    ;

INTEGER_LITERAL
    : (LBRACE | DQUOTE)? INTEGER (RBRACE | DQUOTE)?
    ;

// Fragments

fragment ESC
   : '\\' (["\\/bfnrt] | UNICODE)
   ;

fragment UNICODE
   : 'u' HEX HEX HEX HEX
   ;

fragment INTEGER
   : '0' | [1-9] [0-9]*
   ;

fragment HEX
   : [0-9a-fA-F]
   ;

fragment QUOTE_ENCLOSED_SAFECODEPOINT
   : ~ ["\\\u0000-\u001F]
   ;

fragment BRACE_ENCLOSED_SAFECODEPOINT
   : ~ [\\\u0000-\u001F]
   ;

// Whitespace and Comment

WS:
    [ \t\r\n\u000C]+ -> skip
    ;

LINE_COMMENT
    : '%' ~[\r\n]*    -> skip
    ;
