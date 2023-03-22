parser grammar BibTeX;

options { tokenVocab=BibTeXLexer; }

bibTex
    : entry*
    ;

// Entries

entry
    : article
    | book
    | booklet
    | inbook
    | incollection
    | inproceedings
    | proceedings
    | manual
    | mastersthesis
    | phdthesis
    | misc
    | techreport
    | unpublished
    ;

field
    : key EQ value COMMA
    ;

key
    : IDENTIFIER
    ;

value
    : INTEGER_LITERAL
    | STRING_LITERAL
    ;

article
    : ARTICLE LBRACE IDENTIFIER COMMA field+ RBRACE
    ;

book
    : BOOK LBRACE IDENTIFIER COMMA field+ RBRACE
    ;

booklet
    : BOOKLET LBRACE IDENTIFIER COMMA field+ RBRACE
    ;

inbook
    : INBOOK LBRACE IDENTIFIER COMMA field+ RBRACE
    ;

incollection
    : INCOLLECTION LBRACE IDENTIFIER COMMA field+ RBRACE
    ;

inproceedings
    : INPROCEEDINGS LBRACE IDENTIFIER COMMA field+ RBRACE
    ;

proceedings
    : PROCEEDINGS LBRACE IDENTIFIER COMMA field+ RBRACE
    ;

manual
    : MANUAL LBRACE IDENTIFIER COMMA field+ RBRACE
    ;

mastersthesis
    : MASTERTHESIS LBRACE IDENTIFIER COMMA field+ RBRACE
    ;

phdthesis
    : PHDTHESIS LBRACE IDENTIFIER COMMA field+ RBRACE
    ;

misc
    : MISC LBRACE IDENTIFIER COMMA field+ RBRACE
    ;

techreport
    : TECHREPORT LBRACE IDENTIFIER COMMA field+ RBRACE
    ;

unpublished
    : UNPUBLISHED LBRACE IDENTIFIER COMMA field+ RBRACE
    ;