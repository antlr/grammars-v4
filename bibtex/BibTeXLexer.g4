/*
 * MIT License
 * 
 * Copyright (c) 2023 Yepeng Ding
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
lexer grammar BibTeXLexer;
options { caseInsensitive = true; }

// Keywords
ARTICLE:
    AT 'article'
    ;

BOOK:
    AT 'book'
    ;

BOOKLET:
    AT 'booklet'
    ;

INBOOK:
    AT 'inbook'
    ;

INCOLLECTION:
    AT 'incollection'
    ;

INPROCEEDINGS:
    AT 'inproceedings'
    ;

PROCEEDINGS:
    AT 'proceedings'
    ;

MANUAL:
    AT 'manual'
    ;

MASTERTHESIS:
    AT 'masterthesis'
    ;

PHDTHESIS:
    AT 'phdthesis'
    ;

MISC:
    AT 'misc'
    ;

TECHREPORT:
    AT 'techreport'
    ;

UNPUBLISHED:
    AT 'unpublished'
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