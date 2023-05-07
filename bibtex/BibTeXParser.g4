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
parser grammar BibTeXParser;

options { tokenVocab=BibTeXLexer; }

bibTex
    : entry* EOF
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