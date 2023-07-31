/*
Python grammar
The MIT License (MIT)
Copyright (c) 2021 Robert Einhorn

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
 */

 /*
  * Project      : an ANTLR4 lexer grammar for Tiny Python
  *                https://github.com/RobEin/tiny-python
  * Developed by : Robert Einhorn
  */

lexer grammar PythonLexer;
options { superClass=PythonLexerBase; }
tokens { INDENT, DEDENT,
// *** the following tokens are only for compatibility with the PythonLexerBase class ***
         OPEN_BRACK, CLOSE_BRACK, OPEN_BRACE, CLOSE_BRACE, TYPE_COMMENT
}

/*
 * lexer rules    https://docs.python.org/3/reference/lexical_analysis.html
 */

ELSE     : 'else';
BREAK    : 'break';
CONTINUE : 'continue';
WHILE    : 'while';
ELIF     : 'elif';
IF       : 'if';

OPEN_PAREN   : '(';  // LPAR
CLOSE_PAREN  : ')';  // RPAR
COLON        : ':';
PLUS         : '+';
MINUS        : '-';
LESS         : '<';
GREATER      : '>';
EQUAL        : '=';
EQEQUAL      : '==';
NOTEQUAL     : '!=';
LESSEQUAL    : '<=';
GREATEREQUAL : '>=';

NAME
    : ID_START ID_CONTINUE*
    ;

NUMBER
    : INTEGER
    ;

//STRING
//    : STRING_LITERAL
//    ;

NEWLINE
    : OS_INDEPENDENT_NL
    ;

COMMENT      : '#' ~[\r\n\f]* -> channel(HIDDEN);
WS           : [ \t]+         -> channel(HIDDEN);
LINE_JOINING : '\\' NEWLINE   -> channel(HIDDEN);


/*
 * fragments
 */

//fragment STRING_LITERAL : '"' .*? '"';

fragment INTEGER        : DEC_INTEGER;
fragment DEC_INTEGER    : NON_ZERO_DIGIT DIGIT* | '0';
fragment NON_ZERO_DIGIT : [1-9];
fragment DIGIT          : [0-9];

fragment OS_INDEPENDENT_NL : '\r'? '\n'; // Unix, Windows

fragment ID_CONTINUE
 : ID_START
 | [0-9]
 ;

fragment ID_START
 : '_'
 | [A-Z]
 | [a-z]
 ;
