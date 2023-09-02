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
tokens { // https://docs.python.org/3.8/reference/lexical_analysis.html#indentation
    INDENT, DEDENT,
    // the following tokens are only for compatibility with the PythonLexerBase class:
    LSQB, RSQB, LBRACE, RBRACE, TYPE_COMMENT
}

/*
 * lexer rules    https://docs.python.org/3.8/reference/lexical_analysis.html#
 */

// https://docs.python.org/3.8/reference/lexical_analysis.html#keywords
ELSE     : 'else';
BREAK    : 'break';
CONTINUE : 'continue';
WHILE    : 'while';
ELIF     : 'elif';
IF       : 'if';

//**** the following two tokens are for demonstration only ****
PRINT    : 'print'; // "print" is not a reserved keyword in Python 3
INPUT    : 'input'; // "input" is not a reserved keyword in Python 3


// https://docs.python.org/3.8/library/token.html#token.OP
LPAR         : '(';  // OPEN_PAREN
RPAR         : ')';  // CLOSE_PAREN
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

// // https://docs.python.org/3.8/reference/lexical_analysis.html#identifiers
NAME
   : ID_START ID_CONTINUE*
   ;

// https://docs.python.org/3.8/reference/lexical_analysis.html#numeric-literals
NUMBER
   : INTEGER
   ;

// // https://docs.python.org/3.8/reference/lexical_analysis.html#string-and-bytes-literals
STRING
   : STRING_LITERAL
   ;

// https://docs.python.org/3.8/reference/lexical_analysis.html#physical-lines
NEWLINE : '\r'? '\n'; // Unix, Windows

// https://docs.python.org/3.8/reference/lexical_analysis.html#comments
COMMENT : '#' ~[\r\n\f]*             -> channel(HIDDEN);

// https://docs.python.org/3.8/reference/lexical_analysis.html#whitespace-between-tokens
WS : [ \t]+                          -> channel(HIDDEN);

// https://docs.python.org/3.8/reference/lexical_analysis.html#explicit-line-joining
EXPLICIT_LINE_JOINING : '\\' NEWLINE -> channel(HIDDEN);


/*
 * fragments
 */

// https://docs.python.org/3.8/reference/lexical_analysis.html#literals

fragment STRING_LITERAL : '"' .*? '"';

// https://docs.python.org/3.8/reference/lexical_analysis.html#integer-literals
fragment INTEGER        : DEC_INTEGER;
fragment DEC_INTEGER    : NON_ZERO_DIGIT DIGIT* | '0';
fragment NON_ZERO_DIGIT : [1-9];
fragment DIGIT          : [0-9];

fragment ID_CONTINUE
   : ID_START
   | [0-9]
   ;

fragment ID_START
   : '_'
   | [A-Z]
   | [a-z]
   ;
