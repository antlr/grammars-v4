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
  * Project      : an ANTLR4 lexer grammar for Python 2.7.18
  *                https://github.com/RobEin/ANTLR4-parser-for-Python-2.7.18
  * Developed by : Robert Einhorn, robert.einhorn.hu@gmail.com
  *
  */

lexer grammar PythonLexer;
options { superClass=PythonLexerBase; }
tokens { INDENT, DEDENT } // https://docs.python.org/2.7/reference/lexical_analysis.html#indentation

/*
 * lexer rules    // https://docs.python.org/2.7/library/tokenize.html
 */

// https://docs.python.org/2.7/reference/lexical_analysis.html#keywords
AND      : 'and';
AS       : 'as';
ASSERT   : 'assert';
BREAK    : 'break';
CLASS    : 'class';
CONTINUE : 'continue';
DEF      : 'def';
DEL      : 'del';
ELIF     : 'elif';
ELSE     : 'else';
EXCEPT   : 'except';
EXEC     : 'exec';
FINALLY  : 'finally';
FOR      : 'for';
FROM     : 'from';
GLOBAL   : 'global';
IF       : 'if';
IMPORT   : 'import';
IN       : 'in';
IS       : 'is';
LAMBDA   : 'lambda';
NOT      : 'not';
OR       : 'or';
PASS     : 'pass';
PRINT    : 'print';
RAISE    : 'raise';
RETURN   : 'return';
TRY      : 'try';
WHILE    : 'while';
WITH     : 'with';
YIELD    : 'yield';

// https://docs.python.org/2.7/library/token.html#token.OP
LPAR             : '(';  // OPEN_PAREN
LSQB             : '[';  // OPEN_BRACK
LBRACE           : '{';  // OPEN_BRACE
RPAR             : ')';  // CLOSE_PAREN
RSQB             : ']';  // CLOSE_BRACK
RBRACE           : '}';  // CLOSE_BRACE
COLON            : ':';
COMMA            : ',';
SEMI             : ';';
PLUS             : '+';
MINUS            : '-';
STAR             : '*';
SLASH            : '/';
VBAR             : '|';
AMPER            : '&';
LESS             : '<';
GREATER          : '>';
EQUAL            : '=';
DOT              : '.';
PERCENT          : '%';
BACKQUOTE        : '`';
EQEQUAL          : '==';
INEQUAL          : '<>';
NOTEQUAL         : '!=';
LESSEQUAL        : '<=';
GREATEREQUAL     : '>=';
TILDE            : '~';
CIRCUMFLEX       : '^';
LEFTSHIFT        : '<<';
RIGHTSHIFT       : '>>';
DOUBLESTAR       : '**';
PLUSEQUAL        : '+=';
MINEQUAL         : '-=';
STAREQUAL        : '*=';
SLASHEQUAL       : '/=';
PERCENTEQUAL     : '%=';
AMPEREQUAL       : '&=';
VBAREQUAL        : '|=';
CIRCUMFLEXEQUAL  : '^=';
LEFTSHIFTEQUAL   : '<<=';
RIGHTSHIFTEQUAL  : '>>=';
DOUBLESTAREQUAL  : '**=';
DOUBLESLASH      : '//';
DOUBLESLASHEQUAL : '//=';
AT               : '@';


// https://docs.python.org/2.7/reference/lexical_analysis.html#identifiers
NAME : IDENTIFIER;

// https://docs.python.org/2.7/reference/lexical_analysis.html#numeric-literals
NUMBER
   : INTEGER
   | LONG_INTEGER
   | FLOAT_NUMBER
   | IMAG_NUMBER
   ;

// https://docs.python.org/2.7/reference/lexical_analysis.html#string-literals
STRING : STRING_LITERAL;

// https://docs.python.org/2.7/reference/lexical_analysis.html#physical-lines
NEWLINE : OS_INDEPENDENT_NL;

// https://docs.python.org/2.7/reference/lexical_analysis.html#comments
COMMENT : '#' ~[\r\n]*               -> channel(HIDDEN);

// https://docs.python.org/2.7/reference/lexical_analysis.html#whitespace-between-tokens
WS : [ \t\f]+                        -> channel(HIDDEN);

// https://docs.python.org/2.7/reference/lexical_analysis.html#explicit-line-joining
EXPLICIT_LINE_JOINING : '\\' NEWLINE -> channel(HIDDEN);

ERROR_TOKEN : . ; // catch unrecognized characters and redirect these errors to the parser


/*
 * fragments
 */

// https://docs.python.org/2.7/reference/lexical_analysis.html#literals

// https://docs.python.org/2.7/reference/lexical_analysis.html#string-literals
fragment STRING_LITERAL : STRING_PREFIX? (SHORT_STRING | LONG_STRING);
fragment STRING_PREFIX  : 'r' | 'u' | 'ur' | 'R' | 'U' | 'UR' | 'Ur' | 'uR' | 'b' | 'B' | 'br' | 'Br' | 'bR' | 'BR';

fragment SHORT_STRING
   : '\'' SHORT_STRING_ITEM_FOR_SINGLE_QUOTE* '\''
   | '"'  SHORT_STRING_ITEM_FOR_DOUBLE_QUOTE* '"'
   ;

fragment LONG_STRING
   : '\'\'\'' LONG_STRING_ITEM*? '\'\'\''
   | '"""'    LONG_STRING_ITEM*? '"""'
   ;

fragment SHORT_STRING_ITEM_FOR_SINGLE_QUOTE : SHORT_STRING_CHAR_NO_SINGLE_QUOTE | ESCAPE_SEQ;
fragment SHORT_STRING_ITEM_FOR_DOUBLE_QUOTE : SHORT_STRING_CHAR_NO_DOUBLE_QUOTE | ESCAPE_SEQ;

fragment LONG_STRING_ITEM : LONG_STRING_CHAR | ESCAPE_SEQ;

fragment SHORT_STRING_CHAR_NO_SINGLE_QUOTE : ~[\\\r\n'];      // <any source character except "\" or newline or single quote>
fragment SHORT_STRING_CHAR_NO_DOUBLE_QUOTE : ~[\\\r\n"];      // <any source character except "\" or newline or double quote>
fragment LONG_STRING_CHAR  : ~'\\';                           // <any source character except "\">
fragment ESCAPE_SEQ
   : '\\' OS_INDEPENDENT_NL // \<newline> escape sequence
   | '\\' [\u0000-\u007F]                                    // "\" <any ASCII character>
   ; // the \<newline> (not \n) escape sequences will be removed from the string literals by the PythonLexerBase class

// https://docs.python.org/2.7/reference/lexical_analysis.html#integer-and-long-integer-literals
fragment LONG_INTEGER    : INTEGER ('l' | 'L');
fragment INTEGER         : DECIMAL_INTEGER | OCT_INTEGER | HEX_INTEGER | BIN_INTEGER;
fragment DECIMAL_INTEGER : NON_ZERO_DIGIT DIGIT* | '0';
fragment OCT_INTEGER     : '0' ('o' | 'O') OCT_DIGIT+ | '0' OCT_DIGIT+;
fragment HEX_INTEGER     : '0' ('x' | 'X') HEX_DIGIT+;
fragment BIN_INTEGER     : '0' ('b' | 'B') BIN_DIGIT+;
fragment NON_ZERO_DIGIT  : [1-9];
fragment OCT_DIGIT       : [0-7];
fragment BIN_DIGIT       : '0' | '1';
fragment HEX_DIGIT       : DIGIT | [a-f] | [A-F];

// https://docs.python.org/2.7/reference/lexical_analysis.html#floating-point-literals
fragment FLOAT_NUMBER   : POINT_FLOAT | EXPONENT_FLOAT;
fragment POINT_FLOAT    : INT_PART? FRACTION | INT_PART '.';
fragment EXPONENT_FLOAT : (INT_PART | POINT_FLOAT) EXPONENT;
fragment INT_PART       : DIGIT+;
fragment FRACTION       : '.' DIGIT+;
fragment EXPONENT       : ('e' | 'E') ('+' | '-')? DIGIT+;

// https://docs.python.org/2.7/reference/lexical_analysis.html#imaginary-literals
fragment IMAG_NUMBER : (FLOAT_NUMBER | INT_PART) ('j' | 'J');

// https://docs.python.org/2.7/reference/lexical_analysis.html#physical-lines
fragment OS_INDEPENDENT_NL : '\r'? '\n'; // Unix, Windows

// https://docs.python.org/2.7/reference/lexical_analysis.html#identifiers
fragment IDENTIFIER : (LETTER | '_') (LETTER | DIGIT | '_')*;
fragment LETTER     : LOWERCASE | UPPERCASE;
fragment LOWERCASE  : [a-z];
fragment UPPERCASE  : [A-Z];
fragment DIGIT      : [0-9];
