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
  * Project      : an ANTLR4 lexer grammar for Python 3 programming language
  *                https://github.com/RobEin/ANTLR4-parser-for-Python-3.14
  * Developed by : Robert Einhorn, robert.einhorn.hu@gmail.com
  */

// https://docs.python.org/3.14/reference/lexical_analysis.html
lexer grammar PythonLexer;

// the helper class for this grammar that assists in tokenizing indentation, interpolated strings, and the encoding declaration
options { superClass=PythonLexerBase; }

tokens {
    ENCODING // https://docs.python.org/3.14/reference/lexical_analysis.html#encoding-declarations
  , INDENT, DEDENT // https://docs.python.org/3.14/reference/lexical_analysis.html#indentation
  , TYPE_COMMENT // not supported, only for compatibility with the parser grammar
  , FSTRING_START, FSTRING_MIDDLE, FSTRING_END // https://peps.python.org/pep-0701/#specification
  , TSTRING_START, TSTRING_MIDDLE, TSTRING_END // https://peps.python.org/pep-0750/#specification
}

/*
 *  default lexer mode
 */

// https://docs.python.org/3.14/reference/lexical_analysis.html#encoding-declarations
BOM : '\uFEFF';
// The BOM unicode character indicates that a BOM byte sequence (for Python is only UTF‑8: EF BB BF) was present at the start of the file.
// It is not part of Python source code and is therefore skipped in PythonLexerBase.

// https://docs.python.org/3.14/library/token.html#module-token
LPAR             : '(';
LSQB             : '[';
LBRACE           : '{';
RPAR             : ')';
RSQB             : ']';
RBRACE           : '}';
DOT              : '.';
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
PERCENT          : '%';
EQEQUAL          : '==';
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
ATEQUAL          : '@=';
RARROW           : '->';
ELLIPSIS         : '...';
COLONEQUAL       : ':=';
EXCLAMATION      : '!';

// https://docs.python.org/3.14/reference/lexical_analysis.html#keywords
FALSE    : 'False';
AWAIT    : 'await';
ELSE     : 'else';
IMPORT   : 'import';
PASS     : 'pass';
NONE     : 'None';
BREAK    : 'break';
EXCEPT   : 'except';
IN       : 'in';
RAISE    : 'raise';
TRUE     : 'True';
CLASS    : 'class';
FINALLY  : 'finally';
IS       : 'is';
RETURN   : 'return';
AND      : 'and';
CONTINUE : 'continue';
FOR      : 'for';
LAMBDA   : 'lambda';
TRY      : 'try';
AS       : 'as';
DEF      : 'def';
FROM     : 'from';
NONLOCAL : 'nonlocal';
WHILE    : 'while';
ASSERT   : 'assert';
DEL      : 'del';
GLOBAL   : 'global';
NOT      : 'not';
WITH     : 'with';
ASYNC    : 'async';
ELIF     : 'elif';
IF       : 'if';
OR       : 'or';
YIELD    : 'yield';

// *** Soft Keywords: https://docs.python.org/3.14/reference/lexical_analysis.html#soft-keywords
                            // the parser grammar determines whether it is an ...
NAME_OR_TYPE     : 'type';  // ... identifier or a type  keyword,   depending on the source code context
NAME_OR_MATCH    : 'match'; // ... identifier or a match keyword,   depending on the source code context
NAME_OR_CASE     : 'case';  // ... identifier or a case  keyword,   depending on the source code context
NAME_OR_WILDCARD : '_';     // ... identifier or a wildcard symbol, depending on the source code context

// https://docs.python.org/3.14/reference/lexical_analysis.html#identifiers
NAME : ID_START ID_CONTINUE*;

// https://docs.python.org/3.14/reference/lexical_analysis.html#numeric-literals
NUMBER
    : INTEGER
    | FLOAT_NUMBER
    | IMAG_NUMBER
    ;

// https://docs.python.org/3.14/reference/lexical_analysis.html#string-and-bytes-literals
STRING
    : STRING_LITERAL
    | BYTES_LITERAL
    ;

// https://docs.python.org/3.14/reference/lexical_analysis.html#physical-lines
NEWLINE : '\r'? '\n'; // Unix, Windows

// https://docs.python.org/3.14/reference/lexical_analysis.html#comments
COMMENT : '#' ~[\r\n]*                    -> channel(HIDDEN);

// https://docs.python.org/3.14/reference/lexical_analysis.html#whitespace-between-tokens
WS : [ \t\f]+                             -> channel(HIDDEN);

// https://docs.python.org/3.14/reference/lexical_analysis.html#explicit-line-joining
EXPLICIT_LINE_JOINING : BACKSLASH_NEWLINE -> channel(HIDDEN);

// https://docs.python.org/3.14/reference/lexical_analysis.html#formatted-string-literals
FSTRING_START : FSTRING_PREFIX STRING_QUOTES; // pushMode(...._FSTRING_MODE) is called in PythonLexerBase
TSTRING_START : TSTRING_PREFIX STRING_QUOTES; // pushMode(...._TSTRING_MODE) is called in PythonLexerBase

// catch unrecognized characters
ERRORTOKEN : . ; // the PythonLexerBase class reports a lexer error for them (ERRORTOKEN also triggers a parser error)

/*
 *  lexer modes for interpolation string literals
 */

// **********************************************************************
// Abbreviations for interpolation string literals (f-strings, t-strings)
// **********************************************************************
// SQ1__ISTRING = short single quoted     interpolation string, e.g.:  f'Hello {name}'
// DQ1__ISTRING = short double quoted     interpolation string, e.g.:  f"Hello {name}"
// SQ1R_ISTRING = short single quoted raw interpolation string, e.g.: rf'Hello {name}'
// DQ1R_ISTRING = short double quoted raw interpolation string, e.g.: rf"Hello {name}"
//
// SQ3__ISTRING = long  single quoted     interpolation string, e.g.:  f'''Hello {name}'''
// DQ3__ISTRING = long  double quoted     interpolation string, e.g.:  f"""Hello {name}"""
// SQ3R_ISTRING = long  single quoted raw interpolation string, e.g.: rf'''Hello {name}'''
// DQ3R_ISTRING = long  double quoted raw interpolation string, e.g.: rf"""Hello {name}"""

mode SQ1__FSTRING_MODE;
     SQ1__FSTRING_END    : [']               -> type(FSTRING_END); // popMode() is called in PythonLexerBase
     SQ1__FSTRING_MIDDLE : SQ1__ISTRING_ITEM -> type(FSTRING_MIDDLE);
     SQ1__FSTRING_LBRACE : '{'               -> type(LBRACE);      // pushMode(SQ1__FSTRING_FORMAT_SPECIFICATION_MODE) is called in PythonLexerBase
     SQ1__FSTRING_ERRORTOKEN : .             -> type(ERRORTOKEN);

mode SQ1__TSTRING_MODE;
     SQ1__TSTRING_END    : [']               -> type(TSTRING_END); // popMode() is called in PythonLexerBase
     SQ1__TSTRING_MIDDLE : SQ1__ISTRING_ITEM -> type(TSTRING_MIDDLE);
     SQ1__TSTRING_LBRACE : '{'               -> type(LBRACE);      // pushMode(SQ1__TSTRING_FORMAT_SPECIFICATION_MODE) is called in PythonLexerBase
     SQ1__TSTRING_ERRORTOKEN : .             -> type(ERRORTOKEN);



mode SQ1R_FSTRING_MODE;
     SQ1R_FSTRING_END    : [']               -> type(FSTRING_END); // popMode() is called in PythonLexerBase
     SQ1R_FSTRING_MIDDLE : SQ1R_ISTRING_ITEM -> type(FSTRING_MIDDLE);
     SQ1R_FSTRING_LBRACE : '{'               -> type(LBRACE);      // pushMode(SQ1R_FSTRING_FORMAT_SPECIFICATION_MODE) is called in PythonLexerBase
     SQ1R_FSTRING_ERRORTOKEN : .             -> type(ERRORTOKEN);

mode SQ1R_TSTRING_MODE;
     SQ1R_TSTRING_END    : [']               -> type(TSTRING_END); // popMode() is called in PythonLexerBase
     SQ1R_TSTRING_MIDDLE : SQ1R_ISTRING_ITEM -> type(TSTRING_MIDDLE);
     SQ1R_TSTRING_LBRACE : '{'               -> type(LBRACE);      // pushMode(SQ1R_TSTRING_FORMAT_SPECIFICATION_MODE) is called in PythonLexerBase
     SQ1R_TSTRING_ERRORTOKEN : .             -> type(ERRORTOKEN);



mode DQ1__FSTRING_MODE;
     DQ1__FSTRING_END    : ["]               -> type(FSTRING_END); // popMode() is called in PythonLexerBase
     DQ1__FSTRING_MIDDLE : DQ1__ISTRING_ITEM -> type(FSTRING_MIDDLE);
     DQ1__FSTRING_LBRACE : '{'               -> type(LBRACE);      // pushMode(DQ1__FSTRING_FORMAT_SPECIFICATION_MODE) is called in PythonLexerBase
     DQ1__FSTRING_ERRORTOKEN : .             -> type(ERRORTOKEN);

mode DQ1__TSTRING_MODE;
     DQ1__TSTRING_END    : ["]               -> type(TSTRING_END); // popMode() is called in PythonLexerBase
     DQ1__TSTRING_MIDDLE : DQ1__ISTRING_ITEM -> type(TSTRING_MIDDLE);
     DQ1__TSTRING_LBRACE : '{'               -> type(LBRACE);      // pushMode(DQ1__TSTRING_FORMAT_SPECIFICATION_MODE) is called in PythonLexerBase
     DQ1__TSTRING_ERRORTOKEN : .             -> type(ERRORTOKEN);



mode DQ1R_FSTRING_MODE;
     DQ1R_FSTRING_END    : ["]               -> type(FSTRING_END); // popMode() is called in PythonLexerBase
     DQ1R_FSTRING_MIDDLE : DQ1R_ISTRING_ITEM -> type(FSTRING_MIDDLE);
     DQ1R_FSTRING_LBRACE : '{'               -> type(LBRACE);      // pushMode(DQ1R_FSTRING_FORMAT_SPECIFICATION_MODE) is called in PythonLexerBase
     DQ1R_FSTRING_ERRORTOKEN : .             -> type(ERRORTOKEN);

mode DQ1R_TSTRING_MODE;
     DQ1R_TSTRING_END    : ["]               -> type(TSTRING_END); // popMode() is called in PythonLexerBase
     DQ1R_TSTRING_MIDDLE : DQ1R_ISTRING_ITEM -> type(TSTRING_MIDDLE);
     DQ1R_TSTRING_LBRACE : '{'               -> type(LBRACE);      // pushMode(DQ1R_TSTRING_FORMAT_SPECIFICATION_MODE) is called in PythonLexerBase
     DQ1R_TSTRING_ERRORTOKEN : .             -> type(ERRORTOKEN);



mode SQ3__FSTRING_MODE;
     SQ3__FSTRING_END    : [']['][']         -> type(FSTRING_END); // popMode() is called in PythonLexerBase
     SQ3__FSTRING_MIDDLE : SQ3__ISTRING_ITEM -> type(FSTRING_MIDDLE);
     SQ3__FSTRING_LBRACE : '{'               -> type(LBRACE);      // pushMode(SQ3__FSTRING_FORMAT_SPECIFICATION_MODE) is called in PythonLexerBase
     SQ3__FSTRING_ERRORTOKEN : .             -> type(ERRORTOKEN);

mode SQ3__TSTRING_MODE;
     SQ3__TSTRING_END    : [']['][']         -> type(TSTRING_END); // popMode() is called in PythonLexerBase
     SQ3__TSTRING_MIDDLE : SQ3__ISTRING_ITEM -> type(TSTRING_MIDDLE);
     SQ3__TSTRING_LBRACE : '{'               -> type(LBRACE);      // pushMode(SQ3__TSTRING_FORMAT_SPECIFICATION_MODE) is called in PythonLexerBase
     SQ3__TSTRING_ERRORTOKEN : .             -> type(ERRORTOKEN);



mode SQ3R_FSTRING_MODE;
     SQ3R_FSTRING_END    : [']['][']         -> type(FSTRING_END); // popMode() is called in PythonLexerBase
     SQ3R_FSTRING_MIDDLE : SQ3R_ISTRING_ITEM -> type(FSTRING_MIDDLE);
     SQ3R_FSTRING_LBRACE : '{'               -> type(LBRACE);      // pushMode(SQ3R_FSTRING_FORMAT_SPECIFICATION_MODE) is called in PythonLexerBase
     SQ3R_FSTRING_ERRORTOKEN : .             -> type(ERRORTOKEN);

mode SQ3R_TSTRING_MODE;
     SQ3R_TSTRING_END    : [']['][']         -> type(TSTRING_END); // popMode() is called in PythonLexerBase
     SQ3R_TSTRING_MIDDLE : SQ3R_ISTRING_ITEM -> type(TSTRING_MIDDLE);
     SQ3R_TSTRING_LBRACE : '{'               -> type(LBRACE);      // pushMode(SQ3R_TSTRING_FORMAT_SPECIFICATION_MODE) is called in PythonLexerBase
     SQ3R_TSTRING_ERRORTOKEN : .             -> type(ERRORTOKEN);



mode DQ3__FSTRING_MODE;
     DQ3__FSTRING_END    : ["]["]["]         -> type(FSTRING_END); // popMode() is called in PythonLexerBase
     DQ3__FSTRING_MIDDLE : DQ3__ISTRING_ITEM -> type(FSTRING_MIDDLE);
     DQ3__FSTRING_LBRACE : '{'               -> type(LBRACE);      // pushMode(DQ3__FSTRING_FORMAT_SPECIFICATION_MODE) is called in PythonLexerBase
     DQ3__FSTRING_ERRORTOKEN : .             -> type(ERRORTOKEN);

mode DQ3__TSTRING_MODE;
     DQ3__TSTRING_END    : ["]["]["]         -> type(TSTRING_END); // popMode() is called in PythonLexerBase
     DQ3__TSTRING_MIDDLE : DQ3__ISTRING_ITEM -> type(TSTRING_MIDDLE);
     DQ3__TSTRING_LBRACE : '{'               -> type(LBRACE);      // pushMode(DQ3__TSTRING_FORMAT_SPECIFICATION_MODE) is called in PythonLexerBase
     DQ3__TSTRING_ERRORTOKEN : .             -> type(ERRORTOKEN);



mode DQ3R_FSTRING_MODE;
     DQ3R_FSTRING_END    : ["]["]["]         -> type(FSTRING_END); // popMode() is called in PythonLexerBase
     DQ3R_FSTRING_MIDDLE : DQ3R_ISTRING_ITEM -> type(FSTRING_MIDDLE);
     DQ3R_FSTRING_LBRACE : '{'               -> type(LBRACE);      // pushMode(DQ3R_FSTRING_FORMAT_SPECIFICATION_MODE) is called in PythonLexerBase
     DQ3R_FSTRING_ERRORTOKEN : .             -> type(ERRORTOKEN);

mode DQ3R_TSTRING_MODE;
     DQ3R_TSTRING_END    : ["]["]["]         -> type(TSTRING_END); // popMode() is called in PythonLexerBase
     DQ3R_TSTRING_MIDDLE : DQ3R_ISTRING_ITEM -> type(TSTRING_MIDDLE);
     DQ3R_TSTRING_LBRACE : '{'               -> type(LBRACE);      // pushMode(DQ3R_TSTRING_FORMAT_SPECIFICATION_MODE) is called in PythonLexerBase
     DQ3R_TSTRING_ERRORTOKEN : .             -> type(ERRORTOKEN);



// *** format specification modes for interpolated strings ***
mode SQ1__FSTRING_FORMAT_SPECIFICATION_MODE; // called from the PythonLexerBase class; only used after a format specifier colon
     SQ1__FSTRING_FORMAT_SPECIFICATION_FSTRING_MIDDLE : SQ1__ISTRING_PART+ -> type(FSTRING_MIDDLE);
     SQ1__FSTRING_FORMAT_SPECIFICATION_LBRACE         : '{'                -> type(LBRACE); // closed in DEFAULT_MODE by the PythonLexerBase class
     SQ1__FSTRING_FORMAT_SPECIFICATION_RBRACE         : '}'                -> type(RBRACE); // popMode() to SQ1__FSTRING_MODE is called in PythonLexerBase
     SQ1__FSTRING_FORMAT_SPECIFICATION_ERRORTOKEN     : .                  -> type(ERRORTOKEN);

mode SQ1__TSTRING_FORMAT_SPECIFICATION_MODE; // called from the PythonLexerBase class; only used after a format specifier colon
     SQ1__TSTRING_FORMAT_SPECIFICATION_TSTRING_MIDDLE : SQ1__ISTRING_PART+ -> type(TSTRING_MIDDLE);
     SQ1__TSTRING_FORMAT_SPECIFICATION_LBRACE         : '{'                -> type(LBRACE); // closed in DEFAULT_MODE by the PythonLexerBase class
     SQ1__TSTRING_FORMAT_SPECIFICATION_RBRACE         : '}'                -> type(RBRACE); // popMode() to SQ1__TSTRING_MODE is called in PythonLexerBase
     SQ1__TSTRING_FORMAT_SPECIFICATION_ERRORTOKEN     : .                  -> type(ERRORTOKEN);



mode SQ1R_FSTRING_FORMAT_SPECIFICATION_MODE; // called from the PythonLexerBase class; only used after a format specifier colon
     SQ1R_FSTRING_FORMAT_SPECIFICATION_FSTRING_MIDDLE : SQ1R_ISTRING_PART+ -> type(FSTRING_MIDDLE);
     SQ1R_FSTRING_FORMAT_SPECIFICATION_LBRACE         : '{'                -> type(LBRACE); // closed in DEFAULT_MODE by the PythonLexerBase class
     SQ1R_FSTRING_FORMAT_SPECIFICATION_RBRACE         : '}'                -> type(RBRACE); // popMode() to SQ1R_FSTRING_MODE is called in PythonLexerBase
     SQ1R_FSTRING_FORMAT_SPECIFICATION_ERRORTOKEN     : .                  -> type(ERRORTOKEN);

mode SQ1R_TSTRING_FORMAT_SPECIFICATION_MODE; // called from the PythonLexerBase class; only used after a format specifier colon
     SQ1R_TSTRING_FORMAT_SPECIFICATION_TSTRING_MIDDLE : SQ1R_ISTRING_PART+ -> type(TSTRING_MIDDLE);
     SQ1R_TSTRING_FORMAT_SPECIFICATION_LBRACE         : '{'                -> type(LBRACE); // closed in DEFAULT_MODE by the PythonLexerBase class
     SQ1R_TSTRING_FORMAT_SPECIFICATION_RBRACE         : '}'                -> type(RBRACE); // popMode() to SQ1R_TSTRING_MODE is called in PythonLexerBase
     SQ1R_TSTRING_FORMAT_SPECIFICATION_ERRORTOKEN     : .                  -> type(ERRORTOKEN);



mode DQ1__FSTRING_FORMAT_SPECIFICATION_MODE; // called from the PythonLexerBase class; only used after a format specifier colon
     DQ1__FSTRING_FORMAT_SPECIFICATION_FSTRING_MIDDLE : DQ1__ISTRING_PART+ -> type(FSTRING_MIDDLE);
     DQ1__FSTRING_FORMAT_SPECIFICATION_LBRACE         : '{'                -> type(LBRACE); // closed in DEFAULT_MODE by the PythonLexerBase class
     DQ1__FSTRING_FORMAT_SPECIFICATION_RBRACE         : '}'                -> type(RBRACE); // popMode() to DQ1__FSTRING_MODE is called in PythonLexerBase
     DQ1__FSTRING_FORMAT_SPECIFICATION_ERRORTOKEN     : .                  -> type(ERRORTOKEN);

mode DQ1__TSTRING_FORMAT_SPECIFICATION_MODE; // called from the PythonLexerBase class; only used after a format specifier colon
     DQ1__TSTRING_FORMAT_SPECIFICATION_TSTRING_MIDDLE : DQ1__ISTRING_PART+ -> type(TSTRING_MIDDLE);
     DQ1__TSTRING_FORMAT_SPECIFICATION_LBRACE         : '{'                -> type(LBRACE); // closed in DEFAULT_MODE by the PythonLexerBase class
     DQ1__TSTRING_FORMAT_SPECIFICATION_RBRACE         : '}'                -> type(RBRACE); // popMode() to DQ1__TSTRING_MODE is called in PythonLexerBase
     DQ1__TSTRING_FORMAT_SPECIFICATION_ERRORTOKEN     : .                  -> type(ERRORTOKEN);



mode DQ1R_FSTRING_FORMAT_SPECIFICATION_MODE; // called from the PythonLexerBase class; only used after a format specifier colon
     DQ1R_FSTRING_FORMAT_SPECIFICATION_FSTRING_MIDDLE : DQ1R_ISTRING_PART+ -> type(FSTRING_MIDDLE);
     DQ1R_FSTRING_FORMAT_SPECIFICATION_LBRACE         : '{'                -> type(LBRACE); // closed in DEFAULT_MODE by the PythonLexerBase class
     DQ1R_FSTRING_FORMAT_SPECIFICATION_RBRACE         : '}'                -> type(RBRACE); // popMode() to DQ1R_FSTRING_MODE is called in PythonLexerBase
     DQ1R_FSTRING_FORMAT_SPECIFICATION_ERRORTOKEN     : .                  -> type(ERRORTOKEN);

mode DQ1R_TSTRING_FORMAT_SPECIFICATION_MODE; // called from the PythonLexerBase class; only used after a format specifier colon
     DQ1R_TSTRING_FORMAT_SPECIFICATION_TSTRING_MIDDLE : DQ1R_ISTRING_PART+ -> type(TSTRING_MIDDLE);
     DQ1R_TSTRING_FORMAT_SPECIFICATION_LBRACE         : '{'                -> type(LBRACE); // closed in DEFAULT_MODE by the PythonLexerBase class
     DQ1R_TSTRING_FORMAT_SPECIFICATION_RBRACE         : '}'                -> type(RBRACE); // popMode() to DQ1R_TSTRING_MODE is called in PythonLexerBase
     DQ1R_TSTRING_FORMAT_SPECIFICATION_ERRORTOKEN     : .                  -> type(ERRORTOKEN);



mode SQ3__FSTRING_FORMAT_SPECIFICATION_MODE; // called from the PythonLexerBase class; only used after a format specifier colon
     SQ3__FSTRING_FORMAT_SPECIFICATION_FSTRING_MIDDLE : SQ3__ISTRING_PART+ -> type(FSTRING_MIDDLE);
     SQ3__FSTRING_FORMAT_SPECIFICATION_LBRACE         : '{'                -> type(LBRACE); // closed in DEFAULT_MODE by the PythonLexerBase class
     SQ3__FSTRING_FORMAT_SPECIFICATION_RBRACE         : '}'                -> type(RBRACE); // popMode() to SQ3__FSTRING_MODE is called in PythonLexerBase
     SQ3__FSTRING_FORMAT_SPECIFICATION_ERRORTOKEN     : .                  -> type(ERRORTOKEN);

mode SQ3__TSTRING_FORMAT_SPECIFICATION_MODE; // called from the PythonLexerBase class; only used after a format specifier colon
     SQ3__TSTRING_FORMAT_SPECIFICATION_TSTRING_MIDDLE : SQ3__ISTRING_PART+ -> type(TSTRING_MIDDLE);
     SQ3__TSTRING_FORMAT_SPECIFICATION_LBRACE         : '{'                -> type(LBRACE); // closed in DEFAULT_MODE by the PythonLexerBase class
     SQ3__TSTRING_FORMAT_SPECIFICATION_RBRACE         : '}'                -> type(RBRACE); // popMode() to SQ3__TSTRING_MODE is called in PythonLexerBase
     SQ3__TSTRING_FORMAT_SPECIFICATION_ERRORTOKEN     : .                  -> type(ERRORTOKEN);



mode SQ3R_FSTRING_FORMAT_SPECIFICATION_MODE; // called from the PythonLexerBase class; only used after a format specifier colon
     SQ3R_FSTRING_FORMAT_SPECIFICATION_FSTRING_MIDDLE : SQ3R_ISTRING_PART+ -> type(FSTRING_MIDDLE);
     SQ3R_FSTRING_FORMAT_SPECIFICATION_LBRACE         : '{'                -> type(LBRACE); // closed in DEFAULT_MODE by the PythonLexerBase class
     SQ3R_FSTRING_FORMAT_SPECIFICATION_RBRACE         : '}'                -> type(RBRACE); // popMode() to SQ3R_FSTRING_MODE is called in PythonLexerBase
     SQ3R_FSTRING_FORMAT_SPECIFICATION_ERRORTOKEN     : .                  -> type(ERRORTOKEN);

mode SQ3R_TSTRING_FORMAT_SPECIFICATION_MODE; // called from the PythonLexerBase class; only used after a format specifier colon
     SQ3R_TSTRING_FORMAT_SPECIFICATION_TSTRING_MIDDLE : SQ3R_ISTRING_PART+ -> type(TSTRING_MIDDLE);
     SQ3R_TSTRING_FORMAT_SPECIFICATION_LBRACE         : '{'                -> type(LBRACE); // closed in DEFAULT_MODE by the PythonLexerBase class
     SQ3R_TSTRING_FORMAT_SPECIFICATION_RBRACE         : '}'                -> type(RBRACE); // popMode() to SQ3R_TSTRING_MODE is called in PythonLexerBase
     SQ3R_TSTRING_FORMAT_SPECIFICATION_ERRORTOKEN     : .                  -> type(ERRORTOKEN);



mode DQ3__FSTRING_FORMAT_SPECIFICATION_MODE; // called from the PythonLexerBase class; only used after a format specifier colon
     DQ3__FSTRING_FORMAT_SPECIFICATION_FSTRING_MIDDLE : DQ3__ISTRING_PART+ -> type(FSTRING_MIDDLE);
     DQ3__FSTRING_FORMAT_SPECIFICATION_LBRACE         : '{'                -> type(LBRACE); // closed in DEFAULT_MODE by the PythonLexerBase class
     DQ3__FSTRING_FORMAT_SPECIFICATION_RBRACE         : '}'                -> type(RBRACE); // popMode() to DQ3__FSTRING_MODE is called in PythonLexerBase
     DQ3__FSTRING_FORMAT_SPECIFICATION_ERRORTOKEN     : .                  -> type(ERRORTOKEN);

mode DQ3__TSTRING_FORMAT_SPECIFICATION_MODE; // called from the PythonLexerBase class; only used after a format specifier colon
     DQ3__TSTRING_FORMAT_SPECIFICATION_TSTRING_MIDDLE : DQ3__ISTRING_PART+ -> type(TSTRING_MIDDLE);
     DQ3__TSTRING_FORMAT_SPECIFICATION_LBRACE         : '{'                -> type(LBRACE); // closed in DEFAULT_MODE by the PythonLexerBase class
     DQ3__TSTRING_FORMAT_SPECIFICATION_RBRACE         : '}'                -> type(RBRACE); // popMode() to DQ3__TSTRING_MODE is called in PythonLexerBase
     DQ3__TSTRING_FORMAT_SPECIFICATION_ERRORTOKEN     : .                  -> type(ERRORTOKEN);



mode DQ3R_FSTRING_FORMAT_SPECIFICATION_MODE; // called from the PythonLexerBase class; only used after a format specifier colon
     DQ3R_FSTRING_FORMAT_SPECIFICATION_FSTRING_MIDDLE : DQ3R_ISTRING_PART+ -> type(FSTRING_MIDDLE);
     DQ3R_FSTRING_FORMAT_SPECIFICATION_LBRACE         : '{'                -> type(LBRACE); // closed in DEFAULT_MODE by the PythonLexerBase class
     DQ3R_FSTRING_FORMAT_SPECIFICATION_RBRACE         : '}'                -> type(RBRACE); // popMode() to DQ3R_FSTRING_MODE is called in PythonLexerBase
     DQ3R_FSTRING_FORMAT_SPECIFICATION_ERRORTOKEN     : .                  -> type(ERRORTOKEN);

mode DQ3R_TSTRING_FORMAT_SPECIFICATION_MODE; // called from the PythonLexerBase class; only used after a format specifier colon
     DQ3R_TSTRING_FORMAT_SPECIFICATION_TSTRING_MIDDLE : DQ3R_ISTRING_PART+ -> type(TSTRING_MIDDLE);
     DQ3R_TSTRING_FORMAT_SPECIFICATION_LBRACE         : '{'                -> type(LBRACE); // closed in DEFAULT_MODE by the PythonLexerBase class
     DQ3R_TSTRING_FORMAT_SPECIFICATION_RBRACE         : '}'                -> type(RBRACE); // popMode() to DQ3R_TSTRING_MODE is called in PythonLexerBase
     DQ3R_TSTRING_FORMAT_SPECIFICATION_ERRORTOKEN     : .                  -> type(ERRORTOKEN);


/*
 *  fragments
 */

// https://docs.python.org/3.14/reference/lexical_analysis.html#literals
//
// https://docs.python.org/3.14/reference/lexical_analysis.html#string-and-bytes-literals
fragment STRING_LITERAL : STRING_PREFIX? (SHORT_STRING | LONG_STRING);
fragment STRING_PREFIX options { caseInsensitive=true; }  : 'r' | 'u'; // 'r' | 'u' | 'R' | 'U'

fragment SHORT_STRING
    : ['] SHORT_STRING_ITEM_FOR_SINGLE_QUOTE* [']
    | ["] SHORT_STRING_ITEM_FOR_DOUBLE_QUOTE* ["]
    ;

fragment LONG_STRING
    : ['][']['] LONG__STRING_ITEM*? ['][']['] // nongreede
    | ["]["]["] LONG__STRING_ITEM*? ["]["]["] // nongreede
    ;

// https://docs.python.org/3/faq/design.html#why-can-t-raw-strings-r-strings-end-with-a-backslash
fragment SHORT_STRING_ITEM_FOR_SINGLE_QUOTE : SHORT_STRING_CHAR_NO_SINGLE_QUOTE | STRING_ESCAPE_SEQ;
fragment SHORT_STRING_ITEM_FOR_DOUBLE_QUOTE : SHORT_STRING_CHAR_NO_DOUBLE_QUOTE | STRING_ESCAPE_SEQ;
fragment LONG__STRING_ITEM                  : LONG__STRING_CHAR                 | STRING_ESCAPE_SEQ;

fragment SHORT_STRING_CHAR_NO_SINGLE_QUOTE : ~[\\\r\n'];       // <any source character except "\" or newline or single quote>
fragment SHORT_STRING_CHAR_NO_DOUBLE_QUOTE : ~[\\\r\n"];       // <any source character except "\" or newline or double quote>
fragment LONG__STRING_CHAR                 : ~[\\];            // <any source character except "\">

// https://docs.python.org/3/reference/lexical_analysis.html#escape-sequences
fragment STRING_ESCAPE_SEQ : ESCAPE_SEQ_NEWLINE | '\\' .;       // "\" <any source character>

// https://docs.python.org/3.14/reference/lexical_analysis.html#string-and-bytes-literals
fragment BYTES_LITERAL : BYTES_PREFIX (SHORT_BYTES | LONG_BYTES);
fragment BYTES_PREFIX options { caseInsensitive=true; } : 'b' | 'br' | 'rb'; // 'b' | 'B' | 'br' | 'Br' | 'bR' | 'BR' | 'rb' | 'rB' | 'Rb' | 'RB'

fragment SHORT_BYTES
    : ['] SHORT_BYTES_ITEM_FOR_SINGLE_QUOTE* [']
    | ["] SHORT_BYTES_ITEM_FOR_DOUBLE_QUOTE* ["]
    ;

fragment LONG_BYTES
    : ['][']['] LONG_BYTES_ITEM*? ['][']['] // nongreede
    | ["]["]["] LONG_BYTES_ITEM*? ["]["]["] // nongreede
    ;

fragment SHORT_BYTES_ITEM_FOR_SINGLE_QUOTE : SHORT_SINGLE_QUOTED_BYTES_CHAR | BYTES_ESCAPE_SEQ;
fragment SHORT_BYTES_ITEM_FOR_DOUBLE_QUOTE : SHORT_DOUBLE_QUOTED_BYTES_CHAR | BYTES_ESCAPE_SEQ;

fragment LONG_BYTES_ITEM : LONG_BYTES_CHAR | BYTES_ESCAPE_SEQ;

fragment SHORT_SINGLE_QUOTED_BYTES_CHAR                        // <any ASCII character except "\" or newline or single quote>
    : [\u0000-\u0009]
    | [\u000B-\u000C]
    | [\u000E-\u0026]
    | [\u0028-\u005B]
    | [\u005D-\u007F]
    ;

fragment SHORT_DOUBLE_QUOTED_BYTES_CHAR                        // <any ASCII character except "\" or newline or double quote>
    : [\u0000-\u0009]
    | [\u000B-\u000C]
    | [\u000E-\u0021]
    | [\u0023-\u005B]
    | [\u005D-\u007F]
    ;

fragment LONG_BYTES_CHAR  : [\u0000-\u005B] | [\u005D-\u007F]; // <any ASCII character except "\">
fragment BYTES_ESCAPE_SEQ : '\\' [\u0000-\u007F];              // "\" <any ASCII character>

// https://docs.python.org/3.14/reference/lexical_analysis.html#formatted-string-literals
fragment FSTRING_PREFIX options { caseInsensitive=true; } : 'f' | 'fr' | 'rf'; // 'f' | 'F' | 'fr' | 'Fr' | 'fR' | 'FR' | 'rf' | 'rF' | 'Rf' | 'RF'
fragment TSTRING_PREFIX options { caseInsensitive=true; } : 't' | 'tr' | 'rt'; // 't' | 'T' | 'tr' | 'Tr' | 'tR' | 'TR' | 'rt' | 'rT' | 'Rt' | 'RT'
fragment STRING_QUOTES : [']
                       | ["]
                       | [']['][']
                       | ["]["]["]
                       ;

fragment SQ1__ISTRING_ITEM : (SQ1__ISTRING_PART+ TERMINATING_ISTRING_MIDDLE?)      | TERMINATING_ISTRING_MIDDLE;
fragment DQ1__ISTRING_ITEM : (DQ1__ISTRING_PART+ TERMINATING_ISTRING_MIDDLE?)      | TERMINATING_ISTRING_MIDDLE;
fragment SQ3__ISTRING_ITEM : (SQ3__ISTRING_PART+ TERMINATING_SQ3__ISTRING_MIDDLE?) | TERMINATING_SQ3__ISTRING_MIDDLE;
fragment DQ3__ISTRING_ITEM : (DQ3__ISTRING_PART+ TERMINATING_DQ3__ISTRING_MIDDLE?) | TERMINATING_DQ3__ISTRING_MIDDLE;

fragment SQ1R_ISTRING_ITEM : (SQ1R_ISTRING_PART+ TERMINATING_ISTRING_MIDDLE_RAW?)  | TERMINATING_ISTRING_MIDDLE_RAW;
fragment DQ1R_ISTRING_ITEM : (DQ1R_ISTRING_PART+ TERMINATING_ISTRING_MIDDLE_RAW?)  | TERMINATING_ISTRING_MIDDLE_RAW;
fragment SQ3R_ISTRING_ITEM : (SQ3R_ISTRING_PART+ TERMINATING_SQ3R_ISTRING_MIDDLE?) | TERMINATING_SQ3R_ISTRING_MIDDLE;
fragment DQ3R_ISTRING_ITEM : (DQ3R_ISTRING_PART+ TERMINATING_DQ3R_ISTRING_MIDDLE?) | TERMINATING_DQ3R_ISTRING_MIDDLE;



fragment SQ1__ISTRING_PART :                     SQ1_ISTRING_CHAR | ISTRING_ESCAPE_SEQ;
fragment DQ1__ISTRING_PART :                     DQ1_ISTRING_CHAR | ISTRING_ESCAPE_SEQ;
fragment SQ3__ISTRING_PART : ONE_OR_TWO_SQUOTE? (SQ3_ISTRING_CHAR | ISTRING_ESCAPE_SEQ);
fragment DQ3__ISTRING_PART : ONE_OR_TWO_DQUOTE? (DQ3_ISTRING_CHAR | ISTRING_ESCAPE_SEQ);

fragment SQ1R_ISTRING_PART :                     SQ1_ISTRING_CHAR | ISTRING_ESCAPE_SEQ_RAW;
fragment DQ1R_ISTRING_PART :                     DQ1_ISTRING_CHAR | ISTRING_ESCAPE_SEQ_RAW;
fragment SQ3R_ISTRING_PART : ONE_OR_TWO_SQUOTE? (SQ3_ISTRING_CHAR | ISTRING_ESCAPE_SEQ_RAW);
fragment DQ3R_ISTRING_PART : ONE_OR_TWO_DQUOTE? (DQ3_ISTRING_CHAR | ISTRING_ESCAPE_SEQ_RAW);



fragment SQ1_ISTRING_CHAR : ~[\\{}'\r\n];                      // <any source character except "\" or open/close brace or single quote or newline>
fragment DQ1_ISTRING_CHAR : ~[\\{}"\r\n];                      // <any source character except "\" or open/close brace or double quote or newline>
fragment SQ3_ISTRING_CHAR : ~[\\{}'];                          // <any source character except "\" or open/close brace or single quote>
fragment DQ3_ISTRING_CHAR : ~[\\{}"];                          // <any source character except "\" or open/close brace or double quote>



fragment TERMINATING_SQ3__ISTRING_MIDDLE : ONE_OR_TWO_SQUOTE_LBRACE | ONE_OR_TWO_SQUOTE? TERMINATING_ISTRING_MIDDLE;
fragment TERMINATING_DQ3__ISTRING_MIDDLE : ONE_OR_TWO_DQUOTE_LBRACE | ONE_OR_TWO_DQUOTE? TERMINATING_ISTRING_MIDDLE;
fragment TERMINATING_SQ3R_ISTRING_MIDDLE : ONE_OR_TWO_SQUOTE_LBRACE | ONE_OR_TWO_SQUOTE? TERMINATING_ISTRING_MIDDLE_RAW;
fragment TERMINATING_DQ3R_ISTRING_MIDDLE : ONE_OR_TWO_DQUOTE_LBRACE | ONE_OR_TWO_DQUOTE? TERMINATING_ISTRING_MIDDLE_RAW;
fragment ONE_OR_TWO_SQUOTE_LBRACE : ONE_OR_TWO_SQUOTE '{';
fragment ONE_OR_TWO_DQUOTE_LBRACE : ONE_OR_TWO_DQUOTE '{';

fragment TERMINATING_ISTRING_MIDDLE : TERMINATING_ISTRING_MIDDLE_RAW | ESCAPE_SEQ_NAMED_CHAR;
fragment TERMINATING_ISTRING_MIDDLE_RAW : '\\'? DOUBLE_BRACE | '\\{' ; // https://docs.python.org/3/faq/design.html#why-can-t-raw-strings-r-strings-end-with-a-backslash

fragment ISTRING_ESCAPE_SEQ     : ESCAPE_SEQ_NEWLINE | '\\' ~[{}N]; // f"\\}" causes a lexer error
fragment ISTRING_ESCAPE_SEQ_RAW : ESCAPE_SEQ_NEWLINE | '\\' ~[{}];  // fr"\}" causes a lexer error

fragment ONE_OR_TWO_SQUOTE : ['][']?;
fragment ONE_OR_TWO_DQUOTE : ["]["]?;
fragment DOUBLE_BRACE : '{{' | '}}'; // PythonLexerBase replaces double brace with single brace

fragment ESCAPE_SEQ_NAMED_CHAR : '\\N{' .*? '}'; // an escape sequence for a Unicode character specified by name
fragment ESCAPE_SEQ_NEWLINE    : BACKSLASH_NEWLINE; // this escape sequence acts as a line continuation in string literals
                                                    // the backslash and newline are ignored by the Python interpreter
fragment BACKSLASH_NEWLINE : '\\' NEWLINE;

// https://docs.python.org/3.14/reference/lexical_analysis.html#integer-literals
fragment INTEGER        : DEC_INTEGER | BIN_INTEGER | OCT_INTEGER | HEX_INTEGER;
fragment DEC_INTEGER    : NON_ZERO_DIGIT ('_'? DIGIT)* | '0'+ ('_'? '0')*;
fragment BIN_INTEGER    : '0' ('b' | 'B') ('_'? BIN_DIGIT)+;
fragment OCT_INTEGER    : '0' ('o' | 'O') ('_'? OCT_DIGIT)+;
fragment HEX_INTEGER    : '0' ('x' | 'X') ('_'? HEX_DIGIT)+;
fragment NON_ZERO_DIGIT : [1-9];
fragment DIGIT          : [0-9];
fragment BIN_DIGIT      : '0' | '1';
fragment OCT_DIGIT      : [0-7];
fragment HEX_DIGIT      : DIGIT | [a-f] | [A-F];

// https://docs.python.org/3.14/reference/lexical_analysis.html#floating-point-literals
fragment FLOAT_NUMBER   : POINT_FLOAT | EXPONENT_FLOAT;
fragment POINT_FLOAT    : DIGIT_PART? FRACTION | DIGIT_PART '.';
fragment EXPONENT_FLOAT : (DIGIT_PART | POINT_FLOAT) EXPONENT;
fragment DIGIT_PART     : DIGIT ('_'? DIGIT)*;
fragment FRACTION       : '.' DIGIT_PART;
fragment EXPONENT       : ('e' | 'E') ('+' | '-')? DIGIT_PART;

// https://docs.python.org/3.14/reference/lexical_analysis.html#imaginary-literals
fragment IMAG_NUMBER : (FLOAT_NUMBER | DIGIT_PART) ('j' | 'J');

// https://github.com/RobEin/ANTLR4-parser-for-Python-3.14/tree/main/utils/valid_chars_in_py_identifiers
fragment ID_CONTINUE // for Python 3.14.2
    : ID_START
    | '\u{0030}' .. '\u{0039}'
    | '\u{00B7}'
    | '\u{0300}' .. '\u{036F}'
    | '\u{0387}'
    | '\u{0483}' .. '\u{0487}'
    | '\u{0591}' .. '\u{05BD}'
    | '\u{05BF}'
    | '\u{05C1}' .. '\u{05C2}'
    | '\u{05C4}' .. '\u{05C5}'
    | '\u{05C7}'
    | '\u{0610}' .. '\u{061A}'
    | '\u{064B}' .. '\u{0669}'
    | '\u{0670}'
    | '\u{06D6}' .. '\u{06DC}'
    | '\u{06DF}' .. '\u{06E4}'
    | '\u{06E7}' .. '\u{06E8}'
    | '\u{06EA}' .. '\u{06ED}'
    | '\u{06F0}' .. '\u{06F9}'
    | '\u{0711}'
    | '\u{0730}' .. '\u{074A}'
    | '\u{07A6}' .. '\u{07B0}'
    | '\u{07C0}' .. '\u{07C9}'
    | '\u{07EB}' .. '\u{07F3}'
    | '\u{07FD}'
    | '\u{0816}' .. '\u{0819}'
    | '\u{081B}' .. '\u{0823}'
    | '\u{0825}' .. '\u{0827}'
    | '\u{0829}' .. '\u{082D}'
    | '\u{0859}' .. '\u{085B}'
    | '\u{0897}' .. '\u{089F}'
    | '\u{08CA}' .. '\u{08E1}'
    | '\u{08E3}' .. '\u{0903}'
    | '\u{093A}' .. '\u{093C}'
    | '\u{093E}' .. '\u{094F}'
    | '\u{0951}' .. '\u{0957}'
    | '\u{0962}' .. '\u{0963}'
    | '\u{0966}' .. '\u{096F}'
    | '\u{0981}' .. '\u{0983}'
    | '\u{09BC}'
    | '\u{09BE}' .. '\u{09C4}'
    | '\u{09C7}' .. '\u{09C8}'
    | '\u{09CB}' .. '\u{09CD}'
    | '\u{09D7}'
    | '\u{09E2}' .. '\u{09E3}'
    | '\u{09E6}' .. '\u{09EF}'
    | '\u{09FE}'
    | '\u{0A01}' .. '\u{0A03}'
    | '\u{0A3C}'
    | '\u{0A3E}' .. '\u{0A42}'
    | '\u{0A47}' .. '\u{0A48}'
    | '\u{0A4B}' .. '\u{0A4D}'
    | '\u{0A51}'
    | '\u{0A66}' .. '\u{0A71}'
    | '\u{0A75}'
    | '\u{0A81}' .. '\u{0A83}'
    | '\u{0ABC}'
    | '\u{0ABE}' .. '\u{0AC5}'
    | '\u{0AC7}' .. '\u{0AC9}'
    | '\u{0ACB}' .. '\u{0ACD}'
    | '\u{0AE2}' .. '\u{0AE3}'
    | '\u{0AE6}' .. '\u{0AEF}'
    | '\u{0AFA}' .. '\u{0AFF}'
    | '\u{0B01}' .. '\u{0B03}'
    | '\u{0B3C}'
    | '\u{0B3E}' .. '\u{0B44}'
    | '\u{0B47}' .. '\u{0B48}'
    | '\u{0B4B}' .. '\u{0B4D}'
    | '\u{0B55}' .. '\u{0B57}'
    | '\u{0B62}' .. '\u{0B63}'
    | '\u{0B66}' .. '\u{0B6F}'
    | '\u{0B82}'
    | '\u{0BBE}' .. '\u{0BC2}'
    | '\u{0BC6}' .. '\u{0BC8}'
    | '\u{0BCA}' .. '\u{0BCD}'
    | '\u{0BD7}'
    | '\u{0BE6}' .. '\u{0BEF}'
    | '\u{0C00}' .. '\u{0C04}'
    | '\u{0C3C}'
    | '\u{0C3E}' .. '\u{0C44}'
    | '\u{0C46}' .. '\u{0C48}'
    | '\u{0C4A}' .. '\u{0C4D}'
    | '\u{0C55}' .. '\u{0C56}'
    | '\u{0C62}' .. '\u{0C63}'
    | '\u{0C66}' .. '\u{0C6F}'
    | '\u{0C81}' .. '\u{0C83}'
    | '\u{0CBC}'
    | '\u{0CBE}' .. '\u{0CC4}'
    | '\u{0CC6}' .. '\u{0CC8}'
    | '\u{0CCA}' .. '\u{0CCD}'
    | '\u{0CD5}' .. '\u{0CD6}'
    | '\u{0CE2}' .. '\u{0CE3}'
    | '\u{0CE6}' .. '\u{0CEF}'
    | '\u{0CF3}'
    | '\u{0D00}' .. '\u{0D03}'
    | '\u{0D3B}' .. '\u{0D3C}'
    | '\u{0D3E}' .. '\u{0D44}'
    | '\u{0D46}' .. '\u{0D48}'
    | '\u{0D4A}' .. '\u{0D4D}'
    | '\u{0D57}'
    | '\u{0D62}' .. '\u{0D63}'
    | '\u{0D66}' .. '\u{0D6F}'
    | '\u{0D81}' .. '\u{0D83}'
    | '\u{0DCA}'
    | '\u{0DCF}' .. '\u{0DD4}'
    | '\u{0DD6}'
    | '\u{0DD8}' .. '\u{0DDF}'
    | '\u{0DE6}' .. '\u{0DEF}'
    | '\u{0DF2}' .. '\u{0DF3}'
    | '\u{0E31}'
    | '\u{0E33}' .. '\u{0E3A}'
    | '\u{0E47}' .. '\u{0E4E}'
    | '\u{0E50}' .. '\u{0E59}'
    | '\u{0EB1}'
    | '\u{0EB3}' .. '\u{0EBC}'
    | '\u{0EC8}' .. '\u{0ECE}'
    | '\u{0ED0}' .. '\u{0ED9}'
    | '\u{0F18}' .. '\u{0F19}'
    | '\u{0F20}' .. '\u{0F29}'
    | '\u{0F35}'
    | '\u{0F37}'
    | '\u{0F39}'
    | '\u{0F3E}' .. '\u{0F3F}'
    | '\u{0F71}' .. '\u{0F84}'
    | '\u{0F86}' .. '\u{0F87}'
    | '\u{0F8D}' .. '\u{0F97}'
    | '\u{0F99}' .. '\u{0FBC}'
    | '\u{0FC6}'
    | '\u{102B}' .. '\u{103E}'
    | '\u{1040}' .. '\u{1049}'
    | '\u{1056}' .. '\u{1059}'
    | '\u{105E}' .. '\u{1060}'
    | '\u{1062}' .. '\u{1064}'
    | '\u{1067}' .. '\u{106D}'
    | '\u{1071}' .. '\u{1074}'
    | '\u{1082}' .. '\u{108D}'
    | '\u{108F}' .. '\u{109D}'
    | '\u{135D}' .. '\u{135F}'
    | '\u{1369}' .. '\u{1371}'
    | '\u{1712}' .. '\u{1715}'
    | '\u{1732}' .. '\u{1734}'
    | '\u{1752}' .. '\u{1753}'
    | '\u{1772}' .. '\u{1773}'
    | '\u{17B4}' .. '\u{17D3}'
    | '\u{17DD}'
    | '\u{17E0}' .. '\u{17E9}'
    | '\u{180B}' .. '\u{180D}'
    | '\u{180F}' .. '\u{1819}'
    | '\u{18A9}'
    | '\u{1920}' .. '\u{192B}'
    | '\u{1930}' .. '\u{193B}'
    | '\u{1946}' .. '\u{194F}'
    | '\u{19D0}' .. '\u{19DA}'
    | '\u{1A17}' .. '\u{1A1B}'
    | '\u{1A55}' .. '\u{1A5E}'
    | '\u{1A60}' .. '\u{1A7C}'
    | '\u{1A7F}' .. '\u{1A89}'
    | '\u{1A90}' .. '\u{1A99}'
    | '\u{1AB0}' .. '\u{1ABD}'
    | '\u{1ABF}' .. '\u{1ACE}'
    | '\u{1B00}' .. '\u{1B04}'
    | '\u{1B34}' .. '\u{1B44}'
    | '\u{1B50}' .. '\u{1B59}'
    | '\u{1B6B}' .. '\u{1B73}'
    | '\u{1B80}' .. '\u{1B82}'
    | '\u{1BA1}' .. '\u{1BAD}'
    | '\u{1BB0}' .. '\u{1BB9}'
    | '\u{1BE6}' .. '\u{1BF3}'
    | '\u{1C24}' .. '\u{1C37}'
    | '\u{1C40}' .. '\u{1C49}'
    | '\u{1C50}' .. '\u{1C59}'
    | '\u{1CD0}' .. '\u{1CD2}'
    | '\u{1CD4}' .. '\u{1CE8}'
    | '\u{1CED}'
    | '\u{1CF4}'
    | '\u{1CF7}' .. '\u{1CF9}'
    | '\u{1DC0}' .. '\u{1DFF}'
    | '\u{200C}' .. '\u{200D}'
    | '\u{203F}' .. '\u{2040}'
    | '\u{2054}'
    | '\u{20D0}' .. '\u{20DC}'
    | '\u{20E1}'
    | '\u{20E5}' .. '\u{20F0}'
    | '\u{2CEF}' .. '\u{2CF1}'
    | '\u{2D7F}'
    | '\u{2DE0}' .. '\u{2DFF}'
    | '\u{302A}' .. '\u{302F}'
    | '\u{3099}' .. '\u{309A}'
    | '\u{30FB}'
    | '\u{A620}' .. '\u{A629}'
    | '\u{A66F}'
    | '\u{A674}' .. '\u{A67D}'
    | '\u{A69E}' .. '\u{A69F}'
    | '\u{A6F0}' .. '\u{A6F1}'
    | '\u{A802}'
    | '\u{A806}'
    | '\u{A80B}'
    | '\u{A823}' .. '\u{A827}'
    | '\u{A82C}'
    | '\u{A880}' .. '\u{A881}'
    | '\u{A8B4}' .. '\u{A8C5}'
    | '\u{A8D0}' .. '\u{A8D9}'
    | '\u{A8E0}' .. '\u{A8F1}'
    | '\u{A8FF}' .. '\u{A909}'
    | '\u{A926}' .. '\u{A92D}'
    | '\u{A947}' .. '\u{A953}'
    | '\u{A980}' .. '\u{A983}'
    | '\u{A9B3}' .. '\u{A9C0}'
    | '\u{A9D0}' .. '\u{A9D9}'
    | '\u{A9E5}'
    | '\u{A9F0}' .. '\u{A9F9}'
    | '\u{AA29}' .. '\u{AA36}'
    | '\u{AA43}'
    | '\u{AA4C}' .. '\u{AA4D}'
    | '\u{AA50}' .. '\u{AA59}'
    | '\u{AA7B}' .. '\u{AA7D}'
    | '\u{AAB0}'
    | '\u{AAB2}' .. '\u{AAB4}'
    | '\u{AAB7}' .. '\u{AAB8}'
    | '\u{AABE}' .. '\u{AABF}'
    | '\u{AAC1}'
    | '\u{AAEB}' .. '\u{AAEF}'
    | '\u{AAF5}' .. '\u{AAF6}'
    | '\u{ABE3}' .. '\u{ABEA}'
    | '\u{ABEC}' .. '\u{ABED}'
    | '\u{ABF0}' .. '\u{ABF9}'
    | '\u{FB1E}'
    | '\u{FE00}' .. '\u{FE0F}'
    | '\u{FE20}' .. '\u{FE2F}'
    | '\u{FE33}' .. '\u{FE34}'
    | '\u{FE4D}' .. '\u{FE4F}'
    | '\u{FF10}' .. '\u{FF19}'
    | '\u{FF3F}'
    | '\u{FF65}'
    | '\u{FF9E}' .. '\u{FF9F}'
    | '\u{101FD}'
    | '\u{102E0}'
    | '\u{10376}' .. '\u{1037A}'
    | '\u{104A0}' .. '\u{104A9}'
    | '\u{10A01}' .. '\u{10A03}'
    | '\u{10A05}' .. '\u{10A06}'
    | '\u{10A0C}' .. '\u{10A0F}'
    | '\u{10A38}' .. '\u{10A3A}'
    | '\u{10A3F}'
    | '\u{10AE5}' .. '\u{10AE6}'
    | '\u{10D24}' .. '\u{10D27}'
    | '\u{10D30}' .. '\u{10D39}'
    | '\u{10D40}' .. '\u{10D49}'
    | '\u{10D69}' .. '\u{10D6D}'
    | '\u{10EAB}' .. '\u{10EAC}'
    | '\u{10EFC}' .. '\u{10EFF}'
    | '\u{10F46}' .. '\u{10F50}'
    | '\u{10F82}' .. '\u{10F85}'
    | '\u{11000}' .. '\u{11002}'
    | '\u{11038}' .. '\u{11046}'
    | '\u{11066}' .. '\u{11070}'
    | '\u{11073}' .. '\u{11074}'
    | '\u{1107F}' .. '\u{11082}'
    | '\u{110B0}' .. '\u{110BA}'
    | '\u{110C2}'
    | '\u{110F0}' .. '\u{110F9}'
    | '\u{11100}' .. '\u{11102}'
    | '\u{11127}' .. '\u{11134}'
    | '\u{11136}' .. '\u{1113F}'
    | '\u{11145}' .. '\u{11146}'
    | '\u{11173}'
    | '\u{11180}' .. '\u{11182}'
    | '\u{111B3}' .. '\u{111C0}'
    | '\u{111C9}' .. '\u{111CC}'
    | '\u{111CE}' .. '\u{111D9}'
    | '\u{1122C}' .. '\u{11237}'
    | '\u{1123E}'
    | '\u{11241}'
    | '\u{112DF}' .. '\u{112EA}'
    | '\u{112F0}' .. '\u{112F9}'
    | '\u{11300}' .. '\u{11303}'
    | '\u{1133B}' .. '\u{1133C}'
    | '\u{1133E}' .. '\u{11344}'
    | '\u{11347}' .. '\u{11348}'
    | '\u{1134B}' .. '\u{1134D}'
    | '\u{11357}'
    | '\u{11362}' .. '\u{11363}'
    | '\u{11366}' .. '\u{1136C}'
    | '\u{11370}' .. '\u{11374}'
    | '\u{113B8}' .. '\u{113C0}'
    | '\u{113C2}'
    | '\u{113C5}'
    | '\u{113C7}' .. '\u{113CA}'
    | '\u{113CC}' .. '\u{113D0}'
    | '\u{113D2}'
    | '\u{113E1}' .. '\u{113E2}'
    | '\u{11435}' .. '\u{11446}'
    | '\u{11450}' .. '\u{11459}'
    | '\u{1145E}'
    | '\u{114B0}' .. '\u{114C3}'
    | '\u{114D0}' .. '\u{114D9}'
    | '\u{115AF}' .. '\u{115B5}'
    | '\u{115B8}' .. '\u{115C0}'
    | '\u{115DC}' .. '\u{115DD}'
    | '\u{11630}' .. '\u{11640}'
    | '\u{11650}' .. '\u{11659}'
    | '\u{116AB}' .. '\u{116B7}'
    | '\u{116C0}' .. '\u{116C9}'
    | '\u{116D0}' .. '\u{116E3}'
    | '\u{1171D}' .. '\u{1172B}'
    | '\u{11730}' .. '\u{11739}'
    | '\u{1182C}' .. '\u{1183A}'
    | '\u{118E0}' .. '\u{118E9}'
    | '\u{11930}' .. '\u{11935}'
    | '\u{11937}' .. '\u{11938}'
    | '\u{1193B}' .. '\u{1193E}'
    | '\u{11940}'
    | '\u{11942}' .. '\u{11943}'
    | '\u{11950}' .. '\u{11959}'
    | '\u{119D1}' .. '\u{119D7}'
    | '\u{119DA}' .. '\u{119E0}'
    | '\u{119E4}'
    | '\u{11A01}' .. '\u{11A0A}'
    | '\u{11A33}' .. '\u{11A39}'
    | '\u{11A3B}' .. '\u{11A3E}'
    | '\u{11A47}'
    | '\u{11A51}' .. '\u{11A5B}'
    | '\u{11A8A}' .. '\u{11A99}'
    | '\u{11BF0}' .. '\u{11BF9}'
    | '\u{11C2F}' .. '\u{11C36}'
    | '\u{11C38}' .. '\u{11C3F}'
    | '\u{11C50}' .. '\u{11C59}'
    | '\u{11C92}' .. '\u{11CA7}'
    | '\u{11CA9}' .. '\u{11CB6}'
    | '\u{11D31}' .. '\u{11D36}'
    | '\u{11D3A}'
    | '\u{11D3C}' .. '\u{11D3D}'
    | '\u{11D3F}' .. '\u{11D45}'
    | '\u{11D47}'
    | '\u{11D50}' .. '\u{11D59}'
    | '\u{11D8A}' .. '\u{11D8E}'
    | '\u{11D90}' .. '\u{11D91}'
    | '\u{11D93}' .. '\u{11D97}'
    | '\u{11DA0}' .. '\u{11DA9}'
    | '\u{11EF3}' .. '\u{11EF6}'
    | '\u{11F00}' .. '\u{11F01}'
    | '\u{11F03}'
    | '\u{11F34}' .. '\u{11F3A}'
    | '\u{11F3E}' .. '\u{11F42}'
    | '\u{11F50}' .. '\u{11F5A}'
    | '\u{13440}'
    | '\u{13447}' .. '\u{13455}'
    | '\u{1611E}' .. '\u{16139}'
    | '\u{16A60}' .. '\u{16A69}'
    | '\u{16AC0}' .. '\u{16AC9}'
    | '\u{16AF0}' .. '\u{16AF4}'
    | '\u{16B30}' .. '\u{16B36}'
    | '\u{16B50}' .. '\u{16B59}'
    | '\u{16D70}' .. '\u{16D79}'
    | '\u{16F4F}'
    | '\u{16F51}' .. '\u{16F87}'
    | '\u{16F8F}' .. '\u{16F92}'
    | '\u{16FE4}'
    | '\u{16FF0}' .. '\u{16FF1}'
    | '\u{1BC9D}' .. '\u{1BC9E}'
    | '\u{1CCF0}' .. '\u{1CCF9}'
    | '\u{1CF00}' .. '\u{1CF2D}'
    | '\u{1CF30}' .. '\u{1CF46}'
    | '\u{1D165}' .. '\u{1D169}'
    | '\u{1D16D}' .. '\u{1D172}'
    | '\u{1D17B}' .. '\u{1D182}'
    | '\u{1D185}' .. '\u{1D18B}'
    | '\u{1D1AA}' .. '\u{1D1AD}'
    | '\u{1D242}' .. '\u{1D244}'
    | '\u{1D7CE}' .. '\u{1D7FF}'
    | '\u{1DA00}' .. '\u{1DA36}'
    | '\u{1DA3B}' .. '\u{1DA6C}'
    | '\u{1DA75}'
    | '\u{1DA84}'
    | '\u{1DA9B}' .. '\u{1DA9F}'
    | '\u{1DAA1}' .. '\u{1DAAF}'
    | '\u{1E000}' .. '\u{1E006}'
    | '\u{1E008}' .. '\u{1E018}'
    | '\u{1E01B}' .. '\u{1E021}'
    | '\u{1E023}' .. '\u{1E024}'
    | '\u{1E026}' .. '\u{1E02A}'
    | '\u{1E08F}'
    | '\u{1E130}' .. '\u{1E136}'
    | '\u{1E140}' .. '\u{1E149}'
    | '\u{1E2AE}'
    | '\u{1E2EC}' .. '\u{1E2F9}'
    | '\u{1E4EC}' .. '\u{1E4F9}'
    | '\u{1E5EE}' .. '\u{1E5EF}'
    | '\u{1E5F1}' .. '\u{1E5FA}'
    | '\u{1E8D0}' .. '\u{1E8D6}'
    | '\u{1E944}' .. '\u{1E94A}'
    | '\u{1E950}' .. '\u{1E959}'
    | '\u{1FBF0}' .. '\u{1FBF9}'
    | '\u{E0100}' .. '\u{E01EF}'
    ;

fragment ID_START // for Python 3.14.2
    : '\u{0041}' .. '\u{005A}'
    | '\u{005F}'
    | '\u{0061}' .. '\u{007A}'
    | '\u{00AA}'
    | '\u{00B5}'
    | '\u{00BA}'
    | '\u{00C0}' .. '\u{00D6}'
    | '\u{00D8}' .. '\u{00F6}'
    | '\u{00F8}' .. '\u{02C1}'
    | '\u{02C6}' .. '\u{02D1}'
    | '\u{02E0}' .. '\u{02E4}'
    | '\u{02EC}'
    | '\u{02EE}'
    | '\u{0370}' .. '\u{0374}'
    | '\u{0376}' .. '\u{0377}'
    | '\u{037B}' .. '\u{037D}'
    | '\u{037F}'
    | '\u{0386}'
    | '\u{0388}' .. '\u{038A}'
    | '\u{038C}'
    | '\u{038E}' .. '\u{03A1}'
    | '\u{03A3}' .. '\u{03F5}'
    | '\u{03F7}' .. '\u{0481}'
    | '\u{048A}' .. '\u{052F}'
    | '\u{0531}' .. '\u{0556}'
    | '\u{0559}'
    | '\u{0560}' .. '\u{0588}'
    | '\u{05D0}' .. '\u{05EA}'
    | '\u{05EF}' .. '\u{05F2}'
    | '\u{0620}' .. '\u{064A}'
    | '\u{066E}' .. '\u{066F}'
    | '\u{0671}' .. '\u{06D3}'
    | '\u{06D5}'
    | '\u{06E5}' .. '\u{06E6}'
    | '\u{06EE}' .. '\u{06EF}'
    | '\u{06FA}' .. '\u{06FC}'
    | '\u{06FF}'
    | '\u{0710}'
    | '\u{0712}' .. '\u{072F}'
    | '\u{074D}' .. '\u{07A5}'
    | '\u{07B1}'
    | '\u{07CA}' .. '\u{07EA}'
    | '\u{07F4}' .. '\u{07F5}'
    | '\u{07FA}'
    | '\u{0800}' .. '\u{0815}'
    | '\u{081A}'
    | '\u{0824}'
    | '\u{0828}'
    | '\u{0840}' .. '\u{0858}'
    | '\u{0860}' .. '\u{086A}'
    | '\u{0870}' .. '\u{0887}'
    | '\u{0889}' .. '\u{088E}'
    | '\u{08A0}' .. '\u{08C9}'
    | '\u{0904}' .. '\u{0939}'
    | '\u{093D}'
    | '\u{0950}'
    | '\u{0958}' .. '\u{0961}'
    | '\u{0971}' .. '\u{0980}'
    | '\u{0985}' .. '\u{098C}'
    | '\u{098F}' .. '\u{0990}'
    | '\u{0993}' .. '\u{09A8}'
    | '\u{09AA}' .. '\u{09B0}'
    | '\u{09B2}'
    | '\u{09B6}' .. '\u{09B9}'
    | '\u{09BD}'
    | '\u{09CE}'
    | '\u{09DC}' .. '\u{09DD}'
    | '\u{09DF}' .. '\u{09E1}'
    | '\u{09F0}' .. '\u{09F1}'
    | '\u{09FC}'
    | '\u{0A05}' .. '\u{0A0A}'
    | '\u{0A0F}' .. '\u{0A10}'
    | '\u{0A13}' .. '\u{0A28}'
    | '\u{0A2A}' .. '\u{0A30}'
    | '\u{0A32}' .. '\u{0A33}'
    | '\u{0A35}' .. '\u{0A36}'
    | '\u{0A38}' .. '\u{0A39}'
    | '\u{0A59}' .. '\u{0A5C}'
    | '\u{0A5E}'
    | '\u{0A72}' .. '\u{0A74}'
    | '\u{0A85}' .. '\u{0A8D}'
    | '\u{0A8F}' .. '\u{0A91}'
    | '\u{0A93}' .. '\u{0AA8}'
    | '\u{0AAA}' .. '\u{0AB0}'
    | '\u{0AB2}' .. '\u{0AB3}'
    | '\u{0AB5}' .. '\u{0AB9}'
    | '\u{0ABD}'
    | '\u{0AD0}'
    | '\u{0AE0}' .. '\u{0AE1}'
    | '\u{0AF9}'
    | '\u{0B05}' .. '\u{0B0C}'
    | '\u{0B0F}' .. '\u{0B10}'
    | '\u{0B13}' .. '\u{0B28}'
    | '\u{0B2A}' .. '\u{0B30}'
    | '\u{0B32}' .. '\u{0B33}'
    | '\u{0B35}' .. '\u{0B39}'
    | '\u{0B3D}'
    | '\u{0B5C}' .. '\u{0B5D}'
    | '\u{0B5F}' .. '\u{0B61}'
    | '\u{0B71}'
    | '\u{0B83}'
    | '\u{0B85}' .. '\u{0B8A}'
    | '\u{0B8E}' .. '\u{0B90}'
    | '\u{0B92}' .. '\u{0B95}'
    | '\u{0B99}' .. '\u{0B9A}'
    | '\u{0B9C}'
    | '\u{0B9E}' .. '\u{0B9F}'
    | '\u{0BA3}' .. '\u{0BA4}'
    | '\u{0BA8}' .. '\u{0BAA}'
    | '\u{0BAE}' .. '\u{0BB9}'
    | '\u{0BD0}'
    | '\u{0C05}' .. '\u{0C0C}'
    | '\u{0C0E}' .. '\u{0C10}'
    | '\u{0C12}' .. '\u{0C28}'
    | '\u{0C2A}' .. '\u{0C39}'
    | '\u{0C3D}'
    | '\u{0C58}' .. '\u{0C5A}'
    | '\u{0C5D}'
    | '\u{0C60}' .. '\u{0C61}'
    | '\u{0C80}'
    | '\u{0C85}' .. '\u{0C8C}'
    | '\u{0C8E}' .. '\u{0C90}'
    | '\u{0C92}' .. '\u{0CA8}'
    | '\u{0CAA}' .. '\u{0CB3}'
    | '\u{0CB5}' .. '\u{0CB9}'
    | '\u{0CBD}'
    | '\u{0CDD}' .. '\u{0CDE}'
    | '\u{0CE0}' .. '\u{0CE1}'
    | '\u{0CF1}' .. '\u{0CF2}'
    | '\u{0D04}' .. '\u{0D0C}'
    | '\u{0D0E}' .. '\u{0D10}'
    | '\u{0D12}' .. '\u{0D3A}'
    | '\u{0D3D}'
    | '\u{0D4E}'
    | '\u{0D54}' .. '\u{0D56}'
    | '\u{0D5F}' .. '\u{0D61}'
    | '\u{0D7A}' .. '\u{0D7F}'
    | '\u{0D85}' .. '\u{0D96}'
    | '\u{0D9A}' .. '\u{0DB1}'
    | '\u{0DB3}' .. '\u{0DBB}'
    | '\u{0DBD}'
    | '\u{0DC0}' .. '\u{0DC6}'
    | '\u{0E01}' .. '\u{0E30}'
    | '\u{0E32}'
    | '\u{0E40}' .. '\u{0E46}'
    | '\u{0E81}' .. '\u{0E82}'
    | '\u{0E84}'
    | '\u{0E86}' .. '\u{0E8A}'
    | '\u{0E8C}' .. '\u{0EA3}'
    | '\u{0EA5}'
    | '\u{0EA7}' .. '\u{0EB0}'
    | '\u{0EB2}'
    | '\u{0EBD}'
    | '\u{0EC0}' .. '\u{0EC4}'
    | '\u{0EC6}'
    | '\u{0EDC}' .. '\u{0EDF}'
    | '\u{0F00}'
    | '\u{0F40}' .. '\u{0F47}'
    | '\u{0F49}' .. '\u{0F6C}'
    | '\u{0F88}' .. '\u{0F8C}'
    | '\u{1000}' .. '\u{102A}'
    | '\u{103F}'
    | '\u{1050}' .. '\u{1055}'
    | '\u{105A}' .. '\u{105D}'
    | '\u{1061}'
    | '\u{1065}' .. '\u{1066}'
    | '\u{106E}' .. '\u{1070}'
    | '\u{1075}' .. '\u{1081}'
    | '\u{108E}'
    | '\u{10A0}' .. '\u{10C5}'
    | '\u{10C7}'
    | '\u{10CD}'
    | '\u{10D0}' .. '\u{10FA}'
    | '\u{10FC}' .. '\u{1248}'
    | '\u{124A}' .. '\u{124D}'
    | '\u{1250}' .. '\u{1256}'
    | '\u{1258}'
    | '\u{125A}' .. '\u{125D}'
    | '\u{1260}' .. '\u{1288}'
    | '\u{128A}' .. '\u{128D}'
    | '\u{1290}' .. '\u{12B0}'
    | '\u{12B2}' .. '\u{12B5}'
    | '\u{12B8}' .. '\u{12BE}'
    | '\u{12C0}'
    | '\u{12C2}' .. '\u{12C5}'
    | '\u{12C8}' .. '\u{12D6}'
    | '\u{12D8}' .. '\u{1310}'
    | '\u{1312}' .. '\u{1315}'
    | '\u{1318}' .. '\u{135A}'
    | '\u{1380}' .. '\u{138F}'
    | '\u{13A0}' .. '\u{13F5}'
    | '\u{13F8}' .. '\u{13FD}'
    | '\u{1401}' .. '\u{166C}'
    | '\u{166F}' .. '\u{167F}'
    | '\u{1681}' .. '\u{169A}'
    | '\u{16A0}' .. '\u{16EA}'
    | '\u{16EE}' .. '\u{16F8}'
    | '\u{1700}' .. '\u{1711}'
    | '\u{171F}' .. '\u{1731}'
    | '\u{1740}' .. '\u{1751}'
    | '\u{1760}' .. '\u{176C}'
    | '\u{176E}' .. '\u{1770}'
    | '\u{1780}' .. '\u{17B3}'
    | '\u{17D7}'
    | '\u{17DC}'
    | '\u{1820}' .. '\u{1878}'
    | '\u{1880}' .. '\u{18A8}'
    | '\u{18AA}'
    | '\u{18B0}' .. '\u{18F5}'
    | '\u{1900}' .. '\u{191E}'
    | '\u{1950}' .. '\u{196D}'
    | '\u{1970}' .. '\u{1974}'
    | '\u{1980}' .. '\u{19AB}'
    | '\u{19B0}' .. '\u{19C9}'
    | '\u{1A00}' .. '\u{1A16}'
    | '\u{1A20}' .. '\u{1A54}'
    | '\u{1AA7}'
    | '\u{1B05}' .. '\u{1B33}'
    | '\u{1B45}' .. '\u{1B4C}'
    | '\u{1B83}' .. '\u{1BA0}'
    | '\u{1BAE}' .. '\u{1BAF}'
    | '\u{1BBA}' .. '\u{1BE5}'
    | '\u{1C00}' .. '\u{1C23}'
    | '\u{1C4D}' .. '\u{1C4F}'
    | '\u{1C5A}' .. '\u{1C7D}'
    | '\u{1C80}' .. '\u{1C8A}'
    | '\u{1C90}' .. '\u{1CBA}'
    | '\u{1CBD}' .. '\u{1CBF}'
    | '\u{1CE9}' .. '\u{1CEC}'
    | '\u{1CEE}' .. '\u{1CF3}'
    | '\u{1CF5}' .. '\u{1CF6}'
    | '\u{1CFA}'
    | '\u{1D00}' .. '\u{1DBF}'
    | '\u{1E00}' .. '\u{1F15}'
    | '\u{1F18}' .. '\u{1F1D}'
    | '\u{1F20}' .. '\u{1F45}'
    | '\u{1F48}' .. '\u{1F4D}'
    | '\u{1F50}' .. '\u{1F57}'
    | '\u{1F59}'
    | '\u{1F5B}'
    | '\u{1F5D}'
    | '\u{1F5F}' .. '\u{1F7D}'
    | '\u{1F80}' .. '\u{1FB4}'
    | '\u{1FB6}' .. '\u{1FBC}'
    | '\u{1FBE}'
    | '\u{1FC2}' .. '\u{1FC4}'
    | '\u{1FC6}' .. '\u{1FCC}'
    | '\u{1FD0}' .. '\u{1FD3}'
    | '\u{1FD6}' .. '\u{1FDB}'
    | '\u{1FE0}' .. '\u{1FEC}'
    | '\u{1FF2}' .. '\u{1FF4}'
    | '\u{1FF6}' .. '\u{1FFC}'
    | '\u{2071}'
    | '\u{207F}'
    | '\u{2090}' .. '\u{209C}'
    | '\u{2102}'
    | '\u{2107}'
    | '\u{210A}' .. '\u{2113}'
    | '\u{2115}'
    | '\u{2118}' .. '\u{211D}'
    | '\u{2124}'
    | '\u{2126}'
    | '\u{2128}'
    | '\u{212A}' .. '\u{2139}'
    | '\u{213C}' .. '\u{213F}'
    | '\u{2145}' .. '\u{2149}'
    | '\u{214E}'
    | '\u{2160}' .. '\u{2188}'
    | '\u{2C00}' .. '\u{2CE4}'
    | '\u{2CEB}' .. '\u{2CEE}'
    | '\u{2CF2}' .. '\u{2CF3}'
    | '\u{2D00}' .. '\u{2D25}'
    | '\u{2D27}'
    | '\u{2D2D}'
    | '\u{2D30}' .. '\u{2D67}'
    | '\u{2D6F}'
    | '\u{2D80}' .. '\u{2D96}'
    | '\u{2DA0}' .. '\u{2DA6}'
    | '\u{2DA8}' .. '\u{2DAE}'
    | '\u{2DB0}' .. '\u{2DB6}'
    | '\u{2DB8}' .. '\u{2DBE}'
    | '\u{2DC0}' .. '\u{2DC6}'
    | '\u{2DC8}' .. '\u{2DCE}'
    | '\u{2DD0}' .. '\u{2DD6}'
    | '\u{2DD8}' .. '\u{2DDE}'
    | '\u{3005}' .. '\u{3007}'
    | '\u{3021}' .. '\u{3029}'
    | '\u{3031}' .. '\u{3035}'
    | '\u{3038}' .. '\u{303C}'
    | '\u{3041}' .. '\u{3096}'
    | '\u{309D}' .. '\u{309F}'
    | '\u{30A1}' .. '\u{30FA}'
    | '\u{30FC}' .. '\u{30FF}'
    | '\u{3105}' .. '\u{312F}'
    | '\u{3131}' .. '\u{318E}'
    | '\u{31A0}' .. '\u{31BF}'
    | '\u{31F0}' .. '\u{31FF}'
    | '\u{3400}' .. '\u{4DBF}'
    | '\u{4E00}' .. '\u{A48C}'
    | '\u{A4D0}' .. '\u{A4FD}'
    | '\u{A500}' .. '\u{A60C}'
    | '\u{A610}' .. '\u{A61F}'
    | '\u{A62A}' .. '\u{A62B}'
    | '\u{A640}' .. '\u{A66E}'
    | '\u{A67F}' .. '\u{A69D}'
    | '\u{A6A0}' .. '\u{A6EF}'
    | '\u{A717}' .. '\u{A71F}'
    | '\u{A722}' .. '\u{A788}'
    | '\u{A78B}' .. '\u{A7CD}'
    | '\u{A7D0}' .. '\u{A7D1}'
    | '\u{A7D3}'
    | '\u{A7D5}' .. '\u{A7DC}'
    | '\u{A7F2}' .. '\u{A801}'
    | '\u{A803}' .. '\u{A805}'
    | '\u{A807}' .. '\u{A80A}'
    | '\u{A80C}' .. '\u{A822}'
    | '\u{A840}' .. '\u{A873}'
    | '\u{A882}' .. '\u{A8B3}'
    | '\u{A8F2}' .. '\u{A8F7}'
    | '\u{A8FB}'
    | '\u{A8FD}' .. '\u{A8FE}'
    | '\u{A90A}' .. '\u{A925}'
    | '\u{A930}' .. '\u{A946}'
    | '\u{A960}' .. '\u{A97C}'
    | '\u{A984}' .. '\u{A9B2}'
    | '\u{A9CF}'
    | '\u{A9E0}' .. '\u{A9E4}'
    | '\u{A9E6}' .. '\u{A9EF}'
    | '\u{A9FA}' .. '\u{A9FE}'
    | '\u{AA00}' .. '\u{AA28}'
    | '\u{AA40}' .. '\u{AA42}'
    | '\u{AA44}' .. '\u{AA4B}'
    | '\u{AA60}' .. '\u{AA76}'
    | '\u{AA7A}'
    | '\u{AA7E}' .. '\u{AAAF}'
    | '\u{AAB1}'
    | '\u{AAB5}' .. '\u{AAB6}'
    | '\u{AAB9}' .. '\u{AABD}'
    | '\u{AAC0}'
    | '\u{AAC2}'
    | '\u{AADB}' .. '\u{AADD}'
    | '\u{AAE0}' .. '\u{AAEA}'
    | '\u{AAF2}' .. '\u{AAF4}'
    | '\u{AB01}' .. '\u{AB06}'
    | '\u{AB09}' .. '\u{AB0E}'
    | '\u{AB11}' .. '\u{AB16}'
    | '\u{AB20}' .. '\u{AB26}'
    | '\u{AB28}' .. '\u{AB2E}'
    | '\u{AB30}' .. '\u{AB5A}'
    | '\u{AB5C}' .. '\u{AB69}'
    | '\u{AB70}' .. '\u{ABE2}'
    | '\u{AC00}' .. '\u{D7A3}'
    | '\u{D7B0}' .. '\u{D7C6}'
    | '\u{D7CB}' .. '\u{D7FB}'
    | '\u{F900}' .. '\u{FA6D}'
    | '\u{FA70}' .. '\u{FAD9}'
    | '\u{FB00}' .. '\u{FB06}'
    | '\u{FB13}' .. '\u{FB17}'
    | '\u{FB1D}'
    | '\u{FB1F}' .. '\u{FB28}'
    | '\u{FB2A}' .. '\u{FB36}'
    | '\u{FB38}' .. '\u{FB3C}'
    | '\u{FB3E}'
    | '\u{FB40}' .. '\u{FB41}'
    | '\u{FB43}' .. '\u{FB44}'
    | '\u{FB46}' .. '\u{FBB1}'
    | '\u{FBD3}' .. '\u{FC5D}'
    | '\u{FC64}' .. '\u{FD3D}'
    | '\u{FD50}' .. '\u{FD8F}'
    | '\u{FD92}' .. '\u{FDC7}'
    | '\u{FDF0}' .. '\u{FDF9}'
    | '\u{FE71}'
    | '\u{FE73}'
    | '\u{FE77}'
    | '\u{FE79}'
    | '\u{FE7B}'
    | '\u{FE7D}'
    | '\u{FE7F}' .. '\u{FEFC}'
    | '\u{FF21}' .. '\u{FF3A}'
    | '\u{FF41}' .. '\u{FF5A}'
    | '\u{FF66}' .. '\u{FF9D}'
    | '\u{FFA0}' .. '\u{FFBE}'
    | '\u{FFC2}' .. '\u{FFC7}'
    | '\u{FFCA}' .. '\u{FFCF}'
    | '\u{FFD2}' .. '\u{FFD7}'
    | '\u{FFDA}' .. '\u{FFDC}'
    | '\u{10000}' .. '\u{1000B}'
    | '\u{1000D}' .. '\u{10026}'
    | '\u{10028}' .. '\u{1003A}'
    | '\u{1003C}' .. '\u{1003D}'
    | '\u{1003F}' .. '\u{1004D}'
    | '\u{10050}' .. '\u{1005D}'
    | '\u{10080}' .. '\u{100FA}'
    | '\u{10140}' .. '\u{10174}'
    | '\u{10280}' .. '\u{1029C}'
    | '\u{102A0}' .. '\u{102D0}'
    | '\u{10300}' .. '\u{1031F}'
    | '\u{1032D}' .. '\u{1034A}'
    | '\u{10350}' .. '\u{10375}'
    | '\u{10380}' .. '\u{1039D}'
    | '\u{103A0}' .. '\u{103C3}'
    | '\u{103C8}' .. '\u{103CF}'
    | '\u{103D1}' .. '\u{103D5}'
    | '\u{10400}' .. '\u{1049D}'
    | '\u{104B0}' .. '\u{104D3}'
    | '\u{104D8}' .. '\u{104FB}'
    | '\u{10500}' .. '\u{10527}'
    | '\u{10530}' .. '\u{10563}'
    | '\u{10570}' .. '\u{1057A}'
    | '\u{1057C}' .. '\u{1058A}'
    | '\u{1058C}' .. '\u{10592}'
    | '\u{10594}' .. '\u{10595}'
    | '\u{10597}' .. '\u{105A1}'
    | '\u{105A3}' .. '\u{105B1}'
    | '\u{105B3}' .. '\u{105B9}'
    | '\u{105BB}' .. '\u{105BC}'
    | '\u{105C0}' .. '\u{105F3}'
    | '\u{10600}' .. '\u{10736}'
    | '\u{10740}' .. '\u{10755}'
    | '\u{10760}' .. '\u{10767}'
    | '\u{10780}' .. '\u{10785}'
    | '\u{10787}' .. '\u{107B0}'
    | '\u{107B2}' .. '\u{107BA}'
    | '\u{10800}' .. '\u{10805}'
    | '\u{10808}'
    | '\u{1080A}' .. '\u{10835}'
    | '\u{10837}' .. '\u{10838}'
    | '\u{1083C}'
    | '\u{1083F}' .. '\u{10855}'
    | '\u{10860}' .. '\u{10876}'
    | '\u{10880}' .. '\u{1089E}'
    | '\u{108E0}' .. '\u{108F2}'
    | '\u{108F4}' .. '\u{108F5}'
    | '\u{10900}' .. '\u{10915}'
    | '\u{10920}' .. '\u{10939}'
    | '\u{10980}' .. '\u{109B7}'
    | '\u{109BE}' .. '\u{109BF}'
    | '\u{10A00}'
    | '\u{10A10}' .. '\u{10A13}'
    | '\u{10A15}' .. '\u{10A17}'
    | '\u{10A19}' .. '\u{10A35}'
    | '\u{10A60}' .. '\u{10A7C}'
    | '\u{10A80}' .. '\u{10A9C}'
    | '\u{10AC0}' .. '\u{10AC7}'
    | '\u{10AC9}' .. '\u{10AE4}'
    | '\u{10B00}' .. '\u{10B35}'
    | '\u{10B40}' .. '\u{10B55}'
    | '\u{10B60}' .. '\u{10B72}'
    | '\u{10B80}' .. '\u{10B91}'
    | '\u{10C00}' .. '\u{10C48}'
    | '\u{10C80}' .. '\u{10CB2}'
    | '\u{10CC0}' .. '\u{10CF2}'
    | '\u{10D00}' .. '\u{10D23}'
    | '\u{10D4A}' .. '\u{10D65}'
    | '\u{10D6F}' .. '\u{10D85}'
    | '\u{10E80}' .. '\u{10EA9}'
    | '\u{10EB0}' .. '\u{10EB1}'
    | '\u{10EC2}' .. '\u{10EC4}'
    | '\u{10F00}' .. '\u{10F1C}'
    | '\u{10F27}'
    | '\u{10F30}' .. '\u{10F45}'
    | '\u{10F70}' .. '\u{10F81}'
    | '\u{10FB0}' .. '\u{10FC4}'
    | '\u{10FE0}' .. '\u{10FF6}'
    | '\u{11003}' .. '\u{11037}'
    | '\u{11071}' .. '\u{11072}'
    | '\u{11075}'
    | '\u{11083}' .. '\u{110AF}'
    | '\u{110D0}' .. '\u{110E8}'
    | '\u{11103}' .. '\u{11126}'
    | '\u{11144}'
    | '\u{11147}'
    | '\u{11150}' .. '\u{11172}'
    | '\u{11176}'
    | '\u{11183}' .. '\u{111B2}'
    | '\u{111C1}' .. '\u{111C4}'
    | '\u{111DA}'
    | '\u{111DC}'
    | '\u{11200}' .. '\u{11211}'
    | '\u{11213}' .. '\u{1122B}'
    | '\u{1123F}' .. '\u{11240}'
    | '\u{11280}' .. '\u{11286}'
    | '\u{11288}'
    | '\u{1128A}' .. '\u{1128D}'
    | '\u{1128F}' .. '\u{1129D}'
    | '\u{1129F}' .. '\u{112A8}'
    | '\u{112B0}' .. '\u{112DE}'
    | '\u{11305}' .. '\u{1130C}'
    | '\u{1130F}' .. '\u{11310}'
    | '\u{11313}' .. '\u{11328}'
    | '\u{1132A}' .. '\u{11330}'
    | '\u{11332}' .. '\u{11333}'
    | '\u{11335}' .. '\u{11339}'
    | '\u{1133D}'
    | '\u{11350}'
    | '\u{1135D}' .. '\u{11361}'
    | '\u{11380}' .. '\u{11389}'
    | '\u{1138B}'
    | '\u{1138E}'
    | '\u{11390}' .. '\u{113B5}'
    | '\u{113B7}'
    | '\u{113D1}'
    | '\u{113D3}'
    | '\u{11400}' .. '\u{11434}'
    | '\u{11447}' .. '\u{1144A}'
    | '\u{1145F}' .. '\u{11461}'
    | '\u{11480}' .. '\u{114AF}'
    | '\u{114C4}' .. '\u{114C5}'
    | '\u{114C7}'
    | '\u{11580}' .. '\u{115AE}'
    | '\u{115D8}' .. '\u{115DB}'
    | '\u{11600}' .. '\u{1162F}'
    | '\u{11644}'
    | '\u{11680}' .. '\u{116AA}'
    | '\u{116B8}'
    | '\u{11700}' .. '\u{1171A}'
    | '\u{11740}' .. '\u{11746}'
    | '\u{11800}' .. '\u{1182B}'
    | '\u{118A0}' .. '\u{118DF}'
    | '\u{118FF}' .. '\u{11906}'
    | '\u{11909}'
    | '\u{1190C}' .. '\u{11913}'
    | '\u{11915}' .. '\u{11916}'
    | '\u{11918}' .. '\u{1192F}'
    | '\u{1193F}'
    | '\u{11941}'
    | '\u{119A0}' .. '\u{119A7}'
    | '\u{119AA}' .. '\u{119D0}'
    | '\u{119E1}'
    | '\u{119E3}'
    | '\u{11A00}'
    | '\u{11A0B}' .. '\u{11A32}'
    | '\u{11A3A}'
    | '\u{11A50}'
    | '\u{11A5C}' .. '\u{11A89}'
    | '\u{11A9D}'
    | '\u{11AB0}' .. '\u{11AF8}'
    | '\u{11BC0}' .. '\u{11BE0}'
    | '\u{11C00}' .. '\u{11C08}'
    | '\u{11C0A}' .. '\u{11C2E}'
    | '\u{11C40}'
    | '\u{11C72}' .. '\u{11C8F}'
    | '\u{11D00}' .. '\u{11D06}'
    | '\u{11D08}' .. '\u{11D09}'
    | '\u{11D0B}' .. '\u{11D30}'
    | '\u{11D46}'
    | '\u{11D60}' .. '\u{11D65}'
    | '\u{11D67}' .. '\u{11D68}'
    | '\u{11D6A}' .. '\u{11D89}'
    | '\u{11D98}'
    | '\u{11EE0}' .. '\u{11EF2}'
    | '\u{11F02}'
    | '\u{11F04}' .. '\u{11F10}'
    | '\u{11F12}' .. '\u{11F33}'
    | '\u{11FB0}'
    | '\u{12000}' .. '\u{12399}'
    | '\u{12400}' .. '\u{1246E}'
    | '\u{12480}' .. '\u{12543}'
    | '\u{12F90}' .. '\u{12FF0}'
    | '\u{13000}' .. '\u{1342F}'
    | '\u{13441}' .. '\u{13446}'
    | '\u{13460}' .. '\u{143FA}'
    | '\u{14400}' .. '\u{14646}'
    | '\u{16100}' .. '\u{1611D}'
    | '\u{16800}' .. '\u{16A38}'
    | '\u{16A40}' .. '\u{16A5E}'
    | '\u{16A70}' .. '\u{16ABE}'
    | '\u{16AD0}' .. '\u{16AED}'
    | '\u{16B00}' .. '\u{16B2F}'
    | '\u{16B40}' .. '\u{16B43}'
    | '\u{16B63}' .. '\u{16B77}'
    | '\u{16B7D}' .. '\u{16B8F}'
    | '\u{16D40}' .. '\u{16D6C}'
    | '\u{16E40}' .. '\u{16E7F}'
    | '\u{16F00}' .. '\u{16F4A}'
    | '\u{16F50}'
    | '\u{16F93}' .. '\u{16F9F}'
    | '\u{16FE0}' .. '\u{16FE1}'
    | '\u{16FE3}'
    | '\u{17000}' .. '\u{187F7}'
    | '\u{18800}' .. '\u{18CD5}'
    | '\u{18CFF}' .. '\u{18D08}'
    | '\u{1AFF0}' .. '\u{1AFF3}'
    | '\u{1AFF5}' .. '\u{1AFFB}'
    | '\u{1AFFD}' .. '\u{1AFFE}'
    | '\u{1B000}' .. '\u{1B122}'
    | '\u{1B132}'
    | '\u{1B150}' .. '\u{1B152}'
    | '\u{1B155}'
    | '\u{1B164}' .. '\u{1B167}'
    | '\u{1B170}' .. '\u{1B2FB}'
    | '\u{1BC00}' .. '\u{1BC6A}'
    | '\u{1BC70}' .. '\u{1BC7C}'
    | '\u{1BC80}' .. '\u{1BC88}'
    | '\u{1BC90}' .. '\u{1BC99}'
    | '\u{1D400}' .. '\u{1D454}'
    | '\u{1D456}' .. '\u{1D49C}'
    | '\u{1D49E}' .. '\u{1D49F}'
    | '\u{1D4A2}'
    | '\u{1D4A5}' .. '\u{1D4A6}'
    | '\u{1D4A9}' .. '\u{1D4AC}'
    | '\u{1D4AE}' .. '\u{1D4B9}'
    | '\u{1D4BB}'
    | '\u{1D4BD}' .. '\u{1D4C3}'
    | '\u{1D4C5}' .. '\u{1D505}'
    | '\u{1D507}' .. '\u{1D50A}'
    | '\u{1D50D}' .. '\u{1D514}'
    | '\u{1D516}' .. '\u{1D51C}'
    | '\u{1D51E}' .. '\u{1D539}'
    | '\u{1D53B}' .. '\u{1D53E}'
    | '\u{1D540}' .. '\u{1D544}'
    | '\u{1D546}'
    | '\u{1D54A}' .. '\u{1D550}'
    | '\u{1D552}' .. '\u{1D6A5}'
    | '\u{1D6A8}' .. '\u{1D6C0}'
    | '\u{1D6C2}' .. '\u{1D6DA}'
    | '\u{1D6DC}' .. '\u{1D6FA}'
    | '\u{1D6FC}' .. '\u{1D714}'
    | '\u{1D716}' .. '\u{1D734}'
    | '\u{1D736}' .. '\u{1D74E}'
    | '\u{1D750}' .. '\u{1D76E}'
    | '\u{1D770}' .. '\u{1D788}'
    | '\u{1D78A}' .. '\u{1D7A8}'
    | '\u{1D7AA}' .. '\u{1D7C2}'
    | '\u{1D7C4}' .. '\u{1D7CB}'
    | '\u{1DF00}' .. '\u{1DF1E}'
    | '\u{1DF25}' .. '\u{1DF2A}'
    | '\u{1E030}' .. '\u{1E06D}'
    | '\u{1E100}' .. '\u{1E12C}'
    | '\u{1E137}' .. '\u{1E13D}'
    | '\u{1E14E}'
    | '\u{1E290}' .. '\u{1E2AD}'
    | '\u{1E2C0}' .. '\u{1E2EB}'
    | '\u{1E4D0}' .. '\u{1E4EB}'
    | '\u{1E5D0}' .. '\u{1E5ED}'
    | '\u{1E5F0}'
    | '\u{1E7E0}' .. '\u{1E7E6}'
    | '\u{1E7E8}' .. '\u{1E7EB}'
    | '\u{1E7ED}' .. '\u{1E7EE}'
    | '\u{1E7F0}' .. '\u{1E7FE}'
    | '\u{1E800}' .. '\u{1E8C4}'
    | '\u{1E900}' .. '\u{1E943}'
    | '\u{1E94B}'
    | '\u{1EE00}' .. '\u{1EE03}'
    | '\u{1EE05}' .. '\u{1EE1F}'
    | '\u{1EE21}' .. '\u{1EE22}'
    | '\u{1EE24}'
    | '\u{1EE27}'
    | '\u{1EE29}' .. '\u{1EE32}'
    | '\u{1EE34}' .. '\u{1EE37}'
    | '\u{1EE39}'
    | '\u{1EE3B}'
    | '\u{1EE42}'
    | '\u{1EE47}'
    | '\u{1EE49}'
    | '\u{1EE4B}'
    | '\u{1EE4D}' .. '\u{1EE4F}'
    | '\u{1EE51}' .. '\u{1EE52}'
    | '\u{1EE54}'
    | '\u{1EE57}'
    | '\u{1EE59}'
    | '\u{1EE5B}'
    | '\u{1EE5D}'
    | '\u{1EE5F}'
    | '\u{1EE61}' .. '\u{1EE62}'
    | '\u{1EE64}'
    | '\u{1EE67}' .. '\u{1EE6A}'
    | '\u{1EE6C}' .. '\u{1EE72}'
    | '\u{1EE74}' .. '\u{1EE77}'
    | '\u{1EE79}' .. '\u{1EE7C}'
    | '\u{1EE7E}'
    | '\u{1EE80}' .. '\u{1EE89}'
    | '\u{1EE8B}' .. '\u{1EE9B}'
    | '\u{1EEA1}' .. '\u{1EEA3}'
    | '\u{1EEA5}' .. '\u{1EEA9}'
    | '\u{1EEAB}' .. '\u{1EEBB}'
    | '\u{20000}' .. '\u{2A6DF}'
    | '\u{2A700}' .. '\u{2B739}'
    | '\u{2B740}' .. '\u{2B81D}'
    | '\u{2B820}' .. '\u{2CEA1}'
    | '\u{2CEB0}' .. '\u{2EBE0}'
    | '\u{2EBF0}' .. '\u{2EE5D}'
    | '\u{2F800}' .. '\u{2FA1D}'
    | '\u{30000}' .. '\u{3134A}'
    | '\u{31350}' .. '\u{323AF}'
    ;
