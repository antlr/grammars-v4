/*
MIT License

Copyright (c) 2023 Mustafa Said Ağca

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
associated documentation files (the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge, publish, distribute,
sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or
substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT
OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

// $antlr-format alignTrailingComments true, columnLimit 150, minEmptyLines 1, maxEmptyLinesToKeep 1, reflowComments false, useTab false
// $antlr-format allowShortRulesOnASingleLine false, allowShortBlocksOnASingleLine true, alignSemicolons hanging, alignColons hanging


lexer grammar  awkLexer;

// Insert @header for lexer.

options { superClass = awkLexerBase; }

Minus: '-';
Bang: '!';
Dollar: '$';
Percent: '%';
Lp: '(';
Rp: ')';
Star: '*';
Comma: ',';
Slash: '/';
Colon: ':';
Semicolon: ';';
Question: '?';
Lb: '[';
Rb: ']';
Ua: '^';
Lc: '{';
Vb: '|';
Rc: '}';
Sq: '~';
Plus: '+';
Lt: '<';
Eq: '=';
Gt: '>';


BEGIN
    : 'BEGIN'
    ;

BREAK
    : 'break'
    ;

CONTINUE
    : 'continue'
    ;

DELETE
    : 'delete'
    ;

DO
    : 'do'
    ;

ELSE
    : 'else'
    ;

END
    : 'END'
    ;

EXIT
    : 'exit'
    ;

FOR
    : 'for'
    ;

FUNCTION
    : 'function'
    ;

GETLINE
    : 'getline'
    ;

IF
    : 'if'
    ;

IN
    : 'in'
    ;

NEXT
    : 'next'
    ;

PRINT
    : 'print'
    ;

PRINTF
    : 'printf'
    ;

RETURN
    : 'return'
    ;

WHILE
    : 'while'
    ;

BUILTIN_FUNC_NAME
    : 'atan2'
    | 'close'
    | 'cos'
    | 'exp'
    | 'gsub'
    | 'index'
    | 'int'
    | 'length'
    | 'log'
    | 'match'
    | 'rand'
    | 'sin'
    | 'split'
    | 'sprintf'
    | 'sqrt'
    | 'srand'
    | 'sub'
    | 'substr'
    | 'system'
    | 'tolower'
    | 'toupper'
    ;

ADD_ASSIGN
    : '+='
    ;

AND
    : '&&'
    ;

APPEND
    : '>>'
    ;

DECR
    : '--'
    ;

DIV_ASSIGN
    : '/='
    ;

EQ
    : '=='
    ;

GE
    : '>='
    ;

INCR
    : '++'
    ;

LE
    : '<='
    ;

MOD_ASSIGN
    : '%='
    ;

MUL_ASSIGN
    : '*='
    ;

NE
    : '!='
    ;

NO_MATCH
    : '!~'
    ;

OR
    : '||'
    ;

POW_ASSIGN
    : '^='
    ;

SUB_ASSIGN
    : '-='
    ;

COMMENT
    : '#' ~[\r\n]* -> channel(HIDDEN)
    ;

ERE
    : {this.IsNotAfterExpr()}? '/' ERE_BODY* '/'
    ;

fragment ERE_BODY
    : ~[/\\\r\n\u005B]
    | ESCAPE_SEQUENCE
    | '[' '^'? ']'? (~[\]\\\r\n] | ESCAPE_SEQUENCE)* ']'
    ;

ESC_NEWLINE
    : '\\' NEWLINE -> skip
    ;

NEWLINE
    : '\r'? '\n'
    ;

NUMBER
    : DECIMAL_CONSTANT
    | FLOAT_CONSTANT
    | HEX_CONSTANT
    | OCTAL_CONSTANT
    ;

SPACE
    : [ \t]+ -> skip
    ;

STRING
    : '"' (~["\\\r\n] | ESCAPE_SEQUENCE)* '"'
    ;

WORD
    : [A-Za-z_] [A-Za-z_0-9]*
    ;

fragment DECIMAL_CONSTANT
    : [1-9] [0-9]*
    ;

fragment DIGIT_SEQUENCE
    : [0-9]+
    ;

fragment ESCAPE_SEQUENCE
    : '\\' (HEX_NUMBER | OCTAL_NUMBER | .)
    ;

fragment EXPONENT_PART
    : [eE] [+\-]? DIGIT_SEQUENCE
    ;

fragment FLOAT_CONSTANT
    : DIGIT_SEQUENCE '.' DIGIT_SEQUENCE? EXPONENT_PART?
    | '.' DIGIT_SEQUENCE EXPONENT_PART?
    | DIGIT_SEQUENCE EXPONENT_PART
    ;

fragment HEX_CONSTANT
    : '0' [xX] [0-9A-Fa-f]+
    ;

fragment HEX_NUMBER
    : [xX] [0-9A-Fa-f] [0-9A-Fa-f]?
    ;

fragment OCTAL_CONSTANT
    : '0' [0-7]*
    ;

fragment OCTAL_NUMBER
    : [0-7] [0-7]? [0-7]?
    ;
