/*
Objective-C Preprocessor grammar.
The MIT License (MIT).
Copyright (c) 2016-2017, Ivan Kochurkin (kvanttt@gmail.com).

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

lexer grammar ObjectiveCPreprocessorLexer;

channels { COMMENTS_CHANNEL }

SHARP:                    '#'                                        -> mode(DIRECTIVE_MODE);
COMMENT:                  '/*' .*? '*/'                              -> type(CODE);
LINE_COMMENT:             '//' ~[\r\n]*                              -> type(CODE);
SLASH:                    '/'                                        -> type(CODE);
CHARACTER_LITERAL:        '\'' (EscapeSequence | ~('\''|'\\')) '\''  -> type(CODE);
QUOTE_STRING:             '\'' (EscapeSequence | ~('\''|'\\'))* '\'' -> type(CODE);
STRING:                   StringFragment                             -> type(CODE);
CODE:                     ~[#'"/]+;

mode DIRECTIVE_MODE;

IMPORT:  'import' [ \t]+ -> mode(DIRECTIVE_TEXT);
INCLUDE: 'include' [ \t]+ -> mode(DIRECTIVE_TEXT);
PRAGMA:  'pragma' -> mode(DIRECTIVE_TEXT);

DEFINE:  'define' [ \t]+ -> mode(DIRECTIVE_DEFINE);
DEFINED: 'defined';
IF:      'if';
ELIF:    'elif';
ELSE:    'else';
UNDEF:   'undef';
IFDEF:   'ifdef';
IFNDEF:  'ifndef';
ENDIF:   'endif';
TRUE:     T R U E;
FALSE:    F A L S E;
ERROR:   'error' -> mode(DIRECTIVE_TEXT);

BANG:             '!' ;
LPAREN:           '(' ;
RPAREN:           ')' ;
EQUAL:            '==';
NOTEQUAL:         '!=';
AND:              '&&';
OR:               '||';
LT:               '<' ;
GT:               '>' ;
LE:               '<=';
GE:               '>=';

DIRECTIVE_WHITESPACES:      [ \t]+                           -> channel(HIDDEN);
DIRECTIVE_STRING:           StringFragment;
CONDITIONAL_SYMBOL:         LETTER (LETTER | [0-9])*;
DECIMAL_LITERAL:            [0-9]+;
FLOAT:                      ([0-9]+ '.' [0-9]* | '.' [0-9]+);
NEW_LINE:                   '\r'? '\n'                       -> mode(DEFAULT_MODE);
DIRECITVE_COMMENT:          '/*' .*? '*/'                    -> channel(COMMENTS_CHANNEL);
DIRECITVE_LINE_COMMENT:     '//' ~[\r\n]*                    -> channel(COMMENTS_CHANNEL);
DIRECITVE_NEW_LINE:         '\\' '\r'? '\n'                  -> channel(HIDDEN);

mode DIRECTIVE_DEFINE;

DIRECTIVE_DEFINE_CONDITIONAL_SYMBOL: LETTER (LETTER | [0-9])* ('(' (LETTER | [0-9,. \t])* ')')? -> type(CONDITIONAL_SYMBOL), mode(DIRECTIVE_TEXT);

mode DIRECTIVE_TEXT;

DIRECITVE_TEXT_NEW_LINE:         '\\' '\r'? '\n'  -> channel(HIDDEN);
BACK_SLASH_ESCAPE:               '\\' .           -> type(TEXT);
TEXT_NEW_LINE:                   '\r'? '\n'       -> type(NEW_LINE), mode(DEFAULT_MODE);
DIRECTIVE_COMMENT:               '/*' .*? '*/'    -> channel(COMMENTS_CHANNEL), type(DIRECITVE_COMMENT);
DIRECTIVE_LINE_COMMENT:          '//' ~[\r\n]*    -> channel(COMMENTS_CHANNEL), type(DIRECITVE_LINE_COMMENT);
DIRECTIVE_SLASH:                 '/'              -> type(TEXT);
TEXT:                            ~[\r\n\\/]+;

fragment
EscapeSequence
    : '\\' ('b'|'t'|'n'|'f'|'r'|'"'|'\''|'\\')
    | OctalEscape
    | UnicodeEscape
    ;

fragment
OctalEscape
    :   '\\' [0-3] [0-7] [0-7]
    |   '\\' [0-7] [0-7]
    |   '\\' [0-7]
    ;

fragment
UnicodeEscape
    :   '\\' 'u' HexDigit HexDigit HexDigit HexDigit
    ;

fragment HexDigit:          [0-9a-fA-F];

fragment
StringFragment: '"' (~('\\' | '"') | '\\' .)* '"';

fragment LETTER
    : [$A-Za-z_]
    | ~[\u0000-\u00FF\uD800-\uDBFF]
    | [\uD800-\uDBFF] [\uDC00-\uDFFF]
    | [\u00E9]
    ;

fragment A: [aA];
fragment B: [bB];
fragment C: [cC];
fragment D: [dD];
fragment E: [eE];
fragment F: [fF];
fragment G: [gG];
fragment H: [hH];
fragment I: [iI];
fragment J: [jJ];
fragment K: [kK];
fragment L: [lL];
fragment M: [mM];
fragment N: [nN];
fragment O: [oO];
fragment P: [pP];
fragment Q: [qQ];
fragment R: [rR];
fragment S: [sS];
fragment T: [tT];
fragment U: [uU];
fragment V: [vV];
fragment W: [wW];
fragment X: [xX];
fragment Y: [yY];
fragment Z: [zZ];