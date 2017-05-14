/*
Objective-C grammar.
The MIT License (MIT).
Copyright (c) 2016, Alex Petuschak (alex@swiftify.io).
Copyright (c) 2016, Ivan Kochurkin (kvanttt@gmail.com).
Converted to ANTLR 4 by Terence Parr; added @property and a few others.
Updated June 2014, Carlos Mejia.  Fix try-catch, add support for @( @{ @[ and blocks
June 2008 Cedric Cuche

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

lexer grammar ObjectiveCLexer;

channels { COMMENTS_CHANNEL, DIRECTIVE_CHANNEL, IGNORED_MACROS }

// 3.9 Keywords

AUTORELEASEPOOL : '@autoreleasepool';
CATCH           : '@catch';
CLASS           : '@class';
DYNAMIC         : '@dynamic';
ENCODE          : '@encode';
END             : '@end';
FINALLY         : '@finally';
IMPLEMENTATION  : '@implementation';
INTERFACE       : '@interface';
IMPORT          : '@import';
PACKAGE         : '@package';
PROTOCOL        : '@protocol';
OPTIONAL        : '@optional';
PRIVATE         : '@private';
PROPERTY        : '@property';
PROTECTED       : '@protected';
PUBLIC          : '@public';
REQUIRED        : '@required';
SELECTOR        : '@selector';
SYNCHRONIZED    : '@synchronized';
SYNTHESIZE      : '@synthesize';
THROW           : '@throw';
TRY             : '@try';

ABSTRACT      : 'abstract';
AUTO          : 'auto';
BOOLEAN       : 'boolean';
BREAK         : 'break';
BYCOPY        : 'bycopy';
BYREF         : 'byref';
CASE          : 'case';
CHAR          : 'char';
CONST         : 'const';
CONTINUE      : 'continue';
DEFAULT       : 'default';
DO            : 'do';
DOUBLE        : 'double';
ELSE          : 'else';
ENUM          : 'enum';
EXTERN        : 'extern';
FLOAT         : 'float';
FOR           : 'for';
ID            : 'id';
IF            : 'if';
IN            : 'in';
INOUT         : 'inout';
INSTANCETYPE  : 'instancetype';
GOTO          : 'goto';
INT           : 'int';
LONG          : 'long';
ONEWAY        : 'oneway';
OUT           : 'out';
REGISTER      : 'register';
RETURN        : 'return';
SHORT         : 'short';
SIGNED        : 'signed';
SIZEOF        : 'sizeof';
STATIC        : 'static';
STRUCT        : 'struct';
SWITCH        : 'switch';
TYPEDEF       : 'typedef';
UNION         : 'union';
UNSIGNED      : 'unsigned';
VOID          : 'void';
VOLATILE      : 'volatile';
WHILE         : 'while';

NS_OPTIONS          : 'NS_OPTIONS';
NS_ENUM             : 'NS_ENUM';
WWEAK               : '__weak';
WUNSAFE_UNRETAINED  : '__unsafe_unretained';
WUNUSED             : '__unused';
WDEPRECATED         : '__deprecated';
WAUTORELEASING      : '__autoreleasing';
TYPEOF              : 'typeof';
TYPEOF__            : '__typeof';
TYPEOF____          : '__typeof__';
KINDOF__            : '__kindof';
COVARIANT           : '__covariant';
CONTRAVARIANT       : '__contravariant';
ATTRIBUTE           : '__attribute__';

NULLABLE            : 'nullable';
NONNULL             : 'nonnull';

// Ignored macros.
NS_ASSUME_NONNULL_BEGIN : 'NS_ASSUME_NONNULL_BEGIN' ~[\r\n]*  -> channel(IGNORED_MACROS);
NS_ASSUME_NONNULL_END   : 'NS_ASSUME_NONNULL_END' ~[\r\n]*    -> channel(IGNORED_MACROS);
EXTERN_SUFFIX           : [_A-Z]+ '_EXTERN'                   -> channel(IGNORED_MACROS);
IOS_SUFFIX              : [_A-Z]+ '_IOS(' ~')'+ ')'           -> channel(IGNORED_MACROS);
MAC_SUFFIX              : [_A-Z]+ '_MAC(' ~')'+ ')'           -> channel(IGNORED_MACROS);
TVOS_PROHIBITED         : '__TVOS_PROHIBITED'                 -> channel(IGNORED_MACROS);

// 3.11 Separators

LP              : '(';
RP              : ')';
LBRACE          : '{';
RBRACE          : '}';
LBRACK          : '[';
RBRACK          : ']';
SEMI            : ';';
COMMA           : ',';
DOT             : '.';
STRUCTACCESS    : '->';
AT              : '@';

// Operators

ASSIGN          : '=';
GT              : '>';
LT              : '<';
BANG            : '!';
TILDE           : '~';
QUESTION        : '?';
COLON           : ':';
EQUAL           : '==';
LE              : '<=';
GE              : '>=';
NOTEQUAL        : '!=';
AND             : '&&';
OR              : '||';
INC             : '++';
DEC             : '--';
ADD             : '+';
SUB             : '-';
MUL             : '*';
DIV             : '/';
BITAND          : '&';
BITOR           : '|';
CARET           : '^';
MOD             : '%';

// Assignment

ADD_ASSIGN      : '+=';
SUB_ASSIGN      : '-=';
MUL_ASSIGN      : '*=';
DIV_ASSIGN      : '/=';
AND_ASSIGN      : '&=';
OR_ASSIGN       : '|=';
XOR_ASSIGN      : '^=';
MOD_ASSIGN      : '%=';
LSHIFT_ASSIGN   : '<<=';
RSHIFT_ASSIGN   : '>>=';
ELIPSIS         : '...';

// Property attributes
ASSIGNPA        : 'assign';
GETTER          : 'getter';
NONATOMIC       : 'nonatomic';
SETTER          : 'setter';
STRONG          : 'strong';
RETAIN          : 'retain';
READONLY        : 'readonly';
READWRITE       : 'readwrite';
WEAK            : 'weak';

IB_OUTLET               : 'IBOutlet';
IB_OUTLET_COLLECTION    : 'IBOutletCollection';

IDENTIFIER: Letter LetterOrDec*;

CHARACTER_LITERAL:         '\'' (EscapeSequence | ~('\''|'\\')) '\'';
QUOTE_STRING:              '\'' (EscapeSequence | ~('\''|'\\'))* '\'';
STRING:                     StringFragment;

HEX_LITERAL:     '0' [xX] HexDigit+ IntegerTypeSuffix?;
OCTAL_LITERAL:   '0' [0-7]+ IntegerTypeSuffix?;
BINARY_LITERAL:  '0' [bB] [01]+ IntegerTypeSuffix?;
DECIMAL_LITERAL: [0-9]+ IntegerTypeSuffix?;

FLOATING_POINT_LITERAL
  : (Dec+ '.' Dec* | '.' Dec+) Exponent? FloatTypeSuffix?
  | Dec+ (Exponent FloatTypeSuffix? | FloatTypeSuffix)
  ;

WS:           [ \r\n\t\u000C]+ -> channel(HIDDEN);
COMMENT:      '/*' .*? '*/'    -> channel(COMMENTS_CHANNEL);
LINE_COMMENT: '//' ~[\r\n]*    -> channel(COMMENTS_CHANNEL);
BACKSLASH:    '\\'             -> channel(HIDDEN);

SHARP:  '#'                    -> channel(DIRECTIVE_CHANNEL), mode(DIRECTIVE_MODE);

mode DIRECTIVE_MODE;

DIRECTIVE_IMPORT:              'import' [ \t]+  -> channel(DIRECTIVE_CHANNEL), mode(DIRECTIVE_TEXT_MODE);
DIRECTIVE_INCLUDE:             'include' [ \t]+ -> channel(DIRECTIVE_CHANNEL), mode(DIRECTIVE_TEXT_MODE);
DIRECTIVE_PRAGMA:              'pragma'         -> channel(DIRECTIVE_CHANNEL), mode(DIRECTIVE_TEXT_MODE);
                               
DIRECTIVE_DEFINE:              'define' [ \t]+  -> channel(DIRECTIVE_CHANNEL), mode(DEFINE);
DIRECTIVE_DEFINED:             'defined'        -> channel(DIRECTIVE_CHANNEL);
DIRECTIVE_IF:                  'if'             -> channel(DIRECTIVE_CHANNEL);
DIRECTIVE_ELIF:                'elif'           -> channel(DIRECTIVE_CHANNEL);
DIRECTIVE_ELSE:                'else'           -> channel(DIRECTIVE_CHANNEL);
DIRECTIVE_UNDEF:               'undef'          -> channel(DIRECTIVE_CHANNEL);
DIRECTIVE_IFDEF:               'ifdef'          -> channel(DIRECTIVE_CHANNEL);
DIRECTIVE_IFNDEF:              'ifndef'         -> channel(DIRECTIVE_CHANNEL);
DIRECTIVE_ENDIF:               'endif'          -> channel(DIRECTIVE_CHANNEL);
DIRECTIVE_TRUE:                 T R U E         -> channel(DIRECTIVE_CHANNEL);
DIRECTIVE_FALSE:                F A L S E       -> channel(DIRECTIVE_CHANNEL);
DIRECTIVE_ERROR:               'error'          -> channel(DIRECTIVE_CHANNEL), mode(DIRECTIVE_TEXT_MODE);
DIRECTIVE_WARNING:             'warning'        -> channel(DIRECTIVE_CHANNEL), mode(DIRECTIVE_TEXT_MODE);

DIRECTIVE_BANG:                '!'              -> channel(DIRECTIVE_CHANNEL);
DIRECTIVE_LP:                  '('              -> channel(DIRECTIVE_CHANNEL);
DIRECTIVE_RP:                  ')'              -> channel(DIRECTIVE_CHANNEL);
DIRECTIVE_EQUAL:               '=='             -> channel(DIRECTIVE_CHANNEL);
DIRECTIVE_NOTEQUAL:            '!='             -> channel(DIRECTIVE_CHANNEL);
DIRECTIVE_AND:                 '&&'             -> channel(DIRECTIVE_CHANNEL);
DIRECTIVE_OR:                  '||'             -> channel(DIRECTIVE_CHANNEL);
DIRECTIVE_LT:                  '<'              -> channel(DIRECTIVE_CHANNEL);
DIRECTIVE_GT:                  '>'              -> channel(DIRECTIVE_CHANNEL);
DIRECTIVE_LE:                  '<='             -> channel(DIRECTIVE_CHANNEL);
DIRECTIVE_GE:                  '>='             -> channel(DIRECTIVE_CHANNEL);

DIRECTIVE_WS:                  [ \t]+                           -> channel(HIDDEN);
DIRECTIVE_STRING:              StringFragment                   -> channel(DIRECTIVE_CHANNEL);
DIRECTIVE_CONDITIONAL_SYMBOL:  Letter LetterOrDec*              -> channel(DIRECTIVE_CHANNEL);
DIRECTIVE_DECIMAL_LITERAL:     Dec+                             -> channel(DIRECTIVE_CHANNEL);
DIRECTIVE_FLOAT:               (Dec+ '.' Dec* | '.' Dec+)       -> channel(DIRECTIVE_CHANNEL);
DIRECTIVE_NEW_LINE:            '\r'? '\n'                       -> channel(DIRECTIVE_CHANNEL), mode(DEFAULT_MODE);
DIRECTIVE_COMMENT:             '/*' .*? '*/'                    -> channel(COMMENTS_CHANNEL);
DIRECTIVE_LINE_COMMENT:        '//' ~[\r\n]*                    -> channel(COMMENTS_CHANNEL);
DIRECTIVE_BACKSLASH_NEW_LINE:  '\\' '\r'? '\n'                  -> skip;

mode DEFINE;

DIRECTIVE_DEFINE_CONDITIONAL_SYMBOL: Letter LetterOrDec* ('(' (LetterOrDec | [,. \t])* ')')? -> channel(DIRECTIVE_CHANNEL), type(DIRECTIVE_CONDITIONAL_SYMBOL), mode(DIRECTIVE_TEXT_MODE);

mode DIRECTIVE_TEXT_MODE;

DIRECTIVE_TEXT_NEW_LINE:          '\\' '\r'? '\n'  -> channel(HIDDEN);
DIRECTIVE_BACK_SLASH_ESCAPE:      '\\' .           -> channel(DIRECTIVE_CHANNEL), type(DIRECTIVE_TEXT);
DIRECTIVE_TEXT_BACKSLASH_NEW_LINE:'\r'? '\n'       -> channel(DIRECTIVE_CHANNEL), type(DIRECTIVE_NEW_LINE), mode(DEFAULT_MODE);
DIRECTIVE_TEXT_COMMENT:           '/*' .*? '*/'    -> channel(DIRECTIVE_CHANNEL), type(DIRECTIVE_COMMENT);
DIRECTIVE_TEXT_LINE_COMMENT:      '//' ~[\r\n]*    -> channel(DIRECTIVE_CHANNEL), type(DIRECTIVE_LINE_COMMENT);
DIRECTIVE_SLASH:                  '/'              -> channel(DIRECTIVE_CHANNEL), type(DIRECTIVE_TEXT);
DIRECTIVE_TEXT:                   ~[\r\n\\/]+      -> channel(DIRECTIVE_CHANNEL);

fragment LetterOrDec
    : Letter
    | Dec
    ;

fragment Letter
    : [$A-Za-z_]
    | ~[\u0000-\u00FF\uD800-\uDBFF]
    | [\uD800-\uDBFF] [\uDC00-\uDFFF]
    | [\u00E9]
    ;

fragment IntegerTypeSuffix: [uUlL] [uUlL]?;
fragment Exponent:          [eE] [+\-]? Dec+;
fragment Dec:               [0-9];
fragment FloatTypeSuffix:   [fFdD];

fragment EscapeSequence
    : '\\' ('b'|'t'|'n'|'f'|'r'|'"'|'\''|'\\')
    | OctalEscape
    | UnicodeEscape
    ;

fragment OctalEscape
    :   '\\' [0-3] [0-7] [0-7]
    |   '\\' [0-7] [0-7]
    |   '\\' [0-7]
    ;

fragment UnicodeEscape:   '\\' 'u' HexDigit HexDigit HexDigit HexDigit;
fragment HexDigit:          [0-9a-fA-F];

fragment StringFragment: ('L' | '@')? '"' (EscapeSequence | '\\' ('\r'? '\n' | '\r') | ~('\\'|'"') )* ('"' | '\\');

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
