/*
Objective-C grammar.
The MIT License (MIT).
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

channels { COMMENTS_CHANNEL }

// §3.9 Keywords

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

SUPER           : 'super';
SELF            : 'self';

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
VA_ARG        : 'va_arg';
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

NULLABLE            : 'nullable';

L_STR               : 'L';

// §3.11 Separators

LPAREN          : '(';
RPAREN          : ')';
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

IDENTIFIER
  : LETTER (LETTER | [0-9])*
  ;

CHARACTER_LITERAL:        '\'' (EscapeSequence | ~('\''|'\\')) '\'';
STRING: StringFragment;
QUOTE_STRING:             '\'' (EscapeSequence | ~('\''|'\\'))* '\'';

HEX_LITERAL : '0' [xX] HexDigit+ IntegerTypeSuffix? ;

DECIMAL_LITERAL : ('0' | '1'..'9' '0'..'9'*) IntegerTypeSuffix? ;

OCTAL_LITERAL : '0' ('0'..'7')+ IntegerTypeSuffix? ;

FLOATING_POINT_LITERAL
  : ([0-9]+ '.' [0-9]* | '.' [0-9]+) Exponent? FloatTypeSuffix? | [0-9]+ (Exponent FloatTypeSuffix? | FloatTypeSuffix)
  ;

WS:           [ \r\n\t\u000C]+ -> channel(HIDDEN);
COMMENT:      '/*' .*? '*/'    -> channel(COMMENTS_CHANNEL);
LINE_COMMENT: '//' ~[\r\n]*    -> channel(COMMENTS_CHANNEL);
BACKSLASH:    '\\'             -> channel(HIDDEN);

fragment LETTER
    : [$A-Za-z_]
    | ~[\u0000-\u00FF\uD800-\uDBFF]
    | [\uD800-\uDBFF] [\uDC00-\uDFFF]
    | [\u00E9]
    ;

fragment IntegerTypeSuffix: [uUlL] [uUlL]?;
fragment Exponent:          [eE] [+\-]? [0-9]+;

fragment
FloatTypeSuffix: [fFdD];

fragment
EscapeSequence
    : '\\' ('b'|'t'|'n'|'f'|'r'|'\"'|'\''|'\\')
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

fragment
ANGLE_STRING:   '<' .*? '>';

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
