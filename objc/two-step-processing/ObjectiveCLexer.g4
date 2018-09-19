/*
Objective-C grammar.
The MIT License (MIT).
Copyright (c) 2016-2017, Ivan Kochurkin (kvanttt@gmail.com).
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

channels { COMMENTS_CHANNEL, IGNORED_MACROS }

// Words you can't use

AUTO:                     'auto';
BREAK:                    'break';
CASE:                     'case';
CHAR:                     'char';
CONST:                    'const';
CONTINUE:                 'continue';
DEFAULT:                  'default';
DO:                       'do';
DOUBLE:                   'double';
ELSE:                     'else';
ENUM:                     'enum';
EXTERN:                   'extern';
FLOAT:                    'float';
FOR:                      'for';
GOTO:                     'goto';
IF:                       'if';
INLINE:                   'inline';
INT:                      'int';
LONG:                     'long';
REGISTER:                 'register';
RESTRICT:                 'restrict';
RETURN:                   'return';
SHORT:                    'short';
SIGNED:                   'signed';
SIZEOF:                   'sizeof';
STATIC:                   'static';
STRUCT:                   'struct';
SWITCH:                   'switch';
TYPEDEF:                  'typedef';
UNION:                    'union';
UNSIGNED:                 'unsigned';
VOID:                     'void';
VOLATILE:                 'volatile';
WHILE:                    'while';
BOOL_:                    '_Bool';
COMPLEX:                  '_Complex';
IMAGINERY:                '_Imaginery';
TRUE:                     'true';
FALSE:                    'false';

// Words you shouldn't use

BOOL:                     'BOOL';
Class:                    'Class';
BYCOPY:                   'bycopy';
BYREF:                    'byref';
ID:                       'id';
IMP:                      'IMP';
IN:                       'in';
INOUT:                    'inout';
NIL:                      'nil';
NO:                       'NO';
NULL:                     'NULL';
ONEWAY:                   'oneway';
OUT:                      'out';
PROTOCOL_:                'Protocol';
SEL:                      'SEL';
SELF:                     'self';
SUPER:                    'super';
YES:                      'YES';
AUTORELEASEPOOL:          '@autoreleasepool';
CATCH:                    '@catch';
CLASS:                    '@class';
DYNAMIC:                  '@dynamic';
ENCODE:                   '@encode';
END:                      '@end';
FINALLY:                  '@finally';
IMPLEMENTATION:           '@implementation';
INTERFACE:                '@interface';
IMPORT:                   '@import';
PACKAGE:                  '@package';
PROTOCOL:                 '@protocol';
OPTIONAL:                 '@optional';
PRIVATE:                  '@private';
PROPERTY:                 '@property';
PROTECTED:                '@protected';
PUBLIC:                   '@public';
REQUIRED:                 '@required';
SELECTOR:                 '@selector';
SYNCHRONIZED:             '@synchronized';
SYNTHESIZE:               '@synthesize';
THROW:                    '@throw';
TRY:                      '@try';
ATOMIC:                   'atomic';
NONATOMIC:                'nonatomic';
RETAIN:                   'retain';

// Attributes with `__` prefix

ATTRIBUTE:                '__attribute__';
AUTORELEASING_QUALIFIER:  '__autoreleasing';
BLOCK:                    '__block';
BRIDGE:                   '__bridge';
BRIDGE_RETAINED:          '__bridge_retained';
BRIDGE_TRANSFER:          '__bridge_transfer';
COVARIANT:                '__covariant';
CONTRAVARIANT:            '__contravariant';
DEPRECATED:               '__deprecated';
KINDOF:                   '__kindof';
STRONG_QUALIFIER:         '__strong';
TYPEOF:                   'typeof' | '__typeof' | '__typeof__';
UNSAFE_UNRETAINED_QUALIFIER:'__unsafe_unretained';
UNUSED:                   '__unused';
WEAK_QUALIFIER:           '__weak';

// Nullability specifiers

NULL_UNSPECIFIED:         'null_unspecified' | '__null_unspecified' | '_Null_unspecified';
NULLABLE:                 'nullable' | '__nullable' | '_Nullable';
NONNULL:                  'nonnull' | '__nonnull' | '_Nonnull';
NULL_RESETTABLE:          'null_resettable';

// NS prefix

NS_INLINE:                'NS_INLINE';
NS_ENUM:                  'NS_ENUM';
NS_OPTIONS:               'NS_OPTIONS';

// Property attributes

ASSIGN:                   'assign';
COPY:                     'copy';
GETTER:                   'getter';
SETTER:                   'setter';
STRONG:                   'strong';
READONLY:                 'readonly';
READWRITE:                'readwrite';
WEAK:                     'weak';
UNSAFE_UNRETAINED:        'unsafe_unretained';

// Interface Builder attributes

IB_OUTLET:                'IBOutlet';
IB_OUTLET_COLLECTION:     'IBOutletCollection';
IB_INSPECTABLE:           'IBInspectable';
IB_DESIGNABLE:            'IB_DESIGNABLE';

// Ignored macros

NS_ASSUME_NONNULL_BEGIN:  'NS_ASSUME_NONNULL_BEGIN' ~[\r\n]*  -> channel(IGNORED_MACROS);
NS_ASSUME_NONNULL_END:    'NS_ASSUME_NONNULL_END' ~[\r\n]*    -> channel(IGNORED_MACROS);
EXTERN_SUFFIX:            [_A-Z]+ '_EXTERN'                   -> channel(IGNORED_MACROS);
IOS_SUFFIX:               [_A-Z]+ '_IOS(' ~')'+ ')'           -> channel(IGNORED_MACROS);
MAC_SUFFIX:               [_A-Z]+ '_MAC(' ~')'+ ')'           -> channel(IGNORED_MACROS);
TVOS_PROHIBITED:          '__TVOS_PROHIBITED'                 -> channel(IGNORED_MACROS);

// Identifier

IDENTIFIER:               Letter LetterOrDec*;

// Separators

LP:                       '(';
RP:                       ')';
LBRACE:                   '{';
RBRACE:                   '}';
LBRACK:                   '[';
RBRACK:                   ']';
SEMI:                     ';';
COMMA:                    ',';
DOT:                      '.';
STRUCTACCESS:             '->';
AT:                       '@';

// Operators

ASSIGNMENT:               '=';
GT:                       '>';
LT:                       '<';
BANG:                     '!';
TILDE:                    '~';
QUESTION:                 '?';
COLON:                    ':';
EQUAL:                    '==';
LE:                       '<=';
GE:                       '>=';
NOTEQUAL:                 '!=';
AND:                      '&&';
OR:                       '||';
INC:                      '++';
DEC:                      '--';
ADD:                      '+';
SUB:                      '-';
MUL:                      '*';
DIV:                      '/';
BITAND:                   '&';
BITOR:                    '|';
BITXOR:                    '^';
MOD:                      '%';

// Assignments

ADD_ASSIGN:               '+=';
SUB_ASSIGN:               '-=';
MUL_ASSIGN:               '*=';
DIV_ASSIGN:               '/=';
AND_ASSIGN:               '&=';
OR_ASSIGN:                '|=';
XOR_ASSIGN:               '^=';
MOD_ASSIGN:               '%=';
LSHIFT_ASSIGN:            '<<=';
RSHIFT_ASSIGN:            '>>=';
ELIPSIS:                  '...';

// Literals

CHARACTER_LITERAL:        '\'' (EscapeSequence | ~('\'' | '\\')) '\'';
STRING_START:             StringStart -> mode(STRING_MODE);

HEX_LITERAL:              '0' [xX] HexDigit+ IntegerTypeSuffix?;
OCTAL_LITERAL:            '0' [0-7]+ IntegerTypeSuffix?;
BINARY_LITERAL:           '0' [bB] [01]+ IntegerTypeSuffix?;
DECIMAL_LITERAL:          [0-9]+ IntegerTypeSuffix?;

FLOATING_POINT_LITERAL
                        : (Dec+ '.' Dec* | '.' Dec+) Exponent? FloatTypeSuffix?
                        | Dec+ (Exponent FloatTypeSuffix? | FloatTypeSuffix)
                        ;

// Comments and whitespaces

WS:                       Ws+             -> channel(HIDDEN);
MULTI_COMMENT:            '/*' .*? '*/'   -> channel(COMMENTS_CHANNEL);
SINGLE_COMMENT:           '//' ~[\r\n]*   -> channel(COMMENTS_CHANNEL);
BACKSLASH:                '\\'            -> channel(HIDDEN);

// Strings

mode STRING_MODE;

STRING_NEWLINE:   '\\' '\r'? '\n'         -> channel(DEFAULT_TOKEN_CHANNEL);
STRING_ESCAPE:    EscapeSequence          -> channel(DEFAULT_TOKEN_CHANNEL), type(STRING_VALUE);
STRING_END:       '"'                     -> channel(DEFAULT_TOKEN_CHANNEL), mode(DEFAULT_MODE);
STRING_VALUE:     ~["\\]+                 -> channel(DEFAULT_TOKEN_CHANNEL);

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

fragment IntegerTypeSuffix: [uUlL] [uUlL]? [uUlL]?;
fragment Exponent:          [eE] [+\-]? Dec+;
fragment Dec:               [0-9];
fragment FloatTypeSuffix:   [fFdD];

fragment StringStart:       (('L' | '@') Ws*)? '"';

fragment EscapeSequence
    : '\\' ('b' | 't' | 'n' | 'f' | 'r' | '"' | '\'' | '\\')
    | OctalEscape
    | UnicodeEscape
    ;

fragment OctalEscape
    :   '\\' [0-3] [0-7] [0-7]
    |   '\\' [0-7] [0-7]
    |   '\\' [0-7]
    ;

fragment UnicodeEscape:   '\\' 'u' HexDigit HexDigit HexDigit HexDigit;
fragment HexDigit:        [0-9a-fA-F];

fragment Ws: [ \r\n\t\u000C];
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