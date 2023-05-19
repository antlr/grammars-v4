/*
 [The "BSD licence"]
 Copyright (c) 2018 Mario Schroeder
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:
 1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.
 3. The name of the author may not be used to endorse or promote products
    derived from this software without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

/*
 A grammar for Hyperledger Composer Modeling Language
 https://hyperledger.github.io/composer/v0.19/reference/cto_language.html
 */

lexer grammar CtoLexer;

// Keywords
ABSTRACT:           'abstract';
ASSET:              'asset';
CONCEPT:            'concept';
DEFAULT:            'default';
ENUM:               'enum';
EVENT:              'event';
EXTENDS:            'extends';
IDENTIFIED:         'identified by';
IMPORT:             'import';
NAMESPACE:          'namespace';
OPTIONAL:           'optional';
PARTICIPANT:        'participant';
RANGE:              'range';
REGEX:              'regex';
TRANSACTION:        'transaction';

//primitive types
BOOLEAN:            'Boolean';
DATE_TIME:          'DateTime';
DOUBLE:             'Double';
INTEGER:            'Integer';
LONG:               'Long';
STRING:             'String';

// Separators
LPAREN:             '(';
RPAREN:             ')';
LBRACE:             '{';
RBRACE:             '}';
LBRACK:             '[';
RBRACK:             ']';
SEMI:               ';';
COMMA:              ',';
DOT:                '.';
COLON:              ':';

// Operators
ASSIGN:             '=';
MUL:                '*';

// Additional symbols
AT:                 '@';
ELLIPSIS:           '...';
REF:                '--> ';
VAR:                'o ';

// Literals
DECIMAL_LITERAL:    ('0' | [1-9] (Digits? | '_'+ Digits)) [lL]?;
OCT_LITERAL:        '0' '_'* [0-7] ([0-7_]* [0-7])? [lL]?;
FLOAT_LITERAL:      (Digits '.' Digits? | '.' Digits) ExponentPart? [fFdD]?
             |       Digits (ExponentPart [fFdD]? | [fFdD])
             ;
BOOL_LITERAL:       'true'
            |       'false'
            ;
DATE_TIME_LITERAL: Bound FullDate 'T' FullTime Bound;

// Whitespace and comments
WS:                 [ \t\r\n\u000C]+ -> skip;
LINE_COMMENT:       '//' ~[\r\n]*    -> channel(HIDDEN);
COMMENT:            '/*' .*? '*/'    -> channel(HIDDEN);

//REGEX Expr
REGEX_EXPR:         '/'.*?'/';

fragment Bound: '"' | '\'';
fragment FullDate: Year '-' Month '-' Day;
fragment Year: Digit Digit Digit Digit;
fragment Month: [0][0-9]|[1][0-2];
fragment Day: [0-2][0-9]|[0-3][01];

fragment FullTime 
    : PartialTime TimeOffset;
fragment TimeOffset
    : 'Z' | TimeNumOffset;
fragment TimeNumOffset 
    : '-' [01][0-2] (':' HalfHour)?
    | '+' [01][0-5] (':' (HalfHour | [4][5]))?
    ;
fragment HalfHour: [0][0] | [3][0];
fragment PartialTime 
    : [0-2][0-3] ':' Sixty ':' Sixty ('.' [0-9]*)?;
fragment Sixty: [0-5] Digit;
fragment Digit: [0-9];


IDENTIFIER:         LetterOrDigit+;

CHAR_LITERAL:       '\'' (~["\\\r\n] | EscapeSequence)* '\'';
STRING_LITERAL:     '"' (~["\\\r\n] | EscapeSequence)* '"';

// Fragment rules
fragment ExponentPart
    : [eE] [+-]? Digits
    ;

fragment EscapeSequence
    : '\\' [btnfr"'\\]
    | '\\' ([0-3]? [0-7])? [0-7]
    | '\\' 'u'+ HexDigit HexDigit HexDigit HexDigit
    ;

fragment HexDigits
    : HexDigit ((HexDigit | '_')* HexDigit)?
    ;

fragment HexDigit
    : [0-9a-fA-F]
    ;

fragment Digits
    : [0-9] ([0-9_]* [0-9])?
    ;

fragment LetterOrDigit
    : Letter
    | [0-9]
    ;

fragment Letter
    : [a-zA-Z$_] // these are below 0x7F
    | ~[\u0000-\u007F\uD800-\uDBFF] // covers all characters above 0x7F which are not a surrogate
    | [\uD800-\uDBFF] [\uDC00-\uDFFF] // covers UTF-16 surrogate pairs encodings for U+10000 to U+10FFFF
    ;
