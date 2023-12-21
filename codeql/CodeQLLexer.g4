/*
 CodeQL grammar
 The MIT License (MIT).
 Copyright (c) 2023, Dijun Liu

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

 CodeQL grammar built from the CodeQL Specification https://codeql.github.com/docs/ql-language-reference/ql-language-specification
*/

lexer grammar CodeQLLexer;

// Keywords
AND:   'and';
ANY:   'any';
AS:   'as';
ASC:   'asc';
AVG:   'avg';
BOOLEAN:   'boolean';
BY:   'by';
CLASS:   'class';
CONCAT:   'concat';
COUNT:   'count';
DATE:   'date';
DESC:   'desc';
ELSE:   'else';
EXISTS:   'exists';
EXTENDS:   'extends';
FALSE:   'false';
FLOAT:   'float';
FORALL:   'forall';

FOREX:   'forex';
FROM:   'from';
IF:   'if';
IMPLIES:   'implies';
IMPORT:   'import';
IN:   'in';
INSTANCEOF:   'instanceof';
INT:   'int';
MAX:   'max';
MIN:   'min';
MODULE:   'module';
NEWTYPE:   'newtype';
NONE:   'none';
NOT:   'not';
OR:   'or';
ORDER:   'order';
PREDICATE:   'predicate';
RANK:   'rank';
RESULT:   'result';
SELECT:   'select';
STRICTCONCAT:   'strictconcat';
STRICTCOUNT:   'strictcount';
STRICTSUM:   'strictsum';
STRING:   'string';
SUM:   'sum';
SUPER:   'super';
THEN:   'then';
THIS:   'this';
TRUE:   'true';
UNIQUE:   'unique';
WHERE:   'where';

IMPLEMENTS:            'implements';
SIGNATURE:             'signature';
DEFAULT:               'default';
ABSTRACT:              'abstract';
CACHED:                'cached';
EXTERNAL:              'external';
EXTENSIBLE:            'extensible';
FINAL:                 'final';
TRANSIENT:             'transient';
LIBRARY:               'library';
PRIVATE:               'private';
DEPRECATED:            'deprecated';
OVERRIDE:              'override';
ADDITIONAL:            'additional';
QUERY:                 'query';
PRAGMA:                'pragma';
INLINE:                'inline';
INLINE_LATE:           'inline_late';
NOINLINE:              'noinline';
NOMAGIC:               'nomagic';
NOOPT:                 'noopt';
ASSUME_SMALL_DELTA:    'assume_small_delta';
LANGUAGE:              'language';
MONOTONICAGGREGATES:   'monotonicAggregates';
BINDINGSET:            'bindingset';
ONLY_BIND_OUT:         'only_bind_out';
ONLY_BIND_INTO:        'only_bind_into';

//operator
LT:                 '<';
LE:                 '<=';
ASSIGN:             '=';
GT:                 '>';
GE:                 '>=';
SUB:                '-';
DONTCARE:           '_';
COMMA:              ',';
SEMI:               ';';
NOTEQUAL:           '!=';
DIV:                '/';
DOT:                '.';
DOTDOT:             '..';
LPAREN:             '(';
RPAREN:             ')';
LBRACE:             '{';
RBRACE:             '}';
LBRACK:             '[';
RBRACK:             ']';
MUL:                '*';
MOD:                '%';
ADD:                '+';
BITOR:              '|';
COLONCOLON:         '::';



// Literals
INT_LITERAL :     [0-9]+;


FLOAT_LITERAL:      INT_LITERAL ('.' INT_LITERAL)? ;

BOOL_LITERAL:       TRUE
            |       FALSE
            ;

STRING_LITERAL:     '"' (~["\\\r\n] | EscapeSequence)* '"';

AT:                 '@';
// Additional symbols not defined in the lexical specification
ELLIPSIS:           '...';

// Whitespace and comments

WS:                 [ \t\r\n\u000C]+ -> channel(HIDDEN);
COMMENT:            '/*' ~[*]*?.*? '*/'    -> channel(HIDDEN);
QL_DOC:            '/**' .*? '*/'    -> channel(HIDDEN);
LINE_COMMENT:       '//' ~[\r\n]*    -> channel(HIDDEN);

// Identifiers

// 将 LowerId 和 UpperId 定义为词法规则

LOWERID: [a-z][a-zA-Z0-9_]* ;
UPPERID: [A-Z][a-zA-Z0-9_]* ;
// 将 AtLowerId 定义为词法规则
ATLOWERID: '@' LOWERID ;



fragment EscapeSequence
    : '\\' 'u005c'? [btnfr"'\\]
    | '\\' 'u005c'? ([0-3]? [0-7])? [0-7]
    ;

