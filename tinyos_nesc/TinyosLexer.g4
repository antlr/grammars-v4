/*
    This file is the grammar for the nesC of the TinyOS.
    
    This grammar is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.
    You should have received a copy of the GNU Lesser General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/
/*
    Antlr4 TinyOS(nesC) by Hussein Marah, 2020.
*/


lexer grammar TinyosLexer;

//Keywords
@lexer::members {
    public static final int WHITESPACE = 1;
}

ABSTRACT:           'abstract';
AS:                 'as';
ASYNC:              'async';
ATOMIC:             'atomic';
BOOLEAN:            'boolean';
BREAK:              'break';
CALL:               'call';
CASE:               'case';
CHAR:               'char';
COFIGURATION:       'configuration';
COMMAND:            'command'; 
COMPONENT:          'component';
COMPONENTS:         'components';
CONTINUE:           'continue';
DO:                 'do';
DOUBLE:             'double';
DEFINED:            'defined';
DEFINE:             'define';
DEFAULT:            'default'; 
ELSE:               'else';
ENUM:               'enum';
EVENT:              'event';
EXTENDS:            'extends';
ELIF:               'elif';
ENDIF:              'endif';
ERROR:              'error';
FALSE:              'false';
FINAL:              'final';
FOR:                'for';
GENERIC:            'generic';
IF:                 'if';
IMPLEMENTATION:     'implementation';
INCLUDE:            'include';  
INCLUDES:           'includes';
INTERFACE:          'interface';
LOG:                'log';
LONG:               'long';
MODULE:             'module';
NEW:                'new';
POST:               'post';
PROVIDES:           'provides';
RETURN:             'return';
SHORT:              'short';
SIGNAL:             'signal';
STATIC:             'static';
SWITCH:             'switch';
TASK:               'task';
TRUE:               'true';
USES:               'uses';
VOID:               'void';
WHILE:              'while';
TYPEDEF:            'typedef';



OR : '||';
AND : '&&';
EQ : '==';
NEQ : '!=';
GT : '>';
LT : '<';
GTEQ : '>=';
LTEQ : '<=';
PLUS : '+';
MINUS : '-';
MULT : '*';
DIV : '/';
MOD : '%';
POW : '^';
NOT : '!';

ASSIGN:          '=';
TILDE:           '~';
QUESTION:        '?';
COLON:           ':';
INC:             '++';
DEC:             '--';
BITAND:          '&';
BITOR:           '|';
HASHTAG:         '#';




SCOL:            ';';
OBRACK:          '[';
CBRACK:          ']';
OPAR:            '(';
CPAR:            ')';
OBRACE:          '{';
CBRACE:          '}';
FORWARDARROW:    '->';
BACKARROW:       '<-';
COLONCOLON:      '::';
AT:              '@';
COMMA:           ',';
DOT:             '.';
UNDERSCORE:      '_';

ID
: [a-zA-Z_] [a-zA-Z_0-9]*
;

INT
: [0-9]+
;

FLOAT
: [0-9]+ '.' [0-9]* 
| '.' [0-9]+
;

STRING
: '"' (~["\r\n] | '""')* '"'
;

COMMENT
: '/*' .*? '*/'   -> channel(HIDDEN)
;

LINE_COMMENT
: '//' ~[\r\n]*   -> channel(HIDDEN)
;

SPACE
:   (' ' | '\t' | '\r' | '\n')+   -> channel(WHITESPACE)
;

OTHER
: . 
;

HEX
:'0x' ([a-fA-F0-9])+
;