lexer grammar FSharpLexer;

WHITE_SPACE: [ \t\r\n]+ -> channel(HIDDEN) ;
LINECOMMENT: '//' ~[\r\n]* -> channel(HIDDEN);
BLOCKCOMMENT: ('(*' .*? '*)') -> channel(HIDDEN);

INT: [0-9]+('uy'|'y'|'s'|'us'|'u'|'L')?;

FLOAT: [0-9]+'.'[0-9]+ ('f'|'m')?;

INTERPOLATIONSIGN: '%s'|'%d'|'%f'|'%c';

CHAR: '\''(~[\\']|'\\'['"nt\\]|'\\u'[0-9a-fA-F][0-9a-fA-F][0-9a-fA-F][0-9a-fA-F])'\'';

STRING: DOUBLE_QUOTES (~[\\"]|'\\'[\\"'unt])* DOUBLE_QUOTES;

INTERPOLATED_STRING: DOLLAR STRING;

BOOL: 'true' | 'false';

UNIT: '(' ' '* ')';

REC: 'rec';

PUBLIC: 'public';

PRIVATE: 'private';

INTERNAL: 'internal';

MUTABLE: 'mutable';

LET: 'let';

FUN: 'fun';

WHILE: 'while';

DO: 'do';

FOR: 'for';

TO: 'to';

DOWNTO: 'downto';

IN: 'in';

DOLLAR: '$';

TYPE: 'type';

MODULE: 'module';

OPEN: 'open';

NAMESPACE: 'namespace';

CLASS: 'class';

END: 'end';

STRUCT: 'struct';

WITH_AND: 'and';

INTERFACE: 'interface';

GET: 'get';

INHERIT: 'inherit';

OVERRIDE: 'override';

DEFAULT: 'default';

ABSTRACT: 'abstract';

BASE: 'base';

ASYNC: 'async';

TASK: 'task';

NEW: 'new';

THEN: 'then';

THIS: 'this';

MEMBER: 'member';

SEQ: 'seq';

MAP: 'Map';

SET: 'set';

RAISE: 'raise';

RERAISE: 'reraise';

FAILWITH: 'failwith';

INVALIDARG: 'invalidArg';

VAL: 'val';

TRY: 'try';

FINALLY: 'finally';

MATCH: 'match';

WITH: 'with';

USE: 'use';

USING: 'using';

WHEN: 'when';

EXCEPTION: 'exception';

OF: 'of';

DOT: '.';

DOTDOT: '..';

EXCLAMATION_MARK: '!';

COMMA: ',';

SEMICOLON: ';';

COLON: ':';

ASSIGN: '<-';

PIPE: '|>';

MISSING_ARG: '_';

RIGHT_ARROW: '->';

COMPOS: '>>';

ADD: '+';

MINUS: '-';

MUL: '*';

DIV: '/';

POW: '**';

MOD: '%';

EQUAL: '=';

NOT_EQUAL: '<>';

LESS: '<';

GREATER: '>';

LESS_EQUAL: '<=';

GREATER_EQUAL: '>=';

AND:'&&';

OR:'||';

LSHIFT: '<<<';

RSHIFT: '>>>';

LOG_MUL: '&&&';

LOG_ADD: '|||';

LOG_XOR: '^^^';

LOG_NOT: '~~~';

NOT: 'not';

COLON_Q: ':?';

OPEN_PAREN: '(';

CLOSE_PAREN: ')';

OPEN_BRACE: '{';

CLOSE_BRACE: '}';

OPEN_BRACKET: '[';

CLOSE_BRACKET: ']';

DOUBLE_QUOTES: '"';

IF: 'if';

ELIF: 'elif';

ELSE: 'else';

VERTICAL_LINE: '|';

IDENTIFIER: [a-zA-Z_0-9][a-zA-Z_0-9]*;

SINGLE_CHARACTER: [.];
