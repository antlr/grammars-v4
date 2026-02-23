lexer grammar PlantUMLLexer;

ENUM: 'enum';
CLASS: 'class';
STARTUML: '@startuml';
ENDUML: '@enduml';
INTERFACE: 'interface';
ABSTRACT: 'abstract';
MANY: 'many';
ONE: 'one';
EXTENDS: 'extends';
STATIC: 'static';
LIST: 'List';

GT: '>';
LT: '<';
PLUS: '+';
DASH: '-';
DOT: '.';
DOUBLE_DOT: '..';
STAR: '*';
O: 'o';
CARET: '^';
PIPE_GT: '|>';
LT_PIPE: '<|';
COMMA: ',';
LPAREN: '(';
RPAREN: ')';
LBRACK: '[';
RBRACK: ']';
AMP: '#';
EQUALS: '=';
NULL_LITERAL: 'null' ;
CLASS_BODY_START: '{' -> pushMode(BODY);

STEREOTYPE_TEXT: '<<' ~[\r\n>]* '>>';
ASSOC_DETAIL: '"' ~[\r\n"]* '"';
NOTE: ('note' ~[\r\n]*) ->channel(HIDDEN);
DIAMOND: ('<>' ~[\r\n]*) ->channel(HIDDEN);

AFTER_COLON_TEXT
    : ':' ~[\r\n]* // Match ':' followed by any characters except line breaks
    ;

NEWLINE: '\r'? '\n';
WS  : [ \t]+ -> skip ;
COMMENT : ('/' '/' ~[\r\n]* | '/*' .*? '*/' | '\'' ~[\r\n]* ) -> channel(HIDDEN);

FLOAT_LITERAL: [0-9]+ '.' [0-9]+ ;
NUMBER: [0-9]+ ;

IDENT: [a-zA-Z] [a-zA-Z0-9_]* ;


mode BODY;
BODY_INLINE_BRACES: '{' ~[{}\r\n]+ '}';
BODY_OPEN: '{';
BODY_CLOSE: '}' -> popMode;
BODY_CONTENT: ~[{}\r\n]+;
BODY_NL: [\r\n];

mode ENUM_FREE_TEXT;
ENUM_FREE_TEXT_CLOSE: '>>' -> popMode;
ENUM_FREE_TEXT_CONTENT : ~[\r\n>]+ ;

mode ASSOC_DETAIL_TEXT;
ASSOC_DETAIL_TEXT_CLOSE: '"' -> popMode;
ASSOC_DETAIL_TEXT_CONTENT : ~[\r\n"]+ ;
