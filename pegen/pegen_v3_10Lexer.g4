// Derived from https://www.python.org/dev/peps/pep-0617/
// Tokens assumed to be derived from https://raw.githubusercontent.com/python/cpython/3.10/Grammar/Tokens
// Ken Domino, 2 Sep 2021

lexer grammar pegen_v3_10Lexer;

options { superClass = Adaptor; }
tokens { ACTION }

MEMO: 'memo';
OP: 'op';
NAME: LETTER ('_' | DIGIT | LETTER)* ;
fragment LETTER  : CAPITAL | SMALL ;
fragment CAPITAL : [A-Z\u00C0-\u00D6\u00D8-\u00DE] ;
fragment SMALL   : [a-z\u00DF-\u00F6\u00F8-\u00FF] ;
fragment DIGIT   : [0-9] ;
NUMBER : DIGIT+;
STRING : '"""' .*? '"""' | '\'\'\'' .*? '\'\'\'' ;
STRING2 :  '"' -> more, mode(STRINGMODE);
CHAR : '\''   -> more, mode(CHARMODE);
fragment NEWLINE : [\n\r]+ ;
LPAR :                  '(';
RPAR :                  ')';
LSQB :                  '[';
RSQB :                  ']';
COLON :                 ':';
COMMA :                 ',';
SEMI :                  ';';
PLUS :                  '+';
MINUS :                 '-';
STAR :                  '*';
SLASH :                 '/';
VBAR :                  '|';
AMPER :                 '&';
LESS :                  '<';
GREATER :               '>';
EQUAL :                 '=';
DOT :                   '.';
PERCENT :               '%';
LBRACE :                '{' -> more, pushMode(ACTION_MODE) ;
RBRACE :                '}';
EQEQUAL :               '==';
NOTEQUAL :              '!=';
LESSEQUAL :             '<=';
GREATEREQUAL :          '>=';
TILDE :                 '~';
CIRCUMFLEX :            '^';
LEFTSHIFT :             '<<';
RIGHTSHIFT :            '>>';
DOUBLESTAR :            '**';
PLUSEQUAL :             '+=';
MINEQUAL :              '-=';
STAREQUAL :             '*=';
SLASHEQUAL :            '/=';
PERCENTEQUAL :          '%=';
AMPEREQUAL :            '&=';
VBAREQUAL :             '|=';
CIRCUMFLEXEQUAL :       '^=';
LEFTSHIFTEQUAL :        '<<=';
RIGHTSHIFTEQUAL :       '>>=';
DOUBLESTAREQUAL :       '**=';
DOUBLESLASH :           '//';
DOUBLESLASHEQUAL :      '//=';
AT :                    '@';
ATEQUAL :               '@=';
RARROW :                '->';
ELLIPSIS :              '...';
COLONEQUAL :            ':=';
DOLLAR: '$';
BANG: '!';
QUESTION: '?';

SKIP_
 : ( SPACES | COMMENT | LINE_JOINING | NEWLINE ) -> skip
 ;

fragment SPACES
 : [ \t]+
 ;

fragment COMMENT
 : '#' ~[\r\n\f]*
 ;

fragment LINE_JOINING
 : '\\' SPACES? ( '\r'? '\n' | '\r' | '\f')
 ;

// Escapable sequences
fragment
Escapable : ('"' | '\\' | 'n' | 't' | 'r' | 'f' | '\n' | '\r');

mode STRESCAPE;
STRESCAPED : Escapable  -> more, popMode ;

mode STRINGMODE;
STRINGESC : '\\' -> more , pushMode(STRESCAPE);
STRINGEND : '"' ->  type(STRING), mode(DEFAULT_MODE);
STRINGTEXT : (~["\\] | '""') -> more;

mode CHARESCAPE;
CHARESCAPED : Escapable  -> more, popMode ;

mode CHARMODE;
CHARESC : '\\' -> more , pushMode(CHARESCAPE);
CHAREND : '\'' ->  type(STRING), mode(DEFAULT_MODE);
CHARTEXT : ~['\\] -> more;

mode ACTION_MODE;
NESTED_ACTION : LBRACE -> more, pushMode (ACTION_MODE) ;
ACTION_ESCAPE : EscAny -> more ;
ACTION : RBRACE { this.AtEnd() }? -> popMode ;
CLOSE : RBRACE -> more, popMode ;
UNTERMINATED_ACTION : EOF -> popMode ;
CONTENT : . -> more ;
fragment EscAny : Esc . ;
fragment Esc : '\\' ;

