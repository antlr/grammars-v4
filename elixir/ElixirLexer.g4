lexer grammar ElixirLexer;

COMMENT : '#' ~[\r\n]* -> skip;

NL : '\r'? '\n' | '\r';

SPACES : [ \t]+ -> channel(HIDDEN);

ATOM
 : ':' ( [\p{L}_] [\p{L}_0-9@]* [!?]?
       | OPERATOR
       | SINGLE_LINE_STRING
       | SINGLE_LINE_CHARLIST
       )
 ;
// Keywords
TRUE : 'true';
FALSE : 'false';
NIL : 'nil';
WHEN : 'when';
AND : 'and';
OR : 'or';
NOT : 'not';
IN : 'in';
FN : 'fn';
DO : 'do';
END : 'end';
CATCH : 'catch';
RECUE : 'rescue';
AFTER : 'after';
ELSE : 'else';

// Non-keywords, which also need to be included in the `variable` parser rule
CASE : 'case';
COND : 'cond';
IF : 'if';
UNLESS : 'unless';
DEFMODULE : 'defmodule';
DEFMACRO : 'defmacro';
DEF : 'def';
DEFP : 'defp';
FOR : 'for';
WITH : 'with';
TRY : 'try';

CODEPOINT : '?' ( '\\' . | ~[\\] ) ;

SIGIL
 : '~' [a-zA-Z] ( '/' ( ESCAPE | '\\/' | ~[/\\] )* '/'
                | '|' ( ESCAPE | '\\|' | ~[|\\] )* '|'
                | SINGLE_LINE_STRING
                | MULTI_LINE_STRING
                | SINGLE_LINE_CHARLIST
                | MULTI_LINE_CHARLIST
                | '\'' ( ESCAPE | '\\\'' | ~['\\] )* '\''
                | '(' ( ESCAPE | '\\)' | ~[)\\] )* ')'
                | '[' ( ESCAPE | '\\]' | ~[\]\\] )* ']'
                | '{' ( ESCAPE | '\\}' | ~[}\\] )* '}'
                | '<' ( ESCAPE | '\\>' | ~[>\\] )* '>'
                )
                [a-zA-Z]*
 ;

HEXADECIMAL : '0x' HEX+ ( '_' HEX+ )*;
OCTAL : '0o' [0-7]+ ( '_' [0-7]+ )*;
BINARY : '0b' [01]+ ( '_' [01]+ )*;
INTEGER : D+ ( '_' D+ )*;
FLOAT : D+ '.' D+ EXPONENT?;

SINGLE_LINE_STRING : '"' ( ESCAPE | '\\"' | ~[\\"] )* '"';
MULTI_LINE_STRING : '"""' .*? '"""';
SINGLE_LINE_CHARLIST : '\'' ( ESCAPE | '\\\'' | ~[\\'] )* '\'';
MULTI_LINE_CHARLIST : '\'\'\'' .*? '\'\'\'';

ALIAS : [A-Z] [a-zA-Z_0-9]*;

VARIABLE : [\p{Ll}_] [\p{L}_0-9]* [!?]?;

AT : '@';
DOT : '.';
EXCL : '!';
CARET : '^';
TILDE3 : '~~~';
MUL : '*';
DIV : '/';
ADD : '+';
SUB : '-';
ADD2 : '++';
SUB2 : '--';
ADD3 : '+++';
SUB3 : '---';
DOT2 : '..';
LTGT : '<>';
PIPE_GT : '|>';
LT3 : '<<<';
GT3 : '>>>';
GT2_TILDE: '<<~';
TILDE_GT2: '~>>';
LT_TILDE: '<~';
TILDE_GT: '~>';
LT_TILDE_GT : '<~>';
LT_PIPE_GT : '<|>';
LT : '<';
GT : '>';
LT_EQ : '<=';
GT_EQ : '>=';
EQ2 : '==';
EXCL_EQ : '!=';
EQ_TILDE : '=~';
EQ3 : '===';
EXCL_EQ2 : '!==';
AMP2 : '&&';
AMP3 : '&&&';
PIPE2 : '||';
PIPE3 : '|||';
EQ : '=';
AMP : '&';
ARROW : '=>';
PIPE : '|';
COL2 : '::';
LARROW : '<-';
RARROW : '->';
BSLASH2 : '\\\\';

OPAR : '(';
CPAR : ')';
OBRACK : '[';
CBRACK : ']';
OBRACE : '{';
CBRACE : '}';
LT1 : '<<';
GT2 : '>>';
OMAP : '%' ( [a-zA-Z_.] [a-zA-Z_.0-9]* )? '{';
COMMA : ',';
COL : ':';
SCOL : ';';

fragment OPERATOR
 : AT | DOT | EXCL | CARET | SUB3 | MUL | DIV | ADD | SUB | ADD2 | SUB2 | ADD3 | DOT2 | LTGT | PIPE_GT
 | LT3 | GT3 | GT2_TILDE | TILDE_GT2 | LT_TILDE | TILDE_GT | LT_TILDE_GT | LT_PIPE_GT | LT | GT | LT_EQ
 | GT_EQ | EQ2 | EXCL_EQ | EQ_TILDE | EQ3 | EXCL_EQ2 | AMP2 | AMP3 | PIPE2 | PIPE3 | EQ | AMP | PIPE
 | COL2 | LARROW | RARROW | BSLASH2
 ;

fragment D : [0-9];
fragment HEX : [0-9a-fA-F];
fragment EXPONENT : [eE] [+-]? D+;

fragment ESCAPE
 : '\\' ( [\\abdefnrstv0]
        | 'x' HEX HEX
        | 'u' HEX HEX HEX HEX
        | 'u{' HEX+ '}'
        )
 ;