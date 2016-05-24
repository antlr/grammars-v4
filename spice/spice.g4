
grammar spice;

netlist
   : (line? CR) +
   ;

line
   : (node | simulationcommand | END)
   ;

node
   : NAME vertexID vertexID + (expression | parameter)*
   ;

vertexID
   : number
   ;

parameter
   : NAME
   ;

simulationcommand
   : dotdc
   ;

dotdc
   : '.DC' VNODENAME expression +
   ;

expression
   : multiplyingExpression ((PLUS | MINUS) multiplyingExpression)*
   ;

multiplyingExpression
   : powExpression ((TIMES | DIV) powExpression)*
   ;

powExpression
   : atom (POW expression)?
   ;

atom
   : value
   | LPAREN expression RPAREN
   ;

value
   : scientific
   ;

// a number with a potential metrix suffix and a potential exponent
scientific
   : NUMBER METRIC_PREFIX? (E NUMBER METRIC_PREFIX?)?
   ;

number
   : NUMBER
   ;


NUMBER
   : MINUS? DIGIT* (POINT DIGIT +)?
   ;


LPAREN
   : '('
   ;


RPAREN
   : ')'
   ;


PLUS
   : '+'
   ;


MINUS
   : '-'
   ;


TIMES
   : '*'
   ;


DIV
   : '/'
   ;


EQ
   : '='
   ;


POINT
   : '.'
   ;


METRIC_PREFIX
   : ('a' .. 'z' | 'A' .. 'Z')
   ;


NAME
   : ('a' .. 'z' | 'A' .. 'Z') ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9')*
   ;


fragment DIGIT
   : ('0' .. '9')
   ;


END
   : '.END' | '.end'
   ;


COMMENT_1
   : '*' ~ [\r\n]* -> skip
   ;


COMMENT_2
   : ';' ~ [\r\n]* -> skip
   ;


CR
   : [\r\n]
   ;


WS
   : [ \t] + -> channel (HIDDEN)
   ;
