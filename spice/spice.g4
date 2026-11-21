grammar spice;

options { caseInsensitive = true; }

netlist
   : EOL* (title EOL)? (line EOL+)* line? EOF
   ;

title
   : TITLE
   ;

line
   : component
   | directive
   | continuation
   | comment
   ;

component
   : NAME argument+ inlineComment?
   ;

directive
   : DIRECTIVE argument* inlineComment?
   ;

continuation
   : '+' argument+ inlineComment?
   ;

comment
   : '*' ~EOL*
   ;

inlineComment
   : ';' ~EOL*
   ;

argument
   : signedNumber
   | NAME ('=' expression)?
   | NAME '(' arguments? ')'
   | '{' expression '}'
   | STRING
   ;

arguments
   : argument (','? argument)*
   ;

expression
   : product (('+' | '-') product)*
   ;

product
   : power (('*' | '/') power)*
   ;

power
   : unary ('^' power)?
   ;

unary
   : ('+' | '-') unary
   | NUMBER
   | NAME ('(' expression (',' expression)* ')')?
   | '(' expression ')'
   | '{' expression '}'
   ;

signedNumber
   : ('+' | '-')? NUMBER
   ;

TITLE
   : [a-z_] [a-z0-9_-]* '.cir' [ \t]+ ~[\r\n]*
   ;

DIRECTIVE
   : '.' [a-z] [a-z0-9_]*
   ;

NUMBER
   : ([0-9]+ ('.' [0-9]*)? | '.' [0-9]+) ('e' [+-]? [0-9]+)? [a-z]*
   ;

NAME
   : [a-z_] [a-z0-9_./:-]*
   ;

STRING
   : '"' ('\\' . | ~["\\\r\n])* '"'
   | '\'' ('\\' . | ~['\\\r\n])* '\''
   ;

EOL
   : '\r'? '\n'
   ;

WS
   : [ \t]+ -> skip
   ;

OTHER
   : .
   ;
