grammar spice;

spicefile
    : netlist END CR?
    ;

netlist
    : node+
    ;    

node
    : vnode
    | rnode
    | cnode
    | lnode
    ;

vertexID
    : DIGIT+
    ;

vnode
    : vnodeName vertexID vertexID 'dc' expression;

vnodeName
    : ('V' | 'v') DIGIT+
    ;

rnode
    : rnodeName vertexID vertexID expression;

rnodeName
    :('R' | 'r') DIGIT+
    ;

cnode
    : codeName vertexID vertexID expression ('ic' EQUALS expression)?;

codeName
    :('c' | 'C') DIGIT+
    ;

lnode
    : lodeName vertexID vertexID expression;

lodeName
    :('l' | 'L') DIGIT+
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
   ;

// a number with a potential metrix suffix
value
    : scientific ('k')?
    ;
   
scientific
   : number (E number)?
   ;

number
   : MINUS? DIGIT+ (POINT DIGIT+)?
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

E
   : 'e' | 'E'
   ;

DIGIT
   : ('0' .. '9')
   ;

END
    : '.END' | '.end'
    ;

COMMENT_1
    : '*' .*? CR -> skip
    ;

COMMENT_2
    : ';' .*? CR -> skip
    ;

CR
    : [\r\n]+
    ;

WS
    : [ \t]->skip
    ;