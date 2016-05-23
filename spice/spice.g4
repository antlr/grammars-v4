
grammar spice;

spicefile
   : netlist END CR?
   ;

netlist
   : (node | simulationcommand) +
   ;

node
   : vnode
   | rnode
   | cnode
   | lnode
   ;

vertexID
   : DIGIT +
   ;

vnode
   : VNODENAME vertexID vertexID VSOURCETYPE expression
   ;


VNODENAME
   : V DIGIT +
   ;


VSOURCETYPE
   : (D C) | (A C)
   ;

rnode
   : RNODENAME vertexID vertexID expression
   ;


RNODENAME
   : R DIGIT +
   ;

cnode
   : CNODENAME vertexID vertexID expression ('ic' EQUALS expression)?
   ;


CNODENAME
   : C DIGIT +
   ;

lnode
   : LNODENAME vertexID vertexID expression
   ;


LNODENAME
   : L DIGIT +
   ;

simulationcommand
   : dotdc
   ;

dotdc
   : '.DC' VNODENAME expression expression expression
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
   : number (E number)?
   ;

number
   : MINUS? DIGIT* METRIC_PREFIX? (POINT DIGIT + METRIC_PREFIX?)?
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
   : E | P | T | G | M | K | H | D | C | M | U | N | F | A
   ;


fragment A
   : ('a' | 'A')
   ;


fragment B
   : ('b' | 'B')
   ;


fragment C
   : ('c' | 'C')
   ;


fragment D
   : ('d' | 'D')
   ;


fragment E
   : ('e' | 'E')
   ;


fragment F
   : ('f' | 'F')
   ;


fragment G
   : ('g' | 'G')
   ;


fragment H
   : ('h' | 'H')
   ;


fragment I
   : ('i' | 'I')
   ;


fragment J
   : ('j' | 'J')
   ;


fragment K
   : ('k' | 'K')
   ;


fragment L
   : ('l' | 'L')
   ;


fragment M
   : ('m' | 'M')
   ;


fragment N
   : ('n' | 'N')
   ;


fragment O
   : ('o' | 'O')
   ;


fragment P
   : ('p' | 'P')
   ;


fragment Q
   : ('q' | 'Q')
   ;


fragment R
   : ('r' | 'R')
   ;


fragment S
   : ('s' | 'S')
   ;


fragment T
   : ('t' | 'T')
   ;


fragment U
   : ('u' | 'U')
   ;


fragment V
   : ('v' | 'V')
   ;


fragment W
   : ('w' | 'W')
   ;


fragment X
   : ('x' | 'X')
   ;


fragment Y
   : ('y' | 'Y')
   ;


fragment Z
   : ('z' | 'Z')
   ;


DIGIT
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


WS
   : [ \r\n\t] + -> channel (HIDDEN)
   ;
