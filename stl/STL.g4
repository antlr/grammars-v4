grammar STL;
file_: header triangle* footer EOF;
triangle:
	'facet' 'normal' n = triple 'outer' 'loop' vertex vertex vertex 'endloop' 'endfacet';
vertex: 'vertex' triple;
triple: i = FLOAT j = FLOAT k = FLOAT;
FLOAT
    :   FractionalConstant ExponentPart?
    |   [0-9]+ ExponentPart
    ;
fragment
FractionalConstant
    :   [0-9]* '.' [0-9]+
    |   [0-9]+ '.'
    ;
fragment
ExponentPart
    :   [eE] [+-]? [0-9]+
    ;

header: 'solid' name = IDENTIFIER?;
footer: 'endsolid' name = IDENTIFIER;
IDENTIFIER: [a-zA-Z0-9]+;
WS: [\r\n\t ]+ -> skip;