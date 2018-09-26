
grammar wkt;

geometry
   : (polygonGeometry | lineStringGeometry | pointGeometry | multiPointGeometry | multiLineStringGeometry | multiPolygonGeometry)
   ;

pointGeometry
   : POINT LPAR point RPAR EOF
   ;

lineStringGeometry
   : LINESTRING lineString EOF
   ;

polygonGeometry
   : POLYGON polygon EOF
   ;

multiPointGeometry
   : MULTIPOINT LPAR pointOrClosedPoint (COMMA pointOrClosedPoint)* RPAR EOF
   ;

multiLineStringGeometry
   : MULTILINESTRING LPAR lineString (COMMA lineString)* RPAR EOF
   ;

multiPolygonGeometry
   : MULTIPOLYGON LPAR polygon (COMMA polygon)* RPAR EOF
   ;

pointOrClosedPoint
   : point
   | LPAR point RPAR
   ;

polygon
   : LPAR lineString (COMMA lineString)* RPAR
   ;

lineString
   : LPAR point (COMMA point)* RPAR
   ;

point
   : x = Decimal y = Decimal
   ;


/**
 * Coordinates
 */
Decimal
   : '-'? IntegerPart (DOT DecimalPart)?
   ;


IntegerPart
   : '0' | NonZeroDigit Digit*
   ;


DecimalPart
   : Digit +
   ;


fragment Digit
   : '0' | NonZeroDigit
   ;


fragment NonZeroDigit
   : [1-9]
   ;


fragment DOT
   : '.'
   ;


COMMA
   : ','
   ;


LPAR
   : '('
   ;


RPAR
   : ')'
   ;


/**
 * Case-insensitive geometry types
 */
POINT
   : P O I N T
   ;


LINESTRING
   : L I N E S T R I N G
   ;


POLYGON
   : P O L Y G O N
   ;


MULTIPOINT
   : M U L T I P O I N T
   ;


MULTILINESTRING
   : M U L T I L I N E S T R I N G
   ;


MULTIPOLYGON
   : M U L T I P O L Y G O N
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


WS
   : [ \t\r\n] + -> skip
   ;
