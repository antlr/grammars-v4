
grammar wkt;

file_ : geometry* EOF ;

geometryCollection
   : GEOMETRYCOLLECTION ( LPAR geometry (COMMA geometry)* RPAR | EMPTY_)
   ;

geometry
   : (polygonGeometry | lineStringGeometry | pointGeometry | compoundCurveGeometry | curvePolygonGeometry | multiSurfaceGeometry | multiCurveGeometry | multiPointGeometry | multiLineStringGeometry | multiPolygonGeometry | circularStringGeometry | multiPolyhedralSurfaceGeometry | multiTinGeometry | geometryCollection)
   ;

pointGeometry
   : POINT ((name? LPAR point RPAR) | EMPTY_)
   ;

lineStringGeometry
   : LINESTRING lineString
   ;

polygonGeometry
   : POLYGON polygon
   ;

multiCurveGeometry
   : MULTICURVE ((LPAR (lineString | circularStringGeometry | compoundCurveGeometry) (COMMA (circularStringGeometry | lineString | compoundCurveGeometry))* RPAR) | EMPTY_)
   ;

multiSurfaceGeometry
   : MULTISURFACE ((LPAR (polygon | curvePolygonGeometry) (COMMA (polygon | curvePolygonGeometry))* RPAR) | EMPTY_)
   ;

curvePolygonGeometry
   : CURVEPOLYGON ((LPAR (lineString | circularStringGeometry | compoundCurveGeometry) (COMMA (circularStringGeometry | lineString | compoundCurveGeometry))* RPAR) | EMPTY_)
   ;

compoundCurveGeometry
   : COMPOUNDCURVE ((LPAR (lineString | circularStringGeometry) (COMMA (circularStringGeometry | lineString))* RPAR) | EMPTY_)
   ;

multiPointGeometry
   : MULTIPOINT ((LPAR pointOrClosedPoint (COMMA pointOrClosedPoint)* RPAR) | EMPTY_)
   ;

multiLineStringGeometry
   : MULTILINESTRING ((LPAR lineString (COMMA lineString)* RPAR) | EMPTY_)
   ;

multiPolygonGeometry
   : MULTIPOLYGON ((LPAR polygon (COMMA polygon)* RPAR) | EMPTY_)
   ;

multiPolyhedralSurfaceGeometry
   : POLYHEDRALSURFACE ((LPAR polygon (COMMA polygon)* RPAR) | EMPTY_)
   ;

multiTinGeometry
   : TIN ((LPAR polygon (COMMA polygon)* RPAR) | EMPTY_)
   ;

circularStringGeometry
   : CIRCULARSTRING LPAR point (COMMA point)* RPAR
   ;

pointOrClosedPoint
   : point
   | LPAR point RPAR
   ;

polygon
   : LPAR lineString (COMMA lineString)* RPAR | EMPTY_
   ;

lineString
   : LPAR point (COMMA point)* RPAR | EMPTY_
   ;

point
   : DECIMAL +
   ;

name
   : STRING
   ;


DECIMAL
   : '-'? INTEGERPART (DOT DECIMALPART)?
   ;


INTEGERPART
   : '0' | NONZERODIGIT DIGIT*
   ;


DECIMALPART
   : DIGIT +
   ;


fragment DIGIT
   : '0' | NONZERODIGIT
   ;


fragment NONZERODIGIT
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


GEOMETRYCOLLECTION
   : G E O M E T R Y C O L L E C T I O N
   ;


EMPTY_
   : E M P T Y
   ;


CIRCULARSTRING
   : C I R C U L A R S T R I N G
   ;


COMPOUNDCURVE
   : C O M P O U N D C U R V E
   ;

MULTISURFACE
   : M U L T I S U R F A C E
   ;

CURVEPOLYGON
   : C U R V E P O L Y G O N
   ;


MULTICURVE
   : M U L T I C U R V E
   ;


TRIANGLE
   : T R I A N G L E
   ;


TIN
   : T I N
   ;


POLYHEDRALSURFACE
   : P O L Y H E D R A L S U R F A C E
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


STRING
   : [a-zA-Z] +
   ;


WS
   : [ \t\r\n] + -> skip
   ;

