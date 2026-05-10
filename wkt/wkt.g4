// $antlr-format alignTrailingComments true, columnLimit 150, minEmptyLines 1, maxEmptyLinesToKeep 1, reflowComments false, useTab false
// $antlr-format allowShortRulesOnASingleLine false, allowShortBlocksOnASingleLine true, alignSemicolons hanging, alignColons hanging

grammar wkt;

file_
    : geometry* EOF
    ;

geometry
    : pointGeometry
    | lineStringGeometry
    | linearRingGeometry
    | polygonGeometry
    | triangleGeometry
    | multiPointGeometry
    | multiLineStringGeometry
    | multiPolygonGeometry
    | geometryCollection
    | circularStringGeometry
    | compoundCurveGeometry
    | curvePolygonGeometry
    | multiCurveGeometry
    | multiSurfaceGeometry
    | polyhedralSurfaceGeometry
    | tinGeometry
    ;

dim
    : Z_
    | M_
    | ZM_
    ;

pointGeometry
    : POINT dim? (LPAR point RPAR | EMPTY_)
    ;

lineStringGeometry
    : LINESTRING dim? lineStringText
    ;

linearRingGeometry
    : LINEARRING dim? lineStringText
    ;

polygonGeometry
    : POLYGON dim? polygonText
    ;

triangleGeometry
    : TRIANGLE dim? polygonText
    ;

multiPointGeometry
    : MULTIPOINT dim? (LPAR pointOrClosedPoint (COMMA pointOrClosedPoint)* RPAR | EMPTY_)
    ;

multiLineStringGeometry
    : MULTILINESTRING dim? (LPAR lineStringText (COMMA lineStringText)* RPAR | EMPTY_)
    ;

multiPolygonGeometry
    : MULTIPOLYGON dim? (LPAR polygonText (COMMA polygonText)* RPAR | EMPTY_)
    ;

geometryCollection
    : GEOMETRYCOLLECTION dim? (LPAR geometry (COMMA geometry)* RPAR | EMPTY_)
    ;

circularStringGeometry
    : CIRCULARSTRING dim? lineStringText
    ;

compoundCurveGeometry
    : COMPOUNDCURVE dim? (LPAR compoundCurveMember (COMMA compoundCurveMember)* RPAR | EMPTY_)
    ;

compoundCurveMember
    : lineStringText
    | circularStringGeometry
    | clothoidGeometry
    ;

clothoidGeometry
    : CLOTHOID LPAR ordinate COMMA ordinate COMMA ordinate RPAR
    ;

curvePolygonGeometry
    : CURVEPOLYGON dim? (LPAR curveMember (COMMA curveMember)* RPAR | EMPTY_)
    ;

multiCurveGeometry
    : MULTICURVE dim? (LPAR curveMember (COMMA curveMember)* RPAR | EMPTY_)
    ;

curveMember
    : lineStringText
    | circularStringGeometry
    | compoundCurveGeometry
    ;

multiSurfaceGeometry
    : MULTISURFACE dim? (LPAR surfaceMember (COMMA surfaceMember)* RPAR | EMPTY_)
    ;

surfaceMember
    : polygonText
    | curvePolygonGeometry
    ;

polyhedralSurfaceGeometry
    : POLYHEDRALSURFACE dim? (LPAR polygonText (COMMA polygonText)* RPAR | EMPTY_)
    ;

tinGeometry
    : TIN dim? (LPAR polygonText (COMMA polygonText)* RPAR | EMPTY_)
    ;

polygonText
    : LPAR lineStringText (COMMA lineStringText)* RPAR
    | EMPTY_
    ;

lineStringText
    : LPAR point (COMMA point)* RPAR
    | EMPTY_
    ;

pointOrClosedPoint
    : point
    | LPAR point RPAR
    | EMPTY_
    ;

point
    : ordinate ordinate ordinate? ordinate?
    ;

ordinate
    : DECIMAL
    | NAN_
    | INF_
    | NEG_INF_
    ;

DECIMAL
    : '-'? INTEGERPART (DOT DECIMALPART)? EXPONENT?
    ;

INTEGERPART
    : '0'
    | NONZERODIGIT DIGIT*
    ;

DECIMALPART
    : DIGIT+
    ;

fragment EXPONENT
    : ('e' | 'E') ('+' | '-')? DIGIT+
    ;

fragment DIGIT
    : '0'
    | NONZERODIGIT
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
 * Case-insensitive geometry type keywords
 */
POINT
    : P O I N T
    ;

LINESTRING
    : L I N E S T R I N G
    ;

LINEARRING
    : L I N E A R R I N G
    ;

POLYGON
    : P O L Y G O N
    ;

TRIANGLE
    : T R I A N G L E
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

CIRCULARSTRING
    : C I R C U L A R S T R I N G
    ;

COMPOUNDCURVE
    : C O M P O U N D C U R V E
    ;

CLOTHOID
    : C L O T H O I D
    ;

CURVEPOLYGON
    : C U R V E P O L Y G O N
    ;

MULTICURVE
    : M U L T I C U R V E
    ;

MULTISURFACE
    : M U L T I S U R F A C E
    ;

POLYHEDRALSURFACE
    : P O L Y H E D R A L S U R F A C E
    ;

TIN
    : T I N
    ;

EMPTY_
    : E M P T Y
    ;

ZM_
    : Z M
    ;

Z_
    : Z
    ;

M_
    : M
    ;

NAN_
    : N A N
    ;

NEG_INF_
    : '-' I N F
    ;

INF_
    : I N F
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
    : [ \t\r\n]+ -> skip
    ;
