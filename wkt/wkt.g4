grammar wkt;

geometry
	:	(polygonGeometry
		| lineStringGeometry
		| pointGeometry
		| multiPointGeometry
		| multiLineStringGeometry
		| multiPolygonGeometry)
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
	: LPAR lineString (COMMA lineString )* RPAR
	;

lineString
	: LPAR point (COMMA point)* RPAR
	;

point
	: x=Decimal y=Decimal
	;


/**
 * Coordinates
 */
Decimal
	: '-'? IntegerPart (DOT DecimalPart)?
	;

IntegerPart
	: '0'
	| NonZeroDigit Digit*
	;

DecimalPart
	: Digit+
	;


fragment Digit
	: '0'
	| NonZeroDigit
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
	: 'POINT'
	| 'point'
	| 'Point'
	;

LINESTRING
	: 'LINESTRING'
	| 'linestring'
	| 'LineString'
	;

POLYGON
	: 'POLYGON'
	| 'polygon'
	| 'Polygon'
	;

MULTIPOINT
	: 'MULTIPOINT'
	| 'multipoint'
	| 'MultiPoint'
	;

MULTILINESTRING
	: 'MULTILINESTRING'
	| 'multilinestring'
	| 'MultiLineString'
	;

MULTIPOLYGON
	: 'MULTIPOLYGON'
	| 'multipolygon'
	| 'MultiPolygon'
	;


// Skip white space
WS  :   ( ' '
        | '\t'
        | '\r'
        | '\n'
        ) -> skip
    ;