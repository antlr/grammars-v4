// $antlr-format alignTrailingComments true, columnLimit 150, minEmptyLines 1, maxEmptyLinesToKeep 1, reflowComments false, useTab false
// $antlr-format allowShortRulesOnASingleLine false, allowShortBlocksOnASingleLine true, alignSemicolons hanging, alignColons hanging

parser grammar wktParser;

options {
    tokenVocab = wktLexer;
}

file_
    : geometry* EOF
    ;

/**
 * ISO/IEC 13249-3:2016, 5.1.67 <well-known text representation>. The alternatives are ordered as the standard groups them:
 * <point text representation>, <curve text representation>, <surface text representation>, <solid text representation> and
 * <collection text representation>. LINEARRING is not part of the standard, but is a widely used extension.
 */
geometry
    : pointGeometry
    | lineStringGeometry
    | linearRingGeometry
    | circularStringGeometry
    | circleGeometry
    | geodesicStringGeometry
    | ellipticalCurveGeometry
    | nurbsCurveGeometry
    | clothoidGeometry
    | spiralCurveGeometry
    | compoundCurveGeometry
    | polygonGeometry
    | triangleGeometry
    | curvePolygonGeometry
    | polyhedralSurfaceGeometry
    | tinGeometry
    | compoundSurfaceGeometry
    | brepSolidGeometry
    | multiPointGeometry
    | multiLineStringGeometry
    | multiPolygonGeometry
    | multiCurveGeometry
    | multiSurfaceGeometry
    | geometryCollection
    ;

dim
    : Z_
    | M_
    | ZM_
    ;

pointGeometry
    : POINT dim? pointText
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
    : TRIANGLE dim? triangleText
    ;

/**
 * The standard defines <triangle text> as three bare points; the OGC Simple Features form wraps a closed ring in a second pair
 * of parentheses. Both are accepted.
 */
triangleText
    : LPAR point COMMA point COMMA point RPAR
    | polygonText
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

circleGeometry
    : CIRCLE dim? lineStringText
    ;

geodesicStringGeometry
    : GEODESICSTRING dim? lineStringText
    ;

compoundCurveGeometry
    : COMPOUNDCURVE dim? (LPAR compoundCurveMember (COMMA compoundCurveMember)* RPAR | EMPTY_)
    ;

/**
 * The standard defines the members of a <compoundcurve text> as <curve text>, the same production a curve polygon's rings use.
 */
compoundCurveMember
    : curveMember
    ;

curvePolygonGeometry
    : CURVEPOLYGON dim? (LPAR curveMember (COMMA curveMember)* RPAR | EMPTY_)
    ;

multiCurveGeometry
    : MULTICURVE dim? (LPAR curveMember (COMMA curveMember)* RPAR | EMPTY_)
    ;

/**
 * <curve text>, and the textually identical <ring text> used by <curvepolygon text>.
 */
curveMember
    : lineStringText
    | circularStringGeometry
    | circleGeometry
    | geodesicStringGeometry
    | ellipticalCurveGeometry
    | nurbsCurveGeometry
    | clothoidGeometry
    | spiralCurveGeometry
    | compoundCurveGeometry
    ;

multiSurfaceGeometry
    : MULTISURFACE dim? (LPAR surfaceMember (COMMA surfaceMember)* RPAR | EMPTY_)
    ;

/**
 * <surface text>.
 */
surfaceMember
    : polygonText
    | curvePolygonGeometry
    | triangleGeometry
    | polyhedralSurfaceGeometry
    | tinGeometry
    | compoundSurfaceGeometry
    ;

polyhedralSurfaceGeometry
    : POLYHEDRALSURFACE dim? polyhedralSurfaceText
    ;

/**
 * The standard introduces the patch list with PATCHES and gives every patch its own POLYGON or TRIANGLE keyword; the OGC Simple
 * Features form lists bare polygon texts. Both are accepted.
 */
polyhedralSurfaceText
    : LPAR PATCHES polygonPatchesText RPAR
    | LPAR polygonText (COMMA polygonText)* RPAR
    | EMPTY_
    ;

polygonPatchesText
    : LPAR polygonOrTriangleGeometry (COMMA polygonOrTriangleGeometry)* RPAR
    ;

/**
 * <polygon text representation>.
 */
polygonOrTriangleGeometry
    : polygonGeometry
    | triangleGeometry
    ;

tinGeometry
    : TIN dim? tinText
    ;

/**
 * The standard introduces the patch list with PATCHES and follows it with an optional element list and maximum side length; the
 * OGC Simple Features form lists bare polygon texts. Both are accepted.
 */
tinText
    : LPAR PATCHES trianglePatchesText (ELEMENTS tinElementList)? maxSideLength? RPAR
    | LPAR polygonText (COMMA polygonText)* RPAR
    | EMPTY_
    ;

trianglePatchesText
    : LPAR triangleText (COMMA triangleText)* RPAR
    ;

tinElementList
    : LPAR tinElementTypeText (COMMA tinElementTypeText)* RPAR
    ;

tinElementTypeText
    : POINTS elementLabelText multiPointGeometry
    | GROUPSPOT elementLabelText multiPointGeometry
    | BOUNDARY elementLabelText polygonOrTriangleGeometry
    | BREAKLINE elementLabelText lineStringGeometry
    | SOFTBREAK elementLabelText lineStringGeometry
    | CONTROLCONTOUR elementLabelText lineStringGeometry
    | BREAKVOID elementLabelText polygonOrTriangleGeometry
    | DRAPEVOID elementLabelText polygonOrTriangleGeometry
    | VOID elementLabelText polygonOrTriangleGeometry
    | HOLE elementLabelText polygonOrTriangleGeometry
    | STOPLINE elementLabelText lineStringGeometry
    ;

elementLabelText
    : (ID elementId)? (TAG elementTag)?
    ;

elementId
    : DECIMAL
    ;

elementTag
    : QUOTED_LETTERS
    ;

maxSideLength
    : MAXSIDELENGTH DECIMAL
    ;

compoundSurfaceGeometry
    : COMPOUNDSURFACE dim? (LPAR surfaceMember (COMMA surfaceMember)* RPAR | EMPTY_)
    ;

/**
 * <brepsolid text representation>. The standard mandates a Z dimension for this type.
 */
brepSolidGeometry
    : BREPSOLID Z_ (LPAR shellText (COMMA shellText)* RPAR | EMPTY_)
    ;

shellText
    : polyhedralSurfaceGeometry
    | compoundSurfaceGeometry
    ;

/**
 * <elliptical text representation>.
 */
ellipticalCurveGeometry
    : ELLIPTICALCURVE dim? ellipticalText
    ;

ellipticalText
    : LPAR referenceLocationText COMMA uAxisLengthText COMMA vAxisLengthText COMMA startAngleText COMMA endAngleText (
        COMMA startMText COMMA endMText
    )? RPAR
    | EMPTY_
    ;

uAxisLengthText
    : UAXISLENGTH numberText
    ;

vAxisLengthText
    : VAXISLENGTH numberText
    ;

startAngleText
    : STARTANGLE angleText
    ;

endAngleText
    : ENDANGLE angleText
    ;

/**
 * <angle text>, defined by ISO/IEC 13249-3:2016, 16.1.21 <angle text representation>.
 */
angleText
    : DEGREES LPAR DECIMAL RPAR
    | GRADIANS LPAR DECIMAL RPAR
    | RADIANS LPAR DECIMAL RPAR
    ;

/**
 * <nurbs text representation>.
 */
nurbsCurveGeometry
    : NURBSCURVE dim? nurbsText
    ;

nurbsText
    : LPAR degreeText COMMA controlPointsText COMMA knotsText (COMMA startMText COMMA endMText)? RPAR
    | EMPTY_
    ;

degreeText
    : DEGREE DECIMAL
    ;

controlPointsText
    : CONTROLPOINTS Z_? (LPAR nurbsPointText (COMMA nurbsPointText)* RPAR | EMPTY_)
    ;

nurbsPointText
    : NURBSPOINT (LPAR weightedPointText COMMA weightText RPAR | EMPTY_)
    ;

weightedPointText
    : WEIGHTEDPOINT Z_? pointText
    ;

weightText
    : WEIGHT DECIMAL
    ;

knotsText
    : KNOTS (LPAR knotText (COMMA knotText)* RPAR | EMPTY_)
    ;

knotText
    : KNOT (LPAR valueText COMMA multiplicityText RPAR | EMPTY_)
    ;

valueText
    : VALUE DECIMAL
    ;

multiplicityText
    : MULTIPLICITY DECIMAL
    ;

/**
 * <clothoid text representation>.
 */
clothoidGeometry
    : CLOTHOID dim? clothoidText
    ;

/**
 * Two forms are accepted. The first is <clothoid text> as the standard defines it. The second is the (k0, k1, L) form
 * introduced for JTS by CLOTHOID_PROPOSAL.md, where the start point, tangent and curvature are inherited from the preceding
 * segment of the enclosing COMPOUNDCURVE. They are told apart by the token after the opening parenthesis.
 */
clothoidText
    : LPAR referenceLocationText COMMA scaleFactorText COMMA startDistanceText COMMA endDistanceText (
        COMMA startMText COMMA endMText
    )? RPAR
    | LPAR ordinate COMMA ordinate COMMA ordinate RPAR
    | EMPTY_
    ;

scaleFactorText
    : SCALEFACTOR numberText
    ;

startDistanceText
    : STARTDISTANCE numberText
    ;

endDistanceText
    : ENDDISTANCE numberText
    ;

/**
 * <spiral text representation>.
 */
spiralCurveGeometry
    : SPIRALCURVE dim? spiralText
    ;

spiralText
    : LPAR referenceLocationText COMMA spiralLengthText COMMA startCurvatureText COMMA endCurvatureText COMMA spiralTypeText (
        COMMA startMText COMMA endMText
    )? RPAR
    | EMPTY_
    ;

spiralLengthText
    : LENGTH numberText
    ;

startCurvatureText
    : STARTCURVATURE numberText
    ;

endCurvatureText
    : ENDCURVATURE numberText
    ;

/**
 * <spiraltype text> is free-form <letters>. 4.2.12 ST_SpiralCurve names clothoid, bloss, biquadratic, sine and cosine as the
 * initial value set, and 5.1.68 length-prefixes the value in the binary representation, so the set is open. SPIRALTYPE puts
 * the lexer into a mode that reads whatever follows up to the terminating comma or parenthesis.
 */
spiralTypeText
    : SPIRALTYPE (SPIRAL_TYPE_NAME | EMPTY_)
    ;

/**
 * <referencelocation text representation> and the ST_AffinePlacement it carries.
 */
referenceLocationText
    : REFERENCELOCATION affinePlacementText
    ;

affinePlacementText
    : AFFINEPLACEMENT Z_? (LPAR locationText COMMA referenceDirectionsText RPAR | EMPTY_)
    ;

locationText
    : LOCATION Z_? pointText
    ;

referenceDirectionsText
    : REFERENCEDIRECTIONS (LPAR vectorText (COMMA vectorText)* RPAR | EMPTY_)
    ;

/**
 * <vector text representation>, defined by ISO/IEC 13249-3:2016, 17.2.22 <well-known text representation>.
 */
vectorText
    : VECTOR Z_? (LPAR vector RPAR | EMPTY_)
    ;

vector
    : ordinate ordinate ordinate?
    ;

startMText
    : STARTM DECIMAL
    ;

endMText
    : ENDM DECIMAL
    ;

/**
 * The shared shape of <length text>, <scalefactor text>, <distance text>, <spirallength text> and <curvature text>.
 */
numberText
    : DECIMAL
    | EMPTY_
    ;

polygonText
    : LPAR lineStringText (COMMA lineStringText)* RPAR
    | EMPTY_
    ;

lineStringText
    : LPAR point (COMMA point)* RPAR
    | EMPTY_
    ;

pointText
    : LPAR point RPAR
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
