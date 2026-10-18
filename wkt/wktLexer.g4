// $antlr-format alignTrailingComments true, columnLimit 150, minEmptyLines 1, maxEmptyLinesToKeep 1, reflowComments false, useTab false
// $antlr-format allowShortRulesOnASingleLine false, allowShortBlocksOnASingleLine true, alignSemicolons hanging, alignColons hanging

lexer grammar wktLexer;

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

CIRCLE
    : C I R C L E
    ;

GEODESICSTRING
    : G E O D E S I C S T R I N G
    ;

ELLIPTICALCURVE
    : E L L I P T I C A L C U R V E
    ;

NURBSCURVE
    : N U R B S C U R V E
    ;

CLOTHOID
    : C L O T H O I D
    ;

SPIRALCURVE
    : S P I R A L C U R V E
    ;

COMPOUNDCURVE
    : C O M P O U N D C U R V E
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

COMPOUNDSURFACE
    : C O M P O U N D S U R F A C E
    ;

BREPSOLID
    : B R E P S O L I D
    ;

TIN
    : T I N
    ;

/**
 * Case-insensitive keywords naming the components of the curve, surface and solid types
 */
REFERENCELOCATION
    : R E F E R E N C E L O C A T I O N
    ;

AFFINEPLACEMENT
    : A F F I N E P L A C E M E N T
    ;

LOCATION
    : L O C A T I O N
    ;

REFERENCEDIRECTIONS
    : R E F E R E N C E D I R E C T I O N S
    ;

VECTOR
    : V E C T O R
    ;

UAXISLENGTH
    : U A X I S L E N G T H
    ;

VAXISLENGTH
    : V A X I S L E N G T H
    ;

STARTANGLE
    : S T A R T A N G L E
    ;

ENDANGLE
    : E N D A N G L E
    ;

DEGREES
    : D E G R E E S
    ;

GRADIANS
    : G R A D I A N S
    ;

RADIANS
    : R A D I A N S
    ;

DEGREE
    : D E G R E E
    ;

CONTROLPOINTS
    : C O N T R O L P O I N T S
    ;

NURBSPOINT
    : N U R B S P O I N T
    ;

WEIGHTEDPOINT
    : W E I G H T E D P O I N T
    ;

WEIGHT
    : W E I G H T
    ;

KNOTS
    : K N O T S
    ;

KNOT
    : K N O T
    ;

VALUE
    : V A L U E
    ;

MULTIPLICITY
    : M U L T I P L I C I T Y
    ;

SCALEFACTOR
    : S C A L E F A C T O R
    ;

STARTDISTANCE
    : S T A R T D I S T A N C E
    ;

ENDDISTANCE
    : E N D D I S T A N C E
    ;

LENGTH
    : L E N G T H
    ;

STARTCURVATURE
    : S T A R T C U R V A T U R E
    ;

ENDCURVATURE
    : E N D C U R V A T U R E
    ;

SPIRALTYPE
    : S P I R A L T Y P E -> pushMode(SPIRAL_TYPE_MODE)
    ;

STARTM
    : S T A R T M
    ;

ENDM
    : E N D M
    ;

/**
 * Case-insensitive keywords naming the parts of a polyhedral surface or a TIN
 */
PATCHES
    : P A T C H E S
    ;

ELEMENTS
    : E L E M E N T S
    ;

MAXSIDELENGTH
    : M A X S I D E L E N G T H
    ;

ID
    : I D
    ;

TAG
    : T A G
    ;

POINTS
    : P O I N T S
    ;

GROUPSPOT
    : G R O U P S P O T
    ;

BOUNDARY
    : B O U N D A R Y
    ;

BREAKLINE
    : B R E A K L I N E
    ;

SOFTBREAK
    : S O F T B R E A K
    ;

CONTROLCONTOUR
    : C O N T R O L C O N T O U R
    ;

BREAKVOID
    : B R E A K V O I D
    ;

DRAPEVOID
    : D R A P E V O I D
    ;

VOID
    : V O I D
    ;

HOLE
    : H O L E
    ;

STOPLINE
    : S T O P L I N E
    ;

/**
 * <element tag>: <letters> between SQL double quotes. <letters> covers the simple Latin letters, the digits and the <special>
 * characters listed by the standard.
 */
QUOTED_LETTERS
    : '"' LETTER+ '"'
    ;

fragment LETTER
    : [a-zA-Z0-9()\-_.' ]
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

/**
 * The value of a SPIRALTYPE. <letters> admits spaces and punctuation, so the only reliable terminators are the comma that
 * separates the value from the next component and the parenthesis that closes the spiral. Surrounding whitespace is not part
 * of the name; interior whitespace is.
 */
mode SPIRAL_TYPE_MODE;

SPIRAL_TYPE_WS
    : [ \t\r\n]+ -> skip
    ;

SPIRAL_TYPE_EMPTY
    : E M P T Y -> type(EMPTY_), popMode
    ;

SPIRAL_TYPE_NAME
    : ~[,() \t\r\n] (~[,()]* ~[,() \t\r\n])? -> popMode
    ;
