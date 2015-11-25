
/*	Wavefront's Advanced Visualizer ASCII .OBJ file format grammer.
*/
grammar WavefrontOBJ;

start
   : line+
   ;

line
   : ( vertex | face | mtllib | object | use_material | group ) ( '\r' )? '\n'
   ;

face
   : FACE ( INTEGER )+
   ;

vertex
   : ((GEOMETRIC_VERTEX | TEXTURE_VERTEX | VERTEX_NORMAL) DECIMAL DECIMAL DECIMAL)
     | (PARAMETER_SPACE_VERTEX DECIMAL DECIMAL)
   ;

mtllib
   : MATERIAL_LIBRARY a = NAME '.mtl' 
   ;

object
   : OBJECT_NAME NAME
   ;

use_material
   : MATERIAL_NAME NAME
   ;

group
   : SMOOTHING_GROUP ( 'on' | 'off' )
   ;

// Vertex data	

GEOMETRIC_VERTEX
   : 'v'
   ;

TEXTURE_VERTEX
   : 'vt'
   ;

VERTEX_NORMAL
   : 'vn'
   ;

PARAMETER_SPACE_VERTEX
   : 'vp'
   ;

// Free-form curve/surface attributes

DEGREE
   : 'deg'
   ;


BASIS_MATRIX
   : 'bmat'
   ;


STEP_SIZE
   : 'step'
   ;


CURVE_SURF_TYPE
   : 'cstype'
   ;

// Elements	

POINT
   : 'p'
   ;


LINE
   : 'l'
   ;


FACE
   : 'f'
   ;


CURVE
   : 'curv'
   ;


CURVE2D
   : 'curv2'
   ;


SURF
   : 'surf'
   ;

// Free-form curve/surface body statements

PARAM
   : 'parm'
   ;


OUTER_TRIMMING_HOLE
   : 'trim'
   ;


INNER_TRIMMING_HOLE
   : 'hole'
   ;


SPECIAL_CURVE
   : 'scrv'
   ;


SPECIAL_POINT
   : 'sp'
   ;


END
   : 'end'
   ;

// Connectivity between free-form surfaces

CONNECT
   : 'con'
   ;

// Grouping

GROUP_NAME
   : 'g'
   ;


SMOOTHING_GROUP
   : 's'
   ;


MERGING_GROUP
   : 'mg'
   ;


OBJECT_NAME
   : 'o'
   ;

// Display/render attributes

BEVEL_INTERPOLATION
   : 'bevel'
   ;


COLOR_INTERPOLATION
   : 'c_interp'
   ;


DISSOLVE_INTERPOLATION
   : 'd_interp'
   ;


LEVEL_OF_DETAIL
   : 'lod'
   ;


MATERIAL_NAME
   : 'usemtl'
   ;


MATERIAL_LIBRARY
   : 'mtllib'
   ;


SHADOW_CASTING
   : 'shadow_obj'
   ;


RAY_TRACING
   : 'trace_obj'
   ;


CURVE_APPROX
   : 'ctech'
   ;


SURF_APPROX
   : 'stech'
   ;


fragment DIGIT
   : '0' .. '9'
   ;


INTEGER
   : '-'? ( DIGIT )+
   ;


DECIMAL
   : INTEGER ( '.' DIGIT* )?
   ;

COMMENT
   : '#' ~ ( '\n' | '\r' )* '\r'? '\n' -> skip
   ;


NAME
   : ( 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '_' | '~' | '(' | ')' )+
   ;


WS
   : ( ' ' | '\t' )+ -> skip
   ;
