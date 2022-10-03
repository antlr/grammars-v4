
/*
 * Wavefront's Advanced Visualizer ASCII .OBJ file format grammer.
 *
 * Reference : http ://fegemo.github.io/cefet-cg/attachments/obj-spec.pdf
 */
grammar WavefrontOBJ;

// PARSER RULES

start
    : NL* (statement (NL+ | EOF))+
    ;

statement
    : call | csh
    | vertex | vertex_normal | vertex_texture | vertex_parameter
    | points | lines | faces
    | curve_surface_type | degree | basis_matrix | step
    | free_form_surface
    | connectivity
    | group | smoothing_group | merging_group | object_name
    | bevel | color_interpolation | dissolve_interpolation | level_of_detail
    | map_library | use_map | material_library | use_material
    | shadow_object | trace_object
    | curve_approximation_technique | surface_approximation_technique
    ;


// General statement
// -----------------

// Calls out to evaluate another .obj file
call : filename=FILENAME args=NON_WS*;

// Executes a UNIX command.
// It is unclear whether the intent is for the output of the command to be treated as new input.
csh : command=FILENAME '-'? args=NON_WS*;


// Vertex data
// -----------

// Specifies a geometric vertex and its `x`, `y`, `z` coordinates.
// The fourth value (`w`, or weight) defaults to 1.0.
vertex : 'v' x=decimal y=decimal z=decimal w=decimal?;

// Specifies a point in the parameter space of a curve or surface.
// - `u` is the point in the parameter space of a curve or the first
//   coordinate in the parameter space of a surface.
// - `v` is the second coordinate in the parameter space of a surface
// - `w` is the weight required for rational trimming curves. If you do
//   not specify a value for w, it defaults to 1.0.
vertex_parameter : 'vp' u=decimal v=decimal? w=decimal?;

// Specifies a normal vector with components i, j, and k.
// (We'll still name them `x`, `y` and `z` because that is the modern convention.)
vertex_normal : 'vn' x=decimal y=decimal z=decimal;

// Specifies a texture vertex and its coordinates.
// - `u` is the value for the horizontal direction of the texture.
// - `v` is the value for the vertical direction of the texture. The default is 0.
// - `w` is a value for the depth of the texture. The default is 0.
vertex_texture : 'vt' u=decimal v=decimal? w=decimal?;


// Free-form curve/surface attributes
// ----------------------------------

// Specifies the type of curve or surface and indicates a rational or
// non-rational form.
// rat specifies a rational form for the curve or surface type. If rat is
// not included, the curve or surface is non-rational.
// type specifies the curve or surface type.
curve_surface_type
    : 'cstype'
       rational='rat'?
       cstype=( 'bmatrix' | 'bezier' | 'bspline' | 'cardinal' | 'taylor' )
    ;

// Sets the polynomial degree for curves and surfaces.
// - `u` is the degree in the u direction. It is required for both curves
//   and surfaces.
// - `v` is the degree in the v direction. It is required only for surfaces.
//   For Bezier, B-spline, Taylor, and basis matrix.
//   For Cardinal, the degree is always 3. If some other value is given
//   for Cardinal, it will be ignored.
degree : 'deg' u=INTEGER v=INTEGER?;

// Sets the basis matrices used for basis matrix curves and surfaces.
// The u and v values must be specified in separate bmat statements.
// Note : The deg statement must be given before the bmat statements
// and the size of the matrix must be appropriate for the degree: (deg+1) x (deg+1)
// - `u` specifies that the basis matrix is applied in the u direction.
// - `v` specifies that the basis matrix is applied in the v direction.
basis_matrix : 'bmat' ( 'u' | 'v' ) decimal+;

// Sets the step size for curves and surfaces that use a basis matrix.
// - `u` is the step size in the u direction. It is required for both
//   curves and surfaces that use a basis matrix.
// - `v` is the step size in the v direction. It is required only for
//   surfaces that use a basis matrix.
step : 'step' u=INTEGER v=INTEGER?;


// Elements
// --------

// Specifies a point element and its vertex. You can specify multiple
// points with this statement.
points : 'p' v=INTEGER+;

// Specifies a line and its vertex reference numbers. You can
// optionally include the texture vertex reference numbers.
// Should specify at least 2 points.
lines : 'l' v=( INTEGER | INTEGER_PAIR )+;

// Specifies a face element and its vertex reference number. You can
// optionally include the texture vertex and vertex normal reference
// numbers.
// Should specify at least 3 points.
faces : 'f' v=( INTEGER | INTEGER_PAIR | INTEGER_TRIPLET )+;

// Block defining a free-form surface
free_form_surface
   : ( curve | curve2d | surface ) NL+
     ( ( parameter | outer_trimming_loop | inner_trimming_loop | special_curve | special_point )
       NL+
     )*
     end
   ;

// Specifies a curve, its parameter range, and its control vertices.
// - `u0` is the starting parameter value for the curve.
// - `u1` is the ending parameter value for the curve.
// - `v` is the vertex reference number for a control point. You can
//    specify multiple control points. A minimum of two control points
//   are required for a curve.
// For a non-rational curve, the control points must be 3D. For a
// rational curve, the control points are 3D or 4D. The fourth
// coordinate (weight) defaults to 1.0 if omitted.
curve : 'curv' u0=decimal u1=decimal v=INTEGER+;

// Specifies a 2D curve on a surface and its control points.
// A 2D curve is used as an outer or inner trimming curve, as a
// special curve, or for connectivity.
// - `vp` is the parameter vertex reference number for the control point.
//   You can specify multiple control points. A minimum of two
//   control points is required for a 2D curve.
// The control points are parameter vertices because the curve must
// lie in the parameter space of some surface. For a non-rational
// curve, the control vertices can be 2D. For a rational curve, the
// control vertices can be 2D or 3D. The third coordinate (weight)
// defaults to 1.0 if omitted.
curve2d : 'curv2' vp1=INTEGER vp2=INTEGER+;

// Specifies a surface, its parameter range, and its control vertices.
// The surface is evaluated within the global parameter range from
// s0 to s1 in the u direction and t0 to t1 in the v direction.
// - `s0` is the starting parameter value for the surface in the u direction.
// - `s1` is the ending parameter value for the surface in the u direction.
// - `t0` is the starting parameter value for the surface in the v direction.
// - `t1` is the ending parameter value for the surface in the v direction.
// - `v` is a triplet of vertex, texture, normal indices, separated by slashes.
// For a non-rational surface the control vertices are 3D. For a
// rational surface the control vertices can be 3D or 4D. The fourth
// coordinate (weight) defaults to 1.0 if omitted.
surface
    : 'surf'
      s0=decimal s1=decimal
      t0=decimal t1=decimal
      v=( INTEGER | INTEGER_PAIR | INTEGER_TRIPLET )+
    ;


// Free-form curve/surface body statements
// ---------------------------------------

// Specifies global parameter values.
// For B-spline curves and surfaces, this specifies the knot vectors.
// - `u` specifies the u direction for the parameter values.
// - `v` specifies the v direction for the parameter values.
//   To set u and v values, use separate command lines.
// - `p` is the global parameter or knot value. You can specify multiple
//   values. A minimum of two parameter values are required.
//   Parameter values must increase monotonically. The type of
//   surface and the degree dictate the number of values required.
parameter : 'parm' ( 'u' | 'v' ) p=decimal+;

// Specifies a sequence of curves to build a single outer trimming loop.
// - `u0` is the starting parameter value for the trimming curve `curv2d`.
// - `u1` is the ending parameter value for the trimming curve `curv2d`.
// - `curv2d` is the index of the trimming curve lying in the parameter
//   space of the surface. This curve must have been previously
//   defined with the curv2 statement.
outer_trimming_loop : 'trim' ( u0=decimal u1=decimal curv2d=INTEGER )+;

// Specifies a sequence of curves to build a single inner trimming loop (hole).
// - `u0` is the starting parameter value for the trimming curve `curv2d`.
// - `u1` is the ending parameter value for the trimming curve `curv2d`.
// - `curv2d` is the index of the trimming curve lying in the parameter
//   space of the surface. This curve must have been previously
//   defined with the curv2 statement.
inner_trimming_loop : 'hole' ( u0=decimal u1=decimal curv2d=INTEGER )+;

// Specifies a sequence of curves which lie on the given surface to
// build a single special curve.
// - `u0` is the starting parameter value for the special curve `curv2d`.
// - `u1` is the ending parameter value for the special curve `curv2d`.
// - `curv2d` is the index of the special curve lying in the parameter
//   space of the surface. This curve must have been previously
//   defined with the curv2 statement.
special_curve : 'scrv' ( u0=decimal u1=decimal curv2d=INTEGER )+;

// Specifies special geometric points to be associated with a curve or
// surface. For space curves and trimming curves, the parameter
// vertices must be 1D. For surfaces, the parameter vertices must be 2D.
// - `vp` is the reference number for the parameter vertex of a special
//   point to be associated with the parameter space point of the curve
//   or surface.
special_point : 'sp' vp=INTEGER+;

// Specifies the end of a curve or surface body begun by a `curv`, `curv2`,
// or `surf` statement.
end : 'end';


// Connectivity between free-form surfaces
// ---------------------------------------

// Specifies connectivity between two surfaces.
// - `surf_1` is the index of the first surface.
// - `q0_1` is the starting parameter for the curve referenced by `curv2d_1`.
// - `q1_1` is the ending parameter for the curve referenced `bycurv2d_1`.
// - `curv2d_1` is the index of a curve on the first surface. This curve
//   must have been previously defined with the curv2 statement.
// - `surf_2` is the index of the second surface.
// - `q0_2` is the starting parameter for the curve referenced by `curv2d_2`.
// - `q1_2` is the ending parameter for the curve referenced by `curv2d_2`.
// - `curv2d_2` is the index of a curve on the second surface. This curve
//   must have been previously defined with the curv2 statement.
connectivity
    : 'con'
      surf_1=INTEGER q0_1=decimal q1_1=decimal curv2d_1=INTEGER
      surf_2=INTEGER q0_2=decimal q1_2=decimal curv2d_2=INTEGER
    ;

// Grouping

// Specifies the group name for the elements that follow it.
// You can have multiple group names. If there are multiple groups on one
// line, the data that follows belong to all groups. Group information
// is optional.
// - `group_name` is the name for the group. Letters, numbers, and
//   combinations of letters and numbers are accepted for group
//   names. The default group name is 'default'.
group : 'g' group_name=NAME+;

// Sets the smoothing group for the elements that follow it.
// If you do not want to use a smoothing group, specify off or a value of 0.
// This is essentially a shortcut for providing smooth vertex normals.
smoothing_group
    : 's'
      ( group_number=INTEGER
      | 'off'
      )
    ;

// Sets the merging group and merge resolution for the free-form
// surfaces that follow it. If you do not want to use a merging group,
// specify off or a value of 0.
// Adjacency detection is performed only within groups, never
// between groups. Connectivity between surfaces in different
// merging groups is not allowed. Surfaces in the same merging
// group are merged together along edges that are within the
// distance res apart.
// - `group_number` is the merging group number. To turn off adjacency
//   detection, use a value of 0 or off.
// - `res` is the maximum distance between two surfaces that will be
//   merged together. The resolution must be a value greater than 0.
//   This is a required argument only when using merging groups.
merging_group
    : 'mg'
      ( group_number=INTEGER res=decimal
      | 'off'
      )
    ;

// Specifies a user-defined object name for the elements
// defined after this statement.
// - object_name is the user-defined object name.
object_name : 'o' name=NAME;

// Display/render attributes

// Turns bevel interpolation on or off.
// Bevel interpolation uses normal vector interpolation to give an
// illusion of roundness to a flat bevel. It does not affect the
// smoothing of non-bevelled faces.
bevel : 'bevel' ( 'on' | 'off' );

// Sets color interpolation on or off.
// Color interpolation creates a blend across the surface of a polygon
// between the materials assigned to its vertices.
// Color interpolation applies to the values for ambient (Ka), diffuse (Kd),
// specular (Ks), and specular highlight (Ns) material properties.
color_interpolation : 'c_interp' ( 'on' | 'off' );

// Sets dissolve interpolation on or off.
// Dissolve interpolation creates an interpolation or blend across a
// polygon between the dissolve (d) values of the materials assigned
// to its vertices. This feature is used to create effects exhibiting
// varying degrees of apparent transparency, as in glass or clouds.
dissolve_interpolation : 'd_interp' ( 'on' | 'off' );

// Specifies level of detail. Used only by PreView application
// as a hint to which elements to hide for performance.
level_of_detail : 'lod' level=INTEGER;

// Specifies the map library file for texture map definitions set with
// `usemap`. You can specify multiple filenames. If multiple filenames are
// specified, the first file listed is searched first for the map
// definition, the second file is searched next, and so on.
// - filename is the name of the library file where the texture maps are defined.
map_library : 'maplib' filename=FILENAME+;

// Specifies the texture map name for the element following it.
// If you specify texture mapping for a face without texture vertices,
// the texture map will be ignored.
// - map_name is the name of the texture map. 'off' turns off texture mapping.
use_map
    : 'usemap'
      ( map_name=NAME
      | 'off'
      )
    ;

// Specifies the material library file for the material definitions set
// with `usemtl`. You can specify multiple filenames. If multiple filenames are
// specified, the first file listed is searched first for the material definition,
// the second file is searched next, and so on.
// - filename is the name of the library file that defines the materials.
material_library : 'mtllib' filename=FILENAME;

// Specifies the material name for the element following it.
// Once a material is assigned, it cannot be turned off; it can only be changed.
// material_name is the name of the material. If a material name is not
// specified, a white material is used.
use_material : 'usemtl' NAME;

// Specifies the shadow object filename. This object is used to cast
// shadows for the current object.
// The shadow object is invisible except for its shadow.
// An object will cast shadows only if it has a shadow object.
// You can use an object as its own shadow object.
// However, a simplified version of the original object is usually
// preferable for shadow objects, since shadow casting can greatly
// increase rendering time.
// - filename is the filename for the shadow object.
//   The object file can be an .obj or .mod file. If a filename is
//   given without an extension, an extension of .obj is assumed.
// If more than one shadow object is specified, the last one specified
// will be used.
shadow_object : 'shadow_obj' filename=FILENAME;


// Specifies the ray tracing object filename.
// This object will be used in generating reflections of the current
// object on reflective surfaces.
// An object will appear in reflections only if it has a trace object. You
// can use an object as its own trace object.
// However, a simplified version of the original object is usually preferable
// for trace objects, since ray tracing can greatly increase rendering time.
// - filename is the filename for the ray tracing object.
//   The object file can be an .obj or .mod file. If a filename is given
//   without an extension, an extension of .obj is assumed.
// If more than one trace object is specified, the last one specified
// will be used.
trace_object : 'trace_obj' filename=FILENAME;

// Specifies a curve approximation technique. The arguments
// specify the technique and resolution for the curve.
curve_approximation_technique
    : 'ctech'
      ( 'cparm' res=decimal
      | 'cspace' maxlength=decimal
      | 'curv' maxdist=decimal maxangle=decimal
      )
    ;

// Specifies a surface approximation technique. The arguments
// specify the technique and resolution for the surface.
surface_approximation_technique
    : 'stech'
      ( 'cparma' ures=decimal vres=decimal
      | 'cparmb' uvres=decimal
      | 'cspace' maxlength=decimal
      | 'curv' maxdist=decimal maxangle=decimal
      )
    ;


// Compound type of decimal or integer, because any decimal values can accept integers.
decimal: ( DECIMAL | INTEGER );


// LEXER RULES

// Pair of vertex and texture index, separated by a slash.
INTEGER_PAIR : INTEGER '/' INTEGER;

// Triplet of vertex, texture, normal index, separated by slashes.
// Texture index can be omitted from the middle.
INTEGER_TRIPLET : INTEGER '/' INTEGER? '/' INTEGER;

INTEGER : '-'? ( DIGIT )+;

DECIMAL : INTEGER ( '.' DIGIT* )?;

fragment DIGIT : '0' .. '9';

COMMENT : '#' NON_NL* (NL|EOF) -> skip;

// Names of identifiers specified in this file format and other related formats.
// Possibly more lenient than desired. Officially, for example,
// "Letters, numbers, and combinations of letters and numbers are accepted for group names."
NAME : ( 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '-' | '_' | '~' | '(' | ')' )+;

// While we could carefully ban all invalid filenames, the rules for this are complex.
FILENAME: ( ~[/ \t\r\n] )+;

WS : ( ' ' | '\t' | '\\' NL )+ -> skip;
NL : ( '\r' '\n'? | '\n' );
NON_NL : ~[\r\n];
NON_WS : ( ~[ \t\r\n] )+;


