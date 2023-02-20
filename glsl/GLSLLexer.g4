/*
MIT License

Copyright (c) 2022 Mustafa Said Ağca

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

lexer grammar GLSLLexer;
channels { COMMENTS , DIRECTIVES }

ATOMIC_UINT : 'atomic_uint' ;
ATTRIBUTE : 'attribute' ;
BOOL : 'bool' ;
BREAK : 'break' ;
BUFFER : 'buffer' ;
BVEC2 : 'bvec2' ;
BVEC3 : 'bvec3' ;
BVEC4 : 'bvec4' ;
CASE : 'case' ;
CENTROID : 'centroid' ;
COHERENT : 'coherent' ;
CONST : 'const' ;
CONTINUE : 'continue' ;
DEFAULT : 'default' ;
DISCARD : 'discard' ;
DMAT2 : 'dmat2' ;
DMAT2X2 : 'dmat2x2' ;
DMAT2X3 : 'dmat2x3' ;
DMAT2X4 : 'dmat2x4' ;
DMAT3 : 'dmat3' ;
DMAT3X2 : 'dmat3x2' ;
DMAT3X3 : 'dmat3x3' ;
DMAT3X4 : 'dmat3x4' ;
DMAT4 : 'dmat4' ;
DMAT4X2 : 'dmat4x2' ;
DMAT4X3 : 'dmat4x3' ;
DMAT4X4 : 'dmat4x4' ;
DO : 'do' ;
DOUBLE : 'double' ;
DVEC2 : 'dvec2' ;
DVEC3 : 'dvec3' ;
DVEC4 : 'dvec4' ;
ELSE : 'else' ;
FALSE : 'false' ;
FLAT : 'flat' ;
FLOAT : 'float' ;
FOR : 'for' ;
HIGHP : 'highp' ;
IF : 'if' ;
IIMAGE1D : 'iimage1D' ;
IIMAGE1DARRAY : 'iimage1DArray' ;
IIMAGE2D : 'iimage2D' ;
IIMAGE2DARRAY : 'iimage2DArray' ;
IIMAGE2DMS : 'iimage2DMS' ;
IIMAGE2DMSARRAY : 'iimage2DMSArray' ;
IIMAGE2DRECT : 'iimage2DRect' ;
IIMAGE3D : 'iimage3D' ;
IIMAGEBUFFER : 'iimageBuffer' ;
IIMAGECUBE : 'iimageCube' ;
IIMAGECUBEARRAY : 'iimageCubeArray' ;
IMAGE1D : 'image1D' ;
IMAGE1DARRAY : 'image1DArray' ;
IMAGE2D : 'image2D' ;
IMAGE2DARRAY : 'image2DArray' ;
IMAGE2DMS : 'image2DMS' ;
IMAGE2DMSARRAY : 'image2DMSArray' ;
IMAGE2DRECT : 'image2DRect' ;
IMAGE3D : 'image3D' ;
IMAGEBUFFER : 'imageBuffer' ;
IMAGECUBE : 'imageCube' ;
IMAGECUBEARRAY : 'imageCubeArray' ;
IN : 'in' ;
INOUT : 'inout' ;
INT : 'int' ;
INVARIANT : 'invariant' ;
ISAMPLER1D : 'isampler1D' ;
ISAMPLER1DARRAY : 'isampler1DArray' ;
ISAMPLER2D : 'isampler2D' ;
ISAMPLER2DARRAY : 'isampler2DArray' ;
ISAMPLER2DMS : 'isampler2DMS' ;
ISAMPLER2DMSARRAY : 'isampler2DMSArray' ;
ISAMPLER2DRECT : 'isampler2DRect' ;
ISAMPLER3D : 'isampler3D' ;
ISAMPLERBUFFER : 'isamplerBuffer' ;
ISAMPLERCUBE : 'isamplerCube' ;
ISAMPLERCUBEARRAY : 'isamplerCubeArray' ;
ISUBPASSINPUT : 'isubpassInput' ;
ISUBPASSINPUTMS : 'isubpassInputMS' ;
ITEXTURE1D : 'itexture1D' ;
ITEXTURE1DARRAY : 'itexture1DArray' ;
ITEXTURE2D : 'itexture2D' ;
ITEXTURE2DARRAY : 'itexture2DArray' ;
ITEXTURE2DMS : 'itexture2DMS' ;
ITEXTURE2DMSARRAY : 'itexture2DMSArray' ;
ITEXTURE2DRECT : 'itexture2DRect' ;
ITEXTURE3D : 'itexture3D' ;
ITEXTUREBUFFER : 'itextureBuffer' ;
ITEXTURECUBE : 'itextureCube' ;
ITEXTURECUBEARRAY : 'itextureCubeArray' ;
IVEC2 : 'ivec2' ;
IVEC3 : 'ivec3' ;
IVEC4 : 'ivec4' ;
LAYOUT : 'layout' ;
LOWP : 'lowp' ;
MAT2 : 'mat2' ;
MAT2X2 : 'mat2x2' ;
MAT2X3 : 'mat2x3' ;
MAT2X4 : 'mat2x4' ;
MAT3 : 'mat3' ;
MAT3X2 : 'mat3x2' ;
MAT3X3 : 'mat3x3' ;
MAT3X4 : 'mat3x4' ;
MAT4 : 'mat4' ;
MAT4X2 : 'mat4x2' ;
MAT4X3 : 'mat4x3' ;
MAT4X4 : 'mat4x4' ;
MEDIUMP : 'mediump' ;
NOPERSPECTIVE : 'noperspective' ;
OUT : 'out' ;
PATCH : 'patch' ;
PRECISE : 'precise' ;
PRECISION : 'precision' ;
READONLY : 'readonly' ;
RESTRICT : 'restrict' ;
RETURN : 'return' ;
SAMPLE : 'sample' ;
SAMPLER : 'sampler' ;
SAMPLER1D : 'sampler1D' ;
SAMPLER1DARRAY : 'sampler1DArray' ;
SAMPLER1DARRAYSHADOW : 'sampler1DArrayShadow' ;
SAMPLER1DSHADOW : 'sampler1DShadow' ;
SAMPLER2D : 'sampler2D' ;
SAMPLER2DARRAY : 'sampler2DArray' ;
SAMPLER2DARRAYSHADOW : 'sampler2DArrayShadow' ;
SAMPLER2DMS : 'sampler2DMS' ;
SAMPLER2DMSARRAY : 'sampler2DMSArray' ;
SAMPLER2DRECT : 'sampler2DRect' ;
SAMPLER2DRECTSHADOW : 'sampler2DRectShadow' ;
SAMPLER2DSHADOW : 'sampler2DShadow' ;
SAMPLER3D : 'sampler3D' ;
SAMPLERBUFFER : 'samplerBuffer' ;
SAMPLERCUBE : 'samplerCube' ;
SAMPLERCUBEARRAY : 'samplerCubeArray' ;
SAMPLERCUBEARRAYSHADOW : 'samplerCubeArrayShadow' ;
SAMPLERCUBESHADOW : 'samplerCubeShadow' ;
SAMPLERSHADOW : 'samplerShadow' ;
SHARED : 'shared' ;
SMOOTH : 'smooth' ;
STRUCT : 'struct' ;
SUBPASSINPUT : 'subpassInput' ;
SUBPASSINPUTMS : 'subpassInputMS' ;
SUBROUTINE : 'subroutine' ;
SWITCH : 'switch' ;
TEXTURE1D : 'texture1D' ;
TEXTURE1DARRAY : 'texture1DArray' ;
TEXTURE2D : 'texture2D' ;
TEXTURE2DARRAY : 'texture2DArray' ;
TEXTURE2DMS : 'texture2DMS' ;
TEXTURE2DMSARRAY : 'texture2DMSArray' ;
TEXTURE2DRECT : 'texture2DRect' ;
TEXTURE3D : 'texture3D' ;
TEXTUREBUFFER : 'textureBuffer' ;
TEXTURECUBE : 'textureCube' ;
TEXTURECUBEARRAY : 'textureCubeArray' ;
TRUE : 'true' ;
UIMAGE1D : 'uimage1D' ;
UIMAGE1DARRAY : 'uimage1DArray' ;
UIMAGE2D : 'uimage2D' ;
UIMAGE2DARRAY : 'uimage2DArray' ;
UIMAGE2DMS : 'uimage2DMS' ;
UIMAGE2DMSARRAY : 'uimage2DMSArray' ;
UIMAGE2DRECT : 'uimage2DRect' ;
UIMAGE3D : 'uimage3D' ;
UIMAGEBUFFER : 'uimageBuffer' ;
UIMAGECUBE : 'uimageCube' ;
UIMAGECUBEARRAY : 'uimageCubeArray' ;
UINT : 'uint' ;
UNIFORM : 'uniform' ;
USAMPLER1D : 'usampler1D' ;
USAMPLER1DARRAY : 'usampler1DArray' ;
USAMPLER2D : 'usampler2D' ;
USAMPLER2DARRAY : 'usampler2DArray' ;
USAMPLER2DMS : 'usampler2DMS' ;
USAMPLER2DMSARRAY : 'usampler2DMSArray' ;
USAMPLER2DRECT : 'usampler2DRect' ;
USAMPLER3D : 'usampler3D' ;
USAMPLERBUFFER : 'usamplerBuffer' ;
USAMPLERCUBE : 'usamplerCube' ;
USAMPLERCUBEARRAY : 'usamplerCubeArray' ;
USUBPASSINPUT : 'usubpassInput' ;
USUBPASSINPUTMS : 'usubpassInputMS' ;
UTEXTURE1D : 'utexture1D' ;
UTEXTURE1DARRAY : 'utexture1DArray' ;
UTEXTURE2D : 'utexture2D' ;
UTEXTURE2DARRAY : 'utexture2DArray' ;
UTEXTURE2DMS : 'utexture2DMS' ;
UTEXTURE2DMSARRAY : 'utexture2DMSArray' ;
UTEXTURE2DRECT : 'utexture2DRect' ;
UTEXTURE3D : 'utexture3D' ;
UTEXTUREBUFFER : 'utextureBuffer' ;
UTEXTURECUBE : 'utextureCube' ;
UTEXTURECUBEARRAY : 'utextureCubeArray' ;
UVEC2 : 'uvec2' ;
UVEC3 : 'uvec3' ;
UVEC4 : 'uvec4' ;
VARYING : 'varying' ;
VEC2 : 'vec2' ;
VEC3 : 'vec3' ;
VEC4 : 'vec4' ;
VOID : 'void' ;
VOLATILE : 'volatile' ;
WHILE : 'while' ;
WRITEONLY : 'writeonly' ;

ADD_ASSIGN : '+=' ;
AMPERSAND : '&' ;
AND_ASSIGN : '&=' ;
AND_OP : '&&' ;
BANG : '!' ;
CARET : '^' ;
COLON : ':' ;
COMMA : ',' ;
DASH : '-' ;
DEC_OP : '--' ;
DIV_ASSIGN : '/=' ;
DOT : '.' ;
EQ_OP : '==' ;
EQUAL : '=' ;
GE_OP : '>=' ;
INC_OP : '++' ;
LE_OP : '<=' ;
LEFT_ANGLE : '<' ;
LEFT_ASSIGN : '<<=' ;
LEFT_BRACE : '{' ;
LEFT_BRACKET : '[' ;
LEFT_OP : '<<' ;
LEFT_PAREN : '(' ;
MOD_ASSIGN : '%=' ;
MUL_ASSIGN : '*=' ;
NE_OP : '!=' ;
NUMBER_SIGN : '#' -> channel (DIRECTIVES) , pushMode (DIRECTIVE_MODE) ;
OR_ASSIGN : '|=' ;
OR_OP : '||' ;
PERCENT : '%' ;
PLUS : '+' ;
QUESTION : '?' ;
RIGHT_ANGLE : '>' ;
RIGHT_ASSIGN : '>>=' ;
RIGHT_BRACE : '}' ;
RIGHT_BRACKET : ']' ;
RIGHT_OP : '>>' ;
RIGHT_PAREN : ')' ;
SEMICOLON : ';' ;
SLASH : '/' ;
STAR : '*' ;
SUB_ASSIGN : '-=' ;
TILDE : '~' ;
VERTICAL_BAR : '|' ;
XOR_ASSIGN : '^=' ;
XOR_OP : '^^' ;

DOUBLECONSTANT : FRACTIONAL_CONSTANT EXPONENT_PART? DOUBLE_SUFFIX | DIGIT_SEQUENCE EXPONENT_PART DOUBLE_SUFFIX ;
FLOATCONSTANT : FRACTIONAL_CONSTANT EXPONENT_PART? FLOAT_SUFFIX? | DIGIT_SEQUENCE EXPONENT_PART FLOAT_SUFFIX? ;
INTCONSTANT : DECIMAL_CONSTANT | HEX_CONSTANT | OCTAL_CONSTANT ;
UINTCONSTANT : INTCONSTANT INTEGER_SUFFIX ;
BLOCK_COMMENT : '/*' .*? '*/' -> channel (COMMENTS) ;
LINE_COMMENT : '//' (~ [\r\n\\] | '\\' (NEWLINE | .))* -> channel (COMMENTS) ;
LINE_CONTINUATION : '\\' NEWLINE -> channel (HIDDEN) ;
IDENTIFIER : [a-zA-Z_] [a-zA-Z0-9_]* ;
WHITE_SPACE : [ \t\r\n]+ -> channel (HIDDEN) ;

mode DIRECTIVE_MODE;
DEFINE_DIRECTIVE : 'define' -> channel (DIRECTIVES) , mode (DEFINE_DIRECTIVE_MODE) ;
ELIF_DIRECTIVE : 'elif' -> channel (DIRECTIVES) , popMode , mode (ELIF_DIRECTIVE_MODE) ;
ELSE_DIRECTIVE : 'else' -> channel (DIRECTIVES) , popMode , mode (PROGRAM_TEXT_MODE) ;
ENDIF_DIRECTIVE : 'endif' -> channel (DIRECTIVES) , popMode , popMode , popMode ;
ERROR_DIRECTIVE : 'error' -> channel (DIRECTIVES) , mode (ERROR_DIRECTIVE_MODE) ;
EXTENSION_DIRECTIVE : 'extension' -> channel (DIRECTIVES) , mode (EXTENSION_DIRECTIVE_MODE) ;
IF_DIRECTIVE : 'if' -> channel (DIRECTIVES) , mode (IF_DIRECTIVE_MODE) ;
IFDEF_DIRECTIVE : 'ifdef' -> channel (DIRECTIVES) , mode (IFDEF_DIRECTIVE_MODE) ;
IFNDEF_DIRECTIVE : 'ifndef' -> channel (DIRECTIVES) , mode (IFDEF_DIRECTIVE_MODE) ;
LINE_DIRECTIVE : 'line' -> channel (DIRECTIVES) , mode (LINE_DIRECTIVE_MODE) ;
PRAGMA_DIRECTIVE : 'pragma' -> channel (DIRECTIVES) , mode (PRAGMA_DIRECTIVE_MODE) ;
UNDEF_DIRECTIVE : 'undef' -> channel (DIRECTIVES) , mode (UNDEF_DIRECTIVE_MODE) ;
VERSION_DIRECTIVE : 'version' -> channel (DIRECTIVES) , mode (VERSION_DIRECTIVE_MODE) ;
SPACE_TAB_0 : SPACE_TAB -> skip ;
NEWLINE_0 : NEWLINE -> skip , popMode ;

mode DEFINE_DIRECTIVE_MODE;
MACRO_NAME : IDENTIFIER MACRO_ARGS? -> channel (DIRECTIVES) , mode (MACRO_TEXT_MODE) ;
NEWLINE_1 : NEWLINE -> skip , popMode ;
SPACE_TAB_1 : SPACE_TAB -> skip ;

mode ELIF_DIRECTIVE_MODE;
CONSTANT_EXPRESSION : ~ [\r\n]+ -> channel (DIRECTIVES) ;
NEWLINE_2 : NEWLINE -> skip , mode (PROGRAM_TEXT_MODE) ;

mode ERROR_DIRECTIVE_MODE;
ERROR_MESSAGE : ~ [\r\n]+ -> channel (DIRECTIVES) ;
NEWLINE_3 : NEWLINE -> skip , popMode ;

mode EXTENSION_DIRECTIVE_MODE;
BEHAVIOR : ('require' | 'enable' | 'warn' | 'disable') -> channel (DIRECTIVES) ;
COLON_0 : COLON -> channel (DIRECTIVES) , type (COLON) ;
EXTENSION_NAME : IDENTIFIER -> channel (DIRECTIVES) ;
NEWLINE_4 : NEWLINE -> skip , popMode ;
SPACE_TAB_2 : SPACE_TAB -> skip ;

mode IF_DIRECTIVE_MODE;
CONSTANT_EXPRESSION_0 : CONSTANT_EXPRESSION -> channel (DIRECTIVES) , type (CONSTANT_EXPRESSION) ;
NEWLINE_5 : NEWLINE -> skip , pushMode (PROGRAM_TEXT_MODE) ;

mode IFDEF_DIRECTIVE_MODE;
MACRO_IDENTIFIER : IDENTIFIER -> channel (DIRECTIVES) ;
NEWLINE_6 : NEWLINE -> skip , pushMode (PROGRAM_TEXT_MODE) ;
SPACE_TAB_3 : SPACE_TAB -> skip ;

mode LINE_DIRECTIVE_MODE;
LINE_EXPRESSION : ~ [\r\n]+ -> channel (DIRECTIVES) ;
NEWLINE_7 : NEWLINE -> skip , mode (PROGRAM_TEXT_MODE) ;

mode MACRO_TEXT_MODE;
BLOCK_COMMENT_0 : BLOCK_COMMENT -> channel (COMMENTS) , type(BLOCK_COMMENT) ;
MACRO_ESC_NEWLINE : '\\' NEWLINE -> channel(DIRECTIVES) ;
MACRO_ESC_SEQUENCE : '\\' . -> channel(DIRECTIVES) , type (MACRO_TEXT) ;
MACRO_TEXT : ~ [/\r\n\\]+ -> channel (DIRECTIVES) ;
NEWLINE_8 : NEWLINE -> skip , popMode ;
SLASH_0 : SLASH -> more ;

mode PRAGMA_DIRECTIVE_MODE;
DEBUG : 'debug' -> channel (DIRECTIVES) ;
LEFT_PAREN_0 : LEFT_PAREN -> channel (DIRECTIVES) , type (LEFT_PAREN) ;
NEWLINE_9 : NEWLINE -> skip , popMode ;
OFF : 'off' -> channel (DIRECTIVES) ;
ON : 'on' -> channel (DIRECTIVES) ;
OPTIMIZE : 'optimize' -> channel (DIRECTIVES) ;
RIGHT_PAREN_0 : RIGHT_PAREN -> channel (DIRECTIVES) , type (RIGHT_PAREN) ;
SPACE_TAB_5 : SPACE_TAB -> skip ;
STDGL : 'STDGL' -> channel (DIRECTIVES) ;

mode PROGRAM_TEXT_MODE;
BLOCK_COMMENT_1 : BLOCK_COMMENT -> channel (COMMENTS) , type (BLOCK_COMMENT) ;
LINE_COMMENT_0 : LINE_COMMENT -> channel (COMMENTS) , type (LINE_COMMENT) ;
NUMBER_SIGN_0 : NUMBER_SIGN -> channel (DIRECTIVES) , type (NUMBER_SIGN) , pushMode (DIRECTIVE_MODE) ;
PROGRAM_TEXT : ~ [#/]+ -> channel (DIRECTIVES) ;
SLASH_1 : SLASH -> more ;

mode UNDEF_DIRECTIVE_MODE;
MACRO_IDENTIFIER_0 : MACRO_IDENTIFIER -> channel (DIRECTIVES) , type (MACRO_IDENTIFIER) ;
NEWLINE_10 : NEWLINE -> skip , popMode ;
SPACE_TAB_6 : SPACE_TAB -> skip ;

mode VERSION_DIRECTIVE_MODE;
NEWLINE_11 : NEWLINE -> skip , popMode ;
NUMBER : [0-9]+ -> channel (DIRECTIVES) ;
PROFILE : ('core' | 'compatibility' | 'es') -> channel (DIRECTIVES) ;
SPACE_TAB_7 : SPACE_TAB -> skip ;

fragment DECIMAL_CONSTANT : [1-9] [0-9]* ;
fragment DIGIT_SEQUENCE : [0-9]+ ;
fragment DOUBLE_SUFFIX : 'lf' | 'LF' ;
fragment EXPONENT_PART : [eE] [+\-]? DIGIT_SEQUENCE ;
fragment FLOAT_SUFFIX : [fF] ;
fragment FRACTIONAL_CONSTANT : '.' DIGIT_SEQUENCE | DIGIT_SEQUENCE '.' DIGIT_SEQUENCE? ;
fragment HEX_CONSTANT : '0' [xX] [0-9a-fA-F]+ ;
fragment INTEGER_SUFFIX : [uU] ;
fragment MACRO_ARGS : '(' (MACRO_ARGS | ~ [()])* ')' ;
fragment NEWLINE : '\r'? '\n' ;
fragment OCTAL_CONSTANT : '0' [0-7]* ;
fragment SPACE_TAB : [ \t]+ ;
