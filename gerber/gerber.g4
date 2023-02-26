/*
BSD License

Copyright (c) 2023, Tom Everett
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. Neither the name of Tom Everett nor the names of its contributors
   may be used to endorse or promote products derived from this software
   without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
grammar gerber;

file_
   : statement+ m02 EOF
   ;

statement
   :
   | single_statement
   | compound_statement
   ;

single_statement
   :
   | operation
   | interpolation_state_command
   | dnn
   | g04
   | attribute_command
   | ad
   | am
   | coordinate_command
   | transformation_state_command
   ;

compound_statement
   :
   | region_statement
   | sr_statement
   | ab_statement
   ;

coordinate_command
   :
   | fs
   | mo
   ;

operation
   :
   | d01
   | d02
   | d03
   ;

interpolation_state_command
   :
   | g01
   | g02
   | g03
   | g75
   ;

transformation_state_command
   :
   | lp
   | lm
   | lr
   | ls
   ;

attribute_command
   :
   | to
   | td
   | ta
   | tf
   ;

fs
   : '%' ('FS' 'LA' 'X' COORDINATE_DIGITS 'Y' COORDINATE_DIGITS) '*%'
   ;

mo
   : '%' ('MO' ('MM' | 'IN')) '*%'
   ;

d01
   : (('X' INTEGER)? ('Y' INTEGER)? ('I' INTEGER 'J' INTEGER)? 'D01') '*'
   ;

d02
   : (('X' INTEGER)? ('Y' INTEGER)? 'D02') '*'
   ;

d03
   : (('X' INTEGER)? ('Y' INTEGER)? 'D03') '*'
   ;

g01
   : ('G01') '*'
   ;

g02
   : ('G02') '*'
   ;

g03
   : ('G03') '*'
   ;

g75
   : ('G75') '*'
   ;

dnn
   : (APERTURE_INDENT) '*'
   ;

g04
   : ('G04' STRING) '*'
   ;

m02
   : ('M02') '*'
   ;

lp
   : '%' ('LP' ('C' | 'D')) '*%'
   ;

lm
   : '%' ('LM' ('N' | 'XY' | 'Y' | 'X')) '*%'
   ;

lr
   : '%' ('LR' DEICMAL) '*%'
   ;

ls
   : '%' ('LS' DEICMAL) '*%'
   ;

ad
   : '%' ('AD' APERTURE_INDENT template_call) '*%'
   ;
   //aperture_shape : name [',' decimal {'X' decimal}*];
   
template_call
   :
   | 'C' fst_par nxt_par?
   | 'R' fst_par nxt_par nxt_par?
   | 'O' fst_par nxt_par nxt_par?
   | 'P' fst_par nxt_par (nxt_par nxt_par?)?
   //   |!(('C'|'R'|'O'|'P')(','|'*')) name (fst_par {nxt_par}*)?
   
   ;

fst_par
   : ',' (DEICMAL)
   ;

nxt_par
   : 'X' (DEICMAL)
   ;

am
   : '%' ('AM' macro_name macro_body) '%'
   ;

macro_name
   : NAME '*'
   ;

macro_body
   : in_macro_block+
   ;

in_macro_block
   :
   | primitive
   | variable_definition
   ;

variable_definition
   : macro_variable '=' expression '*'
   ;

macro_variable
   : '$' POSITIVE_INTEGER
   ;

primitive
   :
   | ('0' STRING) '*'
   | ('1' par par par par par?) '*'
   | ('20' par par par par par par par) '*'
   | ('21' par par par par par par) '*'
   | ('4' par par par par (par par)+ par) '*'
   | ('5' par par par par par par) '*'
   | ('7' par par par par par par) '*'
   ;

par
   : ',' expression
   ;

region_statement
   : g36 (contour)+ g37
   ;

contour
   : d02 (d01 | interpolation_state_command)*
   ;

g36
   : 'G36' '*'
   ;

g37
   : 'G37' '*'
   ;

ab_statement
   : ab_open in_block_statement* ab_close
   ;

ab_open
   : '%' ('AB' APERTURE_INDENT) '*%'
   ;

ab_close
   : '%' ('AB') '*%'
   ;

sr_statement
   : sr_open in_block_statement* sr_close
   ;

sr_open
   : '%' ('SR' 'X' POSITIVE_INTEGER 'Y' POSITIVE_INTEGER 'I' DEICMAL 'J' DEICMAL) '*%'
   ;

sr_close
   : '%' ('SR') '*%'
   ;

in_block_statement
   :
   | single_statement
   | region_statement
   | ab_statement
   ;

tf
   : '%' ('TF' tf_atts) '*%'
   ;

ta
   : '%' ('TA' ta_atts) '*%'
   ;

to
   : '%' ('TO' to_atts) '*%'
   ;

td
   : '%' ('TD' all_atts?) '*%'
   ;

tf_atts
   :
   | '.Part' nxt_field
   | '.FileFunction' nxt_field*
   | '.FilePolarity' nxt_field
   | '.SameCoordinates' nxt_field?
   | '.CreationDate' nxt_field
   | '.GenerationSoftware' nxt_field nxt_field nxt_field?
   | '.ProjectId' nxt_field nxt_field nxt_field
   | '.MD5' nxt_field
   | USER_NAME
   ;

ta_atts
   :
   | '.AperFunction' nxt_field*
   | '.DrillTolerance' nxt_field nxt_field
   | '.FlashText' nxt_field*
   | USER_NAME nxt_field*
   ;

to_atts
   :
   | '.N' nxt_field nxt_field*
   | '.P' nxt_field nxt_field nxt_field?
   | '.C' nxt_field
   | '.CRot' nxt_field
   | '.CMfr' nxt_field
   | '.CMPN' nxt_field
   | '.CVal' nxt_field
   | '.CMnt' nxt_field
   | '.CFtp' nxt_field
   | '.CPgN' nxt_field
   | '.CPgD' nxt_field
   | '.CHgt' nxt_field
   | '.CLbN' nxt_field
   | '.CLbD' nxt_field
   | '.CSup' nxt_field nxt_field (nxt_field nxt_field)*
   | USER_NAME nxt_field*
   ;

all_atts
   :
   | tf_atts
   | ta_atts
   | to_atts
   ;

nxt_field
   : ',' FIELD
   ;

expression
   :
   | (ADDSUB_OPERATOR term)+
   | expression ADDSUB_OPERATOR term
   | term
   ;

term
   :
   | term MULDIV_OPERATOR factor
   | factor
   ;

factor
   :
   //   | '(' ~ expression ')'
   | macro_variable
   | UNSIGNED_DECIMAL
   ;

COORDINATE_DIGITS
   : [1-6] [56]
   ;

ADDSUB_OPERATOR
   : [+-]
   ;

MULDIV_OPERATOR
   : [x/]
   ;

UNSIGNED_INTEGER
   : [0-9]+
   ;

POSITIVE_INTEGER
   : [0-9]* [1-9] [0-9]*
   ;

INTEGER
   : [+-]? [0-9]+
   ;

UNSIGNED_DECIMAL
   : ((([0-9]+) ('.' [0-9]*)?) | ('.' [0-9]+))
   ;

DEICMAL
   : [+-]? ((([0-9]+) ('.' [0-9]*)?) | ('.' [0-9]+))
   ;

APERTURE_INDENT
   : 'D' [0-9]
   {2,}
   ;

NAME
   : [._a-zA-Z$] [._a-zA-Z0-9]*
   ;

USER_NAME
   : [_a-zA-Z$] [._a-zA-Z0-9]*
   ;

STRING
   : [^%*]+
   ;

FIELD
   : [^%*,]+
   ;

WS
   : [ \r\n\t]+ -> skip
   ;

