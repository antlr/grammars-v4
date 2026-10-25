/*
BSD License

Copyright (c) 2026, Tom Everett
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
parser grammar GerberParser;


options { tokenVocab = GerberLexer; }
file_
   : statement+ m02 EOF
   ;

statement
   : single_statement
   | compound_statement
   ;

single_statement
   : operation
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
   : region_statement
   | sr_statement
   | ab_statement
   ;

coordinate_command
   : fs
   | mo
   ;

operation
   : d01
   | d02
   | d03
   ;

interpolation_state_command
   : g01
   | g02
   | g03
   | g75
   ;

transformation_state_command
   : lp
   | lm
   | lr
   | ls
   ;

attribute_command
   : to
   | td
   | ta
   | tf
   ;

fs
   : PCT (FS LA X COORDINATE_DIGITS Y COORDINATE_DIGITS) STAR_PCT
   ;

mo
   : PCT (MO (MM | IN_)) STAR_PCT
   ;

d01
   : ((X INTEGER)? (Y INTEGER)? (I INTEGER J INTEGER)? D01) STAR
   ;

d02
   : ((X INTEGER)? (Y INTEGER)? D02) STAR
   ;

d03
   : ((X INTEGER)? (Y INTEGER)? D03) STAR
   ;

g01
   : (G01) STAR
   ;

g02
   : (G02) STAR
   ;

g03
   : (G03) STAR
   ;

g75
   : (G75) STAR
   ;

dnn
   : (APERTURE_INDENT) STAR
   ;

g04
   : (G04 FIELD) STAR
   ;

m02
   : (M02) STAR
   ;

lp
   : PCT (LP (C | D)) STAR_PCT
   ;

lm
   : PCT (LM (N | XY | Y | X)) STAR_PCT
   ;

lr
   : PCT (LR DEICMAL) STAR_PCT
   ;

ls
   : PCT (LS DEICMAL) STAR_PCT
   ;

ad
   : PCT (AD APERTURE_INDENT template_call) STAR_PCT
   ;

template_call
   : C fst_par nxt_par?
   | R fst_par nxt_par nxt_par?
   | O fst_par nxt_par nxt_par?
   | P fst_par nxt_par (nxt_par nxt_par?)?
   | NAME fst_par*
   ;

fst_par
   : COMMA (DEICMAL)
   ;

nxt_par
   : X (DEICMAL)
   ;

am
   : PCT (AM macro_name macro_body) PCT
   ;

macro_name
   : NAME STAR
   ;

macro_body
   : in_macro_block+
   ;

in_macro_block
   : primitive
   | variable_definition
   ;

variable_definition
   : macro_variable EQ expression STAR
   ;

macro_variable
   : DOLLAR INTEGER
   ;

primitive
   : (INTEGER par par par par par?) STAR
   | (INTEGER par par par par par par par) STAR
   | (INTEGER par par par par par par) STAR
   | (INTEGER par par par par (par par)+ par) STAR
   ;

par
   : COMMA expression
   ;

region_statement
   : g36 (contour)+ g37
   ;

contour
   : d02 (d01 | interpolation_state_command)*
   ;

g36
   : G36 STAR
   ;

g37
   : G37 STAR
   ;

ab_statement
   : ab_open in_block_statement* ab_close
   ;

ab_open
   : PCT (AB APERTURE_INDENT) STAR_PCT
   ;

ab_close
   : PCT (AB) STAR_PCT
   ;

sr_statement
   : sr_open in_block_statement* sr_close
   ;

sr_open
   : PCT (SR X INTEGER Y INTEGER I DEICMAL J DEICMAL) STAR_PCT
   ;

sr_close
   : PCT (SR) STAR_PCT
   ;

in_block_statement
   : single_statement
   | region_statement
   ;

tf
   : PCT (TF tf_atts) STAR_PCT
   ;

ta
   : PCT (TA ta_atts) STAR_PCT
   ;

to
   : PCT (TO_ to_atts) STAR_PCT
   ;

td
   : PCT (TD all_atts?) STAR_PCT
   ;

tf_atts
   : PART_ATT nxt_field
   | FILEFUNCTION_ATT nxt_field*
   | FILEPOLARITY_ATT nxt_field
   | SAMECOORDINATES_ATT nxt_field?
   | CREATIONDATE_ATT nxt_field
   | GENERATIONSOFTWARE_ATT nxt_field nxt_field nxt_field?
   | PROJECTID_ATT nxt_field nxt_field nxt_field
   | MD5_ATT nxt_field
   | USER_NAME
   ;

ta_atts
   : APERFUNCTION_ATT nxt_field*
   | DRILLTOLERANCE_ATT nxt_field nxt_field
   | FLASHTEXT_ATT nxt_field*
   | USER_NAME nxt_field*
   ;

to_atts
   : N_ATT nxt_field nxt_field*
   | P_ATT nxt_field nxt_field nxt_field?
   | C_ATT nxt_field
   | CROT_ATT nxt_field
   | CMFR_ATT nxt_field
   | CMPN_ATT nxt_field
   | CVAL_ATT nxt_field
   | CMNT_ATT nxt_field
   | CFTP_ATT nxt_field
   | CPGN_ATT nxt_field
   | CPGD_ATT nxt_field
   | CHGT_ATT nxt_field
   | CLBN_ATT nxt_field
   | CLBD_ATT nxt_field
   | CSUP_ATT nxt_field nxt_field (nxt_field nxt_field)*
   | USER_NAME nxt_field*
   ;

all_atts
   : tf_atts
   | ta_atts
   | to_atts
   ;

nxt_field
   : COMMA FIELD
   ;

expression
   : (ADDSUB_OPERATOR term)+
   | expression ADDSUB_OPERATOR term
   | term
   ;

term
   : term MULDIV_OPERATOR factor
   | factor
   ;

factor
   : macro_variable
   | DEICMAL
   ;

