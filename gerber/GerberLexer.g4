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
lexer grammar GerberLexer;

tokens { FIELD }
DOLLAR
   : '$'
   ;

PCT
   : '%'
   ;

STAR_PCT
   : '*%'
   ;

STAR
   : '*'
   ;

COMMA
   : ','
   ;

EQ
   : '='
   ;

AB
   : 'AB'
   ;

AD
   : 'AD'
   ;

AM
   : 'AM'
   ;

D01
   : 'D01'
   ;

D02
   : 'D02'
   ;

D03
   : 'D03'
   ;

FS
   : 'FS'
   ;

G01
   : 'G01'
   ;

G02
   : 'G02'
   ;

G03
   : 'G03'
   ;

G36
   : 'G36'
   ;

G37
   : 'G37'
   ;

G75
   : 'G75'
   ;

LA
   : 'LA'
   ;

LM
   : 'LM'
   ;

LP
   : 'LP'
   ;

LR
   : 'LR'
   ;

LS
   : 'LS'
   ;

M02
   : 'M02'
   ;

MM
   : 'MM'
   ;

MO
   : 'MO'
   ;

IN_
   : 'IN'
   ;

SR
   : 'SR'
   ;

XY
   : 'XY'
   ;

C
   : 'C'
   ;

D
   : 'D'
   ;

I
   : 'I'
   ;

J
   : 'J'
   ;

N
   : 'N'
   ;

O
   : 'O'
   ;

P
   : 'P'
   ;

R
   : 'R'
   ;

X
   : 'X'
   ;

Y
   : 'Y'
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

INTEGER
   : [+-]? [0-9]+
   ;

DEICMAL
   : [+-]? ((([0-9]+) ('.' [0-9]*)?) | ('.' [0-9]+))
   ;

APERTURE_INDENT
   : 'D' [0-9] [0-9]+
   ;

NAME
   : [._a-z$] [._a-zA-Z0-9]*
   ;

G04
   : 'G04' -> pushMode (COMMENT_MODE)
   ;

TF
   : 'TF' -> pushMode (ATTR_MODE)
   ;

TA
   : 'TA' -> pushMode (ATTR_MODE)
   ;

TO_
   : 'TO' -> pushMode (ATTR_MODE)
   ;

TD
   : 'TD' -> pushMode (ATTR_MODE)
   ;

WS
   : [ \r\n\t]+ -> skip
   ;

mode COMMENT_MODE;
COMMENT_FIELD
   : ~ [%*]+ -> type (FIELD)
   ;

COMMENT_CLOSE
   : '*' -> type (STAR) , popMode
   ;

mode ATTR_MODE;
ATTR_COMMA
   : ',' -> type (COMMA)
   ;

PART_ATT
   : '.Part'
   ;

FILEFUNCTION_ATT
   : '.FileFunction'
   ;

FILEPOLARITY_ATT
   : '.FilePolarity'
   ;

SAMECOORDINATES_ATT
   : '.SameCoordinates'
   ;

CREATIONDATE_ATT
   : '.CreationDate'
   ;

GENERATIONSOFTWARE_ATT
   : '.GenerationSoftware'
   ;

PROJECTID_ATT
   : '.ProjectId'
   ;

MD5_ATT
   : '.MD5'
   ;

APERFUNCTION_ATT
   : '.AperFunction'
   ;

DRILLTOLERANCE_ATT
   : '.DrillTolerance'
   ;

FLASHTEXT_ATT
   : '.FlashText'
   ;

N_ATT
   : '.N'
   ;

P_ATT
   : '.P'
   ;

CSUP_ATT
   : '.CSup'
   ;

C_ATT
   : '.C'
   ;

CROT_ATT
   : '.CRot'
   ;

CMFR_ATT
   : '.CMfr'
   ;

CMPN_ATT
   : '.CMPN'
   ;

CVAL_ATT
   : '.CVal'
   ;

CMNT_ATT
   : '.CMnt'
   ;

CFTP_ATT
   : '.CFtp'
   ;

CPGN_ATT
   : '.CPgN'
   ;

CPGD_ATT
   : '.CPgD'
   ;

CHGT_ATT
   : '.CHgt'
   ;

CLBN_ATT
   : '.CLbN'
   ;

CLBD_ATT
   : '.CLbD'
   ;

USER_NAME
   : [_a-z$] [._a-zA-Z0-9]*
   ;

ATTR_FIELD
   : ~ [%*,]+ -> type (FIELD)
   ;

ATTR_CLOSE
   : '*%' -> type (STAR_PCT) , popMode
   ;

