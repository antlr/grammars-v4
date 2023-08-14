/*
 [The "BSD licence"] Copyright (c) 2023 Nikolay Fiykov All rights reserved.
 
 Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met: 1. Redistributions of source code must retain the above
 copyright notice, this list of conditions and the following disclaimer. 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following
 disclaimer in the documentation and/or other materials provided with the distribution. 3. The name of the author may not be used to endorse or promote products derived from this software without
 specific prior written permission.
 
 THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
 GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
 OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
/*
 * For parsing propeties file (like GeoTools epsg.properties), use starting rule "propsFile".
 * For parsing single WKT CRS definition, use starting rule "wkt".
 */
grammar wktcrsv1;

propsFile: propRow* EOF;

propRow:     commentLine | epsgDefLine;
commentLine: COMMENT_LINE;
epsgDefLine: epsgCode EQ wkt;

wkt:           compdcs | projcs | geogcs | vertcs | geoccs | localcs;
compdcs:       COMPD_CS LPAR name COMMA (projcs | geogcs) COMMA vertcs COMMA authority RPAR;
projcs:        PROJCS LPAR name COMMA geogcs COMMA projection COMMA (parameter COMMA)+ unit COMMA (axis COMMA)* authority RPAR;
geoccs:        GEOCCS LPAR name COMMA datum COMMA primem COMMA unit COMMA (axis COMMA)+ authority RPAR;
geogcs:        GEOGCS LPAR name COMMA datum COMMA primem COMMA unit (COMMA axis)* (COMMA authority)? RPAR;
vertcs:        VERT_CS LPAR name COMMA vertdatum COMMA unit COMMA axis COMMA authority RPAR;
localcs:       LOCAL_CS LPAR name COMMA localdatum COMMA unit COMMA (axis COMMA)+ authority RPAR;
datum:         DATUM LPAR name COMMA spheroid (COMMA towgs84)? (COMMA authority)? RPAR;
vertdatum:     VERT_DATUM LPAR name COMMA type COMMA authority RPAR;
localdatum:    LOCAL_DATUM LPAR name COMMA type (COMMA authority)? RPAR;
spheroid:      SPHEROID LPAR name COMMA semiMajorAxis COMMA inverseFlattening (COMMA authority)? RPAR;
towgs84:       TOWGS84 LPAR dXBF COMMA dYBF COMMA dZBF (COMMA rXBF COMMA rYBF COMMA rZBF (COMMA mBF)?)? RPAR;
authority:     AUTHORITY LPAR authorityName COMMA code RPAR;
primem:        PRIMEM LPAR name COMMA longitude (COMMA authority)? RPAR;
unit:          UNIT LPAR name COMMA angularUnit (COMMA authority)? RPAR;
axis:          AXIS LPAR name COMMA axisOrient RPAR;
projection:    PROJECTION LPAR name (COMMA authority)? RPAR;
parameter:     PARAMETER LPAR name COMMA value RPAR;
authorityName: '"EPSG"' | '"ESRI"';
axisOrient:
    'EAST'
    | 'WEST'
    | 'NORTH'
    | 'SOUTH'
    | 'NORTH_EAST'
    | 'NORTH_WEST'
    | 'UP'
    | 'DOWN'
    | 'GEOCENTRIC_X'
    | 'GEOCENTRIC_Y'
    | 'GEOCENTRIC_Z'
    | name;

epsgCode:          PKEY | NUMBER;
name:              TEXT;
number:            NUMBER;
type:              NUMBER;
semiMajorAxis:     NUMBER;
inverseFlattening: NUMBER;
dXBF:              NUMBER;
dYBF:              NUMBER;
dZBF:              NUMBER;
rXBF:              NUMBER;
rYBF:              NUMBER;
rZBF:              NUMBER;
mBF:               NUMBER;
code:              TEXT;
longitude:         NUMBER;
angularUnit:       NUMBER;
value:             NUMBER;

COMPD_CS:    C O M P D '_' C S;
PROJCS:      P R O J C S;
GEOGCS:      G E O G C S;
DATUM:       D A T U M;
SPHEROID:    S P H E R O I D;
TOWGS84:     T O W G S '84';
AUTHORITY:   A U T H O R I T Y;
PRIMEM:      P R I M E M;
UNIT:        U N I T;
AXIS:        A X I S;
PROJECTION:  P R O J E C T I O N;
PARAMETER:   P A R A M E T E R;
VERT_CS:     V E R T '_' C S;
VERT_DATUM:  V E R T '_' D A T U M;
GEOCCS:      G E O C C S;
LOCAL_CS:    L O C A L '_' C S;
LOCAL_DATUM: L O C A L '_' D A T U M;

NUMBER:       PM? INT ('.' INT)? EXP?;
TEXT:         '"' ('""' | ~'"')* '"';
PKEY:         [A-Z] [0-9A-Za-z]+;
COMMENT_LINE: '#' ~[\r\n]*;
WS:           [ \r\n\t]+ -> skip;

COMMA: ',';
LPAR:  '[' | '(';
RPAR:  ']' | ')';
EQ:    '=';

fragment INT: [0-9]+;
fragment EXP: [Ee] PM? INT;
fragment PM:  '+' | '-';

fragment A: [aA];
fragment B: [bB];
fragment C: [cC];
fragment D: [dD];
fragment E: [eE];
fragment F: [fF];
fragment G: [gG];
fragment H: [hH];
fragment I: [iI];
fragment J: [jJ];
fragment K: [kK];
fragment L: [lL];
fragment M: [mM];
fragment N: [nN];
fragment O: [oO];
fragment P: [pP];
fragment Q: [qQ];
fragment R: [rR];
fragment S: [sS];
fragment T: [tT];
fragment U: [uU];
fragment V: [vV];
fragment W: [wW];
fragment X: [xX];
fragment Y: [yY];
fragment Z: [zZ];
