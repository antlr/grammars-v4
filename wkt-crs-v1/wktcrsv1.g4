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
 * For parsing propeties file (like GeoTools epsg.properties), use starting rule "propsFile". For parsing single WKT CRS definition, use starting rule "wkt".
 */
grammar wktcrsv1;

propsFile: propRow* EOF;

propRow:     commentLine | epsgDefLine;
commentLine: COMMENT_LINE;
epsgDefLine: epsgCode EQ wkt;

wkt:           compdcs | projcs | geogcs | vertcs | geoccs | localcs;
compdcs:       'COMPD_CS' LPAR name COMMA (projcs | geogcs) COMMA vertcs COMMA authority RPAR;
projcs:        'PROJCS' LPAR name COMMA geogcs COMMA projection COMMA (parameter COMMA)+ unit COMMA (axis COMMA)* authority RPAR;
geoccs:        'GEOCCS' LPAR name COMMA datum COMMA primem COMMA unit COMMA (axis COMMA)+ authority RPAR;
geogcs:        'GEOGCS' LPAR name COMMA datum COMMA primem COMMA unit (COMMA axis)* (COMMA authority)? RPAR;
vertcs:        'VERT_CS' LPAR name COMMA vertdatum COMMA unit COMMA axis COMMA authority RPAR;
localcs:       'LOCAL_CS' LPAR name COMMA localdatum COMMA unit COMMA (axis COMMA)+ authority RPAR;
datum:         'DATUM' LPAR name COMMA spheroid (COMMA towgs84)? (COMMA authority)? RPAR;
vertdatum:     'VERT_DATUM' LPAR name COMMA type COMMA authority RPAR;
localdatum:    'LOCAL_DATUM' LPAR name COMMA type (COMMA authority)? RPAR;
spheroid:      'SPHEROID' LPAR name COMMA semiMajorAxis COMMA inverseFlattening (COMMA authority)? RPAR;
towgs84:       'TOWGS84' LPAR dx COMMA dy COMMA dz (COMMA ex COMMA ey COMMA ez (COMMA ppm)?)? RPAR;
authority:     'AUTHORITY' LPAR authorityName COMMA code RPAR;
primem:        'PRIMEM' LPAR name COMMA longitude (COMMA authority)? RPAR;
unit:          'UNIT' LPAR name COMMA conversionFactor (COMMA authority)? RPAR;
axis:          'AXIS' LPAR name COMMA axisOrient RPAR;
projection:    'PROJECTION' LPAR name (COMMA authority)? RPAR;
parameter:     'PARAMETER' LPAR name COMMA value RPAR;
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
dx:              NUMBER;
dy:              NUMBER;
dz:              NUMBER;
ex:              NUMBER;
ey:              NUMBER;
ez:              NUMBER;
ppm:               NUMBER;
code:              TEXT;
longitude:         NUMBER;
conversionFactor:  NUMBER;
value:             NUMBER;

NUMBER:       PM? INT ('.' INT)? EXP?;
TEXT:         '"' ('""' | ~'"')* '"';
PKEY:         [A-Z] [0-9A-Z]+;
COMMENT_LINE: '#' ~[\r\n]*;
WS:           [ \r\n\t]+ -> skip;

COMMA: ',';
LPAR:  '[' | '(';
RPAR:  ']' | ')';
EQ:    '=';

fragment INT: [0-9]+;
fragment EXP: [Ee] PM? INT;
fragment PM:  '+' | '-';
