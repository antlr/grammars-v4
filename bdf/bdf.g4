/*
 BSD License
 
 Copyright (c) 2022, Tom Everett All rights reserved.
 
 Redistribution and use in source and binary forms, with or without modification, are permitted
 provided that the following conditions are met:
 
 1. Redistributions of source code must retain the above copyright notice, this list of conditions
 and the following disclaimer. 2. Redistributions in binary form must reproduce the above copyright
 notice, this list of conditions and the following disclaimer in the documentation and/or other
 materials provided with the distribution. 3. Neither the name of Tom Everett nor the names of its
 contributors may be used to endorse or promote products derived from this software without specific
 prior written permission.
 
 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
 IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
 FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
 CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
 IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
grammar bdf;

font: startfont EOF;

startfont:
	'STARTFONT' ARG (
		fontdecl
		| sizedecl
		| fontboundingboxdecl
		| propertiesdecl
		| charsdecl
		| chardecl
	)+ 'ENDFONT';

fontdecl: 'FONT' ARG;

sizedecl: 'SIZE' ARG ARG ARG;

fontboundingboxdecl: 'FONTBOUNDINGBOX' ARG ARG ARG ARG;

propertiesdecl:
	'STARTPROPERTIES' ARG (
		fontascentdecl
		| fontdecentdecl
		| foundrydecl
		| familynanmedecl
		| weightnamedecl
		| slantdecl
		| setwidthnamedecl
		| addstylenamedecl
		| pixelsizedecl
		| pointsizedecl
		| resolutionxdecl
		| resolutionydecl
		| spacingdecl
		| averagewidthdecl
		| charsetregistrydecl
		| charsetencoding
		| fontnameregistry
		| charsetcollectionsdecl
		| fontnamedecl
		| facenamedecl
		| copyrightdecl
		| fontversiondecl
		| underlinepositiondecl
		| underlinethicknessdecl
		| xheightdecl
		| capheighdecl
		| rawascentdecl
		| rawdescentdecl
		| normspacedecl
		| relativeweightdecl
		| relaticesetwidthdecl
		| figurewidthdecl
		| avglowercasewidthdecl
		| avguppercasewidthdecl
	)* 'ENDPROPERTIES';

foundrydecl: 'FOUNDRY' QUOTEDSTRING;

familynanmedecl: 'FAMILY_NAME' QUOTEDSTRING;

weightnamedecl: 'WEIGHT_NAME' QUOTEDSTRING;

slantdecl: 'SLANT' QUOTEDSTRING;

setwidthnamedecl: 'SETWIDTH_NAME' QUOTEDSTRING;

addstylenamedecl: 'ADD_STYLE_NAME' QUOTEDSTRING;

pixelsizedecl: 'PIXEL_SIZE' ARG;

pointsizedecl: 'POINT_SIZE' ARG;

resolutionxdecl: 'RESOLUTION_X' ARG;

resolutionydecl: 'RESOLUTION_Y' ARG;

spacingdecl: 'SPACING' QUOTEDSTRING;

averagewidthdecl: 'AVERAGE_WIDTH' ARG;

charsetregistrydecl: 'CHARSET_REGISTRY' QUOTEDSTRING;

charsetencoding: 'CHARSET_ENCODING' QUOTEDSTRING;

fontnameregistry: 'FONTNAME_REGISTRY' QUOTEDSTRING;

fontascentdecl: 'FONT_ASCENT' ARG;

charsetcollectionsdecl: 'CHARSET_COLLECTIONS' QUOTEDSTRING;

fontnamedecl: 'FONT_NAME' QUOTEDSTRING;

facenamedecl: 'FACE_NAME' QUOTEDSTRING;

copyrightdecl: 'COPYRIGHT' QUOTEDSTRING;

fontdecentdecl: 'FONT_DESCENT' ARG;

fontversiondecl: 'FONT_VERSION' QUOTEDSTRING;

underlinepositiondecl: 'UNDERLINE_POSITION' ARG;

underlinethicknessdecl: 'UNDERLINE_THICKNESS' ARG;

xheightdecl: 'X_HEIGHT' ARG;

capheighdecl: 'CAP_HEIGHT' ARG;

rawascentdecl: 'RAW_ASCENT' ARG;

rawdescentdecl: 'RAW_DESCENT' ARG;

normspacedecl: 'NORM_SPACE' ARG;

relativeweightdecl: 'RELATIVE_WEIGHT' ARG;

relaticesetwidthdecl: 'RELATIVE_SETWIDTH' ARG;

figurewidthdecl: 'FIGURE_WIDTH' ARG;

avglowercasewidthdecl: 'AVG_LOWERCASE_WIDTH' ARG;

avguppercasewidthdecl: 'AVG_UPPERCASE_WIDTH' ARG;

charsdecl: 'CHARS' ARG;

chardecl:
	'STARTCHAR' ARG (
		encodingdecl
		| swidthdecl
		| dwidthdecl
		| bbxdecl
		| bitmapdecl
	)* 'ENDCHAR';

encodingdecl: 'ENCODING' ARG;

swidthdecl: 'SWIDTH' ARG ARG;

dwidthdecl: 'DWIDTH' ARG ARG;

bbxdecl: 'BBX' ARG ARG ARG ARG;

bitmapdecl: 'BITMAP' ARG*;

ARG: ('U' '+')? '-'? [a-zA-Z0-9-.]+ ('.' [0-9A-Fa-f]+)?;

QUOTEDSTRING: '"' .*? '"';

WS: [ \r\n\t]+ -> skip;

