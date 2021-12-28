/*
BSD License

Copyright (c) 2022, Tom Everett
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
grammar bdf;

font
   : startfont EOF
   ;

startfont
   : 'STARTFONT' NUM (fontdecl | sizedecl | fontboundingboxdecl | propertiesdecl | charsdecl | chardecl)+ 'ENDFONT'
   ;

fontdecl
   : 'FONT' STRING
   ;

sizedecl
   : 'SIZE' NUM NUM NUM
   ;

fontboundingboxdecl
   : 'FONTBOUNDINGBOX' NUM NUM NUM NUM
   ;

propertiesdecl
   : 'STARTPROPERTIES' NUM (fontascentdecl | fontdecentdecl)* 'ENDPROPERTIES'
   ;

fontascentdecl
   : 'FONT_ASCENT' NUM
   ;

fontdecentdecl
   : 'FONT_DESCENT' NUM
   ;

charsdecl
   : 'CHARS' NUM
   ;

chardecl
   : 'STARTCHAR' NUM (encodingdecl | swidthdecl | dwidthdecl | bbxdecl | bitmapdecl)* 'ENDCHAR'
   ;

encodingdecl
   : 'ENCODING' NUM
   ;

swidthdecl
   : 'SWIDTH' NUM NUM
   ;

dwidthdecl
   : 'DWIDTH' NUM NUM
   ;

bbxdecl
   : 'BBX' NUM NUM NUM NUM
   ;

bitmapdecl
   : 'BITMAP' NUM NUM NUM NUM NUM NUM NUM NUM NUM NUM NUM NUM NUM NUM NUM NUM
   ;

NUM
   : ('U' '+')? '-'? [0-9A-Fa-f]+ ('.' [0-9A-Fa-f]+)?
   ;

STRING
   : [a-z-] [a-zA-Z0-9-]*
   ;

WS
   : [ \r\n\t]+ -> skip
   ;

