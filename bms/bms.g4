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

// https://bm98.yaneu.com/bm98/bmsformat.html

grammar bms;


options { caseInsensitive = true; }
file_
   : line_* EOF
   ;
   // A BMS file is a sequence of lines. A line beginning with '#' is either a
   
   // channel/object ("data") line of the form #AAABB:CCCC..., or a header /
   
   // control directive of the form #KEY VALUE (#TITLE, #BPM, #WAVxx, #RANDOM,
   
   // #IF, #ENDIF, etc). Any other line (including '*'-prefixed section
   
   // separators and '//' comments) is ignored, per the spec.
   
line_
   : dataline_
   | headerline_
   ;

dataline_
   : DATALINE
   ;

headerline_
   : HEADERLINE
   ;
   // aaa (measure/track 000-999) and bb (channel 00-FF) are fixed-width, so
   
   // they are matched directly in the data-line token; they are immediately
   
   // distinguishable from a header line because header keys always start with
   
   // a letter, never a digit.
   
DATALINE
   : '#' [0-9] [0-9] [0-9] [0-9A-F] [0-9A-F] ':' [0-9A-F]*
   ;

HEADERLINE
   : '#' [A-Z] [A-Z0-9]* ([ \t]+ ~ [\r\n]*)?
   ;
   // Any line not starting with '#' (comments, '*' section separators,
   
   // '//' comments, blank lines) is simply ignored.
   
OTHERLINE
   : ~ [\r\n#]+ -> skip
   ;

WS
   : [ \r\n\t]+ -> skip
   ;

