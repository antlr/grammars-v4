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

// DDS (Data Description Specifications) source is traditionally a fixed
// column format (sequence number in columns 1-5, form type in column 6,
// and so on), but the meaning of the remaining columns differs by DDS
// type (physical/logical file, display file, printer file). Rather than
// enforce one of those column layouts, this grammar treats each line as
// a free sequence of items -- bare words (names, lengths, row/column
// numbers, usage codes), keyword calls such as COLHDG('A' 'B'), and
// quoted string literals. A DDS quoted string may be continued onto the
// next physical line (ending the line with '-' just before the implicit
// line break), so QUOTEDSTRING is allowed to span a newline.

grammar dds;

dds
   : line* EOF
   ;

line
   : item* EOL
   | item+
   ;

item
   : QUOTEDSTRING
   | call
   | WORD
   ;

call
   : WORD '(' arg+ ')'
   ;

arg
   : QUOTEDSTRING
   | WORD
   ;

QUOTEDSTRING
   : '\'' ('\'\'' | ~['])* '\''
   ;

WORD
   : ~[ \t\r\n'()]+
   ;

EOL
   : [\r\n]+
   ;

WS
   : [ \t]+ -> skip
   ;
