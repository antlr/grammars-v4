/*
BSD License

Copyright (c) 2020, Tom Everett
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

// http://user.it.uu.se/~andersa/gedcom/ch1.html

grammar gedcom;

gedcom
   : line+ EOF
   ;

line
   : level opt_xref_id? tag line_value? EOL
   ;

level
   : DIGIT+
   ;

opt_xref_id
   : pointer
   ;

tag
   : alphanum+
   ;

line_value
   : line_item+
   ;

line_item
   : pointer
   | escape
   | anychar
   ;

escape
   : '@' '#' escape_text '@' non_at
   ;

non_at
   : ALPHA
   | DIGIT
   | otherchar
   | '#'
   ;

escape_text
   : anychar+
   ;

pointer
   : '@' alphanum pointer_string '@'
   ;

pointer_string
   : pointer_char+
   ;

pointer_char
   : ALPHA
   | DIGIT
   | otherchar
   | '#'
   ;

alphanum
   : ALPHA
   | DIGIT
   ;

anychar
   : ALPHA
   | DIGIT
   | otherchar
   | '#'
   | '@@'
   ;

ALPHA
   : [a-zA-Z_]
   ;

DIGIT
   : [0-9]
   ;

otherchar
   : '!'
   | '"'
   | '$'
   | '&'
   | '\''
   | '('
   | ')'
   | '*'
   | '+'
   | '-'
   | ','
   | '.'
   | '/'
   ;

EOL
   : [\r\n]+
   ;

WS
   : [ \t] -> skip
   ;

