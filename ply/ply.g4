/*
BSD License

Copyright (c) 2018, Tom Everett
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

grammar ply;

ply
   : header vertices? faces? EOF
   ;

header
   : plydeclaration format_ (element | property_)* end_header
   ;

end_header
   : 'end_header' EOL
   ;

format_
   : 'format' 'ascii 1.0' EOL
   ;

element
   : 'element' string number EOL
   ;

property_
   : scalarproperty
   | listproperty
   ;

scalarproperty
   : 'property' type_ string EOL
   ;

listproperty
   : 'property' 'list' type_ type_ string EOL
   ;

type_
   : 'char'
   | 'uchar'
   | 'short'
   | 'ushort'
   | 'int'
   | 'uint'
   | 'float'
   | 'double'
   | 'float32'
   | 'uint8'
   | 'int32'
   ;

plydeclaration
   : 'ply' EOL
   ;

vertices
   : vertex +
   ;

faces
   : face +
   ;

vertex
   : number number number EOL
   ;

face
   : number number number number + EOL
   ;

number
   : NUMBER
   ;

string
   : STRING
   ;


STRING
   : [a-zA-Z] [a-zA-Z0-9_.]*
   ;


NUMBER
   : ('-' | '+')? ('0' .. '9') + ('.' ('0' .. '9') +)?
   ;


COMMENT
   : 'comment' ~ [\r\n]* EOL -> skip
   ;


EOL
   : [\r\n] +
   ;


WS
   : [ \t\r\n] -> skip
   ;
