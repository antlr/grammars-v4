/*
BSD License

Copyright (c) 2016, Tom Everett
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

grammar clf;

log
   : (line? EOL) + line?
   ;

// combined log format has the referer and useragent fields
line
   : host logname username datetimetz request statuscode bytes (referer useragent)?
   ;

host
   : STRING
   | IP
   ;

logname
   : STRING
   ;

username
   : STRING
   ;

datetimetz
   : '[' DATE ':' TIME TZ ']'
   ;


DATE
   : [0-9] + '/' STRING '/' [0-9] +
   ;


TIME
   : [0-9] + ':' [0-9] + ':' [0-9] +
   ;


TZ
   : '-' [0-9] +
   ;

referer
   : LITERAL
   ;

request
   : LITERAL
   ;

useragent
   : LITERAL
   ;

statuscode
   : STRING
   ;

bytes
   : STRING
   ;


LITERAL
   : '"' ~ '"'* '"'
   ;


IP
   : [0-9] + '.' [0-9] + '.' [0-9] + '.' [0-9] +
   ;


STRING
   : [a-zA-Z0-9();._-] +
   ;


EOL
   : '\r'? '\n'
   ;


WS
   : [ \t\r\n] -> skip
   ;
