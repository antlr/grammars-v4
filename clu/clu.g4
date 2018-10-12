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

grammar clu;

module:
          equate* (procedure | iterator | cluster);

procedure:
             idn '=' 'proc' params? args rets? signals? where?  routine_body 'end' idn;

iterator:
            idn '=' 'iter' params? args yields? signals? where? routine_body 'end' idn;

cluster:
           idn '=' 'cluster' params? 'is' idn (',' idn)* where cluster_body 'end' idn;


params:
         param (',' param)*;

param:
         idn (',' idn)* : ('type'  | type_spec);

args:
        '(' decl (',' decl)* ')';

rets:
        'returns' type_spec (',' type_spec)*;
          
yields:
          'yeilds' type_spec (',' type_spec)*;

signals
    : 'signals' exception (',' exception)*;

exception
    : name (type_spec (',' type_spec)*)?;

where:
         'where' restriction (',' restriction)*;

restriction
    : (idn 'has' oper_decl (',' oper_decl)*) | (idn 'in' type_set);



comment
   : COMMENT
   ;


STRINGLITERAL
   : '"' STRING
   ;


STRING
   : [a-zA-Z] [a-zA-Z0-9_]*
   ;


NUMBER
   : [0-9] +
   ;


COMMENT
   : ';' ~ [\r\n]*
   ;


EOL
   : '\r'? '\n'
   ;


WS
   : [ \t\r\n] -> skip
   ;
