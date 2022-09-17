/*
BSD License

Copyright (c) 2013, Tom Everett
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
'AS IS' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
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

grammar emailaddress;

emailaddress
    : (mailbox
    |  group ) EOF ;

group       
    :  phrase ':' mailbox* ';';

mailbox     
    :  addrspec                   
    |  (phrase routeaddr );

routeaddr  
    :  '<' route* addrspec '>';

route       
    :  '@' domain ':';          

addrspec   
    :  localpart '@' domain;

localpart  
    :  word ('.' word)*;          
                                           
domain      
    :  subdomain ('.' subdomain)*;

subdomain 
    :  domainref | domainliteral;

domainref  
    : atom;

phrase      
    :  word+;

word        
    :  atom | quotedstring;

lwspchar
    : SPACE | HTAB;

lwsp
    : (CRLF? lwspchar)+;

delimeters
    : SPECIALS 
    | lwsp
    | comment;

//text        
 //   : CHAR+;

atom        
    : CHAR+;

quotedpair 
    :  '\\' CHAR; 

domainliteral
    : '[' (DTEXT | quotedpair)* ']';

quotedstring 
    : '\'' (QTEXT | quotedpair)* '\'';

comment     
    : '(' (CTEXT | quotedpair | comment)* ')';

CHAR
    : [\u0000-\u0127];
   
ALPHA
    : [\u0065-\u0090]; 

DIGIT
    : [\u0048-\u0057];

CTL
    : [\u0000-\u0031]; 

CR
    : '\n';

LF
    : '\r';

SPACE
    : ' ';

HTAB
    : '\t';

CRLF
    : '\r\n'; 

SPECIALS
    : '(' | ')' | '<' | '>' | '@'  |  ',' | ';' | ':' | '\\' | '\''|  '.' | '[' | ']' ;             

QUOTE
    : '"';

QTEXT
    : ~[\r\n];

DTEXT
    : ~[[\]\n\\];

CTEXT
    : ~[()\n\\];
    