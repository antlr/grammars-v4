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

/**
* scheme:[//[user:password@]host[:port]][/]path[?query][#fragment]
*/

// $antlr-format alignTrailingComments true, columnLimit 150, minEmptyLines 1, maxEmptyLinesToKeep 1, reflowComments false, useTab false
// $antlr-format allowShortRulesOnASingleLine false, allowShortBlocksOnASingleLine true, alignSemicolons hanging, alignColons hanging

grammar url;

url
    : uri EOF
    ;

uri
    : scheme '://' login? host (':' port)? ('/' path?)? query? frag? WS?
    ;

scheme
    : string
    ;

host
    : '/'? hostname
    ;

hostname
    : string         # DomainNameOrIPv4Host
    | '[' v6host ']' # IPv6Host
    ;

v6host
    : '::'? (string | DIGITS) ((':' | '::') (string | DIGITS))*
    ;

port
    : DIGITS
    ;

path
    : string ('/' string)* '/'?
    ;

user
    : string
    ;

login
    : user (':' password)? '@'
    ;

password
    : string
    ;

frag
    : '#' (string | DIGITS)
    ;

query
    : '?' search
    ;

search
    : searchparameter ('&' searchparameter)*
    ;

searchparameter
    : string ('=' (string | DIGITS | HEX))?
    ;

string
    : STRING
    | DIGITS
    ;

DIGITS
    : [0-9]+
    ;

HEX
    : ('%' [a-fA-F0-9] [a-fA-F0-9])+
    ;

STRING
    : ([a-zA-Z~0-9] | HEX) ([a-zA-Z0-9.+-] | HEX)*
    ;

WS
    : [\r\n]+
    ;