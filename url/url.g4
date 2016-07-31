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

grammar url;

fragmentaddress
   : uri ('#' fragmentid)?
   ;

uri
   : url
   ;

url
   : generic
   | httpaddress
   | ftpaddress
   | newsaddress
   | telnetaddress
   | waisaddress
   ;

generic
   : scheme ':' path ('?' search)?
   ;

scheme
   : IALPHA
   ;

httpaddress
   : 'http://' hostport ('/' path)? ('?' search)?
   ;

ftpaddress
   : 'ftp://' login path
   ;

afsaddress
   : 'afs://' cellname path
   ;

newsaddress
   : 'news:' groupart
   ;

waisaddress
   : waisindex
   | waisdoc
   ;

waisindex
   : 'wais://' hostport database ('?' search)?
   ;

waisdoc
   : 'wais://' hostport database wtype DIGITS path
   ;

groupart
   : '*'
   | group
   | article
   ;

group
   : IALPHA ('.' group)?
   ;

article
   : XALPHAS '@' host
   ;

database
   : XALPHAS
   ;

wtype
   : XALPHAS
   ;

hsoname
   : path
   ;

version
   : DIGITS
   ;

attributes
   : attribute +
   ;

attribute
   : ALPHANUMS
   ;

telnetaddress
   : 'telnet://' login
   ;

login
   : (user (':' password)? '@')? hostport
   ;

hostport
   : host (':' port)?
   ;

host
   : hostname
   | hostnumber
   ;

cellname
   : hostname
   ;

hostname
   : IALPHA ('.' hostname)?
   ;

hostnumber
   : DIGITS '.' DIGITS '.' DIGITS '.' DIGITS
   ;

port
   : DIGITS
   ;

selector
   : path
   ;

path
   : XALPHAS ('/' path)?
   ;

search
   : XALPHAS ('+' search)?
   ;

user
   : XALPHAS
   ;

password
   : XALPHAS
   ;

fragmentid
   : XALPHAS
   ;

gtype
   : XALPHA
   ;


XALPHA
   : ALPHA | DIGIT | SAFE | EXTRA | ESCAPE
   ;


XALPHAS
   : XALPHA +
   ;

xpalpha
   : XALPHA
   | '+'
   ;

xpalphas
   : XALPHA (XALPHAS)?
   ;


IALPHA
   : ALPHA XALPHAS +
   ;


ALPHA
   : [a-zA-Z]
   ;


DIGIT
   : [0-9]
   ;


SAFE
   : '$' | '-' | '_' | '@' | '.' | '&'
   ;


EXTRA
   : '!' | '*' | '"' | '\'' | '(' | ')' | ':' | ';' | ',' | ' '
   ;


ESCAPE
   : '%' HEX HEX
   ;


HEX
   : DIGIT | 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'A' | 'B' | 'C' | 'D' | 'E' | 'F'
   ;


NATIONAL
   : '{' | '}' | '|' | '[' | ']' | '\\' | '^' | '~'
   ;


PUNCTUATION
   : '<' | '>'
   ;


DIGITS
   : DIGIT +
   ;


ALPHANUM
   : ALPHA | DIGIT
   ;


ALPHANUMS
   : ALPHANUM +
   ;
