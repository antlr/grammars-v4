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
grammar beep;

frame
   : data EOF
   ;

data
   : header payload_trailer
   ;

header
   : msg
   | rpy
   | err
   | ans
   | nul
   ;

msg
   : MSG SP common
   ;

rpy
   : RPY SP common
   ;

ans
   : ANS SP common SP ansno
   ;

err
   : ERR SP common
   ;

nul
   : NUL SP common
   ;

common
   : channel SP msgno SP more SP seqno SP size
   ;

channel
   : NUMBER
   ;

msgno
   : NUMBER
   ;

more
   : DOT
   | STAR
   ;

seqno
   : NUMBER
   ;

size
   : NUMBER
   ;

ansno
   : NUMBER
   ;

payload_trailer
   : PAYLOAD_TRAILER CRLF?
   ;

DOT
   : '.'
   ;

STAR
   : '*'
   ;

NUL
   : 'NUL'
   ;

ERR
   : 'ERR'
   ;

ANS
   : 'ANS'
   ;

RPY
   : 'RPY'
   ;

MSG
   : 'MSG'
   ;

NUMBER
   : [0-9]+
   ;

SP
   : ' '
   ;

CRLF
   : [\r\n]+
   ;

PAYLOAD_TRAILER
   : CRLF .*? 'END'
   ;

