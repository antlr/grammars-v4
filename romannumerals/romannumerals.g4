/*
BSD License

Copyright (c) 2017, Tom Everett
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
/*
* Essentials of the grammar from here: https://compilers.iecc.com/comparch/article/07-03-118
*/

grammar romannumerals;

expression
   : thousands
   ;

thousands
   : thous_part hundreds
   | thous_part
   | hundreds
   ;

thous_part
   : thous_part M
   | M
   ;

hundreds
   : hun_part tens
   | hun_part
   | tens
   ;

hun_part
   : hun_rep
   | CD
   | D
   | D hun_rep
   | CM
   ;

hun_rep
   : C
   | CC
   | CCC
   ;

tens
   : tens_part ones
   | tens_part
   | ones
   ;

tens_part
   : tens_rep
   | XL
   | L
   | L tens_rep
   | XC
   ;

tens_rep
   : X
   | XX
   | XXX
   ;

ones
   : ones_rep
   | IV
   | V
   | V ones_rep
   | IX
   ;

ones_rep
   : I
   | II
   | III
   ;


M
   : 'M'
   ;


CD
   : 'CD'
   ;


D
   : 'D'
   ;


CM
   : 'CM'
   ;


C
   : 'C'
   ;


CC
   : 'CC'
   ;


CCC
   : 'CCC'
   ;


XL
   : 'XL'
   ;


L
   : 'L'
   ;


XC
   : 'XC'
   ;


X
   : 'X'
   ;


XX
   : 'XX'
   ;


XXX
   : 'XXX'
   ;


IV
   : 'IV'
   ;


V
   : 'V'
   ;


IX
   : 'IX'
   ;


I
   : 'I'
   ;


II
   : 'II'
   ;


III
   : 'III'
   ;


WS
   : [ \r\n\t] + -> skip
   ;
