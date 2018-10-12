
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

// https://web.archive.org/web/20060901063907/http://www.iit.edu/~tc/euler.pdf

grammar euler;

program : block 'end';
vardecl : new id NEWDECL! ;
fordecl : formal id FORMALDECL! ;
labdecl : label id LABELDECL!;
var : id VARID! { '[' expr ']' SUBSCR! | '.' DOT! } ;
logval : true LOGVALTRUE! ;
logval : false LOGVALFALSE! ;
number : realN | integerN;
reference : '@' var REFERENCE! ;
# listhead -> '(' LISTHD1!;
# listhead -> listhead expr ',' LISTHD2!;
# listN -> listhead ')' LISTN1!;
# listN -> listhead expr ')' LISTN2!;
listN : '(' LISTHD1! ( ')' LISTN1! | expr listTl ) ;
listTl : ')' LISTN2! | ',' LISTHD2! ( expr listTl | ')' LISTN1! ) ;
prochead : '’' PROCHD! { fordecl ';' PROCFORDECL! } ;
procdef : prochead expr '’' PROCDEF! ;
primary : var ( listN CALL! | VALUE!) | primary1 ;
primary1 : logval LOADLOGVAL! | number LOADNUM! | symbol LOADSYMB!| reference |listN | tail primary UOP! | procdef |undef LOADUNDEF! | '[' expr ']' PARENS! | in INPUT! |isb var UOP! | isn var UOP! | isr var UOP! | isl var UOP! | isli var UOP! | isy var UOP! |isp var UOP! | isu var UOP! | abs primary UOP! |length var UOP! | integer primary UOP! | real primary UOP! | logical primary UOP! | list primary UOP! ;
factor : primary factortail;
factortail : { '**' primary BOP! } ;
term : factor termtail;
termtail : { '*' factor BOP! | '/' factor BOP! | div factor BOP! | mod factor BOP! } ;
sum : ('+' term UPLUS! | '-' term NEG! | term) sumtail;
sumtail : { '+' term BOP! | '-' term BOP! } ;
choice : sum choicetail;
choicetail : { min sum BOP! | max sum BOP! } ;
relation : choice relationtail;
relationtail : [ '=' choice BOP! | '~=' choice BOP! | '<' choice BOP! | '<=' choice BOP! | '>' choice BOP! | '>=' choice BOP! ] ;
negation : '~' relation UOP! | relation ;
conj : negation conjtail;
conjtail : [ and CONJHD! conj CONJ! ];
disj : conj disjtail;
disjtail : [ or DISJHD! disj DISJ! ] ;
catenatail : { '&' primary BOP! };
truepart : expr else TRUEPT! ;
ifclause : if expr then IFCLSE! ;
expr : var exprtail | expr1;
exprtail : '<-' expr BOP! | ( listN CALL! | VALUE!) factortail termtail sumtail choicetail relationtail conjtail disjtail catenatail ;
expr1 : block ;
expr1 : ifclause truepart expr IFEXPR! ;
expr1 : goto primary UOP! ;
expr1 : out expr UOP! ;
expr1 :primary1 factortail termtail sumtail choicetail relationtail conjtail disjtail catenatail ;
expr1 : ( '+' term UPLUS! | '-' term NEG! ) sumtail choicetail relationtail conjtail disjtail catenatail ;
expr1 : '~' relation UOP! conjtail disjtail catenatail ;
stat : expr1 | id ( ':' LABDEF! stat LABSTMT! | VARID! { '[' expr ']' SUBSCR! | '.' DOT! } exprtail ) ;
block : 'begin' BEGIN! { vardecl ';' BLKHD! | labdecl ';' BLKHD!} stat { ';' BLKBODY! stat } 'end' BLK! ;

WS
   : (' ' | '\r' | '\t' | '\u000C' | '\n') -> channel (HIDDEN)
   ;

