/*
BSD License

Copyright (c) 2022, Tom Everett
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

grammar pl360;
<ADR OP>     : @@
               | @;
<ALPHA>      : A | B | C | D | E | F | G | H | I | J | K | L | M
               | N | O | P | Q | R | S | T | U | V | W | X | Y | Z;
<ALPHA NUM>  : <ALPHA>
               | <DIGIT>;
<ARITH OP>   : '--'
               | '++'
               | '-'
               | '+'
               | '*'
               | '/'
               | '=:';
<ASS STEP>   : <K REG ASS> 'STEP' <T NUMBER>;
<BLOCKBODY>  : <BLOCKBODY> <LABEL DEF>
               | <BLOCKBODY> <STATEMENT> ';'
               | <BLOCKHEAD>;
<BLOCKHEAD>  : <BLOCKHEAD> <DECL> ';'
               | 'BEGIN';
<BRINDX OP>  : 'GT'
               | 'LE';
<BYTE NUM>   : <INTEGER> 'X';
<CASE HEAD>  : <CASE HEAD> <DECL> ';'
               | 'CASE' <K REG> 'OF' 'BEGIN';
<CASE SEQ>   : <CASE HEAD>
               | <CASE SEQ> <LABEL DEF>
               | <CASE SEQ> <STATEMENT> ;
<CELL ST>    : <CELL ST> <LOG OP> <STRING>
               | <CELL ST> <LOG OP> <T CELL>
               | <CELL ST> <LOG OP> <T NUMBER>
               | <T CELL> ':=' <K REG>
               | <T CELL> ':=' <STRING>
               | <T CELL> ':=' <T CELL>
               | <T CELL> ':=' <T NUMBER>;
<CHARS>      : <CHARS> <CHAR1>
               | <CHAR1>;
<CHAR1>      : Any character except "
               | ""
<COMP AOR>   : <COMP AOR> <STATEMENT> ';'
               | <COMP COND> 'AND'
               | <COMP COND> 'OR';
<COMP COND>  : <COMP AOR> <COND>
               | <CONDITION>;
<COND DO>    : <COMP COND> 'DO';
<COND THEN>  : <COMP COND> 'THEN';
<COND>       : <CONDITION>;
<CONDITION>  : <K REG> <BRINDX OP> <K REG>
               | <K REG> <REL OP> <K REG>
               | <K REG> <REL OP> <STRING>
               | <K REG> <REL OP> <T CELL>
               | <K REG> <REL OP> <T NUMBER>
               | <REL OP>
               | <RP> <PRIM COND> )
               | <T CELL>
               | <T CELL> <REL OP> <STRING>
               | <T CELL> <REL OP> <T CELL>
               | <T CELL> <REL OP> <T NUMBER>
               | <T NUMBER>
               | <UNARY REG>
               | '~' <T CELL>
               | '~' <T NUMBER>
               | '~' <UNARY REG>;
<DECL>       : <DSEG TYPE> 'BASE' <K REG>
               | <EQU SYN2>
               | <FUNC DC7>
               | <PROC HD7> <STATEMENT*>
               | <PROC SYN> <PROC ID>
               | <SYN DC2>
               | <T DECL4>
               | 'CLOSE' 'BASE';
<DIGIT>      : 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9;
<DO>         : 'DO';
<DSEG TYPE>  : 'COMMON'
               | 'COMMON' 'DATA' <ID>
               | 'DUMMY'
               | 'EXTERNAL' 'DATA' <ID>
               | 'GLOBAL' 'DATA' <ID>
               | 'SEGMENT';
<EQU SYN1>   : <EQUATE> <ID> 'SYN';
<EQU SYN2>   : <EQU SYN1> <K REG>
               | <EQU SYN1> <STRING>
               | <EQU SYN1> <T NUMBER>
               | <EQU SYN1> <UNARY NUM>
               | <EQU SYN2> <ARITH OP> <STRING>
               | <EQU SYN2> <ARITH OP> <T NUMBER>
               | <EQU SYN2> <LOG OP> <STRING>
               | <EQU SYN2> <LOG OP> <T NUMBER>
               | <EQU SYN2> <SHIFT OP> <T NUMBER>
               | <EQU SYN3> <ARITH OP> <T CELL>;
<EQU SYN3>   : <EQU SYN1> <T CELL>'
<EQUATE>     : <EQU SYN2> ','
               | EQUATE
               | SHORT EQUATE;
<FILL>       : <ADR OP> <PROC ID>
               | <ADR OP> <T CELL>
               | <REP LIST2> )
               | <STRING>
               | <T NUMBER>
<FRAC NUM>   : <FRAC NUM> <DIGIT>
               | <INTEGER> .
<FORUNTIL>   : UNTIL
<FUNC DC1>   : <FUNC DC7> ,
               | FUNCTION
<FUNC DC2>   : <FUNC DC1> <ID>
<FUNC DC3>   : <FUNC DC2> (
<FUNC DC4>   : <FUNC DC3> <T NUMBER>
<FUNC DC5>   : <FUNC DC4> ,
<FUNC DC6>   : <FUNC DC5> <T NUMBER>
<FUNC DC7>   : <FUNC DC6> )
<FUNC ID>    : <ID>
<FUNC1>      : <FUNC2> <FUNC3>
               | <FUNC2> <K REG>
               | <FUNC2> <STRING>
               | <FUNC2> <T CELL>
               | <FUNC2> <T NUMBER>
<FUNC2>      : <FUNC ID> (
               | <FUNC1> ,
<FUNC3>      : <FUNC1> )
<GLOB HD>    : <GLOB HD1>
               | <GLOB HD1> BASE <K REG>
<GLOB HD1>   : GLOBAL <PROC HD4>
<GOTO ST*>   : GOTO <ID>
<GOTO ST>    : <GOTO ST*>
<HEX DIGIT>  : <DIGIT> | A | B | C | D | E | F
<HEX VAL>    : <HEX VAL> <HEX DIGIT>
               | # <HEX DIGIT>
<ID>         : <ID> <ALPHA NUM>
               | <ALPHA>
<IF>         : <IF> <STATEMENT> ';'
               | IF
<INT NUM>    : <HEX VAL>
               | <INTEGER>
<INTEGER>    : <INTEGER> <DIGIT>
               | <DIGIT>
<K REG ASS>  : <K REG ASS> <ARITH OP> <K REG>
               | <K REG ASS> <ARITH OP> <STRING>
               | <K REG ASS> <ARITH OP> <T CELL>
               | <K REG ASS> <ARITH OP> <T NUMBER>
               | <K REG ASS> <LOG OP> <K REG>
               | <K REG ASS> <LOG OP> <STRING>
               | <K REG ASS> <LOG OP> <T CELL>
               | <K REG ASS> <LOG OP> <T NUMBER>
               | <K REG ASS> <SHIFT OP> <K REG>
               | <K REG ASS> <SHIFT OP> <T NUMBER>
               | <K REG>
               | <K REG> := <ADR OP> <PROC ID>
               | <K REG> := <ADR OP> <T CELL>
               | <K REG> := <K REG>
               | <K REG> := <STRING>
               | <K REG> := <T CELL>
               | <K REG> := <T NUMBER>
               | <K REG> := <UNARY CELL>
               | <K REG> := <UNARY NUM>
               | <K REG> := <UNARY REG>
<K REG>      : <ID>
<LABEL DEF>  : <ID> :
<LIMIT>      : <FORUNTIL> <K REG>
               | <FORUNTIL> <T CELL>
               | <FORUNTIL> <T NUMBER>
<LOG OP>     : AND
               | OR
               | XOR
<LONG NUM>   : <INT NUM> L
               | <FRAC NUM> ' <SCALE> L
               | <INTEGER> ' <SCALE> L
               | <FRAC NUM> L
<NUMBER>     : <LONG NUM>
               | <REAL NUM>
               | <SHORT NUM>
               | <BYTE NUM>
               | <INT NUM>
<PRIM COND>  : <COMP COND>
<PROC HD1>   : PROCEDURE <ID>
<PROC HD2>   : <PROC HD1> (
<PROC HD3>   : <PROC HD2> <K REG>
<PROC HD4>   : <PROC HD3> )
<PROC HD5>   : EXTERNAL <PROC HD4>
               | GLOBAL <PROC HD4>
               | SEGMENT <PROC HD4>
<PROC HD6>   : <PROC HD4>
               | <PROC HD5>
               | <PROC HD5> BASE <K REG>
               | COMMON <PROC HD4>
<PROC HD7>   : <PROC HD6> ';'
<PROC ID>    : <ID>
<PROC SYN>   : PROCEDURE <ID> SYN
<PROC1>      : <PROC ID> (
<PROC2>      : <PROC1> <K REG>
<PROGRAM*>   : <PROGRAM+> <STATEMENT*>
<PROGRAM+>   : .
               | . <GLOB HD> ';'
<PROGRAM>    : <PROGRAM*> .
<REAL NUM>   : <INT NUM> R
               | <FRAC NUM> ' <SCALE>
               | <INTEGER> ' <SCALE>
               | <FRAC NUM>
<REL OP>     : =
               | ~=
               | >=
               | <=
               | >
               | <
<REP LIST1>  : <REP LIST2> ,
               | <T NUMBER> (
               | (
<REP LIST2>  : <REP LIST1> <FILL>
<SCALE>      : _ <INTEGER>
               | <INTEGER>
<SHIFT OP>   : SHLA
               | SHLL
               | SHRA
               | SHRL
<SHORT NUM>  : <INT NUM> S
<REPEAT>     : <REPEAT> <LABEL DEF>
               | <REPEAT> <STATEMENT> ';'
               | REPEAT
<REPUNTIL>   : UNTIL
<RP>         : <RP> <STATEMENT> ';'
               | (
<SI T TYPE>  : BYTE
               | CHARACTER
               | INTEGER
               | LOGICAL
               | LONG REAL
               | REAL
               | SHORT INTEGER
<SIMPLE ST>  : <BLOCKBODY> END
               | <CASE SEQ> END
               | <CELL ST>
               | <FUNC ID>
               | <FUNC3>
               | <K REG ASS>
               | <PROC ID>
               | <PROC2> )
               | NULL
<STATEMENT*> : <GOTO ST>
               | <STATEMENT+>
<STATEMENT+> : <IF> <COND THEN> <GOTO ST>
               | <IF> <COND THEN> <STATEMENT+>
               | <IF> <COND THEN> <TRUE PART> <GOTO ST>
               | <IF> <COND THEN> <TRUE PART> <STATEMENT+>
               | <REPEAT> <REPUNTIL> <PRIM COND>
               | <SIMPLE ST>
               | <WHILE> <COND DO> <STATEMENT*>
               | FOR <ASS STEP> <LIMIT> <DO> <STATEMENT*>
<STATEMENT>  : <STATEMENT*>
<STRING>     : <HEX VAL> X
               | " <CHARS> "
<SYN DC1>    : <SI T TYPE> REGISTER <ID> SYN
               | <SYN DC3> <ID> SYN
               | <T TYPE> <ID> SYN
<SYN DC2>    : <SYN DC1> <K REG>
               | <SYN DC1> <T CELL>
               | <SYN DC1> <T NUMBER>
<SYN DC3>    : <SYN DC2> ,
<T CELL ID>  : <ID>
<T CELL>     : <T CELL ID>
               | <T CELL1> )
               | <T CELL2> )
<T CELL1>    : <T CELL1> <ARITH OP> <STRING>
               | <T CELL1> <ARITH OP> <T NUMBER>
               | <T CELL2> <ARITH OP> <K REG>
               | <T CELL2> <ARITH OP> <T NUMBER>
               | <T CELL3> <STRING>
               | <T CELL3> <T NUMBER>
<T CELL2>    : <T CELL3> <K REG>
<T CELL3>    : <T CELL ID> (
<T DECL1>    : <T DECL2>
               | <T DECL2> <ID>
               | <T TYPE>
               | <T TYPE> <ID>
<T DECL2>    : <T DECL4> ,
<T DECL3>    : <T DECL1> =
<T DECL4>    : <T DECL1>
               | <T DECL3> <FILL>
<T NUMBER>   : <ID>
               | _ <NUMBER>
               | <NUMBER>
<T TYPE>     : <SI T TYPE>
               | ARRAY <T NUMBER> <SI T TYPE>
<TRUE PART>  : <GOTO ST*> ELSE
               | <SIMPLE ST> ELSE
<UNARY CELL> : <UNARY OP> <T CELL>
               | <UNARY OP> <UNARY CELL>
<UNARY NUM>  : <UNARY OP> <T NUMBER>
               | <UNARY OP> <UNARY NUM>
<UNARY OP>   : ABS
               | DEC
               | HALF
               | NEG
<UNARY REG>  : <UNARY OP> <K REG>
               | <UNARY OP> <UNARY REG>
<WHILE>      : <WHILE> <STATEMENT> ';'
               | WHILE


WS
   : [ \r\n\t] + -> skip
   ;
