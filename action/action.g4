/*
BSD License

Copyright (c) 2024, Tom Everett
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

// $antlr-format alignTrailingComments true, columnLimit 150, minEmptyLines 1, maxEmptyLinesToKeep 1, reflowComments false, useTab false
// $antlr-format allowShortRulesOnASingleLine false, allowShortBlocksOnASingleLine true, alignSemicolons hanging, alignColons hanging

grammar action;

file_ : program EOF;

program :  ('MODULE' progmodule+ )+;
progmodule : systemdecls* routinelist;

systemdecls : definedecl | typedecl |
                       vardecl;

definedecl : 'DEFINE' deflist;
deflist : def (',' def)*;
def : IDENTIFIER '=' STRCONST;


typedecl : 'TYPE' typeerecidentlist;
typeerecidentlist : typerecident+ ;

typerecident : recname '=' fieldinit+;
recname : IDENTIFIER;
fieldinit : funddecl;

vardecl :basevardecl+;
basevardecl : funddecl | pointerdecl |
                             arraydecl | recorddecl;

 funddecl : basefunddecl +;
 basefunddecl : fundtype fundidentlist;

 fundtype : 'CARD' | 'CHAR' | 'BYTE' | 'INT';
 fundidentlist : fundident (','
                           fundident);
 fundident : IDENTIFIER ('='initopts)?;
 initopts : addr | value+;
 addr : COMPCONST;
 value : NUMCONST;


 pointerdecl : ptrtype 'POINTER' ptridentlist;
 ptrtype : fundtype | recname;
 ptridentlist : ptrident (','
                         ptrident)*;
 ptrident : IDENTIFIER ('='value)?;


 arraydecl : fundtype 'ARRAY' arridentlist;
 arridentlist : arrident (',' arrident)*
                         ;
 arrident : IDENTIFIER ('(' dim')' )? ('=' arrinitopts)?;
 dim :  NUMCONST;
 arrinitopts : addr | value+ | STRCONST;
 valuelist :  value+;

 recorddecl : IDENTIFIER recidentlist;
 recidentlist : recident (','
                         recident)*;
 recident : IDENTIFIER ('=' address)?;
address : COMPCONST;


 memreference : memcontents | '@' IDENTIFIER;
 memcontents : fundref | arrref | ptrref |
                         recref;
 fundref : IDENTIFIER;
 arrref : IDENTIFIER'('arithexp')';
 ptrref : IDENTIFIER '^';
 recref : IDENTIFIER '.' IDENTIFIER;


 routinelist : routine+;
 routine : procroutine | funcroutine;

 procroutine : procdecl ?systemdecls?
                        stmtlist? 'RETURN'?;
 procdecl : PROC IDENTIFIER ('='addr)? '('paramdecl? ')';

 funcroutine : funcdecl systemdecls?
                        stmtlist? ('RETURN' '(' arithexp')')?;
 funcdecl : fundtype 'FUNC' IDENTIFIER ('.' addr)?
                        '('paramdecl?')';

routinecall : funccall | proccall;
funccall : IDENTIFIER'(' params? ')';
proccall : IDENTIFIER'(' params? ')';
params: paramdecl (',' paramdecl)*;
paramdecl :vardecl;

stmtlist : stmt+;
stmt : simpstmt | strucstmt | codeblock;
simpstmt : assignstmt | exitstmt | routinecall;
strucstmt : ifstmt | doloop | whileloop |
                    forloop;

assignstmt : memcontents '=' arithexp;

exitstmt : 'EXIT';

ifstmt : 'IF' condexp 'THEN' stmtlist?
                       elseifexten* elseexten? 'FI';
elseifexten : 'ELSEIF' condexp 'THEN' stmtlist?;
elseexten : 'ELSE' stmtlist?;

doloop : 'DO' stmtlist? untilstmt? 'OD';

untilstmt : 'UNTIL' condexp;

whileloop : 'WHILE' condexp doloop;

forloop : FOR IDENTIFIER=start TO finish
                   ('STEP' inc)? doloop;
start : arithexp;
finish : arithexp;
inc : arithexp;

codeblock : compconstlist+;
compconstlist :  COMPCONST +
                        ;


complexrel : complexrel  SPECIALOP simprelexp |
                  simprelexp SPECIALOP simprelexp;
simprelexp : arithexp RELOP arithexp;

arithexp : arithexp ADDOP multexp |
                    multexp;
multexp : multexp MULTOP valuevalue | valuevalue;
valuevalue : NUMCONST | memreference | (arithexp);




FUNDTYPE : 'CARD' | 'CHAR' | 'BYTE' | 'INT';


SPECIALOP : 'AND' | 'OR' | '&' | '%';
RELOP : 'XOR' | '!' | '=' | '#' | '<>' | '<' | '<=' | '>' | '>=';
ADDOP: '+' | '-';
MULTOP : '*' | '/' | 'MOD' | 'LSH' | 'RSH';
UNARYOP: '@' | '-';


NUMCONST: DECNUM | HEXNUM | CHAR;
DECNUM : DIGIT+;
HEXNUM : HEXDIGIT+ | '$' HEXNUM;
//CHAR : '<any printable character>';
HEXDIGIT :[0-9A-F];
DIGIT : [0-9];

STRCONST: '"' ~'"'* '"';

IDENTIFIER : [a-zA-Z][a-zA-Z0=9]+;

COMPCONST: BASECOMPCONST+;

BASECOMPCONST : IDENTIFIER| NUMCONST |
               PTRREF | '*';



WS
    : [ \r\n\t]+ -> skip
    ;