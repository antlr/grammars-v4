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

grammar plm;


character : apostrophe | nonquotecharacter;
apostrophe: '\'';
nonquotecharacter: letter | decimaldigit | '$' | specialcharacter | blank;

letter : uppercaseletter | lowercaseletter;
uppercaseletter : 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' | 'K' | 'L' |
'M' | 'N' | 'O' | 'P' | 'Q' | 'R' | 'S' | 'T' | 'U' | 'V' | 'W' | 'X' |
'Y' | 'Z';
lowercaseletter: 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' |
'm' | 'n' | 'o' | 'p' | 'q' | 'r' | 's' | 't' | 'u' | 'v' | 'w' | 'x' |
'y' | 'z';
decimaldigit: '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9';
specialcharacter:  '+' | '-' | '*' | '/' | '<' | '>' | '=' | ':' | ';' | '.' | ',' |'(' | ')' | '@' | '_';

token: delimiter | identifier | reservedword | numericconstant | string;

delimiter: simpledelimiter | compounddelimiter;
simpledelimiter: '+' | '-' | '*' | '/' | '<' | '>' | '=' | ':' | ';' | '.' | ',' | '(' |
')' | '@';
compounddelimiter: '<>' | '<=' | '>=' | ':=';

firstidcharacter: letter | '_';
identifier: firstidcharacter (letter | decimaldigit | '$' | '_' )*;


numericconstant: binarynumber | octalnumber |decimalnumber | hexadecimalnumber |
floatingpointnumber;
binarynumber: binarydigit (binarydigit | '$')* 'B' | 'Q';
octalnumber: octaldigit (octaldigit | '$')* ( 'O' | 'Q' );
decimalnumber: decimaldigit (decimaldigit | '$')* ('D')?;
hexadecimalnumber: decimaldigit (hexadecimaldigit| '$')* 'H';
floatingpointnumber: digitstring fractionalpart (exponentpart)?;
fractionalpart: ('.' digitstring)?;
exponentpart: 'E' ('+' | '-')? digitstring;
digitstring: decimaldigit (decimaldigit | '$')*;
binarydigit: '0' | '1';
octaldigit: binarydigit | '2' | '3' | '4' | '5' | '6' | '7';
decimaldigit: octaldigit| '8' | '9';
hexadecimaldigit: decimaldigit| 'A' | 'B' | 'C' | 'D' | 'E' | 'F';

<string>::= '<string body element> [<string body element>]... ';
<string body element>::= <non-quote character> |";
<pl/m text>::= <token> | <separator> [<token> | <separator>]...;
<separator>::= blank | <comment>;
<comment>::= /* [<character>]... */;

compilation: module [EOF];
module: module name ':' simpledoblock;
modulename : identifier;

declaration: declarestatement | proceduredefinition;

declarestatement: 'DECLARE' declareelementlist;
declareelementlist: declareelement (',' declareelement)*;
declareelement: factoredelement | unfactoredelement;
unfactoredelement: variableelement | literalelement |labelelement;
factoredelement: factoredvariableelement |factoredlabelelement;

variableelement: variablenamespecifier (arrayspecifier)? variabletype | (variableattributes)?;
variablenamespecifier: nonbasedname | basedname 'BASED' basespecifier;
nonbasedname: variablename;
basedname: variablename;
variablename: identifier;
basespecifier: identifier ('.' identifier)?;
variableattributes: 'PUBLIC'? locator? initialization? | 'EXTERNAL'? constantattribute?;
locator: 'AT' (expression);
constantattribute: 'DATA';
arrayspecifier: explicitdimension | implicitdimension;
explicitdimension: numericconstant;
implicitdimension: '*';

variabletype: basictype | structuretype;
basictype: 'Address' | 'BYTE' | 'HWORD' | 'DWORD' | 'QWORD' | 'CHARINT' |
'OFFSET' | 'SHORTINT' | 'INTEGER' | 'REAL' | 'SELECTOR' |
'POINTER' | 'OFFSET';

labelelement: identifier 'LABEL' ('PUBLIC' | 'EXTERNAL')?;

literalelement: identifier 'LITERALLY' string;

factoredvariableelement: ( variablenamespecifier (',' variablenamespecifier)*) explicitdimension? variabletype variableattributes?;

factoredlabelelement: (identifier (','<identifier>)*) 'LABEL' ('PUBLIC' | 'EXTERNAL')?;

structuretype: 'STRUCTURE' (memberelement (',' memberelement)*);
memberelement: unfactoredmember | factoredmember;
unfactoredmember: membername (explicitdimension)? variabletype;
membername: identifier;
factoredmember: (membername(',' membername)*) explicitdimension? variabletype;

proceduredefinition:procedurestatement (declaration)? (unit*)? ending;
procedurestatement: procedurename ':' 'PROCEDURE' formalparameterlist? proceduretype? procedureattributes?;
procedurename: identifier;
proceduretype: basictype;
formalparameterlist: formalparameter (',' formalparameter)*;
formalparameter: identifier;
procedureattributes: ('EXTERNAL' | 'PUBLIC' | interrupt | 'REENTRANT')+;

locator: 'AT' expression;

interrupt: 'INTERRUPT';

initialization: ('INITIAL' | 'DATA') (initialvalue (',' initialvalue)*);
initialvalue: expression | string;

<unit>::= <conditional clause> | <do block> | <basic statement> |
<label definition><unit>
<basic statement>::= <assignment statement> | <call statement> |
<goto statement> | <null statement> |
<return statement> |
<microprocessor dependent statement>
<scoping statement>::= <simple do statement> | <do-case statement> |
<do-while statement> |
<iterative do statement> | <end statement> |
<procedure statement>;
labeldefinition: identifier:

<assignment statement>::= <left part >=<expression>;

<left part>::= <variable reference> [,<variable reference>]...

<call statement>::= CALL <simple variable>[<parameter list>];
<parameter list>::= (<expression>[,<expression>]...)
<simple variable>::= <identifier> | <identifier>.<identifier>

<goto statement>::= {GOTO | GO TO} <identifier>

nullstatement: ;

<return statement>::= <typed return> | <untyped return>
<typed return>::= RETURN <expression>;
<untyped return>::= RETURN;

<microprocessor dependent statement>::= <disable statement> |
<enable statement> |
<halt statement> |
<cause interrupt statement>
<disable statement>::= DISABLE;
<enable statement>::= ENABLE;
<halt statement>::= HALT;
<cause interrupt statement>::= CAUSE$INTERRUPT (numeric constant);

<simple do statement>::= DO;

<do-case statement>::= DO CASE <expression>;

<do-while statement>::= DO WHILE <expression>;

<iterative do statement>::= DO <index part> <to part> [<by part>];
<index part>::= <index variable>=<start expression>
<to part>::= TO <bound expression>
<by part>::= BY <step expression>
<index variable>::= <simple variable>
<start expression>::= <expression>
<bound expression>::= <expression>
<step expression>::= <expression>

<end statement>::= END [<identifier>];

<procedure statement>::= <procedure name> : PROCEDURE
[<formal parameter list>] [<procedure type>]
[<procedure attributes>];

<conditional clause>::= <if condition><true unit> |
<if condition><true element> ELSE
<false element>
<if condition>::= IF <expression> THEN <true unit>
<true element>::= [<label definition>...] <do block> |
[<label definition>...] <basic statement>
<false element>::= <unit>
<true unit>::= <unit>

<do block>::= <simple do block> | <do-case block> | <do-while block> |
<iterative do block>

<simple do block>::= <simple do statement>[<declaration>...]
[<unit>...]<ending>
<ending>::= [<label definition>...]<end statement>

<do-case block>::= <do-case statement> [<unit>...] <ending>

<do-while block>::= <do-while statement> [<unit>...] <ending>

<iterative do block>::= <iterative do statement> [<unit>...] <ending>
<primary>::= <constant> | <variable reference> | <location reference>
| <subexpression>
subexpression: expression;

constant: numericconstant | string;

variablereference: datareference | functionreference;
datareference: name subscript? memberspecifier?;
subscript:expression;
memberspecifier: '.' membername subscript?;
functionreference: name actualparameters?;
actualparameters: expression (',' expression)*;
membername: identifier;
name: identifier;
locationreference: '@' constantlist | '@' variablereference;
constantlist: constant (',' constant)*;
operator: logicaloperator | relationaloperator | arithmeticoperator;
logicaloperator: 'AND' | 'OR' | 'NOT' | 'XOR';
relationaloperator: '<' | '>' | '<=' | '>=' | '<>' | '=';
arithmeticoperator: '+' | '-' | 'PLUS' | 'MINUS' | '*' | '/' | 'MOD';

expression: logicalexpression | embeddedassignment;
embeddedassignment: variablereference ':=' logicalexpression;
logicalexpression: logicalfactor | logicalexpression oroperator logicalfactor;
oroperator: 'OR' | 'XOR';
logicalfactor: logicalsecondary | logicalfactor andoperator logicalsecondary;
andoperator: 'AND';
logicalsecondary: notoperator? logicalprimary;
notoperator: 'NOT';
logicalprimary: arithmeticexpression (relationaloperator arithmeticexpression)?;
relationaloperator: '<' | '>' | '<=' | '>=' | '<>' | '=';
arithmeticexpression: term | arithmeticexpression addingoperator term;
addingoperator: '+' | '-' | 'PLUS' | 'MINUS';
term: secondary | term multiplyingoperator secondary;
multiplyingoperator: '*' | '/' | 'MOD';
secondary: (unaryminus | unaryplus)? primary;
unaryminus: '-';
unaryplus: '+';


WS
   : [ \t\r\n] -> skip
   ;
