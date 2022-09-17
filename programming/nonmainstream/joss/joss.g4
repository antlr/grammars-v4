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
grammar joss;

prog
   : statement+ EOF
   ;

statement
   : direct '.'
   | indirect_ '.'
   | formCMD
   | assignment
   ;

direct
   : cancelCMD
   | deleteCmdCMD
   | goCMD
   | arbitraryCMD
   ;

indirect_
   : STEPNUMBER indirectCMD
   ;

indirectCMD
   : toCMD
   | doneCMD
   | stopCMD
   | demandCMD
   | arbitraryCMD
   ;

arbitraryCMD
   : setCMD
   | doCMD
   | typeCMD
   | deleteValCMD
   | LINECMD
   | PAGECMD
   ;

goCMD
   : 'Go'
   ;

cancelCMD
   : 'Cancel'
   ;

deleteCmdCMD
   : 'Delete' delCmdSel
   ;

delCmdSel
   : stepSel
   | partSel
   | formSel
   | delCmdAllSel
   ;

delCmdAllSel
   : 'all'
   | 'all' delCmdAllType
   ;

delCmdAllType
   : 'steps'
   | 'parts'
   | 'forms'
   ;

doneCMD
   : 'Done'
   ;

stopCMD
   : 'Stop'
   ;

demandCMD
   : 'Demand' variable
   ;

toCMD
   : 'To' toSel
   ;

toSel
   : stepSel
   | partSel
   ;

setCMD
   : 'Set' assignment
   ;

doCMD
   : 'Do' doSel
   | 'Do' doSel conditional
   ;

doSel
   : stepSel
   | partSel
   ;

typeCMD
   : 'Type' typeContent
   ;

typeContent
   : typeForm
   | typeData
   | typeSel
   | typeSys
   ;

typeSys
   : 'size'
   | 'time'
   | 'users'
   ;

typeForm
   : varList 'in' formSel
   ;

typeData
   : typeElem
   | typeElem ',' typeData
   ;

typeElem
   : '\'' STRING '\''
   | variable
   ;

typeSel
   : stepSel
   | partSel
   | formSel
   | typeAllSel
   ;

typeAllSel
   : 'all'
   | 'all' typeAllType
   ;

typeAllType
   : 'steps'
   | 'parts'
   | 'forms'
   | 'values'
   ;

deleteValCMD
   : varList
   | 'all' 'values'
   ;

formCMD
   : 'Form' PARTNUMBER ':'
   ;

formContent
   : formObject
   | formObject formContent
   ;

formObject
   : formPH
   | STRING
   ;

formPH
   : sciNotation
   | fixedNotation
   ;

sciNotation
   : '..' dot
   ;

dot
   : '.'
   | '.' dot
   ;

fixedNotation
   : '__' uScore '.__' uScore
   ;

uScore
   : '_'+
   ;

stepSel
   : 'step' STEPNUMBER
   ;

partSel
   : 'part' PARTNUMBER
   ;

formSel
   : 'form' PARTNUMBER
   ;

conditional
   : if_
   | for_
   ;

if_
   : 'if' boolExp
   ;

boolExp
   : '(' boolExp ')'
   | '[' boolExp ']'
   | comparison
   | comparison boolOp boolExp
   ;

boolOp
   : 'and'
   | 'or'
   ;

comparison
   : mathExp boolComp mathExp
   ;

boolComp
   : '='
   | '!='
   | '<='
   | '=>'
   | '<'
   | '>'
   ;

for_
   : 'for' range_
   ;

range_
   : variable '=' rangeExp
   ;

rangeExp
   : rangeVal
   | rangeVal ',' rangeExp
   ;

rangeVal
   : mathExp '(' mathExp ')' rangeVal
   | mathExp
   ;

mathExp
   : term
   | term ADDOP mathExp
   ;

term
   : factor
   | factor MULOP term
   ;

factor
   : '(' mathExp ')'
   | '[' mathExp ']'
   | value
   ;

assignment
   : variable '=' mathExp
   ;

value
   : NUMBER
   | variable
   | function_
   ;

function_
   : funcSqrt
   | funcLog
   | funcExp
   | funcSin
   | funcCos
   | funcIp
   | funcFp
   | funcDp
   | funcXp
   | funcSgn
   | funcMax
   | funcMin
   ;

funcSqrt
   : 'sqrt(' mathExp ')'
   ;

funcLog
   : 'log(' mathExp ')'
   ;

funcExp
   : 'exp(' mathExp ')'
   ;

funcSin
   : 'sin(' mathExp ')'
   ;

funcCos
   : 'cos(' mathExp ')'
   ;

funcIp
   : 'ip(' mathExp ')'
   ;

funcFp
   : 'fp(' mathExp ')'
   ;

funcDp
   : 'dp(' mathExp ')'
   ;

funcXp
   : 'xp(' mathExp ')'
   ;

funcSgn
   : 'sgn(' mathExp ')'
   ;

funcMax
   : 'max(' mathExp ',' argList ')'
   ;

funcMin
   : 'min(' mathExp ',' argList ')'
   ;

argList
   : mathExp
   | mathExp ',' argList
   ;

variable
   : ALPHA
   | ALPHA '(' mathExp ')'
   ;

varList
   : variable
   | variable ',' varList
   ;

LINECMD
   : 'Line'
   ;

PAGECMD
   : 'Page'
   ;

MULOP
   : '*'
   | '/'
   ;

ADDOP
   : '+'
   | '-'
   ;

STRING
   : CHAR+
   ;

CHAR
   : ALPHA
   | DIGIT
   | SPECIALCHAR
   ;

ALPHA
   : [A-Za-z]
   ;

SPECIALCHAR
   : '.'
   | ','
   | ';'
   | ':'
   | '\''
   | ' '
   | '#'
   | '$'
   | '?'
   ;

NUMBER
   : SIGN? NUMBERPART
   ;

NUMBERPART
   : INTPART
   | INTPART '.' DECIMALPART
   ;

INTPART
   : '0'
   | NZDIGIT
   | NZDIGIT DECIMALPART
   ;

DECIMALPART
   : DIGIT+
   ;

DIGIT
   : '0'
   | NZDIGIT
   ;

NZDIGIT
   : [1-9]
   ;

SIGN
   : '+'
   | '-'
   ;

PARTNUMBER
   : NZDIGIT
   | NZDIGIT INTPART
   ;

STEPNUMBER
   : PARTNUMBER '.' PARTNUMBER
   ;

WS
   : [ \r\n\t]+ -> skip
   ;

