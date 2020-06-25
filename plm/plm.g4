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

compilation
   : module
   ;

module
   : (modulename ':')? simpledoblock
   ;

modulename
   : identifier
   ;

declaration
   : declarestatement
   | proceduredefinition
   ;

declarestatement
   : 'DECLARE' declareelementlist ';'
   ;

declareelementlist
   : declareelement (',' declareelement)*
   ;

declareelement
   : factoredelement
   | unfactoredelement
   ;

unfactoredelement
   : variableelement
   | literalelement
   | labelelement
   ;

factoredelement
   : factoredvariableelement
   | factoredlabelelement
   ;

variableelement
   : variablenamespecifier arrayspecifier? (variabletype | variableattributes?)
   ;

variablenamespecifier
   : nonbasedname
   | basedname 'BASED' basespecifier
   ;

nonbasedname
   : variablename
   ;

basedname
   : variablename
   ;

variablename
   : identifier
   ;

basespecifier
   : identifier ('.' identifier)?
   ;

variableattributes
   : 'PUBLIC'? locator? initialization?
   | 'EXTERNAL'? constantattribute?
   ;

constantattribute
   : 'DATA'
   ;

arrayspecifier
   : explicitdimension
   | implicitdimension
   ;

explicitdimension
   : '(' numericconstant ')'
   ;

implicitdimension
   : '('* ')'
   ;

variabletype
   : BASICTYPE
   | structuretype
   ;

labelelement
   : identifier 'LABEL' ('PUBLIC' | 'EXTERNAL')?
   ;

literalelement
   : identifier 'LITERALLY' STRING
   ;

factoredvariableelement
   : '(' variablenamespecifier (',' variablenamespecifier)* ')' explicitdimension? variabletype variableattributes?
   ;

factoredlabelelement
   : '(' identifier (',' identifier)* ')' 'LABEL' ('PUBLIC' | 'EXTERNAL')?
   ;

structuretype
   : 'STRUCTURE' '(' memberelement (',' memberelement)* ')'
   ;

memberelement
   : unfactoredmember
   | factoredmember
   ;

unfactoredmember
   : membername (explicitdimension)? variabletype
   ;

factoredmember
   : '(' membername ('(' ',' membername ')')* ')' explicitdimension? variabletype
   ;

proceduredefinition
   : procedurestatement declaration* unit* ending
   ;

procedurestatement
   : procedurename ':' 'PROCEDURE' formalparameterlist? proceduretype? procedureattributes? ';'
   ;

procedurename
   : identifier
   ;

proceduretype
   : BASICTYPE
   ;

formalparameterlist
   : formalparameter (',' formalparameter)*
   ;

formalparameter
   : identifier
   ;

procedureattributes
   : ('EXTERNAL' | 'PUBLIC' | interrupt | 'REENTRANT')+
   ;

locator
   : 'AT' '(' expression ')'
   ;

interrupt
   : 'INTERRUPT'
   ;

initialization
   : ('INITIAL' | 'DATA') (initialvalue (',' initialvalue)*)
   ;

initialvalue
   : expression
   | STRING
   ;

unit
   : conditionalclause
   | doblock
   | basicstatement
   | labeldefinition unit
   | scopingstatement //???
   
   ;

basicstatement
   : assignmentstatement
   | callstatement
   | gotostatement
   | returnstatement
   | microprocessordependentstatement
   ;

scopingstatement
   : simpledostatement
   | docasestatement
   | dowhilestatement
   | iterativedostatement
   | endstatement
   | procedurestatement
   ;

labeldefinition
   : identifier
   ;

assignmentstatement
   : leftpart '=' expression
   ;

leftpart
   : variablereference (',' variablereference)*
   ;

callstatement
   : 'CALL' simplevariable parameterlist?
   ;

parameterlist
   : expression (',' expression)*
   ;

simplevariable
   : identifier
   | identifier '.' identifier
   ;

gotostatement
   : ('GOTO' | 'GO TO') identifier
   ;

returnstatement
   : typedreturn
   | untypedreturn
   ;

typedreturn
   : 'RETURN' expression
   ;

untypedreturn
   : 'RETURN'
   ;

microprocessordependentstatement
   : disablestatement
   | enablestatement
   | haltstatement
   | causeinterruptstatement
   ;

disablestatement
   : 'DISABLE'
   ;

enablestatement
   : 'ENABLE'
   ;

haltstatement
   : 'HALT'
   ;

causeinterruptstatement
   : 'CAUSE$INTERRUPT' '(' numericconstant ')'
   ;

simpledostatement
   : 'DO' ';'
   ;

docasestatement
   : 'DO' 'CASE' expression ';'
   ;

dowhilestatement
   : 'DO' 'WHILE' expression ';'
   ;

iterativedostatement
   : 'DO' indexpart topart bypart? ';'
   ;

indexpart
   : indexvariable '=' startexpression
   ;

topart
   : 'TO' boundexpression
   ;

bypart
   : 'BY' stepexpression
   ;

indexvariable
   : simplevariable
   ;

startexpression
   : expression
   ;

boundexpression
   : expression
   ;

stepexpression
   : expression
   ;

endstatement
   : 'END' identifier? ';'
   ;

conditionalclause
   : ifcondition trueunit
   | ifcondition trueelement 'ELSE' falseelement
   ;

ifcondition
   : 'IF' expression 'THEN' trueunit
   ;

trueelement
   : labeldefinition* doblock
   | labeldefinition* basicstatement
   ;

falseelement
   : unit
   ;

trueunit
   : unit
   ;

doblock
   : simpledoblock
   | docaseblock
   | dowhileblock
   | iterativedoblock
   ;

simpledoblock
   : simpledostatement declaration* unit* ending
   ;

ending
   : labeldefinition* endstatement
   ;

docaseblock
   : docasestatement unit* ending
   ;

dowhileblock
   : dowhilestatement unit* ending
   ;

iterativedoblock
   : iterativedostatement unit* ending
   ;

constant
   : numericconstant
   | STRING
   ;

variablereference
   : datareference
   | functionreference
   ;

datareference
   : name subscript? memberspecifier?
   ;

subscript
   : expression
   ;

memberspecifier
   : '.' membername subscript?
   ;

functionreference
   : name actualparameters?
   ;

actualparameters
   : expression (',' expression)*
   ;

membername
   : identifier
   ;

name
   : identifier
   ;

locationreference
   : '@' constantlist
   | '@' variablereference
   ;

constantlist
   : constant (',' constant)*
   ;

expression
   : logicalexpression
   | embeddedassignment
   ;

embeddedassignment
   : variablereference ':=' logicalexpression
   ;

logicalexpression
   : logicalfactor
   | logicalexpression oroperator logicalfactor
   ;

logicalfactor
   : logicalsecondary
   | logicalfactor andoperator logicalsecondary
   ;

logicalsecondary
   : notoperator? logicalprimary
   ;

logicalprimary
   : arithmeticexpression (RELATIONALOPERATOR arithmeticexpression)?
   ;

arithmeticexpression
   : term
   | arithmeticexpression addingoperator term
   ;

term
   : secondary
   | term multiplyingoperator secondary
   ;

secondary
   : ('-' | '+')? primary
   ;

primary
   : constant
   | variablereference
   | locationreference
   | subexpression
   ;

subexpression
   : '(' expression ')'
   ;

identifier
   : IDENTIFIER
   ;

numericconstant
   : BINARYNUMBER
   | OCTALNUMBER
   | DECIMALNUMBER
   | HEXADECIMALNUMBER
   | FLOATINGPOINTNUMBER
   ;

oroperator
   : 'OR'
   | 'XOR'
   ;

andoperator
   : 'AND'
   ;

notoperator
   : 'NOT'
   ;

addingoperator
   : '+'
   | '-'
   | 'PLUS'
   | 'MINUS'
   ;

multiplyingoperator
   : '*'
   | '/'
   | 'MOD'
   ;

BASICTYPE
   : 'Address'
   | 'BYTE'
   | 'HWORD'
   | 'DWORD'
   | 'QWORD'
   | 'CHARINT'
   | 'OFFSET'
   | 'SHORTINT'
   | 'INTEGER'
   | 'REAL'
   | 'SELECTOR'
   | 'POINTER'
   ;

IDENTIFIER
   : (LETTER | '_') (LETTER | DECIMALDIGIT | '$' | '_')*
   ;

FLOATINGPOINTNUMBER
   : DIGITSTRING FRACTIONALPART? (EXPONENTPART)?
   ;

EXPONENTPART
   : 'E' ('+' | '-')? DIGITSTRING
   ;

FRACTIONALPART
   : '.' DIGITSTRING
   ;

ARITHMETICOPERASTOR
   : '+'
   | '-'
   | 'PLUS'
   | 'MINUS'
   | '*'
   | '/'
   | 'MOD'
   ;

RELATIONALOPERATOR
   : '<'
   | '>'
   | '<='
   | '>='
   | '<>'
   | '='
   ;

LOGICALOPERATOR
   : 'AND'
   | 'OR'
   | 'NOT'
   | 'XOR'
   ;

BINARYDIGIT
   : '0'
   | '1'
   ;

BINARYNUMBER
   : BINARYDIGIT (BINARYDIGIT | '$')* 'B'
   | 'Q'
   ;

DECIMALNUMBER
   : DECIMALDIGIT (DECIMALDIGIT | '$')* ('D')?
   ;

HEXADECIMALNUMBER
   : DECIMALDIGIT (HEXADECIMALDIGIT | '$')* 'H'
   ;

OCTALNUMBER
   : OCTALDIGIT (OCTALDIGIT | '$')* ('O' | 'Q')
   ;

OCTALDIGIT
   : BINARYDIGIT
   | '2'
   | '3'
   | '4'
   | '5'
   | '6'
   | '7'
   ;

DIGITSTRING
   : DECIMALDIGIT (DECIMALDIGIT | '$')*
   ;

DECIMALDIGIT
   : OCTALDIGIT
   | '8'
   | '9'
   ;

HEXADECIMALDIGIT
   : DECIMALDIGIT
   | 'A'
   | 'B'
   | 'C'
   | 'D'
   | 'E'
   | 'F'
   ;

STRING
   : '\'' STRINGBODYELEMENT+ '\''
   ;

STRINGBODYELEMENT
   : NONQUOTECHAR
   | '"'
   ;

CHARACTER
   : APOSTROPHE
   | NONQUOTECHAR
   ;

APOSTROPHE
   : '\''
   ;

NONQUOTECHAR
   : LETTER
   | DECIMALDIGIT
   | '$'
   | SPECIALCHARACTER
   ;

LETTER
   : LOWERCASELETTER
   | UPPERCASELETTER
   ;

UPPERCASELETTER
   : 'A'
   | 'B'
   | 'C'
   | 'D'
   | 'E'
   | 'F'
   | 'G'
   | 'H'
   | 'I'
   | 'J'
   | 'K'
   | 'L'
   | 'M'
   | 'N'
   | 'O'
   | 'P'
   | 'Q'
   | 'R'
   | 'S'
   | 'T'
   | 'U'
   | 'V'
   | 'W'
   | 'X'
   | 'Y'
   | 'Z'
   ;

LOWERCASELETTER
   : 'a'
   | 'b'
   | 'c'
   | 'd'
   | 'e'
   | 'f'
   | 'g'
   | 'h'
   | 'i'
   | 'j'
   | 'k'
   | 'l'
   | 'm'
   | 'n'
   | 'o'
   | 'p'
   | 'q'
   | 'r'
   | 's'
   | 't'
   | 'u'
   | 'v'
   | 'w'
   | 'x'
   | 'y'
   | 'z'
   ;

SPECIALCHARACTER
   : '+'
   | '-'
   | '*'
   | '/'
   | '<'
   | '>'
   | '='
   | ':'
   | ';'
   | '.'
   | ','
   | '('
   | ')'
   | '@'
   | '_'
   ;

COMMENT
   : '/' '*' .*? '*' '/' -> skip
   ;

WS
   : [ \t\r\n] -> skip
   ;

