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

// https://en.wikipedia.org/wiki/SP/k


// https://dl.acm.org/doi/10.1145/953057.810456


//


// SP/k is a sequence of structured subsets of PL/I, designed for teaching.


// Since it is syntactically just a (small) subset of PL/I, the lexical and


// expression layers missing from the original sketch of this grammar are


// completed here following standard PL/I conventions.

grammar spk;

prog
   : identifier ':' 'PROCEDURE' 'OPTIONS' '(' identifier ')' ';' declaration* definition* statement* 'END' ';'
   ;

declaration
   : 'DECLARE' variable (',' variable)* attribute (',' variable (',' variable)* attribute)* ';'
   ;

attribute
   : 'FIXED'
   | 'FLOAT'
   | 'CHARACTER' '(' INTEGER ')' 'VARYING'?
   | 'BIT'
   ;

definition
   : identifier ':' 'PROCEDURE' ('(' identifier (',' identifier)* ')')? ('RETURNS' '(' attribute ')')? ';' declaration* definition* statement* 'END' ';'
   ;

statement
   : putstatement
   | putpagestatement
   | getstatement
   | assignstatement
   | ifstatement
   | dowhilestatement
   | do2statement
   | dostatement
   | callstatement
   | returnstatement
   ;

putstatement
   : 'PUT' 'SKIP'? 'LIST' '(' expression (',' expression)* ')' ';'
   ;

putpagestatement
   : 'PUT' 'PAGE' 'LIST' '(' expression (',' expression)* ')' ';'
   ;

getstatement
   : 'GET' 'SKIP'? 'LIST' '(' variable (',' variable)* ')' ';'
   ;

assignstatement
   : variable '=' expression ';'
   ;

ifstatement
   : 'IF' condition 'THEN' statement ('ELSE' statement)?
   ;

dowhilestatement
   : 'DO' 'WHILE' '(' condition ')' ';' statement* 'END' ';'
   ;

do2statement
   : 'DO' identifier '=' expression 'TO' expression ('BY' expression)? ';' statement* 'END' ';'
   ;

dostatement
   : 'DO' ';' statement* 'END' ';'
   ;

callstatement
   : 'CALL' identifier ('(' (expression (',' expression)*)? ')')? ';'
   ;

returnstatement
   : 'RETURN' ('(' expression ')')? ';'
   ;

condition
   : orcondition
   ;

orcondition
   : andcondition ('|' andcondition)*
   ;

andcondition
   : notcondition ('&' notcondition)*
   ;

notcondition
   : NOT? relation
   ;

relation
   : expression relationaloperator expression
   | '(' condition ')'
   ;

relationaloperator
   : '='
   | NE
   | '<'
   | '<='
   | '>'
   | '>='
   ;

expression
   : term (('+' | '-' | '||') term)*
   ;

term
   : factor (('*' | '/') factor)*
   ;

factor
   : ('+' | '-')? primary
   ;

primary
   : INTEGER
   | FLOATLITERAL
   | STRING
   | identifier ('(' (expression (',' expression)*)? ')')?
   | '(' expression ')'
   ;

variable
   : identifier
   ;

identifier
   : ID
   ;

NOT
   : '¬'
   | '^'
   ;

NE
   : '¬='
   | '^='
   | '<>'
   ;

ID
   : [a-zA-Z] [a-zA-Z0-9_]*
   ;

INTEGER
   : [0-9]+
   ;

FLOATLITERAL
   : [0-9]+ '.' [0-9]+
   ;

STRING
   : '\'' ('\'\'' | ~ ['\r\n])* '\''
   ;

COMMENT
   : '/*' .*? '*/' -> skip
   ;

WS
   : [ \r\n\t]+ -> skip
   ;

