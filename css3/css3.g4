/*
BSD License

Copyright (c) 2013, Tom Everett
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
    This grammar is an Antlr4 port of the CSS3 grammar from Antlr3, 
    written by "trihus"
 
    http://www.antlr3.org/grammar/1214945003224/csst3.g
*/
grammar css3;

stylesheet
    : importRule* (nested | ruleset)+
    ;

importRule
    : ('@import' | '@include')  STRING 
    ;

nested
    : '@' nest '{' properties? nested* '}'
    ;

nest
    : IDENT IDENT* pseudo*
    ;
	
ruleset
    : selectors '{' properties? '}'
    ;
	
selectors
    : selector (',' selector)*
    ;
	
selector
    : elem selectorOperation* attrib* pseudo? 
    ;

selectorOperation
    : selectop? elem 
    ;

selectop
    : '>'
    | '+'  
    ;

properties
    : declaration (';' declaration?)* 
    ;
	
elem
    : IDENT 
    | '#' IDENT 
    | '.' IDENT
    ;

pseudo
    : (':'|'::') IDENT
    | (':'|'::') function
    ;

attrib
    : '[' IDENT (attribRelate (STRING | IDENT))? ']' 
    ;
	
attribRelate
    : '=' 
    | '~='
    | '|='
    ;	
  
declaration
    : IDENT ':' args 
    ;

args
    : expr (','? expr)* 
    ;

expr
    : (NUM unit?)
    | IDENT
    | COLOR
    | STRING
    | function
    ;

unit
    : ('%'|'px'|'cm'|'mm'|'in'|'pt'|'pc'|'em'|'ex'|'deg'|'rad'|'grad'|'ms'|'s'|'hz'|'khz')
    ;
	
function
    : IDENT '(' args? ')' 
    ;

IDENT
    : ('_' | 'a'..'z'| 'A'..'Z' | '\u0100'..'\ufffe' ) ('_' | '-' | 'a'..'z'| 'A'..'Z' | '\u0100'..'\ufffe' | '0'..'9')*
    | '-' ('_' | 'a'..'z'| 'A'..'Z' | '\u0100'..'\ufffe' ) ('_' | '-' | 'a'..'z'| 'A'..'Z' | '\u0100'..'\ufffe' | '0'..'9')*
    ;

STRING
    : '"' (~('"'|'\n'|'\r'))* '"'
    | '\'' (~('\''|'\n'|'\r'))* '\''
    ;

NUM
    : '-' (('0'..'9')* '.')? ('0'..'9')+
    | (('0'..'9')* '.')? ('0'..'9')+
    ;

COLOR
    : '#' ('0'..'9'|'a'..'f'|'A'..'F')+
    ;


SL_COMMENT
    : '//' (~('\n'|'\r'))* ('\n'|'\r'('\n')?) -> skip	
    ;
	
COMMENT
    : '/*' .* '*/'-> skip
    ;

WS
    : ( ' ' | '\t' | '\r' | '\n' | '\f' )+-> skip
    ;



