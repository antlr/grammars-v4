/*
 [The "BSD licence"]
 Copyright (c) 2020 Tom Everett
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:
 1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.
 3. The name of the author may not be used to endorse or promote products
    derived from this software without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
grammar alloy;

alloyModule
   : moduleDecl? import_* paragraph+ EOF
   ;

moduleDecl
   : 'module' qualName name (',' name)*
   ;

import_
   : 'open' qualName (',' qualName)* ('as' name)?
   ;

paragraph
   : sigDecl
   | factDecl
   | predDecl
   | funDecl
   | assertDecl
   | cmdDecl
   ;

sigDecl
   : 'abstract'? mult? 'sig' name (',' name)* sigExt? '{' (decl (',' decl)*)? '}' block?
   ;

sigExt
   : 'extends' qualName
   | 'in' qualName ('+' qualName)*
   ;

mult
   : 'lone'
   | 'some'
   | 'one'
   ;

decl
   : 'disj'? name (',' name)* ':' 'disj'? expr
   ;

factDecl
   : 'fact' name? block
   ;

predDecl
   : 'pred' (qualName '.')? name paraDecls? block
   ;

funDecl
   : 'fun' (qualName '.')? name paraDecls? ':' expr '{' expr '}'
   ;

paraDecls
   : '(' decl (',' decl)* ')'
   | '[' decl (',' decl)* ']'
   ;

assertDecl
   : 'assert' name? block
   ;

cmdDecl
   : (name ':')? ('run' | 'check')? (qualName | block) scope?
   ;

scope
   : 'for' number ('but' typescope (',' typescope)*)?
   | 'for' typescope (',' typescope)*
   ;

typescope
   : 'exactly'? number qualName
   ;

expr
   : const_
   | qualName
   | '@' name
   | 'this'
   | unOp expr
   | expr binOp expr
   | expr arrowOp expr
   | expr '[' (',' expr)+ ']'
   | expr ('!' | 'not')? compareOp expr
   | expr ('=>' | 'implies')? expr 'else' expr
   | 'let' letDecl (',' letDecl)* blockOrBar
   | quant decl (',' decl)* blockOrBar
   | '{' decl (',' decl)* blockOrBar '}'
   | '(' expr ')'
   | block
   ;

const_
   : '-'? number
   | 'none'
   | 'univ'
   | 'iden'
   ;

unOp
   : '!'
   | 'not'
   | 'no'
   | mult
   | 'set'
   | '#'
   | '~'
   | '*'
   | '^'
   ;

binOp
   : '||'
   | 'or'
   | '&&'
   | 'and'
   | '<=>'
   | 'iff'
   | '=>'
   | 'implies'
   | '&'
   | '+'
   | '-'
   | '++'
   | '<:'
   | ':>'
   | '.'
   ;

arrowOp
   : (mult | 'set')? '->' (mult | 'set')?
   ;

compareOp
   : 'in'
   | '='
   | '<'
   | '>'
   | '=<'
   | '>='
   ;

letDecl
   : name '=' expr
   ;

block
   : '{' expr* '}'
   ;

blockOrBar
   : block
   | BAR expr
   ;

quant
   : 'all'
   | 'no'
   | 'sum'
   | mult
   ;

qualName
   : 'this/'? (name '/')* name
   ;

name
   : IDENTIFIER
   ;

number
   : DIGIT+
   ;

BAR
   : '|'
   ;

DIGIT
   : '0' .. '9'
   ;

IDENTIFIER
   : ('a' .. 'z' | 'A' .. 'Z') ('a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_')*
   ;

COMMENT
   : '//' ~ [\r\n]* -> skip
   ;

WS
   : [ \t\r\n] -> skip
   ;

