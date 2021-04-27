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
grammar hume;

program
   : decls
   ;

module
   : 'module' modid 'where' decls
   ;

decls
   : (decl ';')+
   ;

decl
   : 'import' modid idlist?
   | 'export' idlist
   | 'exception' exnid '::' type
   | 'data' typeid varids '=' constrs
   | 'type' typeid varids '=' type
   | 'constant' varid '=' cexpr
   | 'stream' iodes
   | 'port' iodes
   | 'memory' iodes
   | 'interrupt' iodes
   | 'fifo' iodes
   | foreigndecl
   | 'operation' boxid 'as' STRINGCONST '::' type
   | 'expression' expr
   | boxdecl
   | wiringdecl
   | fundecl
   ;

constrs
   : (conid type+)+
   ;

iodes
   : ioid ('from' | 'to') STRINGCONST (('timeout' | 'within') TIMECONST ('raise' exnid)?)?
   ;

fundecl
   : (varid '::' type)
   | (varid args '=' expr)
   | (patt op patt '=' expr)
   ;

args
   : patt+
   ;

vardecl
   : varid '::' type
   | varid '=' expr
   ;

vardecls
   : vardecl+ ';'
   ;

foreigndecl
   : 'foreign' 'import' callconv safety? STRINGCONST? id '::' type
   ;

safety
   : 'safe'
   | 'unsafe'
   ;

callconv
   : 'ccall'
   | 'stdcall'
   | 'cplusplus'
   | 'jvm'
   | 'dotnet'
   ;

type
   : basetype
   | ('vector' NATCONST 'of' type)
   | '()'
   | ('(' type (',' type)+ ')')
   | ('[' types ']')
   | (typeid type+)
   | type '->' type
   | '(' type ')'
   ;

types
   : type (',' type)+
   ;

basetype
   : 'int' size
   | 'nat' size
   | 'bool'
   | 'char'
   | 'string' NATCONST?
   | 'word' size
   | 'float' size
   ;

size
   : NATCONST
   ;

expr
   : constant
   | varid
   //  | (expr op expr)
   | (varid expr+)
   | (conid expr+)
   | ('[' exprs ']')
   | '()'
   | ('(' expr (',' expr)+ ')')
   | ('<<' exprs '>>')
   | ('case' expr 'of' matches)
   | ('if' expr 'then' expr 'else' expr)
   | ('let' vardecls 'in' expr)
   | expr '::' type
   | expr 'as' type
   | 'raise' exnid expr
   | expr 'within' constraint ('raise' exnid)*
   | 'profile' expr
   | 'verify' expr
   | 'trace' expr expr
   | '(' expr ')'
   | '{' expr '}'
   | '*'
   ;

constraint
   : cexpr (',' cexpr ('(' cexpr ')')?)?
   ;

cexpr
   : expr
   ;

exprs
   : expr (',' expr)*
   ;

matches
   : match ('|' match)*
   ;

match
   : patt '->' expr
   ;

constant
   : INTCONST
   | FLOATCONST
   | BOOLCONST
   | CHARCONST
   | STRINGCONST
   | WORDCONST
   ;

patt
   : constant
   | varid
   | '_'
   | '[' patts ']'
   | '<<' patts '>>'
   | '()'
   | ('(' patt (',' patt)* ')')
   | conid
   | (conid patt+)
   | '(' patt ')'
   | '*'
   | '_*'
   | varid '@' patt
   ;

patts
   : patt (',' patt)*
   ;

boxdecl
   : prelude body
   ;

prelude
   : 'box' boxid 'in' inoutlist 'out' inoutlist ('handles' exnidlist)? ('within' TIMECONST)?
   ;

inoutlist
   : '(' inout (',' inout)* ')'
   ;

inout
   : varid (',' varid)* '::' type
   ;

body
   : ('match' | 'fair') boxmatches ('handle' handlers)?
   ;

handlers
   : handler ('|' handler)*
   ;

boxmatches
   : matches
   ;

handler
   : hpatt '->' cexpr
   ;

hpatt
   : exnid patt+
   ;

wiringdecl
   : 'replicate' wireid 'as' wireid ('*' NATCONST)?
   | 'instantiate' wireid 'as' boxid ('*' NATCONST)?
   //| 'macro' mid id+ '=' expr
   | 'initial' wireid inits
   | templatedecl
   | wiredecl
   | 'for' id '=' expr 'to' expr ('except' excepts)? wiringdecl
   ;

inits
   : '(' init (',' init)* ')'
   ;

init
   : wireid '=' expr
   ;

templatedecl
   : 'template' templateid prelude body
   ;

excepts
   : '(' expr (',' expr)* ')'
   | id
   ;

wiredecl
   : 'wire' wireid sources dests
   | 'wire' wireid idlist '=' wireid sources dests
   | 'wire' link 'to' link
   ;

sources
   : '(' link (',' link)* ')'
   ;

dests
   : '(' link (',' link)* ')'
   ;

link
   : linkspec linkprops
   ;

linkspec
   : connection
   | ioid
   ;

connection
   : boxid '.' varid
   ;

linkprops
   : linkprop+
   ;

linkprop
   : 'initially' expr
   | 'trace'
   | 'within' TIMECONST
   | 'timeout' TIMECONST
   ;

id
   : (modid '.')? LOCALID
   ;

idlist
   : '(' id (',' id)* ')'
   ;

varids
   : varid+
   ;

exnidlist
   : exnid (',' exnid)*
   ;

boxid
   : id
   ;

exnid
   : id
   ;

templateid
   : id
   ;

varid
   : id
   ;

conid
   : id
   ;

typeid
   : id
   ;

modid
   : LOCALID
   ;

streamid
   : id
   ;

portid
   : id
   ;

intid
   : id
   ;

fifoid
   : id
   ;

memid
   : id
   ;

ioid
   : streamid
   | portid
   | intid
   | fifoid
   | memid
   ;

wireid
   : id
   | id '{' expr '}'
   | '{' id '}'
   ;

op
   : OP
   ;

LOCALID
   : ('_' | LETTER) (LETTER | DIGIT)*
   ;

OP
   : ('+' | '-' | '*' | '/')+
   ;

NATCONST
   : DIGIT+
   ;

HEXDIGIT
   : [0-9A-F]
   ;

INTCONST
   : NATCONST
   | '(-' NATCONST ')'
   ;

FLOATCONST
   : POSTFLOATCONST
   | '(-' POSTFLOATCONST ')'
   ;

POSTFLOATCONST
   : NATCONST '.' NATCONST ('e' ('+' | '-')? NATCONST)?
   ;

BOOLCONST
   : 'true'
   | 'false'
   ;

CHARCONST
   : '\'' CHAR '\''
   ;

STRINGCONST
   : '"' ~ '"'* '"'
   ;

WORDCONST
   : '0x' HEXDIGIT+
   | '(-0x' HEXDIGIT+ ')'
   ;

TIMECONST
   : NATCONST TIMEDES
   ;

SPACECONST
   : NATCONST SPACEDES
   ;

TIMEDES
   : 'ps'
   | 'ns'
   | 'us'
   | 'ms'
   | 's'
   | 'min'
   ;

SPACEDES
   : 'B'
   | 'KB'
   | 'MB'
   ;

DIGIT
   : [0-9]
   ;

LETTER
   : [A-Za-z']
   ;

CHAR
   : [A-Za-z]
   | '\\\\' DIGIT+
   | ('\\' DIGIT+)
   | ('0x' HEXDIGIT+)
   ;

BLOCKCOMMENT
   : '{-' .*? ('-}' | EOF) -> skip
   ;

LINECOMMENT
   : '--' ~ [\r\n]* -> skip
   ;

WS
   : [ \r\n\t]+ -> skip
   ;

