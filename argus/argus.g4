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

grammar argus;
module :  equate*  equates
|  equate * guardian
|  equate * procedure
|  equate * iterator
|  equate * cluster
;
equates : idn '=' 'equates' ( parms  where? )?
equate  equate *
'end' idn;


guardian : idn '=' 'guardian'  parms? 'is' idn (',' idn)*  ( 'handles' idn (',' idn)*)? where?
 equate *
 state_decl *
( 'recover' body 'end' )?
( 'background' body 'end' )?
 operation * creator  operation *
'end' idn;


cluster : idn '=' 'cluster'  parms ? 'is' opidn (',' opidn)* ? where ?
 equate*  'rep' '=' type_spec  equate *
 own_var *
routine  routine *
'end' idn;



operation : creator
| handler
| routine;


routine : procedure
| iterator;


procedure : idn '=' 'proc'  parms? args  returnz ?  signals ?  where ?
routine body
'end' idn;


iterator : idn '=' 'iter'  parms ? args  yields ?  signals?   where ?
routine body
'end' idn;


creator : idn '=' 'creator' args  returnz?  signals ?
routine body
'end' idn;


handler : idn '=' 'handler' args  returnz? signals ?
routine body
'end' idn;


routine_body :  equate *
 own_var *
statement *;


parms : ( parm (',' parm)* )?;

parm : (idn ',' idn)* ':' 'type'
| (idn ',' idn)* ':' type_spec;

args : '(' ( decl (',' decl)* )* ')';

decl : (idn ',' idn)* ':' type_spec;

returnz : 'returns' '(' type_spec (',' type_spec)* ')';

yields : 'yields' '(' type_spec (',' type_spec)* ')';

signals : 'signals' '(' exception (',' exception)* ')';

exception : name  ( type_spec (',' type_spec)*  )?;


opidn : idn | 'transmit';

where : 'where' restriction (',' restriction)*;

restriction : idn 'has' oper_decl (',' oper_decl)* | idn 'in' type_set;

type_set : ( idn | idn 'has' oper_decl (',' oper_decl)*  equate
*
)*
| idn | reference '$' name;


oper_decl : name (',' name)* ':' type_spec | 'transmit';

constant : expression | type_spec;

state_decl :  'stable'
? decl
|  'stable'?
 idn ':' type_spec ':=' expression
|  'stable'
? decl (',' decl)* ':=' call;


equate : idn '=' constant | idn '=' type_set | idn '=' reference;

own_var : 'own' decl | 'own' idn ':' type_spec ':=' expression | 'own' decl (',' decl)* ':=' call ( '@' primary )?;

statement : decl
| idn ':' type_spec ':=' expression
| decl (',' decl)* ':=' call ( '@' primary )?
| idn (',' idn)* ':=' call ( '@' primary )?
| idn (',' idn)* ':=' expression (',' expression)*
| primary '.' name ':=' expression
| primary  expression? ':=' expression
| call ( '@' primary )?
| 'fork' call
| 'seize' expression 'do' body 'end'
| 'pause'
| 'terminate'
| enter_stmt
| 'coenter' coarm  coarm* 'end'
|  'abort'?
 'leave'


| 'while' expression 'do' body 'end'
| for_stmt
| if_stmt
| tagcase_stmt
| tagtest_stmt
| tagwait_stmt
|  'abort'
 'return'
( expression (',' expression)*

)?
| 'yield'
  ( expression (',' expression)*
)*

|  'abort'?
 'signal' name
  ( expression (',' expression)*
)?

|  'abort'?
 'exit' name
  ( expression (',' expression)*
)?

|  'abort'?
 'break'
|  'abort'
? 'continue'
| 'begin' body 'end'
| statement  'abort'?
 'resignal' name (',' name)*
| statement 'except'
 when_handler*

 others_handler?

'end';

enter_stmt : 'enter' 'topaction' body 'end' | 'enter' 'action' body 'end';


coarm : armtag ( 'foreach' decl (',' decl)* 'in' call )* body;

armtag : 'action'
| 'topaction'
| 'process';

for_stmt : 'for' ( decl (',' decl)*)? 'in' call 'do' body 'end'
| 'for' ( idn (',' idn)* )? 'in' call 'do' body 'end';

if_stmt : 'if' expression 'then' body
( 'elseif' expression 'then' body )*
( 'else' body )?
'end';

tagcase_stmt : 'tagcase' expression
tag_arm  tag_arm*
( 'others' ':' body )?
'end';

tagtest_stmt : 'tagtest' expression
atag_arm  atag_arm*
( 'others' ':' body )?
'end';

tagwait_stmt : 'tagwait' expression
atag_arm  atag_arm*
'end';

tag_arm : 'tag' name (',' name)* ( ( idn ':' type_spec ) )? ':' body;

atag_arm : tag_kind name (',' name)* ( ( idn ':' type_spec ) )* ':' body;

tag_kind : 'tag'
| 'wtag';

when_handler : 'when' name (',' name)* ( ( decl (',' decl)* ))* ':' body
| 'when' name (',' name)* '(' '*' ')' ':' body;

others_handler : 'others' ( ( idn ':' type_spec ) )* ':' body;
body :  equate *
 statement* ;


type_spec : 'null'
| 'node'
| 'bool'
| 'int'
| 'real'
| 'char'
| 'string'
| 'any'
| 'image'
| 'rep'
| 'cvt'
| 'sequence'  type_actual ?
| 'array'  type_actual ?
| 'atomic_array'  type_actual ?
| 'struct' ( field_spec (',' field_spec)* )?
| 'record' ( field_spec (',' field_spec)* )?
| 'atomic_record' ( field_spec (',' field_spec)* )?
| 'oneof' ( field_spec (',' field_spec)* )?
| 'variant' ( field_spec (',' field_spec)* )?
| 'atomic_variant' ( field_spec (',' field_spec)* )?
| 'proctype' ( ( type_spec (',' type_spec)* )? )  returnz ?  signals ?
| 'itertype' ( ( type_spec (',' type_spec)* )? )  yields ?  signals ?
| 'creatortype' ( ( type_spec (',' type_spec)* )? )  returnz ?  signals ?
| 'handlertype' ( ( type_spec (',' type_spec)* )? )  returnz ?  signals ?
| 'mutex' type_actual ?
| 'reference';

field_spec : name (',' name)* ':' type_actual;

reference : idn
| idn ( actual_parm (',' actual_parm)* )?
| reference '$' name;

actual_parm : constant
| type_actual;

type_actual : type_spec ( 'with' ( opbinding (',' opbinding)* )* )?;
opbinding : name (',' name)* ':' primary;



expression : primary | call '@' primary | ( expression ) | '~' expression  | '−' expression | expression '**' expression  | expression '//' expression  | expression '/' expression  | expression '*' expression  | expression '||' expression  | expression '+' expression  | expression '−' expression  | expression '<' expression  | expression '<=' expression  | expression '=' expression  | expression '>=' expression  | expression '>' expression  | expression '~<' expression  | expression '~<=' expression  | expression '~=' expression  | expression '~>=' expression | expression '~>' expression | expression '&' expression  | expression 'cand' expression  | expression '|' expression  | expression 'cor' expression ;
primary : entity | call | primary
. name
| primary [ expression ];

call : primary (
( expression (',' expression)* )*
);

entity : 'nil' | 'true' | 'false' | int_literal | real_literal | char_literal | string_literal | 'self' | reference | entity '.' name
| entity  expression?
| 'bind' entity ( ( bind_arg (',' bind_arg)* )?
)
| type_spec '$' ( field (',' field)*
)*
| type_spec '$' (
( expression ':' )? ( expression (',' expression)* )?
)?
| type_spec '$' name
( ( actual_parm (',' actual_parm)*
)?
)?
| 'up' ( expression ) | 'down' ( expression );

field : name (',' name)* ':' expression;
bind_arg :
'*'
| expression;

real_literal: DIGIT+ '.' DIGIT+;
char_literal: 'a';

string_literal: STRING;

STRING: '"' .*? '"';

name: IDENTIFIER;
idn: IDENTIFIER;

IDENTIFIER: [a-zA-Z] [a-zA-Z0-9]+;

int_literal: DIGIT+;

DIGIT: [0-9];

WS
   : [ \r\n\t] + -> skip
   ;
