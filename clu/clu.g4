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

grammar clu;

module
   : equate* (procedure | iterator | cluster)
   ;

procedure
   : idn '=' 'proc' parms? args returnz? signals? where? routine_body 'end' idn
   ;

iterator
   : idn '=' 'iter' parms? args yeilds? signals? where? routine_body 'end' idn
   ;

cluster
   : idn '=' 'cluster' parms? 'is' idn (',' idn)* where? cluster_body 'end' idn
   ;

parms
   : (param (',' param)*)?
   ;

param
   : idn (',' idn)* ':' ('type' | type_spec)
   ;

args
   : '(' decl (',' decl)* ')'
   ;

decl
   : idn (',' idn)* ':' type_spec
   ;

returnz
   : 'returns' '(' type_spec (',' type_spec)* ')'
   ;

yeilds
   : 'yeilds' '(' type_spec (',' type_spec)* ')'
   ;

signals
   : 'signals' '(' exception (',' exception)* ')'
   ;

exception
   : name (type_spec (',' type_spec)*)?
   ;

where
   : 'where' restriction (',' restriction)*
   ;

restriction
   : (idn 'has' oper_decl (',' oper_decl)*)
   | (idn 'in' type_set)
   ;

type_set
   : (idn | (idn 'has' oper_decl (',' oper_decl)* equate*))*
   | idn
   ;

oper_decl
   : op_name (',' op_name)* ':' type_spec
   ;

op_name
   : name '[' (constant (',' constant)*)? ']'
   ;

constant
   : expression
   | type_spec
   ;

routine_body
   : equate* own_var* statement*
   ;

cluster_body
   : equate* 'rep' '=' type_spec equate* own_var* 'routine' routine*
   ;

routine
   : procedure
   | iterator
   ;

equate
   : idn '=' (constant | type_set)
   ;

own_var
   : ('own' decl)
   | ('own' idn ':' type_spec ':=' expression)
   | ('own' (decl (',' decl)* ':=' invocation))
   ;

type_spec
   : 'null'
   | 'bool'
   | 'int'
   | 'real'
   | 'char'
   | 'string'
   | 'any'
   | 'rep'
   | 'cvt'
   | ('array' type_spec?)
   | ('sequence' type_spec?)
   | ('record' (field_spec (',' field_spec)*)?)
   | ('struct' (field_spec (',' field_spec)*)?)
   | ('oneof' (field_spec (',' field_spec)*)?)
   | ('variant' (field_spec (',' field_spec)*)?)
   | ('proctype' (field_spec (',' field_spec)*)? returnz? signals?)
   | ('itertype' (field_spec (',' field_spec)*)? yeilds? signals?)
   | (idn (constant (',' constant)*)?)
   | idn
   ;

field_spec
   : name (',' name)* ':' type_spec
   ;

statement
   : decl
   | (idn ':' type_spec ':=' expression)
   | (decl (',' decl)* ':=' invocation)
   | (idn (',' idn)* ':=' invocation)
   | (idn (',' idn)* ':=' expression (',' expression)*)
   | (primary '.' name ':=' expression)
   | invocation
   | whilestatement
   | forstatement2
   | forstatement1
   | ifstatement
   | tagcasestatement
   | returnstatement
   | yeildstatement
   | signalstatement
   | exitstatement
   | breakstatement
   | continuestatement
   | beginstatement
   | resignalstatement
   | exceptstatement
   ;

whilestatement
   : 'while' expression 'do' body 'end'
   ;

forstatement2
   : 'for' (decl (',' decl)*)? 'in' invocation 'do' body 'end'
   ;

forstatement1
   : 'for' (idn (',' idn)*)? 'in' invocation 'do' body 'end'
   ;

ifstatement
   : 'if' expression 'then' body ('elseif' expression 'then' body)* ('else' body)? 'end'
   ;

tagcasestatement
   : 'tagcase' expression tag_arm* ('others' ':' body)? 'end'
   ;

returnstatement
   : 'return' (expression (',' expression)*)?
   ;

yeildstatement
   : 'yeild' (expression (',' expression)*)?
   ;

signalstatement
   : 'signal' name (expression (',' expression)*)?
   ;

exitstatement
   : 'exit' name (expression (',' expression)*)?
   ;

continuestatement
   : 'break'
   ;

breakstatement
   : 'break'
   ;

beginstatement
   : 'begin' body 'end'
   ;

resignalstatement
   : statement 'resignal' name (',' name)*
   ;

exceptstatement
   : statement 'except' when_handler* others_handler? 'end'
   ;

tag_arm
   : 'tag' name (',' name)* ('(' idn ':' type_spec ')')? ':' body
   ;

when_handler
   : ('when' name (',' name)* (decl (',' decl)*)? ':' body)
   | ('when' name (',' name)* '(' '*' ')' ':' body)
   ;

others_handler
   : 'others' ('(' idn ':' type_spec ')')? ':' body
   ;

body
   : equate* statement*
   ;

expression
   : primary
   | ('(' expression ')')
   | ('~' expression)
   | ('-' expression)
   | (expression '**' expression)
   | (expression '//' expression)
   | (expression '/' expression)
   | (expression '*' expression)
   | (expression '||' expression)
   | (expression '+' expression)
   | (expression '-' expression)
   | (expression '<' expression)
   | (expression '<=' expression)
   | (expression '=' expression)
   | (expression '>=' expression)
   | (expression '>' expression)
   | (expression '~<' expression)
   | (expression '~<=' expression)
   | (expression '~=' expression)
   | (expression '~>=' expression)
   | (expression '~>' expression)
   | (expression '&' expression)
   | (expression 'cand' expression)
   | (expression '|' expression)
   | (expression 'cor' expression)
   ;

primary
   : 'nil'
   | 'true'
   | 'false'
   | int_literal
   | real_literal
   | string_literal
   | idn
   | (idn (constant (',' constant)*))
   | (primary '.' name)
   | (primary expression?)
   | invocation
   | (type_spec '$' field (',' field)*)
   | (type_spec '$' '[' (expression ':')? (expression (',' expression)* ']'))
   | (type_spec '$' '[' constant (',' constant)* ']')
   | ('force' type_spec?)
   | ('up' '(' expression ')')
   | ('down' '(' expression ')')
   ;

invocation
   : primary '(' expression (',' expression)* ')'
   ;

field
   : name (',' name)* ':' expression
   ;

idn
   : STRING
   ;

name
   : STRING
   ;

int_literal
   : INT
   ;

real_literal
   : FLOAT
   ;

string_literal
   : STRINGLITERAL
   ;


STRINGLITERAL
   : '"' ~ ['"'] '"'
   ;


STRING
   : [a-zA-Z] [a-zA-Z0-9_]*
   ;


INT
   : [0-9] +
   ;


FLOAT
   : [0-9] + '.' [0-9]*
   ;


COMMENT
   : ';' ~ [\r\n]* -> skip
   ;


WS
   : [ \t\r\n] -> skip
   ;
