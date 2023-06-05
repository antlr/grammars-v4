/*
 BSD License

 Copyright (c) 2018, Tom Everett All rights reserved.

 Redistribution and use in source and binary forms, with or without modification, are permitted
 provided that the following conditions are met:

 1. Redistributions of source code must retain the above copyright notice, this list of conditions
 and the following disclaimer. 2. Redistributions in binary form must reproduce the above copyright
 notice, this list of conditions and the following disclaimer in the documentation and/or other
 materials provided with the distribution. 3. Neither the name of Tom Everett nor the names of its
 contributors may be used to endorse or promote products derived from this software without specific
 prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
 IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
 FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
 CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
 IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

grammar clu;

module
   : equate* (procedure | iterator | cluster) EOF
   ;

procedure
   : idn '=' 'proc' parms? args returnz? signals? where_? routine_body 'end' idn
   ;

iterator
   : idn '=' 'iter' parms? args yields? signals? where_? routine_body 'end' idn
   ;

cluster
   : idn '=' 'cluster' parms? 'is' idn_list where_? cluster_body 'end' idn
   ;

parms
   : param (',' param)*
   ;

param
   : idn_list ':' ('type' | type_spec)
   ;

args
   : '(' decl_list? ')'
   ;

decl_list
   : decl (',' decl)*
   ;

decl
   : idn_list ':' type_spec
   ;

returnz
   : 'returns' '(' type_spec_list ')'
   ;

yields
   : 'yields' '(' type_spec_list ')'
   ;

signals
   : 'signals' '(' exception_ (',' exception_)* ')'
   ;

exception_
   : name type_spec_list?
   ;

type_spec_list
   : type_spec (',' type_spec)*
   ;

where_
   : 'where' restriction (',' restriction)*
   ;

restriction
   : idn ('has' oper_decl_list | 'in' type_set)
   ;

type_set
   : (idn | idn 'has' oper_decl_list equate*)*
   | idn
   ;

oper_decl_list
   : oper_decl (',' oper_decl)*
   ;

oper_decl
   : op_name_list ':' type_spec
   ;

op_name_list
   : op_name (',' op_name)*
   ;

op_name
   : name '[' constant_list? ']'
   ;

constant_list
   : constant (',' constant)*
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
   : 'own' (decl | idn ':' type_spec ':=' expression | decl_list ':=' invocation)
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
   | 'array'
   | 'sequence' type_spec?
   | ('record' | 'struct' | 'oneof' | 'variant') field_spec_list?
   | ('proctype' | 'itertype') field_spec_list? returnz? signals?
   | idn constant_list?
   ;

field_spec_list
   : field_spec (',' field_spec)*
   ;

field_spec
   : name_list ':' type_spec
   ;

statement
   : decl
   | idn ':' type_spec ':=' expression
   | decl_list ':=' invocation
   | idn_list ':=' (invocation | expression_list)
   | primary '.' name ':=' expression
   | invocation
   | 'while' expression 'do' body 'end'
   | 'for' (decl_list | idn_list)? 'in' invocation 'do' body 'end'
   | 'if' expression 'then' body ('elseif' expression 'then' body)* ('else' body)? 'end'
   | 'tagcase' expression tag_arm* ('others' ':' body)? 'end'
   | ('return' | 'yield') expression_list?
   | 'signal' name expression_list?
   | 'exit' name expression_list?
   | 'break'
   | 'begin' body 'end'
   | statement ('resignal' name_list | 'except' when_handler* others_handler? 'end')
   ;

tag_arm
   : 'tag' name_list ('(' idn ':' type_spec ')')? ':' body
   ;

when_handler
   : 'when' name_list ('(' '*' ')' | decl_list?) ':' body
   ;

others_handler
   : 'others' ('(' idn ':' type_spec ')')? ':' body
   ;

body
   : equate* statement*
   ;

expression_list
   : expression (',' expression)*
   ;

expression
   : primary
   | '(' expression ')'
   | ('~' | '-') expression
   | expression '**' expression
   | expression ('//' | '/' | '*') expression
   | expression ('||' | '+' | '-') expression
   | expression ('<' | '<=' | '=' | '>=' | '>' | '~<' | '~<=' | '~=' | '~>=' | '~>') expression
   | expression ('&' | 'cand') expression
   | expression ('|' | 'cor') expression
   ;

primary
   : 'nil'
   | 'true'
   | 'false'
   | int_literal
   | real_literal
   | string_literal
   | idn constant_list?
   | primary ('.' name | expression | '(' expression_list? ')') 
   | type_spec '$' (field_list | '[' (expression ':')? expression_list ']' | name constant_list?)
   | 'force' type_spec?
   | ('up' | 'down') '(' expression ')'
   ;

invocation
   : primary '(' expression_list ')'
   ;

field_list
   : field (',' field)*
   ;

field
   : name_list ':' expression
   ;

idn_list
   : idn (',' idn)*
   ;

idn
   : STRING
   ;

name_list
   : name (',' name)*
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
   : '"' ~'"'* '"'
   ;

STRING
   : [a-zA-Z] [a-zA-Z0-9_]*
   ;

INT
   : [0-9]+
   ;

FLOAT
   : [0-9]+ '.' [0-9]*
   ;

COMMENT
   : '%' ~[\r\n]* -> channel(HIDDEN)
   ;

WS
   : [ \t\r\n]+ -> channel(HIDDEN)
   ;

