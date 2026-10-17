/*
 BSD License

 Copyright (c) 2026, Tom Everett All rights reserved.

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
grammar alpherd;

compilation_unit
   : ('begin' block 'end')
   | exec_decl_list
   ;

block
   : exec_decl_list ';' stmt (';' stmt)*
   | exec_decl_list
   | stmt (';' stmt)*
   ;

exec_decl_list
   : exec_decl (';' exec_decl)*
   ;

exec_decl
   : var_decl
   | const_decl
   | proc_decl
   | form_decl
   | label_decl
   ;

expression
   : invocation
   | conditional_expression
   | value_expression
   | with_expression
   | first_expression
   ;

stmt
   : expression
   | loop_stmt
   | exit_stmt
   | null_stmt
   | inner_block
   | labeled_stmt
   | assert_stmt
   ;

labeled_stmt
   : identifier ':' stmt
   ;

invocation
   : special_literal
   | simple_invocation ('(' actuals ')')?
   | '(' invocation ')'
   ;

actuals
   : actual (',' actual)*
   ;

actual
   : expression
   | type_description
   ;

simple_invocation
   : ((identifier '$')* identifier)
   | special_identifier
   ;

conditional_expression
   : if_expression
   | case_expression
   ;

if_expression
   : 'if' expression 'then' block ('elif' expression 'then' block)* ('else' block)* 'fi'
   ;

case_expression
   : 'case' expression 'of' case ('elof' case)* ('else' block)* 'esac'
   ;

case
   : expression (',' expression)* '::' block
   ;

value_expression
   : 'value' identifier (':' obj_type)* 'of' block 'fo'
   ;

with_expression
   : 'with' with_list 'in' block 'ni'
   ;

with_list
   : (identifier ':' invocation) (',' (identifier ':' invocation))*
   ;

first_expression
   : 'first' template 'suchthat' expression ('then' block)* ('else' block)* 'fi'
   ;

template
   : identifier 'from' identifier (';' identifier)* type_description
   | identifier 'from' invocation
   ;

loop_stmt
   : simple_loop
   | while_stmt
   | for_stmt
   ;

simple_loop
   : 'do' block 'od'
   ;

while_stmt
   : 'while' expression simple_loop
   ;

for_stmt
   : 'for' template simple_loop
   ;

exit_stmt
   : 'exitloop'
   | 'leave' identifier
   ;

null_stmt
   : 'skip'
   ;

inner_block
   : 'begin' block 'end'
   | 'begin' block 'endof' (identifier)*
   ;

assert_stmt
   : 'assert' assertion
   ;

assertion
   : ASSERTION
   ;

label_decl
   : 'label' identifier_list
   ;

var_decl
   : aux 'var' (obj_decl_group init_fin_clause) (',' (obj_decl_group init_fin_clause))*
   ;

const_decl
   : aux 'const' (obj_decl_group init_fin_clause | const_assign (',' const_assign)*)
   ;

aux
   : 'aux'*
   ;

obj_decl_group
   : identifier_list ':' obj_type
   ;

obj_type
   : type_description
   | 'as' 'specified'
   ;

init_fin_clause
   : '=' expression
   | ('init' stmt)? ('final' stmt)?
   ;

const_assign
   : identifier '=' expression
   ;

type_description
   : simple_invocation ('(' (actual (',' actual)*)? ')')? update_set*
   | '?' identifier update_set*
   ;

update_set
   : '<' (identifier | special_identifier) (',' (identifier | special_identifier))* '>'
   ;

formal
   : routine_formal
   | binding obj_formal
   | type_formal
   ;

formals
   : formal (',' formal)*
   ;

routine_formal
   : formal_id_list 'proc' parms
   | formal_id_list ('vproc' | 'func' | 'sel') vparms
   ;

binding
   : ('copy' | 'alias'*) ('const' | 'var')*
   ;

formal_id_list
   : identifier_list ':'
   ;

obj_formal
   : formal_id_list type_description
   ;

type_formal
   : formal_id_list ('form' | 'pform') update_set*
   ;

routine_decl
   : vproc_decl
   | proc_decl
   | sel_decl
   ;

vproc_decl
   : aux 'inline'* ('vproc' | 'proc') routine_id vparms pre_post assumes routine_body*
   ;

proc_decl
   : aux 'inline'* 'proc' routine_id parms pre_post assumes routine_body*
   ;

sel_decl
   : aux 'inline'* 'sel' routine_id vparms pre_post assumes routine_body*
   ;

routine_id
   : identifier
   | special_identifier
   ;

parms
   : ('(' formals ')')?
   ;

vparms
   : ('(' formals ')')? ':' type_description
   ;

pre
   : ('pre' assertion ';')*
   ;

pre_post
   : pre ('post' assertion ';')*
   ;

routine_body
   : 'is' stmt
   | 'is' 'as' 'specified'
   | 'is' 'forward'
   | 'is' 'external' '(' system_specs ')'
   ;

system_specs
   : STRING
   ;

form_decl
   : aux 'form' identifier formals* pre assumes 'is' form_body
   | aux 'generator' identifier formals* ':' type_description pre assumes 'is' form_body
   ;

form_body
   : specs* impl* 'end' identifier*
   | abbrev_body
   | 'forward'
   | 'external' '(' system_specs ')'
   | 'as' 'specified'
   ;

specs
   : 'specs' (var_decl | other_form_decls) (';' (var_decl | other_form_decls))*
   ;

impl
   : 'impl' (shared var_decl | other_form_decls) (';' (shared var_decl | other_form_decls))*
   ;

other_form_decls
   : routine_decl
   | form_decl
   | axiom
   | shared const_decl
   ;

shared
   : 'shared'*
   ;

axiom
   : 'invariant' assertion
   | 'initially' assertion
   | 'axiom' assertion
   | 'remap' assertion
   | 'rule' identifier assertion
   ;

abbrev_body
   : record_type
   | enumerated_type
   ;

record_type
   : 'record' obj_decl_group (',' obj_decl_group)*
   ;

enumerated_type
   : 'enumerated' identifier_list
   ;

assumes
   : ('assumes' form_decl)*
   ;

alphanumeric
   : LETTER
   | DIGIT
   ;
   //special_symbol: BASIC_SYMBOL|operator;
   
identifier
   : IDENTIFIER
   ;

identifier_list
   : identifier (',' identifier)*
   ;

string
   : STRING
   ;

special_literal
   : unsigned_integer
   | unsigned_real
   | string
   | BOOLEAN
   | radix
   ;

unsigned_integer
   : DIGIT+
   ;

unsigned_real
   : unsigned_rational ('E' scale_factor)?
   | unsigned_integer ('E' scale_factor)?
   ;

unsigned_rational
   : unsigned_integer '.' unsigned_integer
   ;

scale_factor
   : ('+' | '-')? unsigned_integer
   ;

special_identifier
   : '@start'
   | '@finish'
   | '@next'
   | '@done'
   | '@value'
   | '@subscript'
   | '@' operator
   ;

unary_operator
   : '+'
   | '-'
   | 'not'
   ;

binary_operator
   : '$'
   | '/'
   | 'div'
   | 'rem'
   | '+'
   | '-'
   | '<'
   | '<='
   | '='
   | '>='
   | '>'
   | 'and'
   | 'or'
   | 'cand'
   | 'cor'
   | 'imp'
   | assign_op
   ;

assign_op
   : ':='
   ;

operator
   : unary_operator
   | binary_operator
   ;
   //?
   
radix
   : alphanumeric+
   ;

BOOLEAN
   : 'true'
   | 'false'
   ;

STRING
   : '"' .*? '"'
   ;

ASSERTION
   : '{' ~ [{}]* '}'
   ;

IDENTIFIER
   : [a-zA-Z] [a-zA-Z0-9]*
   ;

LETTER
   : [a-zA-Z]
   ;

DIGIT
   : [0-9]
   ;

WS
   : [ \t\r\n] -> skip
   ;

