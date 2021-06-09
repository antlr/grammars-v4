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

parser grammar PowerBuilderParser;

options { tokenVocab = PowerBuilderLexer; }

start_rule
   : header_rule? body_rule+ EOF
   ;

header_rule
   : EXPORT_HEADER* (RELEASE NUMBER SEMI)? window_property+
   ;

body_rule
   : datatype_decl
   | access_modif
   | forward_decl
   | type_variables_decl
   | global_variables_decl
   | variable_decl
   | constant_decl
   | function_forward_decl
   | functions_forward_decl
   | function_body
   | on_body
   | event_body
   ;

window_property
   : attribute_name array_decl_sub? LPAREN window_property_attribute_sub* RPAREN
   ;

window_property_attribute_sub
   : ( NULL_
     | numeric_atom
     | DQUOTED_STRING
     | DATE
     | TIME
     | attribute_name eq=EQ (attribute_value array_decl_sub? | LPAREN window_property_attribute_sub+ RPAREN)
     ) COMMA?
   ;

attribute_name
   : (identifier_name | TYPE | UPDATE) NUMBER? (DOT (identifier_name | CASE | TYPE | ON | DYNAMIC))*
   ;

attribute_value
   : atom_sub_call1
   | atom_sub_member1
   | MINUS? numeric_atom
   | boolean_atom
   | ENUM
   | DQUOTED_STRING
   | QUOTED_STRING
   | DATE
   | TIME
   | TYPE
   | TO
   | FROM
   | REF
   | NULL_
   | OPEN
   | LPAREN LPAREN (expression | dataTypeSub) (COMMA (expression | dataTypeSub))? RPAREN (COMMA LPAREN (expression | dataTypeSub) (COMMA (expression | dataTypeSub))? RPAREN)* RPAREN
   | dataTypeSub (LPAREN NUMBER RPAREN)?
   ;

forward_decl
   : FORWARD (datatype_decl | variable_decl)+ END FORWARD
   ;

datatype_decl
   : scope_modif? TYPE identifier_name FROM (identifier_name TICK)? data_type_name (WITHIN identifier_name)? AUTOINSTANTIATE? (DESCRIPTOR DQUOTED_STRING EQ DQUOTED_STRING)?
     (variable_decl | event_forward_decl)*
     END TYPE
   ;

type_variables_decl
   : TYPE VARIABLES (access_modif | variable_decl | constant_decl)* END VARIABLES
   ;

global_variables_decl
   : (GLOBAL | SHARED) VARIABLES (variable_decl | constant_decl)* END VARIABLES
   ;

variable_decl
   : variable_decl_sub SEMI
   ;

variable_decl_sub
   : INDIRECT? access_modif_part? scope_modif?
   ;

decimal_decl_sub
   : LCURLY NUMBER RCURLY
   ;

array_decl_sub
   : BRACES
   | LBRACE ((PLUS | MINUS)? NUMBER (TO (PLUS | MINUS)? NUMBER)? (COMMA (PLUS | MINUS)? NUMBER (TO (PLUS | MINUS)? NUMBER)?)*)? RBRACE
   ;

constant_decl_sub
   :
   // TODO EQ is mandatory for constants 'CONSTANT' variable_decl_sub
   ;

constant_decl
   : constant_decl_sub SEMI
   ;

function_forward_decl
   : access_modif_part? scope_modif? (FUNCTION data_type_name | SUBROUTINE) identifier_name LPAREN parameters_list_sub? RPAREN (LIBRARY (DQUOTED_STRING | QUOTED_STRING) (ALIAS FOR (DQUOTED_STRING | QUOTED_STRING))?)? (RPCFUNC ALIAS FOR (DQUOTED_STRING | QUOTED_STRING))? (THROWS identifier_name)?
   ;

parameter_sub
   : READONLY? REF? data_type_name decimal_decl_sub? identifier_name array_decl_sub?
   ;

parameters_list_sub
   : parameter_sub (COMMA parameter_sub)* (COMMA DOTDOTDOT)?
   ;

functions_forward_decl
   : (FORWARD | TYPE) PROTOTYPES function_forward_decl+ END PROTOTYPES
   ;

function_body
   : access_type? scope_modif? (FUNCTION data_type_name | SUBROUTINE) identifier_name LPAREN parameters_list_sub? RPAREN (THROWS identifier_name)? (SEMI statement)* END (FUNCTION | SUBROUTINE)
   ;

on_body
   : ON (identifier | OPEN | CLOSE)
   ;

event_forward_decl
   : EVENT (identifier_name | CREATE | DESTROY) identifier_name? (LPAREN parameters_list_sub? RPAREN)?
   | EVENT TYPE data_type_name identifier_name (LPAREN parameters_list_sub? RPAREN)
   ;

event_body
   : EVENT (TYPE data_type_name)? (identifier_name COLONCOLON)? (identifier_name | OPEN | CLOSE) (LPAREN parameters_list_sub? RPAREN)? (SEMI statement)* END EVENT
   ;

access_type
   : PUBLIC
   | PRIVATE
   | PROTECTED
   ;

access_modif
   : access_type ':'
   ;

access_modif_part
   : PUBLIC
   | PRIVATE
   | PRIVATEREAD
   | PRIVATEWRITE
   | PROTECTED
   | PROTECTEDREAD
   | PROTECTEDWRITE
   ;

scope_modif
   : GLOBAL
   | LOCAL
   ;

expression
   : close_call_sub
   | LCURLY
   ;

expression_list
   : REF? expression (COMMA REF? expression)*
   ;

boolean_expression
   : condition_or
   ;

condition_or
   : condition_and (OR condition_and)*
   ;

condition_and
   : condition_not (AND condition_not)*
   ;

condition_not
   : NOT? condition_comparison
   ;

condition_comparison
   : add_expr ((EQ | GT | LT | GTLT | GTE | LTE) add_expr)?
   ;

add_expr
   : mul_expr ((MINUS | PLUS) mul_expr)*
   ;

mul_expr
   : unary_sign_expr ((MULT | DIV | CARAT) unary_sign_expr)*
   ;

unary_sign_expr
   : LPAREN expression RPAREN
   | (MINUS | PLUS)? atom
   ;

statement
   : if_simple_statement
   | DESCRIBE identifier_name
   | assignment_statement
   | statement_sub SEMI
   | if_statement
   | TRY
   | post_event
   | function_call_statement
   | event_call_statement
   | constant_decl
   | variable_decl
   | super_call_statement
   | do_loop_while_statement
   | do_while_loop_statement
   | create_call_statement
   | destroy_call_statement
   | label_stat
   | identifier
   | throw_stat
   | goto_stat
   | choose_statement
   | return_statement
   | for_loop_statement
   | continue_statement
   | exit_statement
   | atom
   ;

statement_sub
   : function_virtual_call_expression_sub
   | function_call_expression_sub
   | return_statement
   | open_call_sub
   | close_call_sub
   | variable_decl_sub
   | super_call_statement
   | create_call_sub
   | destroy_call_sub
   | continue_sub
   | goto_stat
   | assignment_sub
   ;

assignment_sub
   : lvalue_sub (EQ | PLUSEQ | MINUSEQ | MULTEQ | DIVEQ) (NOT | LCURLY | boolean_expression | expression)
   ;

assignment_statement
   : assignment_sub SEMI?
   ;

lvalue_sub
   : atom_sub DOT identifier_name_ex
   | atom_sub_call1
   | atom_sub_array1
   | atom_sub_ref1
   | atom_sub_member1
   ;

return_statement
   : RETURN expression?
   ;

function_call_expression_sub
   : atom_sub DOT identifier_name_ex
   | atom_sub_call1
   ;

function_virtual_call_expression_sub
   : identifier_name DOT (DYNAMIC EVENT? | EVENT DYNAMIC) function_call_expression_sub
   ;

open_call_sub
   : OPEN LPAREN expression_list RPAREN
   ;

close_call_sub
   : CLOSE LPAREN expression_list RPAREN
   | HALT CLOSE
   ;

function_call_statement
   : function_call_expression_sub
   | function_virtual_call_expression_sub
   | open_call_sub
   | close_call_sub
   ;

super_call_statement
   : CALL (identifier_name TICK)? (atom_sub_call1 | atom_sub_member1)
   ;

event_call_statement_sub
   : (identifier_name DOT (identifier_name DOT)? | SUPER COLONCOLON)? EVENT atom_sub_call1
   ;

event_call_statement
   : event_call_statement_sub
   ;

create_call_sub
   : CREATE USING? (identifier_name DOT)? data_type_name (LPAREN expression_list? RPAREN)?
   ;

create_call_statement
   : create_call_sub
   ;

destroy_call_sub
   : DESTROY expression
   ;

destroy_call_statement
   : destroy_call_sub
   ;

for_loop_statement
   : FOR lvalue_sub EQ expression TO expression (STEP expression)? statement NEXT
   ;

do_while_loop_statement
   : DO (WHILE | UNTIL) boolean_expression statement* LOOP
   ;

do_loop_while_statement
   : DO statement* LOOP (WHILE | UNTIL) boolean_expression
   ;

if_statement
   : IF boolean_expression THEN statement* (ELSEIF boolean_expression THEN statement*)* (ELSE statement*)? END IF SEMI?
   ;

if_simple_statement
   : IF boolean_expression THEN statement
   ;

continue_statement
   : CONTINUE
   ;

continue_sub
   : CONTINUE
   ;

post_event
   : (atom_sub_member1 DOT)? (POST | TRIGGER) EVENT? identifier_name_ex LPAREN expression_list? RPAREN
   ;

exit_statement
   : EXIT
   ;

choose_statement
   : CHOOSE CASE expression ( choose_case_range_sub
                            | choose_case_cond_sub
                            | choose_case_else_sub
                            | choose_case_value_sub)+
     END CHOOSE
   ;

choose_case_value_sub
   : CASE expression (COMMA expression)* statement*
   ;

choose_case_cond_sub
   : CASE IS (EQ | GT | LT | GTLT | GTE | LTE) expression statement*
   ;

choose_case_range_sub
   : CASE atom TO atom statement*
   ;

choose_case_else_sub
   : CASE ELSE statement*
   ;

goto_stat
   : GOTO identifier_name
   ;

label_stat
   : identifier_name COLON
   ;

try_catch_block
   : TRY statement* (CATCH LPAREN variable_decl_sub RPAREN statement*)* (FINALLY statement*)? END TRY
   ;

throw_stat
   : THROW expression
   ;

identifier
   : identifier_name
   | SUPER COLONCOLON (CREATE | DESTROY | identifier_name_ex)
   | identifier_name COLONCOLON (CREATE | DESTROY)
   | identifier_name DOT (CREATE | DESTROY)
   | identifier_name COLONCOLON identifier_name_ex
   ;

identifier_name_ex
   : identifier_name
   | SELECT
   | TYPE
   | UPDATE
   | DELETE
   | OPEN
   | CLOSE
   | GOTO
   | INSERT
   | DESCRIBE
   | TIME
   | READONLY
   ;

identifier_name
   : ID
   ;

atom_sub
   : array_access_atom
   | identifier_name (LPAREN expression_list? RPAREN)?
   ;

atom_sub_call1
   : (identifier | DESCRIBE) LPAREN expression_list? RPAREN
   ;

atom_sub_array1
   : identifier_name LBRACE expression_list RBRACE
   ;

atom_sub_ref1
   : identifier_name BRACES
   ;

atom_sub_member1
   : identifier
   ;

atom
   : event_call_statement_sub
   | atom_sub (DOT identifier_name_ex)
   | cast_expression
   | atom_sub_call1
   | atom_sub_array1
   | atom_sub_ref1
   | atom_sub_member1
   | numeric_atom
   | boolean_atom
   | ENUM
   | DQUOTED_STRING
   | QUOTED_STRING
   | DATE
   | TIME
   ;

array_access_atom
   : identifier_name LBRACE expression_list RBRACE
   ;

numeric_atom
   : NUMBER
   ;

boolean_atom
   : TRUE
   | FALSE
   ;

cast_expression
   : dataTypeSub LPAREN expression (COMMA expression)* RPAREN
   ;

data_type_name
   : dataTypeSub
   | identifier_name
   ;

dataTypeSub
   : ANY
   | BLOB
   | BOOLEAN
   | BYTE
   | CHARACTER
   | CHAR
   | DATE_TYPE
   | DATETIME
   | DECIMAL
   | DEC
   | DOUBLE
   | INTEGER
   | INT
   | LONG
   | LONGLONG
   | REAL
   | STRING
   | TIME_TYPE
   | UNSIGNEDINTEGER
   | UINT
   | UNSIGNEDLONG
   | ULONG
   | WINDOW
   ;
