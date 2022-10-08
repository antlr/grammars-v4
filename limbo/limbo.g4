/*
BSD License

Copyright (c) 2021, Tom Everett
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
grammar limbo;

program
   : 'implement' IDENTIFIER ';' top_declaration_sequence EOF
   ;

top_declaration_sequence
   : top_declaration+
   ;

top_declaration
   : declaration
   | identifier_list ':=' expression ';'
   | identifier_list ASSIGN expression ';'
   | '(' identifier_list ')' ':=' expression ';'
   | module_declaration
   | function_definition
   | adt_declaration
   ;

declaration
   : identifier_list ':' type_ ';'
   | identifier_list ':' type_ ASSIGN expression ';'
   | identifier_list ':' 'con' expression ';'
   | identifier_list ':' 'import' IDENTIFIER ';'
   | identifier_list ':' 'type' type_ ';'
   | 'include' string_constant ';'
   ;

identifier_list
   : IDENTIFIER
   | identifier_list ',' IDENTIFIER
   ;

expression_list
   : expression+
   ;

type_
   : data_type
   | function_type
   ;

data_type
   : byte_
   | int_
   | big
   | real_
   | string_
   | tuple_type
   | 'array' 'of' data_type
   | 'list' 'of' data_type
   | 'chan' 'of' data_type
   | adt_type
   | 'ref' adt_type
   | module_type
   | module_qualified_type
   | type_name
   ;

string_
   : STRING
   ;

real_
   : REAL
   ;

byte_
   : INT
   ;

int_
   : INT
   ;

big
   : INT
   ;

tuple_type
   : '(' data_type_list ')'
   ;

data_type_list
   : data_type (',' data_type)*
   ;

adt_type
   : IDENTIFIER
   | module_qualified_type
   ;

module_type
   : IDENTIFIER
   ;

module_qualified_type
   : IDENTIFIER '->' IDENTIFIER
   ;

type_name
   : IDENTIFIER
   ;

function_type
   : 'fn' function_arg_ret
   ;

function_arg_ret
   : '(' formal_arg_list? ')'
   | '(' formal_arg_list? ')' ':' data_type
   ;

formal_arg_list
   : formal_arg (',' formal_arg)*
   ;

formal_arg
   : nil_or_D_list ':' type_
   | nil_or_D ':' 'self' 'refopt' IDENTIFIER
   | nil_or_D ':' 'self' IDENTIFIER
   | '*'
   ;

nil_or_D_list
   : nil_or_D (',' nil_or_D)*
   ;

nil_or_D
   : IDENTIFIER
   | 'nil'
   ;

module_declaration
   : IDENTIFIER ':' 'module' '{' mod_member_list? '}' ';'
   ;

mod_member_list
   : mod_member+
   ;

mod_member
   : identifier_list ':' function_type ';'
   | identifier_list ':' data_type ';'
   | adt_declaration ';'
   | identifier_list ':' 'con' expression ';'
   | identifier_list ':' 'type' type_ ';'
   ;

adt_declaration
   : IDENTIFIER ':' 'adt' '{' adt_member_list? '}' ';'
   ;

adt_member_list
   : adt_member
   | adt_member_list adt_member
   ;

adt_member
   : identifier_list ':' 'cyclicopt' data_type ';'
   | identifier_list ':' 'con' expression ';'
   | identifier_list ':' function_type ';'
   | 'pick' '{' pick_member_list '}' ';'
   ;

pick_member_list
   : pick_tag_list '=>'
   | pick_member_list pick_tag_list '=>'
   | pick_member_list identifier_list ':' 'cyclicopt' data_type ';'
   ;

pick_tag_list
   : IDENTIFIER ('or' IDENTIFIER)*
   ;

function_definition
   : function_name_part function_arg_ret '{' statements_ '}'
   ;

function_name_part
   : IDENTIFIER ('.' IDENTIFIER)*
   ;

statements_
   : (declaration | statement)*
   ;

statement
   : expression ';'
   | ';'
   | '{' statements_ '}'
   | 'if' '(' expression ')' statement
   | 'if' '(' expression ')' statement 'else' statement
   | label? 'while' '(' expression? ')' statement
   | label? 'do' statement 'while' '(' expression? ')' ';'
   | label? 'for' '(' expression? ';' expression? ';' expression? ')' statement
   | label? 'case' expression '{' qual_statement_sequence '}'
   | label? 'alt' '{' qual_statement_sequence '}'
   | label? 'pick' IDENTIFIER ':=' expression '{' pqual_statement_sequence '}'
   | 'break' IDENTIFIER? ';'
   | 'continue' IDENTIFIER? ';'
   | 'return' expression? ';'
   | 'spawn' term '(' expression_list? ')' ';'
   | 'exit' ';'
   ;

label
   : IDENTIFIER ':'
   ;

qual_statement_sequence
   : qual_list '=>'
   | qual_statement_sequence qual_list '=>'
   | qual_statement_sequence statement
   | qual_statement_sequence declaration
   ;

qual_list
   : qualifier ('or' qualifier)*
   ;

qualifier
   : expression
   | expression 'to' expression
   | '*'
   ;

pqual_statement_sequence
   : pqual_list '=>'
   | pqual_statement_sequence pqual_list '=>'
   | pqual_statement_sequence statement
   | pqual_statement_sequence declaration
   ;

pqual_list
   : pqualifier ('or' pqualifier)*
   ;

pqualifier
   : IDENTIFIER
   | '*'
   ;

expression
   : binary_expression
   | lvalue_expression ASSIGNMENT_OPERATOR expression
   | '(' lvalue_expression_list ')' ASSIGN expression
   | send_expression
   | declare_expression
   | load_expression
   ;

binary_expression
   : monadic_expression
   | binary_expression BINARY_OPERATOR binary_expression
   ;

lvalue_expression
   : IDENTIFIER
   | 'nil'
   | term '[' expression ']'
   | term '[' expression ':' ']'
   | term '.' IDENTIFIER
   | '(' lvalue_expression_list ')'
   | '*' monadic_expression
   ;

lvalue_expression_list
   : lvalue_expression (',' lvalue_expression)*
   ;

init_list
   : element (',' element)*
   ;

term
   : IDENTIFIER
   //  | constant
   | real_constant
   | string_constant
   | 'nil'
   | '(' expression_list ')'
   | term '.' IDENTIFIER
   | term '->' term
   | term '(' expression_list? ')'
   | term '[' expression ']'
   | term '[' expression ':' expression ']'
   | term '[' expression ':' ']'
   | term '++'
   | term '--'
   ;

real_constant
   : REAL
   ;

string_constant
   : STRING
   ;

element
   : expression
   | expression '=>' expression
   | '*' '=>' expression
   ;

send_expression
   : lvalue_expression '<-' ASSIGN expression
   ;

declare_expression
   : lvalue_expression ':=' expression
   ;

load_expression
   : 'load' IDENTIFIER expression
   ;

monadic_expression
   : term
   | MONADICOPERATOR monadic_expression
   | 'array' '[' expression ']' 'of' data_type
   | 'array' '[' expression? ']' 'of' '{' init_list '}'
   | 'list' 'of' '{' expression_list '}'
   | 'chan' 'of' data_type
   | data_type monadic_expression
   ;

STRING
   : '"' ~ '"'* '"'
   ;

IDENTIFIER
   : [a-zA-Z] [a-zA-Z0-9_]*
   ;

REAL
   : [0-9]+ '.' ('e' | 'E')? [0-9a-zA-Z]+
   ;

INT
   : ([0-9]+ 'r' | 'R')? [0-9]+
   ;

ASSIGNMENT_OPERATOR
   : ASSIGN
   | '&='
   | '|='
   | '^='
   | '<<='
   | '>>='
   | '+='
   | '-='
   | '*='
   | '/='
   | '%='
   ;

ASSIGN
   : '='
   ;

BINARY_OPERATOR
   : MULT
   | '/'
   | '%'
   | PLUS
   | MINUS
   | '<<'
   | '>>'
   | '<'
   | '>'
   | '<='
   | '>='
   | '=='
   | '!='
   | '&'
   | '^'
   | '|'
   | '::'
   | '&&'
   | '||'
   ;

MONADICOPERATOR
   : PLUS
   | MINUS
   | '!'
   | '~'
   | 'ref'
   | MULT
   | '++'
   | '--'
   | '<-'
   | 'hd'
   | 'tl'
   | 'len'
   | 'tagof'
   ;

MULT
   : '*'
   ;

PLUS
   : '+'
   ;

MINUS
   : '-'
   ;

COMMENT
   : '#' ~ [\r\n]* -> skip
   ;

WS
   : [ \r\n\t]+ -> skip
   ;

