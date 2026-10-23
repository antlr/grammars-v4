/*
BSD License

Copyright (c) 2026, Tom Everett
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
. Neither the name of Tom Everett nor the names of its contributors
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

// A grammar for HAL/S, following the BNF working grammar (Appendix G) of the


// HAL/S Language Specification (PASS 32.0/BFS 17.0, USA003088):


// https://web.archive.org/web/20090226213557/http://klabs.org/richcontent/software_content/hal_s/hal-s_language_specification.pdf

grammar hals;

compilation
   : compile_list '_'
   | '_'
   ;

compile_list
   : block_definiton
   | compile_list block_definiton
   ;

arith_exp
   : term
   | '+' term
   | '-' term
   | arith_exp '+' term
   | arith_exp '-' term
   ;

term
   : product
   | product '/' term
   ;

product
   : factor
   | factor '*' product
   | factor '.' product
   | factor product
   ;

factor
   : primary
   | primary starstar factor
   ;

starstar
   : '**'
   ;

pre_primary
   : '(' arith_exp ')'
   | number
   | compound_number
   | arith_func_head '(' call_list ')'
   ;

compound_number
   : '(' number (',' number)* ')'
   ;

arith_func_head
   : arith_func
   | arith_conv subscript
   ;

arith_func
   : identifier
   ;

arith_conv
   : INTEGER
   | SCALAR
   | VECTOR
   | MATRIX
   ;

primary
   : arith_var
   | modified_arith_func
   | arith_unline_def block_body closing ';'
   | pre_primary
   | pre_primary qualifier
   ;

other_statement
   : on_phrase statement
   | if_statement
   | label_definition other_statement
   ;

statement
   : basic_statement
   | other_statement
   ;

any_statement
   : statement
   | block_definiton
   ;

basic_statement
   : label_definition basic_statement
   | assignment ';'
   | EXIT ';'
   | EXIT label ';'
   | REPEAT ';'
   | REPEAT label ';'
   | GO TO label ';'
   | ';'
   | call_key ';'
   | call_key '(' call_list ')' ';'
   | call_key assign '(' call_assign_list ')' ';'
   | call_key '(' call_list ')' assign '(' call_assign_list ')' ';'
   | RETURN ';'
   | RETURN expression ';'
   | do_group_head ending ';'
   | read_key ';'
   | read_phrase ';'
   | write_key ';'
   | write_phrase ';'
   | file_exp '=' expression ';'
   | variable '=' file_exp ';'
   | wait_key FOR DEPENDENT ';'
   | wait_key arith_exp ';'
   | wait_key UNTIL arith_exp ';'
   | wait_key FOR bit_exp ';'
   | terminator ';'
   | terminator terminate_list ';'
   | UPDATE PRIORITY TO arith_exp ';'
   | UPDATE PRIORITY label_var TO arith_exp ';'
   | schedule_phrase ';'
   | schedule_phrase schedule_control ';'
   | signal_clause ';'
   | SEND ERROR subscript ';'
   | on_clause ';'
   | on_clause AND signal_clause ';'
   | OFF ERROR subscript ';'
   | pct_macro_name ';'
   | pct_macro_head pct_macro_arg ')' ';'
   ;

pct_macro_name
   : PCT_NAME
   ;

pct_macro_head
   : pct_macro_name '('
   | pct_macro_head pct_macro_arg ','
   ;

pct_macro_arg
   : name_var
   | constant
   ;

bit_prim
   : bit_var
   | label_var
   | event_var
   | bit_const
   | '(' bit_exp ')'
   | modified_bit_func
   | bit_inline_def block_body closing ';'
   | subbit_head expression ')'
   | bit_func_head '(' call_list ')'
   ;

bit_func_head
   : bit_func
   | BIT sub_or_qualifier
   ;

bit_func
   : identifier
   ;

bit_cat
   : bit_prim
   | bit_cat cat bit_prim
   | not bit_prim
   | bit_cat cat not bit_prim
   ;

bit_factor
   : bit_cat
   | bit_factor and bit_cat
   ;

bit_exp
   : bit_factor
   | bit_exp or bit_factor
   ;

relational_op
   : '='
   | not '='
   | '<'
   | '>'
   | '<='
   | '>='
   | not '<'
   | not '>'
   ;

comparison
   : arith_exp relational_op arith_exp
   | char_exp relational_op char_exp
   | bit_cat relational_op bit_cat
   | structure_exp relational_op structure_exp
   | name_exp relational_op name_exp
   ;

relational_factor
   : rel_prim
   | relational_factor and rel_prim
   ;

relational_exp
   : relational_factor
   | relational_exp or relational_factor
   ;

rel_prim
   : '(' relational_exp ')'
   | not '(' relational_exp ')'
   | comparison
   ;

char_prim
   : char_var
   | char_const
   | modified_char_func
   | char_inline_def block_body closing ';'
   | char_func_head '(' call_list ')'
   | '(' char_exp ')'
   ;

char_func_head
   : char_func
   | CHARACTER sub_or_qualifier
   ;

char_func
   : identifier
   ;

sub_or_qualifier
   : subscript
   | bit_qualifier
   ;

char_exp
   : char_prim
   | char_exp cat char_prim
   | char_exp cat arith_exp
   | arith_exp cat arith_exp
   | arith_exp cat char_prim
   ;

assignment
   : variable eq expression
   | variable ',' assignment
   ;

if_statement
   : if_clause statement
   | true_part statement
   ;

true_part
   : if_clause basic_statement ELSE
   ;

if_clause
   : if relational_exp THEN
   | if bit_exp THEN
   ;

if
   : IF
   ;

do_group_head
   : DO ';'
   | DO for_list ';'
   | DO for_list while_clause ';'
   | DO while_clause ';'
   | DO CASE arith_exp ';'
   | case_else statement
   | do_group_head any_statement
   | do_group_head temporary_stmt
   ;

case_else
   : DO CASE arith_exp ';' ELSE
   ;

while_key
   : WHILE
   | UNTIL
   ;

while_clause
   : while_key bit_exp
   | while_key relational_exp
   ;

for_list
   : for_key arith_exp iteration_control
   | for_key iteration_body
   ;

iteration_body
   : arith_exp
   | iteration_body ',' arith_exp
   ;

iteration_control
   : TO arith_exp
   | TO arith_exp BY arith_exp
   ;

for_key
   : FOR arith_var '='
   | FOR TEMPORARY identifier '='
   ;

ending
   : END
   | END label
   | label_definition ending
   ;

on_phrase
   : ON ERROR subscript
   ;

on_clause
   : ON ERROR subscript SYSTEM
   | ON ERROR subscript IGNORE
   ;

signal_clause
   : SET event_var
   | RESET event_var
   | SIGNAL event_var
   ;

file_exp
   : file_head ',' arith_exp ')'
   ;

file_head
   : FILE '(' number
   ;

call_key
   : CALL label_var
   ;

call_list
   : list_exp
   | call_list ',' list_exp
   ;

call_assign_list
   : variable
   | call_assign_list ',' variable
   ;

expression
   : arith_exp
   | bit_exp
   | char_exp
   | structure_exp
   | name_exp
   ;

structure_exp
   : structure_var
   | modified_struct_func
   | struc_inline_def block_body closing ';'
   | struct_func_head '(' call_list ')'
   ;

struct_func_head
   : struct_func
   ;

struct_func
   : identifier
   ;

list_exp
   : expression
   | arith_exp '#' expression
   ;

variable
   : arith_var
   | structure_var
   | bit_var
   | event_var
   | subbit_head variable ')'
   | char_var
   | name_key '(' name_var ')'
   ;

name_var
   : variable
   | label_var
   | modified_arith_func
   | modified_bit_func
   | modified_char_func
   | modified_struct_func
   ;

name_exp
   : name_key '(' name_var ')'
   | NULL
   | name_key '(' NULL ')'
   ;

name_key
   : NAME
   ;

label_var
   : prefix label subscript
   ;

modified_arith_func
   : prefix no_arg_arith_func subscript
   ;

modified_bit_func
   : prefix no_arg_bit_func subscript
   ;

modified_char_func
   : prefix no_arg_char_func subscript
   ;

modified_struct_func
   : prefix no_arg_struct_func subscript
   ;

no_arg_arith_func
   : identifier
   ;

no_arg_bit_func
   : identifier
   ;

no_arg_char_func
   : identifier
   ;

no_arg_struct_func
   : identifier
   ;

structure_var
   : qual_struct subscript
   ;

arith_var
   : prefix arith_id subscript
   ;

char_var
   : prefix char_id subscript
   ;

bit_var
   : prefix bit_id subscript
   ;

event_var
   : prefix event_name subscript
   ;

arith_id
   : identifier
   ;

char_id
   : identifier
   ;

bit_id
   : identifier
   ;

event_name
   : identifier
   ;

structure_id
   : identifier
   ;

struct_template
   : identifier
   ;

qual_struct
   : structure_id
   | qual_struct '.' structure_id
   ;

prefix
   : empty
   | qual_struct '.'
   ;

subbit_head
   : subbit_key subscript '('
   ;

subbit_key
   : SUBBIT
   ;

subscript
   : sub_head ')'
   | qualifier
   | dollar number
   | dollar arith_var
   | empty
   ;

sub_head
   : dollar '('
   | dollar '(' '@' prec_spec ','
   | sub_head ';'
   | sub_head ':'
   | sub_head ','
   | sub_head sub
   ;

sub
   : sub_exp
   | '*'
   | sub_run_head sub_exp
   | arith_exp AT sub_exp
   ;

sub_run_head
   : sub_exp TO
   ;

sub_exp
   : arith_exp
   | hash_expression
   ;

hash_expression
   : '#'
   | hash_expression '+' term
   | hash_expression '-' term
   ;

eq
   : '='
   ;

dollar
   : '$'
   ;

and
   : '&'
   | AND
   ;

or
   : '=|'
   | OR
   ;

not
   : '¬'
   | '~'
   | NOT
   ;

cat
   : '||'
   | CAT
   ;

qualifier
   : dollar '(' '@' prec_spec ')'
   | dollar '(' scale_head arith_exp ')'
   | dollar '(' '@' prec_spec ',' scale_head arith_exp ')'
   ;

scale_head
   : '@'
   | '@@'
   ;

bit_qualifier
   : '<$' '(' '@' radix ')'
   ;

radix
   : HEX
   | OCT
   | BIN
   | DEC
   ;

bit_const_head
   : radix
   | radix '(' number ')'
   ;

bit_const
   : bit_const_head char_string
   | TRUE
   | FALSE
   | ON
   | OFF
   ;

char_const
   : char_string
   | CHAR '(' number ')' char_string
   ;

io_control
   : SKIP_IO '(' arith_exp ')'
   | TAB '(' arith_exp ')'
   | COLUMN '(' arith_exp ')'
   | LINE '(' arith_exp ')'
   | PAGE '(' arith_exp ')'
   ;

read_phrase
   : read_key read_arg
   | read_phrase ',' read_arg
   ;

write_phrase
   : write_key write_arg
   | write_phrase ',' write_arg
   ;

read_arg
   : variable
   | io_control
   ;

write_arg
   : expression
   | io_control
   ;

read_key
   : READ '(' number ')'
   | READALL '(' number ')'
   ;

write_key
   : WRITE '(' number ')'
   ;

block_definiton
   : block_stmt block_body closing ';'
   ;

block_body
   : empty
   | declare_group
   | block_body any_statement
   ;

arith_unline_def
   : FUNCTION arith_spec ';'
   | FUNCTION ';'
   ;

bit_inline_def
   : FUNCTION bit_spec ';'
   ;

char_inline_def
   : FUNCTION char_spec ';'
   ;

struc_inline_def
   : FUNCTION struct_spec ';'
   ;

block_stmt
   : block_stmt_top ';'
   ;

block_stmt_top
   : block_stmt_top ACCESS
   | block_stmt_top RIGID
   | block_stmt_head
   | block_stmt_head EXCLUSIVE
   | block_stmt_head REENTRANT
   ;

label_definition
   : label ':'
   ;

label_external
   : label_definition
   | label_definition EXTERNAL
   ;

block_stmt_head
   : label_external PROGRAM
   | label_external COMPOOL
   | label_definition TASK
   | label_definition UPDATE
   | UPDATE
   | function_name
   | function_name func_stmt_body
   | procedure_name
   | procedure_name proc_stmt_body
   ;

function_name
   : label_external FUNCTION
   ;

procedure_name
   : label_external PROCEDURE
   ;

func_stmt_body
   : parameter_list
   | type_spec
   | parameter_list type_spec
   ;

proc_stmt_body
   : parameter_list
   | assign_list
   | parameter_list assign_list
   ;

parameter_list
   : parameter_head identifier ')'
   ;

parameter_head
   : '('
   | parameter_head identifier ','
   ;

assign_list
   : assign parameter_list
   ;

assign
   : ASSIGN
   ;

declare_element
   : declare_statement
   | replace_stmt
   | structure_stmt
   | EQUATE EXTERNAL identifier TO variable ';'
   ;

replace_stmt
   : REPLACE replace_head BY text_ ';'
   ;

replace_head
   : identifier
   | identifier '(' arg_list ')'
   ;

arg_list
   : identifier
   | arg_list ',' identifier
   ;

text_
   : expression
   ;

temporary_stmt
   : TEMPORARY declare_body ';'
   ;

declare_statement
   : DECLARE declare_body ';'
   ;

declare_body
   : declaration_list
   | attributes ',' declaration_list
   ;

declaration_list
   : declaration
   | declaration_list ',' declaration
   ;

declare_group
   : declare_element
   | declare_group declare_element
   ;

structure_stmt
   : STRUCTURE struct_stmt_head struct_stmt_tail
   ;

struct_stmt_head
   : identifier ':' level
   | identifier minor_attr_list ':' level
   | struct_stmt_head declaration ',' level
   ;

struct_stmt_tail
   : declaration ';'
   ;

struct_spec
   : struct_template struct_spec_body
   ;

struct_spec_body
   : '-' STRUCTURE
   | struct_spec_head literal_exp_or_star ')'
   ;

struct_spec_head
   : '-' STRUCTURE '('
   ;

declaration
   : name_id
   | name_id attributes
   ;

name_id
   : identifier
   | identifier NAME
   ;

attributes
   : array_spec type_and_minor_attr
   | array_spec
   | type_and_minor_attr
   ;

array_spec
   : array_head literal_exp_or_star ')'
   | FUNCTION
   | PROCEDURE
   | PROGRAM
   | TASK
   ;

array_head
   : ARRAY '('
   | array_head literal_exp_or_star ','
   ;

type_and_minor_attr
   : type_spec
   | type_spec minor_attr_list
   | minor_attr_list
   ;

type_spec
   : struct_spec
   | bit_spec
   | char_spec
   | arith_spec
   | EVENT
   ;

bit_spec
   : BOOLEAN
   | BIT '(' literal_exp_or_star ')'
   ;

char_spec
   : CHARACTER '(' literal_exp_or_star ')'
   ;

arith_spec
   : prec_spec
   | sq_dq_name
   | sq_dq_name prec_spec
   ;

sq_dq_name
   : doubly_qual_name_head literal_exp_or_star ')'
   | INTEGER
   | SCALAR
   | VECTOR
   | MATRIX
   ;

doubly_qual_name_head
   : VECTOR '('
   | MATRIX '(' literal_exp_or_star ','
   ;

literal_exp_or_star
   : arith_exp
   | '*'
   ;

prec_spec
   : SINGLE
   | DOUBLE
   ;

minor_attr_list
   : minor_attribute
   | minor_attr_list minor_attribute
   ;

minor_attribute
   : STATIC
   | AUTOMATIC
   | DENSE
   | ALIGNED
   | ACCESS
   | LOCK '(' literal_exp_or_star ')'
   | REMOTE
   | RIGID
   | init_const_head repeated_constant ')'
   | init_const_head '*)'
   | LATCHED
   | NONHAL '(' level ')'
   ;

init_const_head
   : INITIAL '('
   | CONSTANT '('
   | init_const_head repeated_constant ','
   ;

repeated_constant
   : EXPRESSION
   | repeat_head variable
   | repeat_head constant
   | nested_repeat_head repeated_constant ')'
   | repeat_head
   ;

repeat_head
   : arith_exp '#'
   ;

nested_repeat_head
   : repeat_head '('
   | nested_repeat_head repeated_constant ','
   ;

constant
   : number
   | compound_number
   | bit_const
   | char_const
   ;

number
   : simple_number
   | level
   ;

simple_number
   : NUMBER
   ;

level
   : NUMBER
   ;

closing
   : CLOSE
   | CLOSE label
   | label_definition closing
   ;

terminator
   : TERMINATE
   | CANCEL
   ;

terminate_list
   : label_var
   | terminate_list ',' label_var
   ;

wait_key
   : WAIT
   ;

schedule_head
   : SCHEDULE label_var
   | schedule_head AT arith_exp
   | schedule_head IN arith_exp
   | schedule_head ON bit_exp
   ;

schedule_phrase
   : schedule_head
   | schedule_head PRIORITY '(' arith_exp ')'
   | schedule_phrase DEPENDENT
   ;

schedule_control
   : stopping
   | timing
   | timing stopping
   ;

timing
   : repeat EVERY arith_exp
   | repeat AFTER arith_exp
   | repeat
   ;

repeat
   : ',' REPEAT
   ;

stopping
   : while_key arith_exp
   | while_key bit_exp
   ;

identifier
   : IDENTIFIER
   ;

char_string
   : STRING
   ;

label
   : IDENTIFIER
   ;

empty
   :
   ;

STRING
   : '\'' (~ '\'' | '\'\'')* '\''
   ;

NUMBER
   : DIGITS ('.' DIGITS?)? EXPONENT*
   | '.' DIGITS EXPONENT*
   ;

fragment DIGITS
   : [0-9]+
   ;

fragment EXPONENT
   : [BEH] [+-]? DIGITS
   ;

PCT_NAME
   : '%' [a-zA-Z_] [a-zA-Z0-9_]*
   ;

ACCESS
   : 'ACCESS'
   ;

AFTER
   : 'AFTER'
   ;

ALIGNED
   : 'ALIGNED'
   ;

AND
   : 'AND'
   ;

ARRAY
   : 'ARRAY'
   ;

ASSIGN
   : 'ASSIGN'
   ;

AT
   : 'AT'
   ;

AUTOMATIC
   : 'AUTOMATIC'
   ;

BIN
   : 'BIN'
   ;

BIT
   : 'BIT'
   ;

BOOLEAN
   : 'BOOLEAN'
   ;

BY
   : 'BY'
   ;

CALL
   : 'CALL'
   ;

CANCEL
   : 'CANCEL'
   ;

CASE
   : 'CASE'
   ;

CAT
   : 'CAT'
   ;

CHAR
   : 'CHAR'
   ;

CHARACTER
   : 'CHARACTER'
   ;

CLOSE
   : 'CLOSE'
   ;

COLUMN
   : 'COLUMN'
   ;

COMPOOL
   : 'COMPOOL'
   ;

CONSTANT
   : 'CONSTANT'
   ;

DEC
   : 'DEC'
   ;

DECLARE
   : 'DECLARE'
   ;

DENSE
   : 'DENSE'
   ;

DEPENDENT
   : 'DEPENDENT'
   ;

DO
   : 'DO'
   ;

DOUBLE
   : 'DOUBLE'
   ;

ELSE
   : 'ELSE'
   ;

END
   : 'END'
   ;

EQUATE
   : 'EQUATE'
   ;

ERROR
   : 'ERROR'
   ;

EVENT
   : 'EVENT'
   ;

EVERY
   : 'EVERY'
   ;

EXCLUSIVE
   : 'EXCLUSIVE'
   ;

EXIT
   : 'EXIT'
   ;

EXPRESSION
   : 'EXPRESSION'
   ;

EXTERNAL
   : 'EXTERNAL'
   ;

FALSE
   : 'FALSE'
   ;

FILE
   : 'FILE'
   ;

FOR
   : 'FOR'
   ;

FUNCTION
   : 'FUNCTION'
   ;

GO
   : 'GO'
   ;

HEX
   : 'HEX'
   ;

IF
   : 'IF'
   ;

IGNORE
   : 'IGNORE'
   ;

IN
   : 'IN'
   ;

INITIAL
   : 'INITIAL'
   ;

INTEGER
   : 'INTEGER'
   ;

LATCHED
   : 'LATCHED'
   ;

LINE
   : 'LINE'
   ;

LOCK
   : 'LOCK'
   ;

MATRIX
   : 'MATRIX'
   ;

NAME
   : 'NAME'
   ;

NONHAL
   : 'NONHAL'
   ;

NOT
   : 'NOT'
   ;

NULL
   : 'NULL'
   ;

OCT
   : 'OCT'
   ;

OFF
   : 'OFF'
   ;

ON
   : 'ON'
   ;

OR
   : 'OR'
   ;

PAGE
   : 'PAGE'
   ;

PRIORITY
   : 'PRIORITY'
   ;

PROCEDURE
   : 'PROCEDURE'
   ;

PROGRAM
   : 'PROGRAM'
   ;

READ
   : 'READ'
   ;

READALL
   : 'READALL'
   ;

REENTRANT
   : 'REENTRANT'
   ;

REMOTE
   : 'REMOTE'
   ;

REPEAT
   : 'REPEAT'
   ;

REPLACE
   : 'REPLACE'
   ;

RESET
   : 'RESET'
   ;

RETURN
   : 'RETURN'
   ;

RIGID
   : 'RIGID'
   ;

SCALAR
   : 'SCALAR'
   ;

SCHEDULE
   : 'SCHEDULE'
   ;

SEND
   : 'SEND'
   ;

SET
   : 'SET'
   ;

SIGNAL
   : 'SIGNAL'
   ;

SINGLE
   : 'SINGLE'
   ;

SKIP_IO
   : 'SKIP'
   ;

STATIC
   : 'STATIC'
   ;

STRUCTURE
   : 'STRUCTURE'
   ;

SUBBIT
   : 'SUBBIT'
   ;

SYSTEM
   : 'SYSTEM'
   ;

TAB
   : 'TAB'
   ;

TASK
   : 'TASK'
   ;

TEMPORARY
   : 'TEMPORARY'
   ;

TERMINATE
   : 'TERMINATE'
   ;

THEN
   : 'THEN'
   ;

TO
   : 'TO'
   ;

TRUE
   : 'TRUE'
   ;

UNTIL
   : 'UNTIL'
   ;

UPDATE
   : 'UPDATE'
   ;

VECTOR
   : 'VECTOR'
   ;

WAIT
   : 'WAIT'
   ;

WHILE
   : 'WHILE'
   ;

WRITE
   : 'WRITE'
   ;

IDENTIFIER
   : [a-zA-Z] ([a-zA-Z0-9] | '_' [a-zA-Z0-9])*
   ;

COMMENT
   : '/*' .*? '*/' -> skip
   ;

WS
   : [ \r\n\t]+ -> skip
   ;

