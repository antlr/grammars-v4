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
grammar plus;

program
   : statement_list
   ;

statement_list
   : statement (';' statement)* ';'?
   ;

statement
   : type_delaration
   | variable_declaration
   | constant_declaration
   | procedure_declaration
   | macro_declaration
   | open_declaration
   | equate_declaration
   | esape
   | return
   | if_statement
   | pctif_statement
   | select_statement
   | assertion
   | assignment
   | compound
   | storage_reference
   | procedure_definition
   | global_pack
   ;

global_pack
   : 'global' identifier external_name statement_list global_end
   ;

global_end
   : 'end' optional_id
   | 'end' 'global' optional_id
   ;

optional_id
   : identifier?
   ;

external_name
   : ('external' constant_expression)?
   ;

type_delaration
   : 'type' id_list 'is' type
   | type_delaration ',' id_list 'is' type
   ;

variable_declaration
   : 'variable' declaration_element
   | variable_declaration ',' declaration_element
   ;

declaration_element
   : id_list 'is' type allocation
   ;

allocation
   : ('in' 'register' | 'in' 'register' constant_expression | 'in' 'register' constant_expression 'to' constant_expression | 'in' 'storage' | 'at' 'absolute' constant_expression | 'external' | 'external' constant_expression | 'entry' | 'entry' constant_expression)?
   ;

constant_declaration
   : 'constant' id_list 'is' constant_expression allocation
   | constant_declaration ',' id_list 'is' constant_expression allocation
   ;

macro_declaration
   : macro_head macro_body macro_end
   ;

macro_end
   : 'end' optional_id
   | 'end' 'macro' optional_id
   ;

macro_head
   : 'macro' identifier macro_parameters
   ;

macro_parameters
   : ('parameter' 'is' id_list)?
   ;

macro_body
   : statement_list
   | parenthesized_expression
   ;

open_declaration
   : 'open' open_element
   | open_declaration ',' open_element
   ;

open_element
   : storage_reference equate_type
   ;

equate_declaration
   : 'equate' equate_element
   | equate_declaration ',' equate_element
   ;

equate_element
   : identifier 'to' storage_reference equate_type
   ;

equate_type
   : ('as' type)?
   ;

procedure_declaration
   : procedure_head id_list proc_specifications
   | procedure_decl
   ;

procedure_head
   : 'procedure'
   ;

procedure_decl
   : 'proedure' id_list 'is' type proc_specifications
   | procedure_decl ',' id_list 'is' type proc_specifications
   ;

proc_specifications
   : ('external' constant_expression | linkage | 'stacksize' constant_expression)*
   ;

linkage
   : 'linkage' constant_expression
   | 'linkage' 'system'
   | 'linkage' 'none'
   ;

procedure_definition
   : definition_head statement_list procedure_end
   ;

definition_head
   : 'definition' identifier
   | 'definition' identifier entry_options
   | procedure_declaration 'definition'
   | procedure_declaration 'definition' entry_options
   ;

entry_options
   : entry_option
   | entry_options entry_option
   ;

entry_option
   : linkage
   | 'environment' storage_reference
   ;

procedure_end
   : 'end' optional_id
   | 'end' 'procedure' optional_id
   | 'end' 'definition' optional_id
   ;

id_list
   : identifier
   | id_list ',' identifier
   ;

type
   : attribute type
   | basic_type
   ;

basic_type
   : '(' id_list ')'
   | 'bit' '(' constant_expression ')'
   | 'real' '(' constant_expression ')'
   | 'character' '(' constant_expression ')'
   | 'charater' '(' constant_expression 'to' constant_expression ')'
   | '(' constant_expression 'to' constant_expression ')'
   | record_type
   | 'pointer' 'to' type
   | 'set' 'of' type
   | 'array' type 'of' type
   | procedure_type
   | 'unknown'
   | 'global' '(' constant_expression ')'
   | identifier
   ;

attribute
   : 'packed'
   | 'aligned' alignment 'left'
   | 'aligned' alignment 'right'
   | 'fast'
   | 'small'
   | 'value'
   | 'left'
   | 'right'
   | 'environment' type
   | 'system'
   ;

alignment
   : constant_expression
   | constant_expression 'in' constant_expression
   ;

record_type
   : 'record' field_list variant_part end_record
   ;

end_record
   : 'end'
   | 'end' 'record'
   ;

field_list
   : (declaration_element (',' declaration_element)*)?
   ;

variant_part
   : (variant_list | variant_list variant_else)?
   ;

variant_list
   : variant_list variant_element
   | variant_head
   ;

variant_head
   : 'variant' identifier 'of' type 'from'
   | 'variant' type 'from'
   ;

variant_element
   : variant_label_list ':' field_list
   ;

variant_label_list
   : 'case' constant_expression
   | variant_label_list ',' expression
   ;

variant_else
   : 'else' field_list
   ;

procedure_type
   : 'proedure' parameter_list result_part 'end'
   ;

parameter_list
   : parameter_part*
   ;

parameter_part
   : parameter_kind declaration_element
   | parameter_part ',' declaration_element
   ;

parameter_kind
   : optional reference 'parameter'
   ;

optional
   : ('optional' | 'repeated')
   ;

reference
   : ('name' | 'reference')?
   ;

result_part
   : ('result' declaration_element | 'optional' 'result' declaration_element)?
   ;

esape
   : escape_type optional_label when_unless
   ;

escape_type
   : 'exit'
   | 'repeat'
   ;

optional_label
   : label?
   ;

label
   : identifier
   ;

when_unless
   : ('when' expression | 'unless' expression)?
   ;

return
   : 'return' when_unless with_part
   | 'return' with_part when_unless
   | 'return' when_unless
   ;

with_part
   : 'with' expression
   ;

if_statement
   : 'if' if_then_else end_if
   ;

if_then_else
   : expression then_part else_part
   ;

end_if
   : 'end'
   | 'end' 'if'
   ;

then_part
   : 'then' statement_list
   ;

else_part
   : ('else' statement_list | 'elseif' if_then_else)?
   ;

pctif_statement
   : '%if' pctif_then_else pctend_if
   ;

pctif_then_else
   : constant_expression pctthen_part pctelse_part
   ;

pctend_if
   : '%end'
   | '%end' '%if'
   | '%end' 'if'
   ;

pctthen_part
   : '%then' statement_list
   ;

pctelse_part
   : ('%else' statement_list | '%elseif' pctif_then_else)?
   ;

select_statement
   : select_start select_alternatives end_select
   ;

select_start
   : 'select' expression 'from'
   ;

end_select
   : 'end'
   | 'end' 'select'
   ;

select_alternatives
   : select_alternatives_list
   | select_alternatives_list 'else' statement_list
   ;

select_alternatives_list
   : select_alternative*
   ;

select_alternative
   : select_label_list ':' statement_list
   ;

select_label_list
   : 'case' constant_expression
   | select_label_list ',' expression
   ;

assertion
   : 'assert' expression
   ;

assignment
   : storage_reference assign_op expression
   | storage_reference ',' assignment
   ;

assign_op
   : ':='
   | adding_op ':='
   | multiplying_op ':='
   ;

compound
   : label unlabelled_compound label
   | unlabelled_compound
   ;

unlabelled_compound
   : 'begin' statement_list 'end'
   | 'cycle' statement_list end_cycle
   | do_head statement_list end_do
   ;

end_cycle
   : 'end'
   | 'end' 'cycle'
   ;

end_do
   : 'end'
   | 'end' 'do'
   ;

do_head
   : 'do' storage_reference ':=' expression direction expression
   | 'do' storage_reference ':=' 'each' expression
   ;

direction
   : 'to'
   | 'downto'
   ;

storage_reference
   : identifier
   | '%' identifier
   | storage_reference '(' (expression (',' expression)*)? (('return' 'code' | 'with') storage_reference)? ')'
   | storage_reference '.' identifier
   | storage_reference '@'
   ;

constant_expression
   : expression
   ;

expression
   : logical_formula
   | conjunction
   | disjunction
   ;

disjunction
   : logical_formula 'or' logical_formula
   | disjunction 'or' logical_formula
   ;

conjunction
   : logical_formula 'and' logical_formula
   | conjunction 'and' logical_formula
   ;

logical_formula
   : arithmetic_expression relation arithmetic_expression
   | arithmetic_expression
   ;

relation
   : relation_op
   | 'FIX ME' relation_op
   | 'not' relation_op
   ;

relation_op
   : '<'
   | '<='
   | '<<'
   | '<<='
   | '>'
   | '>='
   | '>>'
   | '>>='
   | '='
   | 'in'
   | 'subset'
   ;

arithmetic_expression
   : term
   | arithmetic_expression adding_op term
   ;

adding_op
   : '+'
   | '-'
   | '||'
   | '|'
   | 'xor'
   ;

term
   : primary
   | term multiplying_op primary
   ;

multiplying_op
   : '*'
   | '/'
   | 'mod'
   | '&'
   ;

primary
   : unary_op primary
   | storage_reference
   | number
 //  | bit_string
   | STRING
   | parenthesized_expression
   | set '}'
   | '{' '}'
   ;

parenthesized_expression
   : '(' expression ')'
   ;

set
   : '{' expression
   | set ',' expression
   ;

unary_op
   : '+'
   | '-'
   | ':'
   | 'not'
   | 'abs'
   ;

identifier
   : IDENTIFIER
   ;

number:
    ('+' | '-') NUMBER ('.' NUMBER)?;

STRING:
    '"' ~'"'* '"';

NUMBER:
    [0-9]+;


IDENTIFIER
   : [a-zA-Z] [a-zA-Z0-9_]*
   ;

COMMENT
   : '/*' .*? '*/' -> skip
   ;

WS
   : [ \r\n\t]+ -> skip
   ;

