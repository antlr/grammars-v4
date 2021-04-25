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

grammar plucid;

program : expression;
expression : constant
| identifier
| 'error'
| 'eod'
| prefix_operator expression
| expression infix_operator expression
| 'filter' ( expression ',' expression ','  expression )
| 'substr' ( expression ',' expression ','  expression )
| 'length' expression
| 'arg' expression
| list_expression
| if_expression
| case_expression
| cond_expression
| function_call
| where_clause;

constant : numeric_constant
| word_constant
| STRING_CONSTANT
| list_constant;


numeric_constant : integer_constant
| real_constant;

integer_constant : DIGIT+
| (n_sign integer_constant);

real_constant : integer_constant '.'  DIGIT*;
n_sign : '˜';

word_constant: quote word_constant_less_the_quotes quote;

word_constant_less_the_quotes : (LETTER  ALPHANUMERIC *)
| SIGN+
| BRACKET
| period
| separator
| quote;

SIGN : '+' | '-' | '*' | '$' | '&' | '=' | '<' | '>' | ':' | '#' | 'ˆ';

quote : '"';
BRACKET : '(' | ')' | '[%' | '%]' |'(%' | '%)' ;

period : '.';
 separator : ',' | ';';

STRING_CONSTANT : '‘' .*? '’';
list_constant : 'nil' | '[]' |'[' list_constant_element* ']';


list_constant_element : numeric_constant
| word_constant_less_the_quotes
| STRING_CONSTANT
| list_constant;

ALPHANUMERIC : DIGIT | LETTER;
DIGIT : [0-9];
LETTER : [a-zA-Z];
identifier: LETTER  ALPHANUMERIC* ;
prefix_operator : p_numeric_operator
| p_word_operator
| p_string_operator
| p_list_operator
| p_lucid_operator
| p_special_operator;
p_numeric_operator : 'sin' | 'cos' | 'tan' | 'sqrt' | 'abs' | 'log10'| 'log' | 'isnumber';
p_word_operator : 'isword' | 'not' | 'mkstring';
p_string_operator : 'isstring' | 'mkword';
p_list_operator : 'hd' | 'tl' | 'isatom' | 'isnull' | 'islist';
p_lucid_operator : 'first' | 'next';
p_special_operator : 'iseod' | 'iserror';
infix_operator : i_numeric_operator
| i_word_operator
| i_string_operator
| i_list_operator
| i_lucid_operator;

i_numeric_operator : '+' | '-' | '**' | '*' | 'div'|'mod' | '/'
| 'eq' | 'ne' | '<=' |'<' | '>' | '>=';

i_word_operator : 'and' | 'or' | 'eq' | 'ne';
i_string_operator : 'ˆ' | 'eq' | 'ne';
i_list_operator : '<>' | '::' | 'eq' | 'ne';
i_lucid_operator : 'fby' | 'whenever'|'wvr' | 'upon' | 'asa' | 'attime';
list_expression : '[%%]'
| ('[%' expressions_list* '%]');

expressions_list : expression_item
| (expression_item ',' expressions_list*);
expression_item : expression
| list_expression;
if_expression : 'if' expression 'then' expression endif;
endif : 'else' expression 'fi'
| 'elseif' expression 'then' expression 'endif';
case_expression : 'case' expression 'of' cbody 'end';
cond_expression : 'cond' cbody 'end';
cbody : (expression ':' expression ';')* defaultcase;
defaultcase : 'default' ':' expression;
function_call : identifier '(' actuals_list ')';
actuals_list : expression
| (expression ',' actuals_list);
where_clause : expression 'where' body 'end';
body : declarations_list definitions_list;
declarations_list : ( current_declaration ';' )*;
current_declaration : identifier 'is' 'current' expression;
definitions_list : ( definition';' )*;
definition: simple_definition
| function_definition;
simple_definition : identifier '=' expression;
function_definition:
identifier ( formals_list ) '=' expression;
formals_list : identifier
| identifier ',' formals_list;


WS
   : [ \r\n\t] + -> skip
   ;
