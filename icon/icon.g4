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

grammar icon;

start
   : prog EOF
   ;

prog
   : declaration
   | (declaration prog)
   ;

endOfExpr
   : ';'
   ;

declaration
   : link_declaration
   | global_declaration
   | record_declaration
   | procedure_declaration
   ;

link_declaration
   : 'link' link_list
   ;

link_list
   : file_name
   | file_name ',' link_list
   ;

file_name
   : identifier
   | string_literal
   ;

global_declaration
   : 'global' identifier_list
   ;

identifier_list
   : identifier
   | identifier_list ',' identifier
   ;

record_declaration
   : 'record' identifier '(' field_list_opt ')'
   ;

field_list_opt
   : field_list
   ;

field_list
   : field_name
   | field_list ',' field_name
   ;

field_name
   : identifier
   ;

procedure_declaration
   : proc_header locals_opt? initial_opt? expression_sequence 'end'
   ;

proc_header
   : 'procedure' identifier '(' parameter_list_opt? ')'
   ;

parameter_list_opt
   : parameter_list
   ;

parameter_list
   : (identifier)
   | (identifier '[' ']')
   | (identifier ',' parameter_list)
   ;

locals_opt
   : localz
   ;

localz
   : local_specification identifier_list
   | local_specification identifier_list endOfExpr localz
   ;

local_specification
   : 'local'
   | 'static'
   ;

initial_opt
   : 'initial' expression endOfExpr
   |
   ;

expression_sequence
   : expression_opt
   | expression_sequence endOfExpr expression_opt
   ;

expression_opt
   : expression
   ;

expression
   : 'break' expression_opt
   | 'create' expression
   | 'return' expression_opt
   | 'suspend' expression_opt suspend_do_clause_opt
   | 'fail'
   | 'next'
   | 'case' expression 'of' '{' case_list '}'
   | 'if' expression 'then' expression else_clause_opt
   | 'repeat' expression
   | 'while' expression while_do_clause_opt
   | 'until' expression until_do_clause_opt
   | 'every' expression every_do_clause_opt
   | expr1
   ;

suspend_do_clause_opt
   : 'do' expression
   ;

while_do_clause_opt
   : 'do' expression
   ;

until_do_clause_opt
   : 'do' expression
   ;

every_do_clause_opt
   : 'do' expression
   ;

else_clause_opt
   : 'else' expression
   ;

case_list
   : case_clause
   | case_list endOfExpr case_clause
   ;

case_clause
   : expression ':' expression
   | 'default' ':' expression
   ;

expr1
   : expr1 '&' expr2
   | expr2
   ;

expr2
   : expr2 '?' expr3
   | expr3
   ;

expr3
   : expr4 ':=' expr3
   | expr4 ':=:' expr3
   | expr4 '<-' expr3
   | expr4 '<->' expr3
   | expr4
   ;

expr4
   : expr4 'to' expr5
   | expr4 'to' expr5 'by' expr5
   | expr5
   ;

expr5
   : expr5 '|' expr6
   | expr6
   ;

expr6
   : expr6 '<' expr7
   | expr6 '<=' expr7
   | expr6 '=' expr7
   | expr6 '>=' expr7
   | expr6 '>' expr7
   | expr6 '~=' expr7
   | expr6 '<<' expr7
   | expr6 '<<=' expr7
   | expr6 '==' expr7
   | expr6 '>>=' expr7
   | expr6 '>>' expr7
   | expr6 '~==' expr7
   | expr6 '===' expr7
   | expr6 '~===' expr7
   | expr7
   ;

expr7
   : expr7 '||' expr8
   | expr7 '|||' expr8
   | expr8
   ;

expr8
   : expr8 '+' expr9
   | expr8 '-' expr9
   | expr8 '++' expr9
   | expr8 '--' expr9
   | expr9
   ;

expr9
   : expr9 '*' expr10
   | expr9 '/' expr10
   | expr9 '%' expr10
   | expr9 '**' expr10
   | expr10
   ;

expr10
   : expr11 '^' expr10
   | expr11
   ;

expr11
   : expr11 '\\' expr12
   | expr11 '@' expr12
   | expr11 '!' expr12
   | expr12
   ;

expr12
   : 'not' expr12
   | '|' expr12
   | '!' expr12
   | '*' expr12
   | '+' expr12
   | '-' expr12
   | '.' expr12
   | '/' expr12
   | '\\' expr12
   | '=' expr12
   | '?' expr12
   | '~' expr12
   | '@' expr12
   | '^' expr12
   | expr13
   ;

expr13
   : '(' expression_list ')'
   | '{' expression_sequence '}'
   | '[' expression_list ']'
   | expr13 '.' field_name
   | expr13 '(' expression_list ')'
   | expr13 '{' expression_list '}'
   | expr13 '[' subscript_list ']'
   | identifier
   | keyword
   | literal
   ;

expression_list
   : expression_opt
   | expression_list ',' expression_opt
   ;

subscript_list
   : subscript_
   | subscript_list ',' subscript_
   ;

subscript_
   : expression
   | expression ':' expression
   | expression '+:' expression
   | expression '-:' expression
   ;

keyword
   : '&' identifier
   ;

identifier
   : IDENTIFIER
   ;

literal
   : string_literal
   | integer_literal
   | real_literal
   ;

string_literal
   : STRING_LITERAL
   ;

real_literal
   : REAL_LITERAL
   ;

integer_literal
   : INTEGER_LITERAL
   ;


IDENTIFIER
   : [a-zA-Z] [a-zA-Z0-9]*
   ;


INTEGER_LITERAL
   : ('0' .. '9') +
   ;


REAL_LITERAL
   : ('0' .. '9')* '.' ('0' .. '9') + (('e' | 'E') ('0' .. '9') +)*
   ;


STRING_LITERAL
   : '"' ~ ["]* '"'
   ;


WS
   : [ \t\r\n] -> skip
   ;
