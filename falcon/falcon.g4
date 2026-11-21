/*
BSD License

Copyright (c) 2026, Tom Everett
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

grammar falcon;

program_
   : NEWLINE* (module_level_statement | statement)* EOF
   ;

module_level_statement
   : load_statement
   | import_statement
   | export_statement
   | compiler_directive
   | declaration_statement
   ;

statement
   : assignment_statement terminator
   | return_statement
   | expression terminator
   | if_statement
   | while_statement
   | forto_statement
   | forin_statement
   | loop_statement
   | switch_statement
   | select_statement
   | try_statement
   | const_statement
   | def_statement
   | declaration_statement
   | empty
   ;

empty
   : terminator
   ;

terminator
   : ';'
   | NEWLINE+
   ;

load_statement
   : 'load' (module_name | module_path) terminator
   ;

import_statement
   : 'import' name_list terminator
   | 'import' name_list 'from' (module_name | module_path) ('in' ('self' | NAME))? terminator
   | 'import' NAME 'from' (module_name | module_path) ('as' NAME)? terminator
   ;

module_name
   : ('self' '.' | '.')? NAME ('.' NAME)*
   ;

module_path
   : STRING
   ;

export_statement
   : 'export' name_list terminator
   ;

compiler_directive
   : 'directive' ('lang' '=' language_code | 'strict' '=' ('on' | 'off') | 'version' '=' NUMBER)* terminator
   ;

name_list
   : NAME (',' NAME)*
   ;

language_code
   : NAME
   ;

assignment_statement
   : lvalue_list ':' expression_list                                                              # simpleAssign
   | lvalue op = ('+=' | '-=' | '*=' | '/=' | '**=' | '%=' | '>>=' | '<<=' | '^&=' | '^|=' | '^^=' | '^=') expression   # autoAssign
   ;

lvalue
   : (NAME | 'self' | 'fself') ('.' NAME | '[' expression ']')*
   ;

lvalue_list
   : lvalue (',' lvalue)*
   ;

expression
   : expression '.' NAME                                                                 # accessExpr
   | expression '[' expression ']'                                                       # indexExpr
   | expression '(' expression_list ')'                                                  # callExpr
   | expression op = ('++' | '--')                                                       # postIncDecExpr
   | op = ('++' | '--') expression                                                       # preIncDecExpr
   | '-' expression                                                                       # unaryMinusExpr
   | '^~' expression                                                                       # bitNotExpr
   | op = ('^?' | '^+' | '^-' | '^%') expression                                          # outOfBandExpr
   | '@' expression                                                                        # stringExpansionExpr
   | '#' expression                                                                        # indirectExpr
   | '^*' expression                                                                        # fastEvalExpr
   | 'not' expression                                                                        # notExpr
   | '$' expression                                                                           # referenceExpr
   | '&' NAME                                                                                 # symbolExpr
   | expression '**' expression                                                               # powerExpr
   | expression op = ('*' | '/' | '%') expression                                             # mulDivModExpr
   | expression op = ('+' | '-') expression                                                    # addSubExpr
   | expression op = ('>>' | '<<') expression                                                  # shiftExpr
   | expression op = ('^&' | '^|' | '^^') expression                                            # bitwiseExpr
   | expression op = ('==' | '!=' | '>' | '<' | '>=' | '<=') expression                          # compareExpr
   | expression op = ('and' | 'or' | 'in' | 'notin' | 'provides') expression                     # logicExpr
   | expression '?' expression ':' expression                                                     # choiceExpr
   | immediate                                                                                      # immediateExpr
   | NAME                                                                                           # nameExpr
   | array_declaration                                                                              # arrayDeclExpr
   | dictionary_declaration                                                                         # dictDeclExpr
   | anonymous_function_declaration                                                                 # anonFuncExpr
   | '(' expression ')'                                                                             # parenExpr
   ;

expression_list
   : (expression (',' expression)*)?
   ;

immediate
   : NUMBER
   | STRING
   | value_token
   ;

value_token
   : 'nil'
   | 'true'
   | 'false'
   | 'self'
   | 'fself'
   ;

array_declaration
   : '[' expression_list ']'
   | '.[' expression_sequence ']'
   ;

expression_sequence
   : expression*
   ;

dictionary_declaration
   : ('[' | '^[' | '.[' | '^.[') '=>' ']'
   | ('[' | '^[') expression_pair_list ']'
   | ('.[' | '^.[') expression_pair_sequence ']'
   ;

expression_pair_list
   : expression_pair (',' expression_pair)*
   ;

expression_pair_sequence
   : expression_pair+
   ;

expression_pair
   : expression '=>' expression
   ;

anonymous_function_declaration
   : 'function' '(' name_list? ')' terminator statement* 'end'
   | '{' name_list? '=>' expression '}'
   | '{' name_list? '=>' statement* '}'
   ;

declaration_statement
   : function_declaration
   | class_declaration
   | enum_declaration
   | object_declaration
   ;

function_declaration
   : 'function' NAME '(' name_list? ')' terminator statement* 'end' terminator
   | 'function' NAME '(' name_list? ')' ':' statement
   ;

enum_declaration
   : 'enum' NAME terminator enum_entry_list terminator 'end' terminator
   ;

enum_entry_list
   : enum_entry (',' terminator? enum_entry)*
   ;

enum_entry
   : NAME ('=' immediate)?
   ;

class_declaration
   : 'class' NAME ('(' name_list? ')')? from_clause? terminator class_element_declaration* 'end' terminator
   ;

from_clause
   : 'from' namespaced_name ('(' expression_list ')')? (',' namespaced_name ('(' expression_list ')')?)*
   ;

namespaced_name
   : NAME ('.' NAME)*
   ;

class_element_declaration
   : init_declaration
   | property_declaration
   | function_declaration
   | state_declaration
   ;

init_declaration
   : 'init' terminator statement* 'end' terminator
   | 'init' ':' statement
   ;

property_declaration
   : NAME '=' expression terminator
   ;

state_declaration
   : '[' NAME ']' terminator function_declaration* 'end' terminator
   ;

object_declaration
   : 'object' NAME from_clause? terminator class_element_declaration* 'end' terminator
   ;

if_statement
   : 'if' expression ':' statement
   | 'if' expression terminator statement* ('elif' expression terminator statement*)* ('else' terminator statement*)? 'end' terminator
   ;

while_statement
   : 'while' expression ':' statement
   | 'while' expression terminator statement_in_loop* 'end' terminator
   ;

forto_statement
   : forto_heading ':' statement
   | forto_heading terminator statement_in_for* 'end' terminator
   ;

forto_heading
   : 'for' NAME '=' NUMBER 'to' NUMBER ('step' NUMBER)?
   ;

forin_statement
   : 'for' name_list 'in' expression ':' statement
   | 'for' name_list 'in' expression terminator statement_in_for* 'end' terminator
   ;

loop_statement
   : 'loop' terminator statement_in_loop* 'end' expression? terminator
   ;

statement_in_loop
   : statement
   | 'continue' terminator
   | 'break' terminator
   ;

statement_in_for
   : forfirst_block
   | formiddle_block
   | forlast_block
   | statement_in_loop
   ;

forfirst_block
   : 'forfirst' ':' statement_in_loop
   | 'forfirst' terminator statement_in_loop* 'end' terminator
   ;

formiddle_block
   : 'formiddle' ':' statement_in_loop
   | 'formiddle' terminator statement_in_loop* 'end' terminator
   ;

forlast_block
   : 'forlast' ':' statement_in_loop
   | 'forlast' terminator statement_in_loop* 'end' terminator
   ;

switch_statement
   : 'switch' expression terminator case_statement* default_statement? 'end' terminator
   ;

select_statement
   : 'select' expression terminator case_statement* default_statement? 'end' terminator
   ;

case_statement
   : 'case' case_limits ':' statement
   | 'case' case_limits terminator statement*
   ;

default_statement
   : 'default' ':' statement
   | 'default' terminator statement*
   ;

case_limits
   : case_limit (',' case_limit)*
   ;

case_limit
   : expression ('to' expression)?
   ;

try_statement
   : 'try' terminator statement* ('catch' NAME? ('in' NAME)? terminator statement*)* 'end' terminator
   ;

const_statement
   : 'const' multiple_assignment terminator
   ;

def_statement
   : 'def' multiple_assignment terminator
   ;

return_statement
   : 'return' expression? terminator
   ;

multiple_assignment
   : NAME '=' expression (',' NAME '=' expression)*
   ;

NAME
   : [a-zA-Z_] [a-zA-Z_0-9]*
   ;

NUMBER
   : '0x' HEXDIGIT ('_'? HEXDIGIT)*
   | '0o' OCTDIGIT ('_'? OCTDIGIT)*
   | '0b' BINDIGIT ('_'? BINDIGIT)*
   | DIGIT ('_'? DIGIT)* ('.' DIGIT ('_'? DIGIT)*)? ([eE] [+-]? DIGIT+)?
   ;

STRING
   : '"' (ESC | ~["\\])* '"'
   | '\'' (ESC | ~['\\])* '\''
   ;

fragment ESC
   : '\\' .
   ;

fragment HEXDIGIT
   : [0-9a-fA-F]
   ;

fragment OCTDIGIT
   : [0-7]
   ;

fragment BINDIGIT
   : [01]
   ;

fragment DIGIT
   : [0-9]
   ;

SHEBANG
   : '#!' ~[\r\n]* -> skip
   ;

LINE_COMMENT
   : '//' ~[\r\n]* -> skip
   ;

BLOCK_COMMENT
   : '/*' .*? '*/' -> skip
   ;

NEWLINE
   : '\r'? '\n'
   ;

WS
   : [ \t] + -> skip
   ;
