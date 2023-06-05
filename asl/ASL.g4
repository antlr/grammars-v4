/*
Part of CEDAR: the Common Event Driven Application Runtime project.
Copyright 2021 Ultra Electronics Limited, trading as Precision Control Systems (dpmai@ultra-pcs.com)
SPDX-License-Identifier: GPL-3.0-or-later
*/

grammar ASL;

/*
 * Parser Rules
 */

asl 				        : statement+ EOF ;
statement                   : (simple_statement | sequential_logic_statement | native_language_section) NEWLINE+ ;
simple_statement            : assignment_statement
                            | create_statement
                            | delete_statement
                            | find_statement
                            | relationship_navigation
                            | associative_relationship_navigation
                            | relationship_link
                            | relationship_unlink
                            | event_generation
                            | operation_invocation
                            | timer_operation
                            | set_operation
                            | struct_statement ;
sequential_logic_statement  : switch_statement
                            | if_statement
                            | for_loop
                            | loop_statement ;

assignment_statement        : constant_assignment | date_time_assignment | variable_assignment | object_attribute_assignment
                            | arithmetic_assignment ;
constant_assignment         : (variable | object_attribute) '=' (constant | UNDEFINED) ;
date_time_assignment        : (variable | object_attribute) '=' (CURRENT_DATE | CURRENT_TIME) ;
variable_assignment         : (variable | object_attribute) '=' (variable | THIS) ;
object_attribute_assignment : (variable | object_attribute) '=' object_attribute ;
arithmetic_assignment       : (variable | object_attribute) '=' arithmetic_expression ;
arithmetic_expression       : arithmetic_component ARITHMETIC_OPERATOR (arithmetic_component | arithmetic_expression)
                            | '(' arithmetic_component ARITHMETIC_OPERATOR (arithmetic_component | arithmetic_expression) ')' 
                            | countof_function ;
arithmetic_component        : variable | object_attribute | constant | countof_function ;
countof_function            : COUNTOF (set_variable | (variable | THIS) '->' relationship_spec) ;

create_statement            : (variable | THIS) '=' CREATE UNIQUE? object_name (WITH attribute_assignments)? ;
attribute_assignments       : attribute_assignment ('&' attribute_assignment)* ;
attribute_assignment        : attribute '=' (constant | variable | object_attribute | arithmetic_expression | CURRENT_DATE | CURRENT_TIME | UNDEFINED);
delete_statement            : DELETE (variable | THIS) ;

find_statement              : find_instance_statement | find_set_statement ;
find_instance_statement     : variable '=' (FIND_ONE | FIND_ONLY) (object_name | set_variable) (WHERE object_condition)? ; 
find_set_statement          : set_variable '=' 
                            ((FIND object_name | FIND? set_variable) WHERE object_condition 
                            | FIND_ALL object_name)
                            ((ORDERED_BY | REVERSE_ORDERED_BY) attribute)? ;
object_condition            : attribute logical_operator (variable | object_attribute | constant | UNDEFINED) (('&' | '|') object_condition)* 
                            | '(' attribute logical_operator (variable | constant | UNDEFINED) ')' (('&' | '|') object_condition)* ;
equality_operator           : EQ | NEQ | EQUALS | NOT_EQUALS ;
logical_operator            : equality_operator | LT | GT | LTE | GTE | LESS_THAN | GREATER_THAN ;
relationship_navigation     : (variable | set_variable) '=' (variable | set_variable | THIS) ('->' relationship_spec)+ (WHERE object_condition)? 
                            ((ORDERED_BY | REVERSE_ORDERED_BY) attribute)? ;
associative_relationship_navigation : variable '=' (variable | THIS) AND (variable | THIS) '->' (qualified_relationship | relationship_role) ;
relationship_link           : LINK (variable | THIS) relationship_spec variable (USING variable)?
                            | LINK variable relationship_spec THIS (USING variable)? ;
relationship_unlink         : UNLINK (variable | THIS) relationship_spec variable
                            | UNLINK variable relationship_spec THIS ;
relationship_spec           : R_NUMBER | qualified_relationship | relationship_role ;
qualified_relationship      : R_NUMBER DOT object_name ;
relationship_role           : R_NUMBER DOT LOWERCASE_TEXT ;

event_generation            : internal_event_generation | external_event_generation ;
internal_event_generation   : GENERATE event_specification '(' event_parameters? ')' ';' (TO (variable | THIS))? ;
external_event_generation   : GENERATE external_event_specification '(' event_parameters? ')' ';' ;
event_specification         : UPPERCASE_NUMBER_ID ':' LEADING_UPPERCASE_ID ;
external_event_specification : UPPERCASE_NUMBER_ID ':' LEADING_UPPERCASE_ID ;
event_parameters            : event_parameter (',' event_parameter)* ;
event_parameter             : variable | constant | THIS | object_attribute | structure_field | UNDEFINED ;

operation_invocation        : object_operation | domain_operation | bridge_operation ;
object_operation            : '[' output_parameter_list? ']' '=' oo_specification '[' input_parameter_list? ']' ;
domain_operation            : '[' output_parameter_list? ']' '=' do_specification '[' input_parameter_list? ']' ;
bridge_operation            : '[' output_parameter_list? ']' '=' bo_specification '[' input_parameter_list? ']' ;
oo_specification            : UPPERCASE_NUMBER_ID ':' LEADING_UPPERCASE_ID ;
do_specification            : UPPERCASE_NUMBER_ID ':' LEADING_UPPERCASE_ID ;
bo_specification            : UPPERCASE_NUMBER_ID ':' LEADING_UPPERCASE_ID ;
output_parameter_list       : output_parameter (',' output_parameter)* ;
output_parameter            : variable | set_variable | object_attribute ;
input_parameter_list        : input_parameter (',' input_parameter)* ;
input_parameter             : variable | set_variable | constant | object_attribute ;

timer_operation             : timer_creation | timer_deletion | set_relative_timer | set_absolute_timer | set_recurring_timer | timer_reset ;
timer_creation              : '[' timer_id ']' '=' CREATE_TIMER '[' ']' ;
timer_deletion              : '[' ']' '=' DELETE_TIMER '[' timer_id ']' ;
set_relative_timer          : GENERATE TIM1 '(' time_to_fire ',' return_event ',' target_instance ')' ';' TO timer_id ;
set_absolute_timer          : GENERATE TIM10 '(' fire_year ',' fire_month ',' fire_date ',' fire_hour ',' fire_minute ',' fire_second
                                ',' return_event ',' target_instance ')' ';' TO timer_id ;
set_recurring_timer         : GENERATE TIM3 '(' start_time ',' period ',' return_event ',' target_instance ')' ';' TO timer_id ; 
timer_reset                 : GENERATE TIM2 '(' ')' ';' TO timer_id ;
timer_id                    : variable | object_attribute ;
time_to_fire                : variable | object_attribute | INTEGER_VALUE ;
return_event                : UPPERCASE_NUMBER_ID ;
target_instance             : variable | THIS ;
fire_year                   : variable | object_attribute | INTEGER_VALUE ;
fire_month                  : variable | object_attribute | INTEGER_VALUE ;
fire_date                   : variable | object_attribute | INTEGER_VALUE ;
fire_hour                   : variable | object_attribute | INTEGER_VALUE ;
fire_minute                 : variable | object_attribute | INTEGER_VALUE ;
fire_second                 : variable | object_attribute | INTEGER_VALUE ;
start_time                  : variable | object_attribute | INTEGER_VALUE ;
period                      : variable | object_attribute | INTEGER_VALUE ;

set_operation               : unique_set | set_combination | set_difference ;
unique_set                  : set_variable '=' UNIQUE set_variable ;
set_combination             : set_variable '=' (UNION_OF | DISUNION_OF | INTERSECTION_OF) set_variable AND set_variable ;
set_difference              : set_variable '=' set_variable NOT_IN set_variable ;

struct_statement            : struct_definition | struct_instantiation | struct_assembly | struct_for_loop 
                            | order_struct | struct_subset ;
struct_definition           : DEFINE STRUCTURE struct_type NEWLINE
                              (member_name member_type NEWLINE)+
                              ENDDEFINE ;
member_name                 : LOWERCASE_ID | LOWERCASE_NUMBER_ID ;
member_type                 : INTEGER | REAL | BOOLEAN | TEXT | DATE | TIME | struct_type | user_defined_type ;
struct_instantiation        : struct_ IS struct_type ;
struct_assembly             : APPEND '[' value_list ']' TO struct_ ((ORDERED_BY | REVERSE_ORDERED_BY) member_name)? ;
struct_for_loop             : FOR '[' variable_list ']' IN struct_ DO NEWLINE
                              statement*
                              (break_statement statement*)?
                              ENDFOR ;
order_struct                : struct_ '=' struct_ (ORDERED_BY | REVERSE_ORDERED_BY) member_name ;
struct_subset               : struct_ '=' struct_ WHERE struct_condition ;
struct_condition            : member_name logical_operator component ( (AND | OR | '&' | '|') struct_condition)* ;

struct_                     : '{' (LOWERCASE_ID | LOWERCASE_NUMBER_ID) '}' ;
struct_type                 : LOWERCASE_ID | LOWERCASE_NUMBER_ID | LEADING_FIRST_UPPERCASE_ID '.' LOWERCASE_ID ;
value_list                  : struct_value (',' struct_value)* ;
struct_value                : variable | set_variable | constant | object_attribute | struct_ ;
variable_list               : (variable | struct_) (',' (variable | struct_))* ;

switch_statement            : SWITCH (variable | object_attribute) NEWLINE
                              (CASE constant NEWLINE
                              statement*)+
                              (DEFAULT NEWLINE
                              statement+)?
                              ENDSWITCH ;

if_statement                : IF (logical_condition | L_PAREN logical_condition  R_PAREN) THEN NEWLINE
                              statement+
                              (ELSE NEWLINE
                              statement+)?
                              ENDIF ;
logical_condition           : ( '!' | NOT )? compound_logical_condition ( (AND | OR | '&' | '|') ( '!' | NOT )? compound_logical_condition)* ;
compound_logical_condition  : ( '!' | NOT )? simple_logical_condition ( (AND | OR | '&' | '|') ( '!' | NOT )? simple_logical_condition)*
                            | L_PAREN ( '!' | NOT )? simple_logical_condition ( (AND | OR | '&' | '|') ( '!' | NOT )? simple_logical_condition)* R_PAREN ;
simple_logical_condition    : L_PAREN? (variable
                               | component logical_operator component
                               | component equality_operator UNDEFINED) R_PAREN? ;
component                   : variable | object_attribute | constant | COUNTOF set_variable | arithmetic_expression | THIS ;

for_loop                    : FOR variable IN set_variable DO NEWLINE
                              statement*
                              (break_statement statement*)?
                              ENDFOR ;

break_statement             : (BREAK | BREAKIF logical_condition) NEWLINE ;

loop_statement              : LOOP NEWLINE
                              statement*
                              break_statement
                              (break_statement | statement)*
                              ENDLOOP ;

native_language_section     : INLINE NEWLINE .*? END_INLINE ;

set_variable                : '{' variable '}' ;
variable                    : LOWERCASE_ID | LOWERCASE_NUMBER_ID ;
object_name                 : UPPERCASE_ID ;

/*
Attribute names with a leading uppercase letter for the first word only are
allowed but for consistency with event names and operation names, this should
be changed to enforce a leading uppercase letter for all words in the future.
*/
attribute                   : LEADING_FIRST_UPPERCASE_ID | LEADING_UPPERCASE_ID | UPPERCASE_ID | INSTANCE_ID ;
object_attribute            : (variable | THIS) '.' attribute ;
constant                    : INTEGER_VALUE | REAL_VALUE | BOOLEAN_VALUE | text_string | enum_value | DATE_VALUE | TIME_VALUE ;
text_string                 : LOWERCASE_TEXT | OTHER_TEXT | QUOTE QUOTE ;
enum_value                  : UPPERCASE_ID | UPPERCASE_OR_NUMBER_ID;
structure_field             : variable '.' LOWERCASE_ID ;
user_defined_type           : UPPERCASE_ID '.' LEADING_FIRST_UPPERCASE_ID ;

/*
 * Lexer Rules
 */

fragment LOWERCASE          : [a-z] ;
fragment UPPERCASE          : [A-Z] ;
fragment DIGIT              : [0-9] ;

THIS                        : 'this' ;
CREATE                      : 'create' ;
UNIQUE                      : 'unique' ;
WITH                        : 'with' ;
DELETE                      : 'delete' ;
UNDEFINED                   : 'UNDEFINED' ;
FIND_ONLY                   : 'find-only' ;
FIND_ONE                    : 'find-one' ;
FIND_ALL                    : 'find-all' ;
FIND                        : 'find' ;
WHERE                       : 'where' ;
EQUALS                      : 'equals' ;
NOT_EQUALS                  : 'not-equals' ;
LESS_THAN                   : 'less-than' ;
GREATER_THAN                : 'greater-than' ;
REVERSE_ORDERED_BY          : 'reverse ordered by' ;
ORDERED_BY                  : 'ordered by' ;
COUNTOF                     : 'countof' ;
AND                         : 'and' ;
OR                          : 'or' ;
NOT                         : 'not' ;
LINK                        : 'link' ;
USING                       : 'using' ;
UNLINK                      : 'unlink' ;
RELATIONSHIP_TRAVERSAL      : '->' ;
GENERATE                    : '%generate' ;
TO                          : 'to' ;
CREATE_TIMER                : 'Create_Timer' ;
DELETE_TIMER                : 'Delete_Timer' ;
TIM10                       : 'TIM10:Set_Absolute_Timer' ;
TIM1                        : 'TIM1:Set_Timer' ;
TIM2                        : 'TIM2:Reset_Timer' ;
TIM3                        : 'TIM3:Set_Chimer' ; 
CURRENT_DATE                : 'current-date' ;
CURRENT_TIME                : 'current-time' ;
SWITCH                      : 'switch' ;
CASE                        : 'case' ;
DEFAULT                     : 'default' ;
ENDSWITCH                   : 'endswitch' ;
IF                          : 'if' ;
THEN                        : 'then' ;
ELSE                        : 'else' ;
ENDIF                       : 'endif' ;
FOR                         : 'for' ;
IN                          : 'in' ;
DO                          : 'do' ;
ENDFOR                      : 'endfor' ;
BREAK                       : 'break' ;
BREAKIF                     : 'breakif' ;
LOOP                        : 'loop' ;
ENDLOOP                     : 'endloop' ;
INLINE                      : '$INLINE' ;
END_INLINE                  : '$ENDINLINE' ;
DEFINE                      : 'define' ;
STRUCTURE                   : 'structure' ;
ENDDEFINE                   : 'enddefine' ;
IS                          : 'is' ;
APPEND                      : 'append' ;
INTEGER                     : 'Integer' ;
REAL                        : 'Real' ;
BOOLEAN                     : 'Boolean' ;
TEXT                        : 'Text' ;
DATE                        : 'Date' ;
TIME                        : 'Time_of_Day' ;

NEQ                         : '!=' ;
EQ                          : '=' ;
LTE                         : '<=' ;
GTE                         : '>=' ;
LT                          : '<' ;
GT                          : '>' ;
LOGICAL_AND                 : '&' ;
LOGICAL_OR                  : '|' ;
LOGICAL_NOT                 : '!' ;
L_PAREN                     : '(' ;
R_PAREN                     : ')' ;
L_CURLY                     : '{' ;
R_CURLY                     : '}' ;
L_SQUARE                    : '[' ;
R_SQUARE                    : ']' ;
UNDERSCORE                  : '_' ;
DOT                         : '.' ;
SEMI_COLON                  : ';' ;
COLON                       : ':' ;
COMMA                       : ',' ;
HASH                        : '#' ;
BACKSLASH                   : '\\' ; 
UNION_OF                    : 'union-of' ;
DISUNION_OF                 : 'disunion-of' ;
INTERSECTION_OF             : 'intersection-of' ;
NOT_IN                      : 'not-in' ;

DATE_VALUE                  : DIGIT DIGIT DIGIT DIGIT '.' DIGIT DIGIT '.' DIGIT DIGIT ;
TIME_VALUE                  : DIGIT DIGIT ':' DIGIT DIGIT ':' DIGIT DIGIT ;
INTEGER_VALUE               : '-'? DIGIT+ ;
REAL_VALUE                  : '-'? DIGIT+ '.' DIGIT+ ;
ARITHMETIC_OPERATOR         : '+' | '-' | '/' | '*' ;
BOOLEAN_VALUE               : 'TRUE' | 'FALSE' ;

LOWERCASE_TEXT              : '"' LOWERCASE+ (' ' LOWERCASE+)* '"' ;
OTHER_TEXT                  : '"' (~('"' | '\\' | '\r' | '\n') | '\\' ('\\' | '"' | 'r' | 'n' | 't'))* '"' ;
QUOTE                       : '"' ;
R_NUMBER                    : 'R' DIGIT+ ;
INSTANCE_ID                 : 'instance_id' ;
UPPERCASE_ID                : UPPERCASE+ ('_' UPPERCASE+)* ;
LOWERCASE_ID                : LOWERCASE+ ('_' LOWERCASE+)* ;
LEADING_UPPERCASE_ID        : UPPERCASE (UPPERCASE | LOWERCASE)* ('_' UPPERCASE (UPPERCASE | LOWERCASE)+)* ;
LEADING_FIRST_UPPERCASE_ID  : UPPERCASE (UPPERCASE | LOWERCASE)* ('_' (UPPERCASE | LOWERCASE)+)* ;
UPPERCASE_NUMBER_ID         : UPPERCASE+ ('_' UPPERCASE+)* DIGIT+;
LOWERCASE_NUMBER_ID         : LOWERCASE (LOWERCASE | DIGIT)* ('_' (LOWERCASE | DIGIT)+)* ;
UPPERCASE_OR_NUMBER_ID      : UPPERCASE (UPPERCASE | DIGIT)* ('_' (UPPERCASE | DIGIT)+)* ;

NATIVE_LANGUAGE             : INLINE NEWLINE .*? END_INLINE NEWLINE+ -> skip ;
CONTINUATION                : BACKSLASH NEWLINE -> skip ;
WHITESPACE			        : [ \t]+ -> skip ;
COMMENT                     : HASH .*? NEWLINE+ -> skip ;
NEWLINE				        : ('\r'? '\n' | '\r')+ ;
OTHER                       : . ;
