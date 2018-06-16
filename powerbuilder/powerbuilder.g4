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

grammar powerbuilder;

start_rule
   :
   /* header_rule? */ body_rule + EOF
   ;

header_rule
   : export_header* release_information? window_property_line*
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

export_header
   : (swallow_to_newline | EXPORT_HEADER | PBSELECT)
   ;

release_information
   : RELEASE NUMBER SEMI
   ;

window_property_line
   : window_property +
   ;

window_property
   : (attribute_name array_decl_sub? LPAREN window_property_attributes_sub? RPAREN)
   ;

window_property_attributes_sub
   : window_property_attribute_sub +
   ;

window_property_attribute_sub
   : ((NULL | numeric_atom | DQUOTED_STRING | DATE | TIME) NEWLINE? COMMA?)
   | (attribute_name eq = '=' (attribute_value array_decl_sub? | LPAREN window_property_attributes_sub RPAREN)) NEWLINE? COMMA?
   ;

attribute_name
   : (identifier_name | TYPE | UPDATE) NUMBER? (DOT (identifier_name | CASE | TYPE | ON | DYNAMIC))*
   ;

attribute_value
   : (atom_sub_call1)
   | (atom_sub_member1)
   | ('-')? numeric_atom
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
   | NULL
   | OPEN
   | LPAREN LPAREN (expression | data_type_sub) (COMMA (expression | data_type_sub))? RPAREN (COMMA LPAREN (expression | data_type_sub) (COMMA (expression | data_type_sub))? RPAREN)* RPAREN
   | data_type_sub (LPAREN NUMBER RPAREN)?
   ;

forward_decl
   : FORWARD (datatype_decl | variable_decl) + END FORWARD
   ;

datatype_decl
   : scope_modif? TYPE identifier_name FROM (identifier_name '`')? data_type_name (WITHIN identifier_name)? (AUTOINSTANTIATE)? ('DESCRIPTOR' DQUOTED_STRING '=' DQUOTED_STRING)? (variable_decl | event_forward_decl)* END TYPE
   ;

type_variables_decl
   : TYPE VARIABLES (access_modif | variable_decl | constant_decl)* END VARIABLES
   ;

global_variables_decl
   : (GLOBAL | SHARED) VARIABLES (variable_decl | constant_decl)* END VARIABLES
   ;

// Variable declaration
variable_decl_sub
   : (INDIRECT)? access_modif_part? scope_modif?
   ;

variable_decl
   : variable_decl_sub (SEMI)
   ;

decimal_decl_sub
   : '{' NUMBER '}'
   ;

array_decl_sub
   : '[]'
   | '[' (('+' | '-')? NUMBER (TO ('+' | '-')? NUMBER)? (COMMA ('+' | '-')? NUMBER (TO ('+' | '-')? NUMBER)?)*)? ']'
   ;

constant_decl_sub
   :
   // TODO '=' is mandatory for constants 'CONSTANT' variable_decl_sub
   ;

constant_decl
   : constant_decl_sub (SEMI)
   ;

function_forward_decl
   : access_modif_part? scope_modif? (FUNCTION data_type_name | SUBROUTINE) identifier_name LPAREN parameters_list_sub? RPAREN ('LIBRARY' (DQUOTED_STRING | QUOTED_STRING) ('ALIAS' FOR (DQUOTED_STRING | QUOTED_STRING))?)? ('RPCFUNC' 'ALIAS' FOR (DQUOTED_STRING | QUOTED_STRING))? ('THROWS' identifier_name)?
   ;

parameter_sub
   : (READONLY)? (REF)? data_type_name decimal_decl_sub? identifier_name array_decl_sub?
   ;

parameters_list_sub
   : parameter_sub (COMMA parameter_sub)*
   ;

functions_forward_decl
   : (FORWARD | TYPE) PROTOTYPES function_forward_decl + END PROTOTYPES
   ;

function_body
   : (PUBLIC | PRIVATE | PROTECTED)? scope_modif? (FUNCTION data_type_name | SUBROUTINE) identifier_name LPAREN parameters_list_sub? RPAREN ('THROWS' identifier_name)? (SEMI statement)* END (FUNCTION | SUBROUTINE)
   ;

on_body
   : ON (identifier | OPEN | CLOSE)
   ;

event_forward_decl_sub
   : EVENT (identifier_name | CREATE | DESTROY) identifier_name? (LPAREN parameters_list_sub? RPAREN)?
   | EVENT TYPE data_type_name identifier_name (LPAREN parameters_list_sub? RPAREN)
   ;

event_forward_decl
   : event_forward_decl_sub
   ;

event_body
   : EVENT (TYPE data_type_name)? (identifier_name '::')? (identifier_name | OPEN | CLOSE) (LPAREN parameters_list_sub? RPAREN)? (SEMI statement)* END EVENT
   ;

access_modif
   : (PUBLIC | PRIVATE | PROTECTED) ':'
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
   : (close_call_sub)
   | ('{')
   ;

expression_list
   : (REF? expression) (COMMA REF? expression)*
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
   : (NOT)? condition_comparison
   ;


condition_comparison
   : add_expr (('=' | '>' | '<' | '<>' | '>=' | '<=') add_expr)?
   ;

add_expr
   : mul_expr (('-' | '+') mul_expr)*
   ;

mul_expr
   : unary_sign_expr (('*' | '/' | '^') unary_sign_expr)*
   ;

unary_sign_expr
   : (LPAREN expression RPAREN)
   | ('-' | '+')? atom
   ;


statement
   : (if_simple_statement)
   | (DESCRIBE identifier_name)
   | (assignment_statement)
   | (statement_sub SEMI)
   | (if_statement)
   | (TRY)
   | (post_event)
   | (function_call_statement)
   | (event_call_statement)
   | (constant_decl)
   | (variable_decl)
   | (super_call_statement)
   | (do_loop_while_statement)
   | (do_while_loop_statement)
   | (create_call_statement)
   | (destroy_call_statement)
   | (label_stat)
   | (identifier)
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
   : (function_virtual_call_expression_sub)
   | (function_call_expression_sub)
   | (return_sub)
   | (open_call_sub)
   | (close_call_sub)
   | (variable_decl_sub)
   | (super_call_sub)
   | (create_call_sub)
   | (destroy_call_sub)
   | (continue_sub)
   | (goto_stat_sub)
   | (assignment_sub)
   ;

assignment_sub
   : lvalue_sub ('=' | '+=' | '-=' | '*=' | '/=') ((NOT) | ('{') | (boolean_expression) | expression)
   ;

assignment_statement
   : assignment_sub SEMI?
   ;

lvalue_sub
   : (atom_sub (DOT identifier_name_ex))
   | (atom_sub_call1)
   | (atom_sub_array1)
   | (atom_sub_ref1)
   | (atom_sub_member1)
   ;

return_sub
   : RETURN expression?
   ;

return_statement
   : return_sub
   ;

function_call_expression_sub
   : (atom_sub (DOT identifier_name_ex))
   | atom_sub_call1
   ;

function_virtual_call_expression_sub
   : identifier_name DOT DYNAMIC (EVENT)? function_call_expression_sub
   | identifier_name DOT EVENT DYNAMIC function_call_expression_sub
   ;

open_call_sub
   : OPEN LPAREN expression_list RPAREN
   ;

close_call_sub
   : CLOSE LPAREN expression_list RPAREN
   | HALT CLOSE
   ;

function_call_statement
   : (function_call_expression_sub | function_virtual_call_expression_sub | open_call_sub | close_call_sub)
   ;

super_call_sub
   : CALL (identifier_name '`')? ((atom_sub_call1) | atom_sub_member1)
   ;

super_call_statement
   : super_call_sub
   ;

event_call_statement_sub
   : ((identifier_name DOT (identifier_name DOT)?) | (SUPER '::'))? EVENT atom_sub_call1
   ;

event_call_statement
   : event_call_statement_sub
   ;

create_call_sub
   : CREATE (USING)? (identifier_name DOT)? data_type_name (LPAREN expression_list? RPAREN)?
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
   : FOR lvalue_sub '=' expression TO expression (STEP expression)? statement NEXT
   ;

do_while_loop_statement
   : DO (WHILE | UNTIL) boolean_expression statement* LOOP
   ;

do_loop_while_statement
   : DO statement* LOOP (WHILE | UNTIL) boolean_expression
   ;

if_statement
   : IF boolean_expression THEN statement* (ELSEIF boolean_expression THEN statement*)* (ELSE statement*)? END IF (SEMI |)
   ;

if_simple_statement
   : IF boolean_expression THEN statement
   ;

continue_sub
   : CONTINUE
   ;

continue_statement
   : continue_sub
   ;

post_event_sub
   : (atom_sub_member1 DOT)? (POST | TRIGGER) (EVENT)? identifier_name_ex LPAREN expression_list? RPAREN
   ;

post_event
   : post_event_sub
   ;

exit_statement_sub
   : EXIT
   ;

exit_statement
   : exit_statement_sub
   ;

choose_statement
   : CHOOSE CASE expression ((choose_case_range_sub) | (choose_case_cond_sub) | (choose_case_else_sub) | choose_case_value_sub) + END CHOOSE
   ;

choose_case_value_sub
   : CASE expression (COMMA expression)* statement*
   ;

choose_case_cond_sub
   : CASE IS ('=' | '>' | '<' | '<>' | '>=' | '<=') expression statement*
   ;

choose_case_range_sub
   : CASE atom TO atom statement*
   ;

choose_case_else_sub
   : CASE ELSE statement*
   ;

goto_stat_sub
   : GOTO identifier_name
   ;

goto_stat
   : goto_stat_sub
   ;

label_stat
   : identifier_name COLON
   ;

try_catch_block
   : TRY statement* (CATCH LPAREN variable_decl_sub RPAREN statement*)* (FINALLY statement*)? END TRY
   ;

throw_stat_sub
   : THROW expression
   ;

throw_stat
   : throw_stat_sub
   ;

identifier
   : identifier_name
   | SUPER '::' (CREATE | DESTROY | identifier_name_ex)
   | identifier_name '::' (CREATE | DESTROY)
   | identifier_name DOT (CREATE | DESTROY)
   | identifier_name '::' identifier_name_ex
   ;

identifier_name
   : ID
   ;

// this one can be used in expressions like:
// excel_object.Application.Sheets("Sheet1").Select()
// identifier_name_ex is never the first part in the identifier (except for DESCRIBE).
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
   | TIME2
   | READONLY
   ;

atom_sub
   : (((array_access_atom) | (identifier_name LPAREN expression_list? RPAREN) | identifier_name))
   ;

atom_sub_call1
   : (identifier | DESCRIBE) LPAREN expression_list? RPAREN
   ;

atom_sub_array1
   : identifier_name '[' expression_list ']'
   ;

atom_sub_ref1
   : identifier_name '[]'
   ;

atom_sub_member1
   : identifier
   ;

atom
   : (event_call_statement_sub)
   | (atom_sub (DOT identifier_name_ex))
   | (cast_expression)
   | (atom_sub_call1)
   | (atom_sub_array1)
   | (atom_sub_ref1)
   | (atom_sub_member1)
   | numeric_atom
   | boolean_atom
   | ENUM
   | DQUOTED_STRING
   | QUOTED_STRING
   | DATE
   | TIME
   ;

swallow_to_semi
   : ~ (SEMI) +
   ;

swallow_to_newline
   : ~ (NEWLINE) +
   ;

array_access_atom
   : identifier_name '[' expression_list ']'
   ;

numeric_atom
   : NUMBER
   ;

boolean_atom
   : BOOLEAN_ATOM
   ;


BOOLEAN_ATOM
   : (T R U E) | (F A L S E)
   ;

cast_expression
   : data_type_sub LPAREN expression (COMMA expression)* RPAREN
   ;

data_type_sub
   : DATA_TYPE_SUB
   ;


DATA_TYPE_SUB
   : (A N Y) | (B L O B) | (B O O L E A N) | (B Y T E) | (C H A R A C T E R) | (C H A R) | (D A T E) | (D A T E T I M E) | (D E C I M A L) | (D E C) | (D O U B L E) | (I N T E G E R) | (I N T) | (L O N G) | (L O N G L O N G) | (R E A L) | (S T R I N G) | (T I M E) | (U N S I G N E D I N T E G E R) | (U I N T) | (U N S I G N E D L O N G) | (U L O N G) | (W I N D O W)
   ;

data_type_name
   : data_type_sub
   | identifier_name
   ;


GLOBAL
   : G L O B A L
   ;


SHARED
   : S H A R E D
   ;


END
   : E N D
   ;


INDIRECT
   : I N D I R E C T
   ;


VARIABLES
   : V A R I A B L E S
   ;


FORWARD
   : F O R W A R D
   ;


PUBLIC
   : P U B L I C
   ;


PRIVATE
   : P R I V A T E
   ;


FUNCTION
   : F U N C T I O N
   ;


SUBROUTINE
   : S U B R O U T I N E
   ;


READONLY
   : R E A D O N L Y
   ;


PROTOTYPES
   : P R O T O T Y P E S
   ;


TYPE
   : T Y P E
   ;


ON
   : O N
   ;


TO
   : T O
   ;


FROM
   : F R O M
   ;


REF
   : R E F
   ;


NULL
   : N U L L
   ;


UPDATE
   : U P D A T E
   ;


CASE
   : C A S E
   ;


DYNAMIC
   : D Y N A M I C
   ;


WITHIN
   : W I T H I N
   ;


PRIVATEWRITE
   : P R I V A T E W R I T E
   ;


PROTECTED
   : P R O T E C T E D
   ;


PRIVATEREAD
   : P R I V A T E R E A D
   ;


PROTECTEDREAD
   : P R O T E C T E D R E A D
   ;


PROTECTEDWRITE
   : P R O T E C T E D W R I T E
   ;


LOCAL
   : L O C A L
   ;


EVENT
   : E V E N T
   ;


OPEN
   : O P E N
   ;


GOTO
   : G O T O
   ;


ELSE
   : E L S E
   ;


IF
   : I F
   ;


THEN
   : T H E N
   ;


ELSEIF
   : E L S E I F
   ;


TRY
   : T R Y
   ;


EXIT
   : E X I T
   ;


CHOOSE
   : C H O O S E
   ;


IS
   : I S
   ;


CONTINUE
   : C O N T I N U E
   ;


DO
   : D O
   ;


WHILE
   : W H I L E
   ;


FOR
   : F O R
   ;


CLOSE
   : C L O S E
   ;


NEXT
   : N E X T
   ;


LOOP
   : L O O P
   ;


UNTIL
   : U N T I L
   ;


STEP
   : S T E P
   ;


CATCH
   : C A T C H
   ;


FINALLY
   : F I N A L L Y
   ;


THROW
   : T H R O W
   ;


RELEASE
   : R E L E A S E
   ;


CREATE
   : C R E A T E
   ;


DESTROY
   : D E S T R O Y
   ;


USING
   : U S I N G
   ;


POST
   : P O S T
   ;


TRIGGER
   : T R I G G E R
   ;


SELECT
   : S E L E C T
   ;


DELETE
   : D E L E T E
   ;


INSERT
   : I N S E R T
   ;


TIME2
   : T I M E
   ;


DESCRIBE
   : D E S C R I B E
   ;


RETURN
   : R E T U R N
   ;


OR
   : O R
   ;


AND
   : A N D
   ;


NOT
   : N O T
   ;


CALL
   : C A L L
   ;


HALT
   : H A L T
   ;


SUPER
   : S U P E R
   ;


AUTOINSTANTIATE
   : A U T O I N S T A N T I A T E
   ;


DESCRIPTOR
   : D E S C R I P T O R
   ;


DQUOTED_STRING
   : '"' (E_TILDE | ~ ('"') | E_DOUBLE_QUOTE)* '"'
   ;


QUOTED_STRING
   : '\'' (~ ('\'') | E_QUOTE)* '\''
   ;


fragment ID_PARTS
   : [a-zA-Z] ([a-zA-Z] | DIGIT | '-' | '$' | '#' | '%' | '_')*
   ;


ENUM
   : ID_PARTS '!'
   ;


COMMA
   : ','
   ;


ID
   : ID_PARTS
   ;


SEMI
   : ';'
   ;


LPAREN
   : '('
   ;


RPAREN
   : ')'
   ;


COLON
   : ':'
   ;


NUMBER
   : ((NUM POINT NUM) | POINT NUM | NUM) ('E' ('+' | '-')? NUM)? ('D' | 'F')?
   ;


fragment NUM
   : DIGIT (DIGIT)*
   ;


DOT
   : POINT
   ;


fragment POINT
   : '.'
   ;


DQUOTE
   : '"'
   ;


fragment TAB
   : '~t'
   ;


fragment CR
   : '~r'
   ;


fragment LF
   : '~n'
   ;


fragment E_DOUBLE_QUOTE
   : '~"'
   ;


fragment E_QUOTE
   : '~\''
   ;


fragment E_TILDE
   : '~~'
   ;

//TILDE : '~';

fragment DIGIT
   : '0' .. '9'
   ;


LINE_CONTINUATION
   : '&' WS* [\r\n] + -> skip
   ;


EXPORT_HEADER
   : '$' 'A' .. 'Z' (('A' .. 'Z' | DIGIT | '-' | '#' | '%' | '_'))* '$' ~ [\r\n] +
   ;


DATE
   : DIGIT DIGIT DIGIT DIGIT '-' DIGIT DIGIT '-' DIGIT DIGIT
   ;


TIME
   : DIGIT DIGIT ':' DIGIT DIGIT ':' DIGIT DIGIT (':' DIGIT DIGIT DIGIT DIGIT DIGIT DIGIT)?
   ;


BINDPAR
   : ':' ID_PARTS
   ;


TQ
   : '???'
   ;


DOUBLE_PIPE
   : '||'
   ;


ASTROOT
   : 'astroot'
   ;


HEADER
   : 'header'
   ;


BOODY
   : 'body'
   ;


DATATYPEDECL
   : 'datatypedecl'
   ;


FORWARDDECL
   : 'forwarddecl'
   ;


TYPEVARIABLESDECL
   : 'typevariablesdecl'
   ;


GLOBALVARIABLESDECL
   : 'globalvariablesdecl'
   ;


VARIABLEDECL
   : 'variabledecl'
   ;


CONSTANTDECL
   : 'constantdecl'
   ;


FUNCTIONFORWARDDECL
   : 'functionforwarddecl'
   ;


FUNCTIONSFORWARDDECL
   : 'functionsforwarddecl'
   ;


FUNCTIONBODY
   : 'functionbody'
   ;


ONBODY
   : 'onbody'
   ;


EVENTBODY
   : 'eventbody'
   ;


STATEMENT
   : 'statement'
   ;


SQLSTATEMENT
   : 'sqlstatement'
   ;


WINDOWPROP
   : 'windowprop'
   ;


WINDOWSUBPROP
   : 'windowsubprop'
   ;


WINDOWSUBPROPNAME
   : 'windowsubpropname'
   ;

// windowsubpropname :: "retreive" is what you're looking for

WINDOWSUBPROPVAL
   : 'windowsubpropval'
   ;


fragment A
   : ('a' | 'A')
   ;


fragment B
   : ('b' | 'B')
   ;


fragment C
   : ('c' | 'C')
   ;


fragment D
   : ('d' | 'D')
   ;


fragment E
   : ('e' | 'E')
   ;


fragment F
   : ('f' | 'F')
   ;


fragment G
   : ('g' | 'G')
   ;


fragment H
   : ('h' | 'H')
   ;


fragment I
   : ('i' | 'I')
   ;


fragment J
   : ('j' | 'J')
   ;


fragment K
   : ('k' | 'K')
   ;


fragment L
   : ('l' | 'L')
   ;


fragment M
   : ('m' | 'M')
   ;


fragment N
   : ('n' | 'N')
   ;


fragment O
   : ('o' | 'O')
   ;


fragment P
   : ('p' | 'P')
   ;


fragment Q
   : ('q' | 'Q')
   ;


fragment R
   : ('r' | 'R')
   ;


fragment S
   : ('s' | 'S')
   ;


fragment T
   : ('t' | 'T')
   ;


fragment U
   : ('u' | 'U')
   ;


fragment V
   : ('v' | 'V')
   ;


fragment W
   : ('w' | 'W')
   ;


fragment X
   : ('x' | 'X')
   ;


fragment Y
   : ('y' | 'Y')
   ;


fragment Z
   : ('z' | 'Z')
   ;


SL_COMMENT
   : '//' ~ ('\n' | '\r')* -> skip
   ;


ML_COMMENT
   : '/*' (.)* '*/' -> skip
   ;


NEWLINE
   : [\r\n] +
   ;


WS
   : (' ' | '\t') -> skip
   ;
