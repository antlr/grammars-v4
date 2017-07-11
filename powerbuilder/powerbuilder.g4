
grammar powerbuilder;

start_rule
   : header_rule body_rule* EOF
   ;

// NOTE: this rule can match an empty string
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

// Header fields
export_header
   : (swallow_to_newline | EXPORT_HEADER | PBSELECT) (NEWLINE | EOF)
   ;

release_information
   : 'RELEASE' NUMBER SEMI
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
   : (('NULL' | numeric_atom | DQUOTED_STRING | DATE | TIME) NEWLINE? COMMA?)
   | (attribute_name eq = '=' (attribute_value array_decl_sub? | LPAREN window_property_attributes_sub RPAREN)) NEWLINE? COMMA?
   ;

attribute_name
   : (identifier_name | 'TYPE' | 'UPDATE') NUMBER? (DOT (identifier_name | 'CASE' | 'TYPE' | 'ON' | 'DYNAMIC'))*
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
   | 'TYPE'
   | 'TO'
   | 'FROM'
   | 'REF'
   | 'NULL'
   | 'OPEN'
   | LPAREN LPAREN (expression | data_type_sub) (COMMA (expression | data_type_sub))? RPAREN (COMMA LPAREN (expression | data_type_sub) (COMMA (expression | data_type_sub))? RPAREN)* RPAREN
   | data_type_sub (LPAREN NUMBER RPAREN)?
   ;

// Forward declaration
forward_decl
   : 'FORWARD' (datatype_decl | variable_decl) + 'END' 'FORWARD'
   ;

// Type declaration
datatype_decl
   : scope_modif? 'TYPE' identifier_name 'FROM' (identifier_name '`')? data_type_name ('WITHIN' identifier_name)? ('AUTOINSTANTIATE')? ('DESCRIPTOR' DQUOTED_STRING '=' DQUOTED_STRING)? (variable_decl | event_forward_decl)* 'END' 'TYPE'
   ;

type_variables_decl
   : 'TYPE' 'VARIABLES' (access_modif | variable_decl | constant_decl)* 'END' 'VARIABLES'
   ;

global_variables_decl
   : ('GLOBAL' | 'SHARED') 'VARIABLES' (variable_decl | constant_decl)* 'END' 'VARIABLES'
   ;

// Variable declaration
variable_decl_sub
   : ('INDIRECT')? access_modif_part? scope_modif?
   ;

variable_decl
   : variable_decl_sub (SEMI)
   ;

decimal_decl_sub
   : '{' NUMBER '}'
   ;

array_decl_sub
   : '[]'
   | '[' (('+' | '-')? NUMBER ('TO' ('+' | '-')? NUMBER)? (COMMA ('+' | '-')? NUMBER ('TO' ('+' | '-')? NUMBER)?)*)? ']'
   ;

constant_decl_sub
   :
   // TODO '=' is mandatory for constants 'CONSTANT' variable_decl_sub
   ;

constant_decl
   : constant_decl_sub (SEMI)
   ;

function_forward_decl
   : access_modif_part? scope_modif? ('FUNCTION' data_type_name | 'SUBROUTINE') identifier_name LPAREN parameters_list_sub? RPAREN ('LIBRARY' (DQUOTED_STRING | QUOTED_STRING) ('ALIAS' 'FOR' (DQUOTED_STRING | QUOTED_STRING))?)? ('RPCFUNC' 'ALIAS' 'FOR' (DQUOTED_STRING | QUOTED_STRING))? ('THROWS' identifier_name)?
   ;

parameter_sub
   : ('READONLY')? ('REF')? data_type_name decimal_decl_sub? identifier_name array_decl_sub?
   ;

parameters_list_sub
   : parameter_sub (COMMA parameter_sub)*
   ;

functions_forward_decl
   : ('FORWARD' | 'TYPE') 'PROTOTYPES' function_forward_decl + 'END' 'PROTOTYPES'
   ;

function_body
   : ('PUBLIC' | 'PRIVATE' | 'PROTECTED')? scope_modif? ('FUNCTION' data_type_name | 'SUBROUTINE') identifier_name LPAREN parameters_list_sub? RPAREN ('THROWS' identifier_name)? (SEMI statement)* 'END' ('FUNCTION' | 'SUBROUTINE')
   ;

on_body
   : 'ON' (identifier | 'OPEN' | 'CLOSE')
   ;

event_forward_decl_sub
   : 'EVENT' (identifier_name | 'CREATE' | 'DESTROY') identifier_name? (LPAREN parameters_list_sub? RPAREN)?
   | 'EVENT' 'TYPE' data_type_name identifier_name (LPAREN parameters_list_sub? RPAREN)
   ;

event_forward_decl
   : event_forward_decl_sub
   ;

event_body
   : 'EVENT' ('TYPE' data_type_name)? (identifier_name '::')? (identifier_name | 'OPEN' | 'CLOSE') (LPAREN parameters_list_sub? RPAREN)? (SEMI statement)* 'END' 'EVENT'
   ;

// Member access modifiers
access_modif
   : ('PUBLIC:' | 'PUBLIC' ':' | 'PRIVATE:' | 'PRIVATE' ':' | 'PROTECTED:' | 'PROTECTED' ':')
   ;

access_modif_part
   : 'PUBLIC'
   | 'PRIVATE'
   | 'PRIVATEREAD'
   | 'PRIVATEWRITE'
   | 'PROTECTED'
   | 'PROTECTEDREAD'
   | 'PROTECTEDWRITE'
   ;

scope_modif
   : 'GLOBAL'
   | 'LOCAL'
   ;

// value expressions
expression
   : (close_call_sub)
   | ('{')
   ;

expression_list
   : ('REF'? expression) (COMMA 'REF'? expression)*
   ;

// LOGICAL expression  
boolean_expression
   : condition_or
   ;

condition_or
   : condition_and ('OR' condition_and)*
   ;

condition_and
   : condition_not ('AND' condition_not)*
   ;

condition_not
   : ('NOT')? condition_comparison
   ;

// RELATIONAL
condition_comparison
   : add_expr (('=' | '>' | '<' | '<>' | '>=' | '<=') add_expr)?
   ;

// ARITHMETICAL
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

// Statements
statement
   : (if_simple_statement)
   | ('DESCRIBE' identifier_name)
   | (assignment_statement)
   | (statement_sub SEMI)
   | (if_statement)
   | ('TRY')
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
   : lvalue_sub ('=' | '+=' | '-=' | '*=' | '/=') (('NOT') | ('{') | (boolean_expression) | expression)
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
   : 'RETURN' expression?
   ;

return_statement
   : return_sub
   ;

function_call_expression_sub
   : (atom_sub (DOT identifier_name_ex))
   | atom_sub_call1
   ;

function_virtual_call_expression_sub
   : identifier_name DOT 'DYNAMIC' ('EVENT')? function_call_expression_sub
   | identifier_name DOT 'EVENT' 'DYNAMIC' function_call_expression_sub
   ;

open_call_sub
   : 'OPEN' LPAREN expression_list RPAREN
   ;

close_call_sub
   : 'CLOSE' LPAREN expression_list RPAREN
   | 'HALT' 'CLOSE'
   ;

function_call_statement
   : (function_call_expression_sub | function_virtual_call_expression_sub | open_call_sub | close_call_sub)
   ;

super_call_sub
   : 'CALL' (identifier_name '`')? ((atom_sub_call1) | atom_sub_member1)
   ;

super_call_statement
   : super_call_sub
   ;

event_call_statement_sub
   : ((identifier_name DOT (identifier_name DOT)?) | ('SUPER' '::'))? 'EVENT' atom_sub_call1
   ;

event_call_statement
   : event_call_statement_sub
   ;

create_call_sub
   : 'CREATE' ('USING')? (identifier_name DOT)? data_type_name (LPAREN expression_list? RPAREN)?
   ;

create_call_statement
   : create_call_sub
   ;

destroy_call_sub
   : 'DESTROY' expression
   ;

destroy_call_statement
   : destroy_call_sub
   ;

for_loop_statement
   : 'FOR' lvalue_sub '=' expression 'TO' expression ('STEP' expression)? statement 'NEXT'
   ;

do_while_loop_statement
   : 'DO' ('WHILE' | 'UNTIL') boolean_expression statement* 'LOOP'
   ;

do_loop_while_statement
   : 'DO' statement* 'LOOP' ('WHILE' | 'UNTIL') boolean_expression
   ;

if_statement
   : 'IF' boolean_expression 'THEN' statement* ('ELSEIF' boolean_expression 'THEN' statement*)* ('ELSE' statement*)? 'END' 'IF' (SEMI |)
   ;

if_simple_statement
   : 'IF' boolean_expression 'THEN' statement
   ;

continue_sub
   : 'CONTINUE'
   ;

continue_statement
   : continue_sub
   ;

// ldir.Post Event SelectionChanged(1)		
post_event_sub
   : (atom_sub_member1 DOT)? ('POST' | 'TRIGGER') ('EVENT')? identifier_name_ex LPAREN expression_list? RPAREN
   ;

post_event
   : post_event_sub
   ;

exit_statement_sub
   : 'EXIT'
   ;

exit_statement
   : exit_statement_sub
   ;

choose_statement
   : 'CHOOSE' 'CASE' expression ((choose_case_range_sub) | (choose_case_cond_sub) | (choose_case_else_sub) | choose_case_value_sub) + 'END' 'CHOOSE'
   ;

choose_case_value_sub
   : 'CASE' expression (COMMA expression)* statement*
   ;

choose_case_cond_sub
   : 'CASE' 'IS' ('=' | '>' | '<' | '<>' | '>=' | '<=') expression statement*
   ;

choose_case_range_sub
   : 'CASE' atom 'TO' atom statement*
   ;

choose_case_else_sub
   : 'CASE' 'ELSE' statement*
   ;

goto_stat_sub
   : 'GOTO' identifier_name
   ;

goto_stat
   : goto_stat_sub
   ;

label_stat
   : identifier_name COLON
   ;

try_catch_block
   : 'TRY' statement* ('CATCH' LPAREN variable_decl_sub RPAREN statement*)* ('FINALLY' statement*)? 'END' 'TRY'
   ;

throw_stat_sub
   : 'THROW' expression
   ;

throw_stat
   : throw_stat_sub
   ;

identifier
   : identifier_name
   | 'SUPER' '::' ('CREATE' | 'DESTROY' | identifier_name_ex)
   | identifier_name '::' ('CREATE' | 'DESTROY')
   | identifier_name DOT ('CREATE' | 'DESTROY')
   | identifier_name '::' identifier_name_ex
   ;

identifier_name
   : ID
   ;

// this one can be used in expressions like:
// excel_object.Application.Sheets("Sheet1").Select()
// identifier_name_ex is never the first part in the identifier (except for 'DESCRIBE').
identifier_name_ex
   : identifier_name
   | 'SELECT'
   | 'TYPE'
   | 'UPDATE'
   | 'DELETE'
   | 'OPEN'
   | 'CLOSE'
   | 'GOTO'
   | 'INSERT'
   | 'DESCRIBE'
   | 'TIME'
   | 'READONLY'
   ;

atom_sub
   : (((array_access_atom) | (identifier_name LPAREN expression_list? RPAREN) | identifier_name))
   ;

atom_sub_call1
   : (identifier | 'DESCRIBE') LPAREN expression_list? RPAREN
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
   : 'TRUE'
   | 'FALSE'
   ;

cast_expression
   : data_type_sub LPAREN expression (COMMA expression)* RPAREN
   ;

data_type_sub
   : 'ANY'
   | 'BLOB'
   | 'BOOLEAN'
   | 'BYTE'
   | 'CHARACTER'
   | 'CHAR'
   | 'DATE'
   | 'DATETIME'
   | 'DECIMAL'
   | 'DEC'
   | 'DOUBLE'
   | 'INTEGER'
   | 'INT'
   | 'LONG'
   | 'LONGLONG'
   | 'REAL'
   | 'STRING'
   | 'TIME'
   | 'UNSIGNEDINTEGER'
   | 'UINT'
   | 'UNSIGNEDLONG'
   | 'ULONG'
   | 'WINDOW'
   ;

data_type_name
   : data_type_sub
   | identifier_name
   ;


DQUOTED_STRING
   : '"' (E_TILDE | ~ ('"') | E_DOUBLE_QUOTE)* '"'
   ;


QUOTED_STRING
   : '\'' (~ ('\'') | E_QUOTE)* '\''
   ;


fragment ID_PARTS
   : 'A' .. 'Z' ('A' .. 'Z' | DIGIT | '-' | '$' | '#' | '%' | '_')*
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


SL_COMMENT
   : '//' ~ ('\n' | '\r')*
   ;


ML_COMMENT
   : '/*' (.)* '*/' -> skip
   ;


WS
   : (' ' | '\t') -> skip
   ;

// PB specific tokens

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


NEWLINE
   : '\r\n'
   ;


LINE_CONTINUATION
   : '&' WS* '\r\n' -> skip
   ;


EXPORT_HEADER
   : '$' 'A' .. 'Z' (('A' .. 'Z' | DIGIT | '-' | '#' | '%' | '_'))* '$' ~ ('\n' | '\r')*
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
