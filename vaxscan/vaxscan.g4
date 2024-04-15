/*
* VAXScan grammar.
*
* VaxScan (as the name implies) was a VAX/VMS compiler designed for text
* manipulation applications (for example, filtering out dates from a test
* output file to avoid spurious test failures.
* 
* Digital Equipment (DEC) having decided not to continue the product or port 
* it to their new hardware plaforms (ALPHA and Itanium) made the source
* code FREEWARE.
*
* This ANTLR4 grammar is based on the BNF provided in the source code.
*
* This software is provided "AS IS" with no warranty of any kind.
*
* William H. Cox (DEC 1979-1995)
*/

// $antlr-format alignTrailingComments true, columnLimit 150, minEmptyLines 1, maxEmptyLinesToKeep 1, reflowComments false, useTab false


// $antlr-format allowShortRulesOnASingleLine false, allowShortBlocksOnASingleLine true, alignSemicolons hanging, alignColons hanging

grammar vaxscan;


options { caseInsensitive = true; }
program
   : MODULE id opt_ident module_statement_list end_module ';' EOF
   ;

opt_ident
   : ';'
   | IDENT ident_string ';'
   ;

ident_string
   : CHARACTER_LITERAL
   ;

end_module
   : END MODULE
   ;

module_statement_list
   : module_statement ';'
   | module_statement_list module_statement ';'
   ;

module_statement
   : declaration
   | set_declaration
   | token_declaration
   | group_declaration
   | procedure_declaration
   | macro_declaration
   | directive
   | collate_directive
   | redefine_directive
   ;

body
   : statement_list?
   ;

statement_list
   : statement
   | statement_list statement
   ;

statement
   : executable ';'
   | declaration ';'
   | procedure_declaration ';'
   | macro_declaration ';'
   | directive ';'
   ;

directive
   : include_directive
   | list_directive
   ;

collate_directive
   : COLLATE collate_seq
   ;

collate_seq
   : NATIVE
   | ASCII
   | EBCDIC
   | DEC_MULTI
   | USER user_seq
   ;

user_seq
   : seq1
   | user_seq alt seq1
   ;

alt
   : '|'
   | '/'
   ;

seq1
   : seq2
   | seq1 seq2
   ;

seq2
   : id
   | string_literal
   | string_literal '..' string_literal
   | '{' user_seq '}'
   ;

redefine_directive
   : REDEFINE redefined_char '=' ctrl_char_literal
   ;

redefined_char
   : SCAN_LITERAL
   ;

include_directive
   : INCLUDE FILE exp
   ;

list_directive
   : LIST ON
   | LIST OFF
   | LIST PAGE
   | LIST TITLE exp
   ;

declaration
   : constant_declaration
   | variable_declaration
   | type_declaration
   | procedure_form_decl
   ;

constant_declaration
   : CONSTANT id constant_type
   ;

constant_type
   : '=' exp
   | EXTERNAL type_dec
   | GLOBAL '=' exp
   ;

set_declaration
   : SET id '(' set_exp ')'
   ;

set_exp
   : set_term
   | set_exp OR set_term
   ;

set_term
   : set_primary
   | set_term AND set_primary
   ;

set_primary
   : set_item
   | NOT set_item
   ;

set_item
   : id
   | string_literal
   | string_literal '..' string_literal
   | '(' set_exp ')'
   ;

string_literal
   : CHARACTER_LITERAL
   | ctrl_char_literal
   | SCAN_LITERAL
   ;

ctrl_char_literal
   : NUL
   | SOH
   | STX
   | ETX
   | EOT
   | ENQ
   | ACK
   | BEL
   | BS
   | HT
   | LF
   | VT
   | FF
   | CR
   | SO
   | SI
   | DLE
   | DC1
   | DC2
   | DC3
   | DC4
   | NAK
   | SYN
   | ETB
   | CAN
   | EM
   | SUB
   | ESC
   | FS
   | GS
   | RS
   | US
   | DEL
   | IND
   | NEL
   | SSA
   | ESA
   | HTS
   | HTJ
   | VTS
   | PID
   | PLU
   | RI
   | SS2
   | SS3
   | DCS
   | PU1
   | PU2
   | STS
   | CCH
   | MW
   | SPA
   | EPA
   | CSI
   | ST
   | OSC
   | PM
   | APC
   ;

token_declaration
   : TOKEN id token_attrs '{' token_exp '}'
   ;

token_attrs
   : token_attr_list?
   ;

token_attr_list
   : token_attr
   | token_attr_list token_attr
   ;

token_attr
   : CASELESS
   | IGNORE
   | ALIAS CHARACTER_LITERAL
   ;

token_exp
   : token_exp1
   | token_exp alt token_exp1
   ;

token_exp1
   : token_exp2
   | token_exp1 token_exp2
   ;

token_exp2
   : token_exp3
   | token_exp3 '...'
   ;

token_exp3
   : string_literal
   | id
   | '[' token_exp ']'
   | '{' token_exp '}'
   ;

group_declaration
   : GROUP id '(' group_exp ')'
   ;

group_exp
   : group_term
   | group_exp OR group_term
   ;

group_term
   : group_primary
   | group_term AND group_primary
   ;

group_primary
   : group_item
   | NOT group_item
   ;

group_item
   : id
   | token_alias
   | '(' group_exp ')'
   ;

token_alias
   : CHARACTER_LITERAL
   ;

macro_declaration
   : MACRO id macro_type opt_macro_attrs '{' opt_picture '}' ';' body END MACRO
   ;

macro_type
   : TRIGGER
   | SYNTAX
   ;

opt_macro_attrs
   : EXPOSE?
   ;

opt_picture
   : picture?
   ;

picture
   : pic1
   | picture alt pic1
   ;

pic1
   : pic2
   | pic2 '\\' pic2
   ;

pic2
   : pic3
   | pic2 pic3
   ;

pic3
   : pic4
   | pic4 '...'
   ;

pic4
   : macro_item
   | id ':' macro_item
   | id ',' name_list ':' macro_item
   | no_symbol ',' name_list ':' macro_item
   ;

no_symbol
   : '*'
   ;

macro_item
   : id
   | id '(' exp_list? ')'
   | token_alias
   | '[' picture ']'
   | '{' picture '}'
   ;

type_declaration
   : TYPE id ':' type_dec
   ;

variable_declaration
   : DECLARE name_list ':' opt_storage_attr? type_dec
   ;

name_list
   : pic_id
   | name_list ',' pic_id
   ;

pic_id
   : '*'
   | id
   ;

opt_storage_attr
   : STATIC
   | AUTOMATIC
   | COMMON
   | GLOBAL
   | EXTERNAL
   ;

common_type_dec
   : INTEGER
   | VARYING STRING length
   | STRING length
   | FIXED STRING length
   | BOOLEAN
   | FILL length
   | POINTER TO type_dec
   | TREEPTR '(' level_type ')' TO type_dec
   | record_keyword component_list END RECORD
   | overlay_keyword component_list END OVERLAY
   | user_defined_type
   ;

type_dec
   : common_type_dec
   | STRING
   | FILE
   | DYNAMIC STRING
   | TREE '(' level_types ')' OF type_dec
   ;

user_defined_type
   : NAME
   | NATIVE
   | ASCII
   | EBCDIC
   | DEC_MULTI
   | USER
   | TITLE
   | ON
   | OFF
   | PAGE
   | OF
   | EXPOSE
   | MAIN
   | CASELESS
   | ALIAS
   | SYNTAX
   | INPUT
   | OUTPUT
   | DATA
   | STACK
   | WIDTH
   ;

length
   : '(' int_ct_exp ')'
   ;

level_types
   : level_type
   | level_types ',' level_type
   ;

level_type
   : STRING
   | INTEGER
   ;

record_keyword
   : RECORD
   ;

overlay_keyword
   : OVERLAY
   ;

component_list
   : component
   | component_list component
   ;

component
   : id ':' common_type_dec ','
   ;

procedure_form_decl
   : proc_name opt_formal_list? opt_result?
   ;

proc_name
   : EXTERNAL PROCEDURE id
   | FORWARD PROCEDURE id
   ;

opt_formal_list
   : '(' formal_list? ')'
   ;

formal_list
   : formal
   | formal_list ',' formal
   ;

formal
   : opt_passing_mech? type_dec
   ;

opt_passing_mech
   : VALUE
   | REFERENCE
   | DESCRIPTOR
   ;

opt_result
   : OF type_dec
   ;

procedure_declaration
   : proc_start opt_parameter_list? opt_result? ';' body END PROCEDURE
   ;

proc_start
   : PROCEDURE id opt_proc_attr?
   ;

opt_proc_attr
   : MAIN
   ;

opt_parameter_list
   : '(' parameter_list? ')'
   ;

parameter_list
   : parameter
   | parameter_list ',' parameter
   ;

parameter
   : id ':' opt_passing_mech? type_dec
   ;

executable
   : keyword_statement
   | /* empty */
   
   | label ':' executable
   | variable_ref '=' exp
   ;

label
   : id
   ;

executable_list
   : executable ';'
   | directive ';'
   | executable_list executable ';'
   ;

keyword_statement
   : if_statement
   | for_statement
   | while_statement
   | call_statement
   | read_statement
   | write_statement
   | open_statement
   | close_statement
   | goto_statement
   | case_statement
   | return_statement
   | stop_scan_statement
   | answer_statement
   | fail_statement
   | start_scan_statement
   | prune_statement
   | allocate_statement
   | free_statement
   ;

if_statement
   : IF exp THEN executable_list? opt_else_statement END IF
   ;

opt_else_statement
   :
   | ELSE executable_list?
   ;

for_statement
   : start_for_loop ';' executable_list? END FOR
   ;

start_for_loop
   : FOR id '=' exp TO exp (STEP exp)?
   ;

while_statement
   : WHILE exp ';' executable_list? END WHILE
   ;

call_statement
   : CALL id argument_list?
   ;

argument_list
   : '(' arg_list? ')'
   ;

arg_list
   : arg
   | arg_list ',' arg
   ;

arg
   : exp
   | '*'
   ;

exp_list
   : exp
   | exp_list ',' exp
   ;

open_statement
   : OPEN file_ref AS exp open_type
   ;

file_ref
   : FILE '(' variable_ref ')'
   ;

close_statement
   : CLOSE file_ref
   ;

open_type
   : FOR OUTPUT
   | FOR INPUT
   ;

read_statement
   : READ file_ref? prompt? variable_ref
   ;

prompt
   : PROMPT '(' exp ')'
   ;

write_statement
   : WRITE file_ref? exp_list?
   ;

goto_statement
   : GOTO id
   ;

case_statement
   : start_case ';' case_list END CASE
   ;

start_case
   : CASE exp FROM int_ct_exp TO int_ct_exp
   ;

case_list
   : case_item
   | case_list case_item
   ;

case_item
   : '[' case_exp_list ']' ':' executable_list?
   ;

case_exp_list
   : case_exp
   | case_exp_list ',' case_exp
   ;

case_exp
   : INRANGE
   | OUTRANGE
   | int_ct_exp
   | int_ct_exp '..' int_ct_exp
   ;

return_statement
   : RETURN exp?
   ;

answer_statement
   : ANSWER trigger? exp_list
   ;

trigger
   : TRIGGER
   ;

fail_statement
   : FAIL
   ;

prune_statement
   : PRUNE prune_list
   ;

allocate_statement
   : ALLOCATE alloc_ptr_list
   ;

alloc_ptr_list
   : variable_ref
   | alloc_ptr_list ',' variable_ref
   ;

free_statement
   : FREE free_ptr_list
   ;

free_ptr_list
   : variable_ref
   | free_ptr_list ',' variable_ref
   ;

prune_list
   : exp
   | prune_list ',' exp
   ;

start_scan_statement
   : START SCAN scan_attr_list?
   ;

scan_attr_list
   : scan_attr
   | scan_attr_list scan_attr
   ;

scan_attr
   : INPUT FILE exp
   | INPUT STRING exp
   | INPUT PROCEDURE id
   | INPUT WIDTH exp
   | OUTPUT FILE exp
   | OUTPUT STRING variable_ref
   | OUTPUT PROCEDURE id
   | OUTPUT WIDTH exp
   | DATA STACK exp
   ;

stop_scan_statement
   : STOP SCAN
   ;

exp
   : exp1
   | exp OR exp1
   | exp XOR exp1
   ;

exp1
   : exp2
   | exp1 AND exp2
   ;

exp2
   : exp3
   | NOT exp2
   ;

exp3
   : exp4
   | exp3 '=' exp4
   | exp3 '==' exp4
   | exp3 '<>' exp4
   | exp3 '><' exp4
   | exp3 '<' exp4
   | exp3 '<=' exp4
   | exp3 '=<' exp4
   | exp3 '>=' exp4
   | exp3 '=>' exp4
   | exp3 '>' exp4
   ;

exp4
   : exp5
   | exp4 '&' exp5
   ;

exp5
   : exp6
   | exp5 '+' exp6
   | exp5 '-' exp6
   ;

exp6
   : exp7
   | exp6 '*' exp7
   | exp6 '/' exp7
   ;

exp7
   : exp8
   | '-' exp7
   | '+' exp7
   ;

exp8
   : exp9
   | exp9 '[' part_ref
   ;

part_ref
   : exp rest_of_part_ref
   ;

rest_of_part_ref
   : ']'
   | '..' ']'
   | '..' exp ']'
   ;

exp9
   : INTEGER_LITERAL
   | string_literal
   | NIL
   | TRUE
   | FALSE
   | ref
   | '(' exp ')'
   ;

var_ref
   : write_id
   | var_ref argument_list
   | var_ref '.' write_id
   | var_ref '->'
   ;

write_id
   : id
   ;

ref
   : id
   | ref argument_list
   | ref '.' id
   | ref '->'
   ;

variable_ref
   : var_ref opt_part_ref?
   ;

opt_part_ref
   : '[' part_ref
   ;

int_ct_exp
   : exp
   ;

id
   : NAME
   | NATIVE
   | ASCII
   | EBCDIC
   | DEC_MULTI
   | USER
   | TITLE
   | ON
   | OFF
   | PAGE
   | OF
   | TREE
   | RECORD
   | OVERLAY
   | INTEGER
   | STRING
   | BOOLEAN
   | POINTER
   | TREEPTR
   | FILL
   | VARYING
   | FIXED
   | DYNAMIC
   | COMMON
   | STATIC
   | AUTOMATIC
   | GLOBAL
   | EXPOSE
   | MAIN
   | CASELESS
   | ALIAS
   | VALUE
   | REFERENCE
   | DESCRIPTOR
   | SYNTAX
   | INPUT
   | OUTPUT
   | DATA
   | STACK
   | AS
   | IDENT
   | IGNORE
   | WIDTH
   ;
/*
* Lexer
*/
   
   
AMPERSAND
   : '&'
   ;
/*APOSTROPHE	: '\'\''; */
   
   
ATSIGN
   : '@'
   ;

BACKSLASH
   : '\\'
   ;

BAR
   : '|'
   ;

COLON
   : ':'
   ;

LEFT_BRACKET
   : '{'
   ;

LEFT_PAREN
   : '('
   ;

MINUS
   : '-'
   ;

RIGHT_BRACKET
   : '}'
   ;

RIGHT_PAREN
   : ')'
   ;

SEMICOLON
   : ';'
   ;

UNDERSCORE
   : '_'
   ;

ALIAS
   : 'ALIAS'
   ;

ALLOCATE
   : 'ALLOCATE'
   ;

AND
   : 'AND'
   ;

ANSWER
   : 'ANSWER'
   ;

AS
   : 'AS'
   ;

ASCII
   : 'ASCII'
   ;

AUTOMATIC
   : 'AUTOMATIC'
   ;

BOOLEAN
   : 'BOOLEAN'
   ;

CALL
   : 'CALL'
   ;

CASE
   : 'CASE'
   ;

CASELESS
   : 'CASELESS'
   ;

CLOSE
   : 'CLOSE'
   ;

COLLATE
   : 'COLLATE'
   ;

COMMON
   : 'COMMON'
   ;

CONSTANT
   : 'CONSTANT'
   ;

DATA
   : 'DATA'
   ;

DECLARE
   : 'DECLARE'
   ;

DEC_MULTI
   : 'DEC_MULTI'
   ;

DESCRIPTOR
   : 'DESCRIPTOR'
   ;

EBCDIC
   : 'EBCDIC'
   ;

DYNAMIC
   : 'DYNAMIC'
   ;

ELSE
   : 'ELSE'
   ;

END
   : 'END'
   ;

EXPOSE
   : 'EXPOSE'
   ;

EXTERNAL
   : 'EXTERNAL'
   ;

FAIL
   : 'FAIL'
   ;

FALSE
   : 'FALSE'
   ;

FILE
   : 'FILE'
   ;

FILL
   : 'FILL'
   ;

FIXED
   : 'FIXED'
   ;

FOR
   : 'FOR'
   ;

FORWARD
   : 'FORWARD'
   ;

FREE
   : 'FREE'
   ;

FROM
   : 'FROM'
   ;

GLOBAL
   : 'GLOBAL'
   ;

GOTO
   : 'GOTO'
   ;

GROUP
   : 'GROUP'
   ;

IDENT
   : 'IDENT'
   ;

IF
   : 'IF'
   ;

IGNORE
   : 'IGNORE'
   ;

INCLUDE
   : 'INCLUDE'
   ;

INPUT
   : 'INPUT'
   ;

INRANGE
   : 'INRANGE'
   ;

INTEGER
   : 'INTEGER'
   ;

LIST
   : 'LIST'
   ;

MACRO
   : 'MACRO'
   ;

MAIN
   : 'MAIN'
   ;

MODULE
   : 'MODULE'
   ;

NATIVE
   : 'NATIVE'
   ;

NIL
   : 'NIL'
   ;

NONE
   : 'NONE'
   ;

NOT
   : 'NOT'
   ;

OF
   : 'OF'
   ;

OFF
   : 'OFF'
   ;

ON
   : 'ON'
   ;

OPEN
   : 'OPEN'
   ;

OR
   : 'OR'
   ;

OUTPUT
   : 'OUTPUT'
   ;

OUTRANGE
   : 'OUTRANGE'
   ;

OVERLAY
   : 'OVERLAY'
   ;

PAGE
   : 'PAGE'
   ;

POINTER
   : 'POINTER'
   ;

PROCEDURE
   : 'PROCEDURE'
   ;

PROMPT
   : 'PROMPT'
   ;

PRUNE
   : 'PRUNE'
   ;

READ
   : 'READ'
   ;

RECORD
   : 'RECORD'
   ;

REDEFINE
   : 'REDEFINE'
   ;

REFERENCE
   : 'REFERENCE'
   ;

RETURN
   : 'RETURN'
   ;

SCAN
   : 'SCAN'
   ;

SET
   : 'SET'
   ;

STACK
   : 'STACK'
   ;

START
   : 'START'
   ;

STATIC
   : 'STATIC'
   ;

STEP
   : 'STEP'
   ;

STOP
   : 'STOP'
   ;

STRING
   : 'STRING'
   ;

SYNTAX
   : 'SYNTAX'
   ;

THEN
   : 'THEN'
   ;

TITLE
   : 'TITLE'
   ;

TO
   : 'TO'
   ;

TOKEN
   : 'TOKEN'
   ;

TREE
   : 'TREE'
   ;

TREEPTR
   : 'TREEPTR'
   ;

TRIGGER
   : 'TRIGGER'
   ;

TRUE
   : 'TRUE'
   ;

TYPE
   : 'TYPE'
   ;

USER
   : 'USER'
   ;

VALUE
   : 'VALUE'
   ;

VARYING
   : 'VARYING'
   ;

WHILE
   : 'WHILE'
   ;

WIDTH
   : 'WIDTH'
   ;

WRITE
   : 'WRITE'
   ;

XOR
   : 'XOR'
   ;

WS
   : [ \t\n\r\f]+ -> skip
   ;

TrailingComment
   : '!' .*? '\n' -> skip
   ;

InlineComment
   : '/*' .*? '*/' -> skip
   ;

CHARACTER_LITERAL
   : '\'' ('\'\'' | ~ '\'')* '\''
   ;

NAME
   : [A-Z] [A-Z0-9$_]*
   ;

INTEGER_LITERAL
   : [0-9]+
   ;

SCAN_LITERAL
   : 's\'eol\''
   | 's\'sos\''
   | 's\'eos\''
   ;

NUL
   : 's\'nul\''
   ;

SOH
   : 's\'soh\''
   ;

STX
   : 's\'stx\''
   ;

ETX
   : 's\'etx\''
   ;

EOT
   : 's\'eot\''
   ;

ENQ
   : 's\'enq\''
   ;

ACK
   : 's\'ack\''
   ;

BEL
   : 's\'bel\''
   ;

BS
   : 's\'bs\''
   ;

HT
   : 's\'ht\''
   ;

LF
   : 's\'lf\''
   ;

VT
   : 's\'ff\''
   ;

FF
   : 's\'vt\''
   ;

CR
   : 's\'cr\''
   ;

SO
   : 's\'so\''
   ;

SI
   : 's\'si\''
   ;

DLE
   : 's\'dle\''
   ;

DC1
   : 's\'dc1\''
   ;

DC2
   : 's\'dc2\''
   ;

DC3
   : 's\'dc3\''
   ;

DC4
   : 's\'dc4\''
   ;

NAK
   : 's\'nak\''
   ;

SYN
   : 's\'syn\''
   ;

ETB
   : 's\'etb\''
   ;

CAN
   : 's\'can\''
   ;

EM
   : 's\'em\''
   ;

SUB
   : 's\'sub\''
   ;

ESC
   : 's\'esc\''
   ;

FS
   : 's\'fs\''
   ;

GS
   : 's\'gs\''
   ;

RS
   : 's\'rs\''
   ;

US
   : 's\'us\''
   ;

DEL
   : 's\'del\''
   ;

IND
   : 's\'ind\''
   ;

NEL
   : 's\'nel\''
   ;

SSA
   : 's\'ssa\''
   ;

ESA
   : 's\'esa\''
   ;

HTS
   : 's\'hts\''
   ;

HTJ
   : 's\'htj\''
   ;

VTS
   : 's\'vts\''
   ;

PID
   : 's\'pid\''
   ;

PLU
   : 's\'plu\''
   ;

RI
   : 's\'ri\''
   ;

SS2
   : 's\'ss2\''
   ;

SS3
   : 's\'ss3\''
   ;

DCS
   : 's\'dcs\''
   ;

PU1
   : 's\'pu1\''
   ;

PU2
   : 's\'pu2\''
   ;

STS
   : 's\'sts\''
   ;

CCH
   : 's\'cch\''
   ;

MW
   : 's\'mw\''
   ;

SPA
   : 's\'spa\''
   ;

EPA
   : 's\'epa\''
   ;

CSI
   : 's\'csi\''
   ;

ST
   : 's\'st\''
   ;

OSC
   : 's\'osc\''
   ;

PM
   : 's\'pm\''
   ;

APC
   : 's\'apc\''
   ;

