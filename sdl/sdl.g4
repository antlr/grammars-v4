/*
* Structure Definition Language (SDL) Grammar.
*
* SDL was a language used to provide a common definition of data structures
* that could be exported and shared by various programming languages. Many
* applications as well as the VMS Operating System and associated tools and
* utilities were written in a variety of programming languages. SDL itself
* contains modules written in Bliss, PLI, and Macro Assembler. It also uses
* a tool written in Pascal.
*
* Due to the lack of a PLI compiler on new porting platforms SDL was rewritten
* in C++. This original version was released as freeware.
*
* See the README.md for more background.
*
* This ANTLR4 grammar is based on the source code.
*
* This software is provided "AS IS" with no warranty of any kind.
*
* William H. Cox (DEC 1979-1995)
*
*/

// $antlr-format alignTrailingComments true, columnLimit 150, minEmptyLines 1, maxEmptyLinesToKeep 1, reflowComments false, useTab false

// $antlr-format allowShortRulesOnASingleLine false, allowShortBlocksOnASingleLine true, alignSemicolons hanging, alignColons hanging

grammar sdl;

options {
    caseInsensitive = true;
}

program
    : module_list EOF
    ;

module_list
    : module_declaration
    | module_list module_declaration
    ;

module_declaration
    : module_statement module_body end_module_statement
    | local_assignment
    | COMMENT
    ;

module_statement
    : m_name m_options? ';'
    ;

m_name
    : MODULE namestring
    ;

m_options
    : IDENT namestring
    ;

module_body
    : module_body statement
    | /* empty */
    ;

end_module_statement
    : END_MODULE name_option ';'
    ;

name_option
    : namestring
    | /* empty */
    ;

statement
    : local_assignment
    | constant_statement
    | entry_statement
    | aggregate_statement
    | item_statement
    | declare_statement
    | ifsymbol_statement
    | conditional_statement
    | literal_statement
    | read_statement
    | COMMENT
    | include_clause
    | ';'
    ;

/* Alter the include_clause. The file will not be inlined. */
include_clause
    : INCLUDE include_name ';'
    ;

include_name
    : STRING_LITERAL
    ;

read_statement
    : READ STRING_LITERAL ';'
    ;

stat_list
    : stat_list statement
    | statement
    ;

local_assignment
    : local_symbol '=' expression ';'
    ;

local_symbol
    : LOCAL_NAME
    ;

constant_statement
    : constant_key constant_body ';'
    ;

constant_key
    : CONSTANT
    ;

constant_body
    : constant_phrase
    | constant_body ',' comment_list constant_phrase
    ;

constant_phrase
    : name_or_list EQUALS value_or_string
    ;

value_or_string
    : constant_value constant_options
    | STRING string_value string_options
    ;

comment_list
    : comment_list COMMENT
    | /* empty */
    ;

name_or_list
    : c_name_1
    | '(' comment_list name_list ')' c_comment_list
    | '(' comment_list name_list c_comma2 ')' c_comment_list
    ;

name_list
    : c_name_1
    | c_comma1 c_name_1
    | name_list c_comma2 c_name_1
    ;

c_name_1
    : c_name c_comment_list
    ;

c_name
    : namestring
    ;

c_comma1
    : c_comma
    | c_comma1 c_comma
    ;

c_comma2
    : c_comma
    | c_comma2 c_comma
    ;

c_comma
    : ',' c_comment_list
    ;

c_comment_list
    : c_comment_list COMMENT
    | /* empty */
    ;

constant_value
    : expression
    ;

constant_options
    : constant_options INCREMENT expression
    | constant_options PREFIX namestring
    | constant_options TAG namestring
    | constant_options COUNTER LOCAL_NAME
    | constant_options TYPENAME typestring
    | /* empty */
    ;

string_value
    : STRING_LITERAL
    ;

string_options
    : string_options PREFIX namestring
    | string_options TAG namestring
    | /* empty */
    ;

namestring
    : NAME
    | STRING_LITERAL
    ;

entry_statement
    : entry_clause entry_option ';'
    ;

entry_clause
    : ENTRY namestring
    ;

entry_option
    : entry_option parameter_keyword '(' param_list ')'
    | entry_option return_keyword ret_data_type return_name_option
    | entry_option return_keyword ret_data_type return_name_option COMMENT
    | entry_option VARIABLE
    | entry_option TYPENAME typestring
    | entry_option ALIAS namestring
    | entry_option LINKAGE namestring
    | /* empty */
    ;

parameter_keyword
    : PARAMETER
    ;

return_keyword
    : RETURNS
    ;

ret_data_type
    : data_or_user_type
    | VOID
    ;

return_name_option
    : NAMED namestring
    | /* empty */
    ;

param_list
    : comment_list p1 param p_opt p2
    | param_list ',' comment_list p1 param p_opt p2
    ;

p1
    : /* empty */
    ;

p2
    : /* empty */
    ;

param
    : data_or_user_type
    | ANY
    ;

p_opt
    : p_opt VALUE
    | p_opt DESCRIPTOR
    | p_opt RTL_STR_DESC
    | p_opt DIMENSION dimen_expr
    | p_opt IN
    | p_opt OUT
    | p_opt LIST
    | p_opt NAMED namestring
    | p_opt DEFAULT expression
    | p_opt OPTIONAL
    | p_opt REFERENCE
    | p_opt COMMENT
    | p_opt TYPENAME typestring
    | /* empty */
    ;

typestring
    : NAME
    | STRING_LITERAL
    ;

dimen_expr
    : expression
    | lodimen ':' expression
    | '*'
    ;

lodimen
    : expression
    ;

data_type
    : BYTE sign_option
    | WORD sign_option
    | LONGWORD sign_option
    | QUADWORD sign_option
    | OCTAWORD sign_option
    | F_FLOATING complex_option
    | D_FLOATING complex_option
    | G_FLOATING complex_option
    | H_FLOATING complex_option
    | DECIMAL PRECISION '(' prec ',' scale ')'
    | BITFIELD vield_options
    | CHARACTER char_options
    | BOOLEAN
    | ADDRESS object_option
    | POINTER object_option
    | POINTER_LONG object_option
    | POINTER_QUAD object_option
    | POINTER_HW object_option
    | INTEGER_BYTE sign_option
    | INTEGER_WORD sign_option
    | INTEGER_LONG sign_option
    | INTEGER_QUAD sign_option
    | INTEGER_HW sign_option
    | INTEGER sign_option
    | HARDWARE_ADDRESS object_option
    | HARDWARE_INTEGER sign_option
    ;

object_option
    : obj_paren object_type base_option ')'
    | /* empty */
    ;

obj_paren
    : '('
    ;

object_type
    : data_or_user_type dimension_option
    | entry_object entry_option
    ;

entry_object
    : ENTRY
    ;

base_option
    : basealign_option
    | /* empty */
    ;

sign_option
    : SIGNED
    | UNSIGNED
    | /* empty */
    ;

complex_option
    : COMPLEX
    | /* empty */
    ;

prec
    : expression
    ;

scale
    : expression
    ;

vield_options
    : vield_options MASK
    | vield_options LENGTH expression
    | vield_options SIGNED
    | vield_options UNSIGNED
    | /* empty */
    ;

char_options
    : char_options VARYING
    | char_options LENGTH length_expr
    | /* empty */
    ;

length_expr
    : '*'
    | expression
    ;

dimension_option
    : DIMENSION dimen_expr
    | /* empty */
    ;

aggregate_statement
    : aggregate_dcl aggregate_body aggregate_end
    ;

aggregate_dcl
    : aggregate_clause aggregate_type impl_union_datatype aggr_options ';'
    ;

aggregate_clause
    : AGGREGATE namestring
    ;

aggregate_type
    : STRUCTURE
    | UNION
    ;

impl_union_datatype
    : data_type
    | /* empty */
    ;

aggr_options
    : aggr_options COMMON
    | aggr_options GLOBAL
    | aggr_options TYPEDEF
    | aggr_options ALIGN
    | aggr_options NOALIGN
    | aggr_options basealign_option
    | aggr_options PREFIX namestring
    | aggr_options TAG namestring
    | aggr_options DIMENSION dimen_expr
    | aggr_options ORIGIN namestring
    | aggr_options BASED namestring
    | aggr_options BASED
    | aggr_options MARKER namestring
    | aggr_options FILL
    | /* empty */
    ;

aggregate_body
    : aggregate_body aggregate_member
    | aggregate_body local_assignment
    | aggregate_body constant_statement
    | aggregate_body COMMENT
    | aggregate_body ifsymbol_clause
    | aggregate_body ';'
    | /* empty */
    ;

aggregate_member
    : member_name rest_of_member
    ;

member_name
    : namestring
    ;

rest_of_member
    : data_or_user_type member_options ';'
    | sub_agg_dcl aggregate_body aggregate_end
    ;

member_options
    : member_options PREFIX namestring
    | member_options TAG namestring
    | member_options FILL
    | member_options DIMENSION dimen_expr
    | member_options ALIGN
    | member_options NOALIGN
    | member_options basealign_option
    | /* empty */
    ;

sub_agg_dcl
    : aggregate_type impl_union_datatype member_options ';'
    ;

aggregate_end
    : END name_option ';'
    ;

ifsymbol_clause
    : ifsymbol_start ';' aggregate_body else_ifsymbol_clause else_symbol_clause ifsymbol_end ';'
    ;

else_ifsymbol_clause
    : else_ifsymbol_clause else_ifsymbol_start ';' aggregate_body
    | /* empty */
    ;

else_symbol_clause
    : else_symbol ';' aggregate_body
    | /* empty */
    ;

ifsymbol_statement
    : ifsymbol_start ';' module_body else_ifsymbol_statement else_symbol_statement ifsymbol_end ';'
    ;

else_ifsymbol_statement
    : else_ifsymbol_statement else_ifsymbol_start ';' module_body
    | /* empty */
    ;

else_symbol_statement
    : else_symbol ';' module_body
    | /* empty */
    ;

ifsymbol_start
    : IFSYMBOL namestring
    ;

else_ifsymbol_start
    : ELSE_IFSYMBOL namestring
    ;

else_symbol
    : ELSE
    ;

ifsymbol_end
    : END_IFSYMBOL
    ;

item_statement
    : item_clause data_or_user_type item_options ';'
    ;

item_clause
    : ITEM namestring
    ;

item_options
    : item_options COMMON
    | item_options GLOBAL
    | item_options TYPEDEF
    | item_options PREFIX namestring
    | item_options TAG namestring
    | item_options DIMENSION dimen_expr
    | item_options basealign_option
    | /* empty */
    ;

data_or_user_type
    : data_type
    | user_typename sizeof_clause_opt
    ;

user_typename
    : namestring
    ;

sizeof_clause_opt
    : sizeof_clause
    | /* empty */
    ;

sizeof_clause
    : sizeof_keyword sizeof_option
    ;

sizeof_keyword
    : SIZEOF
    ;

sizeof_option
    : data_or_user_type
    | '(' expression ')'
    ;

basealign_option
    : basealign_key data_or_user_type
    | basealign_key '(' expression ')'
    ;

basealign_key
    : BASEALIGN
    ;

declare_statement
    : decl_clause decl_body decl_options
    ;

decl_clause
    : DECLARE user_typename
    ;

decl_body
    : sizeof_clause
    ;

decl_options
    : decl_options PREFIX namestring
    | decl_options TAG namestring
    | /* empty */
    ;

conditional_statement
    : cond1 cond2 module_body END_IFLANGUAGE lang_list_opt
    ;

cond1
    : IFLANGUAGE
    ;

cond2
    : language_list
    ;

language_list
    : lang_name
    | language_list lang_name
    ;

lang_name
    : namestring
    ;

lang_list_opt
    : lang_list_opt namestring
    | /* empty */
    ;

literal_statement
    : literal_keyword stringlist END_LITERAL
    ;

literal_keyword
    : LITERAL ';'
    ;

stringlist
    : stringlist STRING_LITERAL
    | /* empty */
    ;

expression
    : expression '&' expression_1
    | expression_1
    ;

expression_1
    : expression_1 '!' expression_2
    | expression_2
    ;

expression_2
    : expression_2 '@' expression_3
    | expression_3
    ;

expression_3
    : expression_3 '+' expression_4
    | expression_3 '-' expression_4
    | expression_4
    ;

expression_4
    : expression_4 '*' expression_5
    | expression_4 '/' expression_5
    | expression_5
    ;

expression_5
    : '+' expression_6
    | '-' expression_6
    | expression_6
    ;

expression_6
    : numeric
    | LOCAL_NAME
    | namestring
    | '.'
    | '^'
    | ':'
    | '(' expression ')'
    ;

numeric
    : INTEGER_NUMBER
    | HEX_NUMBER
    | OCTAL_NUMBER
    | BINARY_NUMBER
    | ASCII_VALUE
    ;

ADDRESS
    : 'ADDRESS'
    ;

AGGREGATE
    : 'AGGREGATE'
    ;

ALIAS
    : 'ALIAS'
    ;

ALIGN
    : 'ALIGN'
    ;

ANY
    : 'ANY'
    ;

BASEALIGN
    : 'BASEALIGN'
    ;

BASED
    : 'BASED'
    ;

BITFIELD
    : 'BITFIELD'
    ;

BOOLEAN
    : 'BOOLEAN'
    ;

BYTE
    : 'BYTE'
    ;

CHARACTER
    : 'CHARACTER'
    ;

COMMON
    : 'COMMON'
    ;

COMPLEX
    : 'COMPLEX'
    ;

CONSTANT
    : 'CONSTANT'
    ;

COUNTER
    : 'COUNTER'
    ;

D_FLOATING
    : 'D_FLOATING'
    ;

DECIMAL
    : 'DECIMAL'
    ;

DECLARE
    : 'DECLARE'
    ;

DEFAULT
    : 'DEFAULT'
    ;

DESCRIPTOR
    : 'DESCRIPTOR'
    ;

DIMENSION
    : 'DIMENSION'
    ;

ELSE
    : 'ELSE'
    ;

ELSE_IFSYMBOL
    : 'ELSE_IFSYMBOL'
    ;

END
    : 'END'
    ;

END_IFSYMBOL
    : 'END_IFSYMBOL'
    ;

END_IFLANGUAGE
    : 'END_IFLANGUAGE'
    ;

END_LITERAL
    : 'END_LITERAL'
    ;

END_MODULE
    : 'END_MODULE'
    ;

ENTRY
    : 'ENTRY'
    ;

EQUALS
    : 'EQUALS'
    ;

F_FLOATING
    : 'F_FLOATING'
    ;

FILL
    : 'FILL'
    ;

G_FLOATING
    : 'G_FLOATING'
    ;

GLOBAL
    : 'GLOBAL'
    ;

H_FLOATING
    : 'H_FLOATING'
    ;

HARDWARE_ADDRESS
    : 'HARDWARE_ADDRESS'
    ;

HARDWARE_INTEGER
    : 'HARDWARE_INTEGER'
    ;

IDENT
    : 'IDENT'
    ;

IFLANGUAGE
    : 'IFLANGUAGE'
    ;

IFSYMBOL
    : 'IFSYMBOL'
    ;

IN
    : 'IN'
    ;

INCLUDE
    : 'INCLUDE'
    ;

INCREMENT
    : 'INCREMENT'
    ;

INTEGER
    : 'INTEGER'
    ;

INTEGER_BYTE
    : 'INTEGER_BYTE'
    ;

INTEGER_HW
    : 'INTEGER_HW'
    ;

INTEGER_LONG
    : 'INTEGER_LONG'
    ;

INTEGER_QUAD
    : 'INTEGER_QUAD'
    ;

INTEGER_WORD
    : 'INTEGER_WORD'
    ;

ITEM
    : 'ITEM'
    ;

LENGTH
    : 'LENGTH'
    ;

LINKAGE
    : 'LINKAGE'
    ;

LIST
    : 'LIST'
    ;

LITERAL
    : 'LITERAL'
    ;

LONGWORD
    : 'LONGWORD'
    ;

MARKER
    : 'MARKER'
    ;

MASK
    : 'MASK'
    ;

MODULE
    : 'MODULE'
    ;

NAMED
    : 'NAMED'
    ;

NOALIGN
    : 'NOALIGN'
    ;

OCTAWORD
    : 'OCTAWORD'
    ;

OPTIONAL
    : 'OPTIONAL'
    ;

ORIGIN
    : 'ORIGIN'
    ;

OUT
    : 'OUT'
    ;

PARAMETER
    : 'PARAMETER'
    ;

POINTER
    : 'POINTER'
    ;

POINTER_HW
    : 'POINTER_HW'
    ;

POINTER_LONG
    : 'POINTER_LONG'
    ;

POINTER_QUAD
    : 'POINTER_QUAD'
    ;

PRECISION
    : 'PRECISION'
    ;

PREFIX
    : 'PREFIX'
    ;

QUADWORD
    : 'QUADWORD'
    ;

READ
    : 'READ'
    ;

REFERENCE
    : 'REFERENCE'
    ;

RETURNS
    : 'RETURNS'
    ;

RTL_STR_DESC
    : 'RTL_STR_DESC'
    ;

SIGNED
    : 'SIGNED'
    ;

SIZEOF
    : 'SIZEOF'
    ;

STRING
    : 'STRING'
    ;

STRUCTURE
    : 'STRUCTURE'
    ;

TAG
    : 'TAG'
    ;

TYPEDEF
    : 'TYPEDEF'
    ;

TYPENAME
    : 'TYPENAME'
    ;

UNION
    : 'UNION'
    ;

UNSIGNED
    : 'UNSIGNED'
    ;

VALUE
    : 'VALUE'
    ;

VARIABLE
    : 'VARIABLE'
    ;

VARYING
    : 'VARYING'
    ;

VOID
    : 'VOID'
    ;

WORD
    : 'WORD'
    ;

INTEGER_NUMBER
    : [0-9]+
    ;

HEX_NUMBER
    : '%X' [0-9A-F]+
    ;

OCTAL_NUMBER
    : '%O' [0-7]+
    ;

BINARY_NUMBER
    : '%B' [0-1]+
    ;

ASCII_VALUE
    : '%A' [A-Z]
    ;

NAME
    : [A-Z0-9$_]+
    ;

LOCAL_NAME
    : '#' [A-Z0-9$_]+
    ;

STRING_LITERAL
    : '"' (ESC | .)*? '"'
    ;

ESC
    : '\\"'
    ;

COMMENT
    : '/*' .*? '\n'
    ;

LOCAL_COMMENT
    : '{' .*? '\n' -> skip
    ;

WS
    : [ \t\r\n\f]+ -> skip
    ;
