%token IDENTIFIER CONSTANT STRING_LITERAL
%token ARRAYMUL ARRAYPOW ARRAYDIV ARRAYRDIV 
%token LE_OP GE_OP EQ_OP NE_OP

%token IF ELSE ELSEIF WHILE FOR BREAK RETURN END 
%token FUNCTION TRANSPOSE NCTRANSPOSE CR GLOBAL CLEAR

%start translation_unit
%%
/* 
  known bugs:
  (1) Cannot distinguish 'function' from 'array' (need symbol table)
  (2) Multidimentional array accepted, i.e., a(:,:,:) (ver. 5.0 support :-)
  (3) Some commands, such as 'hold on', 'load m.txt' cannot be checked.
      (need symbol table to identify functions)
*/

primary_expression
        : IDENTIFIER                  
        | CONSTANT    
        | STRING_LITERAL        
        | '(' expression ')'
        | '[' ']'
        | '[' array_list ']'
        ;

postfix_expression
        : primary_expression
        | array_expression
        | postfix_expression TRANSPOSE
        | postfix_expression NCTRANSPOSE
        ;

index_expression
        : ':'
        | expression
        ;

index_expression_list
        : index_expression
        | index_expression_list ',' index_expression
        ;

array_expression
        : IDENTIFIER '(' index_expression_list ')'
        ;

unary_expression
        : postfix_expression
        | unary_operator postfix_expression
        ;

unary_operator
        : '+'
        | '-'
        | '~'
        ;

multiplicative_expression
        : unary_expression
        | multiplicative_expression '*' unary_expression
        | multiplicative_expression '/' unary_expression
        | multiplicative_expression '\\' unary_expression
        | multiplicative_expression '^' unary_expression
        | multiplicative_expression ARRAYMUL unary_expression
        | multiplicative_expression ARRAYDIV unary_expression
        | multiplicative_expression ARRAYRDIV unary_expression
        | multiplicative_expression ARRAYPOW unary_expression
        ;

additive_expression
        : multiplicative_expression
        | additive_expression '+' multiplicative_expression
        | additive_expression '-' multiplicative_expression
        ;

relational_expression
        : additive_expression
        | relational_expression '<' additive_expression
        | relational_expression '>' additive_expression
        | relational_expression LE_OP additive_expression
        | relational_expression GE_OP additive_expression
        ;

equality_expression
        : relational_expression
        | equality_expression EQ_OP relational_expression
        | equality_expression NE_OP relational_expression
        ;

and_expression
        : equality_expression
        | and_expression '&' equality_expression
        ;

or_expression
        : and_expression
        | or_expression '|' and_expression
        ;

expression
        : or_expression
	| expression ':' or_expression
	;

assignment_expression
        : postfix_expression '=' expression

eostmt
        :  ','
        |  ';'
        |  CR
        ;

statement
        : global_statement
        | clear_statement
        | assignment_statement
        | expression_statement
        | selection_statement
        | iteration_statement
        | jump_statement
        ;

statement_list
        : statement
        | statement_list statement
        ;
	
identifier_list
        : IDENTIFIER
        | identifier_list IDENTIFIER
        ;

global_statement
        : GLOBAL identifier_list eostmt
        ;

clear_statement
        : CLEAR identifier_list eostmt
        ;

expression_statement
        : eostmt
        | expression eostmt
        ;

assignment_statement
        : assignment_expression eostmt
        ;

array_element
        : expression
        | expression_statement
        ;

array_list
        : array_element
        | array_list array_element
        ;
 
selection_statement
        : IF expression statement_list END eostmt
        | IF expression statement_list ELSE statement_list END eostmt
        | IF expression statement_list elseif_clause END eostmt
        | IF expression statement_list elseif_clause
          ELSE statement_list END eostmt
        ;

elseif_clause
        : ELSEIF expression statement_list
	| elseif_clause ELSEIF expression statement_list
        ;
	
iteration_statement
        : WHILE expression statement_list END eostmt
        | FOR IDENTIFIER '=' expression statement_list END eostmt
        | FOR '(' IDENTIFIER '=' expression ')' statement_list END eostmt 
        ;

jump_statement
        : BREAK eostmt
        | RETURN eostmt
        ;

translation_unit
        : statement_list
        | FUNCTION function_declare eostmt statement_list
        ;

func_ident_list
        : IDENTIFIER
        | func_ident_list ',' IDENTIFIER
        ;

func_return_list
        : IDENTIFIER
        | '[' func_ident_list ']'
        ;

function_declare_lhs
        : IDENTIFIER
        | IDENTIFIER '(' ')'
        | IDENTIFIER '(' func_ident_list ')'
        ;

function_declare
        : function_declare_lhs
        | func_return_list '=' function_declare_lhs
        ;
      
%%
#include <stdio.h>

extern char yytext[];
extern int column;

yyerror(s)
char *s;
{
        fflush(stdout);
        printf("\n%*s\n%*s\n", column, "^", column, s);
}
