/*
MIT License

Copyright (c) 2023 Mustafa Said Ağca

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
associated documentation files (the "Software"), to deal in the Software without restriction,
including without limitation the rights to use, copy, modify, merge, publish, distribute,
sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or
substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT
OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*/

grammar awk;

program
	: item_list item? EOF
	;
item_list
	: (item terminator)*
	;
item
	: action_
	| pattern action_
	| normal_pattern
	| FUNCTION func_name '(' param_list_opt ')' newline_opt action_
	;
param_list_opt
	: param_list?
	;
param_list
	: name (',' name)*
	;
pattern
	: normal_pattern
	| special_pattern
	;
normal_pattern
	: expr (',' newline_opt expr)?
	;
special_pattern
	: BEGIN
	| END
	;
action_
	: '{' newline_opt (terminated_statement_list | unterminated_statement_list)? '}'
	;
terminator
	: terminator NEWLINE
	| ';'
	| NEWLINE
	;
terminated_statement_list
	: terminated_statement+
	;
unterminated_statement_list
	: terminated_statement* unterminated_statement
	;
terminated_statement
	: action_ newline_opt
	| IF '(' expr ')' newline_opt terminated_statement (ELSE newline_opt terminated_statement)?
	| WHILE '(' expr ')' newline_opt terminated_statement
	| FOR '(' simple_statement_opt ';' expr_opt ';' simple_statement_opt ')' newline_opt terminated_statement
	| FOR '(' name IN name ')' newline_opt terminated_statement
	| ';' newline_opt
	| terminatable_statement (NEWLINE | ';') newline_opt
	;
unterminated_statement
	: terminatable_statement
	| IF '(' expr ')' newline_opt unterminated_statement
	| IF '(' expr ')' newline_opt terminated_statement ELSE newline_opt unterminated_statement
	| WHILE '(' expr ')' newline_opt unterminated_statement
	| FOR '(' simple_statement_opt ';' expr_opt ';' simple_statement_opt ')' newline_opt unterminated_statement
	| FOR '(' name IN name ')' newline_opt unterminated_statement
	;
terminatable_statement
	: simple_statement
	| BREAK
	| CONTINUE
	| NEXT
	| EXIT expr_opt
	| RETURN expr_opt
	| DO newline_opt terminated_statement WHILE '(' expr ')'
	;
simple_statement_opt
	: simple_statement?
	;
simple_statement
	: DELETE name '[' expr_list ']'
	| expr
	| print_statement
	;
print_statement
	: simple_print_statement output_redirection?
	;
simple_print_statement
	: PRINT print_expr_list_opt
	| PRINT '(' multiple_expr_list ')'
	| PRINTF print_expr_list
	| PRINTF '(' multiple_expr_list ')'
	;
output_redirection
	: '>' expr
	| APPEND expr
	| '|' expr
	;
expr_list_opt
	: expr_list?
	;
expr_list
	: expr (',' newline_opt expr)*
	;
multiple_expr_list
	: expr (',' newline_opt expr)+
	;
expr_opt
	: expr?
	;
expr
	: unary_expr
	| non_unary_expr
	;
unary_expr
	: ('+' | '-') expr
	| <assoc=right> unary_expr '^' expr
	| unary_expr ('*' | '/' | '%') expr
	| unary_expr ('+' | '-') expr
	| unary_expr non_unary_expr
	| unary_expr ('<' | '>' | LE | NE | EQ | GE) expr
	| unary_expr ('~' | NO_MATCH) expr
	| unary_expr IN name
	| unary_expr AND newline_opt expr
	| unary_expr OR newline_opt expr
	| <assoc=right> unary_expr '?' expr ':' expr
	| unary_expr '|' simple_get
	;
non_unary_expr
	: '(' expr ')'
	| '!' expr
	| <assoc=right> non_unary_expr '^' expr
	| non_unary_expr ('*' | '/' | '%') expr
	| non_unary_expr ('+' | '-') expr
	| non_unary_expr non_unary_expr
	| non_unary_expr ('<' | '>' | LE | NE | EQ | GE) expr
	| non_unary_expr ('~' | NO_MATCH) expr
	| non_unary_expr IN name
	| '(' multiple_expr_list ')' IN name
	| non_unary_expr AND newline_opt expr
	| non_unary_expr OR newline_opt expr
	| <assoc=right> non_unary_expr '?' expr ':' expr
	| number
	| string
	| ere
	| lvalue (INCR | DECR)?
	| (INCR | DECR) lvalue
	| <assoc=right> lvalue (POW_ASSIGN | MOD_ASSIGN | MUL_ASSIGN | DIV_ASSIGN | ADD_ASSIGN | SUB_ASSIGN | '=') expr
	| func_name '(' expr_list_opt ')'
	| builtin_func_name ('(' expr_list_opt ')')?
	| simple_get ('<' expr)?
	| non_unary_expr '|' simple_get
	;
print_expr_list_opt
	: print_expr_list?
	;
print_expr_list
	: print_expr (',' newline_opt print_expr)*
	;
print_expr
	: unary_print_expr
	| non_unary_print_expr
	;
unary_print_expr
	: ('+' | '-') print_expr
	| <assoc=right> unary_print_expr '^' print_expr
	| unary_print_expr ('*' | '/' | '%') print_expr
	| unary_print_expr ('+' | '-') print_expr
	| unary_print_expr non_unary_print_expr
	| unary_print_expr ('~' | NO_MATCH) print_expr
	| unary_print_expr IN name
	| unary_print_expr AND newline_opt print_expr
	| unary_print_expr OR newline_opt print_expr
	| <assoc=right> unary_print_expr '?' print_expr ':' print_expr
	;
non_unary_print_expr
	: '(' expr ')'
	| '!' print_expr
	| <assoc=right> non_unary_print_expr '^' print_expr
	| non_unary_print_expr ('*' | '/' | '%') print_expr
	| non_unary_print_expr ('+' | '-') print_expr
	| non_unary_print_expr non_unary_print_expr
	| non_unary_print_expr ('~' | NO_MATCH) print_expr
	| non_unary_print_expr IN name
	| '(' multiple_expr_list ')' IN name
	| non_unary_print_expr AND newline_opt print_expr
	| non_unary_print_expr OR newline_opt print_expr
	| <assoc=right> non_unary_print_expr '?' print_expr ':' print_expr
	| number
	| string
	| ere
	| lvalue (INCR | DECR)?
	| (INCR | DECR) lvalue
	| <assoc=right> lvalue (POW_ASSIGN | MOD_ASSIGN | MUL_ASSIGN | DIV_ASSIGN | ADD_ASSIGN | SUB_ASSIGN | '=') print_expr
	| func_name '(' expr_list_opt ')'
	| builtin_func_name ('(' expr_list_opt ')')?
	;
lvalue
	: name ('[' expr_list ']')?
	| '$' expr
	;
simple_get
	: GETLINE lvalue?
	;
newline_opt
	: NEWLINE*
	;
number
	: NUMBER
	;
string
	: STRING
	;
ere
	: ERE
	;
builtin_func_name
	: BUILTIN_FUNC_NAME
	;
func_name
	: WORD
	;
name
	: WORD
	;

BEGIN : 'BEGIN' ;
BREAK : 'break' ;
CONTINUE : 'continue' ;
DELETE : 'delete' ;
DO : 'do' ;
ELSE : 'else' ;
END : 'END' ;
EXIT : 'exit' ;
FOR : 'for' ;
FUNCTION : 'function' ;
GETLINE : 'getline' ;
IF : 'if' ;
IN : 'in' ;
NEXT : 'next' ;
PRINT : 'print' ;
PRINTF : 'printf' ;
RETURN : 'return' ;
WHILE : 'while' ;
BUILTIN_FUNC_NAME
	: 'atan2'
	| 'close'
	| 'cos'
	| 'exp'
	| 'gsub'
	| 'index'
	| 'int'
	| 'length'
	| 'log'
	| 'match'
	| 'rand'
	| 'sin'
	| 'split'
	| 'sprintf'
	| 'sqrt'
	| 'srand'
	| 'sub'
	| 'substr'
	| 'system'
	| 'tolower'
	| 'toupper'
	;

ADD_ASSIGN : '+=' ;
AND : '&&' ;
APPEND : '>>' ;
DECR : '--' ;
DIV_ASSIGN : '/=' ;
EQ : '==' ;
GE : '>=' ;
INCR : '++' ;
LE : '<=' ;
MOD_ASSIGN : '%=' ;
MUL_ASSIGN : '*=' ;
NE : '!=' ;
NO_MATCH : '!~' ;
OR : '||' ;
POW_ASSIGN : '^=' ;
SUB_ASSIGN : '-=' ;

COMMENT : '#' .*? NEWLINE -> channel(HIDDEN) ;
ERE : '/' (~[/\\\r\n] | ESCAPE_SEQUENCE)* '/' ;
ESC_NEWLINE : '\\' NEWLINE -> skip ;
NEWLINE : '\r'? '\n' ;
NUMBER : DECIMAL_CONSTANT | FLOAT_CONSTANT | HEX_CONSTANT | OCTAL_CONSTANT ;
SPACE : [ \t]+ -> skip ;
STRING : '"' (~["\\\r\n] | ESCAPE_SEQUENCE)* '"' ;
WORD : [A-Za-z_] [A-Za-z_0-9]* ;

fragment DECIMAL_CONSTANT : [1-9] [0-9]* ;
fragment DIGIT_SEQUENCE : [0-9]+ ;
fragment ESCAPE_SEQUENCE : '\\' (HEX_NUMBER | OCTAL_NUMBER | .) ;
fragment EXPONENT_PART : [eE] [+\-]? DIGIT_SEQUENCE ;
fragment FLOAT_CONSTANT : DIGIT_SEQUENCE '.' DIGIT_SEQUENCE EXPONENT_PART? | DIGIT_SEQUENCE EXPONENT_PART ;
fragment HEX_CONSTANT : '0' [xX] [0-9A-Fa-f]+ ;
fragment HEX_NUMBER : [xX] [0-9A-Fa-f] [0-9A-Fa-f]? ;
fragment OCTAL_CONSTANT : '0' [0-7]* ;
fragment OCTAL_NUMBER : [0-7] [0-7]? [0-7]? ;
