// SPDX-License-Identifier: MIT
// Copyright (c) 2026 AJGB (ajgb.eu)
//
// Minimal BASIC — ECMA-55, 1st edition (January 1978) / ANSI X3.60-1978.
// Section references (§x.y) point to ECMA-55. Context rules (line order, END last,
// FOR/NEXT nesting, keyword spacing, argument counts) are left to the interpreter.




parser grammar MinimalBasicParser;

options {
    tokenVocab = MinimalBasicLexer;
}

// §5 — empty lines are accepted as a convenience
program: ( line | NEWLINE)* EOF;
line: line_number statement ( NEWLINE | EOF);
line_number: INTEGER;

statement:
    data_statement
    | def_statement
    | dimension_statement
    | end_statement
    | for_statement
    | gosub_statement
    | goto_statement
    | if_then_statement
    | input_statement
    | let_statement
    | next_statement
    | on_goto_statement
    | option_statement
    | print_statement
    | randomize_statement
    | read_statement
    | remark_statement
    | restore_statement
    | return_statement
    | stop_statement
;

// §7
variable: numeric_variable | string_variable;
numeric_variable:
    simple_numeric_variable
    | numeric_array_element
;
simple_numeric_variable: LETTER_NAME | LETTER_DIGIT_NAME;
numeric_array_element: LETTER_NAME subscript;
subscript:
    LPAREN numeric_expression (COMMA numeric_expression)? RPAREN
;
string_variable: STRING_NAME;

// §8
expression: numeric_expression | string_expression;
numeric_expression: sign? term ( sign term)*;
sign: PLUS | MINUS;
term: factor ( multiplier factor)*;
multiplier: STAR | SLASH;
factor: primary ( CARET primary)*;
primary:
    numeric_variable                   # PrimaryVariable
    | numeric_rep                      # PrimaryNumber
    | numeric_function_ref             # PrimaryFunction
    | LPAREN numeric_expression RPAREN # PrimaryParens
;
numeric_rep: INTEGER | REAL;
numeric_function_ref: numeric_function_name argument_list?;
numeric_function_name:
    FN_NAME
    | ABS
    | ATN
    | COS
    | EXP
    | INT
    | LOG
    | RND
    | SGN
    | SIN
    | SQR
    | TAN
;
argument_list: LPAREN numeric_expression RPAREN;
string_expression: string_variable | STRING;

// §10
def_statement:
    DEF FN_NAME (LPAREN simple_numeric_variable RPAREN)? EQ numeric_expression
;

// §11
let_statement:
    LET numeric_variable EQ numeric_expression # NumericLet
    | LET string_variable EQ string_expression # StringLet
;

// §12
goto_statement: GOTO line_number;
if_then_statement: IF relational_expression THEN line_number;
relational_expression:
    numeric_expression relation numeric_expression          # NumericRelation
    | string_expression equality_relation string_expression # StringRelation
;
relation: equality_relation | LT | GT | GE | LE;
equality_relation: EQ | NE;
gosub_statement: GOSUB line_number;
return_statement: RETURN;
on_goto_statement:
    ON numeric_expression GOTO line_number (COMMA line_number)*
;
stop_statement: STOP;
end_statement: END;

// §13
for_statement:
    FOR simple_numeric_variable EQ numeric_expression TO numeric_expression (
        STEP numeric_expression
    )?
;
next_statement: NEXT simple_numeric_variable;

// §14
print_statement: PRINT print_list;
print_list: print_item? ( print_separator print_item?)*;
print_item: expression | tab_call;
tab_call: TAB LPAREN numeric_expression RPAREN;
print_separator: COMMA | SEMICOLON;

// §15, §16
input_statement: INPUT variable_list;
read_statement: READ variable_list;
restore_statement: RESTORE;
variable_list: variable ( COMMA variable)*;

// §17
data_statement: DATA datum ( DATA_COMMA datum)*;
datum: DATA_QUOTED | DATA_UNQUOTED;

// §18
dimension_statement:
    DIM array_declaration (COMMA array_declaration)*
;
array_declaration:
    LETTER_NAME LPAREN INTEGER (COMMA INTEGER)? RPAREN
;
option_statement: OPTION BASE INTEGER;

// §19, §20
remark_statement: REMARK;
randomize_statement: RANDOMIZE;