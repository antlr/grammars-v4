/*
Python grammar
The MIT License (MIT)
Copyright (c) 2021 Robert Einhorn

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:
The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
 */

 /*
  * Project      : an ANTLR4 parser grammar by the official PEG grammar
  *                https://github.com/RobEin/ANTLR4-parser-for-Python-3.13
  * Developed by : Robert Einhorn
  *
  */

 /*
  * Contributors : [Willie Shen](https://github.com/Willie169)
  */

// Python 3.13.2  https://docs.python.org/3.13/reference/grammar.html#full-grammar-specification

parser grammar PythonParser;

options { tokenVocab=PythonLexer; }

// STARTING RULES
// ==============

file_input: statements? EOF;
interactive: statement_newline;
eval: expressions NEWLINE* EOF;
func_type: '(' type_expressions? ')' '->' expression NEWLINE* EOF;

// GENERAL STATEMENTS
// ==================

statements: statement+;

statement: compound_stmt  | simple_stmts;

statement_newline
    : compound_stmt NEWLINE
    | simple_stmts
    | NEWLINE
    | EOF;

simple_stmts
    : simple_stmt (';' simple_stmt)* ';'? NEWLINE
    ;

// NOTE: assignment MUST precede expression, else parsing a simple assignment
// will throw a SyntaxError.
simple_stmt
    : assignment
    | type_alias
    | star_expressions
    | return_stmt
    | import_stmt
    | raise_stmt
    | 'pass'
    | del_stmt
    | yield_stmt
    | assert_stmt
    | 'break'
    | 'continue'
    | global_stmt
    | nonlocal_stmt;

compound_stmt
    : function_def
    | if_stmt
    | class_def
    | with_stmt
    | for_stmt
    | try_stmt
    | while_stmt
    | match_stmt;

// SIMPLE STATEMENTS
// =================

// NOTE: annotated_rhs may start with 'yield'; yield_expr must start with 'yield'
assignment
    : name ':' expression ('=' annotated_rhs )?
    | ('(' single_target ')'
         | single_subscript_attribute_target) ':' expression ('=' annotated_rhs )?
    | (star_targets '=' )+ (yield_expr | star_expressions) TYPE_COMMENT?
    | single_target augassign (yield_expr | star_expressions);

annotated_rhs: yield_expr | star_expressions;

augassign
    : '+='
    | '-='
    | '*='
    | '@='
    | '/='
    | '%='
    | '&='
    | '|='
    | '^='
    | '<<='
    | '>>='
    | '**='
    | '//=';

return_stmt
    : 'return' star_expressions?;

raise_stmt
    : 'raise' (expression ('from' expression )?)?
    ;

global_stmt: 'global' name (',' name)*;

nonlocal_stmt: 'nonlocal' name (',' name)*;

del_stmt
    : 'del' del_targets;

yield_stmt: yield_expr;

assert_stmt: 'assert' expression (',' expression )?;

import_stmt
    : import_name
    | import_from;

// Import statements
// -----------------

import_name: 'import' dotted_as_names;
// note below: the ('.' | '...') is necessary because '...' is tokenized as ELLIPSIS
import_from
    : 'from' ('.' | '...')* dotted_name 'import' import_from_targets
    | 'from' ('.' | '...')+ 'import' import_from_targets;
import_from_targets
    : '(' import_from_as_names ','? ')'
    | import_from_as_names
    | '*';
import_from_as_names
    : import_from_as_name (',' import_from_as_name)*;
import_from_as_name
    : name ('as' name )?;
dotted_as_names
    : dotted_as_name (',' dotted_as_name)*;
dotted_as_name
    : dotted_name ('as' name )?;
dotted_name
    : dotted_name '.' name
    | name;

// COMPOUND STATEMENTS
// ===================

// Common elements
// ---------------

block
    : NEWLINE INDENT statements DEDENT
    | simple_stmts;

decorators: ('@' named_expression NEWLINE )+;

// Class definitions
// -----------------

class_def
    : decorators class_def_raw
    | class_def_raw;

class_def_raw
    : 'class' name type_params? ('(' arguments? ')' )? ':' block;

// Function definitions
// --------------------

function_def
    : decorators function_def_raw
    | function_def_raw;

function_def_raw
    : 'def' name type_params? '(' params? ')' ('->' expression )? ':' func_type_comment? block
    | 'async' 'def' name type_params? '(' params? ')' ('->' expression )? ':' func_type_comment? block;

// Function parameters
// -------------------

params
    : parameters;

parameters
    : slash_no_default param_no_default* param_with_default* star_etc?
    | slash_with_default param_with_default* star_etc?
    | param_no_default+ param_with_default* star_etc?
    | param_with_default+ star_etc?
    | star_etc;

// Some duplication here because we can't write (',' | {isCurrentTokenType(RPAR)}?),
// which is because we don't support empty alternatives (yet).

slash_no_default
    : param_no_default+ '/' ','?
    ;
slash_with_default
    : param_no_default* param_with_default+ '/' ','?
    ;

star_etc
    : '*' param_no_default param_maybe_default* kwds?
    | '*' param_no_default_star_annotation param_maybe_default* kwds?
    | '*' ',' param_maybe_default+ kwds?
    | kwds;

kwds
    : '**' param_no_default;

// One parameter.  This *includes* a following comma and type comment.
//
// There are three styles:
// - No default_assignment
// - With default_assignment
// - Maybe with default_assignment
//
// There are two alternative forms of each, to deal with type comments:
// - Ends in a comma followed by an optional type comment
// - No comma, optional type comment, must be followed by close paren
// The latter form is for a final parameter without trailing comma.
//

param_no_default
    : param ','? TYPE_COMMENT?
    ;
param_no_default_star_annotation
    : param_star_annotation ','? TYPE_COMMENT?
    ;
param_with_default
    : param default_assignment ','? TYPE_COMMENT?
    ;
param_maybe_default
    : param default_assignment? ','? TYPE_COMMENT?
    ;
param: name annotation?;
param_star_annotation: name star_annotation;
annotation: ':' expression;
star_annotation: ':' star_expression;
default_assignment: '=' expression;

// If statement
// ------------

if_stmt
    : 'if' named_expression ':' block (elif_stmt | else_block?)
    ;
elif_stmt
    : 'elif' named_expression ':' block (elif_stmt | else_block?)
    ;
else_block
    : 'else' ':' block;

// While statement
// ---------------

while_stmt
    : 'while' named_expression ':' block else_block?;

// For statement
// -------------

for_stmt
    : 'async'? 'for' star_targets 'in' star_expressions ':' TYPE_COMMENT? block else_block?
    ;

// With statement
// --------------

with_stmt
    : 'with' '(' with_item (',' with_item)* ','? ')' ':' TYPE_COMMENT? block
    | 'async' 'with' '(' with_item (',' with_item)* ','? ')' ':' block
    | 'async'? 'with' with_item (',' with_item)* ':' TYPE_COMMENT? block
    ;

with_item
    : expression ('as' star_target)?
    ;

// Try statement
// -------------

try_stmt
    : 'try' ':' block finally_block
    | 'try' ':' block except_block+ else_block? finally_block?
    | 'try' ':' block except_star_block+ else_block? finally_block?;


// Except statement
// ----------------

except_block
    : 'except' (expression ('as' name )?)? ':' block
    ;
except_star_block
    : 'except' '*' expression ('as' name )? ':' block;
finally_block
    : 'finally' ':' block;

// Match statement
// ---------------

match_stmt
    : 'match' subject_expr ':' NEWLINE INDENT case_block+ DEDENT;

subject_expr
    : star_named_expression ',' star_named_expressions?
    | named_expression;

case_block
    : 'case' patterns guard? ':' block;

guard: 'if' named_expression;

patterns
    : open_sequence_pattern
    | pattern;

pattern
    : as_pattern
    | or_pattern;

as_pattern
    : or_pattern 'as' pattern_capture_target;

or_pattern
    : closed_pattern ('|' closed_pattern)*;

closed_pattern
    : literal_pattern
    | capture_pattern
    | wildcard_pattern
    | value_pattern
    | group_pattern
    | sequence_pattern
    | mapping_pattern
    | class_pattern;

// Literal patterns are used for equality and identity constraints
literal_pattern
    : signed_number
    | complex_number
    | strings
    | 'None'
    | 'True'
    | 'False';

// Literal expressions are used to restrict permitted mapping pattern keys
literal_expr
    : signed_number
    | complex_number
    | strings
    | 'None'
    | 'True'
    | 'False';

complex_number
    : signed_real_number ('+' | '-') imaginary_number
    ;

signed_number
    : '-'? NUMBER
    ;

signed_real_number
    : '-'? real_number
    ;

real_number
    : NUMBER;

imaginary_number
    : NUMBER;

capture_pattern
    : pattern_capture_target;

pattern_capture_target
    : name_except_underscore;

wildcard_pattern
    : '_';

value_pattern
    : attr;

attr
    : name ('.' name)+
    ;
name_or_attr
    : name ('.' name)*
    ;

group_pattern
    : '(' pattern ')';

sequence_pattern
    : '[' maybe_sequence_pattern? ']'
    | '(' open_sequence_pattern? ')';

open_sequence_pattern
    : maybe_star_pattern ',' maybe_sequence_pattern?;

maybe_sequence_pattern
    : maybe_star_pattern (',' maybe_star_pattern)* ','?;

maybe_star_pattern
    : star_pattern
    | pattern;

star_pattern
    : '*' name
    ;

mapping_pattern
    : LBRACE RBRACE
    | LBRACE double_star_pattern ','? RBRACE
    | LBRACE items_pattern (',' double_star_pattern)? ','? RBRACE
    ;

items_pattern
    : key_value_pattern (',' key_value_pattern)*;

key_value_pattern
    : (literal_expr | attr) ':' pattern;

double_star_pattern
    : '**' pattern_capture_target;

class_pattern
    : name_or_attr '(' ((positional_patterns (',' keyword_patterns)? | keyword_patterns) ','?)? ')'
    ;



positional_patterns
    : pattern (',' pattern)*;

keyword_patterns
    : keyword_pattern (',' keyword_pattern)*;

keyword_pattern
    : name '=' pattern;

// Type statement
// ---------------

type_alias
    : 'type' name type_params? '=' expression;

// Type parameter declaration
// --------------------------

type_params: '[' type_param_seq  ']';

type_param_seq: type_param (',' type_param)* ','?;

type_param
    : name type_param_bound? type_param_default?
    | '*'  name type_param_starred_default?
    | '**' name type_param_default?
    ;


type_param_bound: ':' expression;
type_param_default: '=' expression;
type_param_starred_default: '=' star_expression;

// EXPRESSIONS
// -----------

expressions
    : expression (',' expression )* ','?
    ;


expression
    : disjunction ('if' disjunction 'else' expression)?
    | lambdef
    ;

yield_expr
    : 'yield' ('from' expression | star_expressions?)
    ;

star_expressions
    : star_expression (',' star_expression )* ','?
    ;


star_expression
    : '*' bitwise_or
    | expression;

star_named_expressions: star_named_expression (',' star_named_expression)* ','?;

star_named_expression
    : '*' bitwise_or
    | named_expression;

assignment_expression
    : name ':=' expression;

named_expression
    : assignment_expression
    | expression;

disjunction
    : conjunction ('or' conjunction )*
    ;

conjunction
    : inversion ('and' inversion )*
    ;

inversion
    : 'not' inversion
    | comparison;

// Comparison operators
// --------------------

comparison
    : bitwise_or compare_op_bitwise_or_pair*
    ;

compare_op_bitwise_or_pair
    : eq_bitwise_or
    | noteq_bitwise_or
    | lte_bitwise_or
    | lt_bitwise_or
    | gte_bitwise_or
    | gt_bitwise_or
    | notin_bitwise_or
    | in_bitwise_or
    | isnot_bitwise_or
    | is_bitwise_or;

eq_bitwise_or: '==' bitwise_or;
noteq_bitwise_or
    : ('!=' ) bitwise_or;
lte_bitwise_or: '<=' bitwise_or;
lt_bitwise_or: '<' bitwise_or;
gte_bitwise_or: '>=' bitwise_or;
gt_bitwise_or: '>' bitwise_or;
notin_bitwise_or: 'not' 'in' bitwise_or;
in_bitwise_or: 'in' bitwise_or;
isnot_bitwise_or: 'is' 'not' bitwise_or;
is_bitwise_or: 'is' bitwise_or;

// Bitwise operators
// -----------------

bitwise_or
    : bitwise_or '|' bitwise_xor
    | bitwise_xor;

bitwise_xor
    : bitwise_xor '^' bitwise_and
    | bitwise_and;

bitwise_and
    : bitwise_and '&' shift_expr
    | shift_expr;

shift_expr
    : shift_expr ('<<' | '>>') sum
    | sum
    ;

// Arithmetic operators
// --------------------

sum
    : sum ('+' | '-') term
    | term
    ;

term
    : term ('*' | '/' | '//' | '%' | '@') factor
    | factor
    ;




factor
    : '+' factor
    | '-' factor
    | '~' factor
    | power;

power
    : await_primary ('**' factor)?
    ;

// Primary elements
// ----------------

// Primary elements are things like "obj.something.something", "obj[something]", "obj(something)", "obj" ...

await_primary
    : 'await' primary
    | primary;

primary
    : primary ('.' name | genexp | '(' arguments? ')' | '[' slices ']')
    | atom
    ;



slices
    : slice
    | (slice | starred_expression) (',' (slice | starred_expression))* ','?;

slice
    : expression? ':' expression? (':' expression? )?
    | named_expression;

atom
    : name
    | 'True'
    | 'False'
    | 'None'
    | strings
    | NUMBER
    | (tuple | group | genexp)
    | (list | listcomp)
    | (dict | set | dictcomp | setcomp)
    | '...';

group
    : '(' (yield_expr | named_expression) ')';

// Lambda functions
// ----------------

lambdef
    : 'lambda' lambda_params? ':' expression;

lambda_params
    : lambda_parameters;

// lambda_parameters etc. duplicates parameters but without annotations
// or type comments, and if there's no comma after a parameter, we expect
// a colon, not a close parenthesis.  (For more, see parameters above.)
//
lambda_parameters
    : lambda_slash_no_default lambda_param_no_default* lambda_param_with_default* lambda_star_etc?
    | lambda_slash_with_default lambda_param_with_default* lambda_star_etc?
    | lambda_param_no_default+ lambda_param_with_default* lambda_star_etc?
    | lambda_param_with_default+ lambda_star_etc?
    | lambda_star_etc;

lambda_slash_no_default
    : lambda_param_no_default+ '/' ','?
    ;

lambda_slash_with_default
    : lambda_param_no_default* lambda_param_with_default+ '/' ','?
    ;

lambda_star_etc
    : '*' lambda_param_no_default lambda_param_maybe_default* lambda_kwds?
    | '*' ',' lambda_param_maybe_default+ lambda_kwds?
    | lambda_kwds;

lambda_kwds
    : '**' lambda_param_no_default;

lambda_param_no_default
    : lambda_param ','?
    ;
lambda_param_with_default
    : lambda_param default_assignment ','?
    ;
lambda_param_maybe_default
    : lambda_param default_assignment? ','?
    ;
lambda_param: name;

// LITERALS
// ========

fstring_middle
    : fstring_replacement_field
    | FSTRING_MIDDLE;
fstring_replacement_field
    : LBRACE annotated_rhs '='? fstring_conversion? fstring_full_format_spec? RBRACE;
fstring_conversion
    : '!' name;
fstring_full_format_spec
    : ':' fstring_format_spec*;
fstring_format_spec
    : FSTRING_MIDDLE
    | fstring_replacement_field;
fstring
    : FSTRING_START fstring_middle* FSTRING_END;

string: STRING;
strings: (fstring|string)+;

list
    : '[' star_named_expressions? ']';

tuple
    : '(' (star_named_expression ',' star_named_expressions?  )? ')';

set: LBRACE star_named_expressions RBRACE;

// Dicts
// -----

dict
    : LBRACE double_starred_kvpairs? RBRACE;

double_starred_kvpairs: double_starred_kvpair (',' double_starred_kvpair)* ','?;

double_starred_kvpair
    : '**' bitwise_or
    | kvpair;

kvpair: expression ':' expression;

// Comprehensions & Generators
// ---------------------------

for_if_clauses
    : for_if_clause+;

for_if_clause
    : 'async'? 'for' star_targets 'in' disjunction ('if' disjunction )*
    ;

listcomp
    : '[' named_expression for_if_clauses ']';

setcomp
    : LBRACE named_expression for_if_clauses RBRACE;

genexp
    : '(' ( assignment_expression | expression) for_if_clauses ')';

dictcomp
    : LBRACE kvpair for_if_clauses RBRACE;

// FUNCTION CALL ARGUMENTS
// =======================

arguments
    : args ','?;

args
    : (starred_expression | ( assignment_expression | expression)) (',' (starred_expression | ( assignment_expression | expression)))* (',' kwargs )?
    | kwargs;

kwargs
    : kwarg_or_starred (',' kwarg_or_starred)* (',' kwarg_or_double_starred (',' kwarg_or_double_starred)*)?
    | kwarg_or_double_starred (',' kwarg_or_double_starred)*
    ;

starred_expression
    : '*' expression;

kwarg_or_starred
    : name '=' expression
    | starred_expression;

kwarg_or_double_starred
    : name '=' expression
    | '**' expression;

// ASSIGNMENT TARGETS
// ==================

// Generic targets
// ---------------

// NOTE: star_targets may contain *bitwise_or, targets may not.
star_targets
    : star_target (',' star_target )* ','?
    ;

star_targets_list_seq: star_target (',' star_target)* ','?;

star_targets_tuple_seq
    : star_target (',' | (',' star_target )+ ','?)
    ;

star_target
    : '*' (star_target)
    | target_with_star_atom;

target_with_star_atom
    : t_primary ('.' name | '[' slices ']')
    | star_atom
    ;

star_atom
    : name
    | '(' target_with_star_atom ')'
    | '(' star_targets_tuple_seq? ')'
    | '[' star_targets_list_seq? ']';

single_target
    : single_subscript_attribute_target
    | name
    | '(' single_target ')';

single_subscript_attribute_target
    : t_primary ('.' name | '[' slices ']')
    ;

t_primary
    : t_primary ('.' name | '[' slices ']' | genexp | '(' arguments? ')')
    | atom
    ;





// Targets for del statements
// --------------------------

del_targets: del_target (',' del_target)* ','?;

del_target
    : t_primary ('.' name | '[' slices ']')
    | del_t_atom
    ;

del_t_atom
    : name
    | '(' del_target ')'
    | '(' del_targets? ')'
    | '[' del_targets? ']';

// TYPING ELEMENTS
// ---------------


// type_expressions allow */** but ignore them
type_expressions
    : expression (',' expression)* (',' ('*' expression (',' '**' expression)? | '**' expression))?
    | '*' expression (',' '**' expression)?
    | '**' expression
    ;



func_type_comment
    : NEWLINE TYPE_COMMENT   // Must be followed by indented block
    | TYPE_COMMENT;

// *** related to soft keywords: https://docs.python.org/3.13/reference/lexical_analysis.html#soft-keywords
name_except_underscore
    : NAME // ***** The NAME token can be used only in this rule *****
    | NAME_OR_TYPE
    | NAME_OR_MATCH
    | NAME_OR_CASE
    ;

// ***** Always use name rule instead of NAME token in this grammar *****
name: NAME_OR_WILDCARD | name_except_underscore;

// ========================= END OF THE GRAMMAR ===========================
