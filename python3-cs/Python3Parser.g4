/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2014 by Bart Kiers
 *
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use,
 * copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following
 * conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
 * OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
 * HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
 * WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
 * OTHER DEALINGS IN THE SOFTWARE.
 *
 * Project      : python3-parser; an ANTLR4 grammar for Python 3
 *                https://github.com/bkiers/python3-parser
 * Developed by : Bart Kiers, bart@big-o.nl
 */
parser grammar Python3Parser;

options { tokenVocab=Python3Lexer; }

root
    : single_input
    | file_input
    | eval_input;

single_input: NEWLINE | simple_stmt | compound_stmt NEWLINE;
file_input: (NEWLINE | stmt)* EOF;
eval_input: testlist NEWLINE* EOF;

decorator: AT dotted_name ( OPEN_PAREN (arglist)? CLOSE_PAREN )? NEWLINE;
decorators: decorator+;
decorated: decorators (classdef | funcdef);

funcdef: (ASYNC)? DEF NAME parameters (func_annotation)? COLON suite;
func_annotation: ARROW test;

parameters: OPEN_PAREN (typedargslist)? CLOSE_PAREN;
typedargslist
    : (def_parameters COMMA)? args (COMMA def_parameters)? (COMMA kwargs)?
    |(def_parameters COMMA)? kwargs
    | def_parameters;
args: STAR named_parameter;
kwargs: POWER named_parameter;
def_parameters: def_parameter (COMMA def_parameter)*;
vardef_parameters: vardef_parameter (COMMA vardef_parameter)*;
def_parameter: named_parameter (ASSIGN test)?;
vardef_parameter: NAME (ASSIGN test)?;
named_parameter: NAME (COLON test)?;
varargslist
    : (vardef_parameters COMMA)? varargs (COMMA vardef_parameters)? (COMMA varkwargs)
    | vardef_parameters;
    
varargs: STAR NAME;
varkwargs: POWER NAME;
vfpdef: NAME;
stmt: simple_stmt | compound_stmt;
simple_stmt: small_stmt (SEMI_COLON small_stmt)* (SEMI_COLON)? (NEWLINE | EOF);
small_stmt: (expr_stmt | del_stmt | pass_stmt | flow_stmt |
             import_stmt | global_stmt | nonlocal_stmt | assert_stmt);

expr_stmt: testlist_star_expr | annassign | augassign | assign;

assign: testlist_star_expr (ASSIGN (yield_expr|testlist_star_expr))*;

annassign: testlist_star_expr COLON test (ASSIGN test)? (yield_expr|testlist);

testlist_star_expr: (test|star_expr) (COMMA (test|star_expr))* (COMMA)?;

augassign: testlist_star_expr op=('+=' | '-=' | '*=' | '@=' | '/=' | '%=' | '&=' | '|=' | '^=' |
              '<<=' | '>>=' | '**=' | '//=') (yield_expr|testlist);

// For normal and annotated assignments, additional restrictions enforced by the interpreter
del_stmt: DEL exprlist;
pass_stmt: PASS;
flow_stmt: break_stmt | continue_stmt | return_stmt | raise_stmt | yield_stmt;
break_stmt: BREAK;
continue_stmt: CONTINUE;
return_stmt: RETURN (testlist)?;
yield_stmt: yield_expr;
raise_stmt: RAISE (test (FROM test)?)?;
import_stmt: import_name | import_from;
import_name: IMPORT dotted_as_names;
// note below: the ('.' | '...') is necessary because '...' is tokenized as ELLIPSIS
import_from: (FROM ((DOT | ELLIPSIS)* dotted_name | (DOT | ELLIPSIS)+)
              IMPORT (STAR | OPEN_PAREN import_as_names CLOSE_PAREN | import_as_names));
import_as_name: NAME (AS NAME)?;
dotted_as_name: dotted_name (AS NAME)?;
import_as_names: import_as_name (COMMA import_as_name)* (COMMA)?;
dotted_as_names: dotted_as_name (COMMA dotted_as_name)*;
dotted_name
  : dotted_name DOT dotted_name
  | NAME;
global_stmt: GLOBAL NAME (COMMA NAME)*;
nonlocal_stmt: NONLOCAL NAME (COMMA NAME)*;
assert_stmt: ASSERT test (COMMA test)?;

compound_stmt: if_stmt | while_stmt | for_stmt | try_stmt | with_stmt | funcdef | classdef | decorated | async_stmt;
async_stmt: ASYNC (with_stmt | for_stmt);
if_stmt: IF test COLON suite (elif_clause)* (else_clause)?;
elif_clause: ELIF test COLON suite;
while_stmt: WHILE test COLON suite (else_clause)?;
for_stmt: FOR exprlist IN testlist COLON suite (else_clause)?;
try_stmt: (TRY COLON suite
           ((except_clause+ else_clause? finaly_clause?) 
            |finaly_clause));
with_stmt: WITH with_item (COMMA with_item)*  COLON suite;
with_item: test (AS expr)?;
// NB compile.c makes sure that the default except clause is last
except_clause: EXCEPT (test (AS NAME)?)? COLON suite;
else_clause: ELSE COLON suite;
finaly_clause: FINALLY COLON suite;
suite: simple_stmt | NEWLINE INDENT stmt+ DEDENT;

test: logical_test (IF logical_test ELSE test)? | lambdef;
test_nocond: logical_test | lambdef_nocond;
lambdef: LAMBDA (varargslist)? COLON test;
lambdef_nocond: LAMBDA (varargslist)? COLON test_nocond;
logical_test
    : logical_test op=OR logical_test
    | logical_test op=AND logical_test
    | NOT logical_test
    | comparison;    
// <> isn't actually a valid comparison operator in Python. It's here for the
// sake of a __future__ import described in PEP 401 (which really works :-)
comparison
    : comparison op=(LESS_THAN|GREATER_THAN|EQUALS|GT_EQ|LT_EQ|NOT_EQ_1|NOT_EQ_1|IN|IS|NOT) comparison
    | comparison (NOT IN | IS NOT) comparison
    | expr;
star_expr: STAR expr;
expr
    : expr op=OR_OP expr
    | expr op=XOR expr
    | expr op=AND_OP expr
    | expr op=(LEFT_SHIFT | RIGHT_SHIFT) expr
    | expr op=(ADD | MINUS) expr
    | expr op=(STAR|'@'|'/'|'%'|'//') expr
    | expr op=('+'|'-'|'~') expr
    | expr op=POWER expr
    | expr op=('+'|'-'|'~') expr
    | atom_expr;
atom_expr: (AWAIT)? atom trailer*;
atom: (OPEN_PAREN (yield_expr|testlist_comp)? CLOSE_PAREN |
       OPEN_BRACK (testlist_comp)? CLOSE_BRACK |
       OPEN_BRACE (dictorsetmaker)? CLOSE_BRACE |
       dotted_name | NUMBER | STRING+ | ELLIPSIS | NONE | TRUE | FALSE);
testlist_comp: (test|star_expr) ( comp_for | (COMMA (test|star_expr))* (COMMA)? );
trailer: OPEN_PAREN (arglist)? CLOSE_PAREN | OPEN_BRACK subscriptlist CLOSE_BRACK;
subscriptlist: subscript (COMMA subscript)* (COMMA)?;
subscript: test | (test)? COLON (test)? (sliceop)?;
sliceop: COLON (test)?;
exprlist: (expr|star_expr) (COMMA (expr|star_expr))* (COMMA)?;
testlist: test (COMMA test)* (COMMA)?;
dictorsetmaker: ( ((test COLON test | POWER expr)
                   (comp_for | (COMMA (test COLON test | POWER expr))* (COMMA)?)) |
                  ((test | star_expr)
                   (comp_for | (COMMA (test | star_expr))* (COMMA)?)) );

classdef: CLASS NAME (OPEN_PAREN (arglist)? CLOSE_PAREN)? COLON suite;

arglist: argument (COMMA argument)*  (COMMA)?;

// The reason that keywords are test nodes instead of NAME is that using NAME
// results in an ambiguity. ast.c makes sure it's a NAME.
// "test ASSIGN test" is really "keyword ASSIGN test", but we have no such token.
// These need to be in a single rule to avoid grammar that is ambiguous
// to our LL(1) parser. Even though 'test' includes '*expr' in star_expr,
// we explicitly match STAR here, too, to give it proper precedence.
// Illegal combinations and orderings are blocked in ast.c:
// multiple (test comp_for) arguments are blocked; keyword unpackings
// that precede iterable unpackings are blocked; etc.
argument: ( test (comp_for)? |
            test ASSIGN test |
            POWER test |
            STAR test );

comp_iter: comp_for | comp_if;
comp_for: (ASYNC)? FOR exprlist IN logical_test (comp_iter)?;
comp_if: IF test_nocond (comp_iter)?;

// not used in grammar, but may appear in "node" passed from Parser to Compiler
encoding_decl: NAME;

yield_expr: YIELD (yield_arg)?;
yield_arg: FROM test | testlist;