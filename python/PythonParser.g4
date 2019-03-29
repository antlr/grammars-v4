// Header included from Python site:
/*
 * Grammar for Python
 *
 * Note:  Changing the grammar specified in this file will most likely
 *        require corresponding changes in the parser module
 *        (../Modules/parsermodule.c).  If you can't make the changes to
 *        that module yourself, please co-ordinate the required changes
 *        with someone who can; ask around on python-dev for help.  Fred
 *        Drake <fdrake@acm.org> will probably be listening there.
 *
 * NOTE WELL: You should also follow all the steps listed in PEP 306,
 * "How to Change Python's Grammar"
 *
 * Start symbols for the grammar:
 *       single_input is a single interactive statement;
 *       file_input is a module or sequence of commands read from an input file;
 *       eval_input is the input for the eval() and input() functions.
 * NB: compound_stmt in single_input is followed by extra NEWLINE!
 */
parser grammar PythonParser;

options { tokenVocab=PythonLexer; }

 root: single_input
    | file_input
    | eval_input;

single_input: NEWLINE | simple_stmt | compound_stmt NEWLINE
    ;
file_input: (NEWLINE | stmt)* EOF
    ;
eval_input: testlist NEWLINE* EOF
    ;


decorator: AT dotted_name ( OPEN_PAREN (arglist)? CLOSE_PAREN )? NEWLINE
    ;
decorators: decorator+
    ;
decorated: decorators (classdef | funcdef)
    ;
funcdef: (ASYNC)? DEF NAME parameters (func_annotation)? COLON suite
    ;
func_annotation: ARROW test
    ;
parameters: OPEN_PAREN (typedargslist)? CLOSE_PAREN
    ;

//python 3 paramters
typedargslist
    : (def_parameters COMMA)? args (COMMA def_parameters)? (COMMA kwargs)?
    |(def_parameters COMMA)? kwargs
    | def_parameters;
args: STAR named_parameter;
kwargs: POWER named_parameter;
def_parameters: def_parameter (COMMA def_parameter)*;
vardef_parameters: vardef_parameter (COMMA vardef_parameter)*;
def_parameter: (named_parameter (ASSIGN test)?) | STAR;
vardef_parameter: NAME (ASSIGN test)?;
named_parameter: NAME (COLON test)?;

//python 2 paramteters
varargslist
    : (vardef_parameters COMMA)? varargs (COMMA vardef_parameters)? (COMMA varkwargs)
    | vardef_parameters;

varargs: STAR NAME;
varkwargs: POWER NAME;
vfpdef: NAME;

stmt: simple_stmt | compound_stmt
    ;
simple_stmt: small_stmt (SEMI_COLON small_stmt)* (SEMI_COLON)? (NEWLINE | EOF)
    ;
small_stmt: (expr_stmt | print_stmt  | del_stmt | pass_stmt | flow_stmt |
             import_stmt | global_stmt | exec_stmt | assert_stmt | nonlocal_stmt)
    ;

//Python 3
nonlocal_stmt: NONLOCAL NAME (COMMA NAME)*;

expr_stmt: testlist_star_expr | annassign | augassign | assign;

// if left expression in assign is bool literal, it's mean that is Python 2 here
assign: testlist_star_expr (ASSIGN (yield_expr|testlist_star_expr))*;

//Only Pytnoh 3 supports annotations for variables
annassign: testlist_star_expr COLON test (ASSIGN (yield_expr|testlist))?;

testlist_star_expr: (test|star_expr) (COMMA (test|star_expr))* (COMMA)?;

augassign: testlist_star_expr op=('+=' | '-=' | '*=' | '@=' | '/=' | '%=' | '&=' | '|=' | '^=' |
              '<<=' | '>>=' | '**=' | '//=') (yield_expr|testlist);
//print_stmt: 'print' ( ( test (COMMA test)* (COMMA)? )? |
//                      '>>' test ( (COMMA test)+ (COMMA)? )? )
//    ;
// python 2
print_stmt: PRINT
            ( ( test (COMMA test)* (COMMA)? ) |
                 '>>' test ( (COMMA test)+ (COMMA)? ) )
    ;
del_stmt: DEL exprlist
    ;
pass_stmt: PASS
    ;
flow_stmt: break_stmt | continue_stmt | return_stmt | raise_stmt | yield_stmt
    ;
break_stmt: BREAK
    ;
continue_stmt: CONTINUE
    ;
return_stmt: RETURN (testlist)?
    ;
yield_stmt: yield_expr
    ;
raise_stmt: RAISE (test (COMMA test (COMMA test)?)?)? (FROM test)?
    ;
import_stmt: import_name | import_from
    ;
import_name: IMPORT dotted_as_names
    ;
import_from: (FROM ((DOT | ELLIPSIS)* dotted_name | (DOT | ELLIPSIS)+)
              IMPORT (STAR | OPEN_PAREN import_as_names CLOSE_PAREN | import_as_names))
    ;
import_as_name: NAME (AS NAME)?
    ;
dotted_as_name: dotted_name (AS NAME)?
    ;
import_as_names: import_as_name (COMMA import_as_name)* (COMMA)?
    ;
dotted_as_names: dotted_as_name (COMMA dotted_as_name)*
    ;
dotted_name
    : dotted_name DOT dotted_name
    | NAME
    ;
global_stmt: GLOBAL NAME (COMMA NAME)*
    ;
exec_stmt: EXEC expr (IN test (COMMA test)?)?
    ;
assert_stmt: ASSERT test (COMMA test)?
    ;
compound_stmt: 
    if_stmt 
    | while_stmt 
    | for_stmt 
    | try_stmt 
    | with_stmt 
    | funcdef 
    | classdef 
    | decorated
    | async_stmt
    ;
async_stmt: ASYNC (funcdef | with_stmt | for_stmt);

if_stmt: IF test COLON suite (elif_clause)* (else_clause)?
    ;
elif_clause: ELIF test COLON suite
    ;    
else_clause: ELSE COLON suite
    ;
while_stmt: WHILE test COLON suite (else_clause)?
    ;
for_stmt: FOR exprlist IN testlist COLON suite (else_clause)?
    ;
try_stmt: (TRY COLON suite
           ((except_clause+ else_clause? finaly_clause?) 
            |finaly_clause))
    ;
finaly_clause: FINALLY COLON suite
    ;
with_stmt: WITH with_item (COMMA with_item)*  COLON suite
    ;
with_item: test (AS expr)?
// NB compile.c makes sure that the default except clause is last
    ;
// Python 2 : EXCEPT test COMMA NAME, Python 3 : EXCEPT test AS NAME
except_clause: EXCEPT (test ((COMMA NAME) | (AS NAME))?)? COLON suite
    ;
suite: simple_stmt | NEWLINE INDENT stmt+ DEDENT
    ;
// Backward compatibility cruft to support:
// [ x for x in lambda: True, lambda: False if x() ]
// even while also allowing:
// lambda x: 5 if x else 2
// (But not a mix of the two)
testlist_safe: old_test ((COMMA old_test)+ (COMMA)?)?
    ;
old_test: logical_test | old_lambdef
    ;
old_lambdef: LAMBDA (varargslist)? COLON old_test
    ;


test: logical_test (IF logical_test ELSE test)? | lambdef
    ;
test_nocond: logical_test | lambdef_nocond
    ;
lambdef_nocond: LAMBDA (varargslist)? COLON test_nocond
    ;

logical_test
    : logical_test op=OR logical_test
    | logical_test op=AND logical_test
    | NOT logical_test
    | comparison
    ;
// <> isn't actually a valid comparison operator in Python. It's here for the
// sake of a __future__ import described in PEP 401 (which really works :-)
comparison
    : comparison op=(LESS_THAN|GREATER_THAN|EQUALS|GT_EQ|LT_EQ|NOT_EQ_1|NOT_EQ_2|IN|IS|NOT) comparison
    | comparison (NOT IN | IS NOT) comparison
    | expr
    ;

star_expr: STAR expr
    ;

expr
    : expr op=OR_OP expr
    | expr op=XOR expr
    | expr op=AND_OP expr
    | expr op=(LEFT_SHIFT | RIGHT_SHIFT) expr
    | expr op=(ADD | MINUS) expr
    | expr op=(STAR|'/'|'%'|'//') expr
    | expr op=('+'|'-') expr
    | expr op=POWER expr
    | op=('+'|'-'|'~') expr
    | atom_expr
    ;

atom_expr: (AWAIT)? atom trailer*
    ;
atom:  (OPEN_PAREN (yield_expr|testlist_comp)? CLOSE_PAREN |
        OPEN_BRACKET (testlist_comp)? CLOSE_BRACKET |
        OPEN_BRACE (dictorsetmaker)? CLOSE_BRACE |
        (REVERSE_QUOTE testlist COMMA? REVERSE_QUOTE) | ELLIPSIS | // tt: added elipses.
        dotted_name | NAME | PRINT | EXEC | NUMBER | MINUS NUMBER | NONE |STRING+)
    ;
    
testlist_comp: (test|star_expr) ( comp_for | (COMMA (test|star_expr))* (COMMA)? )
    ;

lambdef: LAMBDA (varargslist)? COLON test
    ;
trailer: OPEN_PAREN (arglist)? CLOSE_PAREN | '[' subscriptlist ']' | '.' NAME
    ;
subscriptlist: subscript (COMMA subscript)* (COMMA)?
    ;
subscript: ELLIPSIS | test | (test)? COLON (test)? (sliceop)?
    ;
sliceop: COLON (test)?
    ;
exprlist: expr (COMMA expr)* (COMMA)?
    ;
testlist: test (COMMA test)* (COMMA)?
    ;

dictorsetmaker: ( ((test COLON test | POWER expr)
                   (comp_for | (COMMA (test COLON test | POWER expr))* (COMMA)?)) |
                  ((test | star_expr)
                   (comp_for | (COMMA (test | star_expr))* (COMMA)?)) )
    ;

classdef: CLASS NAME (OPEN_PAREN (arglist)? CLOSE_PAREN)? COLON suite
    ;


arglist: argument (COMMA argument)*  (COMMA)?
// The reason that keywords are test nodes instead of NAME is that using NAME
// results in an ambiguity. ast.c makes sure it's a NAME.
    ;
argument: ( test (comp_for)? |
            test ASSIGN test |
            POWER test |
            STAR test );


list_iter: list_for | list_if
    ;
list_for: FOR exprlist IN testlist_safe (list_iter)?
    ;
list_if: IF old_test (list_iter)?
    ;


comp_iter: comp_for | comp_if
    ;
comp_for: FOR exprlist IN logical_test (comp_iter)?
    ;
comp_if: IF old_test (comp_iter)?
    ;

// not used in grammar, but may appear in "node" passed from Parser to Compiler
encoding_decl: NAME
    ;

yield_expr: YIELD (yield_arg)?;
yield_arg: FROM test | testlist;