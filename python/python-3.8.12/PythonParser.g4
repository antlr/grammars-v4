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
 * Project      : an ANTLR4 parser grammar by the official Python 3.8.12 grammar
 *                https://github.com/RobEin/ANTLR4-parser-for-Python-3.8.12
 * Developed by : Robert Einhorn, robert.einhorn.hu@gmail.com
 */

parser grammar PythonParser; // Python 3.8.12    https://docs.python.org/3.8/reference/grammar.html
options { tokenVocab=PythonLexer; superClass=PythonParserBase; }
// ANTLR4 grammar for Python


single_input: NEWLINE | simple_stmt | compound_stmt NEWLINE;
file_input: (NEWLINE | stmt)* EOF;
eval_input: testlist NEWLINE* EOF;

decorator: '@' dotted_name ( '(' arglist? ')' )? NEWLINE;
decorators: decorator+;
decorated: decorators (classdef | funcdef | async_funcdef);

async_funcdef: ASYNC funcdef;
funcdef: 'def' NAME parameters ('->' test)? ':' TYPE_COMMENT? func_body_suite;

parameters: '(' typedargslist? ')';

// The following definition for typedarglist is equivalent to this set of rules:
//
//     arguments = argument (',' TYPE_COMMENT? argument)*
//     argument = tfpdef ('=' test)?
//     kwargs = '**' tfpdef ','? TYPE_COMMENT?
//     args = '*' tfpdef?
//     kwonly_kwargs = (',' TYPE_COMMENT? argument)* (TYPE_COMMENT | (',' TYPE_COMMENT? kwargs?)?)
//     args_kwonly_kwargs = args kwonly_kwargs | kwargs
//     poskeyword_args_kwonly_kwargs = arguments ( TYPE_COMMENT | (',' TYPE_COMMENT? args_kwonly_kwargs?)?)
//     typedargslist_no_posonly  = poskeyword_args_kwonly_kwargs | args_kwonly_kwargs
//     typedarglist = (arguments ',' TYPE_COMMENT? '/' (',' (TYPE_COMMENT? typedargslist_no_posonly)?)?)|(typedargslist_no_posonly)"
//
// It needs to be fully expanded to allow our LL(1) parser to work on it.

typedargslist: (
  (tfpdef ('=' test)? (',' TYPE_COMMENT? tfpdef ('=' test)?)* ',' TYPE_COMMENT? '/' (',' ( TYPE_COMMENT? tfpdef ('=' test)? (
        ',' TYPE_COMMENT? tfpdef ('=' test)?)* (TYPE_COMMENT | (',' TYPE_COMMENT? (
        '*' tfpdef? (',' TYPE_COMMENT? tfpdef ('=' test)?)* (TYPE_COMMENT | (',' TYPE_COMMENT? ('**' tfpdef ','? TYPE_COMMENT?)?)?)
      | '**' tfpdef ','? TYPE_COMMENT?)?)?)
  | '*' tfpdef? (',' TYPE_COMMENT? tfpdef ('=' test)?)* (TYPE_COMMENT | (',' TYPE_COMMENT? ('**' tfpdef ','? TYPE_COMMENT?)?)?)
  | '**' tfpdef ','? TYPE_COMMENT?)?)? )
|  (tfpdef ('=' test)? (',' TYPE_COMMENT? tfpdef ('=' test)?)* (TYPE_COMMENT | (',' TYPE_COMMENT? (
   '*' tfpdef? (',' TYPE_COMMENT? tfpdef ('=' test)?)* (TYPE_COMMENT | (',' TYPE_COMMENT? ('**' tfpdef ','? TYPE_COMMENT?)?)?)
  | '**' tfpdef ','? TYPE_COMMENT?)?)?)
  | '*' tfpdef? (',' TYPE_COMMENT? tfpdef ('=' test)?)* (TYPE_COMMENT | (',' TYPE_COMMENT? ('**' tfpdef ','? TYPE_COMMENT?)?)?)
  | '**' tfpdef ','? TYPE_COMMENT?)
);
tfpdef: NAME (':' test)?;

// The following definition for varargslist is equivalent to this set of rules:
//
//     arguments = argument (',' argument )*
//     argument = vfpdef ('=' test)?
//     kwargs = '**' vfpdef ','?
//     args = '*' vfpdef?
//     kwonly_kwargs = (',' argument )* (',' kwargs?)?
//     args_kwonly_kwargs = args kwonly_kwargs | kwargs
//     poskeyword_args_kwonly_kwargs = arguments (',' args_kwonly_kwargs?)?
//     vararglist_no_posonly = poskeyword_args_kwonly_kwargs | args_kwonly_kwargs
//     varargslist = arguments ',' '/' [','[(vararglist_no_posonly)]] | (vararglist_no_posonly)
//
// It needs to be fully expanded to allow our LL(1) parser to work on it.

varargslist: vfpdef ('=' test )?(',' vfpdef ('=' test)?)* ',' '/' (',' ( (vfpdef ('=' test)? (',' vfpdef ('=' test)?)* (',' (
        '*' vfpdef? (',' vfpdef ('=' test)?)* (',' ('**' vfpdef ','?)?)?
      | '**' vfpdef ','?)?)?
  | '*' vfpdef? (',' vfpdef ('=' test)?)* (',' ('**' vfpdef ','?)?)?
  | '**' vfpdef ','?) )?)? | (vfpdef ('=' test)? (',' vfpdef ('=' test)?)* (',' (
        '*' vfpdef? (',' vfpdef ('=' test)?)* (',' ('**' vfpdef ','?)?)?
      | '**' vfpdef ','?)?)?
  | '*' vfpdef? (',' vfpdef ('=' test)?)* (',' ('**' vfpdef ','?)?)?
  | '**' vfpdef ','?
);
vfpdef: NAME;

stmt: simple_stmt | compound_stmt;
simple_stmt: small_stmt (';' small_stmt)* ';'? NEWLINE;
small_stmt: (expr_stmt | del_stmt | pass_stmt | flow_stmt |
             import_stmt | global_stmt | nonlocal_stmt | assert_stmt);
expr_stmt: testlist_star_expr (annassign | augassign (yield_expr|testlist) |
                     (('=' (yield_expr|testlist_star_expr))+ TYPE_COMMENT?)? );
annassign: ':' test ('=' (yield_expr|testlist_star_expr))?;
testlist_star_expr: (test|star_expr) (',' (test|star_expr))* ','?;
augassign: ('+=' | '-=' | '*=' | '@=' | '/=' | '%=' | '&=' | '|=' | '^=' |
            '<<=' | '>>=' | '**=' | '//=');
// For normal and annotated assignments, additional restrictions enforced by the interpreter
del_stmt: 'del' exprlist;
pass_stmt: 'pass';
flow_stmt: break_stmt | continue_stmt | return_stmt | raise_stmt | yield_stmt;
break_stmt: 'break';
continue_stmt: 'continue';
return_stmt: 'return' testlist_star_expr?;
yield_stmt: yield_expr;
raise_stmt: 'raise' (test ('from' test)?)?;
import_stmt: import_name | import_from;
import_name: 'import' dotted_as_names;
// note below: the ('.' | '...') is necessary because '...' is tokenized as ELLIPSIS
import_from: ('from' (('.' | '...')* dotted_name | ('.' | '...')+)
              'import' ('*' | '(' import_as_names ')' | import_as_names));
import_as_name: NAME ('as' NAME)?;
dotted_as_name: dotted_name ('as' NAME)?;
import_as_names: import_as_name (',' import_as_name)* ','?;
dotted_as_names: dotted_as_name (',' dotted_as_name)*;
dotted_name: NAME ('.' NAME)*;
global_stmt: 'global' NAME (',' NAME)*;
nonlocal_stmt: 'nonlocal' NAME (',' NAME)*;
assert_stmt: 'assert' test (',' test)?;

compound_stmt: if_stmt | while_stmt | for_stmt | try_stmt | with_stmt | funcdef | classdef | decorated | async_stmt;
async_stmt: ASYNC (funcdef | with_stmt | for_stmt);
if_stmt: 'if' namedexpr_test ':' suite ('elif' namedexpr_test ':' suite)* ('else' ':' suite)?;
while_stmt: 'while' namedexpr_test ':' suite ('else' ':' suite)?;
for_stmt: 'for' exprlist 'in' testlist ':' TYPE_COMMENT? suite ('else' ':' suite)?;
try_stmt: ('try' ':' suite
           ((except_clause ':' suite)+
            ('else' ':' suite)?
            ('finally' ':' suite)? |
           'finally' ':' suite));
with_stmt: 'with' with_item (',' with_item)*  ':' TYPE_COMMENT? suite;
with_item: test ('as' expr)?;
// NB compile.c makes sure that the default except clause is last
except_clause: 'except' (test ('as' NAME)?)?;
suite: simple_stmt | NEWLINE INDENT stmt+ DEDENT;

namedexpr_test: test (':=' test)?;
test: or_test ('if' or_test 'else' test)? | lambdef;
test_nocond: or_test | lambdef_nocond;
lambdef: 'lambda' varargslist? ':' test;
lambdef_nocond: 'lambda' varargslist? ':' test_nocond;
or_test: and_test ('or' and_test)*;
and_test: not_test ('and' not_test)*;
not_test: 'not' not_test | comparison;
comparison: expr (comp_op expr)*;
// <> isn't actually a valid comparison operator in Python. It's here for the
// sake of a __future__ import described in PEP 401 (which really works :-)
comp_op: '<'|'>'|'=='|'>='|'<='|'<>'|'!='|'in'|'not' 'in'|'is'|'is' 'not';
star_expr: '*' expr;
expr: xor_expr ('|' xor_expr)*;
xor_expr: and_expr ('^' and_expr)*;
and_expr: shift_expr ('&' shift_expr)*;
shift_expr: arith_expr (('<<'|'>>') arith_expr)*;
arith_expr: term (('+'|'-') term)*;
term: factor (('*'|'@'|'/'|'%'|'//') factor)*;
factor: ('+'|'-'|'~') factor | power;
power: atom_expr ('**' factor)?;
atom_expr: AWAIT? atom trailer*;
atom: ('(' (yield_expr|testlist_comp)? ')' |
       '[' testlist_comp? ']' |
       '{' dictorsetmaker? '}' |
       NAME | NUMBER | STRING+ | '...' | 'None' | 'True' | 'False');
testlist_comp: (namedexpr_test|star_expr) ( comp_for | (',' (namedexpr_test|star_expr))* ','? );
trailer: '(' arglist? ')' | '[' subscriptlist ']' | '.' NAME;
subscriptlist: subscript (',' subscript)* ','?;
subscript: test | test? ':' test? sliceop?;
sliceop: ':' test?;
exprlist: (expr|star_expr) (',' (expr|star_expr))* ','?;
testlist: test (',' test)* ','?;
dictorsetmaker: ( ((test ':' test | '**' expr)
                   (comp_for | (',' (test ':' test | '**' expr))* ','?)) |
                  ((test | star_expr)
                   (comp_for | (',' (test | star_expr))* ','?)) );

classdef: 'class' NAME ('(' arglist? ')')? ':' suite;

arglist: argument (',' argument)*  ','?;

// The reason that keywords are test nodes instead of NAME is that using NAME
// results in an ambiguity. ast.c makes sure it's a NAME.
// "test '=' test" is really "keyword '=' test", but we have no such token.
// These need to be in a single rule to avoid grammar that is ambiguous
// to our LL(1) parser. Even though 'test' includes '*expr' in star_expr,
// we explicitly match '*' here, too, to give it proper precedence.
// Illegal combinations and orderings are blocked in ast.c:
// multiple (test comp_for) arguments are blocked; keyword unpackings
// that precede iterable unpackings are blocked; etc.
argument: ( test comp_for? |
            test ':=' test |
            test '=' test |
            '**' test |
            '*' test );

comp_iter: comp_for | comp_if;
sync_comp_for: 'for' exprlist 'in' or_test comp_iter?;
comp_for: ASYNC? sync_comp_for;
comp_if: 'if' test_nocond comp_iter?;

// not used in grammar, but may appear in "node" passed from Parser to Compiler
encoding_decl: NAME;

yield_expr: 'yield' yield_arg?;
yield_arg: 'from' test | testlist_star_expr;

// the TYPE_COMMENT in suites is only parsed for funcdefs,
// but can't go elsewhere due to ambiguity
func_body_suite: simple_stmt | NEWLINE (TYPE_COMMENT NEWLINE)? INDENT stmt+ DEDENT;

func_type_input: func_type NEWLINE* EOF;
func_type: '(' typelist? ')' '->' test;
// typelist is a modified typedargslist (see above)
typelist: (test (',' test)* (','
       ('*' test? (',' test)* (',' '**' test)? | '**' test)?)?
     |  '*' test? (',' test)* (',' '**' test)? | '**' test);
