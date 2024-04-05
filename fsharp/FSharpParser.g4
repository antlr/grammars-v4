parser grammar FSharpParser;

options {
    tokenVocab=FSharpLexer;
}

// rules
dot:DOT;

identifier: IDENTIFIER;

dotIentifier: identifier (dot identifier)*;

int: INT;

float: FLOAT;

unit: UNIT;

bool: BOOL;

char: CHAR;

underscore: MISSING_ARG MISSING_ARG?;

interpolationSign: INTERPOLATIONSIGN; // don't add to expr

dollar: DOLLAR; //don't add

string: STRING;

interpolated_string: INTERPOLATED_STRING;

attribute: OPEN_BRACKET LESS dotIentifier GREATER CLOSE_BRACKET;

generic: LESS dotIentifier GREATER;

round_brackets: OPEN_PAREN (expression+ COMMA?)+ CLOSE_PAREN;

rec: REC;

public: PUBLIC;

private: PRIVATE;

internal: INTERNAL;

mutable: MUTABLE;

// let: LET ;

let_fun: LET (mutable|internal|rec|public|private)* identifier (mutable|internal|rec|public|private)* ((identifier typezation?)|unit|round_brackets)+
    typezation? equal expression;

let_var: LET (mutable|internal|rec|public|private)* identifier typezation? (equal expression)?;

fun: FUN expression+ RIGHT_ARROW expression;

fun_type: dotIentifier (RIGHT_ARROW dotIentifier)+;

typezation: COLON (round_brackets|dotIentifier);

if_then_elif_else: IF expression+ THEN expression+ 
    (ELIF expression+ THEN expression+)*
    (ELSE expression)?;

while_do: WHILE expression+ DO; 

for: FOR expression+ (TO|DOWNTO|IN) expression+ DO;

add: ADD;

mul: MUL;

div: DIV;

minus: MINUS;

pow: POW;

mod: MOD;

not_equal: NOT_EQUAL;

less: LESS;

less_equal: LESS_EQUAL;

greater: GREATER;

greater_equal: GREATER_EQUAL;

equal: EQUAL;

and: AND;

or: OR;

lshift: LSHIFT;

rshift: RSHIFT;

log_mul: LOG_MUL;

log_add: LOG_ADD;

log_xor: LOG_XOR;

log_not: LOG_NOT;
not: NOT;

pipe: PIPE;

compos: COMPOS;

assign: ASSIGN;

type: TYPE expression+ EQUAL;

module: MODULE;

open: OPEN dotIentifier;

namespace: NAMESPACE ;

class: CLASS expression+ END;

do: DO;

new: NEW expression;

when: WHEN;

// then: THEN;

seq: SEQ OPEN_BRACE expression+ (SEMICOLON? expression+)* CLOSE_BRACE;

list: OPEN_BRACKET expression+ (SEMICOLON? expression+)* CLOSE_BRACKET;

array: OPEN_BRACKET VERTICAL_LINE expression+ (SEMICOLON? expression+)* VERTICAL_LINE CLOSE_BRACKET;

map: MAP OPEN_BRACKET expression+ COMMA expression+ (SEMICOLON? expression+ COMMA expression+)* CLOSE_BRACKET;

generator: (INT|FLOAT) DOTDOT (INT|FLOAT) ((DOTDOT) (INT|FLOAT))?;

set: SET expression;

async_rule: ASYNC OPEN_BRACE expression* CLOSE_BRACE;

task: TASK OPEN_BRACE expression* CLOSE_BRACE;

exclamation_mark: EXCLAMATION_MARK;

match_with: MATCH expression+ WITH
(VERTICAL_LINE expression+ RIGHT_ARROW expression+)+;

try_with_finally: TRY expression* (WITH (VERTICAL_LINE expression* RIGHT_ARROW expression*)*)? 
   (FINALLY expression)?; 

use: USE expression* EQUAL expression;

using: USING round_brackets expression;

raise: RAISE;

reraise: RERAISE;

failwith: FAILWITH;

invalidArg: INVALIDARG;

exception_of: EXCEPTION expression* OF expression;

member: MEMBER (THIS|MISSING_ARG+) dot identifier (identifier|unit)* equal expression;

val: VAL mutable? (internal|public|private)? dotIentifier COLON dotIentifier;

struct: STRUCT expression* END;

with_get_set: (WITH ((private|internal|public)? GET UNIT EQUAL expression) 
    (AND ((private|internal|public)? SET expression* EQUAL expression)?) 
    | (WITH (private|internal|public)? expression* EQUAL expression));

tuple: OPEN_PAREN expression (COMMA expression)+ CLOSE_PAREN;

with: WITH;

record: OPEN_BRACE (((expression COLON dotIentifier) *)| (expression* with expression*)) CLOSE_BRACE;

of:OF;

enum: (VERTICAL_LINE dotIentifier (equal|of) dotIentifier)+;

inherit: INHERIT;

default: DEFAULT;

override: OVERRIDE;

abstract: ABSTRACT;

base: BASE;

colon_q: COLON_Q;

interface: INTERFACE;


expression: dotIentifier
            |dot
            |int
            |float
            |bool
            |char
            |unit
            |underscore
            // |missing_arg
            |string
            |attribute
            |generic
            |let_fun
            |let_var
            |round_brackets
            |rec
            |public
            |private
            |internal
            |mutable
            |fun
            |typezation
            |if_then_elif_else
            |while_do
            |for
            |add
            |mul
            |div
            |minus
            |pow
            |mod
            |not_equal
            |less
            |less_equal
            |greater
            |greater_equal
            |equal
            |and
            |or
            |lshift
            |rshift
            |log_mul
            |log_add
            |log_xor
            |log_not
            |not
            |pipe
            |compos
            |assign
            |fun_type
            |type
            |module
            |open
            |namespace
            |class
            |do
            |new
            // |then
            |seq
            |generator
            |list
            |array
            |map
            |async_rule
            |task
            |exclamation_mark
            |match_with
            |try_with_finally
            |use
            |using
            |raise
            |reraise
            |failwith
            |invalidArg
            |exception_of
            |member
            |val
            |struct
            |with_get_set
            |tuple
            |with
            |record
            |enum
            |inherit
            |default
            |override
            |abstract
            |base
            |colon_q
            |interpolated_string
            |interface
            |when
            ;

exprs: expression* EOF;