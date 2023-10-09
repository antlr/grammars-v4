/*

MIT license

Author: Ken Domino, October 2023

Based on previous work of: Kazunori Sakamoto, Alexander Alexeev

*/

parser grammar  LuaParser;

options { tokenVocab = LuaLexer; }

start_: chunk EOF;

chunk: block ;

block: stat* retstat? ;

stat
    : ';'
    | varlist '=' explist
    | functioncall
    | label
    | 'break'
    | 'goto' NAME
    | 'do' block 'end'
    | 'while' exp 'do' block 'end'
    | 'repeat' block 'until' exp
    | 'if' exp 'then' block ('elseif' exp 'then' block)* ('else' block)? 'end'
    | 'for' NAME '=' exp ',' exp (',' exp)? 'do' block 'end'
    | 'for' namelist 'in' explist 'do' block 'end'
    | 'function' funcname funcbody
    | 'local' 'function' NAME funcbody
    | 'local' attnamelist ('=' explist)?
    ;

attnamelist
    : NAME attrib (',' NAME attrib)*
    ;

attrib
    : ('<' NAME '>')?
    ;

retstat
    : ('return' explist? | 'break' | 'continue') ';'?
    ;

label
    : '::' NAME '::'
    ;

funcname
    : NAME ('.' NAME)* (':' NAME)?
    ;

varlist
    : var (',' var)*
    ;

namelist
    : NAME (',' NAME)*
    ;

explist
    : exp (',' exp)*
    ;

exp
    : 'nil'
    | 'false'
    | 'true'
    | number
    | string
    | '...'
    | functiondef
    | prefixexp
    | tableconstructor
    | <assoc=right> exp ('^') exp
    | ('not' | '#' | '-' | '~') exp
    | exp ('*' | '/' | '%' | '//') exp
    | exp ('+' | '-') exp
    | <assoc=right> exp ('..') exp
    | exp ('<' | '>' | '<=' | '>=' | '~=' | '==') exp
    | exp ('and') exp
    | exp ('or') exp
    | exp ('&' | '|' | '~' | '<<' | '>>') exp
    ;

// var ::=  Name | prefixexp '[' exp ']' | prefixexp '.' Name 
var
    : NAME
    | prefixexp ('[' exp ']' | '.' NAME)
    ;

// prefixexp ::= var | functioncall | '(' exp ')'
prefixexp
    :
    NAME ('[' exp ']' | '.' NAME)*
    | functioncall ('[' exp ']' | '.' NAME)*
    | '(' exp ')' ('[' exp ']' | '.' NAME)*
    ;

// functioncall ::=  prefixexp args | prefixexp ':' Name args;
functioncall:
    NAME ('[' exp ']' | '.' NAME)* args
    | functioncall ('[' exp ']' | '.' NAME)* args
    | '(' exp ')' ('[' exp ']' | '.' NAME)* args
    | NAME ('[' exp ']' | '.' NAME)*  ':' NAME args
    | functioncall ('[' exp ']' | '.' NAME)*  ':' NAME args
    | '(' exp ')' ('[' exp ']' | '.' NAME)*  ':' NAME args
    ;

args
    : '(' explist? ')' | tableconstructor | string
    ;

functiondef
    : 'function' funcbody
    ;

funcbody
    : '(' parlist ')' block 'end'
    ;

/* lparser.c says "is 'parlist' not empty?"
 * That code does so by checking la(1) == ')'.
 * This means that parlist can derive empty.
 */
parlist
    : namelist (',' '...')?
    | '...'
    |
    ;

tableconstructor
    : '{' fieldlist? '}'
    ;

fieldlist
    : field (fieldsep field)* fieldsep?
    ;

field
    : '[' exp ']' '=' exp | NAME '=' exp | exp
    ;

fieldsep
    : ',' | ';'
    ;

number
    : INT | HEX | FLOAT | HEX_FLOAT
    ;

string
    : NORMALSTRING | CHARSTRING | LONGSTRING
    ;
