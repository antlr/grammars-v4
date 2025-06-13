/*

MIT license

Author: Ken Domino, October 2023

Based on previous work of: Kazunori Sakamoto, Alexander Alexeev

*/

// $antlr-format alignTrailingComments true, columnLimit 150, minEmptyLines 1, maxEmptyLinesToKeep 1, reflowComments false, useTab false
// $antlr-format allowShortRulesOnASingleLine false, allowShortBlocksOnASingleLine true, alignSemicolons hanging, alignColons hanging

parser grammar LuaParser;

options {
    tokenVocab = LuaLexer;
    superClass = LuaParserBase;
}

// Insert here @header for parser.

start_
    : chunk EOF
    ;

chunk
    : block
    ;

block
    : stat* retstat?
    ;

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
    : 'return' explist? ';'?
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
    | <assoc = right> exp ('^') exp
    | ('not' | '#' | '-' | '~') exp
    | exp ('*' | '/' | '%' | '//') exp
    | exp ('+' | '-') exp
    | <assoc = right> exp ('..') exp
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
    : { this.IsFunctionCall() }? NAME ( '[' exp ']' | '.' NAME )*
    | functioncall ( '[' exp ']' | '.' NAME )*
    | '(' exp ')' ( '[' exp ']' | '.' NAME )*
    ;

// functioncall ::=  prefixexp args | prefixexp ':' Name args;
functioncall
    :  ( ( NAME | '(' exp ')' ) ( '[' exp ']' | '.' NAME )* ( args | ':' NAME args ) ) ( ( '[' exp ']' | '.' NAME )* ( args | ':' NAME args ) )*
    ;

args
    : '(' explist? ')'
    | tableconstructor
    | string
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
    : '[' exp ']' '=' exp
    | NAME '=' exp
    | exp
    ;

fieldsep
    : ','
    | ';'
    ;

number
    : INT
    | HEX
    | FLOAT
    | HEX_FLOAT
    ;

string
    : NORMALSTRING
    | CHARSTRING
    | LONGSTRING
    ;