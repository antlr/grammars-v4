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
}

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
    | prefixexp
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

// Defines the most basic elements that can start a prefix expression chain
primaryexp
    : NAME
    | '(' exp ')'
    | functiondef  // If 'function() ... end' can be directly called/indexed
    // You might also consider adding 'string', 'tableconstructor', 'number',
    // 'nil', 'true', 'false', '...' here if they can start a chain
    // (e.g., "foo":sub(), {a=1}.a, (123):tostring() via metatables)
    ;

// This is the core rule for handling chained expressions (vars, calls, indexing, member access).
prefixexp
    : primaryexp
      (   args             // Suffix for function call, e.g., primaryexp()
        | ':' NAME args    // Suffix for method call, e.g., primaryexp:name()
        | '[' exp ']'      // Suffix for table indexing, e.g., primaryexp[exp]
        | '.' NAME         // Suffix for member access, e.g., primaryexp.name
      )*
    ;

// The 'var' rule, used for the left-hand side of assignments (varlist).
// A 'var' is a path that can be assigned to. It's a primary expression
// (NAME or parenthesized expression) followed by a chain of *only*
// indexing or member access operations.
var
    : ( NAME | '(' exp ')' ) // Base of the var
      (   '[' exp ']'        // Indexing suffix
        | '.' NAME           // Member access suffix
      )* // Zero or more of these suffixes
    // This definition means:
    // - NAME itself is a var.
    // - NAME.foo, NAME[exp] are vars.
    // - (exp).foo, (exp)[exp] are vars.
    // - (exp) itself is NOT a var by this rule alone (it needs a suffix to become one here, or NAME).
    // - Critically, NAME() is a prefixexp but not matched by this `var` rule if `args` is not a suffix here.
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