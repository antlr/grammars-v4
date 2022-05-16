/*
 [The "BSD licence"]
 Copyright (c) 2022 Boris Zhguchev
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:
 1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.
 3. The name of the author may not be used to endorse or promote products
    derived from this software without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
parser grammar WrenParser;

options { tokenVocab=WrenLexer; }


script: fileAtom+ EOF;

fileAtom
    : class
    | function
    | importModule
    | statement
    | block
    ;

// assignment
assignment: VAR? expression assignOp (expression | assignment+);
assignmentNull: VAR id;
assignOp
    : ASSIGN
    | ADD_ASSIGN
    | SUB_ASSIGN
    | MUL_ASSIGN
    | DIV_ASSIGN
    | AND_ASSIGN
    | OR_ASSIGN
    | XOR_ASSIGN
    | MOD_ASSIGN
    | LSHIFT_ASSIGN
    | RSHIFT_ASSIGN
    | URSHIFT_ASSIGN
    ;


// flow
if: ifCond bodyFlow elseIf* else?;
ifCond: IF LPAREN expression RPAREN;
elseIf: ELSE ifCond bodyFlow;
else : ELSE bodyFlow;

while: WHILE LPAREN (expression | assignment) RPAREN bodyFlow;
for: FOR LPAREN id IN expression RPAREN bodyFlow;

bodyFlow
    : block
    | statement
    ;

// statements
statement
    : expression
    | assignment
    | assignmentNull
    | if
    | while
    | for
    | block
    | return
    ;

lambdaParameters: BITOR (id (COMMA id)*) BITOR;
block: LBRACE lambdaParameters? statement* RBRACE;
return: RETURN expression;

// class
class: attributes? FOREIGN? CLASS  id inheritance? LBRACE classBody RBRACE;
inheritance: IS id ;

// class atributes
attribute
    : simpleAttribute
    | groupAttribute
    ;
attributes: attribute+;
attributeValue: id (ASSIGN atomExpression)?;
simpleAttribute:HASH BANG? attributeValue ;
groupAttribute:HASH BANG? id LPAREN attributeValue (COMMA attributeValue)* RPAREN;

// class body
classBody:(attributes? classBodyTpe? classStatement)*;
classBodyTpe
    : FOREIGN
    | STATIC
    | STATIC FOREIGN
    | FOREIGN STATIC
    ;
// class statement
classStatement
    : function
    | classGetter
    | classSetter
    | classSubscriptGet
    | classSubscriptSet
    | classOpGetter
    | classOpSetter
    | classConstructor
    ;

classConstructor: CONSTRUCT id arguments  block;
operatorGetter
    : SUB
    | TILDE
    | BANG
    | id
    ;
operatorSetter
    : SUB
    | MUL
    | DIV
    | MOD
    | ADD
    | ELLIPSIS_OUT
    | ELLIPSIS_IN
    | LSHIFT
    | RSHIFT
    | BITAND
    | CARET
    | BITOR
    | GT
    | LT
    | EQUAL
    | LE
    | GE
    | NOTEQUAL
    | IS
    | id
    ;
classOpGetter: operatorGetter block;
classOpSetter: operatorSetter oneArgument block;
oneArgument: LPAREN id RPAREN;
subscript: LBRACK enumeration RBRACK;
classSubscriptGet:  subscript block;
classSubscriptSet:  subscript ASSIGN oneArgument block;
classGetter: id block?;
classSetter: id assignmentSetter block;
assignmentSetter:ASSIGN oneArgument;
arguments: LPAREN (id (COMMA id)*)? RPAREN;

function: id arguments block?;

// imports
importModule: IMPORT STRING_LITERAL importVariables?;
importVariable: id (AS id)?;
importVariables: FOR importVariable (COMMA importVariable);

// call
call: callHead (DOT call)*;
callBlock: id block;
callSignature: id LPAREN enumeration? RPAREN;
callHead
    : callSignature
    | callBlock
    | id
    ;
// expressions
expression
    : expression logic
    | expression arithBit
    | expression arithShift
    | expression arithRange
    | expression arithAdd
    | expression arithMul
    | expression DOT call
    | expression IS expression
    | BANG expression
    | LPAREN expression  RPAREN
    | expression elvis
    | atomExpression
    ;
atomExpression
    : id
    | bool
    | char
    | string
    | num
    | null
    | listInit
    | mapInit
    | call
    | range
    | collectionElem
    | BREAK
    | CONTINUE
    | importModule
    | SUB atomExpression
    ;
enumeration: (expression (COMMA expression)*);
pairEnumeration: (expression COLON expression (COMMA expression COLON expression)*);
range:rangeExpression (ELLIPSIS_IN | ELLIPSIS_OUT) rangeExpression;
listInit: LBRACK enumeration? RBRACK;
mapInit: LBRACE pairEnumeration? RBRACE;
elem
    : id
    | string
    | call
    ;
collectionElem: elem listInit ;
rangeExpression
    : id
    | num
    | call
    ;
// arithmetic expressions
arithMul: (MUL | DIV | MOD) expression;
arithAdd: (SUB | ADD) (arithMul | expression);
arithRange:(ELLIPSIS_IN | ELLIPSIS_OUT) (arithAdd | expression);
arithShift:(LSHIFT | RSHIFT) (arithRange | expression);
arithBit: (BITAND | BITOR | CARET) (arithShift | expression);

// logic expressions
logicOp
    : GT
    | LT
    | EQUAL
    | LE
    | GE
    | NOTEQUAL
    ;
atomLogic
    : logicOp expression
    | (AND | OR) expression
    ;
andLogic: atomLogic (AND expression atomLogic)*;
logic: andLogic (OR expression andLogic)*;
elvis: QUESTION expression COLON expression;


// primitives
id: IDENTIFIER;
bool: TRUE | FALSE ;
char: CHAR_LITERAL;
string: STRING_LITERAL | TEXT_BLOCK ;
num
    : DECIMAL_LITERAL
    | HEX_LITERAL
    | FLOAT_LITERAL
    | HEX_FLOAT_LITERAL
    ;
null: NULL;
