/*
MIT License

Copyright (c) 2025 Enaium

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

grammar Squirrel;

nut
    : statements? EOF
    ;

statements
    : multisemi? statementWithSemi*
    ;

statementWithSemi
    : statement delimiter
    ;

delimiter
    : multisemi
    | RBRACE
    | EOF
    ;

semi
    : SEMI
    | SEMI? NL+
    ;

multisemi
    : semi+
    ;

statement
    : block
    | expressionStatement
    | constDeclaration
    | enumDeclaration
    | localDeclaration
    | functionDeclaration
    | classDeclaration
    | forStatement
    | foreachStatement
    | whileStatement
    | doWhileStatement
    | ifStatement
    | switchStatement
    | tryStatement
    | returnStatement
    | breakStatement
    | continueStatement
    | yieldStatement
    | throwStatement
    ;

statementOrEmpty
    : statement | SEMI
    ;

block
    : LBRACE statements? RBRACE
    ;

constDeclaration
    : CONST id EQ literal
    ;

enumDeclaration
    : ENUM id LBRACE enumItemDeclarations? RBRACE
    ;

enumItemDeclarations
    : enumItem (COMMA enumItem)* COMMA?
    ;

enumItem
    : id (EQ literal)?
    ;

localDeclaration
    : LOCAL variableOrFunctionDeclarations
    ;

variableOrFunctionDeclarations
    : varDeclarationList
    | functionDeclaration
    ;

varDeclarationList
    : varItem (COMMA varItem)*
    ;

varItem
    : id varInit?
    ;

varInit
    : EQ expressionWithNoComma
    ;

functionDeclaration
    : FUNCTION functionName parameters NL* functionBody
    ;

functionName
    : id (DOUBLE_COLON (id | CONSTRUCTOR))*
    ;

functionBody
    : statementOrEmpty
    ;

parameters
    : LPAREN parameterList? RPAREN
    ;

parameterList
    : parameter (COMMA parameter)* ((COMMA defaultParameter)+ | (COMMA variableLengthParameter))?
    | defaultParameter (COMMA defaultParameter)*
    | variableLengthParameter
    ;

parameter
    : id
    ;

defaultParameter
    : id EQ expressionWithNoComma
    ;

variableLengthParameter
    : MULTI_ARGS
    ;

classDeclaration
    : CLASS className (EXTENDS className)? classBody
    ;

className
    : id (DOT id)*
    ;

classBody
    : LBRACE NL* classMembers? RBRACE
    ;

classMembers
    : classMember (semi* classMember)* semi?
    ;

classMember
    : (classAttribute NL*)? STATIC? (constructorDeclaration | regularClassMember)
    ;

regularClassMember
    : keyValuePair | methodDeclaration
    ;

constructorDeclaration
    : CONSTRUCTOR parameters NL* functionBody
    ;

methodDeclaration
    : FUNCTION functionName parameters NL* functionBody
    ;

classAttribute
    : CLASS_ATTR_START keyValuePairs? CLASS_ATTR_END
    ;

returnStatement
    : RETURN expression?
    ;

breakStatement
    : BREAK
    ;

continueStatement
    : CONTINUE
    ;

yieldStatement
    : YIELD expression
    ;

throwStatement
    : THROW expression
    ;

forStatement
    : FOR LPAREN forLoopParts RPAREN NL* statementOrEmpty
    ;

forLoopParts
    : (localDeclaration | expression)? SEMI expression? SEMI expression?
    ;

foreachStatement
    : FOREACH LPAREN (id COMMA)? id IN expression RPAREN NL* statementOrEmpty
    ;

whileStatement
    : WHILE LPAREN expression RPAREN NL* statementOrEmpty
    ;

doWhileStatement
    : DO NL* statementOrEmpty WHILE LPAREN expression RPAREN
    ;

ifStatement
    : IF LPAREN expression RPAREN NL* statementOrEmpty (semi? ELSE NL* statementOrEmpty)?
    ;

switchStatement
    : SWITCH LPAREN expression RPAREN NL* LBRACE NL* switchCase* defaultCase? RBRACE
    ;

switchCase
    : CASE expression COLON statements?
    ;

defaultCase
    : DEFAULT COLON statements?
    ;

tryStatement
    : TRY statementOrEmpty catchPart
    ;

catchPart
    : CATCH LPAREN id RPAREN statementOrEmpty
    ;

expressionStatement
    : expression
    ;

expression
    : commaExpression
    ;

commaExpression
    : assignExpression (COMMA assignExpression)*
    ;

assignExpression
    : ternaryExpression assignmentOperator assignExpression | ternaryExpression
    ;

ternaryExpression
    : logicOrExpression (QUESTION expression COLON ternaryExpression)?
    ;

logicOrExpression
    : andInExpression (OR_OR andInExpression)*
    ;

andInExpression
    : bitOrExpression ((AND_AND | IN) bitOrExpression)*
    ;

bitOrExpression
    : bitXorExpression (BIT_OR bitXorExpression)*
    ;

bitXorExpression
    : bitAndExpression (BIT_XOR bitAndExpression)*
    ;

bitAndExpression
    : compareExpression (BIT_AND compareExpression)*
    ;

compareExpression
    : relationalExpression (equalityOperator relationalExpression)*
    ;

relationalExpression
    : shiftExpression (relationalOperator shiftExpression)*
    ;

shiftExpression
    : additiveExpression (shiftOperator additiveExpression)*
    ;

additiveExpression
    : multiplicativeExpression (additiveOperator multiplicativeExpression)*
    ;

multiplicativeExpression
    : unaryLevel (multiplicativeOperator unaryLevel)*
    ;

unaryLevel
    : (unaryOperator | unaryAction) unaryLevel
    | prefixOperator namedElementExpression
    | postfixExpression (INSTANCEOF postfixExpression)*
    ;

postfixExpression
    : primaryExpression postfixSuffix*
    ;

postfixSuffix
    : DOT (id | CONSTRUCTOR)
    | arguments
    | LBRACKET expressionWithNoComma RBRACKET postOperator?
    ;

postOperator
    : PLUS_PLUS | MINUS_MINUS
    ;

primaryExpression
    : literalExpression
    | parenthesizedExpression
    | classExpression
    | tableExpression
    | arrayExpression
    | functionExpression
    | lambdaFunctionExpression
    | id postOperator?
    ;

namedElementExpression
    : id postOperator?
    | postfixExpression DOT (id | CONSTRUCTOR)
    | postfixExpression LBRACKET expressionWithNoComma RBRACKET postOperator?
    ;

expressionWithNoComma
    : assignExpression
    ;

parenthesizedExpression
    : LPAREN expression RPAREN
    ;

classExpression
    : CLASS (EXTENDS className)? classBody
    ;

functionExpression
    : FUNCTION parameters NL* functionBody
    ;

lambdaFunctionExpression
    : AT parameters expressionWithNoComma
    ;

arguments
    : LPAREN argumentList? RPAREN
    ;

argumentList
    : argumentListPart (COMMA argumentListPart)*
    ;

argumentListPart
    : expressionWithNoComma
    ;

arrayExpression
    : LBRACKET arrayElementsList? RBRACKET
    ;

arrayElementsList
    : arrayListPart (COMMA arrayListPart)* COMMA?
    ;

arrayListPart
    : expressionWithNoComma
    ;

tableExpression
    : LBRACE keyValuePairs? RBRACE
    ;

keyValuePairs
    : tableItemSeparator* (tableItem tableItemSeparator*)*
    ;

tableItemSeparator
    : COMMA | NL
    ;

tableItem
    : keyValuePair | functionDeclaration
    ;

keyValuePair
    : key EQ expressionWithNoComma
    ;

key
    : id | LBRACKET expression RBRACKET
    ;

literalExpression
    : literal
    ;

literal
    : MINUS? INT
    | MINUS? FLOAT
    | STRING
    | TRUE
    | FALSE
    | NULL
    ;

id
    : DOUBLE_COLON? IDENTIFIER
    ;

multiplicativeOperator
    : MUL
    | DIV
    | REMAINDER
    ;

additiveOperator
    : PLUS
    | MINUS
    ;

shiftOperator
    : SHIFT_LEFT
    | UNSIGNED_SHIFT_RIGHT
    | SHIFT_RIGHT
    ;

relationalOperator
    : LESS
    | LESS_OR_EQ
    | GREATER
    | GREATER_OR_EQ
    ;

equalityOperator
    : EQ_EQ
    | NOT_EQ
    | CMP
    ;

assignmentOperator
    : EQ
    | SEND_CHANNEL
    | MUL_EQ
    | DIV_EQ
    | REMAINDER_EQ
    | PLUS_EQ
    | MINUS_EQ
    ;

unaryOperator
    : MINUS
    | NOT
    | BIT_NOT
    ;

unaryAction
    : TYPEOF
    | CLONE
    | DELETE
    | RESUME
    ;

prefixOperator
    : PLUS_PLUS
    | MINUS_MINUS
    ;

CONST: 'const';
ENUM: 'enum';
LOCAL: 'local';
FUNCTION: 'function';
CLASS: 'class';
EXTENDS: 'extends';
STATIC: 'static';
CONSTRUCTOR: 'constructor';
TRUE: 'true';
FALSE: 'false';
NULL: 'null';
RETURN: 'return';
BREAK: 'break';
CONTINUE: 'continue';
YIELD: 'yield';
THROW: 'throw';
FOR: 'for';
FOREACH: 'foreach';
WHILE: 'while';
DO: 'do';
IF: 'if';
ELSE: 'else';
SWITCH: 'switch';
CASE: 'case';
DEFAULT: 'default';
TRY: 'try';
CATCH: 'catch';
IN: 'in';
TYPEOF: 'typeof';
CLONE: 'clone';
DELETE: 'delete';
RESUME: 'resume';
INSTANCEOF: 'instanceof';

RBRACE: '}';
RBRACKET: ']';
RPAREN: ')';
PLUS_PLUS: '++';
MINUS_MINUS: '--';
LBRACE: '{';
LBRACKET: '[';
LPAREN: '(';
DOUBLE_COLON: '::';
COLON: ':';
SEMI: ';';
COMMA: ',';
MULTI_ARGS: '...';
CLASS_ATTR_START: '</';
CLASS_ATTR_END: '/>';
SHIFT_LEFT: '<<';
SHIFT_RIGHT: '>>';
UNSIGNED_SHIFT_RIGHT: '>>>';
CMP: '<=>';
EQ_EQ: '==';
NOT_EQ: '!=';
LESS_OR_EQ: '<=';
GREATER_OR_EQ: '>=';
SEND_CHANNEL: '<-';
PLUS_EQ: '+=';
MINUS_EQ: '-=';
MUL_EQ: '*=';
DIV_EQ: '/=';
REMAINDER_EQ: '%=';
OR_OR: '||';
AND_AND: '&&';
EQ: '=';
NOT: '!';
BIT_NOT: '~';
BIT_OR: '|';
BIT_XOR: '^';
BIT_AND: '&';
LESS: '<';
GREATER: '>';
PLUS: '+';
MINUS: '-';
MUL: '*';
DIV: '/';
REMAINDER: '%';
QUESTION: '?';
AT: '@';
DOT: '.';

IDENTIFIER: [a-zA-Z_][a-zA-Z_0-9]*;

INT: ('0'[1-7][0-7]* | '0x'[0-9a-fA-F]+ | '\''[a-zA-Z]'\'' | '0' | [1-9][0-9]*);

FLOAT: (([0-9]+'.'[0-9]* | [0-9]*'.'[0-9]+)([eE][+-]?[0-9]+)? | [0-9]+([eE][+-]?[0-9]+));

STRING: ('@"' (~["] | '""')* '"' | '"' ( '\\' . | ~["\r\n] )* '"');

SINGLE_LINE_COMMENT: ('//' | '#') ~[\r\n]* -> skip;

MULTI_LINE_COMMENT: '/*' .*? '*/' -> skip;

NL: [\r\n]+;

WS: [ \t\f]+ -> skip;