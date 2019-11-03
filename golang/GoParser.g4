/*
 [The "BSD licence"]
 Copyright (c) 2017 Sasa Coh, Michał Błotniak
 Copyright (c) 2019 Ivan Kochurkin, kvanttt@gmail.com, Positive Technologies
 Copyright (c) 2019 Dmitry Rassadin, flipparassa@gmail.com, Positive Technologies
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

/*
 * A Go grammar for ANTLR 4 derived from the Go Language Specification
 * https://golang.org/ref/spec
 */
 
parser grammar GoParser;

options {
    tokenVocab=GoLexer;
    superClass=GoParserBase;
}

sourceFile
    : packageClause eos (importDecl eos)* ((functionDecl | methodDecl | declaration) eos)*
    ;

packageClause
    : 'package' IDENTIFIER
    ;

importDecl
    : 'import' (importSpec | '(' (importSpec eos)* ')')
    ;

importSpec
    : ('.' | IDENTIFIER)? importPath
    ;

importPath
    : string_
    ;

declaration
    : constDecl
    | typeDecl
    | varDecl
    ;

constDecl
    : 'const' (constSpec | '(' (constSpec eos)* ')')
    ;

constSpec
    : identifierList (type_? '=' expressionList)?
    ;

identifierList
    : IDENTIFIER (',' IDENTIFIER)*
    ;

expressionList
    : expression (',' expression)*
    ;

typeDecl
    : 'type' (typeSpec | '(' (typeSpec eos)* ')')
    ;

typeSpec
    : IDENTIFIER ASSIGN? type_
    ;

// Function declarations

functionDecl
    : 'func' IDENTIFIER (signature block?)
    ;

methodDecl
    : 'func' receiver IDENTIFIER (signature block?)
    ;

receiver
    : parameters
    ;

varDecl
    : 'var' (varSpec | '(' (varSpec eos)* ')')
    ;

varSpec
    : identifierList (type_ ('=' expressionList)? | '=' expressionList)
    ;

block
    : '{' statementList? '}'
    ;

statementList
    : (statement eos)+
    ;

statement
    : declaration
    | labeledStmt
    | simpleStmt
    | goStmt
    | returnStmt
    | breakStmt
    | continueStmt
    | gotoStmt
    | fallthroughStmt
    | block
    | ifStmt
    | switchStmt
    | selectStmt
    | forStmt
    | deferStmt
    ;

simpleStmt
    : sendStmt
    | expressionStmt
    | incDecStmt
    | assignment
    | shortVarDecl
    | emptyStmt
    ;

expressionStmt
    : expression
    ;

sendStmt
    : expression '<-' expression
    ;

incDecStmt
    : expression (PLUS_PLUS | MINUS_MINUS)
    ;

assignment
    : expressionList assign_op expressionList
    ;

assign_op
    : ('+' | '-' | '|' | '^' | '*' | '/' | '%' | '<<' | '>>' | '&' | '&^')? '='
    ;

shortVarDecl
    : identifierList ':=' expressionList
    ;

emptyStmt
    : ';'
    ;

labeledStmt
    : IDENTIFIER ':' statement
    ;

returnStmt
    : 'return' expressionList?
    ;

breakStmt
    : 'break' IDENTIFIER?
    ;

continueStmt
    : 'continue' IDENTIFIER?
    ;

gotoStmt
    : 'goto' IDENTIFIER
    ;

fallthroughStmt
    : 'fallthrough'
    ;

deferStmt
    : 'defer' expression
    ;

ifStmt
    : 'if' (simpleStmt ';')? expression block ('else' (ifStmt | block))?
    ;

switchStmt
    : exprSwitchStmt
    | typeSwitchStmt
    ;

exprSwitchStmt
    : 'switch' (simpleStmt ';')? expression? '{' exprCaseClause* '}'
    ;

exprCaseClause
    : exprSwitchCase ':' statementList?
    ;

exprSwitchCase
    : 'case' expressionList
    | 'default'
    ;

typeSwitchStmt
    : 'switch' (simpleStmt ';')? typeSwitchGuard '{' typeCaseClause* '}'
    ;

typeSwitchGuard
    : (IDENTIFIER ':=')? primaryExpr '.' '(' 'type' ')'
    ;

typeCaseClause
    : typeSwitchCase ':' statementList?
    ;

typeSwitchCase
    : 'case' typeList
    | 'default'
    ;

typeList
    : (type_ | NIL_LIT) (',' (type_ | NIL_LIT))*
    ;

selectStmt
    : 'select' '{' commClause* '}'
    ;

commClause
    : commCase ':' statementList?
    ;

commCase
    : 'case' (sendStmt | recvStmt)
    | 'default'
    ;

recvStmt
    : (expressionList '=' | identifierList ':=')? expression
    ;

forStmt
    : 'for' (expression | forClause | rangeClause)? block
    ;

forClause
    : simpleStmt? ';' expression? ';' simpleStmt?
    ;

rangeClause
    : (expressionList '=' | identifierList ':=')? 'range' expression
    ;

goStmt
    : 'go' expression
    ;

type_
    : typeName
    | typeLit
    | '(' type_ ')'
    ;

typeName
    : IDENTIFIER
    | qualifiedIdent
    ;

typeLit
    : arrayType
    | structType
    | pointerType
    | functionType
    | interfaceType
    | sliceType
    | mapType
    | channelType
    ;

arrayType
    : '[' arrayLength ']' elementType
    ;

arrayLength
    : expression
    ;

elementType
    : type_
    ;

pointerType
    : '*' type_
    ;

interfaceType
    : 'interface' '{' (methodSpec eos)* '}'
    ;

sliceType
    : '[' ']' elementType
    ;

// It's possible to replace `type` with more restricted typeLit list and also pay attention to nil maps
mapType
    : 'map' '[' type_ ']' elementType
    ;

channelType
    : ('chan' | 'chan' '<-' | '<-' 'chan') elementType
    ;

methodSpec
    : {noTerminatorAfterParams(2)}? IDENTIFIER parameters result
    | typeName
    | IDENTIFIER parameters
    ;

functionType
    : 'func' signature
    ;

signature
    : {noTerminatorAfterParams(1)}? parameters result
    | parameters
    ;

result
    : parameters
    | type_
    ;

parameters
    : '(' (parameterDecl (COMMA parameterDecl)* COMMA?)? ')'
    ;

parameterDecl
    : identifierList? '...'? type_
    ;

expression
    : primaryExpr
    | unaryExpr
    | expression ('*' | '/' | '%' | '<<' | '>>' | '&' | '&^') expression
    | expression ('+' | '-' | '|' | '^') expression
    | expression ('==' | '!=' | '<' | '<=' | '>' | '>=') expression
    | expression '&&' expression
    | expression '||' expression
    ;

primaryExpr
    : operand
    | conversion
    | primaryExpr ( DOT IDENTIFIER
                  | index
                  | slice
                  | typeAssertion
                  | arguments)
    ;

unaryExpr
    : primaryExpr
    | ('+' | '-' | '!' | '^' | '*' | '&' | '<-') expression
    ;

conversion
    : type_ '(' expression ','? ')'
    ;

operand
    : literal
    | operandName
    | methodExpr
    | '(' expression ')'
    ;

literal
    : basicLit
    | compositeLit
    | functionLit
    ;

basicLit
    : NIL_LIT
    | integer
    | string_
    | FLOAT_LIT
    | IMAGINARY_LIT
    | RUNE_LIT
    ;

integer
    : DECIMAL_LIT
    | OCTAL_LIT
    | HEX_LIT
    | IMAGINARY_LIT
    | RUNE_LIT
    ;

operandName
    : IDENTIFIER
    | qualifiedIdent
    ;

qualifiedIdent
    : IDENTIFIER '.' IDENTIFIER
    ;

compositeLit
    : literalType literalValue
    ;

literalType
    : structType
    | arrayType
    | '[' '...' ']' elementType
    | sliceType
    | mapType
    | typeName
    ;

literalValue
    : '{' (elementList ','?)? '}'
    ;

elementList
    : keyedElement (',' keyedElement)*
    ;

keyedElement
    : (key ':')? element
    ;

key
    : IDENTIFIER
    | expression
    | literalValue
    ;

element
    : expression
    | literalValue
    ;

structType
    : 'struct' '{' (fieldDecl eos)* '}'
    ;

fieldDecl
    : ({noTerminatorBetween(2)}? identifierList type_ | anonymousField) string_?
    ;

string_
    : RAW_STRING_LIT
    | INTERPRETED_STRING_LIT
    ;

anonymousField
    : '*'? typeName
    ;

functionLit
    : 'func' signature block // function
    ;

index
    : '[' expression ']'
    ;

slice
    : '[' (expression? ':' expression? | expression? ':' expression ':' expression) ']'
    ;

typeAssertion
    : '.' '(' type_ ')'
    ;

arguments
    : '(' ((expressionList | type_ (',' expressionList)?) '...'? ','?)? ')'
    ;

methodExpr
    : receiverType DOT IDENTIFIER
    ;

receiverType
    : typeName
    | '(' ('*' typeName | receiverType) ')'
    ;

eos
    : ';'
    | EOF
    | {lineTerminatorAhead()}?
    | {checkPreviousTokenText("}")}?
    ;

