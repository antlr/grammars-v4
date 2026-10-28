/*
BSD License

Copyright (c) 2026, Tom Everett
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. Neither the name of Tom Everett nor the names of its contributors
   may be used to endorse or promote products derived from this software
   without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

// A best-effort grammar for a practical subset of the Harbour language
// (https://harbour.github.io/), a modern, cross-platform, Clipper/xBase
// compatible compiler. It covers the common, fixed core of everyday
// Harbour source: FUNCTION/PROCEDURE and CLASS/METHOD declarations,
// variable declarations, the usual control-flow commands (IF, DO WHILE,
// FOR, DO CASE, SWITCH), the expression language (assignment, arithmetic,
// relational and logical operators, object messages, code blocks, and
// array literals), and the `?` print command. Keywords are matched in
// uppercase, the conventional style for Harbour source.
grammar harbour;

program
    : topLevel* EOF
    ;

topLevel
    : functionDecl
    | classDecl
    | methodImpl
    | statement
    ;

functionDecl
    : ('FUNCTION' | 'PROCEDURE') IDENTIFIER ('(' paramList? ')')?
      statement*
    ;

paramList
    : IDENTIFIER (',' IDENTIFIER)*
    ;

classDecl
    : 'CLASS' IDENTIFIER ('FROM' IDENTIFIER (',' IDENTIFIER)*)?
      classMember*
      'ENDCLASS'
    ;

classMember
    : ('DATA' | 'VAR') IDENTIFIER (',' IDENTIFIER)*
    | 'METHOD' IDENTIFIER ('(' paramList? ')')? ('CONSTRUCTOR')? ('INLINE' statement*)?
    ;

methodImpl
    : 'METHOD' IDENTIFIER ('(' paramList? ')')? 'CLASS' IDENTIFIER
      statement*
    ;

statement
    : declStmt
    | ifStmt
    | doWhileStmt
    | forStmt
    | doCaseStmt
    | switchStmt
    | printStmt
    | returnStmt
    | exitStmt
    | loopStmt
    | exprStmt
    ;

declStmt
    : ('LOCAL' | 'STATIC' | 'PUBLIC' | 'PRIVATE' | 'FIELD') declItem (',' declItem)*
    ;

declItem
    : IDENTIFIER ('AS' IDENTIFIER)? ((':=' expression) | ('INIT' expression))?
    ;

ifStmt
    : 'IF' expression statement*
      ('ELSEIF' expression statement*)*
      ('ELSE' statement*)?
      'ENDIF'
    ;

doWhileStmt
    : 'DO' 'WHILE' expression statement* 'ENDDO'
    ;

forStmt
    : 'FOR' IDENTIFIER ':=' expression 'TO' expression ('STEP' expression)?
      statement*
      'NEXT'
    ;

doCaseStmt
    : 'DO' 'CASE'
      ('CASE' expression statement*)*
      ('OTHERWISE' statement*)?
      'ENDCASE'
    ;

switchStmt
    : 'SWITCH' expression
      ('CASE' expression statement*)*
      ('OTHERWISE' statement*)?
      'END'
    ;

printStmt
    : '?' (expression (',' expression)*)?
    ;

returnStmt
    : 'RETURN' expression?
    ;

exitStmt
    : 'EXIT'
    ;

loopStmt
    : 'LOOP'
    ;

exprStmt
    : expression
    ;

expression
    : postfixExpr ':=' expression                                                 # assignExpr
    | expression op=('++' | '--')                                                 # postIncDecExpr
    | '-' expression                                                               # unaryMinusExpr
    | ('NOT' | '.NOT.') expression                                                # notExpr
    | expression op=('*' | '/' | '%') expression                                  # mulExpr
    | expression op=('+' | '-') expression                                        # addExpr
    | expression op=('<=' | '>=' | '<>' | '==' | '<' | '>' | '=' | '#') expression # relExpr
    | expression ('AND' | '.AND.') expression                                      # andExpr
    | expression ('OR' | '.OR.') expression                                        # orExpr
    | postfixExpr                                                                   # postfixExprAlt
    ;

postfixExpr
    : primary postfixOp*
    ;

postfixOp
    : ':' IDENTIFIER ('(' argList? ')')?
    | '(' argList? ')'
    | '[' expression (',' expression)* ']'
    ;

argList
    : expression (',' expression)*
    ;

primary
    : IDENTIFIER
    | NUMBER
    | STRING
    | logical
    | 'NIL'
    | '::' IDENTIFIER
    | '&' IDENTIFIER
    | codeBlock
    | arrayLiteral
    | '(' expression ')'
    ;

logical
    : '.T.'
    | '.F.'
    | '.TRUE.'
    | '.FALSE.'
    ;

codeBlock
    : '{' '|' (IDENTIFIER (',' IDENTIFIER)*)? '|' (expression (',' expression)*)? '}'
    ;

arrayLiteral
    : '{' (expression (',' expression)*)? '}'
    ;

IDENTIFIER
    : [a-zA-Z_] [a-zA-Z0-9_]*
    ;

NUMBER
    : [0-9]+ ('.' [0-9]+)?
    ;

STRING
    : '"' (~["\r\n])* '"'
    | '\'' (~['\r\n])* '\''
    ;

LINE_COMMENT
    : '//' ~[\r\n]* -> skip
    ;

BLOCK_COMMENT
    : '/*' .*? '*/' -> skip
    ;

WS
    : [ \t\r\n]+ -> skip
    ;
