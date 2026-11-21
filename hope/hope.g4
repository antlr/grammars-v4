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

grammar hope;

prog
   : declaration* EOF
   ;

declaration
   : dec_declaration
   | data_declaration
   | equation_declaration
   ;

dec_declaration
   : 'dec' NAME ':' type_expr ';'
   ;

type_expr
   : type_expr '->' type_expr
   | type_expr '#' type_expr
   | 'list' type_expr
   | NAME '(' type_expr (',' type_expr)* ')'
   | '(' type_expr ')'
   | NAME
   ;

data_declaration
   : 'data' NAME ('(' NAME (',' NAME)* ')')? '==' data_alternative ('++' data_alternative)* ';'
   ;

data_alternative
   : NAME ('(' type_expr (',' type_expr)* ')')?
   ;

equation_declaration
   : '---' NAME pattern* '<=' expr ('where' where_clause (',' where_clause)*)? ';'
   ;

where_clause
   : pattern '==' expr
   ;

pattern
   : NAME '(' pattern (',' pattern)* ')' # consPattern
   | pattern '::' pattern                # consInfixPattern
   | '[' (pattern (',' pattern)*)? ']'   # listPattern
   | '(' pattern (',' pattern)* ')'      # tuplePattern
   | NUMBER                              # numberPattern
   | STRING                              # stringPattern
   | '_'                                 # wildcardPattern
   | NAME                                # namePattern
   ;

expr
   : expr '(' expr_list ')'                                        # callExpr
   | op = ('-' | '+') expr                                         # unaryExpr
   | expr op = ('*' | '/') expr                                    # mulDivExpr
   | expr op = ('+' | '-') expr                                    # addSubExpr
   | expr '::' expr                                                # consExpr
   | expr op = ('<' | '>' | '<=' | '>=' | '==' | '~=') expr        # compareExpr
   | 'if' expr 'then' expr 'else' expr                             # ifExpr
   | 'lambda' NAME (',' NAME)* '=>' expr                           # lambdaExpr
   | expr 'where' where_clause (',' where_clause)*                 # whereExpr
   | '(' expr (',' expr)* ')'                                      # tupleOrParenExpr
   | '[' (expr (',' expr)*)? ']'                                   # listExpr
   | NUMBER                                                        # numberExpr
   | STRING                                                        # stringExpr
   | NAME                                                          # nameExpr
   ;

expr_list
   : (expr (',' expr)*)?
   ;

NAME
   : [a-zA-Z_] [a-zA-Z_0-9']*
   ;

NUMBER
   : [0-9]+ ('.' [0-9]+)?
   ;

STRING
   : '"' (ESC | ~["\\])* '"'
   ;

fragment ESC
   : '\\' .
   ;

COMMENT
   : '%' ~[\r\n]* -> skip
   ;

WS
   : [ \r\n\t] + -> skip
   ;
