/*******************************************************************************
 * The MIT License (MIT)
 * 
 * Copyright (c) 2017 Timothy Thomas (timothy-thomas)
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 ******************************************************************************/

/*******************************************************************************
 * MuParser Grammar for ANTLR v4
 *
 * This combined grammar implements the "muparser" mathematical expression
 * language described here:
 * http://beltoforion.de/article.php?a=muparser
 *
 * I also referenced this file for information on the predefined constants:
 * https://github.com/shouze/muparser/blob/master/doc/custom/features.txt
 ******************************************************************************/

/*
 * Parser Rules
 */

grammar MuParser;

prog
 : expr ('\n' expr)*  # progExpr
 ;

expr
 : <assoc=right> expr POW expr                  # powExpr
 | SUB expr                                     # unaryMinusExpr
 | expr op=(MUL | DIV) expr                     # mulDivExpr
 | expr op=(ADD | SUB) expr                     # addSubExpr
 | expr op=(LTEQ | GTEQ | LT | GT) expr         # relationalExpr
 | expr op=(EQ | NEQ) expr                      # equalityExpr
 | expr AND expr                                # andExpr
 | expr OR expr                                 # orExpr
 | expr QUESTION expr COLON expr                # iteExpr
 | op=FUNCTION OPAR expr CPAR                   # functionExpr
 | op=FUNCTIONMULTI OPAR expr (',' expr)* CPAR  # functionMultiExpr
 | atom                                         # atomExpr
 | ID op=( ASSIGN
         | ASSIGNADD
         | ASSIGNSUB
         | ASSIGNMUL
         | ASSIGNDIV) expr                      # assignExpr
 ;

atom
 : OPAR expr CPAR     # parExpr
 | (INT | FLOAT)      # numberAtom
 | (TRUE | FALSE)     # booleanAtom
 | (E | PI)           # predefinedConstantAtom
 | ID                 # idAtom
 ;

/*
 * Lexer Rules
 */

FUNCTION
 : 'sin'
 | 'cos'
 | 'tan'
 | 'asin'
 | 'acos'
 | 'atan'
 | 'sinh'
 | 'cosh'
 | 'tanh'
 | 'asinh'
 | 'acosh'
 | 'atanh'
 | 'log2'
 | 'log10'
 | 'log'
 | 'ln'
 | 'exp'
 | 'sqrt'
 | 'sign'
 | 'rint'
 | 'abs'
 ;

FUNCTIONMULTI
 : 'min'
 | 'max'
 | 'sum'
 | 'avg'
 ;

ASSIGN    : '=' ;
ASSIGNADD : '+=' ;
ASSIGNSUB : '-=' ;
ASSIGNMUL : '*=' ;
ASSIGNDIV : '/=' ;
AND       : '&&' ;
OR        : '||' ;
LTEQ      : '<=' ;
GTEQ      : '>=' ;
NEQ       : '!=' ;
EQ        : '==' ;
LT        : '<' ;
GT        : '>' ;
ADD       : '+' ;
SUB       : '-' ;
MUL       : '*' ;
DIV       : '/' ;
POW       : '^' ;
NOT       : '!' ;

QUESTION  : '?' ;
COLON     : ':' ;

OPAR
 : '('
 ;

CPAR
 : ')'
 ;

INT
 : [0-9]+
 ;

FLOAT
 : [0-9]+ '.' [0-9]* 
 | '.' [0-9]+
 ;

TRUE
 : 'true'
 ;

FALSE
 : 'false'
 ;

E
 : '_e'
 ;

PI
 : '_pi'
 ;

ID
 : [a-zA-Z_] [a-zA-Z_0-9]*
 ;

SPACE
 : [ \t\r\n] -> skip
 ;