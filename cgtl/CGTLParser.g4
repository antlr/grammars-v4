/******************************
* Parser for CSTL/CGTL language to process ASTs (eg., as 
* produced by ANTLR parsers). 
* 
* Copyright (c) 2023 Kevin Lano
* This program and the accompanying materials are made available under the
* terms of the Eclipse Public License 2.0 which is available at
* http://www.eclipse.org/legal/epl-2.0
*
* SPDX-License-Identifier: EPL-2.0
* *****************************/

parser grammar CGTLParser;	

options { tokenVocab=CGTLLexer; }

	
script
  : ruleset+ EOF
  ;

ruleset
    : ID COLONCOLON cstlrule+
    ;

cstlrule
    : lhs MAPSTO rhs ENDRULE
    ; 

	
lhs 
   : lhstoken+
   ; 
   
rhs
    : rhstoken* (WHEN conditions)? (ACTION actions)?
    ; 

lhstoken 
    : simpleExpr
    | SYMBOL
    | COMMA
    | NOT
    ; 

rhstoken 
    : expr
    | SYMBOL
    | COMMA
    | NOT
    ; 

conditions
    : condition (COMMA condition)*
    ; 

actions 
    : action (COMMA action)*
    ; 

condition
    : expr expr
    | expr NOT expr
    | expr ANY expr
    | expr ALL expr
    | expr MATCHES (expr | SYMBOL)+
    ; 

action
    : expr (NOT | REPLACE)? expr
    ; 

expr 
    : simpleExpr 
    | VAR BQ ID
    ; 

simpleExpr
    : ID
    | VAR
    | INT
    | FLOAT_LITERAL
    | STRING_LITERAL
    ; 


identifier: ID ;

