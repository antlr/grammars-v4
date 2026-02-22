/******************************
* Parser for OCL 2.4 with extensions for textual notations
* for UML classes and usecases. 
*
* Arrow operators ->op are used consistently for any OCL 
* operator, not just collection operators. 
* 
* Copyright (c) 2003--2025 Kevin Lano
* This program and the accompanying materials are made available under the
* terms of the Eclipse Public License 2.0 which is available at
* http://www.eclipse.org/legal/epl-2.0
*
* SPDX-License-Identifier: EPL-2.0
* *****************************/
// $antlr-format alignTrailingComments true, columnLimit 150, minEmptyLines 1, maxEmptyLinesToKeep 1, reflowComments false, useTab false
// $antlr-format allowShortRulesOnASingleLine false, allowShortBlocksOnASingleLine true, alignSemicolons hanging, alignColons hanging

grammar UMLRSDS;
import OCL;


specification
    : 'package' ID '{' classifier* '}' EOF
    ;

expressions
    : expression (';' expression)* ';'? EOF
    ;

classifier
    : classDefinition
    | interfaceDefinition
    | usecaseDefinition
    | datatypeDefinition
    | enumeration
    ;

interfaceDefinition
    : 'interface' identifier ('extends' identifier)? '{' classBody? '}'
    ;

enumeration
    : 'enumeration' identifier '{' enumerationLiteral+ '}'
    ;

enumerationLiteral
    : 'literal' identifier
    ;

classDefinition
    : 'class' identifier ('extends' identifier)? ('implements' idList)? '{' classBody? '}'
    ;

classBody
    : classBodyElement+
    ;

classBodyElement
    : attributeDefinition
    | operationDefinition
    | invariant
    | stereotype
    ;

attributeDefinition
    : 'attribute' identifier ('identity' | 'derived')? ':' type ';'
    | 'static' 'attribute' identifier ':' type ';'
    ;

operationDefinition
    : ('static')? 'operation' identifier '(' parameterDeclarations? ')' ':' type 'pre:' expression 'post:' expression ('activity:' statementList)? ';'
    ;

parameterDeclarations
    : (parameterDeclaration ',')* parameterDeclaration
    ;

parameterDeclaration
    : identifier ':' type
    ;

idList
    : (identifier ',')* identifier
    ;

usecaseDefinition
    : 'usecase' identifier (':' type)? '{' usecaseBody? '}'
    | 'usecase' identifier '(' parameterDeclarations ')' (':' type)? '{' usecaseBody? '}'
    ;

usecaseBody
    : usecaseBodyElement+
    ;

usecaseBodyElement
    : 'parameter' identifier ':' type ';'
    | 'precondition' expression ';'
    | 'extends' identifier ';'
    | 'extendedBy' identifier ';'
    | 'activity:' statementList ';'
    | '::' expression
    | stereotype
    ;

invariant
    : 'invariant' expression ';'
    ;

stereotype
    : 'stereotype' identifier ';'
    | 'stereotype' identifier '=' STRING1_LITERAL ';'
    | 'stereotype' identifier '=' STRING2_LITERAL ';'
    | 'stereotype' identifier '=' identifier ';'
    ;

datatypeDefinition
      : 'datatype' identifier '=' type
      ; 

statement
    : 'skip'
    | 'return'
    | 'continue'
    | 'break'
    | 'var' ID ':' type
    | 'if' expression 'then' statement 'else' statement
    | 'while' expression 'do' statement
    | 'for' ID ':' expression 'do' statement
    | 'return' expression
    | basicExpression ':=' expression
    | statement ';' statement
    | 'execute' expression
    | 'call' basicExpression
    | '(' statementList ')'
    ;

statementList
   : statement (';' statement)*  
   ;  