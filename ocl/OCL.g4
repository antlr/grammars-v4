/******************************
* Parser for OCL 2.4 with extensions for textual notations
* for UML classes and usecases. 
*
* Arrow operators ->op are used consistently for any OCL 
* operator, not just collection operators. 
* 
* Copyright (c) 2003--2023 Kevin Lano
* This program and the accompanying materials are made available under the
* terms of the Eclipse Public License 2.0 which is available at
* http://www.eclipse.org/legal/epl-2.0
*
* SPDX-License-Identifier: EPL-2.0
* *****************************/

grammar OCL;	
	
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
    | enumeration
    ;

interfaceDefinition
    :	'interface' ID ('extends' ID)? '{' classBody? '}'
    ; 

classDefinition
    :	'class' ID ('extends' ID)? ('implements' idList)? '{' classBody? '}'
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
    :  'attribute' ID ('identity' | 'derived')? ':' type ';' 
    | 'static' 'attribute' ID ':' type ';'
    ; 

operationDefinition
      : ('static')? 'operation' ID 
        '(' parameterDeclarations? ')' ':' type 
        'pre:' expression 'post:' expression 
        ('activity:' statement)? ';'
      ;

parameterDeclarations
      : (parameterDeclaration ',')* parameterDeclaration
      ;

parameterDeclaration
      : ID ':' type
      ;

idList
     : (ID ',')* ID
     ; 

usecaseDefinition
      : 'usecase' ID (':' type)? '{' usecaseBody? '}' 
      | 'usecase' ID '(' parameterDeclarations ')' (':' type)? '{' usecaseBody? '}'
      ;

usecaseBody
      : usecaseBodyElement+
      ; 

usecaseBodyElement
      : 'parameter' ID ':' type ';' 
      | 'precondition' expression ';' 
      | 'extends' ID ';' 
      | 'extendedBy' ID ';' 
      | 'activity:' statement ';' 
      | '::' expression 
      | stereotype
      ;

invariant
      : 'invariant' expression ';'
      ; 

stereotype
      : 'stereotype' ID ';'  
      | 'stereotype' ID '=' STRING_LITERAL ';'  
      | 'stereotype' ID '=' ID ';' 
      ;

enumeration 
      : 'enumeration' ID '{' enumerationLiteral+ '}'
      ;  

enumerationLiteral
      : 'literal' ID
      ;

type
    : 'Sequence' '(' type ')'  
    | 'Set' '(' type ')'  
    | 'Bag' '(' type ')' 
    | 'OrderedSet' '(' type ')' 
    | 'Map' '(' type ',' type ')' 
    | 'Function' '(' type ',' type ')' 
    | ID
    ; 


expressionList
    : (expression ',')* expression
    ; 

expression
    : logicalExpression  
    | conditionalExpression  
    | lambdaExpression  
    | letExpression
    ;


// Basic expressions can appear on the LHS of . or ->

basicExpression
    : 'null' 
    | basicExpression '.' ID 
    | basicExpression '(' expressionList? ')'  
    | basicExpression '[' expression ']' 
    | ID '@pre'  
    | INT  
    | FLOAT_LITERAL
    | STRING_LITERAL
    | ID   
    | '(' expression ')'
    ; 

conditionalExpression
    : 'if' expression 'then' expression 'else' expression 'endif'
    ; 

lambdaExpression 
    : 'lambda' ID ':' type 'in' expression
    ; 

// A let is just an application of a lambda:

letExpression
    : 'let' ID ':' type '=' expression 'in' expression
    ; 

logicalExpression
    : 'not' logicalExpression  
    | logicalExpression 'and' logicalExpression  
    | logicalExpression '&' logicalExpression 
    | logicalExpression 'or' logicalExpression  
    | logicalExpression 'xor' logicalExpression  
    | logicalExpression '=>' logicalExpression  
    | logicalExpression 'implies' logicalExpression  
    | equalityExpression
    ; 

equalityExpression 
    : additiveExpression 
        ('=' | '<' | '>' | '>=' | '<=' | '/=' | '<>' |
         ':'| '/:' | '<:') additiveExpression 
    | additiveExpression
    ; 

additiveExpression
    : additiveExpression '+' additiveExpression 
    | additiveExpression '-' factorExpression
    | factorExpression ('..' | '|->') factorExpression 
    | factorExpression
    ; 

factorExpression 
    : factor2Expression ('*' | '/' | 'mod' | 'div') 
                                   factorExpression 
    | factor2Expression
    ; 


// factor2Expressions can appear on LHS of ->
// ->subrange is used for ->substring and ->subSequence

factor2Expression
  : ('-' | '+') factor2Expression 
  | factor2Expression '->size()' 
  | factor2Expression '->copy()'  
  | factor2Expression ('->isEmpty()' | 
                       '->notEmpty()' | 
                       '->asSet()' | '->asBag()' | 
                       '->asOrderedSet()' | 
                       '->asSequence()' | 
                       '->sort()' ) 
   | factor2Expression '->any()'   
   | factor2Expression '->log()'  
   | factor2Expression '->exp()' 
   | factor2Expression '->sin()'  
   | factor2Expression '->cos()' 
   | factor2Expression '->tan()'  
   | factor2Expression '->asin()'  
   | factor2Expression '->acos()' 
   | factor2Expression '->atan()'  
   | factor2Expression '->log10()' 
   | factor2Expression '->first()'  
   | factor2Expression '->last()' 
   | factor2Expression '->front()'  
   | factor2Expression '->tail()' 
   | factor2Expression '->reverse()'  
   | factor2Expression '->tanh()'  
   | factor2Expression '->sinh()' 
   | factor2Expression '->cosh()' 
   | factor2Expression '->floor()'  
   | factor2Expression '->ceil()' 
   | factor2Expression '->round()' 
   | factor2Expression '->abs()' 
   | factor2Expression '->oclType()' 
   | factor2Expression '->allInstances()' 
   | factor2Expression '->oclIsUndefined()' 
   | factor2Expression '->oclIsInvalid()' 
   | factor2Expression '->oclIsNew()' 
   | factor2Expression '->sum()'  
   | factor2Expression '->prd()'  
   | factor2Expression '->max()'  
   | factor2Expression '->min()'  
   | factor2Expression '->sqrt()'  
   | factor2Expression '->cbrt()'  
   | factor2Expression '->sqr()' 
   | factor2Expression '->characters()'  
   | factor2Expression '->toInteger()'  
   | factor2Expression '->toReal()' 
   | factor2Expression '->toBoolean()' 
   | factor2Expression '->toUpperCase()'  
   | factor2Expression '->toLowerCase()' 
   | factor2Expression ('->unionAll()' | '->intersectAll()' |
                       '->concatenateAll()')
 
   | factor2Expression ('->pow' | '->gcd') '(' expression ')' 
   | factor2Expression ('->at' | '->union' | '->intersection' 
            | '->includes' | '->excludes' | '->including' 
            | '->excluding' | '->includesAll'  
            | '->symmetricDifference' 
            | '->excludesAll' | '->prepend' | '->append'  
            | '->count' | '->apply') 
                                   '(' expression ')' 
   | factor2Expression ('->hasMatch' | '->isMatch' |
                       '->firstMatch' | '->indexOf' | 
                       '->lastIndexOf' | '->split' | 
                       '->hasPrefix' | 
                       '->hasSuffix' | 
                       '->equalsIgnoreCase' ) 
                                    '(' expression ')' 
   | factor2Expression ('->oclAsType' | '->oclIsTypeOf' | 
                       '->oclIsKindOf' | 
                       '->oclAsSet') '(' expression ')' 
   | factor2Expression '->collect' '(' identifier '|' expression ')' 
   | factor2Expression '->select' '(' identifier '|' expression ')' 
   | factor2Expression '->reject' '(' identifier '|' expression ')' 
   | factor2Expression '->forAll' '(' identifier '|' expression ')' 
   | factor2Expression '->exists' '(' identifier '|' expression ')' 
   | factor2Expression '->exists1' '(' identifier '|' expression ')' 
   | factor2Expression '->one' '(' identifier '|' expression ')' 
   | factor2Expression '->any' '(' identifier '|' expression ')' 
   | factor2Expression '->closure' '(' identifier '|' expression ')' 
   | factor2Expression '->sortedBy' '(' identifier '|' expression ')' 
   | factor2Expression '->isUnique' '(' identifier '|' expression ')' 

   | factor2Expression '->subrange' '(' expression ',' expression ')'  
   | factor2Expression '->replace' '(' expression ',' expression ')'  
   | factor2Expression '->replaceAll' '(' expression ',' expression ')' 
   | factor2Expression '->replaceAllMatches' '(' expression ',' expression ')'  
   | factor2Expression '->replaceFirstMatch' '(' expression ',' expression ')'  
   | factor2Expression '->insertAt' '(' expression ',' expression ')'  
   | factor2Expression '->insertInto' '(' expression ',' expression ')'  
   | factor2Expression '->setAt' '(' expression ',' expression ')' 
   | factor2Expression '->iterate' '(' identifier ';' identifier '=' expression '|' expression ')'  
   | setExpression 
   | basicExpression
   ; 

setExpression 
    : 'OrderedSet{' expressionList? '}'  
    | 'Bag{' expressionList? '}'  
    | 'Set{' expressionList? '}' 
    | 'Sequence{' expressionList? '}' 
    | 'Map{' expressionList? '}'
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
   | '(' statement ')'
   ;  

identifier: ID ;

FLOAT_LITERAL:  Digits '.' Digits ;

STRING_LITERAL:     '"' (~["\\\r\n] | EscapeSequence)* '"';

NULL_LITERAL:       'null';

MULTILINE_COMMENT: '/*' .*? '*/' -> channel(HIDDEN);


fragment EscapeSequence
    : '\\' [btnfr"'\\]
    | '\\' ([0-3]? [0-7])? [0-7]
    | '\\' 'u'+ HexDigit HexDigit HexDigit HexDigit
    ;

fragment HexDigits
    : HexDigit ((HexDigit | '_')* HexDigit)?
    ;

fragment HexDigit
    : [0-9a-fA-F]
    ;


fragment Digits
    : [0-9]+
    ;

NEWLINE : [\r\n]+ -> skip ;
INT     : [0-9]+ ;
ID  :   [a-zA-Z$]+[a-zA-Z0-9_$]* ;      // match identifiers
WS  :   [ \t\n\r]+ -> skip ;

