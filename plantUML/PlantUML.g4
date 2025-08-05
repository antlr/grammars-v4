/******************************
* Parser for PlantUML class diagrams
* 
* Copyright (c) 2003--2025 Kevin Lano
*
* This program and the accompanying materials are made available under the
* terms of the Eclipse Public License 2.0 which is available at
* http://www.eclipse.org/legal/epl-2.0
*
* SPDX-License-Identifier: EPL-2.0
* *****************************/

grammar PlantUML;	
	
classDiagram
  : '@startuml' cdelement* '@enduml' EOF
  ;

cdelement
    : 'class' identifier stereotype? classDefinition?
    | 'interface' identifier stereotype? classDefinition?
    | 'abstract' 'class' identifier stereotype? classDefinition?
    | 'abstract' identifier stereotype? classDefinition?
    | 'entity' identifier stereotype? classDefinition?
    | 'enum' identifier stereotype? enumDefinition?
    | 'annotation' identifier
    | inheritance
    | association
    | aggregation
    | attribute
    | composition
    | dependency
    | method
    ;

classDefinition
    : '{' classElement* '}'
    ; 

classElement
    : internalMethod
    | internalAttribute
    ; 

enumDefinition
    : '{' identifier+ '}'
    ; 

attribute
    : identifier ':' type visibility? identifier 
    ; 

internalAttribute 
    : ('{' modifier '}')? type? visibility? identifier
    | ('{' modifier '}')? visibility? identifier ':' type
    ; 

method
    : identifier ':' visibility? identifier '(' parameters? ')' 
    ;

internalMethod 
    : ('{' modifier '}')? type? visibility? identifier '(' parameters? ')'
    | ('{' modifier '}')? visibility? identifier '(' parameters? ')' ':' type 
    ; 

parameters
      : (identifier ',')* identifier
      ;

stereotype 
      : '<<' identifier '>>'
      ; 

visibility
      : '+' 
      | '-' 
      | '#'
      | '~'
      ; 

modifier
      : 'static'
      | 'classifier' 
      | 'abstract'
      ; 

type
    : 'Sequence' '(' type ')'  
    | 'Set' '(' type ')'  
    | 'Bag' '(' type ')' 
    | 'OrderedSet' '(' type ')' 
    | 'SortedSet' '(' type ')' 
    | identifier '[' integerLiteral? ']'  
    | 'Map' '(' type ',' type ')' 
    | 'SortedMap' '(' type ',' type ')' 
    | 'Function' '(' type ',' type ')' 
    | identifier
    ; 

inheritance
    : identifier (leftAncestorSymbol | rightAncestorSymbol) identifier
    ; 

leftAncestorSymbol
    : '<|--'
    | '<|-'
    | '<|---' 
    | '<|..' 
    | '^--' 
    ; 

rightAncestorSymbol
    : '--|>' 
    | '-|>' 
    | '---|>'
    | '..|>' 
    | '--^' 
    ; 

association
    : identifier stringExpression? associationSymbol stringExpression? identifier lineAnnotation?
    ; 

associationSymbol
    : '-'
    | '--'
    | '---' 
    | '-->' 
    | '->' 
    | '<--'
    | '<-'
    ; 

aggregation
    : identifier stringExpression? aggregationSymbol stringExpression? identifier lineAnnotation?
    ; 

aggregationSymbol
    : 'o--' 
    | '--o' 
    | 'o-->'
    | '<--o'
    ; 

composition
    : identifier stringExpression? compositionSymbol stringExpression? identifier lineAnnotation?
    ; 

compositionSymbol
    : '*--' 
    | '*-' 
    | '--*'
    | '-*'
    | '*-->'
    | '*->'
    | '<--*'
    | '<-*'
    ; 

dependency
    : identifier stringExpression? dependencySymbol stringExpression? identifier lineAnnotation?
    ; 

dependencySymbol
    : '..'
    | '..>'
    | '<..'
    | '...'
    | '...>'
    | '<...'
    ; 

lineAnnotation
    : ':' direction? identifier
    | ':' identifier direction
    ; 

direction
    : '>'
    | '<'
    ; 


identifier: ID ;

stringExpression : STRING1_LITERAL | STRING2_LITERAL ;

integerLiteral : INT ; 

FLOAT_LITERAL:  Digits '.' Digits ;

STRING1_LITERAL:     '"' (~["\\\r\n] | EscapeSequence)* '"';

STRING2_LITERAL:     '\'' (~['\\\r\n] | EscapeSequence)* '\'';

NULL_LITERAL:       'null';

MULTILINE_COMMENT: '/*' .*? '*/' -> channel(HIDDEN);


fragment EscapeSequence
    : '\\' [btnfr"'\\]
    | '\\' ([0-3]? [0-7])? [0-7]
    | '\\' 'u' HexDigit HexDigit HexDigit HexDigit
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

INTEGRAL : '\u222B'; 
SIGMA : '\u2211';

NEWLINE : [\r\n]+ -> skip ;
INT     : [0-9]+ ;
ID  :   [a-zA-Z_$]+[a-zA-Z0-9_$]* ;      // match identifiers
WS  :   [ \t\n\r]+ -> skip ;
