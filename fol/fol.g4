/*
* FOL originally written for Antlr3 by Stephan Opfer
*
* Ported to Antlr4 by Tom Everett
*
*/

grammar fol;

/*------------------------------------------------------------------
 * PARSER RULES
 *------------------------------------------------------------------*/
condition
   : formula EOF
   ;

formula
   : ((FORALL | EXISTS) VARIABLE)? disjunction
   ;

disjunction
   : conjunction (OR conjunction)*
   ;

conjunction
   : negation (AND negation)*
   ;

negation
   : NOT? (predicate | LPAREN formula RPAREN)
   ;

predicate
   : PREPOSITION predicateTuple
   | PREPOSITION
   ;

predicateTuple
   : LPAREN term (',' term)* RPAREN
   ;

term
   : function
   | VARIABLE
   ;

function
   : CONSTANT functionTuple
   | CONSTANT
   ;

functionTuple
   : LPAREN (CONSTANT | VARIABLE) (',' (CONSTANT | VARIABLE))* RPAREN
   ;


LPAREN
   : '('
   ;


RPAREN
   : ')'
   ;


AND
   : '&'
   ;


OR
   : '|'
   ;


NOT
   : '!'
   ;


FORALL
   : 'Forall'
   ;


EXISTS
   : 'Exists'
   ;


VARIABLE
   : '?' (('a' .. 'z') | ('0' .. '9')) CHARACTER*
   ;


CONSTANT
   : (('a' .. 'z') | ('0' .. '9')) CHARACTER*
   ;


PREPOSITION
   : ('A' .. 'Z') CHARACTER*
   ;


fragment CHARACTER
   : ('0' .. '9' | 'a' .. 'z' | 'A' .. 'Z' | '_')
   ;


WS
   : (' ' | '\t' | '\r' | '\n') + -> skip
   ;
