/*
* FOL rewritten for Antlr4 by Kamil Kapałka
*
*/

grammar fol;

/*------------------------------------------------------------------
 * PARSER RULES
 *------------------------------------------------------------------*/

 condition
   :formula (ENDLINE formula)* ENDLINE* EOF
   ;
 formula
   : formula bin_connective formula 
   | NOT formula bin_connective formula
   | NOT formula 
   | FORALL LPAREN variable RPAREN formula 
   | EXISTS LPAREN variable RPAREN formula
   | pred_constant LPAREN term (separator term)* RPAREN
   | term EQUAL term
   ;

term
   : ind_constant
   | variable
   | func_constant LPAREN term (separator term)* RPAREN
   ;

bin_connective
   : CONJ
   | DISJ
   | IMPL
   | BICOND
   ;
//used in FORALL|EXISTS and following predicates
variable
   : '?'CHARACTER*
   ;
//predicate constant - np. _isProfesor(?x)   
pred_constant
   : '_'CHARACTER*
   ;
//individual constant - used in single predicates
ind_constant
   :'#'CHARACTER*
   ;
//used to create functions, np. .presidentOf(?America) = #Trump
func_constant
   :'.'CHARACTER*
   ;

LPAREN
   :'('
   ;
RPAREN
   :')'
   ;
separator
   :','
   ;
EQUAL
   :'='
   ;
NOT
   :'!'
   ;
FORALL
   :'Forall'
   ;
EXISTS
   :'Exists'
   ;
CHARACTER
   :('0' .. '9' | 'a' .. 'z' | 'A' .. 'Z')
   ;
CONJ
   :'\\/'
   ;
DISJ
   :'^'
   ;
IMPL
   :'->'
   ;
BICOND
   :'<->'
   ;
ENDLINE
   :('\r'|'\n')+
   ;
WHITESPACE
   :(' '|'\t')+->skip
   ;