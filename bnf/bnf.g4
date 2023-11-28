/*

"Switching" BNF grammar -- parses a variety of BNF formats (but not EBNF).

MIT License

Author: Ken Domino


BNF has a long and twisted history, and there are slight variations on the metasyntax.
A grammar for each of these types results in a lot of needless grammars.
This grammar is for a number of the more popular BNF styles, but not all.

BNF was first described by [Backus 1959], and later
used in [Backus et al 1963]. The term "Backus Naur Form" was coined by
[Knuth 1964]. BNF as defined in the Algol 60 report could not describe itself.
[Rohl 1968] notes that by reversing the use of the angle brackets, one can then
describe BNF using BNF.

Replacing angle-brackets with quotes, restricting the non-terminal names to not
include spaces, and dropping the explicit concatenation operator, we arrive
at the "modern" form of BNF.

Backus, J.W. (1959). "The Syntax and Semantics of the Proposed International
Algebraic Language of Zürich ACM-GAMM Conference". Proceedings of the 
International Conference on Information Processing. UNESCO. pp. 125–132.

Backus, John W., et al. "Revised report on the algorithm language ALGOL 60."
Communications of the ACM 6.1 (1963): 1-17.

Knuth, Donald E. "Backus Normal Form vs. Backus Naur Form." Communications
of the ACM 7.12 (1964): 735-736.

Rohl, Jeffrey S. "A note on Backus Naur form." The Computer Journal 10.4 (1968): 336-337.

October 2023

*/

grammar bnf;

start_ : (delimited_nonterminal | delimited_terminal)? ws? EOF ;

delimited_nonterminal : prod_dnt+ ;
prod_dnt : ylhs assign yrhs ;
ylhs : ws? '<' .+? '>' ;
yrhs : yalt? (or yalt)* ;
yalt : yelem+? ;
yelem : ws? ('<' .+? '>' | .) ;

delimited_terminal : prod_dt+ ;
prod_dt : xlhs assign xrhs ;
xlhs : ws? ID ;
xrhs : xalt? (or xalt)* ;
xalt : xelem+? ;
xelem : ws? ('"' .*? '"' | '\'' .*? '\'' | '<' .*? '>' | . ) ;

assign : ws? (ASSIGN1 | ASSIGN2 | ASSIGN3 | ASSIGN4 ) ;
or : ws? (OR1 | OR2 | OR3) ;
ws : WS ;


X1 : '<' ;
X2 : '>' ;
X3 : '"' ;
X4 : '\'' ;
ASSIGN1 : '::=' ;
ASSIGN2 : ':' '\u{2261}' ;
ASSIGN3 : '-->' ;
ASSIGN4 : '\u{2192}' ;
OR1 : '|' ;
OR2 : 'o' '\u{203E}' 'r' '\u{203E}' ;
OR3 : '\u{2223}' ;
ID : ('a'..'z'|'A'..'Z') ('a'..'z'|'A'..'Z'|'0'..'9'|'-'|'_')+ ;
WS : [ \r\n\t]+ ;
TEXT : . ;
