/* MIT License

Copyright (c) 2022 Ken Domino

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE. */

parser grammar SpassParser;

options { tokenVocab=SpassLexer; }

problem : 'begin_problem' '(' identifier ')' '.' description logical_part settings* 'end_problem' '.' EOF ;
description : 'list_of_descriptions' '.'
 'name' '(' ( Open text_ Close )? ')' '.'
 'author' '(' ( Open text_ Close )? ')' '.'
 ( 'version' '(' ( Open text_ Close )? ')' '.' )?
 ( 'logic' '(' ( Open text_ Close )? ')' '.' )?
 'status' '(' log_state ')' '.'
 'description' '(' ( Open text_ Close )? ')' '.'
 ( 'date' '(' ( Open text_ Close )? ')' '.' )?
 'end_of_list' '.'
 ;
log_state : 'satisfiable' | 'unsatisfiable' | 'unknown' ;
logical_part : symbol_list? declaration_list? formula_list* clause_list* proof_list* ;
symbol_list : 'list_of_symbols' '.'
 ( 'functions' '[' ( fun_sym | '(' fun_sym ',' arity ')' ) ( ',' ( fun_sym | '(' fun_sym ',' arity ')' ) )* ']' '.' )?
 ( 'predicates' '[' ( pred_sym | '(' pred_sym ',' arity ')' ) ( ',' ( pred_sym | '(' pred_sym ',' arity ')' ) )* ']' '.' )?
 ( 'sorts' '[' sort_sym ( ',' sort_sym )* ']' '.' )?
 'end_of_list' '.'
 ;
declaration_list : 'list_of_declarations' '.'
 declaration*
 'end_of_list' '.'
 ;
declaration : subsort_decl | term_decl | pred_decl | gen_decl ;
gen_decl : 'sort' sort_sym 'freely'? 'generated_by' func_list '.' ;
func_list : '[' fun_sym ( ',' fun_sym )* ']' ;
subsort_decl : 'subsort' '(' sort_sym ',' sort_sym ')' '.' ;
term_decl : 'forall' '(' term_list ',' term ')' '.' | term '.' ;
pred_decl : 'predicate' '(' pred_sym ( ',' sort_sym )+ ')' '.' ;
sort_sym : identifier ;
pred_sym : identifier ;
fun_sym : identifier ;
formula_list : 'list_of_formulae' '(' origin_type ')' '.'
 ( 'formula' '(' term? ( ',' label )? ')' '.' )*
 'end_of_list' '.'
 ;
origin_type : 'axioms' | 'conjectures' ;
label : identifier | number ;
term : quant_sym '(' term_list ',' term ')' | symbol | symbol '(' term ( ',' term )* ')' ;
term_list : '[' term ( ',' term )* ']' ;
quant_sym : 'forall' | 'exists' | identifier ;
symbol : 'equal' | 'true' | 'false' | 'or' | 'and' | 'not' | 'implies' | 'implied' | 'equiv' | identifier ;
clause_list : 'list_of_clauses' '(' origin_type ',' clause_type ')' '.'
 ( 'clause' '(' ( cnf_clause | dnf_clause)? ( ',' label )? ')' '.' )*
 'end_of_list' '.'
 ;
clause_type : 'cnf' | 'dnf' ;
cnf_clause : 'forall' '(' term_list ',' cnf_clause_body ')' | cnf_clause_body ;
dnf_clause : 'exists' '(' term_list ',' dnf_clause_body ')' | dnf_clause_body ;
cnf_clause_body : 'or' '(' term ( ',' term )* ')' ;
dnf_clause_body : 'and' '(' term ( ',' term )* ')' ;
proof_list : 'list_of_proof' ( '(' proof_type ( ',' assoc_list )? ')' )? '.'
 ( 'step' '(' reference ',' result ',' rule_appl ',' parent_list ( ',' assoc_list )? ')' '.' )*
 'end_of_list' '.'
 ;
reference : term | identifier | user_reference ;
result : term | user_result ;
rule_appl : term | identifier | user_rule_appl ;
parent_list : '[' parent_ ( ',' parent_ )* ']' ;
parent_ : term | identifier | user_parent ;
assoc_list : '[' key ':' value ( ',' key ':' value )* ']' ;
key : term | identifier | user_key ;
value : term | identifier | user_value ;
proof_type : identifier | user_proof_type ;
user_reference : number ;
user_result : cnf_clause ;
user_rule_appl : 'GeR' | 'SpL' | 'SpR' | 'EqF' | 'Rew' | 'Obv' | 'EmS' | 'SoR' | 'EqR' | 'MPm' | 'SPm' | 'OPm' | 'SHy' | 'OHy' | 'URR' | 'Fac' | 'Spt' | 'Inp' | 'Con' | 'RRE' | 'SSi' | 'ClR' | 'UnC' | 'Ter' ;
user_parent : number ;
user_proof_type : 'SPASS' ;
user_key : 'splitlevel' ;
user_value : number ;
settings : 'list_of_general_settings' setting_entry+ 'end_of_list' '.'
 | 'list_of_settings' '(' setting_label ')' '.' ( Open text_ Close )? 'end_of_list' '.'
 ;
setting_entry : 'hypothesis' '[' label ( ',' label )* ']' '.' ;
setting_label : 'KIV' | 'LEM' | 'OTTER' | 'PROTEIN' | 'SATURATE' | '3TAP' | 'SETHEO' | 'SPASS' ;
identifier : Identifier ;
arity : '-1' | number ;
number : Digit+ ;
text_ : JustText ;

