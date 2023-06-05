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

lexer grammar SpassLexer;

Special_symbol : '-' ;
And: 'and' ;
Author: 'author' ;
Axioms: 'axioms' ;
Begin_problem: 'begin_problem' ;
Clause: 'clause' ;
CloseB: ']' ;
CloseBc: '}' ;
CloseP: ')' ;
ClR: 'ClR' ;
Cnf: 'cnf' ;
Colon: ':' ;
Comma: ',' ;
Con: 'Con' ;
Conjectures: 'conjectures' ;
Date: 'date' ;
Description: 'description' ;
Dnf: 'dnf' ;
Dot: '.' ;
EmS: 'EmS' ;
End_of_list: 'end_of_list' ;
End_problem: 'end_problem' ;
EqF: 'EqF' ;
EqR: 'EqR' ;
Equal: 'equal' ;
Equiv: 'equiv' ;
Exists: 'exists' ;
Fac: 'Fac' ;
False_: 'false' ;
Forall: 'forall' ;
Formula: 'formula' ;
Freely: 'freely' ;
Functions: 'functions' ;
Generated_by: 'generated_by' ;
GeR: 'GeR' ;
Hypothesis: 'hypothesis' ;
Implied: 'implied' ;
Implies: 'implies' ;
Inp: 'Inp' ;
KIV: 'KIV' ;
LEM: 'LEM' ;
List_of_clauses: 'list_of_clauses' ;
List_of_declarations: 'list_of_declarations' ;
List_of_descriptions: 'list_of_descriptions' ;
List_of_formulae: 'list_of_formulae' ;
List_of_general_settings: 'list_of_general_settings' ;
List_of_proof: 'list_of_proof' ;
List_of_settings: 'list_of_settings' ;
List_of_symbols: 'list_of_symbols' ;
Logic: 'logic' ;
MOne: '-1' ;
MPm: 'MPm' ;
Name: 'name' ;
Not: 'not' ;
Obv: 'Obv' ;
OHy: 'OHy' ;
OpenB: '[' ;
OpenBc: '{' ;
OpenP: '(' ;
OPm: 'OPm' ;
Or: 'or' ;
OTTER: 'OTTER' ;
Predicate: 'predicate' ;
Predicates: 'predicates' ;
PROTEIN: 'PROTEIN' ;
Rew: 'Rew' ;
RRE: 'RRE' ;
Satisfiable: 'satisfiable' ;
SATURATE: 'SATURATE' ;
SETHEO: 'SETHEO' ;
SHy: 'SHy' ;
SoR: 'SoR' ;
Sort: 'sort' ;
Sorts: 'sorts' ;
SPASS: 'SPASS' ;
SpL: 'SpL' ;
Splitlevel: 'splitlevel' ;
SPm: 'SPm' ;
SpR: 'SpR' ;
Spt: 'Spt' ;
SSi: 'SSi' ;
Status: 'status' ;
Step: 'step' ;
Subsort: 'subsort' ;
Ter: 'Ter' ;
ThreeTAP: '3TAP' ;
True_: 'true' ;
UnC: 'UnC' ;
Unknown: 'unknown' ;
Unsatisfiable: 'unsatisfiable' ;
URR: 'URR' ;
Version: 'version' ;
WS : [ \n\r\t]+ -> channel(HIDDEN) ;
Open: '{*' -> pushMode(TextMode) ;
Identifier : Letter (Letter | Digit | Special_symbol)* ;
Letter : 'a' .. 'z' | 'A' .. 'Z' ;
Digit : '0' .. '9' ;

mode TextMode;
Close: '*}' -> popMode ;
JustText: (~'*' | '*' ~'}' )+ ;
