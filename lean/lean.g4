/*
BSD License

Copyright (c) 2026, Tom Everett
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.
3. Neither the name of Tom Everett nor the names of its contributors
   may be used to endorse or promote products derived from this software
   without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

// A best-effort grammar for a practical subset of the Lean 4 language
// (https://lean-lang.org/). Lean's real surface syntax is extensible via
// user-defined notation and is whitespace/indentation sensitive; this
// grammar covers the common, fixed core: imports, namespaces/sections,
// def/theorem/lemma/example/abbrev/axiom declarations, inductive types,
// structures/classes/instances, the common term language (application,
// lambda, let/have/show, if/match/do), and a generic tactic-block syntax.
grammar lean;

file_
    : command* EOF
    ;

command
    : importCmd
    | openCmd
    | namespaceCmd
    | sectionCmd
    | mutualCmd
    | variableCmd
    | abbrevCmd
    | defCmd
    | theoremCmd
    | exampleCmd
    | axiomCmd
    | inductiveCmd
    | structureCmd
    | instanceCmd
    | setOptionCmd
    | attributeCmd
    | evalCmd
    | checkCmd
    | printCmd
    ;

importCmd
    : 'import' qualIdent
    ;

openCmd
    : 'open' qualIdent+
    ;

namespaceCmd
    : 'namespace' qualIdent command* 'end' qualIdent?
    ;

sectionCmd
    : 'section' qualIdent? command* 'end' qualIdent?
    ;

mutualCmd
    : 'mutual' command+ 'end'
    ;

variableCmd
    : ('variable' | 'variables') binder+
    ;

modifier
    : 'private'
    | 'protected'
    | 'noncomputable'
    | 'partial'
    | 'unsafe'
    ;

attrs
    : '@' '[' attrList ']'
    ;

attrList
    : attrItem (',' attrItem)*
    ;

attrItem
    : qualIdent term*
    ;

attributeCmd
    : 'attribute' '[' attrList ']' qualIdent+
    ;

setOptionCmd
    : 'set_option' qualIdent term
    ;

evalCmd
    : '#eval' term
    ;

checkCmd
    : '#check' term
    ;

printCmd
    : '#print' qualIdent
    ;

declId
    : qualIdent
    ;

declValue
    : ':=' term
    | matchAlt+
    ;

abbrevCmd
    : attrs? modifier* 'abbrev' declId binder* (':' term)? declValue
    ;

defCmd
    : attrs? modifier* 'def' declId binder* (':' term)? declValue
    ;

theoremCmd
    : attrs? modifier* ('theorem' | 'lemma') declId binder* ':' term declValue
    ;

exampleCmd
    : attrs? modifier* 'example' binder* ':' term declValue
    ;

axiomCmd
    : attrs? modifier* ('axiom' | 'constant') declId binder* ':' term
    ;

inductiveCmd
    : attrs? modifier* 'inductive' declId binder* (':' term)? 'where'? ctor* derivingClause?
    ;

ctor
    : '|' IDENT binder* (':' term)?
    ;

structureCmd
    : attrs? modifier* ('structure' | 'class') declId binder* (':' term)? ('extends' term (',' term)*)? 'where'? structField* derivingClause?
    ;

structField
    : IDENT+ ':' term
    ;

instanceCmd
    : attrs? modifier* 'instance' declId? binder* (':' term)? ('where' instField+ | declValue)
    ;

instField
    : IDENT ':=' term
    ;

derivingClause
    : 'deriving' qualIdent (',' qualIdent)*
    ;

binder
    : '(' IDENT+ (':' term)? ')'
    | '{' IDENT+ (':' term)? '}'
    | '[' term ']'
    | '⦃' IDENT+ (':' term)? '⦄'
    | IDENT
    ;

matchAlt
    : '|' term (',' term)* '=>' term
    ;

doElem
    : 'let' IDENT (':' term)? ':=' term
    | IDENT '<-' term
    | 'return' term
    | term
    ;

term
    : 'fun' binder+ '=>' term                                                   # lambdaExpr
    | ('∀' | 'forall') binder+ ',' term                                         # forallExpr
    | '∃' binder+ ',' term                                                      # existsExpr
    | 'let' IDENT (':' term)? ':=' term ';'? term                               # letExpr
    | 'have' IDENT? (':' term)? ':=' term ';'? term                             # haveExpr
    | 'show' term 'from' term                                                   # showExpr
    | 'if' term 'then' term 'else' term                                         # iteExpr
    | 'match' term (',' term)* 'with' matchAlt+                                 # matchExpr
    | 'do' doElem+                                                              # doExpr
    | 'by' tacticSeq                                                            # byExpr
    | '¬' term                                                                  # notExpr
    | term op=('*' | '/' | '%') term                                           # mulExpr
    | term op=('+' | '-') term                                                 # addExpr
    | term '::' term                                                            # consExpr
    | term '++' term                                                           # appendExpr
    | term op=('=' | '==' | '≠' | '<' | '<=' | '≤' | '>' | '>=' | '≥') term     # relExpr
    | term '∧' term                                                            # andExpr
    | term '∨' term                                                            # orExpr
    | term ('->' | '→') term                                                   # arrowExpr
    | application                                                              # appExpr
    ;

application
    : atom+
    ;

atom
    : qualIdent
    | NUM
    | STRING
    | CHAR
    | '(' term (':' term)? ')'
    | '⟨' term (',' term)* '⟩'
    | '[' (term (',' term)*)? ']'
    | '_'
    | 'Type'
    | 'Prop'
    | 'Sort'
    ;

tacticSeq
    : tacticStep ((';' | '<;>')? tacticStep)*
    ;

tacticStep
    : 'have' IDENT? (':' term)? ':=' term
    | 'let' IDENT (':' term)? ':=' term
    | 'show' term
    | IDENT atom* ('with' tacticAlt+)?
    | '{' tacticSeq '}'
    ;

tacticAlt
    : '|' term (',' term)* '=>' tacticSeq
    ;

qualIdent
    : IDENT ('.' IDENT)*
    ;

IDENT
    : [\p{L}_] [\p{L}\p{Nd}_']*
    ;

NUM
    : [0-9]+ ('.' [0-9]+)?
    ;

STRING
    : '"' (ESC_SEQ | ~["\\])* '"'
    ;

CHAR
    : '\'' (ESC_SEQ | ~['\\]) '\''
    ;

fragment ESC_SEQ
    : '\\' .
    ;

LINE_COMMENT
    : '--' ~[\r\n]* -> skip
    ;

BLOCK_COMMENT
    : '/-' .*? '-/' -> skip
    ;

WS
    : [ \t\r\n]+ -> skip
    ;
