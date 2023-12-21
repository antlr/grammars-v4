/*
 CodeQL grammar
 The MIT License (MIT).
 Copyright (c) 2023, Dijun Liu

 Permission is hereby granted, free of charge, to any person obtaining a copy
 of this software and associated documentation files (the "Software"), to deal
 in the Software without restriction, including without limitation the rights
 to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 copies of the Software, and to permit persons to whom the Software is
 furnished to do so, subject to the following conditions:

 The above copyright notice and this permission notice shall be included in
 all copies or substantial portions of the Software.

 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 THE SOFTWARE.

 CodeQL grammar built from the CodeQL Specification https://codeql.github.com/docs/ql-language-reference/ql-language-specification
*/

parser grammar CodeQLParser;

options { tokenVocab=CodeQLLexer; }

literalId
    : LOWERID
    | ATLOWERID
    | UPPERID
    | NONE
    | ANY
    | keywordAllowed
    ;

// keywords allowed named in user variable and user predicate decl
keywordAllowed
    : 'unique'
    | 'implements'
    | 'signature'
    | 'default'
    | 'abstract'
    | 'cached'
    | 'external'
    | 'extensible'
    | 'final'
    | 'transient'
    | 'library'
    | 'private'
    | 'deprecated'
    | 'override'
    | 'additional'
    | 'query'
    | 'pragma'
    | 'inline'
    | 'inline_late'
    | 'noinline'
    | 'nomagic'
    | 'noopt'
    | 'assume_small_delta'
    | 'language'
    | 'monotonicAggregates'
    | 'bindingset'
    | 'only_bind_out'
    | 'only_bind_into'
    ;

// keyword can used in predicate call
keywordPredCallAllowed
    : keywordAllowed
    | 'any'
    | 'none'
    ;

// varname ::= lowerId
varname
    : LOWERID
    | keywordAllowed
    ;

//ql ::= QL_DOC? moduleBody
ql
    : QL_DOC? moduleBody EOF
    ;

//module ::= annotation* 'module' modulename parameters? implements? '{' moduleBody '}'
module
    : annotation* 'module' modulename parameters? implements? '{' moduleBody '}'
    ;

//parameters ::= '<' signatureExpr parameterName (',' signatureExpr parameterName)* '>'
parameters
    : '<' signatureExpr parameterName (',' signatureExpr parameterName)* '>'
    ;

//implements ::= IMPLEMENTS moduleSignatureExpr (',' moduleSignatureExpr)*
implements
    : IMPLEMENTS moduleSignatureExpr (',' moduleSignatureExpr)*
    ;

//moduleBody ::= (import | predicate | class | module | alias | select)*
moduleBody
    : (importDeclaration | predicate | signature | datatype | typeunion | class | module | alias | select)*
    ;

//import ::= annotations 'import' importModuleExpr ('as' modulename)?
importDeclaration
    :  annotations 'import' importModuleExpr ('as' modulename)?
    ;

//qualId ::= SIMPLEID | qualId '.' SIMPLEID
simpleId
    : LOWERID
    | UPPERID
    ;

qualId
    : simpleId
    | qualId '.' simpleId
    ;

//importModuleExpr ::= qualId | importModuleExpr '::' modulename arguments?
importModuleExpr
    : qualId
    | importModuleExpr '::' modulename arguments?
    ;

//arguments ::= '<' argument (',' argument)* '>'
arguments
    : '<' argument (',' argument)* '>'
    ;

//argument ::= moduleExpr | type | predicateRef '/' int
argument
    : moduleExpr
    | type
    | predicateRef '/' INT_LITERAL
    ;

//signature ::= predicateSignature | typeSignature | moduleSignature
signature
    : predicateSignature
    | typeSignature
    | moduleSignature
    ;

//predicateSignature ::= QL_DOC? annotations SIGNATURE head ';'
predicateSignature
    : QL_DOC? annotation* SIGNATURE head ';'
    ;

//typeSignature ::= QL_DOC? annotations SIGNATURE 'class' classname ('extends' type (',' type)*)? (';' | '{' signaturePredicate* '}')
typeSignature
    : QL_DOC? annotations SIGNATURE 'class' classname ('extends' type (',' type)*)? (';' | '{' signaturePredicate* '}')
    ;

//moduleSignature ::= QL_DOC? annotation* SIGNATURE 'module' moduleSignatureName parameters? '{' moduleSignatureBody '}'
moduleSignature
    : QL_DOC? annotation* SIGNATURE 'module' moduleSignatureName parameters? '{' moduleSignatureBody '}'
    ;

//moduleSignatureBody ::= (signaturePredicate | defaultPredicate | signatureType)*
moduleSignatureBody
    : (signaturePredicate | defaultPredicate | signatureType)*
    ;

//signaturePredicate ::= QL_DOC? annotations head ';'
signaturePredicate
    : QL_DOC? annotations head ';'
    ;

//defaultPredicate ::= QL_DOC? annotations DEFAULT head '{' formula '}'
defaultPredicate
    : QL_DOC? annotations DEFAULT head '{' formula '}'
    ;

//signatureType ::= QL_DOC? annotations 'class' classname ('extends' type (',' type)*)? '{' signaturePredicate* '}'
signatureType
    : QL_DOC? annotations 'class' classname ('extends' type (',' type)*)? (';' | '{' signaturePredicate* '}')
    ;

//select ::= ('from' var_decls)? ('where' formula)? 'select' as_exprs ('order' 'by' orderbys)?
select
    : ('from' var_decls)? ('where' formula)? 'select' as_exprs ('order' 'by' orderbys)?
    ;

//as_exprs ::= as_expr (',' as_expr)*
as_exprs
    : as_expr (',' as_expr)*
    ;

//as_expr ::= expr ('as' lowerId)?
as_expr
    : expr ('as' LOWERID)?
    ;

//orderbys ::= orderby (',' orderby)*
orderbys
    : orderby (',' orderby)*
    ;

//orderby ::= lowerId ('asc' | 'desc')?
orderby
    : LOWERID ('asc' | 'desc')?
    ;

dtName
    : UPPERID
    ;

dtBranches
    : dtBranche ('or' dtBranche)*
    ;

dtBranche
    : dtBranchName'(' var_decls ')' ('{' formula '}')?
    ;

dtBranchName
    : UPPERID
    ;

datatype
    : QL_DOC? annotations 'newtype' dtName '=' dtBranches
    ;

unionBranches
    : type ('or' type)*
    ;

typeunion
    : QL_DOC? annotations 'class' UPPERID '=' unionBranches ';'
    ;

//predicate ::= QL_DOC? annotations head optbody
predicate
    : QL_DOC? annotations head optbody
    ;

//annotations ::= annotation*
annotations
    : annotation*
    ;

//annotation ::= simpleAnnotation | argsAnnotation
annotation
    : simpleAnnotation
    | argsAnnotation
    ;

//simpleAnnotation ::= ABSTRACT
//                 |   CACHED
//                 |   EXTERNAL
//                 |   EXTENSIBLE
//                 |   FINAL
//                 |   TRANSIENT
//                 |   LIBRARY
//                 |   PRIVATE
//                 |   DEPRECATED
//                 |   OVERRIDE
//                 |   ADDITIONAL
//                 |   QUERY
simpleAnnotation
    : ABSTRACT
    | CACHED
    | EXTERNAL
    | EXTENSIBLE
    | FINAL
    | TRANSIENT
    | LIBRARY
    | PRIVATE
    | DEPRECATED
    | OVERRIDE
    | ADDITIONAL
    | QUERY
    ;

//argsAnnotation
//    : PRAGMA '[' (INLINE | INLINE_LATE | NOINLINE | NOMAGIC | NOOPT | ASSUME_SMALL_DELTA) ']'
//    | LANGUAGE '[' 'monotonicAggregates' ']'
//    | BINDINGSET '[' (variable ( ',' variable)*)? ']'
//    ;
pragmaArg
    :INLINE | INLINE_LATE | NOINLINE | NOMAGIC | NOOPT | ASSUME_SMALL_DELTA
    ;


argsAnnotation
    : PRAGMA '[' pragmaArg (',' pragmaArg)* ']'
    | LANGUAGE '[' 'monotonicAggregates' ']'
    | BINDINGSET '[' (variable ( ',' variable)*)? ']'
    ;

//head ::= ('predicate' | type) predicateName '(' var_decls ')'
head
    : ('predicate' | type) predicateName '(' var_decls ')'
    ;

//optbody ::= ';'
//        |  '{' formula '}'
//        |  '=' literalId '(' (predicateRef '/' int (',' predicateRef '/' int)*)? ')' '(' (exprs)? ')'
optbody
    : ';'
    |  '{' formula '}'
    |  '=' literalId '(' (predicateRef '/' INT_LITERAL (',' predicateRef '/' INT_LITERAL)*)? ')' '(' (exprs)? ')'
    ;

//class ::= QL_DOC? annotations 'class' classname ('extends' type (',' type)*)? ('instanceof' type (',' type)*)?  '{' member* '}'
class
    : QL_DOC? annotations 'class' classname ('extends' type (',' type)*)? ('instanceof' type (',' type)*)?  '{' member* '}'
    ;

//member ::= character | predicate | field
member
    : character
    | predicate
    | field
    ;

//character ::= QL_DOC? annotations classname "(" ")" "{" formula "}"
character
    : QL_DOC? annotations classname '(' ')' '{' formula '}'
    ;

//field ::= QL_DOC? annotations var_decl ";"
field
    : QL_DOC? annotations var_decl ';'
    ;

//moduleExpr ::= modulename arguments? | moduleExpr "::" modulename arguments?
moduleExpr
    : modulename arguments?
    | moduleExpr '::' modulename arguments?
    ;

//moduleSignatureExpr ::= (moduleExpr "::")? moduleSignatureName arguments?
moduleSignatureExpr
    : (moduleExpr '::')? moduleSignatureName arguments?
    ;

//signatureExpr : (moduleExpr "::")? SIMPLEID ("/" Integer | arguments)?
signatureExpr
    : (moduleExpr '::')? simpleId ('/' INT_LITERAL | arguments)?
    ;

//type ::= (moduleExpr "::")? classname | dbasetype | "boolean" | "date" | "float" | "int" | "string"
type
    : (moduleExpr '::')? classname
    | dbbasetype
    | 'boolean'
    | 'date'
    | 'float'
    | 'int'
    | 'string'
    ;

//exprs ::= expr ("," expr)*
exprs
    : expr (',' expr)*
    ;

//alias ::= QL_DOC? annotations "predicate" literalId "=" predicateRef "/" int ";"
//      |  QL_DOC? annotations "class" classname "=" type ";"
//      |  QL_DOC? annotations "module" modulename "=" moduleExpr ";"
alias
    : QL_DOC? annotations 'predicate' predicateName '=' predicateRef '/' INT_LITERAL ';'
    | QL_DOC? annotations 'class' classname '=' type ';'
    | QL_DOC? annotations 'module' modulename '=' moduleExpr ';'
    ;

//var_decls ::= (var_decl ("," var_decl)*)?
var_decls
    : (var_decl (',' var_decl)*)?
    ;

//var_decl ::= type lowerId
var_decl
    : type varname
    ;

//formula ::= fparen
//        |   disjunction
//        |   conjunction
//        |   implies
//        |   ifthen
//        |   negated
//        |   quantified
//        |   comparison
//        |   instanceof
//        |   inrange
//        |   call
formula
    : conjunction_formula ('or' formula)*
    ;

formula_base
    : fparen
    | ifthen
    | negated
    | quantified
    | comparison
    | instanceof
    | inrange
    | call
    ;

//disjunction_formula
//    : conjunction_formula ('or' formula)*
//    ;

conjunction_formula
    : implies_formula ('and' formula)*
    ;

implies_formula
    : formula_base ('implies' formula)*
    ;

//fparen ::= "(" formula ")"
fparen
    : '(' formula ')'
    ;

//disjunction ::= formula "or" formula
disjunction
    : formula 'or' formula
    ;

//conjunction ::= formula "and" formula
conjunction
    : formula 'and' formula
    ;

//implies ::= formula "implies" formula
implies
    : formula 'implies' formula
    ;

//ifthen ::= "if" formula "then" formula "else" formula
ifthen
    : 'if' formula 'then' formula 'else' formula
    ;

//negated ::= "not" formula
negated
    : 'not' formula
    ;
//
//quantified ::= "exists" "(" expr ")"
//           |   "exists" "(" var_decls ("|" formula)? ("|" formula)? ")"
//           |   "forall" "(" var_decls ("|" formula)? "|" formula ")"
//           |   "forex"  "(" var_decls ("|" formula)? "|" formula ")"
quantified
    : 'exists' '(' expr ')'
    | 'exists' '(' var_decls ('|' formula)? ('|' formula)? ')'
    | 'forall' '(' var_decls ('|' formula)? '|' formula ')'
    | 'forex'  '(' var_decls ('|' formula)? '|' formula ')'
    ;
//
//comparison ::= expr compop expr
comparison
    : expr compop expr
    ;

//compop ::= "=" | "!=" | "<" | ">" | "<=" | ">="
compop
    : '='
    | '!='
    | '<'
    | '>'
    | '<='
    | '>='
    ;

//instanceof ::= expr "instanceof" type
instanceof
    : expr 'instanceof' type
    ;

//inrange ::= expr "in" (range | setliteral)
inrange
    : expr 'in' (range | setliteral)
    ;

//
//call ::= predicateRef (closure)? "(" (exprs)? ")"
//     |   primary "." predicateName (closure)? "(" (exprs)? ")"
call
    : predicateRef (closure)? '(' (exprs)? ')'
    | primary '.' predicateName (closure)? '(' (exprs)? ')'
    ;

//closure ::= "*" | "+"
closure
    : '*' | '+'
    ;
//expr ::= dontcare
//     |   unop
//     |   binop
//     |   cast
//     |   primary
expr
    : multExpr (('+' | '-') multExpr)*
    ;

multExpr
    : unaryExpr (('*' | '/' | '%') unaryExpr)*
    ;

unaryExpr
    : nonOpExpr
    | unop
    ;

nonOpExpr
    : DONTCARE
    | cast
    | primary
    ;

//primary ::= eparen
//        |   literal
//        |   variable
//        |   super_expr
//        |   postfix_cast
//        |   callwithresults
//        |   aggregation
//        |   expression_pragma
//        |   any
//        |   range
//        |   setliteral
primary
    : primaryBase (primaryPostfixOp)*
    ;

primaryPostfixOp
     : '.' '(' type ')'
     | '.' predicateName (closure)? '(' (exprs)? ')'
     ;

callwithresults_nomember
    : predicateRef (closure)? '(' (exprs)? ')'
    ;

primaryBase
    : eparen
    | literal
    | variable
    | super_expr
    | aggregation
    | expression_pragma
    | callwithresults_nomember
    | any
    | none
    | range
    | setliteral
    ;

none
    : NONE '(' ')'
    ;

//eparen ::= "(" expr ")"
eparen
    : '(' expr ')'
    ;

//literal ::= "false" | "true" | int | float | string
literal
    : 'false'
    | 'true'
    | INT_LITERAL
    | FLOAT_LITERAL
    | STRING_LITERAL
    ;

//unop ::= "+" expr
//     |   "-" expr
unop
    : '+' expr
    | '-' expr
    ;

//binop ::= expr "+" expr
//      |   expr "-" expr
//      |   expr "*" expr
//      |   expr "/" expr
//      |   expr "%" expr
binop
    : expr '+' expr
    | expr '-' expr
    | expr '*' expr
    | expr '/' expr
    | expr '%' expr
    ;

//variable ::= varname | "this" | "result"
variable
    : varname
    | 'this'
    | 'result'
    ;

//super_expr ::= "super" | type "." "super"
super_expr
    : 'super'
    | type '.' 'super'
    ;
//cast ::= "(" type ")" expr
cast
    : '(' type ')' expr
    ;
//postfix_cast ::= primary "." "(" type ")"
//postfix_cast
//    : primary '.' '(' type ')'
//    ;
//aggregation ::= aggid ("[" expr "]")? "(" var_decls ("|" (formula)? ("|" as_exprs ("order" "by" aggorderbys)?)?)? ")"
//            |   aggid ("[" expr "]")? "(" as_exprs ("order" "by" aggorderbys)? ")"
//            |   "unique" "(" var_decls "|" (formula)? ("|" as_exprs)? ")"
aggregation
    : aggid ('[' expr ']')? '(' var_decls ('|' (formula)? ('|' as_exprs ('order' 'by' aggorderbys)?)?)? ')'
    | aggid ('[' expr ']')? '(' as_exprs ('order' 'by' aggorderbys)? ')'
    | 'unique' '(' var_decls '|' (formula)? ('|' as_exprs)? ')'
    ;
//expression_pragma ::= "pragma" "[" expression_pragma_type "]" "(" expr ")"
expression_pragma
    :PRAGMA '[' expression_pragma_type ']' '(' expr ')'
    ;
//expression_pragma_type ::= "only_bind_out" | "only_bind_into"
expression_pragma_type
    : 'only_bind_out' | 'only_bind_into'
    ;
//aggid ::= "avg" | "concat" | "count" | "max" | "min" | "rank" | "strictconcat" | "strictcount" | "strictsum" | "sum"
aggid
    : 'avg' | 'concat' | 'count' | 'max' | 'min' | 'rank' | 'strictconcat' | 'strictcount' | 'strictsum' | 'sum'
    ;
//aggorderbys ::= aggorderby ("," aggorderby)*
aggorderbys
     :aggorderby (',' aggorderby)*
     ;

//aggorderby ::= expr ("asc" | "desc")?
aggorderby
    : expr ('asc' | 'desc')?
    ;
//any ::= "any" "(" var_decls ("|" (formula)? ("|" expr)?)? ")"
any
    :'any' '(' var_decls ('|' (formula)? ('|' expr)?)? ')'
    ;

//callwithresults ::= predicateRef (closure)? "(" (exprs)? ")"
//                |   primary "." predicateName (closure)? "(" (exprs)? ")"
//callwithresults
//    : predicateRef (closure)? '(' (exprs)? ')'
//    | primary '.' predicateName (closure)? '(' (exprs)? ')'
//    ;

//range ::= "[" expr ".." expr "]"
range
    : '[' expr '..' expr ']'
    ;
//setliteral ::= "[" expr ("," expr)* ","? "]"
setliteral
    : '[' expr (',' expr)* ','? ']'
    ;

//SIMPLEID ::= lowerId | upperId
//SIMPLEID
//    :
//    : LowerId
//    | UpperId
//    ;

//modulename ::= SIMPLEID
modulename
    : simpleId
    ;

//moduleSignatureName ::= upperId
moduleSignatureName
    : UPPERID
    ;

//classname ::= upperId
classname
    : UPPERID
    ;

//dbbasetype ::= atLowerId
dbbasetype
    : ATLOWERID
    ;

//predicateRef ::= (moduleExpr "::")? literalId
predicateRef
    : (moduleExpr '::')? predicateCallName
    ;

//predicateName ::= lowerId
predicateName
    : LOWERID
    | keywordAllowed
    ;

predicateCallName
    : LOWERID
    | ATLOWERID
    | UPPERID
    | keywordPredCallAllowed
    ;

//parameterName ::= SIMPLEID
parameterName
    : simpleId
    ;

