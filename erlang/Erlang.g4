/*
 [The "BSD licence"]
 Copyright (c) 2013 Terence Parr
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:
 1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.
 3. The name of the author may not be used to endorse or promote products
    derived from this software without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
*/

// An ANTLR4 Grammar of Erlang R16B01 made by Pierre Fenoll from
// https://github.com/erlang/otp/blob/maint/lib/stdlib/src/erl_parse.yrl

// Update to Erlang/OTP 23.3

// $antlr-format alignTrailingComments true, columnLimit 150, minEmptyLines 1, maxEmptyLinesToKeep 1, reflowComments false, useTab false
// $antlr-format allowShortRulesOnASingleLine false, allowShortBlocksOnASingleLine true, alignSemicolons hanging, alignColons hanging

grammar Erlang;

forms
    : form+ EOF
    ;

form
    : (attribute | function_) '.'
    ;

/// Tokens

fragment DIGIT
    : [0-9]
    ;

fragment LOWERCASE
    : [a-z]
    | '\u00df' ..'\u00f6'
    | '\u00f8' ..'\u00ff'
    ;

fragment UPPERCASE
    : [A-Z]
    | '\u00c0' ..'\u00d6'
    | '\u00d8' ..'\u00de'
    ;

tokAtom
    : TokAtom
    ;

TokAtom
    : LOWERCASE (DIGIT | LOWERCASE | UPPERCASE | '_' | '@')*
    | '\'' ( '\\' (~'\\' | '\\') | ~[\\'])* '\''
    ;

tokVar
    : TokVar
    ;

TokVar
    : (UPPERCASE | '_') (DIGIT | LOWERCASE | UPPERCASE | '_' | '@')*
    ;

tokFloat
    : TokFloat
    ;

TokFloat
    : '-'? DIGIT+ '.' DIGIT+ ([Ee] [+-]? DIGIT+)?
    ;

tokInteger
    : TokInteger
    ;

TokInteger
    : '-'? DIGIT+ ('#' (DIGIT | [a-zA-Z])+)?
    ;

tokChar
    : TokChar
    ;

TokChar
    : '$' ('\\'? ~[\r\n] | '\\' DIGIT DIGIT DIGIT)
    ;

tokString
    : TokString
    ;

TokString
    : '"' ('\\' (~'\\' | '\\') | ~[\\"])* '"'
    ;

// antlr4 would not accept spec as an Atom otherwise.
AttrName
    : '-' ('spec' | 'callback')
    ;

Comment
    : '%' ~[\r\n]* '\r'? '\n' -> skip
    ;

WS
    : [\u0000-\u0020\u0080-\u00a0]+ -> skip
    ;

attribute
    : '-' tokAtom attrVal
    | '-' tokAtom typedAttrVal
    | '-' tokAtom '(' typedAttrVal ')'
    | AttrName typeSpec
    ;

/// Typing

typeSpec
    : specFun typeSigs
    | '(' specFun typeSigs ')'
    ;

specFun
    : tokAtom
    | tokAtom ':' tokAtom
    ;

typedAttrVal
    : expr ',' typedRecordFields
    | expr '::' topType
    ;

typedRecordFields
    : '{' typedExprs '}'
    ;

typedExprs
    : typedExpr
    | typedExpr ',' typedExprs
    | expr ',' typedExprs
    | typedExpr ',' exprs
    ;

typedExpr
    : expr '::' topType
    ;

typeSigs
    : typeSig (';' typeSig)*
    ;

typeSig
    : funType ('when' typeGuards)?
    ;

typeGuards
    : typeGuard (',' typeGuard)*
    ;

typeGuard
    : tokAtom '(' topTypes ')'
    | tokVar '::' topType
    ;

topTypes
    : topType (',' topType)*
    ;

topType
    : (tokVar '::')? topType100
    ;

topType100
    : type200 ('|' topType100)?
    ;

type200
    : type300 ('..' type300)?
    ;

type300
    : type300 addOp type400
    | type400
    ;

type400
    : type400 multOp type500
    | type500
    ;

type500
    : prefixOp? type_
    ;

type_
    : '(' topType ')'
    | tokVar
    | tokAtom
    | tokAtom '(' ')'
    | tokAtom '(' topTypes ')'
    | tokAtom ':' tokAtom '(' ')'
    | tokAtom ':' tokAtom '(' topTypes ')'
    | '[' ']'
    | '[' topType ']'
    | '[' topType ',' '...' ']'
    | '#' '{' '}'
    | '#' '{' mapPairTypes '}'
    | '{' '}'
    | '{' topTypes '}'
    | '#' tokAtom '{' '}'
    | '#' tokAtom '{' fieldTypes '}'
    | binaryType
    | tokInteger
    | tokChar
    | 'fun' '(' ')'
    | 'fun' '(' funType100 ')'
    ;

funType100
    : '(' '...' ')' '->' topType
    | funType
    ;

funType
    : '(' (topTypes)? ')' '->' topType
    ;

mapPairTypes
    : mapPairType (',' mapPairType)*
    ;

mapPairType
    : topType ('=>' | ':=') topType
    ;

fieldTypes
    : fieldType (',' fieldType)*
    ;

fieldType
    : tokAtom '::' topType
    ;

binaryType
    : '<<' '>>'
    | '<<' binBaseType '>>'
    | '<<' binUnitType '>>'
    | '<<' binBaseType ',' binUnitType '>>'
    ;

binBaseType
    : tokVar ':' type_
    ;

binUnitType
    : tokVar ':' tokVar '*' type_
    ;

/// Exprs

attrVal
    : expr
    | '(' expr ')'
    | expr ',' exprs
    | '(' expr ',' exprs ')'
    ;

function_
    : functionClause (';' functionClause)*
    ;

functionClause
    : tokAtom clauseArgs clauseGuard clauseBody
    ;

clauseArgs
    : patArgumentList
    ;

clauseGuard
    : ('when' guard_)?
    ;

clauseBody
    : '->' exprs
    ;

expr
    : 'catch' expr
    | expr100
    ;

expr100
    : expr150 (('=' | '!') expr150)*
    ;

expr150
    : expr160 ('orelse' expr160)*
    ;

expr160
    : expr200 ('andalso' expr200)*
    ;

expr200
    : expr300 (compOp expr300)?
    ;

expr300
    : expr400 (listOp expr400)*
    ;

expr400
    : expr500 (addOp expr500)*
    ;

expr500
    : expr600 (multOp expr600)*
    ;

expr600
    : prefixOp expr600
    | expr650
    ;

expr650
    : mapExpr
    | expr700
    ;

expr700
    : functionCall
    | recordExpr
    | expr800
    ;

expr800
    : exprMax (':' exprMax)?
    ;

exprMax
    : tokVar
    | atomic
    | list_
    | binary
    | listComprehension
    | binaryComprehension
    | tuple_
    | '(' expr ')'
    | 'begin' exprs 'end'
    | ifExpr
    | caseExpr
    | receiveExpr
    | funExpr
    | tryExpr
    ;

patExpr
    : patExpr200 ('=' patExpr)?
    ;

patExpr200
    : patExpr300 (compOp patExpr300)?
    ;

patExpr300
    : patExpr400 (listOp patExpr300)?
    ;

patExpr400
    : patExpr400 addOp patExpr500
    | patExpr500
    ;

patExpr500
    : patExpr500 multOp patExpr600
    | patExpr600
    ;

patExpr600
    : prefixOp patExpr600
    | patExpr650
    ;

patExpr650
    : mapPatExpr
    | patExpr700
    ;

patExpr700
    : recordPatExpr
    | patExpr800
    ;

patExpr800
    : patExprMax
    ;

patExprMax
    : tokVar
    | atomic
    | list_
    | binary
    | tuple_
    | '(' patExpr ')'
    ;

mapPatExpr
    : patExprMax? '#' mapTuple
    | mapPatExpr '#' mapTuple
    ;

recordPatExpr
    : '#' tokAtom ('.' tokAtom | recordTuple)
    ;

list_
    : '[' ']'
    | '[' expr tail
    ;

tail
    : ']'
    | '|' expr ']'
    | ',' expr tail
    ;

binary
    : '<<' '>>'
    | '<<' binElements '>>'
    ;

binElements
    : binElement (',' binElement)*
    ;

binElement
    : bitExpr optBitSizeExpr optBitTypeList
    ;

bitExpr
    : prefixOp? exprMax
    ;

optBitSizeExpr
    : (':' bitSizeExpr)?
    ;

optBitTypeList
    : ('/' bitTypeList)?
    ;

bitTypeList
    : bitType ('-' bitType)*
    ;

bitType
    : tokAtom (':' tokInteger)?
    ;

bitSizeExpr
    : exprMax
    ;

listComprehension
    : '[' expr '||' lcExprs ']'
    ;

binaryComprehension
    : '<<' exprMax '||' lcExprs '>>'
    ;

lcExprs
    : lcExpr (',' lcExpr)*
    ;

lcExpr
    : expr
    | expr '<-' expr
    | binary '<=' expr
    ;

tuple_
    : '{' exprs? '}'
    ;

mapExpr
    : exprMax? '#' mapTuple
    | mapExpr '#' mapTuple
    ;

mapTuple
    : '{' (mapField (',' mapField)*)? '}'
    ;

mapField
    : mapFieldAssoc
    | mapFieldExact
    ;

mapFieldAssoc
    : mapKey '=>' expr
    ;

mapFieldExact
    : mapKey ':=' expr
    ;

mapKey
    : expr
    ;

/* struct : tokAtom tuple ; */

/* N.B. This is called from expr700.
   N.B. Field names are returned as the complete object, even if they are
   always atoms for the moment, this might change in the future.           */

recordExpr
    : exprMax? '#' tokAtom ('.' tokAtom | recordTuple)
    | recordExpr '#' tokAtom ('.' tokAtom | recordTuple)
    ;

recordTuple
    : '{' recordFields? '}'
    ;

recordFields
    : recordField (',' recordField)*
    ;

recordField
    : (tokVar | tokAtom) '=' expr
    ;

/* N.B. This is called from expr700. */

functionCall
    : expr800 argumentList
    ;

ifExpr
    : 'if' ifClauses 'end'
    ;

ifClauses
    : ifClause (';' ifClause)*
    ;

ifClause
    : guard_ clauseBody
    ;

caseExpr
    : 'case' expr 'of' crClauses 'end'
    ;

crClauses
    : crClause (';' crClause)*
    ;

crClause
    : expr clauseGuard clauseBody
    ;

receiveExpr
    : 'receive' crClauses 'end'
    | 'receive' 'after' expr clauseBody 'end'
    | 'receive' crClauses 'after' expr clauseBody 'end'
    ;

funExpr
    : 'fun' tokAtom '/' tokInteger
    | 'fun' atomOrVar ':' atomOrVar '/' integerOrVar
    | 'fun' funClauses 'end'
    ;

atomOrVar
    : tokAtom
    | tokVar
    ;

integerOrVar
    : tokInteger
    | tokVar
    ;

funClauses
    : funClause (';' funClause)*
    ;

funClause
    : patArgumentList clauseGuard clauseBody
    | tokVar patArgumentList clauseGuard clauseBody
    ;

tryExpr
    : 'try' exprs ('of' crClauses)? tryCatch
    ;

tryCatch
    : 'catch' tryClauses 'end'
    | 'catch' tryClauses 'after' exprs 'end'
    | 'after' exprs 'end'
    ;

tryClauses
    : tryClause (';' tryClause)*
    ;

tryClause
    : expr clauseGuard clauseBody
    | (atomOrVar ':')? patExpr tryOptStackTrace clauseGuard clauseBody
    ;

tryOptStackTrace
    : (':' tokVar)?
    ;

argumentList
    : '(' exprs? ')'
    ;

patArgumentList
    : '(' patExprs? ')'
    ;

exprs
    : expr (',' expr)*
    ;

patExprs
    : patExpr (',' patExpr)*
    ;

guard_
    : exprs (';' exprs)*
    ;

atomic
    : tokChar
    | tokInteger
    | tokFloat
    | tokAtom
    | (tokString)+
    ;

prefixOp
    : '+'
    | '-'
    | 'bnot'
    | 'not'
    ;

multOp
    : '/'
    | '*'
    | 'div'
    | 'rem'
    | 'band'
    | 'and'
    ;

addOp
    : '+'
    | '-'
    | 'bor'
    | 'bxor'
    | 'bsl'
    | 'bsr'
    | 'or'
    | 'xor'
    ;

listOp
    : '++'
    | '--'
    ;

compOp
    : '=='
    | '/='
    | '=<'
    | '<'
    | '>='
    | '>'
    | '=:='
    | '=/='
    ;