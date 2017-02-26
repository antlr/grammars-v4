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


grammar Erlang;

forms : form+ EOF ;

form : (attribute | function | ruleClauses) '.' ;

/// Tokens

tokAtom : TokAtom ;
TokAtom : [a-z@][0-9a-zA-Z_@]*
        | '\'' ( '\\' (~'\\'|'\\') | ~[\\'] )* '\'' ;

tokVar : TokVar ;
TokVar : [A-Z_][0-9a-zA-Z_]* ;

tokFloat : TokFloat ;
TokFloat : '-'? [0-9]+ '.' [0-9]+  ([Ee] [+-]? [0-9]+)? ;

tokInteger : TokInteger ;
TokInteger : '-'? [0-9]+ ('#' [0-9a-zA-Z]+)? ;

tokChar : TokChar ;
TokChar : '$' ('\\'? ~[\r\n] | '\\' [0-9] [0-9] [0-9]) ;

tokString : TokString ;
TokString : '"' ( '\\' (~'\\'|'\\') | ~[\\"] )* '"' ;

// antlr4 would not accept spec as an Atom otherwise.
AttrName : '-' ('spec' | 'callback') ;

Comment : '%' ~[\r\n]* '\r'? '\n' -> skip ;

WS : [ \t\r\n]+ -> skip ;



attribute : '-' tokAtom                           attrVal
          | '-' tokAtom                      typedAttrVal
          | '-' tokAtom                  '(' typedAttrVal ')'
          | AttrName                           typeSpec
          ;


/// Typing

typeSpec :     specFun typeSigs
         | '(' specFun typeSigs ')'
         ;

specFun :             tokAtom
        | tokAtom ':' tokAtom
// The following two are retained only for backwards compatibility;
// they are not part of the EEP syntax and should be removed.
        |             tokAtom '/' tokInteger '::'
        | tokAtom ':' tokAtom '/' tokInteger '::'
        ;

typedAttrVal : expr ','  typedRecordFields
             | expr '::' topType
             ;

typedRecordFields : '{' typedExprs '}' ;

typedExprs : typedExpr
           | typedExpr  ',' typedExprs
           | expr       ',' typedExprs
           | typedExpr  ','      exprs ;

typedExpr : expr '::' topType ;

typeSigs : typeSig (';' typeSig)* ;

typeSig : funType ('when' typeGuards)? ;

typeGuards : typeGuard (',' typeGuard)* ;

typeGuard : tokAtom '(' topTypes ')'
          | tokVar '::' topType ;

topTypes : topType (',' topType)* ;

topType : (tokVar '::')? topType100 ;

topType100 : type200 ('|' topType100)? ;

type200 : type300 ('..' type300)? ;

type300 : type300 addOp type400
        |               type400 ;

type400 : type400 multOp type500
        |                type500 ;

type500 : prefixOp? type ;

type : '(' topType ')'
     | tokVar
     | tokAtom
     | tokAtom             '('          ')'
     | tokAtom             '(' topTypes ')'
     | tokAtom ':' tokAtom '('          ')'
     | tokAtom ':' tokAtom '(' topTypes ')'
     | '['                   ']'
     | '[' topType           ']'
     | '[' topType ',' '...' ']'
     | '{'          '}'
     | '{' topTypes '}'
     | '#' tokAtom '{'            '}'
     | '#' tokAtom '{' fieldTypes '}'
     | binaryType
     | tokInteger
     | 'fun' '('            ')'
     | 'fun' '(' funType100 ')' ;

funType100 : '(' '...' ')' '->' topType
           | funType ;

funType : '(' (topTypes)? ')' '->' topType ;

fieldTypes : fieldType (',' fieldType)* ;

fieldType : tokAtom '::' topType ;

binaryType : '<<'                             '>>'
           | '<<' binBaseType                 '>>'
           | '<<'                 binUnitType '>>'
           | '<<' binBaseType ',' binUnitType '>>'
           ;

binBaseType : tokVar ':'            type ;

binUnitType : tokVar ':' tokVar '*' type ;



/// Exprs

attrVal :     expr
        | '(' expr           ')'
        |     expr ',' exprs
        | '(' expr ',' exprs ')' ;

function : functionClause (';' functionClause)* ;

functionClause : tokAtom clauseArgs clauseGuard clauseBody ;


clauseArgs : argumentList ;

clauseGuard : ('when' guard)? ;

clauseBody : '->' exprs ;


expr : 'catch' expr
     | expr100 ;

expr100 : expr150 (('=' | '!') expr150)* ;

expr150 : expr160 ('orelse' expr160)* ;

expr160 : expr200 ('andalso' expr200)* ;

expr200 : expr300 (compOp expr300)? ;

expr300 : expr400 (listOp expr400)* ;

expr400 : expr500 (addOp expr500)* ;

expr500 : expr600 (multOp expr600)* ;

expr600 : prefixOp? expr700 ;

expr700 : functionCall
        | recordExpr
        | expr800 ;

expr800 : exprMax (':' exprMax)? ;

exprMax : tokVar
        | atomic
        | list
        | binary
        | listComprehension
        | binaryComprehension
        | tuple
      //  | struct
        | '(' expr ')'
        | 'begin' exprs 'end'
        | ifExpr
        | caseExpr
        | receiveExpr
        | funExpr
        | tryExpr
        ;

list : '['      ']'
     | '[' expr tail
     ;
tail :          ']'
     | '|' expr ']'
     | ',' expr tail
     ;

binary : '<<'             '>>'
       | '<<' binElements '>>' ;

binElements : binElement (',' binElement)* ;

binElement : bitExpr optBitSizeExpr optBitTypeList ;

bitExpr : prefixOp? exprMax ;

optBitSizeExpr : (':' bitSizeExpr)? ;

optBitTypeList : ('/' bitTypeList)? ;

bitTypeList : bitType ('-' bitType)* ;

bitType : tokAtom (':' tokInteger)? ;

bitSizeExpr : exprMax ;


listComprehension :   '['  expr   '||' lcExprs ']' ;

binaryComprehension : '<<' binary '||' lcExprs '>>' ;

lcExprs : lcExpr (',' lcExpr)* ;

lcExpr : expr
       | expr   '<-' expr
       | binary '<=' expr
       ;

tuple : '{' exprs? '}' ;


/* struct : tokAtom tuple ; */


/* N.B. This is called from expr700.
   N.B. Field names are returned as the complete object, even if they are
   always atoms for the moment, this might change in the future.           */

recordExpr : exprMax?   '#' tokAtom ('.' tokAtom | recordTuple)
           | recordExpr '#' tokAtom ('.' tokAtom | recordTuple)
           ;

recordTuple : '{' recordFields? '}' ;

recordFields : recordField (',' recordField)* ;

recordField : (tokVar | tokAtom) '=' expr ;


/* N.B. This is called from expr700. */

functionCall : expr800 argumentList ;


ifExpr : 'if' ifClauses 'end' ;

ifClauses : ifClause (';' ifClause)* ;

ifClause : guard clauseBody ;


caseExpr : 'case' expr 'of' crClauses 'end' ;

crClauses : crClause (';' crClause)* ;

crClause : expr clauseGuard clauseBody ;


receiveExpr : 'receive' crClauses                         'end'
            | 'receive'           'after' expr clauseBody 'end'
            | 'receive' crClauses 'after' expr clauseBody 'end'
            ;


funExpr : 'fun' tokAtom '/' tokInteger
        | 'fun' atomOrVar ':' atomOrVar '/' integerOrVar
        | 'fun' funClauses 'end'
        ;

atomOrVar : tokAtom | tokVar ;

integerOrVar : tokInteger | tokVar ;


funClauses : funClause (';' funClause)* ;

funClause : argumentList clauseGuard clauseBody ;


tryExpr : 'try' exprs ('of' crClauses)? tryCatch ;

tryCatch : 'catch' tryClauses               'end'
         | 'catch' tryClauses 'after' exprs 'end'
         |                    'after' exprs 'end' ;

tryClauses : tryClause (';' tryClause)* ;

tryClause : (atomOrVar ':')? expr clauseGuard clauseBody ;



argumentList : '(' exprs? ')' ;

exprs : expr (',' expr)* ;

guard : exprs (';' exprs)* ;

atomic : tokChar
       | tokInteger
       | tokFloat
       | tokAtom
       | (tokString)+
       ;

prefixOp : '+'
         | '-'
         | 'bnot'
         | 'not'
         ;

multOp : '/'
       | '*'
       | 'div'
       | 'rem'
       | 'band'
       | 'and'
       ;

addOp : '+'
      | '-'
      | 'bor'
      | 'bxor'
      | 'bsl'
      | 'bsr'
      | 'or'
      | 'xor'
      ;

listOp : '++'
       | '--'
       ;

compOp : '=='
       | '/='
       | '=<'
       | '<'
       | '>='
       | '>'
       | '=:='
       | '=/='
       ;


ruleClauses : ruleClause (';' ruleClause)* ;

ruleClause : tokAtom clauseArgs clauseGuard ruleBody ;

ruleBody : ':-' lcExprs ;

