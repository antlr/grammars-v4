/*
 [The "BSD licence"]
 Copyright (c) 2014 Leonardo Lucena
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
 OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
/*
   Derived from http://www.scala-lang.org/files/archive/spec/2.11/13-syntax-summary.html
 */

grammar Scala;

literal
   : '-'? IntegerLiteral
   | '-'? FloatingPointLiteral
   | BooleanLiteral
   | CharacterLiteral
   | StringLiteral
   | SymbolLiteral
   | 'null'
   ;

qualId
   : Id ('.' Id)*
   ;

ids
   : Id (',' Id)*
   ;

stableId
   : (Id | (Id '.')? 'this') '.' Id
   | (Id '.')? 'super' classQualifier? '.' Id
   ;

classQualifier
   : '[' Id ']'
   ;

type
   : functionArgTypes '=>' type
   | infixType existentialClause?
   ;

functionArgTypes
   : infixType
   | '(' (paramType (',' paramType)*)? ')'
   ;

existentialClause
   : 'forSome' '{' existentialDcl (Semi existentialDcl)* '}'
   ;

existentialDcl
   : 'type' typeDcl
   | 'val' valDcl
   ;

infixType
   : compoundType (Id compoundType)*
   ;

compoundType
   : annotType ('with' annotType)* refinement?
   | refinement
   ;

annotType
   : simpleType annotation*
   ;

simpleType
   : simpleType typeArgs
   | simpleType '#' Id
   | stableId
   | (stableId | (Id '.')? 'this') '.' 'type'
   | '(' types ')'
   ;

typeArgs
   : '[' types ']'
   ;

types
   : type (',' type)*
   ;

refinement
   : '{' refineStat (Semi refineStat)* '}'
   ;

refineStat
   : dcl
   | 'type' typeDef
   |
   ;

typePat
   : type
   ;

ascription
   : ':' infixType
   | ':' annotation +
   | ':' '_' '*'
   ;

expr
   : (bindings | 'implicit'? Id | '_') '=>' expr
   | expr1
   ;

expr1
   : 'if' '(' expr ')' expr (Semi? 'else' expr)?
   | 'while' '(' expr ')' expr
   | 'try' ('{' block '}' | expr) ('catch' '{' caseClauses '}')? ('finally' expr)?
   | 'do' expr Semi? 'while' '(' expr ')'
   | 'for' ('(' enumerators ')' | '{' enumerators '}') 'yield'? expr
   | 'throw' expr
   | 'return' expr?
   | (('new' (classTemplate | templateBody) | blockExpr | simpleExpr1 '_'?) '.') Id '=' expr
   | simpleExpr1 argumentExprs '=' expr
   | postfixExpr
   | postfixExpr ascription
   | postfixExpr 'match' '{' caseClauses '}'
   ;

postfixExpr
   : infixExpr (Id)?
   ;

infixExpr
   : prefixExpr
   | infixExpr Id infixExpr
   ;

prefixExpr
   : ('-' | '+' | '~' | '!')? ('new' (classTemplate | templateBody) | blockExpr | simpleExpr1 '_'?)
   ;

simpleExpr1
   : literal
   | stableId
   | (Id '.')? 'this'
   | '_'
   | '(' exprs? ')'
   | ('new' (classTemplate | templateBody) | blockExpr) '.' Id
   | ('new' (classTemplate | templateBody) | blockExpr) typeArgs
   | simpleExpr1 argumentExprs
   ;

exprs
   : expr (',' expr)*
   ;

argumentExprs
   : '(' exprs? ')'
   | '(' (exprs ',')? postfixExpr ':' '_' '*' ')'
   | blockExpr
   ;

blockExpr
   : '{' caseClauses '}'
   | '{' block '}'
   ;

block
   : blockStat (Semi blockStat)* resultExpr?
   ;

blockStat
   : import_
   | annotation* ('implicit' | 'lazy')? def
   | annotation* localModifier* tmplDef
   | expr1
   |
   ;

resultExpr
   : expr1
   | (bindings | ('implicit'? Id | '_') ':' compoundType) '=>' block
   ;

enumerators
   : generator (Semi generator)*
   ;

generator
   : pattern1 '<-' expr (Semi? guard | Semi pattern1 '=' expr)*
   ;

caseClauses
   : caseClause +
   ;

caseClause
   : 'case' pattern guard? '=>' block
   ;

guard
   : 'if' postfixExpr
   ;

pattern
   : pattern1 ('|' pattern1)*
   ;

pattern1
   : Varid ':' typePat
   | '_' ':' typePat
   | pattern2
   ;

pattern2
   : Varid ('@' pattern3)?
   | pattern3
   ;

pattern3
   : simplePattern
   | simplePattern (Id simplePattern)*
   ;

simplePattern
   : '_'
   | Varid
   | literal
   | stableId ('(' patterns? ')')?
   | stableId '(' (patterns? ',')? (Varid '@')? '_' '*' ')'
   | '(' patterns? ')'
   ;

patterns
   : pattern (',' pattern)*
   | '_'+
   ;

typeParamClause
   : '[' variantTypeParam (',' variantTypeParam)* ']'
   ;

funTypeParamClause
   : '[' typeParam (',' typeParam)* ']'
   ;

variantTypeParam
   : annotation? ('+' | '-')? typeParam
   ;

typeParam
   : (Id | '_') typeParamClause? ('>:' type)? ('<:' type)? ('<%' type)* (':' type)*
   ;

paramClauses
   : paramClause* ('(' 'implicit' params ')')?
   ;

paramClause
   : '(' params? ')'
   ;

params
   : param (',' param)*
   ;

param
   : annotation* Id (':' paramType)? ('=' expr)?
   ;

paramType
   : type
   | '=>' type
   | type '*'
   ;

classParamClauses
   : classParamClause* ('(' 'implicit' classParams ')')?
   ;

classParamClause
   : '(' classParams? ')'
   ;

classParams
   : classParam (',' classParam)*
   ;

classParam
   : annotation* modifier* ('val' | 'var')? Id ':' paramType ('=' expr)?
   ;

bindings
   : '(' binding (',' binding)* ')'
   ;

binding
   : (Id | '_') (':' type)?
   ;

modifier
   : localModifier
   | accessModifier
   | 'override'
   ;

localModifier
   : 'abstract'
   | 'final'
   | 'sealed'
   | 'implicit'
   | 'lazy'
   ;

accessModifier
   : ('private' | 'protected') accessQualifier?
   ;

accessQualifier
   : '[' (Id | 'this') ']'
   ;

annotation
   : '@' simpleType argumentExprs*
   ;

constrAnnotation
   : '@' simpleType argumentExprs
   ;

templateBody
   : '{' selfType? templateStat (Semi templateStat)* '}'
   ;

templateStat
   : import_
   | (annotation)* modifier* def
   | (annotation)* modifier* dcl
   | expr
   |
   ;

selfType
   : Id (':' type)? '=>'
   | 'this' ':' type '=>'
   ;

import_
   : 'import' importExpr (',' importExpr)*
   ;

importExpr
   : stableId '.' (Id | '_' | importSelectors)
   ;

importSelectors
   : '{' (importSelector ',')* (importSelector | '_') '}'
   ;

importSelector
   : Id ('=>' Id | '=>' '_')
   ;

dcl
   : 'val' valDcl
   | 'var' varDcl
   | 'def' funDcl
   | 'type' typeDcl
   ;

valDcl
   : ids ':' type
   ;

varDcl
   : ids ':' type
   ;

funDcl
   : funSig (':' type)?
   ;

funSig
   : Id funTypeParamClause? paramClauses
   ;

typeDcl
   : Id typeParamClause? ('>:' type)? ('<:' type)?
   ;

patVarDef
   : 'val' patDef
   | 'var' varDef
   ;

def
   : patVarDef
   | 'def' funDef
   | 'type' typeDef
   | tmplDef
   ;

patDef
   : pattern2 (',' pattern2)* (':' type)* '=' expr
   ;

varDef
   : patDef
   | ids ':' type '=' '_'
   ;

funDef
   : funSig (':' type)? '=' expr
   | funSig '{' block '}'
   | 'this' paramClause paramClauses ('=' constrExpr | constrBlock)
   ;

typeDef
   : Id typeParamClause? '=' type
   ;

tmplDef
   : 'case'? 'class' classDef
   | 'case' 'object' objectDef
   | 'trait' traitDef
   ;

classDef
   : Id typeParamClause? constrAnnotation* accessModifier? classParamClauses classTemplateOpt
   ;

traitDef
   : Id typeParamClause? traitTemplateOpt
   ;

objectDef
   : Id classTemplateOpt
   ;

classTemplateOpt
   : 'extends' classTemplate
   | ('extends'? templateBody)?
   ;

traitTemplateOpt
   : 'extends' traitTemplate
   | ('extends'? templateBody)?
   ;

classTemplate
   : earlyDefs? classParents templateBody?
   ;

traitTemplate
   : earlyDefs? traitParents templateBody?
   ;

classParents
   : constr ('with' annotType)*
   ;

traitParents
   : annotType ('with' annotType)*
   ;

constr
   : annotType argumentExprs*
   ;

earlyDefs
   : '{' (earlyDef (Semi earlyDef)*)? '}' 'with'
   ;

earlyDef
   : (annotation)* modifier* patVarDef
   ;

constrExpr
   : selfInvocation
   | constrBlock
   ;

constrBlock
   : '{' selfInvocation (Semi blockStat)* '}'
   ;

selfInvocation
   : 'this' argumentExprs +
   ;

topStatSeq
   : topStat (Semi topStat)*
   ;

topStat
   : (annotation)* modifier* tmplDef
   | import_
   | packaging
   | packageObject
   |
   ;

packaging
   : 'package' qualId '{' topStatSeq '}'
   ;

packageObject
   : 'package' 'object' objectDef
   ;

compilationUnit
   : ('package' qualId Semi)* topStatSeq
   ;

// Lexer

BooleanLiteral
   : 'true' | 'false'
   ;


CharacterLiteral
   : '\'' (PrintableChar | CharEscapeSeq) '\''
   ;


StringLiteral
   : '"' StringElement* '"' | '"""' MultiLineChars '"""'
   ;


SymbolLiteral
   : '\'' Plainid
   ;


IntegerLiteral
   : (DecimalNumeral | HexNumeral) ('L' | 'l')
   ;


FloatingPointLiteral
   : Digit + '.' Digit + ExponentPart? FloatType? | '.' Digit + ExponentPart? FloatType? | Digit ExponentPart FloatType? | Digit + ExponentPart? FloatType
   ;


Id
   : Plainid | '`' StringLiteral '`'
   ;


Varid
   : Lower Idrest
   ;


WS
   : [ \r\n\t] -> skip
   ;


Semi
   : ';'
   ;


Paren
   : '(' | ')' | '[' | ']' | '{' | '}'
   ;


Delim
   : '`' | '\'' | '"' | '.' | ';' | ','
   ;


Comment
   : '/*' .*? '*/' | '//' .*?
   ;

// fragments

fragment UnicodeEscape
   : '\\' 'u' 'u'? HexDigit HexDigit HexDigit HexDigit
   ;


fragment WhiteSpace
   : '\u0020' | '\u0009' | '\u000D' | '\u000A'
   ;


fragment Opchar
   : PrintableChar
   ;


fragment Op
   : Opchar +
   ;


fragment Plainid
   : Upper Idrest | Varid | Op
   ;


fragment Idrest
   : (Letter | Digit)* ('_' Op)?
   ;


fragment StringElement
   : '\u0020' | '\u0021' | '\u0023' .. '\u007F' | CharEscapeSeq
   ;


fragment MultiLineChars
   : ('"'? '"'? .*?)* '"'*
   ;


fragment HexDigit
   : '0' .. '9' | 'A' .. 'F' | 'a' .. 'f'
   ;


fragment FloatType
   : 'F' | 'f' | 'D' | 'd'
   ;


fragment Upper
   : 'A' .. 'Z' | '$' | '_'
   ;

// and Unicode category Lu

fragment Lower
   : 'a' .. 'z'
   ;

// and Unicode category Ll

fragment Letter
   : Upper | Lower
   ;

// and Unicode categories Lo, Lt, Nl

fragment ExponentPart
   : ('E' | 'e') ('+' | '-')? Digit +
   ;


fragment PrintableChar
   : '\u0020' .. '\u007F'
   ;


fragment CharEscapeSeq
   : '\\' ('b' | 't' | 'n' | 'f' | 'r' | '"' | '\'' | '\\')
   ;


fragment DecimalNumeral
   : '0' | NonZeroDigit Digit*
   ;


fragment HexNumeral
   : '0' 'x' HexDigit HexDigit +
   ;


fragment Digit
   : '0' | NonZeroDigit
   ;


fragment NonZeroDigit
   : '1' .. '9'
   ;
