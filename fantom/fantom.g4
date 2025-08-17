/*
BSD License

Copyright (c) 2024, Tom Everett
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
'AS IS' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
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
grammar fantom;

file_
   : program? EOF
   ;

compilationUnit :  using* typeDef* doc?;
using           :  usingPod | usingType | usingAs;
usingPod        :  'using' podSpec EOS;
usingType       :  'using' podSpec '::' ID EOS;
usingAs         :  'using' podSpec '::' ID 'as' ID EOS;
podSpec         :  ID | str | ffiPodSpec;
ffiPodSpec      : '[' ID ']' ID ('.' ID)*;

typeDef        :  classDef | mixinDef | facetDef | enumDef;

classDef       :  typeHeader 'class' ID inheritance? '{' slotDefs '}';
mixinDef       :  typeHeader 'mixin' ID inheritance? '{' slotDefs '}';
facetDef       :  typeHeader 'facet' 'class' ID inheritance? '{' slotDefs '}';
enumDef        :  typeHeader 'enum' 'class' ID inheritance? '{' enumValDefs slotDefs '}';

typeHeader     :  doc? facets typeFlags;
typeFlags      :  typeFlag*;
typeFlag       :  protection | 'abstract' | 'final' | 'const' | 'native';
protection     :  'public' | 'protected' | 'private' | 'internal';
inheritance    :  ':' typeList;

enumValDefs    :  enumValDef (',' enumValDef)* EOS;
enumValDef     :  facets ID ('(' args ')')?;

slotDefs       :  slotDef*;
slotDef        :  fieldDef | methodDef | ctorDef | staticInit;

fieldDef       :  facets fieldFlags type ID (':' expr)?
                     ( '{' fieldGetter? fieldSetter? '}' )? EOS;
fieldFlags     :  fieldFlag*;
fieldFlag      :  protection | 'abstract' | 'const' | 'final' | 'native' |
                     'override' | 'readonly' | 'static' | 'virtual';
fieldGetter    :  'get' (EOS | block);
fieldSetter    :  protection 'set' (EOS | block);

methodDef      :  facets methodFlags type ID '(' params ')' methodBody;
methodFlags    :  methodFlag*;
methodFlag     :  protection | 'abstract' | 'native' | 'once' |
                     'override' | 'static' | 'virtual' | 'final';
params         :  (param (',' param)*)?;
param          :  type ID (':' expr)?;
methodBody     :  EOS | ( '{' stmts '}' );

ctorDef        :  facets ctorFlags 'new' ID '(' params ')' ctorChain? methodBody;
ctorFlags      :  protection?;
ctorChain      :  ':' ctorChainThis | ctorChainSuper;
ctorChainThis  :  'this' '.' ID '(' args ')';
ctorChainSuper :  'super' ('.' ID)? '(' args ')';

staticInit     :  'static' '{' stmts '}';


facets        : facet*;
facet         : '@' simpleType [facetVals];
facetVals     : '{' facetVal (EOS facetVal)* '}';
facetVal      : ID '=' expr;

block          :  stmt | ( '{' stmts '}' );
stmts          :  stmt*;
stmt           :  break | continue | for | if | return | switch |
                     throw | while | try | exprStmt | localDef | itAdd;
break          :  'break' EOS;
continue       :  'continue' EOS;
for            :  'for' '(' forInit? ';' expr? ';' expr? ')' block;
forInit        :  expr | localDef;
if             :  'if' '(' expr ')' block ( 'else' block )?;
return         :  'return' expr? EOS;
throw          :  'throw' expr EOS;
while          :  'while' '(' expr ')' block;
exprStmt       :  expr EOS;
localDef       :  type? ID (':' expr)? EOS;
itAdd          :  expr (',' expr)* ','? EOS;

try            :  'try' block catch_* finally_?;
catch_          :  'catch' catchDef? block;
catchDef       :  '(' type ID ')';
finally_        :  'finally' block;

switch         :  'switch' '(' expr ')' '{' case* default? '}';
case           :  'case' expr ':' stmts;
default        :  'default' ':' stmts;

expr           :  assignExpr;
assignExpr     :  ifExpr [assignOp assignExpr];
assignOp       :  '=' | '*=' | '/=' | '%=' | '+=' | '-=';

ifExpr         :  ternaryExpr | elvisExpr;
ternaryExpr    :  condOrExpr ['?' ifExprBody ':' ifExprBody];
elvisExpr      :  condOrExpr '?:' ifExprBody;
ifExprBody     :  condOrExpr | ifExprThrow;
ifExprThrow    :  'throw' expr;

condOrExpr     :  condAndExpr  ('||' condAndExpr)*;
condAndExpr    :  equalityExpr ('&&' equalityExpr)*;
equalityExpr   :  relationalExpr [('==' | '!=' | '===' | '!==') relationalExpr];
relationalExpr :  typeCheckExpr | compareExpr;
typeCheckExpr  :  rangeExpr [('is' | 'as' | 'isnot') type];
compareExpr    :  rangeExpr [('<' | '<=' | '>' | '>=' | '<=>') rangeExpr];
rangeExpr      :  addExpr (('..' | '..<') addExpr)*;
addExpr        :  multExpr (('+' | '-') multExpr)*;
multExpr       :  parenExpr (('*' | '/' | '%') parenExpr)*;
parenExpr      :  unaryExpr | castExpr | groupedExpr;
castExpr       :  '(' type ')' parenExpr;
groupedExpr    :  '(' expr ')' termChain*;
unaryExpr      :  prefixExpr | termExpr | postfixExpr;
prefixExpr     :  ('!' | '+' | '-' | '++' | '--') parenExpr;
postfixExpr    :  termExpr ('++' | '--');

termExpr       :  termBase termChain*;
termBase       :  literal | idExpr | typeBase;
typeBase       :  typeLiteral | slotLiteral | namedSuper | staticCall |
                     dsl | closure | simple | ctorBlock;
typeLiteral    :  type '#';
slotLiteral    :  type? '#' ID;
namedSuper     :  type '.' 'super';
staticCall     :  type '.' slotExpr;
dsl            :  type '<|' anyChar* '|>';
simple         :  type '(' expr ')';
ctorBlock      :  type itBlock;
termChain      :  dotCall | dynCall | safeDotCall | safeDynCall |
                     indexExpr | callOp | itBlock;
itBlock        :  '{' stmts '}';
dotCall        :  '.' slotExpr;
dynCall        :  '->' slotExpr;
safeDotCall    :  '?.' slotExpr;
safeDynCall    :  '?->' slotExpr;
IDExpr         :  local | slotExpr;
slotExpr       :  field | call;
local          :  ID;
field          :  '&'? ID;
call           :  ID ('(' args ')')? closure?;
args           :  (expr (',' expr)*)?;
indexExpr      :  '[' expr ']';
callOp         :  '(' args ')' closure?;

literal        :  'null' | 'this' | 'super' | 'it' |
                     bool | int | float | decimal | str |
                     duration | uri | list | map | uri;
list           :  type? '[' listItems ']';
listItems      :  ',' | (expr (',' expr)*) ','?;
map            :  mapType? '[' mapItems [','] ']';
mapItems       :  ':' | (mapPair (',' mapPair)*);
mapPair        :  expr ':' expr;

closure        :  closureSig '{' stmts '}';
closureSig     :  '|' closureParams ['-' type] '|';
closureParams  :  (closureParam (',' closureParam)*)?;
closureParam   :  formal | ID;

type           :  nullType | nonNullType;
nullType       :  nonNullType '?';
nonNullType    :  simpleType | listType | mapType | funcType;
typeList       :  type (',' type)*;
simpleType     :  ID ('::' ID)?;
listType       :  type '[]';
mapType        :  '['? type ':' type ']'?;
funcType       :  '|' formals? ('-' type)? '|';
formals        :  (formal (',' formal)*)?;
formal         :  formalFull | formalInferred | formalTypeOnly;
formalFull     :  type ID;
formalInferred :  ID;
formalTypeOnly :  type;

ID            : IDSTART (IDCHAR)*;
IDSTART        : [A-Z] | [a-z] | '_';
IDCHAR         : [A-Z] | [a-z] | '_' | [0-9];
EOS            : ';' | '\n' | '}';


COMMENT
   : ';' ~ ('\r' | '\n')* -> skip
   ;

WS
   : [ \r\n\t]+ -> skip
   ;

