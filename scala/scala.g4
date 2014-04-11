grammar scala;

literal           : '-'? IntegerLiteral
                  | '-'? FloatingPointLiteral
                  | BooleanLiteral
                  | CharacterLiteral
                  | StringLiteral
                  | SymbolLiteral
                  | 'null' ; 
                  
qualId            : Id ('.' Id)* ;

ids               : Id (',' Id)* ;

stableId          : Id
                  | (stableId | (Id '.')? 'this') '.' Id
                  | (Id '.')? 'super' classQualifier? '.' Id ; 

classQualifier    : '[' Id ']' ;

type              : functionArgTypes '=>' type
                  | infixType existentialClause? ;

functionArgTypes  : infixType
                  | '(' ( paramType (',' paramType )* )? ')' ; 
                      
existentialClause : 'forSome' '{' existentialDcl (Semi existentialDcl)* '}';

existentialDcl    : 'type' typeDcl 
                  | 'val' valDcl;

infixType         : compoundType (Id Nl? compoundType)*;
  
compoundType      : annotType ('with' annotType)* refinement?
                  | refinement;
  
annotType         : simpleType annotation*;

simpleType        : simpleType typeArgs
                  | simpleType '#' Id
                  | stableId
                  | (stableId | (Id '.')? 'this') '.' 'type'
                  | '(' types ')';
                      
typeArgs          : '[' types ']';

types             : type (',' type)*;

refinement        : Nl? '{' refineStat (Semi refineStat)* '}';

refineStat        : dcl
                  | 'type' typeDef
                  | ;

typePat           : type;

ascription        : ':' infixType
                  | ':' annotation+ 
                  | ':' '_' '*';

expr              : (bindings | 'implicit'? Id | '_') '=>' expr
                  | expr1 ;

expr1             : 'if' '(' expr ')' Nl* expr (Semi? 'else' expr)?
                  | 'while' '(' expr ')' Nl* expr
                  | 'try' ('{' block '}' | expr) ('catch' '{' caseClauses '}')? ('finally' expr)?
                  | 'do' expr Semi? 'while' '(' expr ')'
                  | 'for' ('(' enumerators ')' | '{' enumerators '}') Nl* 'yield'? expr
                  | 'throw' expr
                  | 'return' expr?
                  | (('new' (classTemplate | templateBody)| blockExpr | simpleExpr1 '_'?) '.') Id '=' expr
                  | simpleExpr1 argumentExprs '=' expr
                  | postfixExpr
                  | postfixExpr ascription
                  | postfixExpr 'match' '{' caseClauses '}' ;

postfixExpr       : infixExpr (Id Nl?)? ;

infixExpr         : prefixExpr
                  | infixExpr Id Nl? infixExpr ;

prefixExpr        : ('-' | '+' | '~' | '!')? 
                    ('new' (classTemplate | templateBody)| blockExpr | simpleExpr1 '_'?) ;

simpleExpr1       : literal
                  | (stableId | (Id '.')? 'this')
                  | '_'
                  | '(' exprs? ')'
                  | ('new' (classTemplate | templateBody)| blockExpr | simpleExpr1 '_'?) '.' Id 
                  | ('new' (classTemplate | templateBody)| blockExpr | simpleExpr1 '_'?) typeArgs
                  | simpleExpr1 argumentExprs
       //           | xmlExpr 
       ;
                  
exprs             : expr (',' expr)* ;

argumentExprs     : '(' exprs? ')'
                  | '(' (exprs ',')? postfixExpr ':' '_' '*' ')'
                  | Nl? blockExpr ;
                  
blockExpr         : '{' caseClauses '}'
                  | '{' block '}' ;
block             : blockStat (Semi blockStat)* resultExpr? ;

blockStat         : import_
                  | annotation* ('implicit' | 'lazy')? def
                  | annotation* localModifier* tmplDef
                  | expr1
                  | ;

resultExpr        : expr1
                  | (bindings | ('implicit'? Id | '_') ':' compoundType) '=>' block ;

enumerators       : generator (Semi generator)* ;

generator         : pattern1 '<-' expr (Semi? guard | Semi pattern1 '=' expr)* ;

caseClauses       : caseClause+ ;

caseClause        : 'case' pattern guard? '=>' block ; 
  
guard             : 'if' postfixExpr ;

pattern           : pattern1 ('|' pattern1 )* ;

pattern1          : Varid ':' typePat
                  | '_' ':' typePat
                  | pattern2 ;

pattern2          : Varid ('@' pattern3)?
                  | pattern3 ;

pattern3          : simplePattern
                  | simplePattern (Id Nl? simplePattern)* ;
                  
simplePattern     : '_'
                  | Varid
                  | literal
                  | stableId
                  | stableId '(' patterns? ')'
                  | stableId '(' (patterns ',')? (Varid '@')? '_' '*' ')'
                  | '(' patterns? ')'
                //  | xmlPattern 
                  ;

patterns          : pattern (',' patterns)*
                  | '_' * ;

typeParamClause   : '[' variantTypeParam (',' variantTypeParam)* ']' ;

funTypeParamClause: '[' typeParam (',' typeParam)* ']' ;
  
variantTypeParam  : annotation? ('+' | '-')? typeParam ;

typeParam         : (Id | '_') typeParamClause? ('>:' type)? ('<:' type)? 
                    ('<%' type)* (':' type)* ;
                         
paramClauses      : paramClause* (Nl? '(' 'implicit' params ')')? ;

paramClause       : Nl? '(' params? ')' ;

params            : param (',' param)* ;

param             : annotation* Id (':' paramType)? ('=' expr)? ;

paramType         : type 
                  | '=>' type 
                  | type '*';

classParamClauses : classParamClause* 
                    (Nl? '(' 'implicit' classParams ')')? ;
                         
classParamClause  : Nl? '(' classParams? ')' ;

classParams       : classParam (',' classParam)* ;

classParam        : annotation* modifier* ('val' | 'var')? 
                    Id ':' paramType ('=' expr)? ;
                    
bindings          : '(' binding (',' binding )* ')' ;

binding           : (Id | '_') (':' type)? ;

modifier          : localModifier 
                  | accessModifier
                  | 'override' ;
                  
localModifier     : 'abstract'
                  | 'final'
                  | 'sealed'
                  | 'implicit'
                  | 'lazy' ;
                  
accessModifier    : ('private' | 'protected') accessQualifier? ;

accessQualifier   : '[' (Id | 'this') ']' ;

annotation        : '@' simpleType argumentExprs* ;

constrAnnotation  : '@' simpleType argumentExprs ;

templateBody      : Nl? '{' selfType? templateStat (Semi templateStat)* '}' ;

templateStat      : import_
                  | (annotation Nl?)* modifier* def
                  | (annotation Nl?)* modifier* dcl
                  |  expr
                  | ;
                  
selfType          : Id (':' type)? '=>'
                  | 'this' ':' type '=>' ; 

import_           : 'import' importExpr (',' importExpr)* ;

importExpr        : stableId '.' (Id | '_' | importSelectors) ;

importSelectors   : '{' (importSelector ',')* (importSelector | '_') '}' ;

importSelector    : Id ('=>' Id | '=>' '_') ;
 
dcl               : 'val' valDcl
                  | 'var' varDcl
                  | 'def' funDcl
                  | 'type' Nl* typeDcl ;

valDcl            : ids ':' type ;

varDcl            : ids ':' type ;

funDcl            : funSig (':' type)? ;

funSig            : Id funTypeParamClause? paramClauses ;

typeDcl           : Id typeParamClause? ('>:' type)? ('<:' type)? ;

patVarDef         : 'val' patDef
                  | 'var' varDef ;

def               : patVarDef
                  | 'def' funDef
                  | 'type' Nl* typeDef
                  | tmplDef ;
                  
patDef            : pattern2 (',' pattern2)* (':' type)* '=' expr ;

varDef            : patDef
                  | ids ':' type '=' '_' ;
                  
funDef            : funSig (':' type)? '=' expr
                  | funSig Nl? '{' block '}'
                  | 'this' paramClause paramClauses 
                    ('=' constrExpr | Nl constrBlock) ;

typeDef           :  Id typeParamClause? '=' type ;

tmplDef           : 'case'? 'class' classDef
                  | 'case' 'object' objectDef
                  | 'trait' traitDef ;

classDef          : Id typeParamClause? constrAnnotation* accessModifier? 
                    classParamClauses classTemplateOpt ;
                      
traitDef          : Id typeParamClause? traitTemplateOpt ;

objectDef         : Id classTemplateOpt ;

classTemplateOpt  : 'extends' classTemplate | ('extends'? templateBody)? ;

traitTemplateOpt  : 'extends' traitTemplate | ('extends'? templateBody)? ;

classTemplate     : earlyDefs? classParents templateBody? ;

traitTemplate     : earlyDefs? traitParents templateBody? ;

classParents      : constr ('with' annotType)* ;

traitParents      : annotType ('with' annotType)* ;

constr            : annotType argumentExprs* ;

earlyDefs         : '{' (earlyDef (Semi earlyDef)*)? '}' 'with' ;

earlyDef          : (annotation Nl?)* modifier* patVarDef ;

constrExpr        : selfInvocation 
                  | constrBlock ;
                  
constrBlock       : '{' selfInvocation (Semi blockStat)* '}' ;
selfInvocation    : 'this' argumentExprs+ ;

topStatSeq        : topStat (Semi topStat)* ;

topStat           : (annotation Nl?)* modifier* tmplDef
                  | import_
                  | packaging
                  | packageObject
                  | ;
                    
packaging         : 'package' qualId Nl? '{' topStatSeq '}' ;

packageObject     : 'package' 'object' objectDef ;

compilationUnit   : ('package' qualId Semi)* topStatSeq ;

//

UnicodeEscape:	'\\' 'u' 'u'? HexDigit HexDigit HexDigit HexDigit ;

WhiteSpace       :  '\u0020' | '\u0009' | '\u000D' | '\u000A';
// Digit            :  '0' .. '9';
Paren            :  '(' | ')' | '[' | ']' | '{' | '}';
Delim            :  '`' | '\'' | '"' | '.' | ';' | ',' ;
Opchar           : PrintableChar // printableChar not matched by (whiteSpace | upper | lower |
                     // letter | digit | paren | delim | opchar | Unicode_Sm | Unicode_So)
                     ;
                     
Op               :  Opchar+; 
Varid            :  Lower Idrest;
Plainid          :  Upper Idrest
                 |  Varid
                 |  Op;
Id               :  Plainid
                 |  '`' StringLiteral '`';
Idrest           :  (Letter | Digit)* ('_' Op)?;

IntegerLiteral   :  (DecimalNumeral | HexNumeral) ('L' | 'l');


FloatingPointLiteral 
                 :  Digit+ '.' Digit+ ExponentPart? FloatType?
                 |  '.' Digit+ ExponentPart? FloatType?
                 |  Digit ExponentPart FloatType?
                 |  Digit+ ExponentPart? FloatType;

BooleanLiteral   :  'true' | 'false';

CharacterLiteral :  '\'' (PrintableChar | CharEscapeSeq) '\'';

StringLiteral    :  '"' StringElement* '"'
                 |  '"""' MultiLineChars '"""';
StringElement    :  '\u0020'| '\u0021'|'\u0023' .. '\u007F'  // (PrintableChar  Except '"')
                 |  CharEscapeSeq;
MultiLineChars   :  {'"'? '"'? CharNoDoubleQuote} '"'*;
 
SymbolLiteral    :  '\'' Plainid;

Comment          :  '/*' .*  '*/'
                 |  '//' .*? Nl; //“any sequence of characters up to end of line”

Nl               :  '\r'? '\n';  //“newlinecharacter”
Semi             :  ';' |  Nl+;

fragment
HexDigit :  '0' .. '9'  |  'A' .. 'Z'  |  'a' .. 'z' ;
FloatType        :  'F' | 'f' | 'D' | 'd';
Upper            :  'A'  ..  'Z' | '$' | '_';  // and Unicode category Lu
Lower            :  'a' .. 'z'; // and Unicode category Ll
Letter           :  Upper | Lower; // and Unicode categories Lo, Lt, Nl
ExponentPart     :  ('E' | 'e') ('+' | '-')? Digit+;
PrintableChar    : '\u0020' .. '\u007F' ;
CharEscapeSeq    : '\\' ('b' | 't' | 'n' | 'f' | 'r' | '"' | '\'' | '\\');
DecimalNumeral   :  '0' | NonZeroDigit Digit*;
HexNumeral       :  '0' 'x' HexDigit HexDigit+;
Digit            :  '0' | NonZeroDigit;
NonZeroDigit     :  '1' .. '9';
