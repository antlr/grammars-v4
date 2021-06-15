// XPath v3.1
// Author--Ken Domino
// Date--4 June 2020
// 
// This is a faithful implementation of the XPath version 3.1 grammar
// from the spec at https://www.w3.org/TR/2017/REC-xpath-31-20170321/

grammar XPath31;

// [1]
xpath : expr EOF ;
paramlist : param ( COMMA param)* ;
param : DOLLAR eqname typedeclaration? ;
functionbody : enclosedexpr ;
// [5]
enclosedexpr : OC expr? CC ;
expr : exprsingle ( COMMA exprsingle)* ;
exprsingle : forexpr | letexpr | quantifiedexpr | ifexpr | orexpr ;
forexpr : simpleforclause KW_RETURN exprsingle ;
simpleforclause : KW_FOR simpleforbinding ( COMMA simpleforbinding)* ;
// [10]
simpleforbinding : DOLLAR varname KW_IN exprsingle ;
letexpr :    simpleletclause KW_RETURN exprsingle ;
simpleletclause : KW_LET simpleletbinding ( COMMA simpleletbinding)* ;
simpleletbinding : DOLLAR varname CEQ exprsingle ;
quantifiedexpr :    ( KW_SOME | KW_EVERY) DOLLAR varname KW_IN exprsingle ( COMMA DOLLAR varname KW_IN exprsingle)* KW_SATISFIES exprsingle ;
// [15]
ifexpr : KW_IF OP expr CP KW_THEN exprsingle KW_ELSE exprsingle ;
orexpr : andexpr ( KW_OR andexpr )* ;
andexpr : comparisonexpr ( KW_AND comparisonexpr )* ;
comparisonexpr : stringconcatexpr ( (valuecomp | generalcomp | nodecomp) stringconcatexpr )? ;
stringconcatexpr : rangeexpr ( PP rangeexpr )* ;
// [20]
rangeexpr : additiveexpr ( KW_TO additiveexpr )? ;
additiveexpr : multiplicativeexpr ( (PLUS | MINUS) multiplicativeexpr )* ;
multiplicativeexpr : unionexpr ( (STAR | KW_DIV | KW_IDIV | KW_MOD) unionexpr )* ;
unionexpr : intersectexceptexpr ( (KW_UNION | P) intersectexceptexpr )* ;
intersectexceptexpr : instanceofexpr ( ( KW_INTERSECT | KW_EXCEPT) instanceofexpr )* ;
// [25]
instanceofexpr : treatexpr ( KW_INSTANCE KW_OF sequencetype )? ;
treatexpr : castableexpr ( KW_TREAT KW_AS sequencetype )? ;
castableexpr : castexpr ( KW_CASTABLE KW_AS singletype )? ;
castexpr : arrowexpr ( KW_CAST KW_AS singletype )? ;
arrowexpr : unaryexpr ( EG arrowfunctionspecifier argumentlist )* ;
// [30]
unaryexpr : ( MINUS | PLUS)* valueexpr ;
valueexpr : simplemapexpr ;
generalcomp : EQ | NE | LT | LE | GT | GE ;
valuecomp : KW_EQ | KW_NE | KW_LT | KW_LE | KW_GT | KW_GE ;
nodecomp : KW_IS | LL | GG ;
// [35]
simplemapexpr : pathexpr ( BANG pathexpr)* ;
pathexpr : ( SLASH relativepathexpr?) | ( SS relativepathexpr) | relativepathexpr ;
relativepathexpr : stepexpr (( SLASH | SS) stepexpr)* ;
stepexpr : postfixexpr | axisstep ;
axisstep : (reversestep | forwardstep) predicatelist ;
// [40]
forwardstep : (forwardaxis nodetest) | abbrevforwardstep ;
forwardaxis : ( KW_CHILD COLONCOLON) | ( KW_DESCENDANT COLONCOLON) | ( KW_ATTRIBUTE COLONCOLON) | ( KW_SELF COLONCOLON) | ( KW_DESCENDANT_OR_SELF COLONCOLON) | ( KW_FOLLOWING_SIBLING COLONCOLON) | ( KW_FOLLOWING COLONCOLON) | ( KW_NAMESPACE COLONCOLON) ;
abbrevforwardstep : AT? nodetest ;
reversestep : (reverseaxis nodetest) | abbrevreversestep ;
reverseaxis : ( KW_PARENT COLONCOLON) | ( KW_ANCESTOR COLONCOLON) | ( KW_PRECEDING_SIBLING COLONCOLON) | ( KW_PRECEDING COLONCOLON) | ( KW_ANCESTOR_OR_SELF COLONCOLON) ;
// [45]
abbrevreversestep : DD ;
nodetest : kindtest | nametest ;
nametest : eqname | wildcard ;
wildcard : STAR | (NCName CS) | ( SC NCName) | (BracedURILiteral STAR) ;
postfixexpr : primaryexpr (predicate | argumentlist | lookup)* ;
// [50]
argumentlist : OP (argument ( COMMA argument)*)? CP ;
predicatelist : predicate* ;
predicate : OB expr CB ;
lookup : QM keyspecifier ;
keyspecifier : NCName | IntegerLiteral | parenthesizedexpr | STAR ;
// [55]
arrowfunctionspecifier : eqname | varref | parenthesizedexpr ;
primaryexpr : literal | varref | parenthesizedexpr | contextitemexpr | functioncall | functionitemexpr | mapconstructor | arrayconstructor | unarylookup ;
literal : numericliteral | StringLiteral ;
numericliteral : IntegerLiteral | DecimalLiteral | DoubleLiteral ;
varref : DOLLAR varname ;
// [60]
varname : eqname ;
parenthesizedexpr : OP expr? CP ;
contextitemexpr : D ;
functioncall : 
                      { !(
                           getInputStream().LA(1)==KW_ARRAY
                        || getInputStream().LA(1)==KW_ATTRIBUTE
                        || getInputStream().LA(1)==KW_COMMENT
                        || getInputStream().LA(1)==KW_DOCUMENT_NODE
                        || getInputStream().LA(1)==KW_ELEMENT
                        || getInputStream().LA(1)==KW_EMPTY_SEQUENCE
                        || getInputStream().LA(1)==KW_FUNCTION
                        || getInputStream().LA(1)==KW_IF
                        || getInputStream().LA(1)==KW_ITEM
                        || getInputStream().LA(1)==KW_MAP
                        || getInputStream().LA(1)==KW_NAMESPACE_NODE
                        || getInputStream().LA(1)==KW_NODE
                        || getInputStream().LA(1)==KW_PROCESSING_INSTRUCTION
                        || getInputStream().LA(1)==KW_SCHEMA_ATTRIBUTE
                        || getInputStream().LA(1)==KW_SCHEMA_ELEMENT
                        || getInputStream().LA(1)==KW_TEXT
                        ) }?
                        eqname 
                        argumentlist ;
argument : exprsingle | argumentplaceholder ;
// [65]
argumentplaceholder : QM ;
functionitemexpr : namedfunctionref | inlinefunctionexpr ;
namedfunctionref : eqname POUND IntegerLiteral /* xgc: reserved-function-names */;
inlinefunctionexpr : KW_FUNCTION OP paramlist? CP ( KW_AS sequencetype)? functionbody ;
mapconstructor : KW_MAP OC (mapconstructorentry ( COMMA mapconstructorentry)*)? CC ;
// [70]
mapconstructorentry : mapkeyexpr COLON mapvalueexpr ;
mapkeyexpr : exprsingle ;
mapvalueexpr : exprsingle ;
arrayconstructor : squarearrayconstructor | curlyarrayconstructor ;
squarearrayconstructor : OB (exprsingle ( COMMA exprsingle)*)? CB ;
// [75]
curlyarrayconstructor : KW_ARRAY enclosedexpr ;
unarylookup : QM keyspecifier ;
singletype : simpletypename QM? ;
typedeclaration : KW_AS sequencetype ;
sequencetype : ( KW_EMPTY_SEQUENCE OP CP) | (itemtype occurrenceindicator?) ;
// [80]
occurrenceindicator : QM | STAR | PLUS ;
itemtype : kindtest | ( KW_ITEM OP CP) | functiontest | maptest | arraytest | atomicoruniontype | parenthesizeditemtype ;
atomicoruniontype : eqname ;
kindtest : documenttest | elementtest | attributetest | schemaelementtest | schemaattributetest | pitest | commenttest | texttest | namespacenodetest | anykindtest ;
anykindtest : KW_NODE OP CP ;
// [85]
documenttest : KW_DOCUMENT_NODE OP (elementtest | schemaelementtest)? CP ;
texttest : KW_TEXT OP CP ;
commenttest : KW_COMMENT OP CP ;
namespacenodetest : KW_NAMESPACE_NODE OP CP ;
pitest : KW_PROCESSING_INSTRUCTION OP (NCName | StringLiteral)? CP ;
// [90]
attributetest : KW_ATTRIBUTE OP (attribnameorwildcard ( COMMA typename_)?)? CP ;
attribnameorwildcard : attributename | STAR ;
schemaattributetest : KW_SCHEMA_ATTRIBUTE OP attributedeclaration CP ;
attributedeclaration : attributename ;
elementtest : KW_ELEMENT OP (elementnameorwildcard ( COMMA typename_ QM?)?)? CP ;
// [95]
elementnameorwildcard : elementname | STAR ;
schemaelementtest : KW_SCHEMA_ELEMENT OP elementdeclaration CP ;
elementdeclaration : elementname ;
attributename : eqname ;
elementname : eqname ;
// [100]
simpletypename : typename_ ;
typename_ : eqname ;
functiontest : anyfunctiontest | typedfunctiontest ;
anyfunctiontest : KW_FUNCTION OP STAR CP ;
typedfunctiontest : KW_FUNCTION OP (sequencetype ( COMMA sequencetype)*)? CP KW_AS sequencetype ;
// [105]
maptest : anymaptest | typedmaptest ;
anymaptest : KW_MAP OP STAR CP ;
typedmaptest : KW_MAP OP atomicoruniontype COMMA sequencetype CP ;
arraytest : anyarraytest | typedarraytest ;
anyarraytest : KW_ARRAY OP STAR CP ;
// [110]
typedarraytest : KW_ARRAY OP sequencetype CP ;
parenthesizeditemtype : OP itemtype CP ;

// Error in the spec. EQName also includes acceptable keywords.
eqname : QName | URIQualifiedName
 | KW_ANCESTOR
 | KW_ANCESTOR_OR_SELF
 | KW_AND
 | KW_ARRAY
 | KW_AS
 | KW_ATTRIBUTE
 | KW_CAST
 | KW_CASTABLE
 | KW_CHILD
 | KW_COMMENT
 | KW_DESCENDANT
 | KW_DESCENDANT_OR_SELF
 | KW_DIV
 | KW_DOCUMENT_NODE
 | KW_ELEMENT
 | KW_ELSE
 | KW_EMPTY_SEQUENCE
 | KW_EQ
 | KW_EVERY
 | KW_EXCEPT
 | KW_FOLLOWING
 | KW_FOLLOWING_SIBLING
 | KW_FOR
 | KW_FUNCTION
 | KW_GE
 | KW_GT
 | KW_IDIV
 | KW_IF
 | KW_IN
 | KW_INSTANCE
 | KW_INTERSECT
 | KW_IS
 | KW_ITEM
 | KW_LE
 | KW_LET
 | KW_LT
 | KW_MAP
 | KW_MOD
 | KW_NAMESPACE
 | KW_NAMESPACE_NODE
 | KW_NE
 | KW_NODE
 | KW_OF
 | KW_OR
 | KW_PARENT
 | KW_PRECEDING
 | KW_PRECEDING_SIBLING
 | KW_PROCESSING_INSTRUCTION
 | KW_RETURN
 | KW_SATISFIES
 | KW_SCHEMA_ATTRIBUTE
 | KW_SCHEMA_ELEMENT
 | KW_SELF
 | KW_SOME
 | KW_TEXT
 | KW_THEN
 | KW_TREAT
 | KW_UNION
 ;

// Not per spec. Specified for testing.
auxilary : (expr SEMI )+ EOF;


AT : '@' ;
BANG : '!' ;
CB : ']' ;
CC : '}' ;
CEQ : ':=' ;
COLON : ':' ;
COLONCOLON : '::' ;
COMMA : ',' ;
CP : ')' ;
CS : ':*' ;
D : '.' ;
DD : '..' ;
DOLLAR : '$' ;
EG : '=>' ;
EQ : '=' ;
GE : '>=' ;
GG : '>>' ;
GT : '>' ;
LE : '<=' ;
LL : '<<' ;
LT : '<' ;
MINUS : '-' ;
NE : '!=' ;
OB : '[' ;
OC : '{' ;
OP : '(' ;
P : '|' ;
PLUS : '+' ;
POUND : '#' ;
PP : '||' ;
QM : '?' ;
SC : '*:' ;
SLASH : '/' ;
SS : '//' ;
STAR : '*' ;

// KEYWORDS

KW_ANCESTOR : 'ancestor' ;
KW_ANCESTOR_OR_SELF : 'ancestor-or-self' ;
KW_AND : 'and' ;
KW_ARRAY : 'array' ;
KW_AS : 'as' ;
KW_ATTRIBUTE : 'attribute' ;
KW_CAST : 'cast' ;
KW_CASTABLE : 'castable' ;
KW_CHILD : 'child' ;
KW_COMMENT : 'comment' ;
KW_DESCENDANT : 'descendant' ;
KW_DESCENDANT_OR_SELF : 'descendant-or-self' ;
KW_DIV : 'div' ;
KW_DOCUMENT_NODE : 'document-node' ;
KW_ELEMENT : 'element' ;
KW_ELSE : 'else' ;
KW_EMPTY_SEQUENCE : 'empty-sequence' ;
KW_EQ : 'eq' ;
KW_EVERY : 'every' ;
KW_EXCEPT : 'except' ;
KW_FOLLOWING : 'following' ;
KW_FOLLOWING_SIBLING : 'following-sibling' ;
KW_FOR : 'for' ;
KW_FUNCTION : 'function' ;
KW_GE : 'ge' ;
KW_GT : 'gt' ;
KW_IDIV : 'idiv' ;
KW_IF : 'if' ;
KW_IN : 'in' ;
KW_INSTANCE : 'instance' ;
KW_INTERSECT : 'intersect' ;
KW_IS : 'is' ;
KW_ITEM : 'item' ;
KW_LE : 'le' ;
KW_LET : 'let' ;
KW_LT : 'lt' ;
KW_MAP : 'map' ;
KW_MOD : 'mod' ;
KW_NAMESPACE : 'namespace' ;
KW_NAMESPACE_NODE : 'namespace-node' ;
KW_NE : 'ne' ;
KW_NODE : 'node' ;
KW_OF : 'of' ;
KW_OR : 'or' ;
KW_PARENT : 'parent' ;
KW_PRECEDING : 'preceding' ;
KW_PRECEDING_SIBLING : 'preceding-sibling' ;
KW_PROCESSING_INSTRUCTION : 'processing-instruction' ;
KW_RETURN : 'return' ;
KW_SATISFIES : 'satisfies' ;
KW_SCHEMA_ATTRIBUTE : 'schema-attribute' ;
KW_SCHEMA_ELEMENT : 'schema-element' ;
KW_SELF : 'self' ;
KW_SOME : 'some' ;
KW_TEXT : 'text' ;
KW_THEN : 'then' ;
KW_TO : 'to' ;
KW_TREAT : 'treat' ;
KW_UNION : 'union' ;

// A.2.1. TEMINAL SYMBOLS
// This isn't a complete list of tokens in the language.
// Keywords and symbols are terminals.

IntegerLiteral : FragDigits ;
DecimalLiteral : ('.' FragDigits) | (FragDigits '.' [0-9]*) ;
DoubleLiteral : (('.' FragDigits) | (FragDigits ('.' [0-9]*)?)) [eE] [+-]? FragDigits ;
StringLiteral : ('"' (FragEscapeQuot | ~[^"])*? '"') | ('\'' (FragEscapeApos | ~['])*? '\'') ;
URIQualifiedName : BracedURILiteral NCName ;
BracedURILiteral : 'Q' '{' [^{}]* '}' ;
// Error in spec: EscapeQuot and EscapeApos are not terminals!
fragment FragEscapeQuot : '""' ; 
fragment FragEscapeApos : '\'';
// Error in spec: Comment isn't really a terminal, but an off-channel object.
Comment : '(:' (Comment | CommentContents)*? ':)' -> skip ;
QName  : FragQName ;
NCName : FragmentNCName ;
// Error in spec: Char is not a terminal!
fragment Char : FragChar ;
fragment FragDigits : [0-9]+ ;
fragment CommentContents : Char ;
// https://www.w3.org/TR/REC-xml-names/#NT-QName
fragment FragQName : FragPrefixedName | FragUnprefixedName ;
fragment FragPrefixedName : FragPrefix ':' FragLocalPart ;
fragment FragUnprefixedName : FragLocalPart ;
fragment FragPrefix : FragmentNCName ;
fragment FragLocalPart : FragmentNCName ;
fragment FragNCNameStartChar
  :  'A'..'Z'
  |  '_'
  | 'a'..'z'
  | '\u00C0'..'\u00D6'
  | '\u00D8'..'\u00F6'
  | '\u00F8'..'\u02FF'
  | '\u0370'..'\u037D'
  | '\u037F'..'\u1FFF'
  | '\u200C'..'\u200D'
  | '\u2070'..'\u218F'
  | '\u2C00'..'\u2FEF'
  | '\u3001'..'\uD7FF'
  | '\uF900'..'\uFDCF'
  | '\uFDF0'..'\uFFFD'
  | '\u{10000}'..'\u{EFFFF}'
  ;
fragment FragNCNameChar
  :  FragNCNameStartChar | '-' | '.' | '0'..'9'
  |  '\u00B7' | '\u0300'..'\u036F'
  |  '\u203F'..'\u2040'
  ;
fragment FragmentNCName  :  FragNCNameStartChar FragNCNameChar*  ;

// https://www.w3.org/TR/REC-xml/#NT-Char

fragment FragChar : '\u0009' | '\u000a' | '\u000d'
  | '\u0020'..'\ud7ff'
  | '\ue000'..'\ufffd'
  | '\u{10000}'..'\u{10ffff}'
 ;

// https://github.com/antlr/grammars-v4/blob/17d3db3fd6a8fc319a12176e0bb735b066ec0616/xpath/xpath31/XPath31.g4#L389
Whitespace :  ('\u000d' | '\u000a' | '\u0020' | '\u0009')+ -> skip ;

// Not per spec. Specified for testing.
SEMI : ';' ;
