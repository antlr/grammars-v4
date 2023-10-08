// XPath v2.0
// Author--Ken Domino
// Date--2 Jan 2022
// 
// This is a faithful implementation of the XPath version 2.0 grammar
// from the spec at https://www.w3.org/TR/xpath20/

parser grammar  XPath20Parser;

options { tokenVocab=XPath20Lexer; superClass=XPath20ParserBase; }

// [1]
xpath : expr EOF ;
expr : exprsingle ( COMMA exprsingle)* ;
exprsingle : forexpr | quantifiedexpr | ifexpr | orexpr ;
forexpr : simpleforclause KW_RETURN exprsingle ;
// [5]
simpleforclause : KW_FOR DOLLAR varname KW_IN exprsingle ( COMMA DOLLAR varname KW_IN exprsingle )* ;
quantifiedexpr : ( KW_SOME | KW_EVERY) DOLLAR varname KW_IN exprsingle ( COMMA DOLLAR varname KW_IN exprsingle)* KW_SATISFIES exprsingle ;
ifexpr : KW_IF OP expr CP KW_THEN exprsingle KW_ELSE exprsingle ;
orexpr : andexpr ( KW_OR andexpr )* ;
andexpr : comparisonexpr ( KW_AND comparisonexpr )* ;
// [10]
comparisonexpr : rangeexpr ( (valuecomp | generalcomp | nodecomp) rangeexpr )? ;
rangeexpr : additiveexpr ( KW_TO additiveexpr )? ;
additiveexpr : multiplicativeexpr ( (PLUS | MINUS) multiplicativeexpr )* ;
multiplicativeexpr : unionexpr ( (STAR | KW_DIV | KW_IDIV | KW_MOD) unionexpr )* ;
unionexpr : intersectexceptexpr ( (KW_UNION | P) intersectexceptexpr )* ;
// [15]
intersectexceptexpr : instanceofexpr ( ( KW_INTERSECT | KW_EXCEPT) instanceofexpr )* ;
instanceofexpr : treatexpr ( KW_INSTANCE KW_OF sequencetype )? ;
treatexpr : castableexpr ( KW_TREAT KW_AS sequencetype )? ;
castableexpr : castexpr ( KW_CASTABLE KW_AS singletype )? ;
castexpr : unaryexpr ( KW_CAST KW_AS singletype )? ;
// [20]
unaryexpr : ( MINUS | PLUS)* valueexpr ;
valueexpr : pathexpr ;
generalcomp : EQ | NE | LT | LE | GT | GE ;
valuecomp : KW_EQ | KW_NE | KW_LT | KW_LE | KW_GT | KW_GE ;
nodecomp : KW_IS | LL | GG ;
// [25]
pathexpr : SLASH relativepathexpr? | SS relativepathexpr | relativepathexpr ;
relativepathexpr : stepexpr (( SLASH | SS) stepexpr)* ;
stepexpr : filterexpr | axisstep ;
axisstep : (reversestep | forwardstep) predicatelist ;
forwardstep : forwardaxis nodetest | abbrevforwardstep ;
// [30]
forwardaxis : KW_CHILD COLONCOLON | KW_DESCENDANT COLONCOLON | KW_ATTRIBUTE COLONCOLON | KW_SELF COLONCOLON | KW_DESCENDANT_OR_SELF COLONCOLON | KW_FOLLOWING_SIBLING COLONCOLON | KW_FOLLOWING COLONCOLON | KW_NAMESPACE COLONCOLON ;
abbrevforwardstep : AT? nodetest ;
reversestep : reverseaxis nodetest | abbrevreversestep ;
reverseaxis : KW_PARENT COLONCOLON | KW_ANCESTOR COLONCOLON | KW_PRECEDING_SIBLING COLONCOLON | KW_PRECEDING COLONCOLON | KW_ANCESTOR_OR_SELF COLONCOLON ;
abbrevreversestep : DD ;
// [35]
nodetest : kindtest | nametest ;
nametest : qname | wildcard ;
wildcard : STAR | NCName CS | SC NCName ;
filterexpr : primaryexpr predicatelist ;
predicatelist : predicate* ;
// [40]
predicate : OB expr CB ;
primaryexpr : literal | varref | parenthesizedexpr | contextitemexpr | functioncall ;
literal : numericliteral | StringLiteral ;
numericliteral : IntegerLiteral | DecimalLiteral | DoubleLiteral ;
varref : DOLLAR varname ;
// [45]
varname : qname ;
parenthesizedexpr : OP expr? CP ;
contextitemexpr : D ;
functioncall : { this.IsFuncCall() }? qname OP (exprsingle ( COMMA exprsingle)*)? CP ;
singletype : atomictype QM? ;
// [50]
sequencetype : KW_EMPTY_SEQUENCE OP CP | itemtype occurrenceindicator? ;
occurrenceindicator : QM | STAR | PLUS ;
itemtype : kindtest | KW_ITEM OP CP | atomictype ;
atomictype : qname ;
kindtest : documenttest | elementtest | attributetest | schemaelementtest | schemaattributetest | pitest | commenttest | texttest | anykindtest ;
// [55]
anykindtest : KW_NODE OP CP ;
documenttest : KW_DOCUMENT_NODE OP (elementtest | schemaelementtest)? CP ;
texttest : KW_TEXT OP CP ;
commenttest : KW_COMMENT OP CP ;
pitest : KW_PROCESSING_INSTRUCTION OP (NCName | StringLiteral)? CP ;
// [60]
attributetest : KW_ATTRIBUTE OP (attribnameorwildcard ( COMMA typename_)?)? CP ;
attribnameorwildcard : attributename | STAR ;
schemaattributetest : KW_SCHEMA_ATTRIBUTE OP attributedeclaration CP ;
attributedeclaration : attributename ;
elementtest : KW_ELEMENT OP (elementnameorwildcard ( COMMA typename_ QM?)?)? CP ;
// [65]
elementnameorwildcard : elementname | STAR ;
schemaelementtest : KW_SCHEMA_ELEMENT OP elementdeclaration CP ;
elementdeclaration : elementname ;
attributename : qname ;
elementname : qname ;
// [70]
typename_ : qname ;


// Error in the spec. EQName also includes acceptable keywords.
qname : QName | URIQualifiedName
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