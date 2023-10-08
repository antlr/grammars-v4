// XPath v2.0
// Author--Ken Domino
// Date--2 Jan 2022
//
// This is a faithful implementation of the XPath version 2.0 grammar
// from the spec at https://www.w3.org/TR/xpath20/

lexer grammar  XPath20Lexer;

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

// A.2.1. TERMINAL SYMBOLS
// This isn't a complete list of tokens in the language.
// Keywords and symbols are terminals.

IntegerLiteral : FragDigits ;
DecimalLiteral : '.' FragDigits | FragDigits '.' [0-9]* ;
DoubleLiteral : ('.' FragDigits | FragDigits ('.' [0-9]*)?) [eE] [+-]? FragDigits ;
StringLiteral : '"' (~["] | FragEscapeQuot)* '"' | '\'' (~['] | FragEscapeApos)* '\'' ;
URIQualifiedName : BracedURILiteral NCName ;
BracedURILiteral : 'Q' '{' [^{}]* '}' ;
// Error in spec: EscapeQuot and EscapeApos are not terminals!
fragment FragEscapeQuot : '""' ;
fragment FragEscapeApos : '\'\'';
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
