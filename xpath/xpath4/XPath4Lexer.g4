// XPath v4.0
// Author--Ken Domino
// Based on the XPath 4.0 WG Review Draft at https://qt4cg.org/specifications/xquery-40/xpath-40.html
//
// This is an implementation of the XPath version 4.0 grammar.

// $antlr-format alignTrailingComments true, columnLimit 150, maxEmptyLinesToKeep 1, reflowComments false, useTab false
// $antlr-format allowShortRulesOnASingleLine true, allowShortBlocksOnASingleLine true, minEmptyLines 0, alignSemicolons ownLine
// $antlr-format alignColons trailing, singleLineOverrulesHangingColon true, alignLexerCommands true, alignLabels true, alignTrailers true

lexer grammar XPath4Lexer;

// Operators -- longer multi-char tokens must appear before their shorter prefixes

PLUS_CEQ      : '+:='; // record-put operator (new in XPath 4.0)
MAPPING_ARROW : '=!>'; // mapping arrow operator (new in XPath 4.0)
METHOD_ARROW  : '=?>'; // method call operator (new in XPath 4.0)
SS            : '//';
PP            : '||';
DD            : '..';
COLONCOLON    : '::';
CEQ           : ':=';
CS            : ':*';
SC            : '*:';
LL            : '<<';
GG            : '>>';
EG            : '=>';
GE            : '>=';
LE            : '<=';
NE            : '!=';
AT            : '@';
BANG          : '!';
CB            : ']';
CC            : '}';
COLON         : ':';
COMMA         : ',';
CP            : ')';
D             : '.';
DOLLAR        : '$';
EQ            : '=';
GT            : '>';
LT            : '<';
MINUS         : '-';
OB            : '[';
OC            : '{';
OP            : '(';
P             : '|';
PLUS          : '+';
POUND         : '#';
QM            : '?';
SLASH         : '/';
STAR          : '*';
SEMI          : ';';
TIMES_SIGN    : '\u00D7'; // × (Unicode multiplication sign, alias for *)
DIV_SIGN      : '\u00F7'; // ÷ (Unicode division sign, alias for div)

// KEYWORDS -- longer hyphenated forms must precede shorter ones

KW_ANCESTOR                  : 'ancestor';
KW_ANCESTOR_OR_SELF          : 'ancestor-or-self';
KW_AND                       : 'and';
KW_ARRAY                     : 'array';
KW_AS                        : 'as';
KW_AT                        : 'at';
KW_ATTRIBUTE                 : 'attribute';
KW_CAST                      : 'cast';
KW_CASTABLE                  : 'castable';
KW_CHILD                     : 'child';
KW_COMMENT                   : 'comment';
KW_DECLARE                   : 'declare';
KW_DEFAULT                   : 'default';
KW_DESCENDANT                : 'descendant';
KW_DESCENDANT_OR_SELF        : 'descendant-or-self';
KW_DIV                       : 'div';
KW_DOCUMENT_NODE             : 'document-node';
KW_DOCUMENT                  : 'document';
KW_ELEMENT                   : 'element';
KW_ELSE                      : 'else';
KW_EMPTY_SEQUENCE            : 'empty-sequence';
KW_ENUM                      : 'enum';
KW_EQ                        : 'eq';
KW_EVERY                     : 'every';
KW_EXCEPT                    : 'except';
KW_FN                        : 'fn';
KW_FOLLOWING_SIBLING_OR_SELF : 'following-sibling-or-self';
KW_FOLLOWING_SIBLING         : 'following-sibling';
KW_FOLLOWING_OR_SELF         : 'following-or-self';
KW_FOLLOWING                 : 'following';
KW_FOLLOWS_OR_IS             : 'follows-or-is';
KW_FOLLOWS                   : 'follows';
KW_FOR                       : 'for';
KW_FUNCTION                  : 'function';
KW_GE                        : 'ge';
KW_GNODE                     : 'gnode';
KW_GT                        : 'gt';
KW_IDIV                      : 'idiv';
KW_IF                        : 'if';
KW_IN                        : 'in';
KW_INSTANCE                  : 'instance';
KW_INTERSECT                 : 'intersect';
KW_IS_NOT                    : 'is-not';
KW_IS                        : 'is';
KW_ITEM                      : 'item';
KW_JNODE                     : 'jnode';
KW_KEY                       : 'key';
KW_LE                        : 'le';
KW_LET                       : 'let';
KW_LT                        : 'lt';
KW_MAP                       : 'map';
KW_MEMBER                    : 'member';
KW_MOD                       : 'mod';
KW_NAMESPACE_NODE            : 'namespace-node';
KW_NAMESPACE                 : 'namespace';
KW_NE                        : 'ne';
KW_NODE                      : 'node';
KW_OF                        : 'of';
KW_OR                        : 'or';
KW_OTHERWISE                 : 'otherwise';
KW_PARENT                    : 'parent';
KW_PRECEDES_OR_IS            : 'precedes-or-is';
KW_PRECEDES                  : 'precedes';
KW_PRECEDING_SIBLING_OR_SELF : 'preceding-sibling-or-self';
KW_PRECEDING_SIBLING         : 'preceding-sibling';
KW_PRECEDING_OR_SELF         : 'preceding-or-self';
KW_PRECEDING                 : 'preceding';
KW_PROCESSING_INSTRUCTION    : 'processing-instruction';
KW_RECORD                    : 'record';
KW_RETURN                    : 'return';
KW_SATISFIES                 : 'satisfies';
KW_SCHEMA_ATTRIBUTE          : 'schema-attribute';
KW_SCHEMA_ELEMENT            : 'schema-element';
KW_SELF                      : 'self';
KW_SOME                      : 'some';
KW_TEXT                      : 'text';
KW_THEN                      : 'then';
KW_TO                        : 'to';
KW_TREAT                     : 'treat';
KW_UNION                     : 'union';
KW_VALUE                     : 'value';

// A.2.1. TERMINAL SYMBOLS

IntegerLiteral : FragDigits;
DecimalLiteral : '.' FragDigits | FragDigits '.' [0-9]*;
DoubleLiteral  : ('.' FragDigits | FragDigits ('.' [0-9]*)?) [eE] [+-]? FragDigits;
StringLiteral  : '"' (~["] | FragEscapeQuot)* '"' | '\'' (~['] | FragEscapeApos)* '\'';
// URIQualifiedName: inline the braced-URI pattern so it is not shadowed by BracedURILiteral.
// Allows optional prefix: Q{uri}ncname or Q{uri}prefix:ncname
URIQualifiedName : 'Q' '{' [^{}]* '}' FragmentNCName (':' FragmentNCName)?;
BracedURILiteral : 'Q' '{' [^{}]* '}';

// String template (simplified: full embedded-expression support requires lexer modes).
// A production implementation would push/pop lexer modes on '{' and '}'.
StringTemplate: '`' StringTemplateChar* '`';
fragment StringTemplateChar:
    '{{'   // escaped open brace
    | '}}' // escaped close brace
    | ~[`] // any character except backtick (includes bare { and }, not nested-parsed)
;

// Error in spec: EscapeQuot and EscapeApos are not terminals.
fragment FragEscapeQuot : '""';
fragment FragEscapeApos : '\'\'';

// Error in spec: Comment is not really a terminal, but an off-channel object.
Comment: '(:' (Comment | CommentContents)*? ':)' -> skip;

QName  : FragQName;
NCName : FragmentNCName;

// Error in spec: Char is not a terminal.
fragment Char            : FragChar;
fragment FragDigits      : [0-9]+;
fragment CommentContents : Char;

// https://www.w3.org/TR/REC-xml-names/#NT-QName
fragment FragQName          : FragPrefixedName | FragUnprefixedName;
fragment FragPrefixedName   : FragPrefix ':' FragLocalPart;
fragment FragUnprefixedName : FragLocalPart;
fragment FragPrefix         : FragmentNCName;
fragment FragLocalPart      : FragmentNCName;
fragment FragNCNameStartChar:
    'A' ..'Z'
    | '_'
    | 'a' ..'z'
    | '\u00C0' ..'\u00D6'
    | '\u00D8' ..'\u00F6'
    | '\u00F8' ..'\u02FF'
    | '\u0370' ..'\u037D'
    | '\u037F' ..'\u1FFF'
    | '\u200C' ..'\u200D'
    | '\u2070' ..'\u218F'
    | '\u2C00' ..'\u2FEF'
    | '\u3001' ..'\uD7FF'
    | '\uF900' ..'\uFDCF'
    | '\uFDF0' ..'\uFFFD'
    | '\u{10000}' ..'\u{EFFFF}'
;
fragment FragNCNameChar:
    FragNCNameStartChar
    | '-'
    | '.'
    | '0' ..'9'
    | '\u00B7'
    | '\u0300' ..'\u036F'
    | '\u203F' ..'\u2040'
;
fragment FragmentNCName: FragNCNameStartChar FragNCNameChar*;

// https://www.w3.org/TR/REC-xml/#NT-Char
fragment FragChar:
    '\u0009'
    | '\u000a'
    | '\u000d'
    | '\u0020' ..'\ud7ff'
    | '\ue000' ..'\ufffd'
    | '\u{10000}' ..'\u{10ffff}'
;

Whitespace: ('\u000d' | '\u000a' | '\u0020' | '\u0009')+ -> skip;