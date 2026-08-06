// XQuery v4.0
// Author: Ken Domino
// Based on the XQuery 4.0 spec at https://qt4cg.org/pr/2796/xquery-40/xquery-40-autodiff.html
// Extends the XPath 4.0 lexer with XQuery-specific tokens and direct element constructor modes.
//
// Direct element constructor support requires lexer modes.
// The CC token in DEFAULT_MODE calls PopModeIfNeeded() from XQuery4LexerBase
// to pop back to element content mode when inside an embedded expression.
// IsNCNameStart() is also in XQuery4LexerBase, checking _input.LA(1).

// $antlr-format alignTrailingComments true, columnLimit 150, maxEmptyLinesToKeep 1, reflowComments false, useTab false
// $antlr-format allowShortRulesOnASingleLine true, allowShortBlocksOnASingleLine true, minEmptyLines 0, alignSemicolons ownLine
// $antlr-format alignColons trailing, singleLineOverrulesHangingColon true, alignLexerCommands true, alignLabels true, alignTrailers true

lexer grammar XQuery4Lexer;

options {
    superClass = XQuery4LexerBase;
}

// Token types produced only via -> type() remapping in element/attribute modes.
// Declaring them here ensures ANTLR generates constants for them.
tokens {
    QuotAttrContentChar,
    AposAttrContentChar,
    ElementContentChar,
    LCurlyBraceEscape,
    RCurlyBraceEscape,
    EscapeQuot,
    EscapeApos,
    PredefinedEntityRef,
    CharRef,
    CDataSection,
    DirCommentContents,
    DirPIContents
}

// ============================================================
// DEFAULT MODE
// ============================================================

// Operators -- longer multi-char tokens before shorter prefixes

AT            : '@';
ARROW         : '->';
PLUS_CEQ      : '+:=';
MAPPING_ARROW : '=!>';
METHOD_ARROW  : '=?>';
SS            : '//';
PP            : '||';
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
BANG          : '!';
CB            : ']';
COLON         : ':';
COMMA         : ',';
CP            : ')';
D             : '.';
DD            : '..';
DIV_ALT       : '÷';
DOLLAR        : '$';
EQ            : '=';
GT            : '>';
MINUS         : '-';
OB            : '[';
OC            : '{';
OP            : '(';
P             : '|';
PERPERPERCENT : '%%%';
PLUS          : '+';
POUND         : '#';
QM            : '?';
SLASH         : '/';
STAR          : '*';
STAR_ALT      : '×';
SEMI          : ';';
PERCENT       : '%';
TIMES_SIGN    : '\u00D7'; // × multiplication sign (XPath 4.0 alias for *)
DIV_SIGN      : '\u00F7'; // ÷ division sign (XPath 4.0 alias for div)

// KEYWORDS -- longer hyphenated forms before shorter ones

KW_AFTER                     : 'after';
KW_ALLOWING                  : 'allowing';
KW_ANCESTOR_OR_SELF          : 'ancestor-or-self';
KW_ANCESTOR                  : 'ancestor';
KW_AND                       : 'and';
KW_ARRAY                     : 'array';
KW_AS                        : 'as';
KW_ASCENDING                 : 'ascending';
KW_AT                        : 'at';
KW_ATTRIBUTE                 : 'attribute';
KW_BASE_URI                  : 'base-uri';
KW_BEFORE                    : 'before';
KW_BOUNDARY_SPACE            : 'boundary-space';
KW_BY                        : 'by';
KW_CASE                      : 'case';
KW_CAST                      : 'cast';
KW_CASTABLE                  : 'castable';
KW_CATCH                     : 'catch';
KW_CHILD                     : 'child';
KW_COLLATION                 : 'collation';
KW_COMMENT                   : 'comment';
KW_CONSTRUCTION              : 'construction';
KW_CONTEXT                   : 'context';
KW_COPY_NAMESPACES           : 'copy-namespaces';
KW_COUNT                     : 'count';
KW_DECIMAL_FORMAT            : 'decimal-format';
KW_DECIMAL_SEPARATOR         : 'decimal-separator';
KW_DECLARE                   : 'declare';
KW_DEFAULT                   : 'default';
KW_DESCENDANT_OR_SELF        : 'descendant-or-self';
KW_DESCENDANT                : 'descendant';
KW_DESCENDING                : 'descending';
KW_DIGIT                     : 'digit';
KW_DIV                       : 'div';
KW_DOCUMENT_NODE             : 'document-node';
KW_DOCUMENT                  : 'document';
KW_ELEMENT                   : 'element';
KW_ELSE                      : 'else';
KW_EMPTY_SEQUENCE            : 'empty-sequence';
KW_EMPTY                     : 'empty';
KW_ENCODING                  : 'encoding';
KW_END                       : 'end';
KW_ENUM                      : 'enum';
KW_EQ                        : 'eq';
KW_EVERY                     : 'every';
KW_EXCEPT                    : 'except';
KW_EXPONENT_SEPARATOR        : 'exponent-separator';
KW_EXTERNAL                  : 'external';
KW_FINALLY                   : 'finally';
KW_FIRST                     : 'first';
KW_FIXED                     : 'fixed';
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
KW_GREATEST                  : 'greatest';
KW_GROUP                     : 'group';
KW_GROUPING_SEPARATOR        : 'grouping-separator';
KW_GT                        : 'gt';
KW_IDIV                      : 'idiv';
KW_IF                        : 'if';
KW_IMPORT                    : 'import';
KW_IN                        : 'in';
KW_INFINITY                  : 'infinity';
KW_INHERIT                   : 'inherit';
KW_INSTANCE                  : 'instance';
KW_INTERSECT                 : 'intersect';
KW_IS_NOT                    : 'is-not';
KW_IS                        : 'is';
KW_ITEM                      : 'item';
KW_JNODE                     : 'jnode';
KW_KEY                       : 'key';
KW_LAST                      : 'last';
KW_LAX                       : 'lax';
KW_LE                        : 'le';
KW_LEAST                     : 'least';
KW_LET                       : 'let';
KW_LT                        : 'lt';
KW_MAP                       : 'map';
KW_MEMBER                    : 'member';
KW_MINUS_SIGN                : 'minus-sign';
KW_MOD                       : 'mod';
KW_MODULE                    : 'module';
KW_NAN                       : 'NaN';
KW_NAMESPACE_NODE            : 'namespace-node';
KW_NAMESPACE                 : 'namespace';
KW_NE                        : 'ne';
KW_NEXT                      : 'next';
KW_NO_INHERIT                : 'no-inherit';
KW_NO_PRESERVE               : 'no-preserve';
KW_NODE                      : 'node';
KW_OF                        : 'of';
KW_ONLY                      : 'only';
KW_OPTION                    : 'option';
KW_OR                        : 'or';
KW_ORDER                     : 'order';
KW_ORDERED                   : 'ordered';
KW_ORDERING                  : 'ordering';
KW_OTHERWISE                 : 'otherwise';
KW_PARENT                    : 'parent';
KW_PATTERN_SEPARATOR         : 'pattern-separator';
KW_PERCENT                   : 'percent';
KW_PER_MILLE                 : 'per-mille';
KW_PRECEDES_OR_IS            : 'precedes-or-is';
KW_PRECEDES                  : 'precedes';
KW_PRECEDING_SIBLING_OR_SELF : 'preceding-sibling-or-self';
KW_PRECEDING_SIBLING         : 'preceding-sibling';
KW_PRECEDING_OR_SELF         : 'preceding-or-self';
KW_PRECEDING                 : 'preceding';
KW_PRESERVE                  : 'preserve';
KW_PREVIOUS                  : 'previous';
KW_PROCESSING_INSTRUCTION    : 'processing-instruction';
KW_RECORD                    : 'record';
KW_RETURN                    : 'return';
KW_SATISFIES                 : 'satisfies';
KW_SCHEMA_ATTRIBUTE          : 'schema-attribute';
KW_SCHEMA_ELEMENT            : 'schema-element';
KW_SCHEMA                    : 'schema';
KW_SELF                      : 'self';
KW_SLIDING                   : 'sliding';
KW_SOME                      : 'some';
KW_STABLE                    : 'stable';
KW_START                     : 'start';
KW_STRICT                    : 'strict';
KW_STRIP                     : 'strip';
KW_SWITCH                    : 'switch';
KW_TEXT                      : 'text';
KW_THEN                      : 'then';
KW_TO                        : 'to';
KW_TRACE                     : 'trace';
KW_TREAT                     : 'treat';
KW_TRY                       : 'try';
KW_TUMBLING                  : 'tumbling';
KW_TYPESWITCH                : 'typeswitch';
KW_TYPE                      : 'type';
KW_UNION                     : 'union';
KW_UNORDERED                 : 'unordered';
KW_VALIDATE                  : 'validate';
KW_VALUE                     : 'value';
KW_VARIABLE                  : 'variable';
KW_VERSION                   : 'version';
KW_WHEN                      : 'when';
KW_WHERE                     : 'where';
KW_WHILE                     : 'while';
KW_WINDOW                    : 'window';
KW_XQUERY                    : 'xquery';
KW_ZERO_DIGIT                : 'zero-digit';

// A.2.1. TERMINAL SYMBOLS

IntegerLiteral   : FragDigits;
DecimalLiteral   : '.' FragDigits | FragDigits '.' [0-9]*;
HexIntegerLiteral : '0x' HexDigits;
BinaryIntegerLiteral : '0b' BinaryDigits;
DoubleLiteral    : ('.' FragDigits | FragDigits ('.' [0-9]*)?) [eE] [+-]? FragDigits;
StringLiteral    : '"' (~["] | FragEscapeQuot)* '"' | '\'' (~['] | FragEscapeApos)* '\'';
URIQualifiedName : 'Q' '{' [^{}]* '}' FragmentNCName (':' FragmentNCName)?;
BracedURILiteral : 'Q' '{' [^{}]* '}';

// String template: backtick-delimited interpolated string (XPath/XQuery 4.0)
StringTemplate: '`' StringTemplateChar* '`';

fragment StringTemplateChar : '{{' | '}}' | ~[`];
fragment FragEscapeQuot     : '""';
fragment FragEscapeApos     : '\'\'';

// Pragma: (# EQName PragmaContents #) -- entire pragma is one token.
// PragmaContents ::= (Char* - (Char* '#)' Char*))
Pragma                     : '(#' S? FragQName (S PragmaContentChar*)? '#)';
fragment PragmaContentChar : ~[#] | '#' ~[)];
fragment S                 : [\u0009\u000a\u000d\u0020]+;

// XQuery comments (nested, off-channel)
Comment: '(:' (Comment | CommentContents)*? ':)' -> skip;

// Direct element constructor: '<' when followed by an NCNameStartChar starts an element.
// IsNCNameStart() checks _input.LA(1) in XQuery4LexerBase.
OPEN_TAG : '<' { IsNCNameStart() }? -> pushMode(IN_ELEMENT_TAG);
LT       : '<';

// Right curly brace: pops mode when inside embedded element/attribute expression.
// PopModeIfNeeded() in XQuery4LexerBase calls popMode() only when the mode stack is non-empty.
CC: '}' { PopModeIfNeeded(); };

QName  : FragQName;
NCName : FragmentNCName;


fragment Char            : FragChar;
fragment FragDigits      : [0-9]+;
fragment HexDigits       : [a-fA-F0-9]+;
fragment BinaryDigits    : [01] ([01_]* [01])?;
fragment CommentContents : Char;

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

fragment FragChar:
    '\u0009'
    | '\u000a'
    | '\u000d'
    | '\u0020' ..'\ud7ff'
    | '\ue000' ..'\ufffd'
    | '\u{10000}' ..'\u{10ffff}'
;

StringConstructor: '``[' .*? ']``';



Whitespace: [\u000d\u000a\u0020\u0009]+ -> skip;

// ============================================================
// IN_ELEMENT_TAG: Processing <ElemName attr="val" ... (> or />)
// ============================================================
mode IN_ELEMENT_TAG;

ET_QName    : FragQName                   -> type(QName);
ET_COLON    : ':'                         -> type(COLON);
ET_EQ       : '='                         -> type(EQ);
ET_SLASH_GT : '/>'                        -> popMode;
ET_GT       : '>'                         -> mode(IN_ELEMENT_CONTENT);
ET_DQ_OPEN  : '"'                         -> pushMode(IN_ATTR_VALUE_QUOT);
ET_SQ_OPEN  : '\''                        -> pushMode(IN_ATTR_VALUE_APOS);
ET_WS       : [\u000d\u000a\u0020\u0009]+ -> skip;

// ============================================================
// IN_ATTR_VALUE_QUOT: Inside a double-quoted attribute value
// ============================================================
mode IN_ATTR_VALUE_QUOT;

AV_QUOT_ESCAPE : '""'              -> type(EscapeQuot);
AV_QUOT_ENTITY : FragPredEntityRef -> type(PredefinedEntityRef);
AV_QUOT_CREF   : FragCharRef       -> type(CharRef);
AV_QUOT_OC     : '{'               -> type(OC), pushMode(DEFAULT_MODE);
AV_QUOT_CLOSE  : '"'               -> popMode;
AV_QUOT_CHARS  : ~["{&<]+          -> type(QuotAttrContentChar);

// ============================================================
// IN_ATTR_VALUE_APOS: Inside a single-quoted attribute value
// ============================================================
mode IN_ATTR_VALUE_APOS;

AV_APOS_ESCAPE : '\'\''            -> type(EscapeApos);
AV_APOS_ENTITY : FragPredEntityRef -> type(PredefinedEntityRef);
AV_APOS_CREF   : FragCharRef       -> type(CharRef);
AV_APOS_OC     : '{'               -> type(OC), pushMode(DEFAULT_MODE);
AV_APOS_CLOSE  : '\''              -> popMode;
AV_APOS_CHARS  : ~['&{<]+          -> type(AposAttrContentChar);

// ============================================================
// IN_ELEMENT_CONTENT: Between > and </
// ============================================================
mode IN_ELEMENT_CONTENT;

EC_CDATA  : '<![CDATA[' .*? ']]>' -> type(CDataSection);
EC_XMLCMT : '<!--' .*? '-->'      -> type(DirCommentContents);
EC_PI:
    '<?' FragmentNCName ([\u0009\u000a\u000d\u0020] (~[?] | '?' ~[>])*)? '?>' -> type(DirPIContents)
;
EC_OPEN_TAG  : '<'                         { IsNCNameStart() }? -> type(OPEN_TAG), pushMode(IN_ELEMENT_TAG);
EC_CLOSE_TAG : '</'                        -> pushMode(IN_CLOSE_TAG);
EC_OC_ESCAPE : '{{'                        -> type(LCurlyBraceEscape);
EC_CC_ESCAPE : '}}'                        -> type(RCurlyBraceEscape);
EC_OC        : '{'                         -> type(OC), pushMode(DEFAULT_MODE);
EC_ENTITY    : FragPredEntityRef           -> type(PredefinedEntityRef);
EC_CREF      : FragCharRef                 -> type(CharRef);
EC_CHAR      : FragElemContentChar+        -> type(ElementContentChar);
EC_WS        : [\u0009\u000a\u000d\u0020]+ -> type(ElementContentChar);

// ============================================================
// IN_CLOSE_TAG: Processing </ElemName>
// ============================================================
mode IN_CLOSE_TAG;

CT_QName : FragQName                   -> type(QName);
CT_WS    : [\u0009\u000a\u000d\u0020]+ -> skip;
CT_GT    : '>'                         -> popMode, popMode;

// ============================================================
// Shared fragments across modes
// ============================================================
fragment FragPredEntityRef   : '&' ('lt' | 'gt' | 'amp' | 'quot' | 'apos') ';';
fragment FragCharRef         : '&#' [0-9]+ ';' | '&#x' [0-9a-fA-F]+ ';';
fragment FragElemContentChar : ~[&<{}];