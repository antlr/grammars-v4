/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

grammar turtle;

turtleDoc
   : statement* EOF
   ;

statement
   : directive
   | triples Dot
   ;

directive
   : prefixID
   | base
   | sparqlPrefix
   | sparqlBase
   ;

prefixID
   : AtPrefixKeyword PNameNs IriRef Dot
   ;

base
   : AtBaseKeyword IriRef Dot
   ;

sparqlBase
   : BaseKeyword IriRef
   ;

sparqlPrefix
   : PrefixKeyword PNameNs IriRef
   ;

triples
   : subject predicateObjectList
   | blankNodePropertyList predicateObjectList?
   ;

predicateObjectList
   : predicateObject (Semi predicateObject?)*
   ;

objectList
   : object_ (Coma object_)*
   ;

predicateObject
   : (predicate | LetterA) objectList
   ;

subject
   : iri
   | blankNode
   | collection
   ;

predicate
   : iri
   ;

object_
   : iri
   | blankNode
   | collection
   | blankNodePropertyList
   | literal
   ;

literal
   : rDFLiteral
   | numericLiteral
   | bool_
   ;

blankNodePropertyList
   : LEnd predicateObjectList REnd
   ;

collection
   : LParen object_* RParen
   ;

numericLiteral
   : Integer
   | Decimal
   | Double
   ;

rDFLiteral
   : string (LangTag | ('^^' iri))?
   ;

bool_
   : TrueKeyword
   | FalseKeyword
   ;

string
   : StringLiteralQuote
   | StringLiteralSingleQuote
   | StringLiteralLongSingleQuote
   | StringLiteralLongQuote
   ;

iri
   : IriRef
   | PNameLn
   | PNameNs
   ;

blankNode
   : BlankNodeLabel
   | ANON
   ;


PNameLn
   : PNameNs PNLocal
   ;


LetterA
   : 'a'
   ;


Dot
   : '.'
   ;


Coma
   : ','
   ;


LParen
   : '('
   ;


RParen
   : ')'
   ;


LEnd
   : '['
   ;


REnd
   : ']'
   ;


IriRef
   : '<' ((~ [\u0000-\u0020<>"{}|^`\\]) | Uchar)* '>'
   ;

/* [\u0000]=NULL #01-[\u001F]=control codes [\u0020]=space */

PNameNs
   : PN_Prefix? ':'
   ;


BlankNodeLabel
   : '_:' (PNCharsU | [0-9]) ((PN_CHARS | Dot)* PN_CHARS)?
   ;


Integer
   : [+-]? [0-9] +
   ;


Decimal
   : [+-]? [0-9]* Dot [0-9] +
   ;


Double
   : [+-]? ([0-9] + Dot [0-9]* Exponent | Dot [0-9] + Exponent | [0-9] + Exponent)
   ;


Exponent
   : [eE] [+-]? [0-9] +
   ;


StringLiteralQuote
   : '"' ((~ [\u0022\u005C\u000A\u000D]) | ECHAR | Uchar)* '"'
   ;


StringLiteralSingleQuote
   : '\'' ((~ [\u0027\u005C\u000A\u000D]) | ECHAR | Uchar)* '\''
   ;


StringLiteralLongSingleQuote
   : '\'\'\'' (('\'' | '\'\'')? ((~ ['\\]) | ECHAR | Uchar))* '\'\'\''
   ;


StringLiteralLongQuote
   : '"""' (('\'' | '\'\'')? ((~ ['\\]) | ECHAR | Uchar))* '"""'
   ;


Uchar
   : '\\u' HEX HEX? HEX? HEX? | '\\U' HEX HEX? HEX? HEX? HEX? HEX? HEX? HEX?
   ;


ECHAR
   : '\\' [tbnrf"'\\]
   ;


WS
   : [\u0020\u0009\u000D\u000A] -> skip
   ;


ANON
   : LEnd WS* REnd
   ;


PN_CHARS_BASE
   : [A-Za-z\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02FF\u0370-\u037D\u037F-\u1FFF\u200C-\u200D\u2070-\u218F\u2C00-\u2FEF\u3001-\uD7FF\uF900-\uFDCF\uFDF0-\uFFFD\u1000-\uEFFFF]
   ;


PNCharsU
   : PN_CHARS_BASE | '_'
   ;


PN_CHARS
   : PNCharsU | '-' | [0-9\u00B7\u0300-\u036F\u203F-\u2040]
   ;


PN_Prefix
   : PN_CHARS_BASE ((PN_CHARS | Dot)* PN_CHARS)?
   ;


PNLocal
   : (PNCharsU | ':' | [0-9] | PLX) ((PN_CHARS | Dot | ':' | PLX)* (PN_CHARS | ':' | PLX))?
   ;


PLX
   : PERCENT | PN_LOCAL_ESC
   ;


PERCENT
   : '%' HEX HEX
   ;


HEX
   : [0-9A-Fa-f]
   ;


PN_LOCAL_ESC
   : '\\' ('_' | '~' | '.' | '-' | '!' | '$' | '&' | '\'' | '(' | ')' | '*' | '+' | ',' | ';' | '=' | '/' | '?' | '#' | '@' | '%')
   ;


Semi
   : ';'
   ;


TrueKeyword
   : T R U E
   ;


FalseKeyword
   : F A L S E
   ;


PrefixKeyword
   : P R E F I X
   ;


BaseKeyword
   : B A S E
   ;


AtPrefixKeyword
   : [@] PrefixKeyword
   ;


AtBaseKeyword
   : [@] BaseKeyword
   ;


LangTag
   : '@' [a-zA-Z] + ('-' [a-zA-Z0-9] +)*
   ;


fragment A
   : [aA]
   ;


fragment B
   : [bB]
   ;


fragment C
   : [cC]
   ;


fragment D
   : [dD]
   ;


fragment E
   : [eE]
   ;


fragment F
   : [fF]
   ;


fragment G
   : [gG]
   ;


fragment H
   : [hH]
   ;


fragment I
   : [iI]
   ;


fragment J
   : [jJ]
   ;


fragment K
   : [kK]
   ;


fragment L
   : [lL]
   ;


fragment M
   : [mM]
   ;


fragment N
   : [nN]
   ;


fragment O
   : [oO]
   ;


fragment P
   : [pP]
   ;


fragment Q
   : [qQ]
   ;


fragment R
   : [rR]
   ;


fragment S
   : [sS]
   ;


fragment T
   : [tT]
   ;


fragment U
   : [uU]
   ;


fragment V
   : [vV]
   ;


fragment W
   : [wW]
   ;


fragment X
   : [xX]
   ;


fragment Y
   : [yY]
   ;


fragment Z
   : [zZ]
   ;
