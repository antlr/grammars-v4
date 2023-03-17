/*
 [The "BSD licence"]
 Copyright (c) 2014 Leonardo Lucena
 Copyright (c) 2018 Andrey Stolyarov
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
   Derived from https://github.com/scala/scala/blob/2.12.x/spec/13-syntax-summary.md
 */

lexer grammar ScalaLexer;

tokens {
	LBraceXML,
	RBraceXML,
	XMLComment
}

Null: 'null' {popModeForIdentifier();};
This: 'this' {popModeForIdentifier();};
Super: 'super';
ForSome: 'forSome';
TypeKW: 'type';
Val: 'val';
Var: 'var';
Def: 'def';
With: 'with';
Implicit: 'implicit';
If: 'if';
While: 'while';
Do: 'do';
Else: 'else';
MatchKW: 'match';
Case: 'case';
For: 'for';
Try: 'try';
Catch: 'catch';
Throw: 'throw';
Return: 'return';
Finally: 'finally';
Yield: 'yield';
New: 'new';
Lazy: 'lazy';
Extends: 'extends';
Class: 'class';
Trait: 'trait';
Abstract: 'abstract';
Final: 'final';
Private: 'private';
Protected: 'protected';
Public: 'public';
Package: 'package';
Object: 'object';
Import: 'import';
Override: 'override';
Sealed: 'sealed';
Macro: 'macro';

Minus: '-';
Dot: '.';
Comma: ',';
LBracket: '[';
RBracket: ']';
LParen: '(';
RParen: ')';
LBrace: '{' { addCurly(); };
RBrace: '}' { onRBrace(); };

Hash: '#';
SemiColon: ';';
Colon: ':';
UnderScore: '_' { popModeForIdentifier();};
Star: '*';
Arrow: '=>';
Eq: '=';
Plus: '+';
Tilde: '~';
Exclamation: '!';
LShift: '<<';
Assign: '<-';
Or: '|';
At: '@';
Dollar: '$';
Quote: '\'';
DoubleQuote: '"';
LowerType: '>:';
UpperType: '<:';
ViewBound: '<%';
BackTick: '`';
BooleanLiteral: 'true' | 'false' { popModeForIdentifier();};

PIDefault: '<?' .*? '?>';
CDataChunkDefault: '<![CDATA[' .*? ']]>';
XMLOpenTag: (NL | WhiteSpace)? '<' (XNameStart | '!' | '?') {startXMLMode();};
fragment XNameStart:
	[a-zA-Z]
	| '\u2070' ..'\u218F'
	| '\u2C00' ..'\u2FEF'
	| '\u3001' ..'\uD7FF'
	| '\uF900' ..'\uFDCF'
	| '\uFDF0' ..'\uFFFD';

fragment TripleDoubleQuote
	: DoubleQuote DoubleQuote DoubleQuote
	;

InterpolatedMultiLineStringStart:
	{ canOpenInterpolatedString() }? (AlphaId | VarId) DoubleQuote DoubleQuote DoubleQuote+ { interpolatedStringLevel++;
		} -> pushMode(InterpolationStringMultiLine);

InterpolatedSingleLineStringStart
	: { canOpenInterpolatedString() }? (AlphaId | VarId) DoubleQuote { interpolatedStringLevel++;
		} -> pushMode(InterpolationStringSingleLine)
	;

BackTickId:
	BackTick (CharNoBackQuoteOrNewline | EscapeSeq)* BackTick { popModeForIdentifier();};

AlphaId
	: Upper IdRest {!getText().endsWith("$") || interpolatedStringLevel == 0 }? { popModeForIdentifier(); }
	;

VarId
	: Lower IdRest { !getText().endsWith("$") || interpolatedStringLevel == 0 }? { popModeForIdentifier(); }
	;

fragment EscapeSeq
	: UnicodeEscape
	| CharEscapeSeq
	;

CharacterLiteral
	: Quote ( ~['\n\r] | EscapeSeq) Quote
	;

IntegerLiteral
	: (DecimalNumeral | HexNumeral) [Ll]?
	;

StringLiteral
	: TripleDoubleQuote .*? /*TODO make more specific */ DoubleQuote* TripleDoubleQuote
	| DoubleQuote StringElement* DoubleQuote
	;

FloatingPointLiteral
	: Digit+ Dot Digit+ ExponentPart? FloatType?
	| Dot Digit+ ExponentPart? FloatType?
	| Digit+ ExponentPart FloatType?
	| Digit+ ExponentPart? FloatType
	;

// \u0020-\u0026 """ !"#$%"""
// \u0028-\u007E """()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~"""
fragment CharNoBackQuoteOrNewline
   : [\u0020-\u0026\u0028-\u007E]
   ;

// fragments

fragment UnicodeEscape
   : '\\' 'u'+ HexDigit HexDigit HexDigit HexDigit
   ;

fragment WhiteSpace
	: [ \t]
	;

OpChar
	: [%&<>?/\\^\p{Sm}\p{So}]
	;

fragment Op
	: (
		OpChar
		| '?'
		| Hash
		| Exclamation
		| Plus
		| Minus
		| Colon
		| At
		| Or
		| Tilde
		| Star
		| LShift
		| Eq
	)+
	;

fragment IdRest
	: (Letter | Digit)* (UnderScore Op)?
	;

fragment StringElement
	: ~["\r\n\\] | UnicodeLetter | EscapeSeq
   ;

fragment MultiLineChars
	: (
		DoubleQuote? DoubleQuote? StringElement
		| NL
	)* DoubleQuote*
   ;

fragment HexDigit
	: [0-9A-Fa-f]
	;

fragment FloatType
	: [FfDd]
	;

fragment Upper
	: [A-Z$\p{Lu}\p{Lt}\p{Nl}]
	;

fragment Lower
	: [a-z_\p{LL}\p{Lo}]
	;

fragment Letter
	: Upper | Lower | [\p{Lm}\p{Lt}]
	;

fragment ExponentPart
	: [Ee] [+-]? Digit+
	;

fragment PrintableChar
	: ' '
	| PrintableCharExceptWhitespace
	;

fragment PrintableCharExceptWhitespace
	: PrintableCharExceptWhitespaceQuoteDollar
	| DoubleQuote
	| Dollar
	;

fragment PrintableCharExceptQuoteDollar
	: PrintableCharExceptWhitespaceQuoteDollar
	| ' '
	;

fragment PrintableCharExceptWhitespaceQuoteDollar
	: [\u0021\u0023\u0025-\u007F]
	;

fragment CharEscapeSeq
   : '\\' ('b' | 't' | 'n' | 'f' | 'r' | '"' | '\'' | '\\')
   ;

fragment DecimalNumeral
	: '0'
	| NonZeroDigit ('_' | Digit)*
	;

fragment HexNumeral
	: '0' 'x' HexDigit ('_' | HexDigit)*
   ;

fragment Digit
   : '0'
	| NonZeroDigit
   ;

fragment NonZeroDigit
	: [1-9]
   ;

fragment UnicodeLetter
   : [\p{Lu}\p{LT}\p{LM}\p{LO}]
   ;

//
// Whitespace and comments
//
fragment LineEnding: '\n' | '\r' '\n'?;

NL
   : LineEnding (WhiteSpace* LineEnding)?
   ;

WS
   :  WhiteSpace+ -> channel(HIDDEN)
   ;

COMMENT
   :   '/*' (COMMENT | .)*? '*/' -> channel(HIDDEN)
   ;

LINE_COMMENT
   :   '//' (~[\r\n])* -> channel(HIDDEN)
   ;


mode InterpolationStringSingleLine;

DoubleDollarSingle: '$$';

EscapedQuoteSingle: '$"';

DollarInsideSingle:
	'$' { curlyLevels.push(0); } -> pushMode(DEFAULT_MODE);

DoubleQuoteSingle: '"' { interpolatedStringLevel--; } -> popMode;

EscapeSingle: EscapeSeq;
StringSingle: ~('$' | '\\' | '"')+;

mode InterpolationStringMultiLine;

DoubleDollarMulti: '$$';

EscapedQuoteMulti: '$"';

DollarInsideMulti:
	'$' { curlyLevels.push(0); } -> pushMode(DEFAULT_MODE);

DoubleQuoteMulti: '"';

EscapeMulti: EscapeSeq;
StringMulti: ~('$' | '"')+;
TripleDoubleQuoteMulti:
	'"'* '"""' { interpolatedStringLevel--; } -> popMode;

mode XMLOutsideNode;

EntityRef: '&' Name ';';
CharRef: '&#' DIGIT+ ';' | '&#x' HEXDIGIT+ ';';

XMLCommentOutside:
	'<!--' .*? '-->' {setType(XMLComment);} -> channel(HIDDEN);

PI:
	'<?' .*? '?>';

CDataChunk: '<![CDATA[' .*? ']]>';

LBraceEscaped: '{{' {setType(CharData);};

LBraceXMLOutsideNode:
	'{' {curlyLevels.push(1); xmlLevels.push(0); setType(LBraceXML);  pushMode(DEFAULT_MODE);};

XMLClosingNodeTag:
	'<' '/' {  openingTags.push('/'); pushMode(XMLInsideNode);};

XMLOpenTagMode:
	'<' {xmlLevels.push(xmlLevels.pop()+1); String text = getText(); openingTags.push(text.charAt(text.length()-1)); setType(XMLOpenTag); pushMode(XMLInsideNode);
		};

CharData: ~[<&{]+;

mode XMLInsideNode;

XMLCommentModeInside:
	'<!--' .*? '-->' {setType(XMLComment);} -> channel(HIDDEN);

LBraceXMLInsideNode:
	'{' {curlyLevels.push(1); xmlLevels.push(0); setType(LBraceXML);} -> pushMode(DEFAULT_MODE);

XMLAutoClose:
	'/>' {  if (openingTags.pop() != '/') {
				xmlLevels.push(xmlLevels.pop()-1);
                if (xmlLevels.peek() == 0) {
                    popMode();
                    xmlLevels.pop();
                }
            } else throw new RuntimeException("Bad XML");
            popMode();
};

XMLCloseTag:
	'>' {             if (openingTags.pop() == '/') {
			  	xmlLevels.push(xmlLevels.pop()-1);
                if (xmlLevels.peek() == 0)
                {
                    popMode();
                    xmlLevels.pop();
                }
            }
            popMode();};

SLASH: '/';
EQUALS: '=';
XMLString: '"' ~[<"]* '"' | '\'' ~[<']* '\'';
Name: NameStartChar NameChar*;

S: [ \t\r\n] -> channel(HIDDEN);

fragment HEXDIGIT: [a-fA-F0-9];

fragment DIGIT: [0-9];

fragment NameChar:
	NameStartChar
	| '-'
	| '_'
	| '.'
	| DIGIT
	| '\u00B7'
	| '\u0300' .. '\u036F'
	| '\u203F' .. '\u2040';

fragment NameStartChar:
	[:a-zA-Z]
	| '\u2070' .. '\u218F'
	| '\u2C00' .. '\u2FEF'
	| '\u3001' .. '\uD7FF'
	| '\uF900' .. '\uFDCF'
	| '\uFDF0' .. '\uFFFD';

