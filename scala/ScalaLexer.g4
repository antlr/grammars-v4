lexer grammar ScalaLexer;
@lexer::header {
import java.util.Stack;
}

tokens {
	LBraceXML,
	RBraceXML,
	XMLComment
}

@lexer::members {
private int interpolatedStringLevel = 0;
private Stack<Integer> xmlLevels = new Stack<Integer>();
private Stack<Integer> curlyLevels = new Stack<Integer>();
private Stack<Character> openingTags = new Stack<Character>();

	@Override
	public void reset()
	{
	    interpolatedStringLevel=0;
	    xmlLevels.clear();
	    curlyLevels.clear();
		openingTags.clear();

		super.reset();
	}

	void popModeForIdentifier(){
		if (interpolatedStringLevel > 0
		&& ( _modeStack.peek() == InterpolationStringSingleLine  || _modeStack.peek() == InterpolationStringMultiLine)
		&& curlyLevels.peek() == 0 ){
	        popMode();
	        curlyLevels.pop();
		}
	}

	void addCurly(){
		if (interpolatedStringLevel > 0 || xmlLevels.size() > 0)
		{
	    	curlyLevels.push(curlyLevels.pop() + 1);
	    	if(curlyLevels.peek() == 1){
		    		xmlLevels.push(0);
			}

		}
	}

	void startXMLMode(){
		if(xmlLevels.size() == 0){
			xmlLevels.push(1);
		}
			else if (xmlLevels.peek() == 0) {
				xmlLevels.push(xmlLevels.pop()+1);
			}
			else xmlLevels.push(1);
		openingTags.push( getText().charAt(getText().length()-1) );
		pushMode(XMLOutsideNode);
		pushMode(XMLInsideNode);
	}

	boolean canOpenInterpolatedString(){
		return interpolatedStringLevel == 0 || (curlyLevels.size() > 0 && curlyLevels.peek() > 0);
	}

	void onRBrace() {
		if (interpolatedStringLevel > 0 || xmlLevels.size() > 0)
		{
			curlyLevels.push(curlyLevels.pop() - 1);
			if (curlyLevels.peek() == 0)
			{
				if(_modeStack.peek() == XMLInsideNode || _modeStack.peek() == XMLOutsideNode)
				{
					setType(RBraceXML);
				}
				curlyLevels.pop();
				if(xmlLevels.peek() == 0){
					xmlLevels.pop();
				}
				popMode();
			}

		}
	}

}

Null: 'null' {popModeForIdentifier();};
This: 'this' {popModeForIdentifier();};
Super: 'super';
ForSome: 'forSome';
Type: 'type';
Val: 'val';
Var: 'var';
Def: 'def';
With: 'with';
Implicit: 'implicit';
If: 'if';
While: 'while';
Do: 'do';
Else: 'else';
Match: 'match';
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

XMLOpenTag: (NL | WhiteSpace)? '<' (XNameStart | '!' | '?') {startXMLMode();};
fragment XNameStart:
	[a-zA-Z]
	| '\u2070' ..'\u218F'
	| '\u2C00' ..'\u2FEF'
	| '\u3001' ..'\uD7FF'
	| '\uF900' ..'\uFDCF'
	| '\uFDF0' ..'\uFFFD';

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
UnderScore: '_';
Star: '*';
Arrow: '=>';
Eq: '=';
Plus: '+';
Tilde: '~';
Exclamation: '!';
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

fragment TripleDoubleQuote: DoubleQuote DoubleQuote DoubleQuote;

InterpolatedMultiLineStringStart:
	{ canOpenInterpolatedString() }? (AlphaId | VarId) DoubleQuote DoubleQuote DoubleQuote+ { interpolatedStringLevel++;
		} -> pushMode(InterpolationStringMultiLine);

InterpolatedSingleLineStringStart:
	{ canOpenInterpolatedString() }? (AlphaId | VarId) DoubleQuote { interpolatedStringLevel++;
		} -> pushMode(InterpolationStringSingleLine);

COMMENT: '/*' (COMMENT | .)*? '*/' -> channel(HIDDEN);

LINE_COMMENT: '//' (~[\r\n])* -> channel(HIDDEN);

BackTickId:
	BackTick (CharNoBackQuoteOrNewline | EscapeSeq)* BackTick { popModeForIdentifier();};

AlphaId: Upper IdRest {!getText().endsWith("$") || interpolatedStringLevel == 0 }?  { popModeForIdentifier();};

VarId: Lower IdRest { !getText().endsWith("$") || interpolatedStringLevel == 0 }? { popModeForIdentifier();};

fragment EscapeSeq: UnicodeEscape | CharEscapeSeq;

CharacterLiteral:
	Quote (CharNoQuoteOrNewline | EscapeSeq) Quote;

IntegerLiteral: (DecimalNumeral | HexNumeral) [Ll]?;

StringLiteral:
	TripleDoubleQuote .*? /*TODO make more specific */ DoubleQuote* TripleDoubleQuote
	| DoubleQuote StringElement* DoubleQuote;

FloatingPointLiteral:
	Digit+ Dot Digit+ ExponentPart? FloatType?
	| Dot Digit+ ExponentPart? FloatType?
	| Digit+ ExponentPart FloatType?
	| Digit+ ExponentPart? FloatType;

// \u0020-\u0026 """ !"#$%"""

// \u0028-\u007E """()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~"""
fragment CharNoQuoteOrNewline: [\u0020-\u0026\u0028-\u007E];

fragment CharNoBackQuoteOrNewline: [\u0020-\u005f\u0061-\u007E];

// fragments

fragment UnicodeEscape:
	'\\' 'u'+ HexDigit HexDigit HexDigit HexDigit;

fragment WhiteSpace: [ \t];

OpChar: [%&<>?/\\^\p{Sm}\p{So}];

fragment Op: (
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
		| Eq
	)+;

fragment IdRest: (Letter | Digit)* (UnderScore Op)?;

fragment StringElement:
	[\u0020\u0021\u0023-\u005B\u005D-\u007F\p{So}\p{Sm}\p{Blk=Latin_1_Sup}\p{Symbol}]
	| UnicodeLetter
	| EscapeSeq;

fragment MultiLineChars: (
		DoubleQuote? DoubleQuote? StringElement
		| NL
	)* DoubleQuote*;

fragment HexDigit: [0-9A-Fa-f];

fragment FloatType: [FfDd];

fragment Upper: [A-Z$\p{Lu}\p{Lt}\p{Nl}];

fragment Lower: [a-z_\p{LL}];

fragment Letter: Upper | Lower | [\p{Lo}\p{Lt}];

fragment ExponentPart: [Ee] [+-]? Digit+;

fragment PrintableChar: ' ' | PrintableCharExceptWhitespace;

fragment PrintableCharExceptWhitespace:
	PrintableCharExceptWhitespaceQuoteDollar
	| DoubleQuote
	| Dollar;

fragment PrintableCharExceptQuoteDollar:
	PrintableCharExceptWhitespaceQuoteDollar
	| ' ';

fragment PrintableCharExceptWhitespaceQuoteDollar:
	[\u0021\u0023\u0025-\u007F];

CharEscapeSeq: '\\' [btnfr\\"'];

fragment DecimalNumeral: '0' | NonZeroDigit Digit*;

fragment HexNumeral: '0' 'x' HexDigit HexDigit*;

fragment Digit: '0' | NonZeroDigit;

fragment NonZeroDigit: [1-9];

fragment UnicodeLetter: [\p{Lu}\p{LT}\p{LM}\p{LO}];

//
// Whitespace and comments

fragment LineEnding: '\n' | '\r' '\n'?;

NL: LineEnding (WhiteSpace* LineEnding)*;

WS: WhiteSpace+ -> skip;

mode InterpolationStringSingleLine;

DoubleDollarSingle: '$$';

DollarInsideSingle:
	'$' { curlyLevels.push(0); } -> pushMode(DEFAULT_MODE);

DoubleQuoteSingle: '"' { interpolatedStringLevel--; } -> popMode;

EscapeSingle: EscapeSeq;
StringSingle: ~('$' | '"')+;

mode InterpolationStringMultiLine;

DoubleDollarMulti: '$$';

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