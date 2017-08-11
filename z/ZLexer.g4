/*
 [The "BSD licence"]
 Copyright (c) 2017 Adam Taylor
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


lexer grammar ZLexer;
//import classify;

@lexer::members {
	public static final Integer[] BOTH_VALUES = new Integer[] { ELSE, FUNCTION, GENERIC, LEFTASSOC, PARENTS, RELATION, RIGHTASSOC, SECTION, THEN, FREE_EQUALS, VERTICAL_LINE, LEFT_DOUBLE_ANGLE_BRACKET, RIGHT_DOUBLE_ANGLE_BRACKET, AMPERSAND, RIGHT_TACK, LIST, LOGICAL_AND, LOGICAL_OR, RIGHTWARDS_DOUBLE_ARROW, LEFT_RIGHT_DOUBLE_ARROW, MULTIPLICATION_SIGN, SOLIDUS, EQUALS_SIGN, ELEMENT_OF, DEFINE_EQUAL, COLON, SEMICOLON, COMMA, FULL_STOP, SPOT, BIG_REVERSE_SOLIDUS, SCHEMA_PROJECTION, SCHEMA_COMPOSITION, SCHEMA_PIPING };
	public static final Integer[] AFTER_VALUES = new Integer[] { IF, LET, PRE_KEY, LEFT_SQUARE_BRACKET, ARGUMENT, NOT_SIGN, FOR_ALL, THERE_EXISTS, POWERSET, LEFT_PARENTHESIS, LEFT_CURLY_BRACKET, LEFT_BINDING_BRACKET, GREEK_SMALL_LETTER_LAMBDA, GREEK_SMALL_LETTER_MU, GREEK_SMALL_LETTER_THETA, ZED, AX, SCH /*GENAX GENSCH*/};
	public static final Integer[] BEFORE_VALUES = new Integer[] { RIGHT_SQUARE_BRACKET, RIGHT_PARENTHESIS, RIGHT_CURLY_BRACKET, RIGHT_BINDING_BRACKET, END};
	
	public static final java.util.Set<Integer> BOTH = new java.util.HashSet<Integer>(java.util.Arrays.asList(BOTH_VALUES));
	public static final java.util.Set<Integer> AFTER = new java.util.HashSet<Integer>(java.util.Arrays.asList(AFTER_VALUES));
	public static final java.util.Set<Integer> BEFORE = new java.util.HashSet<Integer>(java.util.Arrays.asList(BEFORE_VALUES));
	
	int lastTokenType = 0;
	public void emit(Token token) {
 	   super.emit(token);
 	   lastTokenType = token.getType();
	}
	
	public boolean shouldNL(int nextToken) {
		if(BOTH.contains(lastTokenType)) {
			return false;
		} else if(AFTER.contains(lastTokenType)) {
			return false;
		} else if(BEFORE.contains(nextToken)) {
			return false;
		}

		return true;
	}
}

// http://standards.iso.org/ittf/PubliclyAvailableStandards/c021573_ISO_IEC_13568_2002(E).zip
// https://www.iso.org/obp/ui/#iso:std:iso-iec:13568:ed-1:v1:cor:1:v1:en

// 6.4.4.4 Box characters
ZED: '\u2500' -> mode(Z); // In line 6, replace "| 0000 2028 LINE SEPARATOR" by "— 0000 2500 BOX DRAWINGS LIGHT HORIZONTAL".
SCH: '\u250C' -> mode(Z); // ┌
AX: '\u2577'  -> mode(Z); // ╷
TEXT: ~[\u2500\u250C\u2577]+ -> channel(HIDDEN);

mode Z;

// 7.5 Newlines
NUMERAL : DECIMAL+ ;
STROKE : (STROKECHAR | SOUTH_EAST_ARROW DECIMAL NORTH_WEST_ARROW);

// 6.4.4.3 Bracket characters
LEFT_PARENTHESIS: '\u0028'; // (
RIGHT_PARENTHESIS: '\u0029'; // )
LEFT_SQUARE_BRACKET: '\u005B'; // [
RIGHT_SQUARE_BRACKET: '\u005D'; // ]
LEFT_CURLY_BRACKET: '\u007B'; // {
RIGHT_CURLY_BRACKET: '\u007D'; // }
LEFT_BINDING_BRACKET: '\u2989'; // ⦉ 
RIGHT_BINDING_BRACKET: '\u298A'; // ⦊ 
LEFT_DOUBLE_ANGLE_BRACKET: '\u27EA'; //《 In line 10, replace "0000 300A LEFT DOUBLE ANGLE BRACKET" by "0000 27EA MATHEMATICAL LEFT DOUBLE ANGLE BRACKET".
RIGHT_DOUBLE_ANGLE_BRACKET: '\u27EB'; // 》In line 11, replace "0000 300B RIGHT DOUBLE ANGLE BRACKET" by "0000 27EB MATHEMATICAL RIGHT DOUBLE ANGLE BRACKET".

// 6.4.4.4 Box characters
GEN: '\u2550'; // ═ 
END: '\u2514' -> mode(DEFAULT_MODE); // In line 10, replace "(new line) 0000 2029 PARAGRAPH SEPARATOR" by "| 0000 2514 BOX DRAWINGS LIGHT UP AND RIGHT".

// 6.4.4.5 Other SPECIAL characters
NLCHAR: '\u2028' -> type(NL); // In line 2, replace "0000 000A LINE FEED" by "0000 2028 LINE SEPARATOR".
//SPACE: '\u0020'; // ' '

WS:  [\p{Zs}]+ -> skip; // All Unicode characters with General Category Zs shall be treated as SPACE.
NL: [\r\n]+ {shouldNL(_input.LA(1))}?;
//NL:  [\r\n]+ -> channel(HIDDEN);
IGNORE_NL: [\r\n]+  -> skip;


// 7.4.2 Alphabetic keywords
ELSE: 'else';
FALSE: 'false';
FUNCTION: 'function';
GENERIC : 'generic';
IF : 'if';
LEFTASSOC : 'leftassoc';
LET : 'let';
POWERSET : '\u2119'; // ℙ
PARENTS : 'parents';
PRE_KEY : 'pre';
RELATION : 'relation';
RIGHTASSOC : 'rightassoc';
SECTION : 'section';
THEN : 'then';
TRUE : 'true';

// 7.4.3 Symbolic keywords
COLON : ':';
DEFINE_EQUAL : '==';
COMMA : ',';
FREE_EQUALS : '::=';
VERTICAL_LINE : '|';
AMPERSAND : '\u0026'; // & 
REVERSE_SOLIDUS : '\u005C'; // \
SOLIDUS: '/';
FULL_STOP : '.'; // SELECT
SEMICOLON : ';';
ARGUMENT: '_';
LIST: ',,';
EQUALS_SIGN: '=';

CONJECTURE : '\u22A2' QUESTION_MARK; // ⊢?
FOR_ALL : '\u2200'; // ∀
SPOT : '\u2981'; // ⦁
THERE_EXISTS : '\u2203'; // ∃
UNIQUE_EXISTS : THERE_EXISTS SOUTH_EAST_ARROW '1' NORTH_WEST_ARROW; // ∃0
LEFT_RIGHT_DOUBLE_ARROW : '\u21D4'; // ⇔
RIGHTWARDS_DOUBLE_ARROW : '\u21D2'; // ⇒
LOGICAL_OR : '\u2228'; // ∨
LOGICAL_AND : '\u2227'; // ∧
NOT_SIGN : '\u00AC'; // ¬
ELEMENT_OF : '\u2208'; // ∈
SCHEMA_PROJECTION : '\u2A21'; // ⨡
MULTIPLICATION_SIGN : '\u00D7'; // x
GREEK_SMALL_LETTER_THETA : '\u03B8'; // θ
GREEK_SMALL_LETTER_LAMBDA : '\u03BB'; // λ
GREEK_SMALL_LETTER_MU : '\u03BC'; // μ
SCHEMA_COMPOSITION : '\u2A1F'; // ⨟
SCHEMA_PIPING : '\u2A20'; // ⨠

NAME
	: WORD STROKE*
	;
	
// 7.2 Formal definition of context free lexis
// modified to fit section 7.4.1 
fragment
WORD
	: WORDPART+
	| (LETTER | NONDECIMAL) ALPHASTR WORDPART*
	| SYMBOL SYMBOLSTR WORDPART*
	| PUNCT+ EQUALS_SIGN?
	;
	
fragment
//WORDPART
//	: ALPHASTR
//	| SYMBOLSTR
//	| SOUTH_EAST_ARROW WORDPART*? NORTH_WEST_ARROW 	// nesting allowed (but should it be?)
//	| NORTH_EAST_ARROW WORDPART*? SOUTH_WEST_ARROW  // nesting allowed (but should it be?)
//	; 
 
WORDGLUE : SOUTH_EAST_ARROW | NORTH_WEST_ARROW | NORTH_EAST_ARROW | SOUTH_WEST_ARROW | '_';
WORDPART : WORDGLUE (ALPHASTR | SYMBOLSTR); 

fragment
ALPHASTR
	:	(LETTER | DIGIT)* 
	;

fragment
SYMBOLSTR
	: SYMBOL*
	;

fragment
DIGIT
	: DECIMAL
	| NONDECIMAL // any other UCS characters with General Category N* except Nd
	;

fragment
DECIMAL
    : [\p{Nd}] // Decimal number
	;
	
fragment
NONDECIMAL
    : [\p{Nl}] // Letter number
    | [\p{No}] // Other number
	;

fragment
LETTER
	: LATIN
	| GREEK
	| OTHERLETTER
	| OTHER_MATH_TOOLKIT_LETTERS // characters of the mathematical toolkit with General Category neither L* nor N*
	| OTHER_UCS_LETTERS // any other UCS characters with General Category L*
	;
	
fragment
OTHER_MATH_TOOLKIT_LETTERS
	: MATHEMATICAL_DOUBLE_STRUCK_CAPITAL_F 
	| DOUBLE_STRUCK_CAPITAL_Z 
	;
	
fragment
OTHER_UCS_LETTERS
    : [\p{L}] // Letter
	;

fragment
LATIN
	: [A-Za-z]
	;

fragment	
GREEK
	: GREEK_CAPITAL_LETTER_DELTA
	| GREEK_CAPITAL_LETTER_XI
	| GREEK_SMALL_LETTER_THETA
	| GREEK_SMALL_LETTER_LAMBDA
	| GREEK_SMALL_LETTER_MU
	;
	
// 6.4.3.2 Greek alphabet characters
GREEK_CAPITAL_LETTER_DELTA : '\u0394'; // Δ
GREEK_CAPITAL_LETTER_XI : '\u039E'; // Ξ

// 6.4.3.3 Other Z core language letter characters
MATHEMATICAL_DOUBLE_STRUCK_CAPITAL_A : '\uD835\uDD38'; // 𝔸
DOUBLE_STRUCK_CAPITAL_N : '\u2115'; // ℕ
	

fragment
OTHERLETTER
	: MATHEMATICAL_DOUBLE_STRUCK_CAPITAL_A
	| DOUBLE_STRUCK_CAPITAL_N
	| POWERSET
	;

fragment
PUNCT
	: COMMA
	| SEMICOLON
	| COLON
	| FULL_STOP//SELECT
	;

fragment
STROKECHAR
	: MODIFIER_LETTER_PRIME
	| EXCLAMATION_MARK
	| QUESTION_MARK
	;
	
// 6.4.4.1 Stroke characters
MODIFIER_LETTER_PRIME : '\u2032'; // ′ In line 2, replace "0000 02B9 MODIFIER LETTER PRIME" by "0000 2032 PRIME".
EXCLAMATION_MARK : '\u0021'; // !
QUESTION_MARK : '\u003F'; // ?

	
// 6.4.4.2 Word glue characters
NORTH_EAST_ARROW : '\u2197'; // ↗
SOUTH_WEST_ARROW : '\u2199'; // ↙
SOUTH_EAST_ARROW : '\u2198'; // ↘
NORTH_WEST_ARROW : '\u2196'; // ↖
//LOW_LINE : '\u005F'; //  _

fragment SYMBOL
	: VERTICAL_LINE
	| AMPERSAND
	| RIGHT_TACK
	| LOGICAL_AND
	| LOGICAL_OR
	| RIGHTWARDS_DOUBLE_ARROW
	| LEFT_RIGHT_DOUBLE_ARROW
	| NOT_SIGN
	| FOR_ALL
	| THERE_EXISTS
	| MULTIPLICATION_SIGN
	| SOLIDUS
	| EQUALS_SIGN
	| ELEMENT_OF
	| SPOT
	| BIG_REVERSE_SOLIDUS
	| SCHEMA_PROJECTION
	| SCHEMA_COMPOSITION
	| SCHEMA_PIPING
	| PLUS_SIGN
	| MATHEMATICAL_TOOLKIT_SYMBOLS // characters of the mathematical toolkit with General Category neither L* nor N*
	| OTHER_UCS_SYMBOLS // any other UCS characters with General Category S*, P* or M* and that are not in SPECIAL
	;
	
fragment
OTHER_UCS_SYMBOLS
    : [\p{S}] // Currency Symbol
    | [\p{P}] // Connector Punctuation
    | [\p{Mc}] // Spacing mark
	;
	
// Insert "6.4.4 Punctuation characters
//COLON : '\u003A'; // : 
//SEMICOLON : '\u003B'; // ;
//COMMA : '\u002C'; // ,
//FULL_STOP : '\u002E'; // . 

// 6.4.5 Symbol characters except mathematical toolkit characters
//VERTICAL_LINE: '\u007C'; // |
//AMPERSAND : '\u0026'; // & 
RIGHT_TACK : '\u22A2'; // ⊢
//LOGICAL_AND : '\u2227'; // ∧
//LOGICAL_OR : '\u2228'; // ∨
//RIGHTWARDS_DOUBLE_ARROW : '\u21D2'; // ⇒
//LEFT_RIGHT_DOUBLE_ARROW : '\u21D4'; // ⇔
//NOT_SIGN : '\u00AC'; // ¬
//FOR_ALL : '\u2200'; // ∀
//THERE_EXISTS : '\u2203'; // ∃
//MULTIPLICATION_SIGN : '\u00D7'; // x
//SOLIDUS : '\u002F'; // /
//EQUALS_SIGN : '\u003D'; // = 
//ELEMENT_OF : '\u2208'; // ∈
//SPOT : '\u2981'; // ⦁
BIG_REVERSE_SOLIDUS : '\u29F9'; // ⧹
//SCHEMA_PROJECTION : '\u2A21'; // ⨡
//SCHEMA_COMPOSITION : '\u2A1F'; // ⨟
//SCHEMA_PIPING : '\u2A20'; // ⨠
PLUS_SIGN : '\u002B'; // + 

// 6.4.6 Mathematical toolkit characters
//fragment
MATHEMATICAL_TOOLKIT_SYMBOLS
	: SET_TOOLKIT
	| RELATION_TOOLKIT
	| FUNCTION_TOOLKIT
	| NUMBER_TOOLKIT
	| SEQUENCE_TOOLKIT
	;
	
// 6.4.6.1 Section set toolkit
LEFT_RIGHT_ARROW : '\u2194'; // ↔
RIGHTWARDS_ARROW : '\u2192'; // →
NOT_EQUAL_TO : '\u2260'; // ≠
NOT_AN_ELEMENT_OF : '\u2209'; // ∉
EMPTY_SET : '\u2205'; // ∅
SUBSET_OF_OR_EQUAL_TO : '\u2286'; // ⊆
SUBSET_OF : '\u2282'; // ⊂
UNION : '\u222A'; // ∪
INTERSECTION : '\u2229'; // ∩
SET_MINUS : '\u2216'; // ∖ In line 11, replace "0000 005C REVERSE SOLIDUS" by "0000 2216 SET MINUS".
CIRCLED_MINUS : '\u2296'; // ⊖
N_ARY_UNION : '\u22C3'; // ⋃
N_ARY_INTERSECTION : '\u22C2'; // ⋂
MATHEMATICAL_DOUBLE_STRUCK_CAPITAL_F : '\uD835\uDD3D'; // 𝔽

fragment 
SET_TOOLKIT
	: LEFT_RIGHT_ARROW
	| RIGHTWARDS_ARROW
	| NOT_EQUAL_TO
	| NOT_AN_ELEMENT_OF
	| EMPTY_SET
	| SUBSET_OF_OR_EQUAL_TO
	| SUBSET_OF
	| UNION
	| INTERSECTION
	| REVERSE_SOLIDUS
	| CIRCLED_MINUS
	| N_ARY_UNION
	| N_ARY_INTERSECTION
	;
	
// 6.4.6.2 Section relation toolkit
RIGHTWARDS_ARROW_FROM_BAR : '\u21A6'; // ↦
RELATIONAL_COMPOSITION : '\u2A3E'; // ⨾
RING_OPERATOR : '\u2218'; // ∘
WHITE_LEFT_POINTING_TRIANGLE : '\u25C1'; // ◁
WHITE_RIGHT_POINTING_TRIANGLE : '\u25B7'; // ▷
DOMAIN_ANTIRESTRICTION : '\u2A64'; // ⩤
RANGE_ANTIRESTRICTION : '\u2A65'; // ⩥
TILDE_OPERATOR : '\u223C'; // ∼
LEFT_IMAGE_BRACKET : '\u2987'; // ⦇
RIGHT_IMAGE_BRACKET : '\u2988'; // ⦈
CIRCLED_PLUS : '\u2295'; // ⊕

fragment
RELATION_TOOLKIT
	: RIGHTWARDS_ARROW_FROM_BAR
	| RELATIONAL_COMPOSITION
	| RING_OPERATOR
	| WHITE_LEFT_POINTING_TRIANGLE
	| WHITE_RIGHT_POINTING_TRIANGLE
	| DOMAIN_ANTIRESTRICTION
	| RANGE_ANTIRESTRICTION
	| TILDE_OPERATOR
	| LEFT_IMAGE_BRACKET
	| RIGHT_IMAGE_BRACKET
	| CIRCLED_PLUS
	;
	
// 6.4.6.3 Section function toolkit
RIGHTWARDS_ARROW_WITH_VERTICAL_STROKE : '\u21F8'; // ⇸
RIGHTWARDS_ARROW_WITH_TAIL_WITH_VERTICAL_STROKE : '\u2914'; // ⤔
RIGHTWARDS_ARROW_WITH_TAIL : '\u21A3'; // ↣
RIGHTWARDS_TWO_HEADED_ARROW_WITH_VERTICAL_STROKE : '\u2900'; // ⤀
RIGHTWARDS_TWO_HEADED_ARROW : '\u21A0'; // ↠
RIGHTWARDS_TWO_HEADED_ARROW_WITH_TAIL : '\u2916'; // ⤖
RIGHTWARDS_ARROW_WITH_DOUBLE_VERTICAL_STROKE : '\u21FB'; // ⇻
RIGHTWARDS_ARROW_WITH_TAIL_WITH_DOUBLE_VERTICAL_STROKE : '\u2915'; // ⤕

fragment
FUNCTION_TOOLKIT
	: RIGHTWARDS_ARROW_WITH_VERTICAL_STROKE
	| RIGHTWARDS_ARROW_WITH_TAIL_WITH_VERTICAL_STROKE
	| RIGHTWARDS_ARROW_WITH_TAIL
	| RIGHTWARDS_TWO_HEADED_ARROW_WITH_VERTICAL_STROKE
	| RIGHTWARDS_TWO_HEADED_ARROW
	| RIGHTWARDS_TWO_HEADED_ARROW_WITH_TAIL
	| RIGHTWARDS_ARROW_WITH_DOUBLE_VERTICAL_STROKE
	| RIGHTWARDS_ARROW_WITH_TAIL_WITH_DOUBLE_VERTICAL_STROKE
	;
	
// 6.4.6.4 Section number toolkit
DOUBLE_STRUCK_CAPITAL_Z : '\u2124'; // ℤ
HYPHEN_MINUS : '\u002D'; // -
MINUS_SIGN : '\u2212'; // −
LESS_THAN_OR_EQUAL_TO : '\u2264'; // ≤
LESS_THAN_SIGN : '\u003C'; // <
GREATER_THAN_OR_EQUAL_TO : '\u2265'; // ≥
GREATER_THAN_SIGN : '\u003E'; // >
ASTERISK : '\u002A'; // *

// 6.4.6.4 Section number toolkit
fragment
NUMBER_TOOLKIT
	: HYPHEN_MINUS
	| MINUS_SIGN
	| LESS_THAN_OR_EQUAL_TO
	| LESS_THAN_SIGN
	| GREATER_THAN_OR_EQUAL_TO
	| GREATER_THAN_SIGN
	| ASTERISK
	;
	
// 6.4.6.5 Section sequence toolkit
NUMBER_SIGN : '\u0023'; // #
CHARACTER_TIE : '\u2040'; // ⁀
UPWARDS_HARPOON_WITH_BARB_LEFTWARDS : '\u21BF'; // ↿
UPWARDS_HARPOON_WITH_BARB_RIGHTWARDS : '\u21BE'; // ↾

LEFT_ANGLE_BRACKET : '\u27E8'; // 〈 In line 3, replace "0000 3008 LEFT ANGLE BRACKET" by "0000 27E8 MATHEMATICAL LEFT ANGLE BRACKET".
RIGHT_ANGLE_BRACKET : '\u27E9'; // 〉In line 4, replace "0000 3009 RIGHT ANGLE BRACKET" by "0000 27E9 MATHEMATICAL RIGHT ANGLE BRACKET".

fragment
SEQUENCE_TOOLKIT
	: NUMBER_SIGN
	| LEFT_ANGLE_BRACKET
	| RIGHT_ANGLE_BRACKET
	| CHARACTER_TIE
	| UPWARDS_HARPOON_WITH_BARB_LEFTWARDS
	| UPWARDS_HARPOON_WITH_BARB_RIGHTWARDS
	;


ID0 : [\p{L}] | '_'             // hand-written rule
;

ID1 : ID0 | [\p{Nd}]      // hand-written rule
;

ID : ID0 ID1 *            // hand-written rule
;

PREP : NAME;
PRE : NAME;
POSTP : NAME;
POST : NAME;
IP : NAME;
I : NAME;
LP :  NAME;
L : NAME;
ELP : NAME;
EL : NAME;
ERP : NAME;
ER : NAME;
SRP : NAME;
SR : NAME;
EREP : NAME;
ERE : NAME;
SREP : NAME;
SRE : NAME;
ES : NAME;
SS : NAME;

UNKNOWN : . ;

