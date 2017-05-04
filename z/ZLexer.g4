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
TEXT: ~[\u2500\u250C]+ -> channel(HIDDEN);

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
AX: '\u2577'; // ╷
GEN: '\u2550'; // ═ 
END: '\u2514' -> mode(DEFAULT_MODE); // In line 10, replace "(new line) 0000 2029 PARAGRAPH SEPARATOR" by "| 0000 2514 BOX DRAWINGS LIGHT UP AND RIGHT".

// 6.4.4.5 Other SPECIAL characters
NLCHAR: '\u2028' -> type(NL); // In line 2, replace "0000 000A LINE FEED" by "0000 2028 LINE SEPARATOR".
//SPACE: '\u0020'; // ' '

WS: CLASSIFY_Zs+ -> skip; // All Unicode characters with General Category Zs shall be treated as SPACE.
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
	: CLASSIFY_Nd // UCS characters with General Category Nd
	;
	
fragment
NONDECIMAL
	: CLASSIFY_Nl
	| CLASSIFY_No
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
	: CLASSIFY_Ll
	| CLASSIFY_Lm
	| CLASSIFY_Lo
	| CLASSIFY_Lt
	| CLASSIFY_Lu
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
	: CLASSIFY_S
	| CLASSIFY_P
	| CLASSIFY_M
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

CLASSIFY___:
      '\u0378'..'\u037a'       // Greek_and_Coptic
    | '\u0380'..'\u0384'       // Greek_and_Coptic
    | '\u038b'                 // Greek_and_Coptic
    | '\u038d'                 // Greek_and_Coptic
    | '\u03a2'                 // Greek_and_Coptic
    | '\u0530'                 // Armenian
    | '\u0557'..'\u0559'       // Armenian
    | '\u0560'                 // Armenian
    | '\u0588'                 // Armenian
    | '\u058b'..'\u058d'       // Armenian
    | '\u0590'                 // Hebrew
    | '\u05c8'..'\u05d0'       // Hebrew
    | '\u05eb'..'\u05f0'       // Hebrew
    | '\u05f5'..'\u0600'       // Hebrew
    | '\u061d'                 // Arabic
    | '\u070e'                 // Syriac
    | '\u074b'..'\u074d'       // Syriac
    | '\u07b2'..'\u07c0'       // Thaana
    | '\u07fb'..'\u0800'       // NKo
    | '\u082e'..'\u0830'       // Samaritan
    | '\u083f'                 // (Absent from Blocks.txt)
    | '\u085c'..'\u085e'       // Mandaic
    | '\u085f'..'\u08a0'       // (Absent from Blocks.txt)
    | '\u08b5'                 // Arabic_Extended-A
    | '\u08be'..'\u08d4'       // Arabic_Extended-A
    | '\u0984'                 // Bengali
    | '\u098d'..'\u098f'       // Bengali
    | '\u0991'..'\u0993'       // Bengali
    | '\u09a9'                 // Bengali
    | '\u09b1'                 // Bengali
    | '\u09b3'..'\u09b6'       // Bengali
    | '\u09ba'..'\u09bc'       // Bengali
    | '\u09c5'..'\u09c7'       // Bengali
    | '\u09c9'..'\u09cb'       // Bengali
    | '\u09cf'..'\u09d7'       // Bengali
    | '\u09d8'..'\u09dc'       // Bengali
    | '\u09de'                 // Bengali
    | '\u09e4'..'\u09e6'       // Bengali
    | '\u09fc'..'\u0a01'       // Bengali
    | '\u0a04'                 // Gurmukhi
    | '\u0a0b'..'\u0a0f'       // Gurmukhi
    | '\u0a11'..'\u0a13'       // Gurmukhi
    | '\u0a29'                 // Gurmukhi
    | '\u0a31'                 // Gurmukhi
    | '\u0a34'                 // Gurmukhi
    | '\u0a37'                 // Gurmukhi
    | '\u0a3a'..'\u0a3c'       // Gurmukhi
    | '\u0a3d'                 // Gurmukhi
    | '\u0a43'..'\u0a47'       // Gurmukhi
    | '\u0a49'..'\u0a4b'       // Gurmukhi
    | '\u0a4e'..'\u0a51'       // Gurmukhi
    | '\u0a52'..'\u0a59'       // Gurmukhi
    | '\u0a5d'                 // Gurmukhi
    | '\u0a5f'..'\u0a66'       // Gurmukhi
    | '\u0a76'..'\u0a81'       // Gurmukhi
    | '\u0a84'                 // Gujarati
    | '\u0a8e'                 // Gujarati
    | '\u0a92'                 // Gujarati
    | '\u0aa9'                 // Gujarati
    | '\u0ab1'                 // Gujarati
    | '\u0ab4'                 // Gujarati
    | '\u0aba'..'\u0abc'       // Gujarati
    | '\u0ac6'                 // Gujarati
    | '\u0aca'                 // Gujarati
    | '\u0ace'..'\u0ad0'       // Gujarati
    | '\u0ad1'..'\u0ae0'       // Gujarati
    | '\u0ae4'..'\u0ae6'       // Gujarati
    | '\u0af2'..'\u0af9'       // Gujarati
    | '\u0afa'..'\u0b01'       // Gujarati
    | '\u0b04'                 // Oriya
    | '\u0b0d'..'\u0b0f'       // Oriya
    | '\u0b11'..'\u0b13'       // Oriya
    | '\u0b29'                 // Oriya
    | '\u0b31'                 // Oriya
    | '\u0b34'                 // Oriya
    | '\u0b3a'..'\u0b3c'       // Oriya
    | '\u0b45'..'\u0b47'       // Oriya
    | '\u0b49'..'\u0b4b'       // Oriya
    | '\u0b4e'..'\u0b56'       // Oriya
    | '\u0b58'..'\u0b5c'       // Oriya
    | '\u0b5e'                 // Oriya
    | '\u0b64'..'\u0b66'       // Oriya
    | '\u0b78'..'\u0b82'       // Oriya
    | '\u0b84'                 // Tamil
    | '\u0b8b'..'\u0b8e'       // Tamil
    | '\u0b91'                 // Tamil
    | '\u0b96'..'\u0b99'       // Tamil
    | '\u0b9b'                 // Tamil
    | '\u0b9d'                 // Tamil
    | '\u0ba0'..'\u0ba3'       // Tamil
    | '\u0ba5'..'\u0ba8'       // Tamil
    | '\u0bab'..'\u0bae'       // Tamil
    | '\u0bba'..'\u0bbe'       // Tamil
    | '\u0bc3'..'\u0bc6'       // Tamil
    | '\u0bc9'                 // Tamil
    | '\u0bce'..'\u0bd0'       // Tamil
    | '\u0bd1'..'\u0bd7'       // Tamil
    | '\u0bd8'..'\u0be6'       // Tamil
    | '\u0bfb'..'\u0c00'       // Tamil
    | '\u0c04'                 // Telugu
    | '\u0c0d'                 // Telugu
    | '\u0c11'                 // Telugu
    | '\u0c29'                 // Telugu
    | '\u0c3a'..'\u0c3d'       // Telugu
    | '\u0c45'                 // Telugu
    | '\u0c49'                 // Telugu
    | '\u0c4e'..'\u0c55'       // Telugu
    | '\u0c57'                 // Telugu
    | '\u0c5b'..'\u0c60'       // Telugu
    | '\u0c64'..'\u0c66'       // Telugu
    | '\u0c70'..'\u0c78'       // Telugu
    | '\u0c84'                 // Kannada
    | '\u0c8d'                 // Kannada
    | '\u0c91'                 // Kannada
    | '\u0ca9'                 // Kannada
    | '\u0cb4'                 // Kannada
    | '\u0cba'..'\u0cbc'       // Kannada
    | '\u0cc5'                 // Kannada
    | '\u0cc9'                 // Kannada
    | '\u0cce'..'\u0cd5'       // Kannada
    | '\u0cd7'..'\u0cde'       // Kannada
    | '\u0cdf'                 // Kannada
    | '\u0ce4'..'\u0ce6'       // Kannada
    | '\u0cf0'                 // Kannada
    | '\u0cf3'..'\u0d01'       // Kannada
    | '\u0d04'                 // Malayalam
    | '\u0d0d'                 // Malayalam
    | '\u0d11'                 // Malayalam
    | '\u0d3b'..'\u0d3d'       // Malayalam
    | '\u0d45'                 // Malayalam
    | '\u0d49'                 // Malayalam
    | '\u0d50'..'\u0d54'       // Malayalam
    | '\u0d64'..'\u0d66'       // Malayalam
    | '\u0d80'..'\u0d82'       // Sinhala
    | '\u0d84'                 // Sinhala
    | '\u0d97'..'\u0d9a'       // Sinhala
    | '\u0db2'                 // Sinhala
    | '\u0dbc'                 // Sinhala
    | '\u0dbe'..'\u0dc0'       // Sinhala
    | '\u0dc7'..'\u0dca'       // Sinhala
    | '\u0dcb'..'\u0dcf'       // Sinhala
    | '\u0dd5'                 // Sinhala
    | '\u0dd7'                 // Sinhala
    | '\u0de0'..'\u0de6'       // Sinhala
    | '\u0df0'..'\u0df2'       // Sinhala
    | '\u0df5'..'\u0e01'       // Sinhala
    | '\u0e3b'..'\u0e3f'       // Thai
    | '\u0e5c'..'\u0e81'       // Thai
    | '\u0e83'                 // Lao
    | '\u0e85'..'\u0e87'       // Lao
    | '\u0e89'                 // Lao
    | '\u0e8b'..'\u0e8d'       // Lao
    | '\u0e8e'..'\u0e94'       // Lao
    | '\u0e98'                 // Lao
    | '\u0ea0'                 // Lao
    | '\u0ea4'                 // Lao
    | '\u0ea6'                 // Lao
    | '\u0ea8'..'\u0eaa'       // Lao
    | '\u0eac'                 // Lao
    | '\u0eba'                 // Lao
    | '\u0ebe'..'\u0ec0'       // Lao
    | '\u0ec5'                 // Lao
    | '\u0ec7'                 // Lao
    | '\u0ece'..'\u0ed0'       // Lao
    | '\u0eda'..'\u0edc'       // Lao
    | '\u0ee0'..'\u0f00'       // Lao
    | '\u0f48'                 // Tibetan
    | '\u0f6d'..'\u0f71'       // Tibetan
    | '\u0f98'                 // Tibetan
    | '\u0fbd'                 // Tibetan
    | '\u0fcd'                 // Tibetan
    | '\u0fdb'..'\u1000'       // Tibetan
    | '\u10c6'                 // Georgian
    | '\u10c8'..'\u10cd'       // Georgian
    | '\u10ce'..'\u10d0'       // Georgian
    | '\u1249'                 // Ethiopic
    | '\u124e'..'\u1250'       // Ethiopic
    | '\u1257'                 // Ethiopic
    | '\u1259'                 // Ethiopic
    | '\u125e'..'\u1260'       // Ethiopic
    | '\u1289'                 // Ethiopic
    | '\u128e'..'\u1290'       // Ethiopic
    | '\u12b1'                 // Ethiopic
    | '\u12b6'..'\u12b8'       // Ethiopic
    | '\u12bf'                 // Ethiopic
    | '\u12c1'                 // Ethiopic
    | '\u12c6'..'\u12c8'       // Ethiopic
    | '\u12d7'                 // Ethiopic
    | '\u1311'                 // Ethiopic
    | '\u1316'..'\u1318'       // Ethiopic
    | '\u135b'..'\u135d'       // Ethiopic
    | '\u137d'..'\u1380'       // Ethiopic
    | '\u139a'..'\u13a0'       // Ethiopic_Supplement
    | '\u13f6'..'\u13f8'       // Cherokee
    | '\u13fe'..'\u1400'       // Cherokee
    | '\u169d'..'\u16a0'       // Ogham
    | '\u16f9'..'\u1700'       // Runic
    | '\u170d'                 // Tagalog
    | '\u1715'..'\u1720'       // Tagalog
    | '\u1737'..'\u1740'       // Hanunoo
    | '\u1754'..'\u1760'       // Buhid
    | '\u176d'                 // Tagbanwa
    | '\u1771'                 // Tagbanwa
    | '\u1774'..'\u1780'       // Tagbanwa
    | '\u17de'..'\u17e0'       // Khmer
    | '\u17ea'..'\u17f0'       // Khmer
    | '\u17fa'..'\u1800'       // Khmer
    | '\u180f'                 // Mongolian
    | '\u181a'..'\u1820'       // Mongolian
    | '\u1878'..'\u1880'       // Mongolian
    | '\u18ab'..'\u18b0'       // Mongolian
    | '\u18f6'..'\u1900'       // Unified_Canadian_Aboriginal_Syllabics_Extended
    | '\u191f'                 // Limbu
    | '\u192c'..'\u1930'       // Limbu
    | '\u193c'..'\u1940'       // Limbu
    | '\u1941'..'\u1944'       // Limbu
    | '\u196e'..'\u1970'       // Tai_Le
    | '\u1975'..'\u1980'       // Tai_Le
    | '\u19ac'..'\u19b0'       // New_Tai_Lue
    | '\u19ca'..'\u19d0'       // New_Tai_Lue
    | '\u19db'..'\u19de'       // New_Tai_Lue
    | '\u1a1c'..'\u1a1e'       // Buginese
    | '\u1a5f'                 // Tai_Tham
    | '\u1a7d'..'\u1a7f'       // Tai_Tham
    | '\u1a8a'..'\u1a90'       // Tai_Tham
    | '\u1a9a'..'\u1aa0'       // Tai_Tham
    | '\u1aae'..'\u1ab0'       // Tai_Tham
    | '\u1abf'..'\u1b00'       // Combining_Diacritical_Marks_Extended
    | '\u1b4c'..'\u1b50'       // Balinese
    | '\u1b7d'..'\u1b80'       // Balinese
    | '\u1bf4'..'\u1bfc'       // Batak
    | '\u1c38'..'\u1c3b'       // Lepcha
    | '\u1c4a'..'\u1c4d'       // Lepcha
    | '\u1c89'..'\u1cc0'       // Cyrillic_Extended-C
    | '\u1cc8'..'\u1cd0'       // Sundanese_Supplement
    | '\u1cf7'                 // Vedic_Extensions
    | '\u1cfa'..'\u1d00'       // Vedic_Extensions
    | '\u1df6'..'\u1dfb'       // Combining_Diacritical_Marks_Supplement
    | '\u1f16'..'\u1f18'       // Greek_Extended
    | '\u1f1e'..'\u1f20'       // Greek_Extended
    | '\u1f46'..'\u1f48'       // Greek_Extended
    | '\u1f4e'..'\u1f50'       // Greek_Extended
    | '\u1f58'                 // Greek_Extended
    | '\u1f5a'                 // Greek_Extended
    | '\u1f5c'                 // Greek_Extended
    | '\u1f5e'                 // Greek_Extended
    | '\u1f7e'..'\u1f80'       // Greek_Extended
    | '\u1fb5'                 // Greek_Extended
    | '\u1fc5'                 // Greek_Extended
    | '\u1fd4'..'\u1fd6'       // Greek_Extended
    | '\u1fdc'                 // Greek_Extended
    | '\u1ff0'..'\u1ff2'       // Greek_Extended
    | '\u1ff5'                 // Greek_Extended
    | '\u1fff'                 // (Absent from Blocks.txt)
    | '\u2072'..'\u2074'       // Superscripts_and_Subscripts
    | '\u208f'                 // Superscripts_and_Subscripts
    | '\u209d'..'\u20a0'       // Superscripts_and_Subscripts
    | '\u20bf'..'\u20d0'       // Currency_Symbols
    | '\u20f1'..'\u2100'       // Combining_Diacritical_Marks_for_Symbols
    | '\u218c'..'\u2190'       // Number_Forms
    | '\u2c2f'                 // Glagolitic
    | '\u2c5f'                 // (Absent from Blocks.txt)
    | '\u2cf4'..'\u2cf9'       // Coptic
    | '\u2d26'                 // Georgian_Supplement
    | '\u2d28'..'\u2d2d'       // Georgian_Supplement
    | '\u2d2e'..'\u2d30'       // Georgian_Supplement
    | '\u2d68'..'\u2d6f'       // Tifinagh
    | '\u2d71'..'\u2d7f'       // Tifinagh
    | '\u2d97'..'\u2da0'       // Ethiopic_Extended
    | '\u2da7'                 // Ethiopic_Extended
    | '\u2daf'                 // Ethiopic_Extended
    | '\u2db7'                 // Ethiopic_Extended
    | '\u2dbf'                 // Ethiopic_Extended
    | '\u2dc7'                 // Ethiopic_Extended
    | '\u2dcf'                 // Ethiopic_Extended
    | '\u2dd7'                 // Ethiopic_Extended
    | '\u2ddf'                 // (Absent from Blocks.txt)
    | '\u2e9a'                 // CJK_Radicals_Supplement
    | '\u2ef4'..'\u2f00'       // CJK_Radicals_Supplement
    | '\u2fd6'..'\u2ff0'       // Kangxi_Radicals
    | '\u2ffc'..'\u3000'       // Ideographic_Description_Characters
    | '\u3040'                 // Hiragana
    | '\u3097'..'\u3099'       // Hiragana
    | '\u3100'..'\u3105'       // Bopomofo
    | '\u312e'..'\u3131'       // Bopomofo
    | '\u318f'                 // (Absent from Blocks.txt)
    | '\u31bb'..'\u31c0'       // Bopomofo_Extended
    | '\u31e4'..'\u31f0'       // CJK_Strokes
    | '\u321f'                 // Enclosed_CJK_Letters_and_Months
    | '\u32ff'                 // (Absent from Blocks.txt)
    | '\u4db6'..'\u4dc0'       // CJK_Unified_Ideographs_Extension_A
    | '\u9fd6'..'\ua000'       // CJK_Unified_Ideographs
    | '\ua48d'..'\ua490'       // Yi_Syllables
    | '\ua4c7'..'\ua4d0'       // Yi_Radicals
    | '\ua62c'..'\ua640'       // Vai
    | '\ua6f8'..'\ua700'       // Bamum
    | '\ua7af'                 // Latin_Extended-D
    | '\ua7b8'..'\ua7f7'       // Latin_Extended-D
    | '\ua82c'..'\ua830'       // Syloti_Nagri
    | '\ua83a'..'\ua840'       // Common_Indic_Number_Forms
    | '\ua878'..'\ua880'       // Phags-pa
    | '\ua8c6'..'\ua8ce'       // Saurashtra
    | '\ua8da'..'\ua8e0'       // Saurashtra
    | '\ua8fe'..'\ua900'       // Devanagari_Extended
    | '\ua954'..'\ua95f'       // Rejang
    | '\ua97d'..'\ua980'       // Hangul_Jamo_Extended-A
    | '\ua9ce'                 // Javanese
    | '\ua9da'..'\ua9de'       // Javanese
    | '\ua9ff'                 // (Absent from Blocks.txt)
    | '\uaa37'..'\uaa40'       // Cham
    | '\uaa4e'..'\uaa50'       // Cham
    | '\uaa5a'..'\uaa5c'       // Cham
    | '\uaac3'..'\uaadb'       // Tai_Viet
    | '\uaaf7'..'\uab01'       // Meetei_Mayek_Extensions
    | '\uab07'..'\uab09'       // Ethiopic_Extended-A
    | '\uab0f'..'\uab11'       // Ethiopic_Extended-A
    | '\uab17'..'\uab20'       // Ethiopic_Extended-A
    | '\uab27'                 // Ethiopic_Extended-A
    | '\uab2f'                 // (Absent from Blocks.txt)
    | '\uab66'..'\uab70'       // Latin_Extended-E
    | '\uabee'..'\uabf0'       // Meetei_Mayek
    | '\uabfa'..'\uac00'       // Meetei_Mayek
    | '\uac01'..'\ud7a3'       // Hangul_Syllables
    | '\ud7a4'..'\ud7b0'       // Hangul_Syllables
    | '\ud7c7'..'\ud7cb'       // Hangul_Jamo_Extended-B
    | '\ud7fc'..'\ud800'       // Hangul_Jamo_Extended-B
    | '\ud801'..'\udb7f'       // High_Surrogates
    | '\udb81'..'\udbff'       // High_Private_Use_Surrogates
    | '\udc01'..'\udfff'       // Low_Surrogates
    | '\ue001'..'\uf8ff'       // Private_Use_Area
    | '\ufa6e'..'\ufa70'       // CJK_Compatibility_Ideographs
    | '\ufada'..'\ufb00'       // CJK_Compatibility_Ideographs
    | '\ufb07'..'\ufb13'       // Alphabetic_Presentation_Forms
    | '\ufb18'..'\ufb1d'       // Alphabetic_Presentation_Forms
    | '\ufb37'                 // Alphabetic_Presentation_Forms
    | '\ufb3d'                 // Alphabetic_Presentation_Forms
    | '\ufb3f'                 // Alphabetic_Presentation_Forms
    | '\ufb42'                 // Alphabetic_Presentation_Forms
    | '\ufb45'                 // Alphabetic_Presentation_Forms
    | '\ufbc2'..'\ufbd3'       // Arabic_Presentation_Forms-A
    | '\ufd40'..'\ufd50'       // Arabic_Presentation_Forms-A
    | '\ufd90'..'\ufd92'       // Arabic_Presentation_Forms-A
    | '\ufdc8'..'\ufdd0'       // Arabic_Presentation_Forms-A
    | '\ufdfe'..'\ufe00'       // Arabic_Presentation_Forms-A
    | '\ufe1a'..'\ufe20'       // Vertical_Forms
    | '\ufe53'                 // Small_Form_Variants
    | '\ufe67'                 // Small_Form_Variants
    | '\ufe6c'..'\ufe70'       // Small_Form_Variants
    | '\ufe75'                 // Arabic_Presentation_Forms-B
    | '\ufefd'..'\ufeff'       // Arabic_Presentation_Forms-B
    | '\uff00'                 // Halfwidth_and_Fullwidth_Forms
    | '\uffbf'..'\uffc2'       // Halfwidth_and_Fullwidth_Forms
    | '\uffc8'..'\uffca'       // Halfwidth_and_Fullwidth_Forms
    | '\uffd0'..'\uffd2'       // Halfwidth_and_Fullwidth_Forms
    | '\uffd8'..'\uffda'       // Halfwidth_and_Fullwidth_Forms
    | '\uffdd'..'\uffe0'       // Halfwidth_and_Fullwidth_Forms
    | '\uffe7'                 // Halfwidth_and_Fullwidth_Forms
    | '\uffef'                 // (Absent from Blocks.txt)
;

CLASSIFY_Cc:
      '\u0000'..'\u0019'       // Basic_Latin
    | '\u007f'..'\u00a0'       // (Absent from Blocks.txt)
;

CLASSIFY_Cf:
      '\u00ad'                 // Latin-1_Supplement
    | '\u0600'..'\u0606'       // Arabic
    | '\u061c'                 // Arabic
    | '\u06dd'                 // Arabic
    | '\u070f'                 // Syriac
    | '\u08e2'                 // Arabic_Extended-A
    | '\u180e'                 // Mongolian
    | '\u200b'..'\u2010'       // General_Punctuation
    | '\u202a'..'\u202f'       // General_Punctuation
    | '\u2060'..'\u2065'       // General_Punctuation
    | '\u2066'..'\u2070'       // General_Punctuation
    | '\ufeff'                 // (Absent from Blocks.txt)
    | '\ufff9'..'\ufffc'       // Specials
;

CLASSIFY_Cn:
      '\u2065'                 // General_Punctuation
    | '\u23ff'                 // (Absent from Blocks.txt)
    | '\u2427'..'\u2440'       // Control_Pictures
    | '\u244b'..'\u2460'       // Optical_Character_Recognition
    | '\u2b74'..'\u2b76'       // Miscellaneous_Symbols_and_Arrows
    | '\u2b96'..'\u2b98'       // Miscellaneous_Symbols_and_Arrows
    | '\u2bba'..'\u2bbd'       // Miscellaneous_Symbols_and_Arrows
    | '\u2bc9'                 // Miscellaneous_Symbols_and_Arrows
    | '\u2bd2'..'\u2bec'       // Miscellaneous_Symbols_and_Arrows
    | '\u2bf0'..'\u2c00'       // Miscellaneous_Symbols_and_Arrows
    | '\u2e45'..'\u2e80'       // Supplemental_Punctuation
    | '\ufdd0'..'\ufdf0'       // Arabic_Presentation_Forms-A
    | '\ufff0'..'\ufff9'       // Specials
;

CLASSIFY_Co:
      '\ue000'                 // Private_Use_Area
    | '\uf8ff'                 // (Absent from Blocks.txt)
;

CLASSIFY_Cs:
      '\ud800'                 // High_Surrogates
    | '\udb7f'..'\udb81'       // (Absent from Blocks.txt)
    | '\udbff'..'\udc01'       // (Absent from Blocks.txt)
    | '\udfff'                 // (Absent from Blocks.txt)
;

CLASSIFY_Ll:
      '\u0061'..'\u007a'       // Basic_Latin
    | '\u00b5'                 // Latin-1_Supplement
    | '\u00df'..'\u00f7'       // Latin-1_Supplement
    | '\u00f8'..'\u0100'       // Latin-1_Supplement
    | '\u0101'                 // Latin_Extended-A
    | '\u0103'                 // Latin_Extended-A
    | '\u0105'                 // Latin_Extended-A
    | '\u0107'                 // Latin_Extended-A
    | '\u0109'                 // Latin_Extended-A
    | '\u010b'                 // Latin_Extended-A
    | '\u010d'                 // Latin_Extended-A
    | '\u010f'                 // Latin_Extended-A
    | '\u0111'                 // Latin_Extended-A
    | '\u0113'                 // Latin_Extended-A
    | '\u0115'                 // Latin_Extended-A
    | '\u0117'                 // Latin_Extended-A
    | '\u0119'                 // Latin_Extended-A
    | '\u011b'                 // Latin_Extended-A
    | '\u011d'                 // Latin_Extended-A
    | '\u011f'                 // Latin_Extended-A
    | '\u0121'                 // Latin_Extended-A
    | '\u0123'                 // Latin_Extended-A
    | '\u0125'                 // Latin_Extended-A
    | '\u0127'                 // Latin_Extended-A
    | '\u0129'                 // Latin_Extended-A
    | '\u012b'                 // Latin_Extended-A
    | '\u012d'                 // Latin_Extended-A
    | '\u012f'                 // Latin_Extended-A
    | '\u0131'                 // Latin_Extended-A
    | '\u0133'                 // Latin_Extended-A
    | '\u0135'                 // Latin_Extended-A
    | '\u0137'..'\u0139'       // Latin_Extended-A
    | '\u013a'                 // Latin_Extended-A
    | '\u013c'                 // Latin_Extended-A
    | '\u013e'                 // Latin_Extended-A
    | '\u0140'                 // Latin_Extended-A
    | '\u0142'                 // Latin_Extended-A
    | '\u0144'                 // Latin_Extended-A
    | '\u0146'                 // Latin_Extended-A
    | '\u0148'..'\u014a'       // Latin_Extended-A
    | '\u014b'                 // Latin_Extended-A
    | '\u014d'                 // Latin_Extended-A
    | '\u014f'                 // Latin_Extended-A
    | '\u0151'                 // Latin_Extended-A
    | '\u0153'                 // Latin_Extended-A
    | '\u0155'                 // Latin_Extended-A
    | '\u0157'                 // Latin_Extended-A
    | '\u0159'                 // Latin_Extended-A
    | '\u015b'                 // Latin_Extended-A
    | '\u015d'                 // Latin_Extended-A
    | '\u015f'                 // Latin_Extended-A
    | '\u0161'                 // Latin_Extended-A
    | '\u0163'                 // Latin_Extended-A
    | '\u0165'                 // Latin_Extended-A
    | '\u0167'                 // Latin_Extended-A
    | '\u0169'                 // Latin_Extended-A
    | '\u016b'                 // Latin_Extended-A
    | '\u016d'                 // Latin_Extended-A
    | '\u016f'                 // Latin_Extended-A
    | '\u0171'                 // Latin_Extended-A
    | '\u0173'                 // Latin_Extended-A
    | '\u0175'                 // Latin_Extended-A
    | '\u0177'                 // Latin_Extended-A
    | '\u017a'                 // Latin_Extended-A
    | '\u017c'                 // Latin_Extended-A
    | '\u017e'..'\u0181'       // Latin_Extended-A
    | '\u0183'                 // Latin_Extended-B
    | '\u0185'                 // Latin_Extended-B
    | '\u0188'                 // Latin_Extended-B
    | '\u018c'..'\u018e'       // Latin_Extended-B
    | '\u0192'                 // Latin_Extended-B
    | '\u0195'                 // Latin_Extended-B
    | '\u0199'..'\u019c'       // Latin_Extended-B
    | '\u019e'                 // Latin_Extended-B
    | '\u01a1'                 // Latin_Extended-B
    | '\u01a3'                 // Latin_Extended-B
    | '\u01a5'                 // Latin_Extended-B
    | '\u01a8'                 // Latin_Extended-B
    | '\u01aa'..'\u01ac'       // Latin_Extended-B
    | '\u01ad'                 // Latin_Extended-B
    | '\u01b0'                 // Latin_Extended-B
    | '\u01b4'                 // Latin_Extended-B
    | '\u01b6'                 // Latin_Extended-B
    | '\u01b9'..'\u01bb'       // Latin_Extended-B
    | '\u01bd'..'\u01c0'       // Latin_Extended-B
    | '\u01c6'                 // Latin_Extended-B
    | '\u01c9'                 // Latin_Extended-B
    | '\u01cc'                 // Latin_Extended-B
    | '\u01ce'                 // Latin_Extended-B
    | '\u01d0'                 // Latin_Extended-B
    | '\u01d2'                 // Latin_Extended-B
    | '\u01d4'                 // Latin_Extended-B
    | '\u01d6'                 // Latin_Extended-B
    | '\u01d8'                 // Latin_Extended-B
    | '\u01da'                 // Latin_Extended-B
    | '\u01dc'..'\u01de'       // Latin_Extended-B
    | '\u01df'                 // Latin_Extended-B
    | '\u01e1'                 // Latin_Extended-B
    | '\u01e3'                 // Latin_Extended-B
    | '\u01e5'                 // Latin_Extended-B
    | '\u01e7'                 // Latin_Extended-B
    | '\u01e9'                 // Latin_Extended-B
    | '\u01eb'                 // Latin_Extended-B
    | '\u01ed'                 // Latin_Extended-B
    | '\u01ef'..'\u01f1'       // Latin_Extended-B
    | '\u01f3'                 // Latin_Extended-B
    | '\u01f5'                 // Latin_Extended-B
    | '\u01f9'                 // Latin_Extended-B
    | '\u01fb'                 // Latin_Extended-B
    | '\u01fd'                 // Latin_Extended-B
    | '\u01ff'                 // Latin_Extended-B
    | '\u0201'                 // Latin_Extended-B
    | '\u0203'                 // Latin_Extended-B
    | '\u0205'                 // Latin_Extended-B
    | '\u0207'                 // Latin_Extended-B
    | '\u0209'                 // Latin_Extended-B
    | '\u020b'                 // Latin_Extended-B
    | '\u020d'                 // Latin_Extended-B
    | '\u020f'                 // Latin_Extended-B
    | '\u0211'                 // Latin_Extended-B
    | '\u0213'                 // Latin_Extended-B
    | '\u0215'                 // Latin_Extended-B
    | '\u0217'                 // Latin_Extended-B
    | '\u0219'                 // Latin_Extended-B
    | '\u021b'                 // Latin_Extended-B
    | '\u021d'                 // Latin_Extended-B
    | '\u021f'                 // Latin_Extended-B
    | '\u0221'                 // Latin_Extended-B
    | '\u0223'                 // Latin_Extended-B
    | '\u0225'                 // Latin_Extended-B
    | '\u0227'                 // Latin_Extended-B
    | '\u0229'                 // Latin_Extended-B
    | '\u022b'                 // Latin_Extended-B
    | '\u022d'                 // Latin_Extended-B
    | '\u022f'                 // Latin_Extended-B
    | '\u0231'                 // Latin_Extended-B
    | '\u0233'..'\u023a'       // Latin_Extended-B
    | '\u023c'                 // Latin_Extended-B
    | '\u023f'..'\u0241'       // Latin_Extended-B
    | '\u0242'                 // Latin_Extended-B
    | '\u0247'                 // Latin_Extended-B
    | '\u0249'                 // Latin_Extended-B
    | '\u024b'                 // Latin_Extended-B
    | '\u024d'                 // Latin_Extended-B
    | '\u024f'..'\u0294'       // (Absent from Blocks.txt)
    | '\u0295'..'\u02b0'       // IPA_Extensions
    | '\u0371'                 // Greek_and_Coptic
    | '\u0373'                 // Greek_and_Coptic
    | '\u0377'                 // Greek_and_Coptic
    | '\u037b'..'\u037e'       // Greek_and_Coptic
    | '\u0390'                 // Greek_and_Coptic
    | '\u03ac'..'\u03cf'       // Greek_and_Coptic
    | '\u03d0'..'\u03d2'       // Greek_and_Coptic
    | '\u03d5'..'\u03d8'       // Greek_and_Coptic
    | '\u03d9'                 // Greek_and_Coptic
    | '\u03db'                 // Greek_and_Coptic
    | '\u03dd'                 // Greek_and_Coptic
    | '\u03df'                 // Greek_and_Coptic
    | '\u03e1'                 // Greek_and_Coptic
    | '\u03e3'                 // Greek_and_Coptic
    | '\u03e5'                 // Greek_and_Coptic
    | '\u03e7'                 // Greek_and_Coptic
    | '\u03e9'                 // Greek_and_Coptic
    | '\u03eb'                 // Greek_and_Coptic
    | '\u03ed'                 // Greek_and_Coptic
    | '\u03ef'..'\u03f4'       // Greek_and_Coptic
    | '\u03f5'                 // Greek_and_Coptic
    | '\u03f8'                 // Greek_and_Coptic
    | '\u03fb'..'\u03fd'       // Greek_and_Coptic
    | '\u0430'..'\u0460'       // Cyrillic
    | '\u0461'                 // Cyrillic
    | '\u0463'                 // Cyrillic
    | '\u0465'                 // Cyrillic
    | '\u0467'                 // Cyrillic
    | '\u0469'                 // Cyrillic
    | '\u046b'                 // Cyrillic
    | '\u046d'                 // Cyrillic
    | '\u046f'                 // Cyrillic
    | '\u0471'                 // Cyrillic
    | '\u0473'                 // Cyrillic
    | '\u0475'                 // Cyrillic
    | '\u0477'                 // Cyrillic
    | '\u0479'                 // Cyrillic
    | '\u047b'                 // Cyrillic
    | '\u047d'                 // Cyrillic
    | '\u047f'                 // Cyrillic
    | '\u0481'                 // Cyrillic
    | '\u048b'                 // Cyrillic
    | '\u048d'                 // Cyrillic
    | '\u048f'                 // Cyrillic
    | '\u0491'                 // Cyrillic
    | '\u0493'                 // Cyrillic
    | '\u0495'                 // Cyrillic
    | '\u0497'                 // Cyrillic
    | '\u0499'                 // Cyrillic
    | '\u049b'                 // Cyrillic
    | '\u049d'                 // Cyrillic
    | '\u049f'                 // Cyrillic
    | '\u04a1'                 // Cyrillic
    | '\u04a3'                 // Cyrillic
    | '\u04a5'                 // Cyrillic
    | '\u04a7'                 // Cyrillic
    | '\u04a9'                 // Cyrillic
    | '\u04ab'                 // Cyrillic
    | '\u04ad'                 // Cyrillic
    | '\u04af'                 // Cyrillic
    | '\u04b1'                 // Cyrillic
    | '\u04b3'                 // Cyrillic
    | '\u04b5'                 // Cyrillic
    | '\u04b7'                 // Cyrillic
    | '\u04b9'                 // Cyrillic
    | '\u04bb'                 // Cyrillic
    | '\u04bd'                 // Cyrillic
    | '\u04bf'                 // Cyrillic
    | '\u04c2'                 // Cyrillic
    | '\u04c4'                 // Cyrillic
    | '\u04c6'                 // Cyrillic
    | '\u04c8'                 // Cyrillic
    | '\u04ca'                 // Cyrillic
    | '\u04cc'                 // Cyrillic
    | '\u04ce'..'\u04d0'       // Cyrillic
    | '\u04d1'                 // Cyrillic
    | '\u04d3'                 // Cyrillic
    | '\u04d5'                 // Cyrillic
    | '\u04d7'                 // Cyrillic
    | '\u04d9'                 // Cyrillic
    | '\u04db'                 // Cyrillic
    | '\u04dd'                 // Cyrillic
    | '\u04df'                 // Cyrillic
    | '\u04e1'                 // Cyrillic
    | '\u04e3'                 // Cyrillic
    | '\u04e5'                 // Cyrillic
    | '\u04e7'                 // Cyrillic
    | '\u04e9'                 // Cyrillic
    | '\u04eb'                 // Cyrillic
    | '\u04ed'                 // Cyrillic
    | '\u04ef'                 // Cyrillic
    | '\u04f1'                 // Cyrillic
    | '\u04f3'                 // Cyrillic
    | '\u04f5'                 // Cyrillic
    | '\u04f7'                 // Cyrillic
    | '\u04f9'                 // Cyrillic
    | '\u04fb'                 // Cyrillic
    | '\u04fd'                 // Cyrillic
    | '\u04ff'                 // (Absent from Blocks.txt)
    | '\u0501'                 // Cyrillic_Supplement
    | '\u0503'                 // Cyrillic_Supplement
    | '\u0505'                 // Cyrillic_Supplement
    | '\u0507'                 // Cyrillic_Supplement
    | '\u0509'                 // Cyrillic_Supplement
    | '\u050b'                 // Cyrillic_Supplement
    | '\u050d'                 // Cyrillic_Supplement
    | '\u050f'                 // Cyrillic_Supplement
    | '\u0511'                 // Cyrillic_Supplement
    | '\u0513'                 // Cyrillic_Supplement
    | '\u0515'                 // Cyrillic_Supplement
    | '\u0517'                 // Cyrillic_Supplement
    | '\u0519'                 // Cyrillic_Supplement
    | '\u051b'                 // Cyrillic_Supplement
    | '\u051d'                 // Cyrillic_Supplement
    | '\u051f'                 // Cyrillic_Supplement
    | '\u0521'                 // Cyrillic_Supplement
    | '\u0523'                 // Cyrillic_Supplement
    | '\u0525'                 // Cyrillic_Supplement
    | '\u0527'                 // Cyrillic_Supplement
    | '\u0529'                 // Cyrillic_Supplement
    | '\u052b'                 // Cyrillic_Supplement
    | '\u052d'                 // Cyrillic_Supplement
    | '\u052f'                 // (Absent from Blocks.txt)
    | '\u0561'..'\u0588'       // Armenian
    | '\u13f8'..'\u13fe'       // Cherokee
    | '\u1c80'..'\u1c89'       // Cyrillic_Extended-C
    | '\u1d00'..'\u1d2c'       // Phonetic_Extensions
    | '\u1d6b'..'\u1d78'       // Phonetic_Extensions
    | '\u1d79'..'\u1d9b'       // Phonetic_Extensions
    | '\u1e01'                 // Latin_Extended_Additional
    | '\u1e03'                 // Latin_Extended_Additional
    | '\u1e05'                 // Latin_Extended_Additional
    | '\u1e07'                 // Latin_Extended_Additional
    | '\u1e09'                 // Latin_Extended_Additional
    | '\u1e0b'                 // Latin_Extended_Additional
    | '\u1e0d'                 // Latin_Extended_Additional
    | '\u1e0f'                 // Latin_Extended_Additional
    | '\u1e11'                 // Latin_Extended_Additional
    | '\u1e13'                 // Latin_Extended_Additional
    | '\u1e15'                 // Latin_Extended_Additional
    | '\u1e17'                 // Latin_Extended_Additional
    | '\u1e19'                 // Latin_Extended_Additional
    | '\u1e1b'                 // Latin_Extended_Additional
    | '\u1e1d'                 // Latin_Extended_Additional
    | '\u1e1f'                 // Latin_Extended_Additional
    | '\u1e21'                 // Latin_Extended_Additional
    | '\u1e23'                 // Latin_Extended_Additional
    | '\u1e25'                 // Latin_Extended_Additional
    | '\u1e27'                 // Latin_Extended_Additional
    | '\u1e29'                 // Latin_Extended_Additional
    | '\u1e2b'                 // Latin_Extended_Additional
    | '\u1e2d'                 // Latin_Extended_Additional
    | '\u1e2f'                 // Latin_Extended_Additional
    | '\u1e31'                 // Latin_Extended_Additional
    | '\u1e33'                 // Latin_Extended_Additional
    | '\u1e35'                 // Latin_Extended_Additional
    | '\u1e37'                 // Latin_Extended_Additional
    | '\u1e39'                 // Latin_Extended_Additional
    | '\u1e3b'                 // Latin_Extended_Additional
    | '\u1e3d'                 // Latin_Extended_Additional
    | '\u1e3f'                 // Latin_Extended_Additional
    | '\u1e41'                 // Latin_Extended_Additional
    | '\u1e43'                 // Latin_Extended_Additional
    | '\u1e45'                 // Latin_Extended_Additional
    | '\u1e47'                 // Latin_Extended_Additional
    | '\u1e49'                 // Latin_Extended_Additional
    | '\u1e4b'                 // Latin_Extended_Additional
    | '\u1e4d'                 // Latin_Extended_Additional
    | '\u1e4f'                 // Latin_Extended_Additional
    | '\u1e51'                 // Latin_Extended_Additional
    | '\u1e53'                 // Latin_Extended_Additional
    | '\u1e55'                 // Latin_Extended_Additional
    | '\u1e57'                 // Latin_Extended_Additional
    | '\u1e59'                 // Latin_Extended_Additional
    | '\u1e5b'                 // Latin_Extended_Additional
    | '\u1e5d'                 // Latin_Extended_Additional
    | '\u1e5f'                 // Latin_Extended_Additional
    | '\u1e61'                 // Latin_Extended_Additional
    | '\u1e63'                 // Latin_Extended_Additional
    | '\u1e65'                 // Latin_Extended_Additional
    | '\u1e67'                 // Latin_Extended_Additional
    | '\u1e69'                 // Latin_Extended_Additional
    | '\u1e6b'                 // Latin_Extended_Additional
    | '\u1e6d'                 // Latin_Extended_Additional
    | '\u1e6f'                 // Latin_Extended_Additional
    | '\u1e71'                 // Latin_Extended_Additional
    | '\u1e73'                 // Latin_Extended_Additional
    | '\u1e75'                 // Latin_Extended_Additional
    | '\u1e77'                 // Latin_Extended_Additional
    | '\u1e79'                 // Latin_Extended_Additional
    | '\u1e7b'                 // Latin_Extended_Additional
    | '\u1e7d'                 // Latin_Extended_Additional
    | '\u1e7f'                 // Latin_Extended_Additional
    | '\u1e81'                 // Latin_Extended_Additional
    | '\u1e83'                 // Latin_Extended_Additional
    | '\u1e85'                 // Latin_Extended_Additional
    | '\u1e87'                 // Latin_Extended_Additional
    | '\u1e89'                 // Latin_Extended_Additional
    | '\u1e8b'                 // Latin_Extended_Additional
    | '\u1e8d'                 // Latin_Extended_Additional
    | '\u1e8f'                 // Latin_Extended_Additional
    | '\u1e91'                 // Latin_Extended_Additional
    | '\u1e93'                 // Latin_Extended_Additional
    | '\u1e95'..'\u1e9e'       // Latin_Extended_Additional
    | '\u1e9f'                 // Latin_Extended_Additional
    | '\u1ea1'                 // Latin_Extended_Additional
    | '\u1ea3'                 // Latin_Extended_Additional
    | '\u1ea5'                 // Latin_Extended_Additional
    | '\u1ea7'                 // Latin_Extended_Additional
    | '\u1ea9'                 // Latin_Extended_Additional
    | '\u1eab'                 // Latin_Extended_Additional
    | '\u1ead'                 // Latin_Extended_Additional
    | '\u1eaf'                 // Latin_Extended_Additional
    | '\u1eb1'                 // Latin_Extended_Additional
    | '\u1eb3'                 // Latin_Extended_Additional
    | '\u1eb5'                 // Latin_Extended_Additional
    | '\u1eb7'                 // Latin_Extended_Additional
    | '\u1eb9'                 // Latin_Extended_Additional
    | '\u1ebb'                 // Latin_Extended_Additional
    | '\u1ebd'                 // Latin_Extended_Additional
    | '\u1ebf'                 // Latin_Extended_Additional
    | '\u1ec1'                 // Latin_Extended_Additional
    | '\u1ec3'                 // Latin_Extended_Additional
    | '\u1ec5'                 // Latin_Extended_Additional
    | '\u1ec7'                 // Latin_Extended_Additional
    | '\u1ec9'                 // Latin_Extended_Additional
    | '\u1ecb'                 // Latin_Extended_Additional
    | '\u1ecd'                 // Latin_Extended_Additional
    | '\u1ecf'                 // Latin_Extended_Additional
    | '\u1ed1'                 // Latin_Extended_Additional
    | '\u1ed3'                 // Latin_Extended_Additional
    | '\u1ed5'                 // Latin_Extended_Additional
    | '\u1ed7'                 // Latin_Extended_Additional
    | '\u1ed9'                 // Latin_Extended_Additional
    | '\u1edb'                 // Latin_Extended_Additional
    | '\u1edd'                 // Latin_Extended_Additional
    | '\u1edf'                 // Latin_Extended_Additional
    | '\u1ee1'                 // Latin_Extended_Additional
    | '\u1ee3'                 // Latin_Extended_Additional
    | '\u1ee5'                 // Latin_Extended_Additional
    | '\u1ee7'                 // Latin_Extended_Additional
    | '\u1ee9'                 // Latin_Extended_Additional
    | '\u1eeb'                 // Latin_Extended_Additional
    | '\u1eed'                 // Latin_Extended_Additional
    | '\u1eef'                 // Latin_Extended_Additional
    | '\u1ef1'                 // Latin_Extended_Additional
    | '\u1ef3'                 // Latin_Extended_Additional
    | '\u1ef5'                 // Latin_Extended_Additional
    | '\u1ef7'                 // Latin_Extended_Additional
    | '\u1ef9'                 // Latin_Extended_Additional
    | '\u1efb'                 // Latin_Extended_Additional
    | '\u1efd'                 // Latin_Extended_Additional
    | '\u1eff'..'\u1f08'       // (Absent from Blocks.txt)
    | '\u1f10'..'\u1f16'       // Greek_Extended
    | '\u1f20'..'\u1f28'       // Greek_Extended
    | '\u1f30'..'\u1f38'       // Greek_Extended
    | '\u1f40'..'\u1f46'       // Greek_Extended
    | '\u1f50'..'\u1f58'       // Greek_Extended
    | '\u1f60'..'\u1f68'       // Greek_Extended
    | '\u1f70'..'\u1f7e'       // Greek_Extended
    | '\u1f80'..'\u1f88'       // Greek_Extended
    | '\u1f90'..'\u1f98'       // Greek_Extended
    | '\u1fa0'..'\u1fa8'       // Greek_Extended
    | '\u1fb0'..'\u1fb5'       // Greek_Extended
    | '\u1fb6'..'\u1fb8'       // Greek_Extended
    | '\u1fbe'                 // Greek_Extended
    | '\u1fc2'..'\u1fc5'       // Greek_Extended
    | '\u1fc6'..'\u1fc8'       // Greek_Extended
    | '\u1fd0'..'\u1fd4'       // Greek_Extended
    | '\u1fd6'..'\u1fd8'       // Greek_Extended
    | '\u1fe0'..'\u1fe8'       // Greek_Extended
    | '\u1ff2'..'\u1ff5'       // Greek_Extended
    | '\u1ff6'..'\u1ff8'       // Greek_Extended
    | '\u210a'                 // Letterlike_Symbols
    | '\u210e'..'\u2110'       // Letterlike_Symbols
    | '\u2113'                 // Letterlike_Symbols
    | '\u212f'                 // Letterlike_Symbols
    | '\u2134'                 // Letterlike_Symbols
    | '\u2139'                 // Letterlike_Symbols
    | '\u213c'..'\u213e'       // Letterlike_Symbols
    | '\u2146'..'\u214a'       // Letterlike_Symbols
    | '\u214e'                 // Letterlike_Symbols
    | '\u2184'                 // Number_Forms
    | '\u2c30'..'\u2c5f'       // Glagolitic
    | '\u2c61'                 // Latin_Extended-C
    | '\u2c65'..'\u2c67'       // Latin_Extended-C
    | '\u2c68'                 // Latin_Extended-C
    | '\u2c6a'                 // Latin_Extended-C
    | '\u2c6c'                 // Latin_Extended-C
    | '\u2c71'                 // Latin_Extended-C
    | '\u2c73'..'\u2c75'       // Latin_Extended-C
    | '\u2c76'..'\u2c7c'       // Latin_Extended-C
    | '\u2c81'                 // Coptic
    | '\u2c83'                 // Coptic
    | '\u2c85'                 // Coptic
    | '\u2c87'                 // Coptic
    | '\u2c89'                 // Coptic
    | '\u2c8b'                 // Coptic
    | '\u2c8d'                 // Coptic
    | '\u2c8f'                 // Coptic
    | '\u2c91'                 // Coptic
    | '\u2c93'                 // Coptic
    | '\u2c95'                 // Coptic
    | '\u2c97'                 // Coptic
    | '\u2c99'                 // Coptic
    | '\u2c9b'                 // Coptic
    | '\u2c9d'                 // Coptic
    | '\u2c9f'                 // Coptic
    | '\u2ca1'                 // Coptic
    | '\u2ca3'                 // Coptic
    | '\u2ca5'                 // Coptic
    | '\u2ca7'                 // Coptic
    | '\u2ca9'                 // Coptic
    | '\u2cab'                 // Coptic
    | '\u2cad'                 // Coptic
    | '\u2caf'                 // Coptic
    | '\u2cb1'                 // Coptic
    | '\u2cb3'                 // Coptic
    | '\u2cb5'                 // Coptic
    | '\u2cb7'                 // Coptic
    | '\u2cb9'                 // Coptic
    | '\u2cbb'                 // Coptic
    | '\u2cbd'                 // Coptic
    | '\u2cbf'                 // Coptic
    | '\u2cc1'                 // Coptic
    | '\u2cc3'                 // Coptic
    | '\u2cc5'                 // Coptic
    | '\u2cc7'                 // Coptic
    | '\u2cc9'                 // Coptic
    | '\u2ccb'                 // Coptic
    | '\u2ccd'                 // Coptic
    | '\u2ccf'                 // Coptic
    | '\u2cd1'                 // Coptic
    | '\u2cd3'                 // Coptic
    | '\u2cd5'                 // Coptic
    | '\u2cd7'                 // Coptic
    | '\u2cd9'                 // Coptic
    | '\u2cdb'                 // Coptic
    | '\u2cdd'                 // Coptic
    | '\u2cdf'                 // Coptic
    | '\u2ce1'                 // Coptic
    | '\u2ce3'..'\u2ce5'       // Coptic
    | '\u2cec'                 // Coptic
    | '\u2cee'                 // Coptic
    | '\u2cf3'                 // Coptic
    | '\u2d00'..'\u2d26'       // Georgian_Supplement
    | '\u2d27'                 // Georgian_Supplement
    | '\u2d2d'                 // Georgian_Supplement
    | '\ua641'                 // Cyrillic_Extended-B
    | '\ua643'                 // Cyrillic_Extended-B
    | '\ua645'                 // Cyrillic_Extended-B
    | '\ua647'                 // Cyrillic_Extended-B
    | '\ua649'                 // Cyrillic_Extended-B
    | '\ua64b'                 // Cyrillic_Extended-B
    | '\ua64d'                 // Cyrillic_Extended-B
    | '\ua64f'                 // Cyrillic_Extended-B
    | '\ua651'                 // Cyrillic_Extended-B
    | '\ua653'                 // Cyrillic_Extended-B
    | '\ua655'                 // Cyrillic_Extended-B
    | '\ua657'                 // Cyrillic_Extended-B
    | '\ua659'                 // Cyrillic_Extended-B
    | '\ua65b'                 // Cyrillic_Extended-B
    | '\ua65d'                 // Cyrillic_Extended-B
    | '\ua65f'                 // Cyrillic_Extended-B
    | '\ua661'                 // Cyrillic_Extended-B
    | '\ua663'                 // Cyrillic_Extended-B
    | '\ua665'                 // Cyrillic_Extended-B
    | '\ua667'                 // Cyrillic_Extended-B
    | '\ua669'                 // Cyrillic_Extended-B
    | '\ua66b'                 // Cyrillic_Extended-B
    | '\ua66d'                 // Cyrillic_Extended-B
    | '\ua681'                 // Cyrillic_Extended-B
    | '\ua683'                 // Cyrillic_Extended-B
    | '\ua685'                 // Cyrillic_Extended-B
    | '\ua687'                 // Cyrillic_Extended-B
    | '\ua689'                 // Cyrillic_Extended-B
    | '\ua68b'                 // Cyrillic_Extended-B
    | '\ua68d'                 // Cyrillic_Extended-B
    | '\ua68f'                 // Cyrillic_Extended-B
    | '\ua691'                 // Cyrillic_Extended-B
    | '\ua693'                 // Cyrillic_Extended-B
    | '\ua695'                 // Cyrillic_Extended-B
    | '\ua697'                 // Cyrillic_Extended-B
    | '\ua699'                 // Cyrillic_Extended-B
    | '\ua69b'                 // Cyrillic_Extended-B
    | '\ua723'                 // Latin_Extended-D
    | '\ua725'                 // Latin_Extended-D
    | '\ua727'                 // Latin_Extended-D
    | '\ua729'                 // Latin_Extended-D
    | '\ua72b'                 // Latin_Extended-D
    | '\ua72d'                 // Latin_Extended-D
    | '\ua72f'..'\ua732'       // Latin_Extended-D
    | '\ua733'                 // Latin_Extended-D
    | '\ua735'                 // Latin_Extended-D
    | '\ua737'                 // Latin_Extended-D
    | '\ua739'                 // Latin_Extended-D
    | '\ua73b'                 // Latin_Extended-D
    | '\ua73d'                 // Latin_Extended-D
    | '\ua73f'                 // Latin_Extended-D
    | '\ua741'                 // Latin_Extended-D
    | '\ua743'                 // Latin_Extended-D
    | '\ua745'                 // Latin_Extended-D
    | '\ua747'                 // Latin_Extended-D
    | '\ua749'                 // Latin_Extended-D
    | '\ua74b'                 // Latin_Extended-D
    | '\ua74d'                 // Latin_Extended-D
    | '\ua74f'                 // Latin_Extended-D
    | '\ua751'                 // Latin_Extended-D
    | '\ua753'                 // Latin_Extended-D
    | '\ua755'                 // Latin_Extended-D
    | '\ua757'                 // Latin_Extended-D
    | '\ua759'                 // Latin_Extended-D
    | '\ua75b'                 // Latin_Extended-D
    | '\ua75d'                 // Latin_Extended-D
    | '\ua75f'                 // Latin_Extended-D
    | '\ua761'                 // Latin_Extended-D
    | '\ua763'                 // Latin_Extended-D
    | '\ua765'                 // Latin_Extended-D
    | '\ua767'                 // Latin_Extended-D
    | '\ua769'                 // Latin_Extended-D
    | '\ua76b'                 // Latin_Extended-D
    | '\ua76d'                 // Latin_Extended-D
    | '\ua76f'                 // Latin_Extended-D
    | '\ua771'..'\ua779'       // Latin_Extended-D
    | '\ua77a'                 // Latin_Extended-D
    | '\ua77c'                 // Latin_Extended-D
    | '\ua77f'                 // Latin_Extended-D
    | '\ua781'                 // Latin_Extended-D
    | '\ua783'                 // Latin_Extended-D
    | '\ua785'                 // Latin_Extended-D
    | '\ua787'                 // Latin_Extended-D
    | '\ua78c'                 // Latin_Extended-D
    | '\ua78e'                 // Latin_Extended-D
    | '\ua791'                 // Latin_Extended-D
    | '\ua793'..'\ua796'       // Latin_Extended-D
    | '\ua797'                 // Latin_Extended-D
    | '\ua799'                 // Latin_Extended-D
    | '\ua79b'                 // Latin_Extended-D
    | '\ua79d'                 // Latin_Extended-D
    | '\ua79f'                 // Latin_Extended-D
    | '\ua7a1'                 // Latin_Extended-D
    | '\ua7a3'                 // Latin_Extended-D
    | '\ua7a5'                 // Latin_Extended-D
    | '\ua7a7'                 // Latin_Extended-D
    | '\ua7a9'                 // Latin_Extended-D
    | '\ua7b5'                 // Latin_Extended-D
    | '\ua7b7'                 // Latin_Extended-D
    | '\ua7fa'                 // Latin_Extended-D
    | '\uab30'..'\uab5b'       // Latin_Extended-E
    | '\uab60'..'\uab66'       // Latin_Extended-E
    | '\uab70'..'\uabc0'       // Cherokee_Supplement
    | '\ufb00'..'\ufb07'       // Alphabetic_Presentation_Forms
    | '\ufb13'..'\ufb18'       // Alphabetic_Presentation_Forms
    | '\uff41'..'\uff5b'       // Halfwidth_and_Fullwidth_Forms
;

CLASSIFY_Lm:
      '\u02b0'..'\u02c2'       // Spacing_Modifier_Letters
    | '\u02c6'..'\u02d2'       // Spacing_Modifier_Letters
    | '\u02e0'..'\u02e5'       // Spacing_Modifier_Letters
    | '\u02ec'                 // Spacing_Modifier_Letters
    | '\u02ee'                 // Spacing_Modifier_Letters
    | '\u0374'                 // Greek_and_Coptic
    | '\u037a'                 // Greek_and_Coptic
    | '\u0559'                 // Armenian
    | '\u0640'                 // Arabic
    | '\u06e5'..'\u06e7'       // Arabic
    | '\u07f4'..'\u07f6'       // NKo
    | '\u07fa'                 // NKo
    | '\u081a'                 // Samaritan
    | '\u0824'                 // Samaritan
    | '\u0828'                 // Samaritan
    | '\u0971'                 // Devanagari
    | '\u0e46'                 // Thai
    | '\u0ec6'                 // Lao
    | '\u10fc'                 // Georgian
    | '\u17d7'                 // Khmer
    | '\u1843'                 // Mongolian
    | '\u1aa7'                 // Tai_Tham
    | '\u1c78'..'\u1c7e'       // Ol_Chiki
    | '\u1d2c'..'\u1d6b'       // Phonetic_Extensions
    | '\u1d78'                 // Phonetic_Extensions
    | '\u1d9b'..'\u1dc0'       // Phonetic_Extensions_Supplement
    | '\u2071'                 // Superscripts_and_Subscripts
    | '\u207f'                 // Superscripts_and_Subscripts
    | '\u2090'..'\u209d'       // Superscripts_and_Subscripts
    | '\u2c7c'..'\u2c7e'       // Latin_Extended-C
    | '\u2d6f'                 // Tifinagh
    | '\u2e2f'                 // Supplemental_Punctuation
    | '\u3005'                 // CJK_Symbols_and_Punctuation
    | '\u3031'..'\u3036'       // CJK_Symbols_and_Punctuation
    | '\u303b'                 // CJK_Symbols_and_Punctuation
    | '\u309d'..'\u309f'       // Hiragana
    | '\u30fc'..'\u30ff'       // Katakana
    | '\ua015'                 // Yi_Syllables
    | '\ua4f8'..'\ua4fe'       // Lisu
    | '\ua60c'                 // Vai
    | '\ua67f'                 // Cyrillic_Extended-B
    | '\ua69c'..'\ua69e'       // Cyrillic_Extended-B
    | '\ua717'..'\ua720'       // Modifier_Tone_Letters
    | '\ua770'                 // Latin_Extended-D
    | '\ua788'                 // Latin_Extended-D
    | '\ua7f8'..'\ua7fa'       // Latin_Extended-D
    | '\ua9cf'                 // Javanese
    | '\ua9e6'                 // Myanmar_Extended-B
    | '\uaa70'                 // Myanmar_Extended-A
    | '\uaadd'                 // Tai_Viet
    | '\uaaf3'..'\uaaf5'       // Meetei_Mayek_Extensions
    | '\uab5c'..'\uab60'       // Latin_Extended-E
    | '\uff70'                 // Halfwidth_and_Fullwidth_Forms
    | '\uff9e'..'\uffa0'       // Halfwidth_and_Fullwidth_Forms
;

CLASSIFY_Lo:
      '\u00aa'                 // Latin-1_Supplement
    | '\u00ba'                 // Latin-1_Supplement
    | '\u01bb'                 // Latin_Extended-B
    | '\u01c0'..'\u01c4'       // Latin_Extended-B
    | '\u0294'                 // IPA_Extensions
    | '\u05d0'..'\u05eb'       // Hebrew
    | '\u05f0'..'\u05f3'       // Hebrew
    | '\u0620'..'\u0640'       // Arabic
    | '\u0641'..'\u064b'       // Arabic
    | '\u066e'..'\u0670'       // Arabic
    | '\u0671'..'\u06d4'       // Arabic
    | '\u06d5'                 // Arabic
    | '\u06ee'..'\u06f0'       // Arabic
    | '\u06fa'..'\u06fd'       // Arabic
    | '\u06ff'                 // (Absent from Blocks.txt)
    | '\u0710'                 // Syriac
    | '\u0712'..'\u0730'       // Syriac
    | '\u074d'..'\u07a6'       // Syriac
    | '\u07b1'                 // Thaana
    | '\u07ca'..'\u07eb'       // NKo
    | '\u0800'..'\u0816'       // Samaritan
    | '\u0840'..'\u0859'       // Mandaic
    | '\u08a0'..'\u08b5'       // Arabic_Extended-A
    | '\u08b6'..'\u08be'       // Arabic_Extended-A
    | '\u0904'..'\u093a'       // Devanagari
    | '\u093d'                 // Devanagari
    | '\u0950'                 // Devanagari
    | '\u0958'..'\u0962'       // Devanagari
    | '\u0972'..'\u0981'       // Devanagari
    | '\u0985'..'\u098d'       // Bengali
    | '\u098f'..'\u0991'       // Bengali
    | '\u0993'..'\u09a9'       // Bengali
    | '\u09aa'..'\u09b1'       // Bengali
    | '\u09b2'                 // Bengali
    | '\u09b6'..'\u09ba'       // Bengali
    | '\u09bd'                 // Bengali
    | '\u09ce'                 // Bengali
    | '\u09dc'..'\u09de'       // Bengali
    | '\u09df'..'\u09e2'       // Bengali
    | '\u09f0'..'\u09f2'       // Bengali
    | '\u0a05'..'\u0a0b'       // Gurmukhi
    | '\u0a0f'..'\u0a11'       // Gurmukhi
    | '\u0a13'..'\u0a29'       // Gurmukhi
    | '\u0a2a'..'\u0a31'       // Gurmukhi
    | '\u0a32'..'\u0a34'       // Gurmukhi
    | '\u0a35'..'\u0a37'       // Gurmukhi
    | '\u0a38'..'\u0a3a'       // Gurmukhi
    | '\u0a59'..'\u0a5d'       // Gurmukhi
    | '\u0a5e'                 // Gurmukhi
    | '\u0a72'..'\u0a75'       // Gurmukhi
    | '\u0a85'..'\u0a8e'       // Gujarati
    | '\u0a8f'..'\u0a92'       // Gujarati
    | '\u0a93'..'\u0aa9'       // Gujarati
    | '\u0aaa'..'\u0ab1'       // Gujarati
    | '\u0ab2'..'\u0ab4'       // Gujarati
    | '\u0ab5'..'\u0aba'       // Gujarati
    | '\u0abd'                 // Gujarati
    | '\u0ad0'                 // Gujarati
    | '\u0ae0'..'\u0ae2'       // Gujarati
    | '\u0af9'                 // Gujarati
    | '\u0b05'..'\u0b0d'       // Oriya
    | '\u0b0f'..'\u0b11'       // Oriya
    | '\u0b13'..'\u0b29'       // Oriya
    | '\u0b2a'..'\u0b31'       // Oriya
    | '\u0b32'..'\u0b34'       // Oriya
    | '\u0b35'..'\u0b3a'       // Oriya
    | '\u0b3d'                 // Oriya
    | '\u0b5c'..'\u0b5e'       // Oriya
    | '\u0b5f'..'\u0b62'       // Oriya
    | '\u0b71'                 // Oriya
    | '\u0b83'                 // Tamil
    | '\u0b85'..'\u0b8b'       // Tamil
    | '\u0b8e'..'\u0b91'       // Tamil
    | '\u0b92'..'\u0b96'       // Tamil
    | '\u0b99'..'\u0b9b'       // Tamil
    | '\u0b9c'                 // Tamil
    | '\u0b9e'..'\u0ba0'       // Tamil
    | '\u0ba3'..'\u0ba5'       // Tamil
    | '\u0ba8'..'\u0bab'       // Tamil
    | '\u0bae'..'\u0bba'       // Tamil
    | '\u0bd0'                 // Tamil
    | '\u0c05'..'\u0c0d'       // Telugu
    | '\u0c0e'..'\u0c11'       // Telugu
    | '\u0c12'..'\u0c29'       // Telugu
    | '\u0c2a'..'\u0c3a'       // Telugu
    | '\u0c3d'                 // Telugu
    | '\u0c58'..'\u0c5b'       // Telugu
    | '\u0c60'..'\u0c62'       // Telugu
    | '\u0c80'                 // Kannada
    | '\u0c85'..'\u0c8d'       // Kannada
    | '\u0c8e'..'\u0c91'       // Kannada
    | '\u0c92'..'\u0ca9'       // Kannada
    | '\u0caa'..'\u0cb4'       // Kannada
    | '\u0cb5'..'\u0cba'       // Kannada
    | '\u0cbd'                 // Kannada
    | '\u0cde'                 // Kannada
    | '\u0ce0'..'\u0ce2'       // Kannada
    | '\u0cf1'..'\u0cf3'       // Kannada
    | '\u0d05'..'\u0d0d'       // Malayalam
    | '\u0d0e'..'\u0d11'       // Malayalam
    | '\u0d12'..'\u0d3b'       // Malayalam
    | '\u0d3d'                 // Malayalam
    | '\u0d4e'                 // Malayalam
    | '\u0d54'..'\u0d57'       // Malayalam
    | '\u0d5f'..'\u0d62'       // Malayalam
    | '\u0d7a'..'\u0d80'       // Malayalam
    | '\u0d85'..'\u0d97'       // Sinhala
    | '\u0d9a'..'\u0db2'       // Sinhala
    | '\u0db3'..'\u0dbc'       // Sinhala
    | '\u0dbd'                 // Sinhala
    | '\u0dc0'..'\u0dc7'       // Sinhala
    | '\u0e01'..'\u0e31'       // Thai
    | '\u0e32'..'\u0e34'       // Thai
    | '\u0e40'..'\u0e46'       // Thai
    | '\u0e81'..'\u0e83'       // Lao
    | '\u0e84'                 // Lao
    | '\u0e87'..'\u0e89'       // Lao
    | '\u0e8a'                 // Lao
    | '\u0e8d'                 // Lao
    | '\u0e94'..'\u0e98'       // Lao
    | '\u0e99'..'\u0ea0'       // Lao
    | '\u0ea1'..'\u0ea4'       // Lao
    | '\u0ea5'                 // Lao
    | '\u0ea7'                 // Lao
    | '\u0eaa'..'\u0eac'       // Lao
    | '\u0ead'..'\u0eb1'       // Lao
    | '\u0eb2'..'\u0eb4'       // Lao
    | '\u0ebd'                 // Lao
    | '\u0ec0'..'\u0ec5'       // Lao
    | '\u0edc'..'\u0ee0'       // Lao
    | '\u0f00'                 // Tibetan
    | '\u0f40'..'\u0f48'       // Tibetan
    | '\u0f49'..'\u0f6d'       // Tibetan
    | '\u0f88'..'\u0f8d'       // Tibetan
    | '\u1000'..'\u102b'       // Myanmar
    | '\u103f'                 // Myanmar
    | '\u1050'..'\u1056'       // Myanmar
    | '\u105a'..'\u105e'       // Myanmar
    | '\u1061'                 // Myanmar
    | '\u1065'..'\u1067'       // Myanmar
    | '\u106e'..'\u1071'       // Myanmar
    | '\u1075'..'\u1082'       // Myanmar
    | '\u108e'                 // Myanmar
    | '\u10d0'..'\u10fb'       // Georgian
    | '\u10fd'..'\u1249'       // Georgian
    | '\u124a'..'\u124e'       // Ethiopic
    | '\u1250'..'\u1257'       // Ethiopic
    | '\u1258'                 // Ethiopic
    | '\u125a'..'\u125e'       // Ethiopic
    | '\u1260'..'\u1289'       // Ethiopic
    | '\u128a'..'\u128e'       // Ethiopic
    | '\u1290'..'\u12b1'       // Ethiopic
    | '\u12b2'..'\u12b6'       // Ethiopic
    | '\u12b8'..'\u12bf'       // Ethiopic
    | '\u12c0'                 // Ethiopic
    | '\u12c2'..'\u12c6'       // Ethiopic
    | '\u12c8'..'\u12d7'       // Ethiopic
    | '\u12d8'..'\u1311'       // Ethiopic
    | '\u1312'..'\u1316'       // Ethiopic
    | '\u1318'..'\u135b'       // Ethiopic
    | '\u1380'..'\u1390'       // Ethiopic_Supplement
    | '\u1401'..'\u166d'       // Unified_Canadian_Aboriginal_Syllabics
    | '\u166f'..'\u1680'       // Unified_Canadian_Aboriginal_Syllabics
    | '\u1681'..'\u169b'       // Ogham
    | '\u16a0'..'\u16eb'       // Runic
    | '\u16f1'..'\u16f9'       // Runic
    | '\u1700'..'\u170d'       // Tagalog
    | '\u170e'..'\u1712'       // Tagalog
    | '\u1720'..'\u1732'       // Hanunoo
    | '\u1740'..'\u1752'       // Buhid
    | '\u1760'..'\u176d'       // Tagbanwa
    | '\u176e'..'\u1771'       // Tagbanwa
    | '\u1780'..'\u17b4'       // Khmer
    | '\u17dc'                 // Khmer
    | '\u1820'..'\u1843'       // Mongolian
    | '\u1844'..'\u1878'       // Mongolian
    | '\u1880'..'\u1885'       // Mongolian
    | '\u1887'..'\u18a9'       // Mongolian
    | '\u18aa'                 // Mongolian
    | '\u18b0'..'\u18f6'       // Unified_Canadian_Aboriginal_Syllabics_Extended
    | '\u1900'..'\u191f'       // Limbu
    | '\u1950'..'\u196e'       // Tai_Le
    | '\u1970'..'\u1975'       // Tai_Le
    | '\u1980'..'\u19ac'       // New_Tai_Lue
    | '\u19b0'..'\u19ca'       // New_Tai_Lue
    | '\u1a00'..'\u1a17'       // Buginese
    | '\u1a20'..'\u1a55'       // Tai_Tham
    | '\u1b05'..'\u1b34'       // Balinese
    | '\u1b45'..'\u1b4c'       // Balinese
    | '\u1b83'..'\u1ba1'       // Sundanese
    | '\u1bae'..'\u1bb0'       // Sundanese
    | '\u1bba'..'\u1be6'       // Sundanese
    | '\u1c00'..'\u1c24'       // Lepcha
    | '\u1c4d'..'\u1c50'       // Lepcha
    | '\u1c5a'..'\u1c78'       // Ol_Chiki
    | '\u1ce9'..'\u1ced'       // Vedic_Extensions
    | '\u1cee'..'\u1cf2'       // Vedic_Extensions
    | '\u1cf5'..'\u1cf7'       // Vedic_Extensions
    | '\u2135'..'\u2139'       // Letterlike_Symbols
    | '\u2d30'..'\u2d68'       // Tifinagh
    | '\u2d80'..'\u2d97'       // Ethiopic_Extended
    | '\u2da0'..'\u2da7'       // Ethiopic_Extended
    | '\u2da8'..'\u2daf'       // Ethiopic_Extended
    | '\u2db0'..'\u2db7'       // Ethiopic_Extended
    | '\u2db8'..'\u2dbf'       // Ethiopic_Extended
    | '\u2dc0'..'\u2dc7'       // Ethiopic_Extended
    | '\u2dc8'..'\u2dcf'       // Ethiopic_Extended
    | '\u2dd0'..'\u2dd7'       // Ethiopic_Extended
    | '\u2dd8'..'\u2ddf'       // Ethiopic_Extended
    | '\u3006'                 // CJK_Symbols_and_Punctuation
    | '\u303c'                 // CJK_Symbols_and_Punctuation
    | '\u3041'..'\u3097'       // Hiragana
    | '\u309f'                 // (Absent from Blocks.txt)
    | '\u30a1'..'\u30fb'       // Katakana
    | '\u30ff'                 // (Absent from Blocks.txt)
    | '\u3105'..'\u312e'       // Bopomofo
    | '\u3131'..'\u318f'       // Hangul_Compatibility_Jamo
    | '\u31a0'..'\u31bb'       // Bopomofo_Extended
    | '\u31f0'..'\u3200'       // Katakana_Phonetic_Extensions
    | '\u3400'..'\u4db6'       // CJK_Unified_Ideographs_Extension_A
    | '\u4e00'..'\u9fd6'       // CJK_Unified_Ideographs
    | '\ua000'..'\ua015'       // Yi_Syllables
    | '\ua016'..'\ua48d'       // Yi_Syllables
    | '\ua4d0'..'\ua4f8'       // Lisu
    | '\ua500'..'\ua60c'       // Vai
    | '\ua610'..'\ua620'       // Vai
    | '\ua62a'..'\ua62c'       // Vai
    | '\ua66e'                 // Cyrillic_Extended-B
    | '\ua6a0'..'\ua6e6'       // Bamum
    | '\ua78f'                 // Latin_Extended-D
    | '\ua7f7'                 // Latin_Extended-D
    | '\ua7fb'..'\ua802'       // Latin_Extended-D
    | '\ua803'..'\ua806'       // Syloti_Nagri
    | '\ua807'..'\ua80b'       // Syloti_Nagri
    | '\ua80c'..'\ua823'       // Syloti_Nagri
    | '\ua840'..'\ua874'       // Phags-pa
    | '\ua882'..'\ua8b4'       // Saurashtra
    | '\ua8f2'..'\ua8f8'       // Devanagari_Extended
    | '\ua8fb'                 // Devanagari_Extended
    | '\ua8fd'                 // Devanagari_Extended
    | '\ua90a'..'\ua926'       // Kayah_Li
    | '\ua930'..'\ua947'       // Rejang
    | '\ua960'..'\ua97d'       // Hangul_Jamo_Extended-A
    | '\ua984'..'\ua9b3'       // Javanese
    | '\ua9e0'..'\ua9e5'       // Myanmar_Extended-B
    | '\ua9e7'..'\ua9f0'       // Myanmar_Extended-B
    | '\ua9fa'..'\ua9ff'       // Myanmar_Extended-B
    | '\uaa00'..'\uaa29'       // Cham
    | '\uaa40'..'\uaa43'       // Cham
    | '\uaa44'..'\uaa4c'       // Cham
    | '\uaa60'..'\uaa70'       // Myanmar_Extended-A
    | '\uaa71'..'\uaa77'       // Myanmar_Extended-A
    | '\uaa7a'                 // Myanmar_Extended-A
    | '\uaa7e'..'\uaab0'       // Myanmar_Extended-A
    | '\uaab1'                 // Tai_Viet
    | '\uaab5'..'\uaab7'       // Tai_Viet
    | '\uaab9'..'\uaabe'       // Tai_Viet
    | '\uaac0'                 // Tai_Viet
    | '\uaac2'                 // Tai_Viet
    | '\uaadb'..'\uaadd'       // Tai_Viet
    | '\uaae0'..'\uaaeb'       // Meetei_Mayek_Extensions
    | '\uaaf2'                 // Meetei_Mayek_Extensions
    | '\uab01'..'\uab07'       // Ethiopic_Extended-A
    | '\uab09'..'\uab0f'       // Ethiopic_Extended-A
    | '\uab11'..'\uab17'       // Ethiopic_Extended-A
    | '\uab20'..'\uab27'       // Ethiopic_Extended-A
    | '\uab28'..'\uab2f'       // Ethiopic_Extended-A
    | '\uabc0'..'\uabe3'       // Meetei_Mayek
    | '\uac00'                 // Hangul_Syllables
    | '\ud7a3'                 // Hangul_Syllables
    | '\ud7b0'..'\ud7c7'       // Hangul_Jamo_Extended-B
    | '\ud7cb'..'\ud7fc'       // Hangul_Jamo_Extended-B
    | '\uf900'..'\ufa6e'       // CJK_Compatibility_Ideographs
    | '\ufa70'..'\ufada'       // CJK_Compatibility_Ideographs
    | '\ufb1d'                 // Alphabetic_Presentation_Forms
    | '\ufb1f'..'\ufb29'       // Alphabetic_Presentation_Forms
    | '\ufb2a'..'\ufb37'       // Alphabetic_Presentation_Forms
    | '\ufb38'..'\ufb3d'       // Alphabetic_Presentation_Forms
    | '\ufb3e'                 // Alphabetic_Presentation_Forms
    | '\ufb40'..'\ufb42'       // Alphabetic_Presentation_Forms
    | '\ufb43'..'\ufb45'       // Alphabetic_Presentation_Forms
    | '\ufb46'..'\ufbb2'       // Alphabetic_Presentation_Forms
    | '\ufbd3'..'\ufd3e'       // Arabic_Presentation_Forms-A
    | '\ufd50'..'\ufd90'       // Arabic_Presentation_Forms-A
    | '\ufd92'..'\ufdc8'       // Arabic_Presentation_Forms-A
    | '\ufdf0'..'\ufdfc'       // Arabic_Presentation_Forms-A
    | '\ufe70'..'\ufe75'       // Arabic_Presentation_Forms-B
    | '\ufe76'..'\ufefd'       // Arabic_Presentation_Forms-B
    | '\uff66'..'\uff70'       // Halfwidth_and_Fullwidth_Forms
    | '\uff71'..'\uff9e'       // Halfwidth_and_Fullwidth_Forms
    | '\uffa0'..'\uffbf'       // Halfwidth_and_Fullwidth_Forms
    | '\uffc2'..'\uffc8'       // Halfwidth_and_Fullwidth_Forms
    | '\uffca'..'\uffd0'       // Halfwidth_and_Fullwidth_Forms
    | '\uffd2'..'\uffd8'       // Halfwidth_and_Fullwidth_Forms
    | '\uffda'..'\uffdd'       // Halfwidth_and_Fullwidth_Forms
;

CLASSIFY_Lt:
      '\u01c5'                 // Latin_Extended-B
    | '\u01c8'                 // Latin_Extended-B
    | '\u01cb'                 // Latin_Extended-B
    | '\u01f2'                 // Latin_Extended-B
    | '\u1f88'..'\u1f90'       // Greek_Extended
    | '\u1f98'..'\u1fa0'       // Greek_Extended
    | '\u1fa8'..'\u1fb0'       // Greek_Extended
    | '\u1fbc'                 // Greek_Extended
    | '\u1fcc'                 // Greek_Extended
    | '\u1ffc'                 // Greek_Extended
;

CLASSIFY_Lu:
      '\u0041'..'\u005a'       // Basic_Latin
    | '\u00c0'..'\u00d7'       // Latin-1_Supplement
    | '\u00d8'..'\u00df'       // Latin-1_Supplement
    | '\u0100'                 // Latin_Extended-A
    | '\u0102'                 // Latin_Extended-A
    | '\u0104'                 // Latin_Extended-A
    | '\u0106'                 // Latin_Extended-A
    | '\u0108'                 // Latin_Extended-A
    | '\u010a'                 // Latin_Extended-A
    | '\u010c'                 // Latin_Extended-A
    | '\u010e'                 // Latin_Extended-A
    | '\u0110'                 // Latin_Extended-A
    | '\u0112'                 // Latin_Extended-A
    | '\u0114'                 // Latin_Extended-A
    | '\u0116'                 // Latin_Extended-A
    | '\u0118'                 // Latin_Extended-A
    | '\u011a'                 // Latin_Extended-A
    | '\u011c'                 // Latin_Extended-A
    | '\u011e'                 // Latin_Extended-A
    | '\u0120'                 // Latin_Extended-A
    | '\u0122'                 // Latin_Extended-A
    | '\u0124'                 // Latin_Extended-A
    | '\u0126'                 // Latin_Extended-A
    | '\u0128'                 // Latin_Extended-A
    | '\u012a'                 // Latin_Extended-A
    | '\u012c'                 // Latin_Extended-A
    | '\u012e'                 // Latin_Extended-A
    | '\u0130'                 // Latin_Extended-A
    | '\u0132'                 // Latin_Extended-A
    | '\u0134'                 // Latin_Extended-A
    | '\u0136'                 // Latin_Extended-A
    | '\u0139'                 // Latin_Extended-A
    | '\u013b'                 // Latin_Extended-A
    | '\u013d'                 // Latin_Extended-A
    | '\u013f'                 // Latin_Extended-A
    | '\u0141'                 // Latin_Extended-A
    | '\u0143'                 // Latin_Extended-A
    | '\u0145'                 // Latin_Extended-A
    | '\u0147'                 // Latin_Extended-A
    | '\u014a'                 // Latin_Extended-A
    | '\u014c'                 // Latin_Extended-A
    | '\u014e'                 // Latin_Extended-A
    | '\u0150'                 // Latin_Extended-A
    | '\u0152'                 // Latin_Extended-A
    | '\u0154'                 // Latin_Extended-A
    | '\u0156'                 // Latin_Extended-A
    | '\u0158'                 // Latin_Extended-A
    | '\u015a'                 // Latin_Extended-A
    | '\u015c'                 // Latin_Extended-A
    | '\u015e'                 // Latin_Extended-A
    | '\u0160'                 // Latin_Extended-A
    | '\u0162'                 // Latin_Extended-A
    | '\u0164'                 // Latin_Extended-A
    | '\u0166'                 // Latin_Extended-A
    | '\u0168'                 // Latin_Extended-A
    | '\u016a'                 // Latin_Extended-A
    | '\u016c'                 // Latin_Extended-A
    | '\u016e'                 // Latin_Extended-A
    | '\u0170'                 // Latin_Extended-A
    | '\u0172'                 // Latin_Extended-A
    | '\u0174'                 // Latin_Extended-A
    | '\u0176'                 // Latin_Extended-A
    | '\u0178'..'\u017a'       // Latin_Extended-A
    | '\u017b'                 // Latin_Extended-A
    | '\u017d'                 // Latin_Extended-A
    | '\u0181'..'\u0183'       // Latin_Extended-B
    | '\u0184'                 // Latin_Extended-B
    | '\u0186'..'\u0188'       // Latin_Extended-B
    | '\u0189'..'\u018c'       // Latin_Extended-B
    | '\u018e'..'\u0192'       // Latin_Extended-B
    | '\u0193'..'\u0195'       // Latin_Extended-B
    | '\u0196'..'\u0199'       // Latin_Extended-B
    | '\u019c'..'\u019e'       // Latin_Extended-B
    | '\u019f'..'\u01a1'       // Latin_Extended-B
    | '\u01a2'                 // Latin_Extended-B
    | '\u01a4'                 // Latin_Extended-B
    | '\u01a6'..'\u01a8'       // Latin_Extended-B
    | '\u01a9'                 // Latin_Extended-B
    | '\u01ac'                 // Latin_Extended-B
    | '\u01ae'..'\u01b0'       // Latin_Extended-B
    | '\u01b1'..'\u01b4'       // Latin_Extended-B
    | '\u01b5'                 // Latin_Extended-B
    | '\u01b7'..'\u01b9'       // Latin_Extended-B
    | '\u01bc'                 // Latin_Extended-B
    | '\u01c4'                 // Latin_Extended-B
    | '\u01c7'                 // Latin_Extended-B
    | '\u01ca'                 // Latin_Extended-B
    | '\u01cd'                 // Latin_Extended-B
    | '\u01cf'                 // Latin_Extended-B
    | '\u01d1'                 // Latin_Extended-B
    | '\u01d3'                 // Latin_Extended-B
    | '\u01d5'                 // Latin_Extended-B
    | '\u01d7'                 // Latin_Extended-B
    | '\u01d9'                 // Latin_Extended-B
    | '\u01db'                 // Latin_Extended-B
    | '\u01de'                 // Latin_Extended-B
    | '\u01e0'                 // Latin_Extended-B
    | '\u01e2'                 // Latin_Extended-B
    | '\u01e4'                 // Latin_Extended-B
    | '\u01e6'                 // Latin_Extended-B
    | '\u01e8'                 // Latin_Extended-B
    | '\u01ea'                 // Latin_Extended-B
    | '\u01ec'                 // Latin_Extended-B
    | '\u01ee'                 // Latin_Extended-B
    | '\u01f1'                 // Latin_Extended-B
    | '\u01f4'                 // Latin_Extended-B
    | '\u01f6'..'\u01f9'       // Latin_Extended-B
    | '\u01fa'                 // Latin_Extended-B
    | '\u01fc'                 // Latin_Extended-B
    | '\u01fe'                 // Latin_Extended-B
    | '\u0200'                 // Latin_Extended-B
    | '\u0202'                 // Latin_Extended-B
    | '\u0204'                 // Latin_Extended-B
    | '\u0206'                 // Latin_Extended-B
    | '\u0208'                 // Latin_Extended-B
    | '\u020a'                 // Latin_Extended-B
    | '\u020c'                 // Latin_Extended-B
    | '\u020e'                 // Latin_Extended-B
    | '\u0210'                 // Latin_Extended-B
    | '\u0212'                 // Latin_Extended-B
    | '\u0214'                 // Latin_Extended-B
    | '\u0216'                 // Latin_Extended-B
    | '\u0218'                 // Latin_Extended-B
    | '\u021a'                 // Latin_Extended-B
    | '\u021c'                 // Latin_Extended-B
    | '\u021e'                 // Latin_Extended-B
    | '\u0220'                 // Latin_Extended-B
    | '\u0222'                 // Latin_Extended-B
    | '\u0224'                 // Latin_Extended-B
    | '\u0226'                 // Latin_Extended-B
    | '\u0228'                 // Latin_Extended-B
    | '\u022a'                 // Latin_Extended-B
    | '\u022c'                 // Latin_Extended-B
    | '\u022e'                 // Latin_Extended-B
    | '\u0230'                 // Latin_Extended-B
    | '\u0232'                 // Latin_Extended-B
    | '\u023a'..'\u023c'       // Latin_Extended-B
    | '\u023d'..'\u023f'       // Latin_Extended-B
    | '\u0241'                 // Latin_Extended-B
    | '\u0243'..'\u0247'       // Latin_Extended-B
    | '\u0248'                 // Latin_Extended-B
    | '\u024a'                 // Latin_Extended-B
    | '\u024c'                 // Latin_Extended-B
    | '\u024e'                 // Latin_Extended-B
    | '\u0370'                 // Greek_and_Coptic
    | '\u0372'                 // Greek_and_Coptic
    | '\u0376'                 // Greek_and_Coptic
    | '\u037f'                 // Greek_and_Coptic
    | '\u0386'                 // Greek_and_Coptic
    | '\u0388'..'\u038b'       // Greek_and_Coptic
    | '\u038c'                 // Greek_and_Coptic
    | '\u038e'..'\u0390'       // Greek_and_Coptic
    | '\u0391'..'\u03a2'       // Greek_and_Coptic
    | '\u03a3'..'\u03ac'       // Greek_and_Coptic
    | '\u03cf'                 // Greek_and_Coptic
    | '\u03d2'..'\u03d5'       // Greek_and_Coptic
    | '\u03d8'                 // Greek_and_Coptic
    | '\u03da'                 // Greek_and_Coptic
    | '\u03dc'                 // Greek_and_Coptic
    | '\u03de'                 // Greek_and_Coptic
    | '\u03e0'                 // Greek_and_Coptic
    | '\u03e2'                 // Greek_and_Coptic
    | '\u03e4'                 // Greek_and_Coptic
    | '\u03e6'                 // Greek_and_Coptic
    | '\u03e8'                 // Greek_and_Coptic
    | '\u03ea'                 // Greek_and_Coptic
    | '\u03ec'                 // Greek_and_Coptic
    | '\u03ee'                 // Greek_and_Coptic
    | '\u03f4'                 // Greek_and_Coptic
    | '\u03f7'                 // Greek_and_Coptic
    | '\u03f9'..'\u03fb'       // Greek_and_Coptic
    | '\u03fd'..'\u0430'       // Greek_and_Coptic
    | '\u0460'                 // Cyrillic
    | '\u0462'                 // Cyrillic
    | '\u0464'                 // Cyrillic
    | '\u0466'                 // Cyrillic
    | '\u0468'                 // Cyrillic
    | '\u046a'                 // Cyrillic
    | '\u046c'                 // Cyrillic
    | '\u046e'                 // Cyrillic
    | '\u0470'                 // Cyrillic
    | '\u0472'                 // Cyrillic
    | '\u0474'                 // Cyrillic
    | '\u0476'                 // Cyrillic
    | '\u0478'                 // Cyrillic
    | '\u047a'                 // Cyrillic
    | '\u047c'                 // Cyrillic
    | '\u047e'                 // Cyrillic
    | '\u0480'                 // Cyrillic
    | '\u048a'                 // Cyrillic
    | '\u048c'                 // Cyrillic
    | '\u048e'                 // Cyrillic
    | '\u0490'                 // Cyrillic
    | '\u0492'                 // Cyrillic
    | '\u0494'                 // Cyrillic
    | '\u0496'                 // Cyrillic
    | '\u0498'                 // Cyrillic
    | '\u049a'                 // Cyrillic
    | '\u049c'                 // Cyrillic
    | '\u049e'                 // Cyrillic
    | '\u04a0'                 // Cyrillic
    | '\u04a2'                 // Cyrillic
    | '\u04a4'                 // Cyrillic
    | '\u04a6'                 // Cyrillic
    | '\u04a8'                 // Cyrillic
    | '\u04aa'                 // Cyrillic
    | '\u04ac'                 // Cyrillic
    | '\u04ae'                 // Cyrillic
    | '\u04b0'                 // Cyrillic
    | '\u04b2'                 // Cyrillic
    | '\u04b4'                 // Cyrillic
    | '\u04b6'                 // Cyrillic
    | '\u04b8'                 // Cyrillic
    | '\u04ba'                 // Cyrillic
    | '\u04bc'                 // Cyrillic
    | '\u04be'                 // Cyrillic
    | '\u04c0'..'\u04c2'       // Cyrillic
    | '\u04c3'                 // Cyrillic
    | '\u04c5'                 // Cyrillic
    | '\u04c7'                 // Cyrillic
    | '\u04c9'                 // Cyrillic
    | '\u04cb'                 // Cyrillic
    | '\u04cd'                 // Cyrillic
    | '\u04d0'                 // Cyrillic
    | '\u04d2'                 // Cyrillic
    | '\u04d4'                 // Cyrillic
    | '\u04d6'                 // Cyrillic
    | '\u04d8'                 // Cyrillic
    | '\u04da'                 // Cyrillic
    | '\u04dc'                 // Cyrillic
    | '\u04de'                 // Cyrillic
    | '\u04e0'                 // Cyrillic
    | '\u04e2'                 // Cyrillic
    | '\u04e4'                 // Cyrillic
    | '\u04e6'                 // Cyrillic
    | '\u04e8'                 // Cyrillic
    | '\u04ea'                 // Cyrillic
    | '\u04ec'                 // Cyrillic
    | '\u04ee'                 // Cyrillic
    | '\u04f0'                 // Cyrillic
    | '\u04f2'                 // Cyrillic
    | '\u04f4'                 // Cyrillic
    | '\u04f6'                 // Cyrillic
    | '\u04f8'                 // Cyrillic
    | '\u04fa'                 // Cyrillic
    | '\u04fc'                 // Cyrillic
    | '\u04fe'                 // Cyrillic
    | '\u0500'                 // Cyrillic_Supplement
    | '\u0502'                 // Cyrillic_Supplement
    | '\u0504'                 // Cyrillic_Supplement
    | '\u0506'                 // Cyrillic_Supplement
    | '\u0508'                 // Cyrillic_Supplement
    | '\u050a'                 // Cyrillic_Supplement
    | '\u050c'                 // Cyrillic_Supplement
    | '\u050e'                 // Cyrillic_Supplement
    | '\u0510'                 // Cyrillic_Supplement
    | '\u0512'                 // Cyrillic_Supplement
    | '\u0514'                 // Cyrillic_Supplement
    | '\u0516'                 // Cyrillic_Supplement
    | '\u0518'                 // Cyrillic_Supplement
    | '\u051a'                 // Cyrillic_Supplement
    | '\u051c'                 // Cyrillic_Supplement
    | '\u051e'                 // Cyrillic_Supplement
    | '\u0520'                 // Cyrillic_Supplement
    | '\u0522'                 // Cyrillic_Supplement
    | '\u0524'                 // Cyrillic_Supplement
    | '\u0526'                 // Cyrillic_Supplement
    | '\u0528'                 // Cyrillic_Supplement
    | '\u052a'                 // Cyrillic_Supplement
    | '\u052c'                 // Cyrillic_Supplement
    | '\u052e'                 // Cyrillic_Supplement
    | '\u0531'..'\u0557'       // Armenian
    | '\u10a0'..'\u10c6'       // Georgian
    | '\u10c7'                 // Georgian
    | '\u10cd'                 // Georgian
    | '\u13a0'..'\u13f6'       // Cherokee
    | '\u1e00'                 // Latin_Extended_Additional
    | '\u1e02'                 // Latin_Extended_Additional
    | '\u1e04'                 // Latin_Extended_Additional
    | '\u1e06'                 // Latin_Extended_Additional
    | '\u1e08'                 // Latin_Extended_Additional
    | '\u1e0a'                 // Latin_Extended_Additional
    | '\u1e0c'                 // Latin_Extended_Additional
    | '\u1e0e'                 // Latin_Extended_Additional
    | '\u1e10'                 // Latin_Extended_Additional
    | '\u1e12'                 // Latin_Extended_Additional
    | '\u1e14'                 // Latin_Extended_Additional
    | '\u1e16'                 // Latin_Extended_Additional
    | '\u1e18'                 // Latin_Extended_Additional
    | '\u1e1a'                 // Latin_Extended_Additional
    | '\u1e1c'                 // Latin_Extended_Additional
    | '\u1e1e'                 // Latin_Extended_Additional
    | '\u1e20'                 // Latin_Extended_Additional
    | '\u1e22'                 // Latin_Extended_Additional
    | '\u1e24'                 // Latin_Extended_Additional
    | '\u1e26'                 // Latin_Extended_Additional
    | '\u1e28'                 // Latin_Extended_Additional
    | '\u1e2a'                 // Latin_Extended_Additional
    | '\u1e2c'                 // Latin_Extended_Additional
    | '\u1e2e'                 // Latin_Extended_Additional
    | '\u1e30'                 // Latin_Extended_Additional
    | '\u1e32'                 // Latin_Extended_Additional
    | '\u1e34'                 // Latin_Extended_Additional
    | '\u1e36'                 // Latin_Extended_Additional
    | '\u1e38'                 // Latin_Extended_Additional
    | '\u1e3a'                 // Latin_Extended_Additional
    | '\u1e3c'                 // Latin_Extended_Additional
    | '\u1e3e'                 // Latin_Extended_Additional
    | '\u1e40'                 // Latin_Extended_Additional
    | '\u1e42'                 // Latin_Extended_Additional
    | '\u1e44'                 // Latin_Extended_Additional
    | '\u1e46'                 // Latin_Extended_Additional
    | '\u1e48'                 // Latin_Extended_Additional
    | '\u1e4a'                 // Latin_Extended_Additional
    | '\u1e4c'                 // Latin_Extended_Additional
    | '\u1e4e'                 // Latin_Extended_Additional
    | '\u1e50'                 // Latin_Extended_Additional
    | '\u1e52'                 // Latin_Extended_Additional
    | '\u1e54'                 // Latin_Extended_Additional
    | '\u1e56'                 // Latin_Extended_Additional
    | '\u1e58'                 // Latin_Extended_Additional
    | '\u1e5a'                 // Latin_Extended_Additional
    | '\u1e5c'                 // Latin_Extended_Additional
    | '\u1e5e'                 // Latin_Extended_Additional
    | '\u1e60'                 // Latin_Extended_Additional
    | '\u1e62'                 // Latin_Extended_Additional
    | '\u1e64'                 // Latin_Extended_Additional
    | '\u1e66'                 // Latin_Extended_Additional
    | '\u1e68'                 // Latin_Extended_Additional
    | '\u1e6a'                 // Latin_Extended_Additional
    | '\u1e6c'                 // Latin_Extended_Additional
    | '\u1e6e'                 // Latin_Extended_Additional
    | '\u1e70'                 // Latin_Extended_Additional
    | '\u1e72'                 // Latin_Extended_Additional
    | '\u1e74'                 // Latin_Extended_Additional
    | '\u1e76'                 // Latin_Extended_Additional
    | '\u1e78'                 // Latin_Extended_Additional
    | '\u1e7a'                 // Latin_Extended_Additional
    | '\u1e7c'                 // Latin_Extended_Additional
    | '\u1e7e'                 // Latin_Extended_Additional
    | '\u1e80'                 // Latin_Extended_Additional
    | '\u1e82'                 // Latin_Extended_Additional
    | '\u1e84'                 // Latin_Extended_Additional
    | '\u1e86'                 // Latin_Extended_Additional
    | '\u1e88'                 // Latin_Extended_Additional
    | '\u1e8a'                 // Latin_Extended_Additional
    | '\u1e8c'                 // Latin_Extended_Additional
    | '\u1e8e'                 // Latin_Extended_Additional
    | '\u1e90'                 // Latin_Extended_Additional
    | '\u1e92'                 // Latin_Extended_Additional
    | '\u1e94'                 // Latin_Extended_Additional
    | '\u1e9e'                 // Latin_Extended_Additional
    | '\u1ea0'                 // Latin_Extended_Additional
    | '\u1ea2'                 // Latin_Extended_Additional
    | '\u1ea4'                 // Latin_Extended_Additional
    | '\u1ea6'                 // Latin_Extended_Additional
    | '\u1ea8'                 // Latin_Extended_Additional
    | '\u1eaa'                 // Latin_Extended_Additional
    | '\u1eac'                 // Latin_Extended_Additional
    | '\u1eae'                 // Latin_Extended_Additional
    | '\u1eb0'                 // Latin_Extended_Additional
    | '\u1eb2'                 // Latin_Extended_Additional
    | '\u1eb4'                 // Latin_Extended_Additional
    | '\u1eb6'                 // Latin_Extended_Additional
    | '\u1eb8'                 // Latin_Extended_Additional
    | '\u1eba'                 // Latin_Extended_Additional
    | '\u1ebc'                 // Latin_Extended_Additional
    | '\u1ebe'                 // Latin_Extended_Additional
    | '\u1ec0'                 // Latin_Extended_Additional
    | '\u1ec2'                 // Latin_Extended_Additional
    | '\u1ec4'                 // Latin_Extended_Additional
    | '\u1ec6'                 // Latin_Extended_Additional
    | '\u1ec8'                 // Latin_Extended_Additional
    | '\u1eca'                 // Latin_Extended_Additional
    | '\u1ecc'                 // Latin_Extended_Additional
    | '\u1ece'                 // Latin_Extended_Additional
    | '\u1ed0'                 // Latin_Extended_Additional
    | '\u1ed2'                 // Latin_Extended_Additional
    | '\u1ed4'                 // Latin_Extended_Additional
    | '\u1ed6'                 // Latin_Extended_Additional
    | '\u1ed8'                 // Latin_Extended_Additional
    | '\u1eda'                 // Latin_Extended_Additional
    | '\u1edc'                 // Latin_Extended_Additional
    | '\u1ede'                 // Latin_Extended_Additional
    | '\u1ee0'                 // Latin_Extended_Additional
    | '\u1ee2'                 // Latin_Extended_Additional
    | '\u1ee4'                 // Latin_Extended_Additional
    | '\u1ee6'                 // Latin_Extended_Additional
    | '\u1ee8'                 // Latin_Extended_Additional
    | '\u1eea'                 // Latin_Extended_Additional
    | '\u1eec'                 // Latin_Extended_Additional
    | '\u1eee'                 // Latin_Extended_Additional
    | '\u1ef0'                 // Latin_Extended_Additional
    | '\u1ef2'                 // Latin_Extended_Additional
    | '\u1ef4'                 // Latin_Extended_Additional
    | '\u1ef6'                 // Latin_Extended_Additional
    | '\u1ef8'                 // Latin_Extended_Additional
    | '\u1efa'                 // Latin_Extended_Additional
    | '\u1efc'                 // Latin_Extended_Additional
    | '\u1efe'                 // Latin_Extended_Additional
    | '\u1f08'..'\u1f10'       // Greek_Extended
    | '\u1f18'..'\u1f1e'       // Greek_Extended
    | '\u1f28'..'\u1f30'       // Greek_Extended
    | '\u1f38'..'\u1f40'       // Greek_Extended
    | '\u1f48'..'\u1f4e'       // Greek_Extended
    | '\u1f59'                 // Greek_Extended
    | '\u1f5b'                 // Greek_Extended
    | '\u1f5d'                 // Greek_Extended
    | '\u1f5f'                 // Greek_Extended
    | '\u1f68'..'\u1f70'       // Greek_Extended
    | '\u1fb8'..'\u1fbc'       // Greek_Extended
    | '\u1fc8'..'\u1fcc'       // Greek_Extended
    | '\u1fd8'..'\u1fdc'       // Greek_Extended
    | '\u1fe8'..'\u1fed'       // Greek_Extended
    | '\u1ff8'..'\u1ffc'       // Greek_Extended
    | '\u2102'                 // Letterlike_Symbols
    | '\u2107'                 // Letterlike_Symbols
    | '\u210b'..'\u210e'       // Letterlike_Symbols
    | '\u2110'..'\u2113'       // Letterlike_Symbols
    | '\u2115'                 // Letterlike_Symbols
    | '\u2119'..'\u211e'       // Letterlike_Symbols
    | '\u2124'                 // Letterlike_Symbols
    | '\u2126'                 // Letterlike_Symbols
    | '\u2128'                 // Letterlike_Symbols
    | '\u212a'..'\u212e'       // Letterlike_Symbols
    | '\u2130'..'\u2134'       // Letterlike_Symbols
    | '\u213e'..'\u2140'       // Letterlike_Symbols
    | '\u2145'                 // Letterlike_Symbols
    | '\u2183'                 // Number_Forms
    | '\u2c00'..'\u2c2f'       // Glagolitic
    | '\u2c60'                 // Latin_Extended-C
    | '\u2c62'..'\u2c65'       // Latin_Extended-C
    | '\u2c67'                 // Latin_Extended-C
    | '\u2c69'                 // Latin_Extended-C
    | '\u2c6b'                 // Latin_Extended-C
    | '\u2c6d'..'\u2c71'       // Latin_Extended-C
    | '\u2c72'                 // Latin_Extended-C
    | '\u2c75'                 // Latin_Extended-C
    | '\u2c7e'..'\u2c81'       // Latin_Extended-C
    | '\u2c82'                 // Coptic
    | '\u2c84'                 // Coptic
    | '\u2c86'                 // Coptic
    | '\u2c88'                 // Coptic
    | '\u2c8a'                 // Coptic
    | '\u2c8c'                 // Coptic
    | '\u2c8e'                 // Coptic
    | '\u2c90'                 // Coptic
    | '\u2c92'                 // Coptic
    | '\u2c94'                 // Coptic
    | '\u2c96'                 // Coptic
    | '\u2c98'                 // Coptic
    | '\u2c9a'                 // Coptic
    | '\u2c9c'                 // Coptic
    | '\u2c9e'                 // Coptic
    | '\u2ca0'                 // Coptic
    | '\u2ca2'                 // Coptic
    | '\u2ca4'                 // Coptic
    | '\u2ca6'                 // Coptic
    | '\u2ca8'                 // Coptic
    | '\u2caa'                 // Coptic
    | '\u2cac'                 // Coptic
    | '\u2cae'                 // Coptic
    | '\u2cb0'                 // Coptic
    | '\u2cb2'                 // Coptic
    | '\u2cb4'                 // Coptic
    | '\u2cb6'                 // Coptic
    | '\u2cb8'                 // Coptic
    | '\u2cba'                 // Coptic
    | '\u2cbc'                 // Coptic
    | '\u2cbe'                 // Coptic
    | '\u2cc0'                 // Coptic
    | '\u2cc2'                 // Coptic
    | '\u2cc4'                 // Coptic
    | '\u2cc6'                 // Coptic
    | '\u2cc8'                 // Coptic
    | '\u2cca'                 // Coptic
    | '\u2ccc'                 // Coptic
    | '\u2cce'                 // Coptic
    | '\u2cd0'                 // Coptic
    | '\u2cd2'                 // Coptic
    | '\u2cd4'                 // Coptic
    | '\u2cd6'                 // Coptic
    | '\u2cd8'                 // Coptic
    | '\u2cda'                 // Coptic
    | '\u2cdc'                 // Coptic
    | '\u2cde'                 // Coptic
    | '\u2ce0'                 // Coptic
    | '\u2ce2'                 // Coptic
    | '\u2ceb'                 // Coptic
    | '\u2ced'                 // Coptic
    | '\u2cf2'                 // Coptic
    | '\ua640'                 // Cyrillic_Extended-B
    | '\ua642'                 // Cyrillic_Extended-B
    | '\ua644'                 // Cyrillic_Extended-B
    | '\ua646'                 // Cyrillic_Extended-B
    | '\ua648'                 // Cyrillic_Extended-B
    | '\ua64a'                 // Cyrillic_Extended-B
    | '\ua64c'                 // Cyrillic_Extended-B
    | '\ua64e'                 // Cyrillic_Extended-B
    | '\ua650'                 // Cyrillic_Extended-B
    | '\ua652'                 // Cyrillic_Extended-B
    | '\ua654'                 // Cyrillic_Extended-B
    | '\ua656'                 // Cyrillic_Extended-B
    | '\ua658'                 // Cyrillic_Extended-B
    | '\ua65a'                 // Cyrillic_Extended-B
    | '\ua65c'                 // Cyrillic_Extended-B
    | '\ua65e'                 // Cyrillic_Extended-B
    | '\ua660'                 // Cyrillic_Extended-B
    | '\ua662'                 // Cyrillic_Extended-B
    | '\ua664'                 // Cyrillic_Extended-B
    | '\ua666'                 // Cyrillic_Extended-B
    | '\ua668'                 // Cyrillic_Extended-B
    | '\ua66a'                 // Cyrillic_Extended-B
    | '\ua66c'                 // Cyrillic_Extended-B
    | '\ua680'                 // Cyrillic_Extended-B
    | '\ua682'                 // Cyrillic_Extended-B
    | '\ua684'                 // Cyrillic_Extended-B
    | '\ua686'                 // Cyrillic_Extended-B
    | '\ua688'                 // Cyrillic_Extended-B
    | '\ua68a'                 // Cyrillic_Extended-B
    | '\ua68c'                 // Cyrillic_Extended-B
    | '\ua68e'                 // Cyrillic_Extended-B
    | '\ua690'                 // Cyrillic_Extended-B
    | '\ua692'                 // Cyrillic_Extended-B
    | '\ua694'                 // Cyrillic_Extended-B
    | '\ua696'                 // Cyrillic_Extended-B
    | '\ua698'                 // Cyrillic_Extended-B
    | '\ua69a'                 // Cyrillic_Extended-B
    | '\ua722'                 // Latin_Extended-D
    | '\ua724'                 // Latin_Extended-D
    | '\ua726'                 // Latin_Extended-D
    | '\ua728'                 // Latin_Extended-D
    | '\ua72a'                 // Latin_Extended-D
    | '\ua72c'                 // Latin_Extended-D
    | '\ua72e'                 // Latin_Extended-D
    | '\ua732'                 // Latin_Extended-D
    | '\ua734'                 // Latin_Extended-D
    | '\ua736'                 // Latin_Extended-D
    | '\ua738'                 // Latin_Extended-D
    | '\ua73a'                 // Latin_Extended-D
    | '\ua73c'                 // Latin_Extended-D
    | '\ua73e'                 // Latin_Extended-D
    | '\ua740'                 // Latin_Extended-D
    | '\ua742'                 // Latin_Extended-D
    | '\ua744'                 // Latin_Extended-D
    | '\ua746'                 // Latin_Extended-D
    | '\ua748'                 // Latin_Extended-D
    | '\ua74a'                 // Latin_Extended-D
    | '\ua74c'                 // Latin_Extended-D
    | '\ua74e'                 // Latin_Extended-D
    | '\ua750'                 // Latin_Extended-D
    | '\ua752'                 // Latin_Extended-D
    | '\ua754'                 // Latin_Extended-D
    | '\ua756'                 // Latin_Extended-D
    | '\ua758'                 // Latin_Extended-D
    | '\ua75a'                 // Latin_Extended-D
    | '\ua75c'                 // Latin_Extended-D
    | '\ua75e'                 // Latin_Extended-D
    | '\ua760'                 // Latin_Extended-D
    | '\ua762'                 // Latin_Extended-D
    | '\ua764'                 // Latin_Extended-D
    | '\ua766'                 // Latin_Extended-D
    | '\ua768'                 // Latin_Extended-D
    | '\ua76a'                 // Latin_Extended-D
    | '\ua76c'                 // Latin_Extended-D
    | '\ua76e'                 // Latin_Extended-D
    | '\ua779'                 // Latin_Extended-D
    | '\ua77b'                 // Latin_Extended-D
    | '\ua77d'..'\ua77f'       // Latin_Extended-D
    | '\ua780'                 // Latin_Extended-D
    | '\ua782'                 // Latin_Extended-D
    | '\ua784'                 // Latin_Extended-D
    | '\ua786'                 // Latin_Extended-D
    | '\ua78b'                 // Latin_Extended-D
    | '\ua78d'                 // Latin_Extended-D
    | '\ua790'                 // Latin_Extended-D
    | '\ua792'                 // Latin_Extended-D
    | '\ua796'                 // Latin_Extended-D
    | '\ua798'                 // Latin_Extended-D
    | '\ua79a'                 // Latin_Extended-D
    | '\ua79c'                 // Latin_Extended-D
    | '\ua79e'                 // Latin_Extended-D
    | '\ua7a0'                 // Latin_Extended-D
    | '\ua7a2'                 // Latin_Extended-D
    | '\ua7a4'                 // Latin_Extended-D
    | '\ua7a6'                 // Latin_Extended-D
    | '\ua7a8'                 // Latin_Extended-D
    | '\ua7aa'..'\ua7af'       // Latin_Extended-D
    | '\ua7b0'..'\ua7b5'       // Latin_Extended-D
    | '\ua7b6'                 // Latin_Extended-D
    | '\uff21'..'\uff3b'       // Halfwidth_and_Fullwidth_Forms
;

CLASSIFY_Mc:
      '\u0903'                 // Devanagari
    | '\u093b'                 // Devanagari
    | '\u093e'..'\u0941'       // Devanagari
    | '\u0949'..'\u094d'       // Devanagari
    | '\u094e'..'\u0950'       // Devanagari
    | '\u0982'..'\u0984'       // Bengali
    | '\u09be'..'\u09c1'       // Bengali
    | '\u09c7'..'\u09c9'       // Bengali
    | '\u09cb'..'\u09cd'       // Bengali
    | '\u09d7'                 // Bengali
    | '\u0a03'                 // Gurmukhi
    | '\u0a3e'..'\u0a41'       // Gurmukhi
    | '\u0a83'                 // Gujarati
    | '\u0abe'..'\u0ac1'       // Gujarati
    | '\u0ac9'                 // Gujarati
    | '\u0acb'..'\u0acd'       // Gujarati
    | '\u0b02'..'\u0b04'       // Oriya
    | '\u0b3e'                 // Oriya
    | '\u0b40'                 // Oriya
    | '\u0b47'..'\u0b49'       // Oriya
    | '\u0b4b'..'\u0b4d'       // Oriya
    | '\u0b57'                 // Oriya
    | '\u0bbe'..'\u0bc0'       // Tamil
    | '\u0bc1'..'\u0bc3'       // Tamil
    | '\u0bc6'..'\u0bc9'       // Tamil
    | '\u0bca'..'\u0bcd'       // Tamil
    | '\u0bd7'                 // Tamil
    | '\u0c01'..'\u0c04'       // Telugu
    | '\u0c41'..'\u0c45'       // Telugu
    | '\u0c82'..'\u0c84'       // Kannada
    | '\u0cbe'                 // Kannada
    | '\u0cc0'..'\u0cc5'       // Kannada
    | '\u0cc7'..'\u0cc9'       // Kannada
    | '\u0cca'..'\u0ccc'       // Kannada
    | '\u0cd5'..'\u0cd7'       // Kannada
    | '\u0d02'..'\u0d04'       // Malayalam
    | '\u0d3e'..'\u0d41'       // Malayalam
    | '\u0d46'..'\u0d49'       // Malayalam
    | '\u0d4a'..'\u0d4d'       // Malayalam
    | '\u0d57'                 // Malayalam
    | '\u0d82'..'\u0d84'       // Sinhala
    | '\u0dcf'..'\u0dd2'       // Sinhala
    | '\u0dd8'..'\u0de0'       // Sinhala
    | '\u0df2'..'\u0df4'       // Sinhala
    | '\u0f3e'..'\u0f40'       // Tibetan
    | '\u0f7f'                 // Tibetan
    | '\u102b'..'\u102d'       // Myanmar
    | '\u1031'                 // Myanmar
    | '\u1038'                 // Myanmar
    | '\u103b'..'\u103d'       // Myanmar
    | '\u1056'..'\u1058'       // Myanmar
    | '\u1062'..'\u1065'       // Myanmar
    | '\u1067'..'\u106e'       // Myanmar
    | '\u1083'..'\u1085'       // Myanmar
    | '\u1087'..'\u108d'       // Myanmar
    | '\u108f'                 // Myanmar
    | '\u109a'..'\u109d'       // Myanmar
    | '\u17b6'                 // Khmer
    | '\u17be'..'\u17c6'       // Khmer
    | '\u17c7'..'\u17c9'       // Khmer
    | '\u1923'..'\u1927'       // Limbu
    | '\u1929'..'\u192c'       // Limbu
    | '\u1930'..'\u1932'       // Limbu
    | '\u1933'..'\u1939'       // Limbu
    | '\u1a19'..'\u1a1b'       // Buginese
    | '\u1a55'                 // Tai_Tham
    | '\u1a57'                 // Tai_Tham
    | '\u1a61'                 // Tai_Tham
    | '\u1a63'..'\u1a65'       // Tai_Tham
    | '\u1a6d'..'\u1a73'       // Tai_Tham
    | '\u1b04'                 // Balinese
    | '\u1b35'                 // Balinese
    | '\u1b3b'                 // Balinese
    | '\u1b3d'..'\u1b42'       // Balinese
    | '\u1b43'..'\u1b45'       // Balinese
    | '\u1b82'                 // Sundanese
    | '\u1ba1'                 // Sundanese
    | '\u1ba6'..'\u1ba8'       // Sundanese
    | '\u1baa'                 // Sundanese
    | '\u1be7'                 // Batak
    | '\u1bea'..'\u1bed'       // Batak
    | '\u1bee'                 // Batak
    | '\u1bf2'..'\u1bf4'       // Batak
    | '\u1c24'..'\u1c2c'       // Lepcha
    | '\u1c34'..'\u1c36'       // Lepcha
    | '\u1ce1'                 // Vedic_Extensions
    | '\u1cf2'..'\u1cf4'       // Vedic_Extensions
    | '\u302e'..'\u3030'       // CJK_Symbols_and_Punctuation
    | '\ua823'..'\ua825'       // Syloti_Nagri
    | '\ua827'                 // Syloti_Nagri
    | '\ua880'..'\ua882'       // Saurashtra
    | '\ua8b4'..'\ua8c4'       // Saurashtra
    | '\ua952'..'\ua954'       // Rejang
    | '\ua983'                 // Javanese
    | '\ua9b4'..'\ua9b6'       // Javanese
    | '\ua9ba'..'\ua9bc'       // Javanese
    | '\ua9bd'..'\ua9c1'       // Javanese
    | '\uaa2f'..'\uaa31'       // Cham
    | '\uaa33'..'\uaa35'       // Cham
    | '\uaa4d'                 // Cham
    | '\uaa7b'                 // Myanmar_Extended-A
    | '\uaa7d'                 // Myanmar_Extended-A
    | '\uaaeb'                 // Meetei_Mayek_Extensions
    | '\uaaee'..'\uaaf0'       // Meetei_Mayek_Extensions
    | '\uaaf5'                 // Meetei_Mayek_Extensions
    | '\uabe3'..'\uabe5'       // Meetei_Mayek
    | '\uabe6'..'\uabe8'       // Meetei_Mayek
    | '\uabe9'..'\uabeb'       // Meetei_Mayek
    | '\uabec'                 // Meetei_Mayek
;

CLASSIFY_Me:
      '\u0488'..'\u048a'       // Cyrillic
    | '\u1abe'                 // Combining_Diacritical_Marks_Extended
    | '\u20dd'..'\u20e1'       // Combining_Diacritical_Marks_for_Symbols
    | '\u20e2'..'\u20e5'       // Combining_Diacritical_Marks_for_Symbols
    | '\ua670'..'\ua673'       // Cyrillic_Extended-B
;

CLASSIFY_Mn:
      '\u0300'..'\u0370'       // Combining_Diacritical_Marks
    | '\u0483'..'\u0488'       // Cyrillic
    | '\u0591'..'\u05be'       // Hebrew
    | '\u05bf'                 // Hebrew
    | '\u05c1'..'\u05c3'       // Hebrew
    | '\u05c4'..'\u05c6'       // Hebrew
    | '\u05c7'                 // Hebrew
    | '\u0610'..'\u061b'       // Arabic
    | '\u064b'..'\u0660'       // Arabic
    | '\u0670'                 // Arabic
    | '\u06d6'..'\u06dd'       // Arabic
    | '\u06df'..'\u06e5'       // Arabic
    | '\u06e7'..'\u06e9'       // Arabic
    | '\u06ea'..'\u06ee'       // Arabic
    | '\u0711'                 // Syriac
    | '\u0730'..'\u074b'       // Syriac
    | '\u07a6'..'\u07b1'       // Thaana
    | '\u07eb'..'\u07f4'       // NKo
    | '\u0816'..'\u081a'       // Samaritan
    | '\u081b'..'\u0824'       // Samaritan
    | '\u0825'..'\u0828'       // Samaritan
    | '\u0829'..'\u082e'       // Samaritan
    | '\u0859'..'\u085c'       // Mandaic
    | '\u08d4'..'\u08e2'       // Arabic_Extended-A
    | '\u08e3'..'\u0903'       // Arabic_Extended-A
    | '\u093a'                 // Devanagari
    | '\u093c'                 // Devanagari
    | '\u0941'..'\u0949'       // Devanagari
    | '\u094d'                 // Devanagari
    | '\u0951'..'\u0958'       // Devanagari
    | '\u0962'..'\u0964'       // Devanagari
    | '\u0981'                 // Bengali
    | '\u09bc'                 // Bengali
    | '\u09c1'..'\u09c5'       // Bengali
    | '\u09cd'                 // Bengali
    | '\u09e2'..'\u09e4'       // Bengali
    | '\u0a01'..'\u0a03'       // Gurmukhi
    | '\u0a3c'                 // Gurmukhi
    | '\u0a41'..'\u0a43'       // Gurmukhi
    | '\u0a47'..'\u0a49'       // Gurmukhi
    | '\u0a4b'..'\u0a4e'       // Gurmukhi
    | '\u0a51'                 // Gurmukhi
    | '\u0a70'..'\u0a72'       // Gurmukhi
    | '\u0a75'                 // Gurmukhi
    | '\u0a81'..'\u0a83'       // Gujarati
    | '\u0abc'                 // Gujarati
    | '\u0ac1'..'\u0ac6'       // Gujarati
    | '\u0ac7'..'\u0ac9'       // Gujarati
    | '\u0acd'                 // Gujarati
    | '\u0ae2'..'\u0ae4'       // Gujarati
    | '\u0b01'                 // Oriya
    | '\u0b3c'                 // Oriya
    | '\u0b3f'                 // Oriya
    | '\u0b41'..'\u0b45'       // Oriya
    | '\u0b4d'                 // Oriya
    | '\u0b56'                 // Oriya
    | '\u0b62'..'\u0b64'       // Oriya
    | '\u0b82'                 // Tamil
    | '\u0bc0'                 // Tamil
    | '\u0bcd'                 // Tamil
    | '\u0c00'                 // Telugu
    | '\u0c3e'..'\u0c41'       // Telugu
    | '\u0c46'..'\u0c49'       // Telugu
    | '\u0c4a'..'\u0c4e'       // Telugu
    | '\u0c55'..'\u0c57'       // Telugu
    | '\u0c62'..'\u0c64'       // Telugu
    | '\u0c81'                 // Kannada
    | '\u0cbc'                 // Kannada
    | '\u0cbf'                 // Kannada
    | '\u0cc6'                 // Kannada
    | '\u0ccc'..'\u0cce'       // Kannada
    | '\u0ce2'..'\u0ce4'       // Kannada
    | '\u0d01'                 // Malayalam
    | '\u0d41'..'\u0d45'       // Malayalam
    | '\u0d4d'                 // Malayalam
    | '\u0d62'..'\u0d64'       // Malayalam
    | '\u0dca'                 // Sinhala
    | '\u0dd2'..'\u0dd5'       // Sinhala
    | '\u0dd6'                 // Sinhala
    | '\u0e31'                 // Thai
    | '\u0e34'..'\u0e3b'       // Thai
    | '\u0e47'..'\u0e4f'       // Thai
    | '\u0eb1'                 // Lao
    | '\u0eb4'..'\u0eba'       // Lao
    | '\u0ebb'..'\u0ebd'       // Lao
    | '\u0ec8'..'\u0ece'       // Lao
    | '\u0f18'..'\u0f1a'       // Tibetan
    | '\u0f35'                 // Tibetan
    | '\u0f37'                 // Tibetan
    | '\u0f39'                 // Tibetan
    | '\u0f71'..'\u0f7f'       // Tibetan
    | '\u0f80'..'\u0f85'       // Tibetan
    | '\u0f86'..'\u0f88'       // Tibetan
    | '\u0f8d'..'\u0f98'       // Tibetan
    | '\u0f99'..'\u0fbd'       // Tibetan
    | '\u0fc6'                 // Tibetan
    | '\u102d'..'\u1031'       // Myanmar
    | '\u1032'..'\u1038'       // Myanmar
    | '\u1039'..'\u103b'       // Myanmar
    | '\u103d'..'\u103f'       // Myanmar
    | '\u1058'..'\u105a'       // Myanmar
    | '\u105e'..'\u1061'       // Myanmar
    | '\u1071'..'\u1075'       // Myanmar
    | '\u1082'                 // Myanmar
    | '\u1085'..'\u1087'       // Myanmar
    | '\u108d'                 // Myanmar
    | '\u109d'                 // Myanmar
    | '\u135d'..'\u1360'       // Ethiopic
    | '\u1712'..'\u1715'       // Tagalog
    | '\u1732'..'\u1735'       // Hanunoo
    | '\u1752'..'\u1754'       // Buhid
    | '\u1772'..'\u1774'       // Tagbanwa
    | '\u17b4'..'\u17b6'       // Khmer
    | '\u17b7'..'\u17be'       // Khmer
    | '\u17c6'                 // Khmer
    | '\u17c9'..'\u17d4'       // Khmer
    | '\u17dd'                 // Khmer
    | '\u180b'..'\u180e'       // Mongolian
    | '\u1885'..'\u1887'       // Mongolian
    | '\u18a9'                 // Mongolian
    | '\u1920'..'\u1923'       // Limbu
    | '\u1927'..'\u1929'       // Limbu
    | '\u1932'                 // Limbu
    | '\u1939'..'\u193c'       // Limbu
    | '\u1a17'..'\u1a19'       // Buginese
    | '\u1a1b'                 // Buginese
    | '\u1a56'                 // Tai_Tham
    | '\u1a58'..'\u1a5f'       // Tai_Tham
    | '\u1a60'                 // Tai_Tham
    | '\u1a62'                 // Tai_Tham
    | '\u1a65'..'\u1a6d'       // Tai_Tham
    | '\u1a73'..'\u1a7d'       // Tai_Tham
    | '\u1a7f'                 // Tai_Tham
    | '\u1ab0'..'\u1abe'       // Combining_Diacritical_Marks_Extended
    | '\u1b00'..'\u1b04'       // Balinese
    | '\u1b34'                 // Balinese
    | '\u1b36'..'\u1b3b'       // Balinese
    | '\u1b3c'                 // Balinese
    | '\u1b42'                 // Balinese
    | '\u1b6b'..'\u1b74'       // Balinese
    | '\u1b80'..'\u1b82'       // Sundanese
    | '\u1ba2'..'\u1ba6'       // Sundanese
    | '\u1ba8'..'\u1baa'       // Sundanese
    | '\u1bab'..'\u1bae'       // Sundanese
    | '\u1be6'                 // Batak
    | '\u1be8'..'\u1bea'       // Batak
    | '\u1bed'                 // Batak
    | '\u1bef'..'\u1bf2'       // Batak
    | '\u1c2c'..'\u1c34'       // Lepcha
    | '\u1c36'..'\u1c38'       // Lepcha
    | '\u1cd0'..'\u1cd3'       // Vedic_Extensions
    | '\u1cd4'..'\u1ce1'       // Vedic_Extensions
    | '\u1ce2'..'\u1ce9'       // Vedic_Extensions
    | '\u1ced'                 // Vedic_Extensions
    | '\u1cf4'                 // Vedic_Extensions
    | '\u1cf8'..'\u1cfa'       // Vedic_Extensions
    | '\u1dc0'..'\u1df6'       // Combining_Diacritical_Marks_Supplement
    | '\u1dfb'..'\u1e00'       // Combining_Diacritical_Marks_Supplement
    | '\u20d0'..'\u20dd'       // Combining_Diacritical_Marks_for_Symbols
    | '\u20e1'                 // Combining_Diacritical_Marks_for_Symbols
    | '\u20e5'..'\u20f1'       // Combining_Diacritical_Marks_for_Symbols
    | '\u2cef'..'\u2cf2'       // Coptic
    | '\u2d7f'                 // (Absent from Blocks.txt)
    | '\u2de0'..'\u2e00'       // Cyrillic_Extended-A
    | '\u302a'..'\u302e'       // CJK_Symbols_and_Punctuation
    | '\u3099'..'\u309b'       // Hiragana
    | '\ua66f'                 // Cyrillic_Extended-B
    | '\ua674'..'\ua67e'       // Cyrillic_Extended-B
    | '\ua69e'..'\ua6a0'       // Cyrillic_Extended-B
    | '\ua6f0'..'\ua6f2'       // Bamum
    | '\ua802'                 // Syloti_Nagri
    | '\ua806'                 // Syloti_Nagri
    | '\ua80b'                 // Syloti_Nagri
    | '\ua825'..'\ua827'       // Syloti_Nagri
    | '\ua8c4'..'\ua8c6'       // Saurashtra
    | '\ua8e0'..'\ua8f2'       // Devanagari_Extended
    | '\ua926'..'\ua92e'       // Kayah_Li
    | '\ua947'..'\ua952'       // Rejang
    | '\ua980'..'\ua983'       // Javanese
    | '\ua9b3'                 // Javanese
    | '\ua9b6'..'\ua9ba'       // Javanese
    | '\ua9bc'                 // Javanese
    | '\ua9e5'                 // Myanmar_Extended-B
    | '\uaa29'..'\uaa2f'       // Cham
    | '\uaa31'..'\uaa33'       // Cham
    | '\uaa35'..'\uaa37'       // Cham
    | '\uaa43'                 // Cham
    | '\uaa4c'                 // Cham
    | '\uaa7c'                 // Myanmar_Extended-A
    | '\uaab0'                 // Tai_Viet
    | '\uaab2'..'\uaab5'       // Tai_Viet
    | '\uaab7'..'\uaab9'       // Tai_Viet
    | '\uaabe'..'\uaac0'       // Tai_Viet
    | '\uaac1'                 // Tai_Viet
    | '\uaaec'..'\uaaee'       // Meetei_Mayek_Extensions
    | '\uaaf6'                 // Meetei_Mayek_Extensions
    | '\uabe5'                 // Meetei_Mayek
    | '\uabe8'                 // Meetei_Mayek
    | '\uabed'                 // Meetei_Mayek
    | '\ufb1e'                 // Alphabetic_Presentation_Forms
    | '\ufe00'..'\ufe10'       // Variation_Selectors
    | '\ufe20'..'\ufe30'       // Combining_Half_Marks
;

CLASSIFY_Nd:
      '\u0030'..'\u0039'       // Basic_Latin
    | '\u0660'..'\u066a'       // Arabic
    | '\u06f0'..'\u06fa'       // Arabic
    | '\u07c0'..'\u07ca'       // NKo
    | '\u0966'..'\u0970'       // Devanagari
    | '\u09e6'..'\u09f0'       // Bengali
    | '\u0a66'..'\u0a70'       // Gurmukhi
    | '\u0ae6'..'\u0af0'       // Gujarati
    | '\u0b66'..'\u0b70'       // Oriya
    | '\u0be6'..'\u0bf0'       // Tamil
    | '\u0c66'..'\u0c70'       // Telugu
    | '\u0ce6'..'\u0cf0'       // Kannada
    | '\u0d66'..'\u0d70'       // Malayalam
    | '\u0de6'..'\u0df0'       // Sinhala
    | '\u0e50'..'\u0e5a'       // Thai
    | '\u0ed0'..'\u0eda'       // Lao
    | '\u0f20'..'\u0f2a'       // Tibetan
    | '\u1040'..'\u104a'       // Myanmar
    | '\u1090'..'\u109a'       // Myanmar
    | '\u17e0'..'\u17ea'       // Khmer
    | '\u1810'..'\u181a'       // Mongolian
    | '\u1946'..'\u1950'       // Limbu
    | '\u19d0'..'\u19da'       // New_Tai_Lue
    | '\u1a80'..'\u1a8a'       // Tai_Tham
    | '\u1a90'..'\u1a9a'       // Tai_Tham
    | '\u1b50'..'\u1b5a'       // Balinese
    | '\u1bb0'..'\u1bba'       // Sundanese
    | '\u1c40'..'\u1c4a'       // Lepcha
    | '\u1c50'..'\u1c5a'       // Ol_Chiki
    | '\ua620'..'\ua62a'       // Vai
    | '\ua8d0'..'\ua8da'       // Saurashtra
    | '\ua900'..'\ua90a'       // Kayah_Li
    | '\ua9d0'..'\ua9da'       // Javanese
    | '\ua9f0'..'\ua9fa'       // Myanmar_Extended-B
    | '\uaa50'..'\uaa5a'       // Cham
    | '\uabf0'..'\uabfa'       // Meetei_Mayek
    | '\uff10'..'\uff1a'       // Halfwidth_and_Fullwidth_Forms
;

CLASSIFY_Nl:
      '\u16ee'..'\u16f1'       // Runic
    | '\u2160'..'\u2183'       // Number_Forms
    | '\u2185'..'\u2189'       // Number_Forms
    | '\u3007'                 // CJK_Symbols_and_Punctuation
    | '\u3021'..'\u302a'       // CJK_Symbols_and_Punctuation
    | '\u3038'..'\u303b'       // CJK_Symbols_and_Punctuation
    | '\ua6e6'..'\ua6f0'       // Bamum
;

CLASSIFY_No:
      '\u00b2'..'\u00b4'       // Latin-1_Supplement
    | '\u00b9'                 // Latin-1_Supplement
    | '\u00bc'..'\u00bf'       // Latin-1_Supplement
    | '\u09f4'..'\u09fa'       // Bengali
    | '\u0b72'..'\u0b78'       // Oriya
    | '\u0bf0'..'\u0bf3'       // Tamil
    | '\u0c78'..'\u0c7f'       // Telugu
    | '\u0d58'..'\u0d5f'       // Malayalam
    | '\u0d70'..'\u0d79'       // Malayalam
    | '\u0f2a'..'\u0f34'       // Tibetan
    | '\u1369'..'\u137d'       // Ethiopic
    | '\u17f0'..'\u17fa'       // Khmer
    | '\u19da'                 // New_Tai_Lue
    | '\u2070'                 // Superscripts_and_Subscripts
    | '\u2074'..'\u207a'       // Superscripts_and_Subscripts
    | '\u2080'..'\u208a'       // Superscripts_and_Subscripts
    | '\u2150'..'\u2160'       // Number_Forms
    | '\u2189'                 // Number_Forms
    | '\u2460'..'\u249c'       // Enclosed_Alphanumerics
    | '\u24ea'..'\u24ff'       // Enclosed_Alphanumerics
    | '\u2776'..'\u2794'       // Dingbats
    | '\u2cfd'                 // Coptic
    | '\u3192'..'\u3196'       // Kanbun
    | '\u3220'..'\u322a'       // Enclosed_CJK_Letters_and_Months
    | '\u3248'..'\u3250'       // Enclosed_CJK_Letters_and_Months
    | '\u3251'..'\u3260'       // Enclosed_CJK_Letters_and_Months
    | '\u3280'..'\u328a'       // Enclosed_CJK_Letters_and_Months
    | '\u32b1'..'\u32c0'       // Enclosed_CJK_Letters_and_Months
    | '\ua830'..'\ua836'       // Common_Indic_Number_Forms
;

CLASSIFY_Pc
//      '\u005f'                 // Basic_Latin
    : '\u203f'..'\u2041'       // General_Punctuation
    | '\u2054'                 // General_Punctuation
    | '\ufe33'..'\ufe35'       // CJK_Compatibility_Forms
    | '\ufe4d'..'\ufe50'       // CJK_Compatibility_Forms
    | '\uff3f'                 // Halfwidth_and_Fullwidth_Forms
;

CLASSIFY_Pd:
      '\u002d'                 // Basic_Latin
    | '\u058a'                 // Armenian
    | '\u05be'                 // Hebrew
    | '\u1400'                 // Unified_Canadian_Aboriginal_Syllabics
    | '\u1806'                 // Mongolian
    | '\u2010'..'\u2016'       // General_Punctuation
    | '\u2e17'                 // Supplemental_Punctuation
    | '\u2e1a'                 // Supplemental_Punctuation
    | '\u2e3a'..'\u2e3c'       // Supplemental_Punctuation
    | '\u2e40'                 // Supplemental_Punctuation
    | '\u301c'                 // CJK_Symbols_and_Punctuation
    | '\u3030'                 // CJK_Symbols_and_Punctuation
    | '\u30a0'                 // Katakana
    | '\ufe31'..'\ufe33'       // CJK_Compatibility_Forms
    | '\ufe58'                 // Small_Form_Variants
    | '\ufe63'                 // Small_Form_Variants
    | '\uff0d'                 // Halfwidth_and_Fullwidth_Forms
;

CLASSIFY_Pe
//      '\u0029'                 // Basic_Latin
//    | '\u005d'                 // Basic_Latin
//    | '\u007d'                 // Basic_Latin
    : '\u0f3b'                 // Tibetan
    | '\u0f3d'                 // Tibetan
    | '\u169c'                 // Ogham
    | '\u2046'                 // General_Punctuation
    | '\u207e'                 // Superscripts_and_Subscripts
    | '\u208e'                 // Superscripts_and_Subscripts
    | '\u2309'                 // Miscellaneous_Technical
    | '\u230b'                 // Miscellaneous_Technical
    | '\u232a'                 // Miscellaneous_Technical
    | '\u2769'                 // Dingbats
    | '\u276b'                 // Dingbats
    | '\u276d'                 // Dingbats
    | '\u276f'                 // Dingbats
    | '\u2771'                 // Dingbats
    | '\u2773'                 // Dingbats
    | '\u2775'                 // Dingbats
    | '\u27c6'                 // Miscellaneous_Mathematical_Symbols-A
    | '\u27e7'                 // Miscellaneous_Mathematical_Symbols-A
//    | '\u27e9'                 // Miscellaneous_Mathematical_Symbols-A
//    | '\u27eb'                 // Miscellaneous_Mathematical_Symbols-A
    | '\u27ed'                 // Miscellaneous_Mathematical_Symbols-A
    | '\u27ef'                 // (Absent from Blocks.txt)
    | '\u2984'                 // Miscellaneous_Mathematical_Symbols-B
    | '\u2986'                 // Miscellaneous_Mathematical_Symbols-B
    | '\u2988'                 // Miscellaneous_Mathematical_Symbols-B
//    | '\u298a'                 // Miscellaneous_Mathematical_Symbols-B
    | '\u298c'                 // Miscellaneous_Mathematical_Symbols-B
    | '\u298e'                 // Miscellaneous_Mathematical_Symbols-B
    | '\u2990'                 // Miscellaneous_Mathematical_Symbols-B
    | '\u2992'                 // Miscellaneous_Mathematical_Symbols-B
    | '\u2994'                 // Miscellaneous_Mathematical_Symbols-B
    | '\u2996'                 // Miscellaneous_Mathematical_Symbols-B
    | '\u2998'                 // Miscellaneous_Mathematical_Symbols-B
    | '\u29d9'                 // Miscellaneous_Mathematical_Symbols-B
    | '\u29db'                 // Miscellaneous_Mathematical_Symbols-B
    | '\u29fd'                 // Miscellaneous_Mathematical_Symbols-B
    | '\u2e23'                 // Supplemental_Punctuation
    | '\u2e25'                 // Supplemental_Punctuation
    | '\u2e27'                 // Supplemental_Punctuation
    | '\u2e29'                 // Supplemental_Punctuation
    | '\u3009'                 // CJK_Symbols_and_Punctuation
    | '\u300b'                 // CJK_Symbols_and_Punctuation
    | '\u300d'                 // CJK_Symbols_and_Punctuation
    | '\u300f'                 // CJK_Symbols_and_Punctuation
    | '\u3011'                 // CJK_Symbols_and_Punctuation
    | '\u3015'                 // CJK_Symbols_and_Punctuation
    | '\u3017'                 // CJK_Symbols_and_Punctuation
    | '\u3019'                 // CJK_Symbols_and_Punctuation
    | '\u301b'                 // CJK_Symbols_and_Punctuation
    | '\u301e'..'\u3020'       // CJK_Symbols_and_Punctuation
    | '\ufd3e'                 // Arabic_Presentation_Forms-A
    | '\ufe18'                 // Vertical_Forms
    | '\ufe36'                 // CJK_Compatibility_Forms
    | '\ufe38'                 // CJK_Compatibility_Forms
    | '\ufe3a'                 // CJK_Compatibility_Forms
    | '\ufe3c'                 // CJK_Compatibility_Forms
    | '\ufe3e'                 // CJK_Compatibility_Forms
    | '\ufe40'                 // CJK_Compatibility_Forms
    | '\ufe42'                 // CJK_Compatibility_Forms
    | '\ufe44'                 // CJK_Compatibility_Forms
    | '\ufe48'                 // CJK_Compatibility_Forms
    | '\ufe5a'                 // Small_Form_Variants
    | '\ufe5c'                 // Small_Form_Variants
    | '\ufe5e'                 // Small_Form_Variants
    | '\uff09'                 // Halfwidth_and_Fullwidth_Forms
    | '\uff3d'                 // Halfwidth_and_Fullwidth_Forms
    | '\uff5d'                 // Halfwidth_and_Fullwidth_Forms
    | '\uff60'                 // Halfwidth_and_Fullwidth_Forms
    | '\uff63'                 // Halfwidth_and_Fullwidth_Forms
;

CLASSIFY_Pf:
      '\u00bb'                 // Latin-1_Supplement
    | '\u2019'                 // General_Punctuation
    | '\u201d'                 // General_Punctuation
    | '\u203a'                 // General_Punctuation
    | '\u2e03'                 // Supplemental_Punctuation
    | '\u2e05'                 // Supplemental_Punctuation
    | '\u2e0a'                 // Supplemental_Punctuation
    | '\u2e0d'                 // Supplemental_Punctuation
    | '\u2e1d'                 // Supplemental_Punctuation
    | '\u2e21'                 // Supplemental_Punctuation
;

CLASSIFY_Pi:
      '\u00ab'                 // Latin-1_Supplement
    | '\u2018'                 // General_Punctuation
    | '\u201b'..'\u201d'       // General_Punctuation
    | '\u201f'                 // General_Punctuation
    | '\u2039'                 // General_Punctuation
    | '\u2e02'                 // Supplemental_Punctuation
    | '\u2e04'                 // Supplemental_Punctuation
    | '\u2e09'                 // Supplemental_Punctuation
    | '\u2e0c'                 // Supplemental_Punctuation
    | '\u2e1c'                 // Supplemental_Punctuation
    | '\u2e20'                 // Supplemental_Punctuation
;

CLASSIFY_Po:
      '\u0022'..'\u0024'       // Basic_Latin
    | '\u0025'..'\u0027'       // Basic_Latin
    | '\u002a'                 // Basic_Latin
//    | '\u002c'                 // Basic_Latin
    | '\u002f'..'\u0030'       // Basic_Latin
//    | '\u003a'                 // Basic_Latin
    | '\u003c'                 // Basic_Latin
    | '\u0040'..'\u0041'       // Basic_Latin
    | '\u005c'                 // Basic_Latin
    | '\u00a1'                 // Latin-1_Supplement
    | '\u00a7'                 // Latin-1_Supplement
    | '\u00b6'..'\u00b8'       // Latin-1_Supplement
    | '\u00bf'                 // Latin-1_Supplement
    | '\u037e'                 // Greek_and_Coptic
    | '\u0387'                 // Greek_and_Coptic
    | '\u055a'..'\u0560'       // Armenian
    | '\u0589'                 // Armenian
    | '\u05c0'                 // Hebrew
    | '\u05c3'                 // Hebrew
    | '\u05c6'                 // Hebrew
    | '\u05f3'..'\u05f5'       // Hebrew
    | '\u0609'..'\u060b'       // Arabic
    | '\u060c'..'\u060e'       // Arabic
    | '\u061b'                 // Arabic
    | '\u061e'..'\u0620'       // Arabic
    | '\u066a'..'\u066e'       // Arabic
    | '\u06d4'                 // Arabic
    | '\u0700'..'\u070e'       // Syriac
    | '\u07f7'..'\u07fa'       // NKo
    | '\u0830'..'\u083f'       // Samaritan
    | '\u085e'                 // Mandaic
    | '\u0964'..'\u0966'       // Devanagari
    | '\u0970'                 // Devanagari
    | '\u0af0'                 // Gujarati
    | '\u0df4'                 // Sinhala
    | '\u0e4f'                 // Thai
    | '\u0e5a'..'\u0e5c'       // Thai
    | '\u0f04'..'\u0f13'       // Tibetan
    | '\u0f14'                 // Tibetan
    | '\u0f85'                 // Tibetan
    | '\u0fd0'..'\u0fd5'       // Tibetan
    | '\u0fd9'..'\u0fdb'       // Tibetan
    | '\u104a'..'\u1050'       // Myanmar
    | '\u10fb'                 // Georgian
    | '\u1360'..'\u1369'       // Ethiopic
    | '\u166d'..'\u166f'       // Unified_Canadian_Aboriginal_Syllabics
    | '\u16eb'..'\u16ee'       // Runic
    | '\u1735'..'\u1737'       // Hanunoo
    | '\u17d4'..'\u17d7'       // Khmer
    | '\u17d8'..'\u17db'       // Khmer
    | '\u1800'..'\u1806'       // Mongolian
    | '\u1807'..'\u180b'       // Mongolian
    | '\u1944'..'\u1946'       // Limbu
    | '\u1a1e'..'\u1a20'       // Buginese
    | '\u1aa0'..'\u1aa7'       // Tai_Tham
    | '\u1aa8'..'\u1aae'       // Tai_Tham
    | '\u1b5a'..'\u1b61'       // Balinese
    | '\u1bfc'..'\u1c00'       // Batak
    | '\u1c3b'..'\u1c40'       // Lepcha
    | '\u1c7e'..'\u1c80'       // Ol_Chiki
    | '\u1cc0'..'\u1cc8'       // Sundanese_Supplement
    | '\u1cd3'                 // Vedic_Extensions
    | '\u2016'..'\u2018'       // General_Punctuation
    | '\u2020'..'\u2027'       // General_Punctuation
    | '\u2030'..'\u2031'       // General_Punctuation
    | '\u2033'..'\u2039'       // General_Punctuation
    | '\u203b'..'\u203f'       // General_Punctuation
    | '\u2041'..'\u2044'       // General_Punctuation
    | '\u2047'..'\u2052'       // General_Punctuation
    | '\u2053'                 // General_Punctuation
    | '\u2055'..'\u205f'       // General_Punctuation
    | '\u2cf9'..'\u2cfd'       // Coptic
    | '\u2cfe'..'\u2d00'       // Coptic
    | '\u2d70'                 // Tifinagh
    | '\u2e00'..'\u2e02'       // Supplemental_Punctuation
    | '\u2e06'..'\u2e09'       // Supplemental_Punctuation
    | '\u2e0b'                 // Supplemental_Punctuation
    | '\u2e0e'..'\u2e17'       // Supplemental_Punctuation
    | '\u2e18'..'\u2e1a'       // Supplemental_Punctuation
    | '\u2e1b'                 // Supplemental_Punctuation
    | '\u2e1e'..'\u2e20'       // Supplemental_Punctuation
    | '\u2e2a'..'\u2e2f'       // Supplemental_Punctuation
    | '\u2e30'..'\u2e3a'       // Supplemental_Punctuation
    | '\u2e3c'..'\u2e40'       // Supplemental_Punctuation
    | '\u2e41'                 // Supplemental_Punctuation
    | '\u2e43'..'\u2e45'       // Supplemental_Punctuation
    | '\u3001'..'\u3004'       // CJK_Symbols_and_Punctuation
    | '\u303d'                 // CJK_Symbols_and_Punctuation
    | '\u30fb'                 // Katakana
    | '\ua4fe'..'\ua500'       // Lisu
    | '\ua60d'..'\ua610'       // Vai
    | '\ua673'                 // Cyrillic_Extended-B
    | '\ua67e'                 // Cyrillic_Extended-B
    | '\ua6f2'..'\ua6f8'       // Bamum
    | '\ua874'..'\ua878'       // Phags-pa
    | '\ua8ce'..'\ua8d0'       // Saurashtra
    | '\ua8f8'..'\ua8fb'       // Devanagari_Extended
    | '\ua8fc'                 // Devanagari_Extended
    | '\ua92e'..'\ua930'       // Kayah_Li
    | '\ua95f'                 // (Absent from Blocks.txt)
    | '\ua9c1'..'\ua9ce'       // Javanese
    | '\ua9de'..'\ua9e0'       // Javanese
    | '\uaa5c'..'\uaa60'       // Cham
    | '\uaade'..'\uaae0'       // Tai_Viet
    | '\uaaf0'..'\uaaf2'       // Meetei_Mayek_Extensions
    | '\uabeb'                 // Meetei_Mayek
    | '\ufe10'..'\ufe17'       // Vertical_Forms
    | '\ufe19'                 // Vertical_Forms
    | '\ufe30'                 // CJK_Compatibility_Forms
    | '\ufe45'..'\ufe47'       // CJK_Compatibility_Forms
    | '\ufe49'..'\ufe4d'       // CJK_Compatibility_Forms
    | '\ufe50'..'\ufe53'       // Small_Form_Variants
    | '\ufe54'..'\ufe58'       // Small_Form_Variants
    | '\ufe5f'..'\ufe62'       // Small_Form_Variants
    | '\ufe68'                 // Small_Form_Variants
    | '\ufe6a'..'\ufe6c'       // Small_Form_Variants
    | '\uff01'..'\uff04'       // Halfwidth_and_Fullwidth_Forms
    | '\uff05'..'\uff08'       // Halfwidth_and_Fullwidth_Forms
    | '\uff0a'                 // Halfwidth_and_Fullwidth_Forms
    | '\uff0c'                 // Halfwidth_and_Fullwidth_Forms
    | '\uff0e'..'\uff10'       // Halfwidth_and_Fullwidth_Forms
    | '\uff1a'..'\uff1c'       // Halfwidth_and_Fullwidth_Forms
    | '\uff1f'..'\uff21'       // Halfwidth_and_Fullwidth_Forms
    | '\uff3c'                 // Halfwidth_and_Fullwidth_Forms
    | '\uff61'                 // Halfwidth_and_Fullwidth_Forms
    | '\uff64'..'\uff66'       // Halfwidth_and_Fullwidth_Forms
;

CLASSIFY_Ps
//      '\u0028'                 // Basic_Latin
//    | '\u005b'                 // Basic_Latin
//    | '\u007b'                 // Basic_Latin
    : '\u0f3a'                 // Tibetan
    | '\u0f3c'                 // Tibetan
    | '\u169b'                 // Ogham
    | '\u201a'                 // General_Punctuation
    | '\u201e'                 // General_Punctuation
    | '\u2045'                 // General_Punctuation
    | '\u207d'                 // Superscripts_and_Subscripts
    | '\u208d'                 // Superscripts_and_Subscripts
    | '\u2308'                 // Miscellaneous_Technical
    | '\u230a'                 // Miscellaneous_Technical
    | '\u2329'                 // Miscellaneous_Technical
    | '\u2768'                 // Dingbats
    | '\u276a'                 // Dingbats
    | '\u276c'                 // Dingbats
    | '\u276e'                 // Dingbats
    | '\u2770'                 // Dingbats
    | '\u2772'                 // Dingbats
    | '\u2774'                 // Dingbats
    | '\u27c5'                 // Miscellaneous_Mathematical_Symbols-A
    | '\u27e6'                 // Miscellaneous_Mathematical_Symbols-A
//    | '\u27e8'                 // Miscellaneous_Mathematical_Symbols-A
//    | '\u27ea'                 // Miscellaneous_Mathematical_Symbols-A
    | '\u27ec'                 // Miscellaneous_Mathematical_Symbols-A
    | '\u27ee'                 // Miscellaneous_Mathematical_Symbols-A
    | '\u2983'                 // Miscellaneous_Mathematical_Symbols-B
    | '\u2985'                 // Miscellaneous_Mathematical_Symbols-B
    | '\u2987'                 // Miscellaneous_Mathematical_Symbols-B
//    | '\u2989'                 // Miscellaneous_Mathematical_Symbols-B
    | '\u298b'                 // Miscellaneous_Mathematical_Symbols-B
    | '\u298d'                 // Miscellaneous_Mathematical_Symbols-B
    | '\u298f'                 // Miscellaneous_Mathematical_Symbols-B
    | '\u2991'                 // Miscellaneous_Mathematical_Symbols-B
    | '\u2993'                 // Miscellaneous_Mathematical_Symbols-B
    | '\u2995'                 // Miscellaneous_Mathematical_Symbols-B
    | '\u2997'                 // Miscellaneous_Mathematical_Symbols-B
    | '\u29d8'                 // Miscellaneous_Mathematical_Symbols-B
    | '\u29da'                 // Miscellaneous_Mathematical_Symbols-B
    | '\u29fc'                 // Miscellaneous_Mathematical_Symbols-B
    | '\u2e22'                 // Supplemental_Punctuation
    | '\u2e24'                 // Supplemental_Punctuation
    | '\u2e26'                 // Supplemental_Punctuation
    | '\u2e28'                 // Supplemental_Punctuation
    | '\u2e42'                 // Supplemental_Punctuation
    | '\u3008'                 // CJK_Symbols_and_Punctuation
    | '\u300a'                 // CJK_Symbols_and_Punctuation
    | '\u300c'                 // CJK_Symbols_and_Punctuation
    | '\u300e'                 // CJK_Symbols_and_Punctuation
    | '\u3010'                 // CJK_Symbols_and_Punctuation
    | '\u3014'                 // CJK_Symbols_and_Punctuation
    | '\u3016'                 // CJK_Symbols_and_Punctuation
    | '\u3018'                 // CJK_Symbols_and_Punctuation
    | '\u301a'                 // CJK_Symbols_and_Punctuation
    | '\u301d'                 // CJK_Symbols_and_Punctuation
    | '\ufd3f'                 // Arabic_Presentation_Forms-A
    | '\ufe17'                 // Vertical_Forms
    | '\ufe35'                 // CJK_Compatibility_Forms
    | '\ufe37'                 // CJK_Compatibility_Forms
    | '\ufe39'                 // CJK_Compatibility_Forms
    | '\ufe3b'                 // CJK_Compatibility_Forms
    | '\ufe3d'                 // CJK_Compatibility_Forms
    | '\ufe3f'                 // CJK_Compatibility_Forms
    | '\ufe41'                 // CJK_Compatibility_Forms
    | '\ufe43'                 // CJK_Compatibility_Forms
    | '\ufe47'                 // CJK_Compatibility_Forms
    | '\ufe59'                 // Small_Form_Variants
    | '\ufe5b'                 // Small_Form_Variants
    | '\ufe5d'                 // Small_Form_Variants
    | '\uff08'                 // Halfwidth_and_Fullwidth_Forms
    | '\uff3b'                 // Halfwidth_and_Fullwidth_Forms
    | '\uff5b'                 // Halfwidth_and_Fullwidth_Forms
    | '\uff5f'                 // Halfwidth_and_Fullwidth_Forms
    | '\uff62'                 // Halfwidth_and_Fullwidth_Forms
;

CLASSIFY_Sc:
      '\u0024'                 // Basic_Latin
    | '\u00a2'..'\u00a6'       // Latin-1_Supplement
    | '\u058f'                 // (Absent from Blocks.txt)
    | '\u060b'                 // Arabic
    | '\u09f2'..'\u09f4'       // Bengali
    | '\u09fb'                 // Bengali
    | '\u0af1'                 // Gujarati
    | '\u0bf9'                 // Tamil
    | '\u0e3f'                 // Thai
    | '\u17db'                 // Khmer
    | '\u20a0'..'\u20bf'       // Currency_Symbols
    | '\ua838'                 // Common_Indic_Number_Forms
    | '\ufdfc'                 // Arabic_Presentation_Forms-A
    | '\ufe69'                 // Small_Form_Variants
    | '\uff04'                 // Halfwidth_and_Fullwidth_Forms
    | '\uffe0'..'\uffe2'       // Halfwidth_and_Fullwidth_Forms
    | '\uffe5'..'\uffe7'       // Halfwidth_and_Fullwidth_Forms
;

CLASSIFY_Sk:
      '\u005e'                 // Basic_Latin
    | '\u0060'                 // Basic_Latin
    | '\u00a8'                 // Latin-1_Supplement
    | '\u00af'                 // Latin-1_Supplement
    | '\u00b4'                 // Latin-1_Supplement
    | '\u00b8'                 // Latin-1_Supplement
    | '\u02c2'..'\u02c6'       // Spacing_Modifier_Letters
    | '\u02d2'..'\u02e0'       // Spacing_Modifier_Letters
    | '\u02e5'..'\u02ec'       // Spacing_Modifier_Letters
    | '\u02ed'                 // Spacing_Modifier_Letters
    | '\u02ef'..'\u0300'       // Spacing_Modifier_Letters
    | '\u0375'                 // Greek_and_Coptic
    | '\u0384'..'\u0386'       // Greek_and_Coptic
    | '\u1fbd'                 // Greek_Extended
    | '\u1fbf'..'\u1fc2'       // Greek_Extended
    | '\u1fcd'..'\u1fd0'       // Greek_Extended
    | '\u1fdd'..'\u1fe0'       // Greek_Extended
    | '\u1fed'..'\u1ff0'       // Greek_Extended
    | '\u1ffd'..'\u1fff'       // Greek_Extended
    | '\u309b'..'\u309d'       // Hiragana
    | '\ua700'..'\ua717'       // Modifier_Tone_Letters
    | '\ua720'..'\ua722'       // Latin_Extended-D
    | '\ua789'..'\ua78b'       // Latin_Extended-D
    | '\uab5b'                 // Latin_Extended-E
    | '\ufbb2'..'\ufbc2'       // Arabic_Presentation_Forms-A
    | '\uff3e'                 // Halfwidth_and_Fullwidth_Forms
    | '\uff40'                 // Halfwidth_and_Fullwidth_Forms
    | '\uffe3'                 // Halfwidth_and_Fullwidth_Forms
;

CLASSIFY_Sm:
      '\u002b'                 // Basic_Latin
    | '\u003c'..'\u003e'       // Basic_Latin
    | '\u007c'                 // Basic_Latin
    | '\u007e'                 // Basic_Latin
    | '\u00ac'                 // Latin-1_Supplement
    | '\u00b1'                 // Latin-1_Supplement
    | '\u00d7'                 // Latin-1_Supplement
    | '\u00f7'                 // Latin-1_Supplement
    | '\u03f6'                 // Greek_and_Coptic
    | '\u0606'..'\u0609'       // Arabic
    | '\u2044'                 // General_Punctuation
    | '\u2052'                 // General_Punctuation
    | '\u207a'..'\u207d'       // Superscripts_and_Subscripts
    | '\u208a'..'\u208d'       // Superscripts_and_Subscripts
    | '\u2118'                 // Letterlike_Symbols
    | '\u2140'..'\u2145'       // Letterlike_Symbols
    | '\u214b'                 // Letterlike_Symbols
    | '\u2190'..'\u2195'       // Arrows
    | '\u219a'..'\u219c'       // Arrows
    | '\u21a0'                 // Arrows
    | '\u21a3'                 // Arrows
    | '\u21a6'                 // Arrows
    | '\u21ae'                 // Arrows
    | '\u21ce'..'\u21d0'       // Arrows
    | '\u21d2'                 // Arrows
    | '\u21d4'                 // Arrows
    | '\u21f4'..'\u2300'       // Arrows
    | '\u2320'..'\u2322'       // Miscellaneous_Technical
    | '\u237c'                 // Miscellaneous_Technical
    | '\u239b'..'\u23b4'       // Miscellaneous_Technical
    | '\u23dc'..'\u23e2'       // Miscellaneous_Technical
    | '\u25b7'                 // Geometric_Shapes
    | '\u25c1'                 // Geometric_Shapes
    | '\u25f8'..'\u2600'       // Geometric_Shapes
    | '\u266f'                 // Miscellaneous_Symbols
    | '\u27c0'..'\u27c5'       // Miscellaneous_Mathematical_Symbols-A
    | '\u27c7'..'\u27e6'       // Miscellaneous_Mathematical_Symbols-A
    | '\u27f0'..'\u2800'       // Supplemental_Arrows-A
    | '\u2900'..'\u2983'       // Supplemental_Arrows-B
    | '\u2999'..'\u29d8'       // Miscellaneous_Mathematical_Symbols-B
    | '\u29dc'..'\u29fc'       // Miscellaneous_Mathematical_Symbols-B
    | '\u29fe'..'\u2b00'       // Miscellaneous_Mathematical_Symbols-B
    | '\u2b30'..'\u2b45'       // Miscellaneous_Symbols_and_Arrows
    | '\u2b47'..'\u2b4d'       // Miscellaneous_Symbols_and_Arrows
    | '\ufb29'                 // Alphabetic_Presentation_Forms
    | '\ufe62'                 // Small_Form_Variants
    | '\ufe64'..'\ufe67'       // Small_Form_Variants
    | '\uff0b'                 // Halfwidth_and_Fullwidth_Forms
    | '\uff1c'..'\uff1f'       // Halfwidth_and_Fullwidth_Forms
    | '\uff5c'                 // Halfwidth_and_Fullwidth_Forms
    | '\uff5e'                 // Halfwidth_and_Fullwidth_Forms
    | '\uffe2'                 // Halfwidth_and_Fullwidth_Forms
    | '\uffe9'..'\uffed'       // Halfwidth_and_Fullwidth_Forms
;

CLASSIFY_So:
      '\u00a6'                 // Latin-1_Supplement
    | '\u00a9'                 // Latin-1_Supplement
    | '\u00ae'                 // Latin-1_Supplement
    | '\u00b0'                 // Latin-1_Supplement
    | '\u0482'                 // Cyrillic
    | '\u058d'..'\u058f'       // Armenian
    | '\u060e'..'\u0610'       // Arabic
    | '\u06de'                 // Arabic
    | '\u06e9'                 // Arabic
    | '\u06fd'..'\u06ff'       // Arabic
    | '\u07f6'                 // NKo
    | '\u09fa'                 // Bengali
    | '\u0b70'                 // Oriya
    | '\u0bf3'..'\u0bf9'       // Tamil
    | '\u0bfa'                 // Tamil
    | '\u0c7f'                 // (Absent from Blocks.txt)
    | '\u0d4f'                 // Malayalam
    | '\u0d79'                 // Malayalam
    | '\u0f01'..'\u0f04'       // Tibetan
    | '\u0f13'                 // Tibetan
    | '\u0f15'..'\u0f18'       // Tibetan
    | '\u0f1a'..'\u0f20'       // Tibetan
    | '\u0f34'                 // Tibetan
    | '\u0f36'                 // Tibetan
    | '\u0f38'                 // Tibetan
    | '\u0fbe'..'\u0fc6'       // Tibetan
    | '\u0fc7'..'\u0fcd'       // Tibetan
    | '\u0fce'..'\u0fd0'       // Tibetan
    | '\u0fd5'..'\u0fd9'       // Tibetan
    | '\u109e'..'\u10a0'       // Myanmar
    | '\u1390'..'\u139a'       // Ethiopic_Supplement
    | '\u1940'                 // Limbu
    | '\u19de'..'\u1a00'       // New_Tai_Lue
    | '\u1b61'..'\u1b6b'       // Balinese
    | '\u1b74'..'\u1b7d'       // Balinese
    | '\u2100'..'\u2102'       // Letterlike_Symbols
    | '\u2103'..'\u2107'       // Letterlike_Symbols
    | '\u2108'..'\u210a'       // Letterlike_Symbols
    | '\u2114'                 // Letterlike_Symbols
    | '\u2116'..'\u2118'       // Letterlike_Symbols
    | '\u211e'..'\u2124'       // Letterlike_Symbols
    | '\u2125'                 // Letterlike_Symbols
    | '\u2127'                 // Letterlike_Symbols
    | '\u2129'                 // Letterlike_Symbols
    | '\u212e'                 // Letterlike_Symbols
    | '\u213a'..'\u213c'       // Letterlike_Symbols
    | '\u214a'                 // Letterlike_Symbols
    | '\u214c'..'\u214e'       // Letterlike_Symbols
    | '\u214f'                 // (Absent from Blocks.txt)
    | '\u218a'..'\u218c'       // Number_Forms
    | '\u2195'                 // Arrows
    | '\u219a'                 // Arrows
    | '\u219c'..'\u21a0'       // Arrows
    | '\u21a1'..'\u21a3'       // Arrows
    | '\u21a4'..'\u21a6'       // Arrows
    | '\u21a7'..'\u21ae'       // Arrows
    | '\u21af'..'\u21ce'       // Arrows
    | '\u21d0'..'\u21d2'       // Arrows
    | '\u21d3'                 // Arrows
    | '\u21d5'..'\u21f4'       // Arrows
    | '\u2300'..'\u2308'       // Miscellaneous_Technical
    | '\u230c'..'\u2320'       // Miscellaneous_Technical
    | '\u2322'..'\u2329'       // Miscellaneous_Technical
    | '\u232b'..'\u237c'       // Miscellaneous_Technical
    | '\u237d'..'\u239b'       // Miscellaneous_Technical
    | '\u23b4'..'\u23dc'       // Miscellaneous_Technical
    | '\u23e2'..'\u23ff'       // Miscellaneous_Technical
    | '\u2400'..'\u2427'       // Control_Pictures
    | '\u2440'..'\u244b'       // Optical_Character_Recognition
    | '\u249c'..'\u24ea'       // Enclosed_Alphanumerics
    | '\u2501'..'\u250b'       // Box_Drawing
    | '\u250d'..'\u2513'       // Box_Drawing
    | '\u2515'..'\u2549'       // Box_Drawing
    | '\u2551'..'\u2576'       // Box_Drawing
    | '\u2578'..'\u25b7'       // Box_Drawing
    | '\u25b8'..'\u25c1'       // Geometric_Shapes
    | '\u25c2'..'\u25f8'       // Geometric_Shapes
    | '\u2600'..'\u266f'       // Miscellaneous_Symbols
    | '\u2670'..'\u2768'       // Miscellaneous_Symbols
    | '\u2794'..'\u27c0'       // Dingbats
    | '\u2800'..'\u2900'       // Braille_Patterns
    | '\u2b00'..'\u2b30'       // Miscellaneous_Symbols_and_Arrows
    | '\u2b45'..'\u2b47'       // Miscellaneous_Symbols_and_Arrows
    | '\u2b4d'..'\u2b74'       // Miscellaneous_Symbols_and_Arrows
    | '\u2b76'..'\u2b96'       // Miscellaneous_Symbols_and_Arrows
    | '\u2b98'..'\u2bba'       // Miscellaneous_Symbols_and_Arrows
    | '\u2bbd'..'\u2bc9'       // Miscellaneous_Symbols_and_Arrows
    | '\u2bca'..'\u2bd2'       // Miscellaneous_Symbols_and_Arrows
    | '\u2bec'..'\u2bf0'       // Miscellaneous_Symbols_and_Arrows
    | '\u2ce5'..'\u2ceb'       // Coptic
    | '\u2e80'..'\u2e9a'       // CJK_Radicals_Supplement
    | '\u2e9b'..'\u2ef4'       // CJK_Radicals_Supplement
    | '\u2f00'..'\u2fd6'       // Kangxi_Radicals
    | '\u2ff0'..'\u2ffc'       // Ideographic_Description_Characters
    | '\u3004'                 // CJK_Symbols_and_Punctuation
    | '\u3012'..'\u3014'       // CJK_Symbols_and_Punctuation
    | '\u3020'                 // CJK_Symbols_and_Punctuation
    | '\u3036'..'\u3038'       // CJK_Symbols_and_Punctuation
    | '\u303e'..'\u3040'       // CJK_Symbols_and_Punctuation
    | '\u3190'..'\u3192'       // Kanbun
    | '\u3196'..'\u31a0'       // Kanbun
    | '\u31c0'..'\u31e4'       // CJK_Strokes
    | '\u3200'..'\u321f'       // Enclosed_CJK_Letters_and_Months
    | '\u322a'..'\u3248'       // Enclosed_CJK_Letters_and_Months
    | '\u3250'                 // Enclosed_CJK_Letters_and_Months
    | '\u3260'..'\u3280'       // Enclosed_CJK_Letters_and_Months
    | '\u328a'..'\u32b1'       // Enclosed_CJK_Letters_and_Months
    | '\u32c0'..'\u32ff'       // Enclosed_CJK_Letters_and_Months
    | '\u3300'..'\u3400'       // CJK_Compatibility
    | '\u4dc0'..'\u4e00'       // Yijing_Hexagram_Symbols
    | '\ua490'..'\ua4c7'       // Yi_Radicals
    | '\ua828'..'\ua82c'       // Syloti_Nagri
    | '\ua836'..'\ua838'       // Common_Indic_Number_Forms
    | '\ua839'                 // Common_Indic_Number_Forms
    | '\uaa77'..'\uaa7a'       // Myanmar_Extended-A
    | '\ufdfd'                 // Arabic_Presentation_Forms-A
    | '\uffe4'                 // Halfwidth_and_Fullwidth_Forms
    | '\uffe8'                 // Halfwidth_and_Fullwidth_Forms
    | '\uffed'..'\uffef'       // Halfwidth_and_Fullwidth_Forms
    | '\ufffc'..'\ufffe'       // Specials
;

CLASSIFY_Zl:
      '\u2028'                 // General_Punctuation
;

CLASSIFY_Zp:
      '\u2029'                 // General_Punctuation
;

CLASSIFY_Zs:
      '\u0020'                 // Basic_Latin
    | '\u00a0'                 // Latin-1_Supplement
    | '\u1680'                 // Ogham
    | '\u2000'..'\u200b'       // General_Punctuation
    | '\u202f'                 // General_Punctuation
    | '\u205f'                 // General_Punctuation
    | '\u3000'                 // CJK_Symbols_and_Punctuation
;
CLASSIFY_C :
       CLASSIFY_Cc
     | CLASSIFY_Cf
     | CLASSIFY_Co
     | CLASSIFY_Cs  // from local/PropertyValueAliases.txt
;

CLASSIFY_LC :
       CLASSIFY_Ll
     | CLASSIFY_Lt
     | CLASSIFY_Lu  // from local/PropertyValueAliases.txt
;

CLASSIFY_M :
       CLASSIFY_Mc
     | CLASSIFY_Me
     | CLASSIFY_Mn  // from local/PropertyValueAliases.txt
;

CLASSIFY_L :
       CLASSIFY_Ll
     | CLASSIFY_Lm
     | CLASSIFY_Lo
     | CLASSIFY_Lt
     | CLASSIFY_Lu  // from local/PropertyValueAliases.txt
;

CLASSIFY_N :
       CLASSIFY_Nd
     | CLASSIFY_Nl
     | CLASSIFY_No  // from local/PropertyValueAliases.txt
;

CLASSIFY_P :
       CLASSIFY_Pc
     | CLASSIFY_Pd
     | CLASSIFY_Pe
     | CLASSIFY_Pf
     | CLASSIFY_Pi
     | CLASSIFY_Po
     | CLASSIFY_Ps  // from local/PropertyValueAliases.txt
;

CLASSIFY_S :
       CLASSIFY_Sc
     | CLASSIFY_Sk
     | CLASSIFY_Sm
     | CLASSIFY_So  // from local/PropertyValueAliases.txt
;

CLASSIFY_Z :
       CLASSIFY_Zl
     | CLASSIFY_Zp
     | CLASSIFY_Zs  // from local/PropertyValueAliases.txt
;


/* Unicode codepoint classification */

CLASSIFY_WS : CLASSIFY_Z +                  // hand-written rule
;

CLASSIFY_ID0 : CLASSIFY_L | '_'             // hand-written rule
;

CLASSIFY_ID1 : CLASSIFY_ID0 | CLASSIFY_N    // hand-written rule
;

ID : CLASSIFY_ID0 CLASSIFY_ID1 *            // hand-written rule
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

