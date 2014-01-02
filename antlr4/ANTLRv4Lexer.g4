/*
 * [The "BSD license"]
 *  Copyright (c) 2012 Terence Parr
 *  Copyright (c) 2012 Sam Harwell
 *  All rights reserved.
 *
 *  Redistribution and use in source and binary forms, with or without
 *  modification, are permitted provided that the following conditions
 *  are met:
 *
 *  1. Redistributions of source code must retain the above copyright
 *     notice, this list of conditions and the following disclaimer.
 *  2. Redistributions in binary form must reproduce the above copyright
 *     notice, this list of conditions and the following disclaimer in the
 *     documentation and/or other materials provided with the distribution.
 *  3. The name of the author may not be used to endorse or promote products
 *     derived from this software without specific prior written permission.
 *
 *  THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 *  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 *  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 *  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 *  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 *  NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 *  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 *  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 *  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 *  THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

/** A grammar for ANTLR v4 tokens */
lexer grammar ANTLRv4Lexer;

tokens {
	TOKEN_REF,
	RULE_REF,
	LEXER_CHAR_SET
}

@members {
	private int _ruleType;

	protected void handleBeginArgAction() {
		if (inLexerRule()) {
			pushMode(LexerCharSet);
			more();
		} else {
			pushMode(ArgAction);
			more();
		}
	}

	@Override
	public Token emit() {
		if (_type == ID) {
			String firstChar = _input.getText(Interval.of(_tokenStartCharIndex, _tokenStartCharIndex));
			if (Character.isUpperCase(firstChar.charAt(0))) {
				_type = TOKEN_REF;
			} else {
				_type = RULE_REF;
			}

			if (_ruleType == Token.INVALID_TYPE) {
				_ruleType = _type;
			}
		} else if (_type == SEMI) {
			_ruleType = Token.INVALID_TYPE;
		}

		return super.emit();
	}

	private boolean inLexerRule() {
		return _ruleType == TOKEN_REF;
	}
}

DOC_COMMENT
	:	'/**' .*? '*/'
	;

BLOCK_COMMENT
	:	'/*' .*? '*/'  //-> channel(HIDDEN)
	;

LINE_COMMENT
	:	'//' ~[\r\n]*  // -> channel(HIDDEN)
	;

DOUBLE_QUOTE_STRING_LITERAL
	:	'"' ('\\' . | ~'"' )*? '"'
	;

BEGIN_ARG_ACTION
	:	'[' {handleBeginArgAction();}
	;

BEGIN_ACTION
	:	'{' -> more, pushMode(Action)
	;

// OPTIONS and TOKENS must also consume the opening brace that captures
// their option block, as this is teh easiest way to parse it separate
// to an ACTION block, despite it usingthe same {} delimiters.
//
OPTIONS      : 'options' [ \t\f\n\r]* '{'  ;
TOKENS		 : 'tokens'  [ \t\f\n\r]* '{'  ;

IMPORT       : 'import'               ;
FRAGMENT     : 'fragment'             ;
LEXER        : 'lexer'                ;
PARSER       : 'parser'               ;
GRAMMAR      : 'grammar'              ;
PROTECTED    : 'protected'            ;
PUBLIC       : 'public'               ;
PRIVATE      : 'private'              ;
RETURNS      : 'returns'              ;
LOCALS       : 'locals'               ;
THROWS       : 'throws'               ;
CATCH        : 'catch'                ;
FINALLY      : 'finally'              ;
MODE         : 'mode'                 ;

COLON        : ':'                    ;
COLONCOLON   : '::'                   ;
COMMA        : ','                    ;
SEMI         : ';'                    ;
LPAREN       : '('                    ;
RPAREN       : ')'                    ;
RARROW       : '->'                   ;
LT           : '<'                    ;
GT           : '>'                    ;
ASSIGN       : '='                    ;
QUESTION     : '?'                    ;
STAR         : '*'                    ;
PLUS         : '+'                    ;
PLUS_ASSIGN  : '+='                   ;
OR           : '|'                    ;
DOLLAR       : '$'                    ;
DOT		     : '.'                    ;
RANGE        : '..'                   ;
AT           : '@'                    ;
POUND        : '#'                    ;
NOT          : '~'                    ;
RBRACE       : '}'                    ;

/** Allow unicode rule/token names */
ID	:	NameStartChar NameChar*;

fragment
NameChar
	:   NameStartChar
	|   '0'..'9'
	|   '_'
	|   '\u00B7'
	|   '\u0300'..'\u036F'
	|   '\u203F'..'\u2040'
	;

fragment
NameStartChar
	:   'A'..'Z'
	|   'a'..'z'
	|   '\u00C0'..'\u00D6'
	|   '\u00D8'..'\u00F6'
	|   '\u00F8'..'\u02FF'
	|   '\u0370'..'\u037D'
	|   '\u037F'..'\u1FFF'
	|   '\u200C'..'\u200D'
	|   '\u2070'..'\u218F'
	|   '\u2C00'..'\u2FEF'
	|   '\u3001'..'\uD7FF'
	|   '\uF900'..'\uFDCF'
	|   '\uFDF0'..'\uFFFD'
	; // ignores | ['\u10000-'\uEFFFF] ;

INT	: [0-9]+
	;

// ANTLR makes no distinction between a single character literal and a
// multi-character string. All literals are single quote delimited and
// may contain unicode escape sequences of the form \uxxxx, where x
// is a valid hexadecimal number (as per Java basically).
STRING_LITERAL
	:  '\'' (ESC_SEQ | ~['\\])* '\''
	;

// Any kind of escaped character that we can embed within ANTLR
// literal strings.
fragment
ESC_SEQ
	:	'\\'
		(	// The standard escaped character set such as tab, newline, etc.
			[btnfr"'\\]
		|	// A Java style Unicode escape sequence
			UNICODE_ESC
		)
	;

fragment
UNICODE_ESC
    :   'u' (HEX_DIGIT (HEX_DIGIT (HEX_DIGIT HEX_DIGIT?)?)?)?
    ;

fragment
HEX_DIGIT : [0-9a-fA-F]	;

WS  :	[ \t\r\n\f]+ ; //-> channel(HIDDEN)	;

// -----------------
// Illegal Character
//
// This is an illegal character trap which is always the last rule in the
// lexer specification. It matches a single character of any value and being
// the last rule in the file will match when no other rule knows what to do
// about the character. It is reported as an error but is not passed on to the
// parser. This means that the parser to deal with the gramamr file anyway
// but we will not try to analyse or code generate from a file with lexical
// errors.
//
/*
ERRCHAR
	:	.	-> channel(HIDDEN)
	;
*/

mode ArgAction; // E.g., [int x, List<String> a[]]

	NESTED_ARG_ACTION
		:	'['                         -> more, pushMode(ArgAction)
		;

	ARG_ACTION_ESCAPE
		:   '\\' .                      -> more
		;

    ARG_ACTION_STRING_LITERAL
        :	('"' ('\\' . | ~["\\])* '"')-> more
        ;

    ARG_ACTION_CHAR_LITERAL
        :	('"' '\\' . | ~["\\] '"')   -> more
        ;

    ARG_ACTION
		:   ']'                         -> popMode
		;

	UNTERMINATED_ARG_ACTION // added this to return non-EOF token type here. EOF did something weird
		:	EOF							-> popMode
		;

    ARG_ACTION_CHAR // must be last
        :   .                           -> more
        ;

// ----------------
// Action structure
//
// Many language targets use {} as block delimiters and so we
// must recursively match {} delimited blocks to balance the
// braces. Additionally, we must make some assumptions about
// literal string representation in the target language. We assume
// that they are delimited by ' or " and so consume these
// in their own alts so as not to inadvertantly match {}.
// This mode is recursive on matching a {
mode Action;

	NESTED_ACTION
		:	'{'
            -> more, pushMode(Action)
		;

	ACTION_ESCAPE
		:   '\\' .                      -> more
		;

    ACTION_STRING_LITERAL
        :	'"' ('\\' . | ~["\\])* '"'  -> more
        ;

    ACTION_CHAR_LITERAL
        :	('"' '\\' . | ~["\\] '"')   -> more
        ;

	ACTION_COMMENT
		:   (BLOCK_COMMENT
            |LINE_COMMENT
            )                           -> more
        ;

    ACTION
		:   '}'
            {
            popMode();
        	if ( _modeStack.size()>0 ) more(); // keep scarfing until outermost }
            }
		;

	UNTERMINATED_ACTION
		:	EOF							-> popMode
		;

    ACTION_CHAR
        :   .                           -> more
        ;

mode LexerCharSet;

	LEXER_CHAR_SET_BODY
		:	(	~[\]\\]
			|	'\\' .
			)+
                                        -> more
		;

	LEXER_CHAR_SET
		:   ']'                         -> popMode
		;

	UNTERMINATED_CHAR_SET
		:	EOF							-> popMode
		;

