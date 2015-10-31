/*
 * [The "BSD license"]
 * Copyright (c) 2011-2014 Terence Parr
 * Copyright (c) 2015 Gerald Rosenberg
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * 3. The name of the author may not be used to endorse or promote products
 * derived from this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

 /**
 *	A grammar for StringTemplate v4 implemented using Antlr v4 syntax
 *
 *	Modified 2015.06.16 gbr
 *	-- update for compatibility with Antlr v4.5
 *	-- use imported standard fragments
 */

lexer grammar STGLexer;

import LexBasic;	// Standard set of fragments

channels {
	OFF_CHANNEL		// non-default channel for whitespace and comments
}

// ------------------------------------------------------------------------------
// mode default

DOC_COMMENT			: DocComment		-> channel(OFF_CHANNEL)	;
BLOCK_COMMENT		: BlockComment		-> channel(OFF_CHANNEL)	;
LINE_COMMENT		: LineComment		-> channel(OFF_CHANNEL)	;

TMPL_COMMENT		: LBang .? RBang	-> channel(OFF_CHANNEL)	;

HORZ_WS				: Hws+			-> channel(OFF_CHANNEL)	;
VERT_WS				: Vws+			-> channel(OFF_CHANNEL)	;

ID        			: NameStartChar NameChar*			;

STRING				: DQuoteLiteral			;
BIGSTRING 			: LDAngle .*? RDAngle	;
BIGSTRING_NO_NL		: LPct .*?	RPct		;
ANON_TEMPLATE		: LBrace .*? RBrace		;


// -----------------------------------
// Symbols

TMPL_ASSIGN	: TmplAssign	;
ASSIGN		: Equal			;

DOT			: Dot			;
COMMA		: Comma			;
COLON		: Colon			;
LPAREN		: LParen		;
RPAREN		: RParen		;
LBRACK		: LBrack		;
RBRACK		: RBrack		;
AT			: At			;
TRUE		: True			;
FALSE		: False			;
ELLIPSIS	: Ellipsis		;

// -----------------------------------
// Key words

DELIMITERS	: 'delimiters'	;
IMPORT		: 'import'		;
DEFAULT		: 'default'		;
KEY			: 'key'			;
VALUE		: 'value'		;

FIRST		: 'first'		;
LAST		: 'last'		;
REST		: 'rest'		;
TRUNC		: 'trunc'		;
STRIP		: 'strip'		;
TRIM		: 'trim'		;
LENGTH		: 'length'		;
STRLEN		: 'strlen'		;
REVERSE		: 'reverse'		;

GROUP		: 'group'		;	// not used by parser?
WRAP		: 'wrap'		;
ANCHOR		: 'anchor'		;
SEPARATOR	: 'separator'	;


// -----------------------------------
// Grammar specific fragments

fragment TmplAssign	: '::='		;
fragment LBang		: '<!'		;
fragment RBang		: '!>'		;
fragment LPct		: '<%'		;
fragment RPct		: '%>'		;
fragment LDAngle	: LShift	;
fragment RDAngle	: RShift	;

