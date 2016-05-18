/* [The "BSD license"]
 * Copyright (c) 2011-2014 Terence Parr
 * Copyright (c) 2015 Gerald Rosenberg
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 	 1. Redistributions of source code must retain the above copyright
 *	 	notice, this list of conditions and the following disclaimer.
 *   2. Redistributions in binary form must reproduce the above copyright
 *		notice, this list of conditions and the following disclaimer in the
 *		documentation and/or other materials provided with the distribution.
 *	 3. The name of the author may not be used to endorse or promote products
 *		derived from this software without specific prior written permission.
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

/*	Antlr grammar for StringTemplate v4.
 *
 *	Modified 2015.06.16 gbr
 *	-- update for compatibility with Antlr v4.5
 *	-- use imported standard fragments
 */

lexer grammar STLexer;

options {
	superClass = LexerAdaptor ;
}

import LexBasic;	// Standard set of fragments

channels {
	OFF_CHANNEL		// non-default channel for whitespace and comments
}

// -----------------------------------
// default mode = Outside


DOC_COMMENT		: DocComment		-> channel(OFF_CHANNEL)	;
BLOCK_COMMENT	: BlockComment		-> channel(OFF_CHANNEL)	;
LINE_COMMENT	: LineComment		-> channel(OFF_CHANNEL)	;

TMPL_COMMENT	: TmplComment		-> channel(OFF_CHANNEL)	;

HORZ_WS			: Hws+				-> channel(OFF_CHANNEL)	;
VERT_WS			: Vws+				-> channel(OFF_CHANNEL)	;

ESCAPE			: . { isLDelim() }? EscSeq . { isRDelim() }?	; // self contained
LDELIM			: . { isLDelim() }?	-> mode(Inside)				; // switch mode to inside
RBRACE			: RBrace { endsSubTemplate(); }					; // conditional switch to inside

TEXT		: . { adjText(); }	;  // have to handle weird terminals


// -----------------------------------
mode Inside ;

INS_HORZ_WS	: Hws+		-> type(HORZ_WS), channel(OFF_CHANNEL)	;
INS_VERT_WS	: Vws+		-> type(VERT_WS), channel(OFF_CHANNEL)	;

LBRACE		: LBrace { startsSubTemplate() }?	-> mode(SubTemplate)	;
RDELIM		: . { isRDelim() }?					-> mode(DEFAULT_MODE)	;

STRING		: DQuoteLiteral	;

IF			: 'if'			;
ELSEIF		: 'elseif'		;
ELSE		: 'else'		;
ENDIF		: 'endif'		;
SUPER		: 'super'		;
END			: '@end'		;

TRUE		: True			;
FALSE		: False			;

AT			: At			;
ELLIPSIS	: Ellipsis		;
DOT			: Dot			;
COMMA		: Comma			;
COLON		: Colon			;
SEMI		: Semi			;
AND			: And			;
OR			: Or			;
LPAREN		: LParen		;
RPAREN		: RParen		;
LBRACK		: LBrack		;
RBRACK		: RBrack		;
EQUALS		: Equal			;
BANG		: Bang			;


// -----------------------------------
// Unknown content in mode Inside

ERR_CHAR	: .	-> skip		;


// -----------------------------------
mode SubTemplate ;

SUB_HORZ_WS		: Hws+		-> type(HORZ_WS), channel(OFF_CHANNEL)	;
SUB_VERT_WS		: Vws+		-> type(VERT_WS), channel(OFF_CHANNEL)	;

ID				: NameStartChar NameChar*			;
SUB_COMMA		: Comma		-> type(COMMA)			;
PIPE			: Pipe		-> mode(DEFAULT_MODE)	;


// -----------------------------------
// Grammar specific fragments

fragment TmplComment	: LTmplMark .*? RTmplMark		;

fragment LTmplMark		: . { isLTmplComment() }? Bang	;
fragment RTmplMark		: Bang . { isRTmplComment() }?	;

