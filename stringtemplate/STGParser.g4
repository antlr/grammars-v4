/*	[The "BSD license"]
 *	Copyright (c) 2011-2014 Terence Parr
 *	Copyright (c) 2013-2015 Gerald Rosenberg
 *	All rights reserved.
 *
 *	Redistribution and use in source and binary forms, with or without
 *	modification, are permitted provided that the following conditions
 *	are met:
 *	1. Redistributions of source code must retain the above copyright
 *		notice, this list of conditions and the following disclaimer.
 *	2. Redistributions in binary form must reproduce the above copyright
 *		notice, this list of conditions and the following disclaimer in the
 *		documentation and/or other materials provided with the distribution.
 *	3. The name of the author may not be used to endorse or promote products
 *		derived from this software without specific prior written permission.
 *
 *	THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 *	IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 *	OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 *	IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 *	INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 *	NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 *	DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 *	THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 *	(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 *	THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

/*	Antlr grammar for StringTemplate v4.
 *
 *	Modified 2013.11.21 gbr
 *	-- updated
 *	Modified 2015.06.21 gbr
 *	-- updated to use imported standard fragments
 */

parser grammar STGParser;

options {
	language=Java;
	tokenVocab=STGLexer;
}

group
	: delimiters? imports?
	( template | dict )+
	EOF
	;

delimiters
	: DELIMITERS STRING COMMA STRING
	;

imports
	: ( IMPORT STRING )+
	;

template
	: ( AT ID DOT ID LPAREN RPAREN
	  | ID LPAREN formalArgs? RPAREN
	  )
		TMPL_ASSIGN
			( STRING			// "..."
			| BIGSTRING			// <<...>>
			| BIGSTRING_NO_NL	// <%...%>
			)
	| ID TMPL_ASSIGN ID			// alias one template to another
	;

formalArgs
	: formalArg ( COMMA formalArg )*
	;

formalArg
	: ID 	( ASSIGN STRING
			| ASSIGN ANON_TEMPLATE
			| ASSIGN TRUE
			| ASSIGN FALSE
			| ASSIGN LBRACK RBRACK
			)?
	;

dict
	: ID TMPL_ASSIGN LBRACK dictPairs RBRACK
	;

dictPairs
	: keyValuePair ( COMMA keyValuePair )* ( COMMA defaultValuePair )?
	| defaultValuePair
	;

keyValuePair		: STRING  COLON keyValue ;
defaultValuePair	: DEFAULT COLON keyValue ;

keyValue
	: BIGSTRING
	| BIGSTRING_NO_NL
	| ANON_TEMPLATE
	| STRING
	| TRUE
	| FALSE
	| LBRACK RBRACK
	| KEY
	;

