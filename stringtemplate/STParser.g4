/*	[The "BSD license"]
 *	Copyright (c) 2011-2014 Terence Parr
 *	Copyright (c) 2015 Gerald Rosenberg
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
 *	Modified 2015.06.21 gbr
 *	-- use imported standard fragments
 */

parser grammar STParser;

options {
	language=Java;
	tokenVocab=STLexer;
}

template
	: elements EOF
	;

elements
	: element*
	;

element
	: singleElement
	| compoundElement
	;

singleElement
	: exprTag
	| TEXT+
	;

compoundElement
	: ifstat
	| region
	;

exprTag
	: LDELIM mapExpr ( SEMI exprOptions )? RDELIM
	;

region
	: LDELIM AT ID RDELIM
	  elements
	  LDELIM END RDELIM
	;

subtemplate
	: LBRACE ( ID ( COMMA  ID )* PIPE )? elements RBRACE
	;

ifstat
	: LDELIM IF LPAREN conditional RPAREN RDELIM
	  elements
	  ( LDELIM ELSEIF LPAREN conditional RPAREN RDELIM elements )*
	  ( LDELIM ELSE RDELIM elements )?
	  LDELIM ENDIF RDELIM
	;

conditional
	: andConditional ( OR andConditional )*
	;

andConditional
	: notConditional ( AND notConditional )*
	;

notConditional
	: BANG notConditional
	| memberExpr
	;

notConditionalExpr
	: ID	( DOT  ID
			| DOT LPAREN mapExpr RPAREN
			)*
	;

exprOptions
	: option ( COMMA option )*
	;

option
	: ID ( EQUALS expr )?
	;

expr
	: memberExpr ( COLON mapTemplateRef )?
	;

// more complicated than necessary to avoid backtracking,
// which ruins error handling
mapExpr
	: memberExpr ( ( COMMA memberExpr )+  COLON mapTemplateRef )?
	  ( COLON  mapTemplateRef ( COMMA mapTemplateRef )*  )*
	;

memberExpr
	: includeExpr
		( DOT ID
		| DOT LPAREN mapExpr RPAREN
		)*
	;

// expr:template(args) apply template to expr
// expr:{arg | ...} apply subtemplate to expr
// expr:(e)(args) convert e to a string template name and apply to expr
mapTemplateRef
	: ID LPAREN args? RPAREN
	| subtemplate
	| LPAREN mapExpr RPAREN LPAREN argExprList? RPAREN
	;

includeExpr
	: ID LPAREN mapExpr? RPAREN
	| SUPER DOT ID LPAREN args? RPAREN
	| ID LPAREN args? RPAREN
	| AT SUPER DOT ID LPAREN  RPAREN
	| AT ID LPAREN  RPAREN
	| primary
	;

primary
	: ID
	| STRING
	| TRUE
	| FALSE
	| subtemplate
	| list
	| LPAREN conditional RPAREN
	| LPAREN mapExpr RPAREN ( LPAREN argExprList? RPAREN )?
	;

list
	: LBRACK argExprList? RBRACK
	;

args
	: argExprList
	| namedArg ( COMMA namedArg )* ( COMMA ELLIPSIS )?
	| ELLIPSIS
	;

argExprList
	: expr ( COMMA expr )*
	;

namedArg
	: ID EQUALS expr
	;

