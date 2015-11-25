
/*
 * The Apache Software License, Version 1.1
 *
 * Copyright (c) 2000-2003 Lucas Bruand.  All rights
 * reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * 3. The end-user documentation included with the redistribution, if
 *    any, must include the following acknowlegement:
 *       "This product includes software developed by the
 *        Apache Software Foundation (http://www.apache.org/)."
 *    Alternately, this acknowlegement may appear in the software itself,
 *    if and wherever such third-party acknowlegements normally appear.
 *
 * 4. The names "jviewcvs" and "Apache Software
 *    Foundation" must not be used to endorse or promote products derived
 *    from this software without prior written permission. For written
 *    permission, please contact apache@apache.org.
 *
 * 5. Products derived from this software may not be called "Apache"
 *    nor may "Apache" appear in their names without prior written
 *    permission of the Apache Group.
 *
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND ANY EXPRESSED OR IMPLIED
 * WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL THE APACHE SOFTWARE FOUNDATION OR
 * ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 * ====================================================================
 *
 * This software consists of voluntary contributions made by many
 * individuals on behalf of the Apache Software Foundation.  For more
 * information on the Apache Software Foundation, please see
 * <http://www.apache.org/>.
 */
/*
 *  Original description of rcsfile can be found: http://www.daemon-systems.org/man/rcsfile.5.html
 */
//----------------------------------------------------------------------------
// The JRCS parser
//----------------------------------------------------------------------------
grammar RCS;

rcstext
   : admin deltalist desc deltatextlist EOF
   ;

rcsheader
   : admin
   ;

rcsrevisions
   : admin deltalist
   ;

admin
   : head ( branch )? access symbols ( locks )? ( strict )? ( comment )? ( expand )? ( newphrase )*
   ;

head
   : LITERAL_HEAD REVISION SEMI
   ;

branch
   : LITERAL_BRANCH BRANCH SEMI
   ;

access
   : LITERAL_ACCESS ( IDENT )* SEMI
   ;

symbols
   : LITERAL_SYMBOLS ( tags )* SEMI
   ;

tags
   : IDENT COLON REVISION
   ;

locks
   : LITERAL_LOCKS ( IDENT )* SEMI
   ;

strict
   : LITERAL_STRICT SEMI
   ;

comment
   : LITERAL_COMMENT ( STRING )? SEMI
   ;

expand
   : LITERAL_EXPAND ( STRING )? SEMI
   ;

deltalist
   : ( delta )*
   ;

delta
   : REVISION delta_date delta_author delta_state delta_branches delta_next ( newphrase )*
   ;

delta_date
   : LITERAL_DATE REVISION SEMI
   ;

delta_author
   : LITERAL_AUTHOR IDENT SEMI
   ;

delta_state
   : LITERAL_STATE IDENT SEMI
   ;

delta_branches
   : LITERAL_BRANCHES ( REVISION )* SEMI
   ;

delta_next
   : LITERAL_NEXT ( REVISION )? SEMI
   ;

desc
   : LITERAL_DESC STRING
   ;

deltatextlist
   : ( deltatext )*
   ;

deltatext
   : REVISION deltatext_log ( newphrase )* deltatext_text
   ;

deltatext_log
   : LITERAL_LOG STRING
   ;

deltatext_text
   : LITERAL_TEXT STRING
   ;

newphrase
   : ( IDENT )+ SEMI
   ;


COMMA
   : 'COMMA'
   ;


BRANCH
   : 'BRANCH'
   ;


LOGS
   : 'LOGS'
   ;


ADMIN
   : 'ADMIN'
   ;


DELTAS
   : 'DELTAS'
   ;


LITERAL_HEAD
   : 'head'
   ;


LITERAL_BRANCH
   : 'branch'
   ;


LITERAL_ACCESS
   : 'access'
   ;


LITERAL_SYMBOLS
   : 'symbols'
   ;


LITERAL_LOCKS
   : 'locks'
   ;


LITERAL_STRICT
   : 'strict'
   ;


LITERAL_COMMENT
   : 'comment'
   ;


LITERAL_EXPAND
   : 'expand'
   ;


LITERAL_DATE
   : 'date'
   ;


LITERAL_AUTHOR
   : 'author'
   ;


LITERAL_STATE
   : 'state'
   ;


LITERAL_BRANCHES
   : 'branches'
   ;


LITERAL_NEXT
   : 'next'
   ;


LITERAL_DESC
   : 'desc'
   ;


LITERAL_LOG
   : 'log'
   ;


LITERAL_TEXT
   : 'text'
   ;

// an identifier.  Note that testLiterals is set to true!  This means
// that after we match the rule, we look in the literals table to see
// if it's a literal or really an identifer

IDENT
   : ( ~ ( '$' | ',' | '.' | ':' | ';' | '@' | ' ' | '\t' | '\n' | '\r' | '\f' ) )+
   ;


INT
   : ( '0' .. '9' )+
   ;


REVISION
   : INT '.' INT ( '.' INT '.' INT )*
   ;

// string literals

STRING
   : '@' (
   /*	'\r' '\n' can be matched in one alternative or by matching
				'\r' in one iteration and '\n' in another.  I am trying to
				handle any flavor of newline that comes in, but the language
				that allows both "\r\n" and "\r" and "\n" to all be valid
				newline is ambiguous.  Consequently, the resulting grammar
				must be ambiguous.  I'm shutting this warning off.
			 */
   /*			options {
				generateAmbigWarnings=false;
			}
		:*/
   //{ LA(2)!='@' }?  '@' '@' | ( '\r\n' | '\r' | '\n' ) | ~ ( '@' | '\n' | '\r' ) )* '@'
   )
   ;


SEMI
   : ';'
   ;


COLON
   : ':'
   ;

// Whitespace -- ignored

WS
   : ( ' ' | '\t' | '\f' | ( '\r\n' | '\r' | '\n' ) )+ -> skip
   ;
