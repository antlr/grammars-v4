/*
 [The "BSD licence"]
 Copyright (c) 2017 Anand Tamariya
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

grammar Upnp;
/*
 * Parser Rules
 */
 
searchCrit : (searchExp | ASTERISK) EOF;
searchExp : relExp | searchExp WCHAR+ LOGOP WCHAR+ searchExp | '(' WCHAR* searchExp WCHAR* ')'  ;
relExp : PROPERTY WCHAR+ BINOP WCHAR+ quotedVal | PROPERTY WCHAR+ EXISTSOP WCHAR+ BOOLVAL ;
quotedVal : DQUOTE escapedQuote DQUOTE ;
escapedQuote :   STRING_LITERAL* WCHAR* STRING_LITERAL*;
 
/*
 * Lexer Rules
 */
 
NUMBER     : [0-9]+ ;
 
WHITESPACE : [\r\t\n] -> skip ;

LOGOP : 'and' | 'or' ;
BINOP : RELOP | STRINGOP ;
RELOP : '=' | '!=' | '<' | '<=' | '>' | '>=' ;
STRINGOP : 'contains' | 'doesnotcontain' | 'derivedfrom' ;
EXISTSOP : 'exists' ;
BOOLVAL : 'true' | 'false' ;
WCHAR : SPACE | HTAB ;
PROPERTY	: 'res@resolution'
				| 'res@duration'
				| 'dc:title'
				| 'dc:creator'
				| 'upnp:actor'
				| 'upnp:artist'
				| 'upnp:genre'
				| 'upnp:album'
				| 'dc:date'
				| 'upnp:class'
				| '@id'
				| '@refID'
				| '@protocolInfo'
				| 'upnp:author'
				| 'dc:description'
				| 'pv:avKeywords'
				| 'pv:rating'
				| 'upnp:seriesTitle'
				| 'upnp:episodeNumber'
				| 'upnp:director'
				| 'upnp:rating'
				| 'upnp:channelNr'
				| 'upnp:channelName'
				| 'upnp:longDescription'
				| 'pv:capturedate'
				| 'pv:custom' ;
HTAB 		 :	 '\t' ;
SPACE        :   ' '  ;      
DQUOTE       :   '"' ;    
ASTERISK     :   '*'  ;
STRING_LITERAL : [a-zA-Z.] | '\\"';  