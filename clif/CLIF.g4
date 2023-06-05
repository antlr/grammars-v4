	
/*
 [The "BSD licence"]
 Copyright (c) 2015 Adam Taylor
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

/*
   Derived from ISO/IEC STANDARD 24707 First edition 2007-10-01
   http://standards.iso.org/ittf/PubliclyAvailableStandards/c039175_ISO_IEC_24707_2007%28E%29.zip
   
   with bug fixes from
   http://metadata-standards.org/Document-library/Documents-by-number/WG2-N1701-N1750/WG2N1703_24707-defect-report.pdf
    
   example clif ontologies available from COLORE
   http://stl.mie.utoronto.ca/colore/
 */

grammar CLIF;

//A.2.3.1 Term sequence
termseq
	:	(term | SEQMARK)*
	;

//A.2.3.2 Name
interpretedname
	:	NUMERAL
	|	QUOTEDSTRING
	;
	
interpretablename
	:	NAMECHARSEQUENCE
	|	ENCLOSEDNAME
	;	
	
name
	:	interpretedname
	|	interpretablename
	;
	
	
//A.2.3.3 Term
term
	:	name
	|	OPEN operator_ termseq CLOSE
	|	OPEN 'cl-comment' QUOTEDSTRING term CLOSE
	;
	
operator_
	:	term
	;
	

//A.2.3.4 Equation
equation
	:	OPEN '=' term term CLOSE
	;
	
	
//A.2.3.5 Sentence
sentence
	:	atomsent
	|	boolsent
	|	quantsent
	|	commentsent
	;


//A.2.3.6 Atomic sentence
atomsent
	:	equation
	|	atom
	;
	
atom
	:	OPEN predicate termseq CLOSE
	|	OPEN term OPEN 'cl-roleset' OPEN name term CLOSE CLOSE CLOSE
	; 
	
predicate
	:	term	
	;


//A.2.3.7 Boolean sentence
boolsent
	:	OPEN ('and' | 'or') sentence* CLOSE
	|	OPEN ('if' | 'iff') sentence sentence CLOSE
	|	OPEN 'not' sentence CLOSE
	;


//A.2.3.8 Quantified sentence
quantsent
	:	OPEN ('forall' | 'exists') interpretablename? boundlist sentence CLOSE
	;
	
boundlist
	:	OPEN 
		(	interpretablename 
		|	SEQMARK 
		|	OPEN (interpretablename | SEQMARK) term CLOSE 
		)* 
		CLOSE
	;

//A.2.3.9 Commented sentence
commentsent
	:	OPEN 'cl-comment' ENCLOSEDNAME sentence CLOSE
	;


//A.2.3.10 Module
module
	:	OPEN 'cl-module' interpretablename (OPEN 'cl-excludes' name* CLOSE)? cltext? CLOSE
	;


//A.2.3.11 Phrase
phrase
	:	sentence
	|	module
	|	OPEN 'cl-imports' interpretablename CLOSE
	|	OPEN 'cl-comment' ENCLOSEDNAME cltext? CLOSE
	;

text
	:	phrase+
	;
	
cltext
	:	module
	|	namedtext
	|	text
	;
	
namedtext
	:	OPEN 'cl-text' interpretablename text? CLOSE ;


//A.2.2.2 Delimiters
OPEN			: '(';
CLOSE			: ')'; 
STRINGQUOTE	: '\''; 
NAMEQUOTE	: '"'; 
BACKSLASH	: '\\';


//A.2.2.3 Characters
fragment
CHAR			: [0-9~!#$%^&*_+{}|:<>?`\-=[\];,./A-Za-z];

fragment
DIGIT			: [0-9];

fragment
HEXA			: [0-9A-Fa-f];


//A.2.2.4 Quoting within strings
fragment
NONASCII		
	: '\\' 'u' HEXA HEXA HEXA HEXA 
	| '\\' 'U' HEXA HEXA HEXA HEXA HEXA HEXA
	;
	
fragment
INNERSTRINGQUOTE	: '\'' ;

fragment
INNERNAMEQUOTE		: '"' ;

fragment
INNERBACKSLASH		: '\\';

NUMERAL				: DIGIT+;
SEQMARK				: '...' CHAR*;


//A.2.2.5 Quoted strings
QUOTEDSTRING : STRINGQUOTE (WHITE | OPEN | CLOSE | CHAR | NONASCII | NAMEQUOTE | INNERSTRINGQUOTE | INNERBACKSLASH )* STRINGQUOTE ;
ENCLOSEDNAME : NAMEQUOTE (WHITE | OPEN | CLOSE | CHAR | NONASCII | STRINGQUOTE | INNERNAMEQUOTE )* NAMEQUOTE ;


//A.2.2.6 Reserved tokens
EQUAL					:	'=';
AND					:	'and';
OR						:	'or';
IFF						:	'iff';
IF						:	'if';
FORALL				:	'forall';
EXISTS					:	'exists';
NOT					:	'not';
CL_ROLESET			:	'cl-roleset';
CL_TEXT				:	'cl-text';
CL_IMPORTS			:	'cl-imports';
CL_EXCLUDES			:	'cl-excludes';
CL_MODULE			:	'cl-module';
CL_COMMENT			:	'cl-comment';
CL_PREFIX				:	'cl-prefix';


//A.2.2.7 Name character sequence
NAMECHARSEQUENCE
	:	CHAR  (CHAR | STRINGQUOTE | NAMEQUOTE | BACKSLASH)*
	;


// A.2.2.1 White space
WHITE
	:	[ \t\n\r\u000B]							-> skip
	;
	
BLOCKCOMMENT 
	:	'/*' (BLOCKCOMMENT | .)*? '*/' 	->	skip // nesting allowed (but should it be?)
	; 
	
LineComment
	:	'//' ~[\u000A\u000D]*			->	skip
	;


