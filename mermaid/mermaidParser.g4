/*
 BSD License

 Copyright (c) 2026, Tom Everett All rights reserved.

 Redistribution and use in source and binary forms, with or without modification, are permitted
 provided that the following conditions are met:

 1. Redistributions of source code must retain the above copyright notice, this list of conditions
 and the following disclaimer. 2. Redistributions in binary form must reproduce the above copyright
 notice, this list of conditions and the following disclaimer in the documentation and/or other
 materials provided with the distribution. 3. Neither the name of Tom Everett nor the names of its
 contributors may be used to endorse or promote products derived from this software without specific
 prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR
 IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
 FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR
 CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER
 IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

parser grammar mermaidParser;
options { tokenVocab = mermaidLexer; }

mermaid: diagram+ EOF;

diagram: erdiagram | classdiagram;

erdiagram: ER_DIAGRAM ER_NL* ((erentity | entityattributes | entityname) (ER_NL+ | EOF))*;

erentity:
	entityname relationship entityname ER_COLON relationshiplabel;

entityattributes: entityname ER_LBRACE ER_NL* (entityattribute ER_NL+)* ER_RBRACE;

entityattribute: entityattributetype entityattributename (ER_KEY (ER_COMMA ER_KEY)*)? ER_STRING?;

entityattributetype: ER_INTTYPE | ER_STRINGTYPE | ER_FLOATTYPE | ER_NAME;

entityattributename: entityname;

relationshiplabel: entityname+ | ER_STRING;

relationship: ER_CARDINALITY ER_LINETYPE ER_CARDINALITY;
entityname: ER_NAME | ER_STRING | ER_INTTYPE | ER_STRINGTYPE | ER_FLOATTYPE | ER_KEY;

classdiagram: CLASS_DIAGRAM CLASS_NL* (classstatement (CLASS_NL+ | EOF))*;

classstatement
    : CLASS CLASS_NAME classbody?
    | CLASS_NAME CLASS_MEMBER
    | CLASS_NAME CLASS_STRING? CLASS_RELATION CLASS_STRING? CLASS_NAME CLASS_MEMBER?
    ;

classbody: CLASS_OPEN (CLASS_BODY_LINE | CLASS_BODY_NL)* CLASS_CLOSE;
