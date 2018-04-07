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


parser grammar ZOperatorParser;
options { tokenVocab=ZLexer; }

specification
	: (TEXT | section | paragraph)* EOF
	;

section
	: ZED SECTION NAME PARENTS formals? END paragraph* #InheritingSection
	| ZED SECTION NAME END paragraph* #BaseSection
	;
	
paragraph
	: ZED operatorTemplate END #OperatorTemplateParagraph
	| AX .*? END #AxiomaticDescriptionParagraph
	| SCH .*? END #SchemaDefinitionParagraph
	| ZED .*? END #NONOperatorTemplateParagraph
	;

formals
	: NAME (COMMA NAME)*
	;

operatorTemplate
	: RELATION template #RelationOperatorTemplate
	| FUNCTION categoryTemplate #FunctionOperatorTemplate
	| GENERIC categoryTemplate #GenericOperatorTemplate
	;

categoryTemplate
	: prefixTemplate
	| postfixTemplate
	| prec assoc infixTemplate
	| nofixTemplate
	;

prec
	: NUMERAL
	;
	
assoc
	: LEFTASSOC
	| RIGHTASSOC
	;

template
	: prefixTemplate
	| postfixTemplate
	| infixTemplate
	| nofixTemplate
	;

prefixTemplate
	: LEFT_PARENTHESIS (prefixName | POWERSET ARGUMENT) RIGHT_PARENTHESIS
	;

postfixTemplate
	: LEFT_PARENTHESIS postfixName RIGHT_PARENTHESIS
	;

infixTemplate
	: LEFT_PARENTHESIS infixName RIGHT_PARENTHESIS
	;

nofixTemplate
	: LEFT_PARENTHESIS nofixName RIGHT_PARENTHESIS
	;
	
optArgName
	: ARGUMENT NAME
	;
	
optListName
	: LIST NAME
	;
	
argName
	: ARGUMENT NAME
	;
	
listName
	: LIST NAME
	;

prefixName 
	: NAME ARGUMENT
	| NAME (optArgName | optListName)* (argName | listName) ARGUMENT
	;
	
postfixName 
	: ARGUMENT NAME
	| ARGUMENT NAME (optArgName | optListName)* (argName | listName)
	;
	
infixName 
	: ARGUMENT NAME ARGUMENT
	| ARGUMENT NAME (optArgName | optListName)* (argName | listName) ARGUMENT
	;
	
nofixName 
	: NAME (optArgName | optListName)* (argName | listName)
	;

