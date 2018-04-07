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


parser grammar ZParser;
options { tokenVocab=ZLexer; }

specification
	: (TEXT | section | paragraph)* EOF
	;

section
	: ZED SECTION NAME PARENTS formals? END paragraph* #InheritingSection
	| ZED SECTION NAME END paragraph* #BaseSection
	;
	
paragraph
	: ZED LEFT_SQUARE_BRACKET formals RIGHT_SQUARE_BRACKET NL? END #GivenTypesParagraph
	| AX schemaText END #AxiomaticDescriptionParagraph
	| SCH NAME NL? schemaText END #SchemaDefinitionParagraph
	| AX GEN LEFT_SQUARE_BRACKET formals RIGHT_SQUARE_BRACKET NL? schemaText END #GenericAxiomaticDescriptionParagraph
	| SCH GEN NAME LEFT_SQUARE_BRACKET formals RIGHT_SQUARE_BRACKET NL? schemaText END #GenericSchemaDefinitionParagraph
	| ZED NL? declNameExpression END #HorizontalDefinitionParagraph
	| ZED declName LEFT_SQUARE_BRACKET formals RIGHT_SQUARE_BRACKET DEFINE_EQUAL expression END #GenericHorizontalDefinitionParagraph
	| ZED genName DEFINE_EQUAL expression END #GenericOperatorDefinitionParagraph
	| ZED freetype (AMPERSAND freetype)* END #FreeTypesParagraph
	| ZED CONJECTURE predicate END #ConjectureParagraph
	| ZED LEFT_SQUARE_BRACKET formals RIGHT_SQUARE_BRACKET CONJECTURE predicate END #GenericConjectureParagraph
	| ZED operatorTemplate END #OperatorTemplateParagraph
	;

freetype
	: NAME FREE_EQUALS branch (VERTICAL_LINE branch)*
	;
	
branch
	: declName (LEFT_DOUBLE_ANGLE_BRACKET expression RIGHT_DOUBLE_ANGLE_BRACKET)?
	;
formals
	: NAME (COMMA NAME)*
	;

predicate
	: predicate NL predicate #NewlineConjunctionPredicate
	| predicate SEMICOLON predicate #SemicolonConjunctionPredicate
	| FOR_ALL schemaText SPOT predicate #UniversalQuantificationPredicate
	| THERE_EXISTS schemaText SPOT predicate #ExistentialQuantificationPredicate
	| UNIQUE_EXISTS schemaText SPOT predicate #UniqueExistentialQuantificationPredicate
	| predicate LEFT_RIGHT_DOUBLE_ARROW predicate #EquivalencePredicate
	| predicate RIGHTWARDS_DOUBLE_ARROW predicate #ImplicationPredicate
	| predicate LOGICAL_OR predicate #DisjunctionPredicate
	| predicate LOGICAL_AND predicate #ConjunctionPredicate
	| NOT_SIGN predicate #NegationPredicate
	| relation #RelationOperatorApplicationPredicate
	| expression #SchemaPredicatePredicate
	| TRUE #TruthPredicate
	| FALSE #FalsityPredicate
	| LEFT_PARENTHESIS predicate RIGHT_PARENTHESIS #ParenthesizedPredicate
	;

expression
	: FOR_ALL schemaText SPOT expression #SchemaUniversalQuantificationExpression
	| THERE_EXISTS schemaText SPOT expression #SchemaExistentialQuantificationExpression
	| UNIQUE_EXISTS schemaText SPOT expression #SchemaUniqueExistentialQuantificationExpression
	| GREEK_SMALL_LETTER_LAMBDA schemaText SPOT expression #FunctionConstructionExpression
	| GREEK_SMALL_LETTER_MU schemaText SPOT expression #DefiniteDescriptionExpression
	| LET declNameExpression (SEMICOLON declNameExpression)* SPOT expression #SubstitutionExpressionExpression
	| expression LEFT_RIGHT_DOUBLE_ARROW expression #SchemaEquivalenceExpression
	| expression RIGHTWARDS_DOUBLE_ARROW expression #SchemaImplicationExpression
	| expression LOGICAL_OR expression #SchemaDisjunctionExpression
	| expression LOGICAL_AND expression #SchemaConjunctionExpression
	| NOT_SIGN expression #SchemaNegationExpression
	| IF predicate THEN expression ELSE expression #ConditionalExpression
	| expression SCHEMA_COMPOSITION expression #SchemaCompositionExpression
	| expression SCHEMA_PIPING expression #SchemaPipingExpression
	| expression REVERSE_SOLIDUS LEFT_PARENTHESIS declName (COMMA declName)* RIGHT_PARENTHESIS #SchemaHidingExpression
	| expression SCHEMA_PROJECTION expression #SchemaProjectionExpression
	| PRE_KEY expression #SchemaPreconditionExpression
	| expression (MULTIPLICATION_SIGN expression)+ #CartesianProductExpression
	| POWERSET expression #PowersetExpression
	| PRE expression #PrefixApplicationExpression
	| L expSep? (expression ERE | expressionList? SRE) expression #GenericPrefixApplicationExpression
	| expression POST #PostfixApplicationExpression
	| expression EL expSep? (expression ER | expressionList? SR) #GenericPostfixApplicationExpression
	| expression {ZSupport.isLeftAssociative(_input)}? I expression #InfixLeftApplicationExpression
	| <assoc=right> expression I expression #InfixRightApplicationExpression
	| expression EL expSep? (expression ERE | expressionList? SRE) expression #GenericInfixApplicationExpression
	| L expSep? (expression ER | expressionList? SR) #NofixApplicationExpression
	| expression expression #ApplicationExpression
	| expression STROKE #SchemaDecorationExpression
	| expression LEFT_SQUARE_BRACKET declName SOLIDUS declName (COMMA declName SOLIDUS declName)* RIGHT_SQUARE_BRACKET #SchemaRenamingExpression
	| expression FULL_STOP refName #BindingSelectionExpression
	| expression FULL_STOP NUMERAL #TupleSelectionExpression
	| GREEK_SMALL_LETTER_THETA expression STROKE* #BindingConstructionExpression
	| refName #ReferenceExpression
	| refName LEFT_SQUARE_BRACKET expressionList? RIGHT_SQUARE_BRACKET #GenericInstantiationExpression
	| NUMERAL #NumberLiteralExpression
	| LEFT_CURLY_BRACKET expressionList? RIGHT_CURLY_BRACKET #SetExtensionExpression
	| LEFT_CURLY_BRACKET schemaText SPOT expression RIGHT_CURLY_BRACKET #SetComprehensionExpression
	| LEFT_CURLY_BRACKET schemaText RIGHT_CURLY_BRACKET #CharacteristicSetComprehensionExpression
	| LEFT_SQUARE_BRACKET schemaText RIGHT_SQUARE_BRACKET #SchemaConstructionExpression
	| LEFT_BINDING_BRACKET (declNameExpression (COMMA declNameExpression)* )? RIGHT_BINDING_BRACKET #BindingExtensionExpression
	| LEFT_PARENTHESIS expression (COMMA expression)+ RIGHT_PARENTHESIS #TupleExtensionExpression
	| LEFT_PARENTHESIS GREEK_SMALL_LETTER_MU schemaText RIGHT_PARENTHESIS #CharacteristicDefiniteDescriptionExpression
	| LEFT_PARENTHESIS expression RIGHT_PARENTHESIS #ParenthesizedExpression
	;

schemaText
	: NL? declPart? NL? (VERTICAL_LINE NL? predicate NL?)? NL?
	;
	
declPart
	: declaration ((SEMICOLON | NL) declaration)*
	;
	
declNameExpression
	: declName DEFINE_EQUAL expression NL?
	;
	
declaration
	: declName (COMMA declName)* COLON expression
	| declNameExpression
	| expression
	;
	
operatorTemplate
	: RELATION template
	| FUNCTION categoryTemplate
	| GENERIC categoryTemplate
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

declName
	: NAME
	| opName
	;

refName
	: NAME
	| LEFT_PARENTHESIS opName RIGHT_PARENTHESIS
	;

opName
	: prefixName
	| postfixName
	| infixName
	| nofixName
	;

prefixName 
	: PRE ARGUMENT
	| PREP ARGUMENT
	| L (ARGUMENT ES | LIST SS)* (ARGUMENT ERE | LIST SRE) ARGUMENT
	| LP (ARGUMENT ES | LIST SS)* (ARGUMENT EREP | LIST SREP) ARGUMENT
	;
	
postfixName 
	: ARGUMENT POST
	| ARGUMENT POSTP
	| ARGUMENT EL (ARGUMENT ES | LIST SS)* (ARGUMENT ER | LIST SR)
	| ARGUMENT ELP (ARGUMENT ES | LIST SS)* (ARGUMENT ERP | LIST SRP)
	;
	
infixName 
	: ARGUMENT I ARGUMENT
	| ARGUMENT IP ARGUMENT
	| ARGUMENT EL (ARGUMENT ES | LIST SS)* (ARGUMENT ERE | LIST SRE) ARGUMENT
	| ARGUMENT ELP (ARGUMENT ES | LIST SS)* (ARGUMENT EREP | LIST SREP) ARGUMENT
	;
	
nofixName 
	: L (ARGUMENT ES | LIST SS)* (ARGUMENT ER | LIST SR)
	| LP (ARGUMENT ES | LIST SS)* (ARGUMENT ERP | LIST SRP)
	;
	
genName 
	: prefixGenName
	| postfixGenName
	| infixGenName
	| nofixGenName
	;
	
prefixGenName
	: PRE NAME
	| L (NAME (ES | SS))* NAME (ERE | SRE) NAME
	;
	
postfixGenName 
	: NAME POST
	| NAME EL (NAME (ES | SS))* NAME (ER | SR)
	;
	
infixGenName
	: NAME I NAME
	| NAME EL (NAME (ES | SS))* NAME (ERE | SRE) NAME
	;
	
nofixGenName 
	:  L (NAME (ES | SS))* NAME (ER | SR)
	;
	
relation 
	: prefixRel
	| postfixRel
	| infixRel
	| nofixRel
	;
	
prefixRel 
	: PREP expression
	| LP expSep? (expression EREP | expressionList? SREP) expression
	;
	
postfixRel
	: expression POSTP
	| expression ELP expSep? (expression ERP | expressionList? SRP)
	;

infixRel
//	: expression ((ELEMENT_OF | EQUALS_SIGN | IP) expression)+
//	| expression ELP expSep? (expression EREP | expressionList? SREP) expression
	: expression {ZSupport.isLeftAssociative(_input)}? ((ELEMENT_OF | EQUALS_SIGN | IP) expression)+
	| <assoc=right> expression ((ELEMENT_OF | EQUALS_SIGN | IP) expression)+
	| expression {ZSupport.isLeftAssociative(_input)}? ELP expSep? (expression EREP | expressionList? SREP) expression
	| <assoc=right> expression ELP expSep? (expression EREP | expressionList? SREP) expression
	;
	
nofixRel
	: LP expSep? (expression ERP | expressionList? SRP)
	;
	
application
	: prefixApp
	| postfixApp
	| infixApp
	| nofixApp
	;
	
prefixApp
	: PRE expression
	| L expSep? (expression ERE | expressionList? SRE) expression
	;

postfixApp
	: expression POST
	| expression EL expSep? (expression ER | expressionList? SR)
	;

infixApp
	: expression I expression
	| expression EL expSep? (expression ERE | expressionList? SRE) expression
	;

nofixApp
	: L expSep? (expression ER | expressionList? SR)
	;
	
expSep
	: (expression ES | expressionList? SS)+
	;

expressionList 
	:  expression (COMMA expression)*
	;
