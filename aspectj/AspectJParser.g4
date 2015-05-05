	
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
   Derived from
   https://eclipse.org/aspectj/doc/next/quick5.pdf
   https://eclipse.org/aspectj/doc/next/progguide/starting.html
   https://eclipse.org/aspectj/doc/next/adk15notebook/grammar.html
 */
 
 /*
  	This grammar builds on top of the ANTLR4 Java grammar, but it uses 
  	lexical modes to lex the annotation form of AspectJ; hence in order to use it
  	you need to break Java.g4 into Separate Lexer (JavaLexer.g4) and Parser (JavaParser.g4) grammars.
  */


parser grammar AspectJParser;

options { tokenVocab=AspectJLexer; }

import JavaParser;

typeDeclaration
    :   classOrInterfaceModifier* classDeclaration
    |   classOrInterfaceModifier* enumDeclaration
    |   classOrInterfaceModifier* interfaceDeclaration
    |   classOrInterfaceModifier* annotationTypeDeclaration
    |	classOrInterfaceModifier* aspectDeclaration
    |   ';'
    ;
    
aspectBody
	:	'{' aspectBodyDeclaration* '}'
	;

classBodyDeclaration
    :   ';'
    |   'static'? block
    |   modifier* memberDeclaration
    |	'static' aspectDeclaration 
    ;
    
aspectBodyDeclaration
	:	classBodyDeclaration
	|	advice
	|	interTypeMemberDeclaration
    |	interTypeDeclaration
	;

memberDeclaration
    :   methodDeclaration
    |   genericMethodDeclaration
    |   fieldDeclaration
    |   constructorDeclaration
    |   genericConstructorDeclaration
    |   interfaceDeclaration
    |   annotationTypeDeclaration
    |   classDeclaration
    |   enumDeclaration
    |	pointcutDeclaration
    ;
    
// ANNOTATIONS

annotation
    :	'@' annotationName ( '(' ( elementValuePairs | elementValue )? ')' )?
    |	'@' 'After' '(' '"' pointcutExpression '"' ')'
    |	'@' 'AfterReturning' '(' '"' pointcutExpression '"' ')'
    |	'@' 'AfterReturning' '(' 'pointcut' '=' '"' pointcutExpression '"' ',' 'returning' '=' '"' id '"' ')'
    |	'@' 'AfterThrowing' '(' '"' pointcutExpression '"' ')'
    |	'@' 'Around' '(' '"' pointcutExpression '"' ')'
	|	'@' 'Aspect' ( '(' '"' perClause  '"' ')' )?
    |	'@' 'Before' '(' '"' pointcutExpression '"' ')'
    |	'@' 'DeclareError' '(' '"' pointcutExpression '"' ')'
    |	'@' 'DeclareMixin' '(' 'value' '=' '"' typePattern '"' ',' 'interfaces' '=' '{' classPatternList '}' ')'
    |	'@' 'DeclareParents' '(' '"' typePattern '"' ')'
    |	'@' 'DeclareParents' '(' 'value' '=' '"' typePattern '"' ',' 'defaultImpl' '=' classPattern ')'
    |	'@' 'DeclarePrecedence'  '(' '"' typePatternList '"' ')'
    |	'@' 'DeclareWarning' '(' '"' pointcutExpression '"' ')'
    |	'@' 'Pointcut' '(' '"' pointcutExpression? '"' ')'
    ;
    	
classPattern
	:	id ('.' id)* '.' 'class'
	;

classPatternList
	:	classPattern (',' classPattern)*
	;
	
    
aspectDeclaration
	:	'privileged'? modifier* 'aspect' id 
		('extends' type)? 
		('implements' typeList)? 
		perClause? 
		aspectBody
	;
	
advice
	:	'strictfp'? adviceSpec ('throws' typeList)? ':' pointcutExpression methodBody
	;

adviceSpec
	:	'before' formalParameters
	|	'after' formalParameters
	|	'after' formalParameters 'returning' ('(' formalParameter? ')')? 
	|	'after' formalParameters 'throwing' ('(' formalParameter? ')')? 
	|	(type | 'void') 'around' formalParameters
	;

perClause
	:	'pertarget' '(' pointcutExpression ')'
	|	'perthis' '(' pointcutExpression ')'
	|	'percflow' '(' pointcutExpression ')' 
	|	'percflowbelow' '(' pointcutExpression ')' 
	|	'pertypewithin' '(' typePattern ')' 
	|	'issingleton' '(' ')'
	;
	
pointcutDeclaration
	:	'abstract' modifier* 'pointcut' id formalParameters ';'
	|	modifier* 'pointcut' id formalParameters ':' pointcutExpression ';'
 	;
	
pointcutExpression
	:	(pointcutPrimitive | referencePointcut)
	|	'!' pointcutExpression
	|	'(' pointcutExpression ')'
	|	pointcutExpression '&&' pointcutExpression
	|	pointcutExpression '||' pointcutExpression 
	;
	
pointcutPrimitive
	:	'call' '(' methodOrConstructorPattern ')'					#CallPointcut
	|	'execution' '(' methodOrConstructorPattern ')'			#ExecutionPointcut
	|	'initialization' '(' constructorPattern ')'					#InitializationPointcut
	|	'preinitialization' '(' constructorPattern ')'              	#PreInitializationPointcut
	|	'staticinitialization' '(' optionalParensTypePattern ')'		#StaticInitializationPointcut
	|	'get' '(' fieldPattern ')'									#GetPointcut
	|	'set' '(' fieldPattern ')'										#SetPointcut
	|	'handler' '(' optionalParensTypePattern ')'					#HandlerPointcut
	|	'adviceexecution' '(' ')'									#AdviceExecutionPointcut
	|	'within' '(' optionalParensTypePattern ')'					#WithinPointcut
	|	'withincode' '(' methodOrConstructorPattern ')'			#WithinCodePointcut
	|	'cflow' '(' pointcutExpression ')'							#CFlowPointcut
	|	'cflowbelow' '(' pointcutExpression ')'						#CFlowBelowPointcut
	|	'if' '(' expression? ')'										#IfPointcut
	|	'this' '(' typeOrIdentifier ')'								#ThisPointcutPointcut
	|	'target' '(' typeOrIdentifier ')'								#TargetPointcut
	|	'args' '(' argsPatternList ')'								#ArgsPointcut
	|	'@' 'this' '(' annotationOrIdentifer ')'						#AnnotationThisPointcut
	|	'@' 'target' '(' annotationOrIdentifer ')'					#AnnotationTargetPointcut
	|	'@' 'args' '(' annotationsOrIdentifiersPattern ')'			#AnnotationArgsPointcut
	|	'@' 'within' '(' annotationOrIdentifer ')'					#AnnotationWithinPointcut
	|	'@' 'withincode' '(' annotationOrIdentifer ')'				#AnnotationWithinCodePointcut
	|	'@' 'annotation' '(' annotationOrIdentifer ')'				#AnnotationPointcut
	;

referencePointcut
	:	(typePattern '.')? id formalParametersPattern
	;
	
interTypeMemberDeclaration
	:	modifier* (type|'void') type '.' id formalParameters ('throws' typeList)? methodBody
	|	modifier* 'abstract' modifier* (type|'void') type '.' id formalParameters ('throws' typeList)? ';'
	|	modifier* type '.' 'new' formalParameters ('throws' typeList)? methodBody
	|	modifier* (type|'void') type '.' id ('=' expression)? ';'
	;

interTypeDeclaration
	:	'declare' 'parents' ':' typePattern 'extends' type ';'
	|	'declare' 'parents' ':' typePattern 'implements' typeList ';' 
	|	'declare' 'warning' ':' pointcutExpression ':' StringLiteral ';'
	|	'declare' 'error' ':' pointcutExpression ':' StringLiteral ';'
	|	'declare' 'soft' ':' type ':' pointcutExpression ';'
	|	'declare' 'precedence' ':' typePatternList ';'
	|	'declare' '@' 'type' ':' typePattern ':' annotation ';'
	|	'declare' '@' 'method' ':' methodPattern ':' annotation ';' 
	|	'declare' '@' 'constructor' ':' constructorPattern ':' annotation ';' 
	|	'declare' '@' 'field' ':' fieldPattern ':' annotation ';'
	;

typePattern
	:	simpleTypePattern
	|	'!' typePattern 
	|	'(' annotationPattern? typePattern ')'
	|	typePattern '&&' typePattern 
  	|	typePattern '||' typePattern
  	;
  	  	
simpleTypePattern
	:	dottedNamePattern '+'? ('[' ']')*
  	;
  	
dottedNamePattern
	:	(type | id | '*' | '.' | '..')+
	|	'void'
	;    

optionalParensTypePattern
	:	'(' annotationPattern? typePattern ')'
	|	annotationPattern? typePattern
	;
	
	
fieldPattern
	:	annotationPattern? fieldModifiersPattern? typePattern (typePattern dotOrDotDot)? simpleNamePattern 
	;
	
fieldModifiersPattern
	:	'!'? fieldModifier fieldModifiersPattern*
	;
	
fieldModifier
	:	(	'public' 
		|	'private' 
		|	'protected' 
		|	'static' 
		|	'transient'
		|	'final' 
		)
	;
	
dotOrDotDot
	:	'.'
	|	'..'
	;		            		      
		            		      		            			
simpleNamePattern
	:	id ('*' id)* '*'?
	|	'*' (id '*')* id?
	;
      
methodOrConstructorPattern
	:	methodPattern
	|	constructorPattern
	;
	
methodPattern
	:	annotationPattern? methodModifiersPattern? typePattern (typePattern dotOrDotDot)? simpleNamePattern formalParametersPattern throwsPattern?
	;
	
	
methodModifiersPattern
	:	'!'? methodModifier methodModifiersPattern*
	;
		
methodModifier
	:	(	'public'
		|	'private'
		|	'protected'
		|	'static'
		|	'synchronized'
		|	'final'
		)
	;
		            		      
formalsPattern
	:	'..' (',' formalsPatternAfterDotDot)* 
	|	optionalParensTypePattern (',' formalsPattern)* 
	|	typePattern '...'
	;
	              
formalsPatternAfterDotDot
	:	optionalParensTypePattern (',' formalsPatternAfterDotDot)* 
	|	typePattern '...'
	;
	                              		                  
throwsPattern
	:	'throws' typePatternList
	;
		
typePatternList
	:	typePattern (',' typePattern)*
	;

		
constructorPattern
	:	annotationPattern? constructorModifiersPattern? (typePattern dotOrDotDot)? 'new' formalParametersPattern throwsPattern?
	;
	
constructorModifiersPattern
	:	'!'? constructorModifier constructorModifiersPattern*
	;
		
constructorModifier
	:	('public' | 'private' | 'protected')
	;

	
annotationPattern
	:	'!'? '@' annotationTypePattern annotationPattern* 
	;

annotationTypePattern
	:	qualifiedName 
	|	'(' typePattern ')'
	;
	
formalParametersPattern
	:	'(' formalsPattern? ')'
	;

typeOrIdentifier
	:	type
	|	variableDeclaratorId
	;
	
annotationOrIdentifer
	:	qualifiedName | id
	;

annotationsOrIdentifiersPattern
	:	'..' (',' annotationsOrIdentifiersPatternAfterDotDot)?
	|	annotationOrIdentifer (',' annotationsOrIdentifiersPattern)*
	|	'*' (',' annotationsOrIdentifiersPattern)*
	;
	                  
annotationsOrIdentifiersPatternAfterDotDot
	:	annotationOrIdentifer (',' annotationsOrIdentifiersPatternAfterDotDot)*
	|	'*' (',' annotationsOrIdentifiersPatternAfterDotDot)*
	;
	
argsPattern
	:	typeOrIdentifier 
	|	('*' | '..')
	;
	
argsPatternList
	:	argsPattern (',' argsPattern)* 
	;


// all of the following rules are only necessary to change rules in the original Java grammar from 'Identifier' to 'id'

id
	:	(	ARGS 
		|	AFTER 
		|	AROUND 
		|	ASPECT
		|	BEFORE
		|	CALL
		|	CFLOW
		|	CFLOWBELOW
		|	DECLARE
		|	ERROR
		|	EXECUTION
		|	GET
		|	HANDLER
		|	INITIALIZATION
		|	ISSINGLETON
		|	PARENTS
		|	PERCFLOW
		|	PERCFLOWBELOW
		|	PERTARGET
		|	PERTHIS
		|	PERTYPEWITHIN
		|	POINTCUT
		|	PRECEDENCE
		|	PREINITIALIZATION
		|	PRIVILEGED
		|	RETURNING
		|	SET
		|	SOFT
		|	STATICINITIALIZATION
		|	TARGET
		|	THROWING
		|	WARNING
		|	WITHIN
		|	WITHINCODE
		)
	|	Identifier
	;
	
	
classDeclaration
    :   'class' id typeParameters?
        ('extends' type)?
        ('implements' typeList)?
        classBody
    ;
    
typeParameter
    :   id ('extends' typeBound)?
    ;
    
enumDeclaration
    :   ENUM id ('implements' typeList)?
        '{' enumConstants? ','? enumBodyDeclarations? '}'
    ;

enumConstant
    :   annotation* id arguments? classBody?
    ;
    
interfaceDeclaration
    :   'interface' id typeParameters? ('extends' typeList)? interfaceBody
    ;
    
methodDeclaration
    :   (type|'void') id formalParameters ('[' ']')*
        ('throws' qualifiedNameList)?
        (   methodBody
        |   ';'
        )
    ;

constructorDeclaration
    :   id formalParameters ('throws' qualifiedNameList)?
        constructorBody
    ;
    
constantDeclarator
    :   id ('[' ']')* '=' variableInitializer
    ;
    
interfaceMethodDeclaration
    :   (type|'void') id formalParameters ('[' ']')*
        ('throws' qualifiedNameList)?
        ';'
    ;
    
variableDeclaratorId
    :   id ('[' ']')*
    ;
    
enumConstantName
    :   id
    ;
    
classOrInterfaceType
    :   id typeArguments? ('.' id typeArguments? )*
    ;

qualifiedName
    :   id ('.' id)*
    ;
    
elementValuePair
    :   id '=' elementValue
    ;
    
annotationTypeDeclaration
    :   '@' 'interface' id annotationTypeBody
    ;
    
annotationMethodRest
    :   id '(' ')' defaultValue?
    ;
    
statement
    :   block
    |   ASSERT expression (':' expression)? ';'
    |   'if' parExpression statement ('else' statement)?
    |   'for' '(' forControl ')' statement
    |   'while' parExpression statement
    |   'do' statement 'while' parExpression ';'
    |   'try' block (catchClause+ finallyBlock? | finallyBlock)
    |   'try' resourceSpecification block catchClause* finallyBlock?
    |   'switch' parExpression '{' switchBlockStatementGroup* switchLabel* '}'
    |   'synchronized' parExpression block
    |   'return' expression? ';'
    |   'throw' expression ';'
    |   'break' id? ';'
    |   'continue' id? ';'
    |   ';'
    |   statementExpression ';'
    |   id ':' statement
    ;
    
catchClause
    :   'catch' '(' variableModifier* catchType id ')' block
    ;
    
expression
    :   primary
    |   expression '.' id
    |   expression '.' 'this'
    |   expression '.' 'new' nonWildcardTypeArguments? innerCreator
    |   expression '.' 'super' superSuffix
    |   expression '.' explicitGenericInvocation
    |   expression '[' expression ']'
    |   expression '(' expressionList? ')'
    |   'new' creator
    |   '(' type ')' expression
    |   expression ('++' | '--')
    |   ('+'|'-'|'++'|'--') expression
    |   ('~'|'!') expression
    |   expression ('*'|'/'|'%') expression
    |   expression ('+'|'-') expression
    |   expression ('<' '<' | '>' '>' '>' | '>' '>') expression
    |   expression ('<=' | '>=' | '>' | '<') expression
    |   expression 'instanceof' type
    |   expression ('==' | '!=') expression
    |   expression '&' expression
    |   expression '^' expression
    |   expression '|' expression
    |   expression '&&' expression
    |   expression '||' expression
    |   expression '?' expression ':' expression
    |   /*<assoc=right>*/ expression
        (   '='
        |   '+='
        |   '-='
        |   '*='
        |   '/='
        |   '&='
        |   '|='
        |   '^='
        |   '>>='
        |   '>>>='
        |   '<<='
        |   '%='
        )
        expression
    ;

primary
    :   '(' expression ')'
    |   'this'
    |   'super'
    |   literal
    |   id
    |   type '.' 'class'
    |   'void' '.' 'class'
    |   nonWildcardTypeArguments (explicitGenericInvocationSuffix | 'this' arguments)
    ;
    
createdName
    :   id typeArgumentsOrDiamond? ('.' id typeArgumentsOrDiamond?)*
    |   primitiveType
    ;

innerCreator
    :   id nonWildcardTypeArgumentsOrDiamond? classCreatorRest
    ;
    
superSuffix
    :   arguments
    |   '.' id arguments?
    ;
    
explicitGenericInvocationSuffix
    :   'super' superSuffix
    |   id arguments
    ;
    
