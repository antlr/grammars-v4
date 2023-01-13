	
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

lexer grammar AspectJLexer;

import JavaLexer;

DOTDOT								: '..';
DQUOTE								: '"';

ADVICEEXECUTION						: 'adviceexecution';
ANNOTATION							: 'annotation';
ARGS									: 'args';
AFTER									: 'after';
AROUND								: 'around';
ASPECT								: 'aspect';
BEFORE									: 'before';
CALL									: 'call';
CFLOW									: 'cflow';
CFLOWBELOW							: 'cflowbelow';
DECLARE								: 'declare';
ERROR									: 'error';
EXECUTION								: 'execution';
GET									: 'get';
HANDLER								: 'handler';
INITIALIZATION							: 'initialization';
ISSINGLETON							: 'issingleton';
PARENTS								: 'parents';
PERCFLOW								: 'percflow';
PERCFLOWBELOW						: 'percflowbelow';
PERTARGET								: 'pertarget';
PERTHIS								: 'perthis';
PERTYPEWITHIN						: 'pertypewithin';
POINTCUT								: 'pointcut';
PRECEDENCE							: 'precedence';
PREINITIALIZATION						: 'preinitialization';
PRIVILEGED								: 'privileged';
RETURNING								: 'returning';
SET										: 'set';
SOFT									: 'soft';
STATICINITIALIZATION 					: 'staticinitialization';
TARGET								: 'target';
THROWING								: 'throwing';
WARNING								: 'warning';
WITHIN									: 'within';
WITHINCODE							: 'withincode';

ANNOTATION_AFTER    				: 'After';
ANNOTATION_AFTERRETURNING 		: 'AfterReturning';
ANNOTATION_AFTERTHROWING  		: 'AfterThrowing';
ANNOTATION_AROUND  				: 'Around';
ANNOTATION_ASPECT  				: 'Aspect';
ANNOTATION_BEFORE  				: 'Before';
ANNOTATION_DECLAREPARENTS  	: 'DeclareParents';
ANNOTATION_DECLAREMIXIN			: 'DeclareMixin';
ANNOTATION_DECLAREWARNING  	: 'DeclareWarning';
ANNOTATION_DECLAREERROR  		: 'DeclareError';
ANNOTATION_DECLAREPRECEDENCE	: 'DeclarePrecedence';
ANNOTATION_POINTCUT  				: 'Pointcut';
ANNOTATION_CONSTRUCTOR			: 'constructor';
ANNOTATION_DEFAULTIMPL			: 'defaultImpl';
ANNOTATION_FIELD					: 'field';
ANNOTATION_INTERFACES				: 'interfaces';
ANNOTATION_TYPE					: 'type';
ANNOTATION_METHOD				: 'method';
ANNOTATION_VALUE					: 'value';

AT : '@'								-> pushMode(Annotation);


mode Annotation;

ANNOTATION_AFTER1    				: ANNOTATION_AFTER						-> type(ANNOTATION_AFTER), mode(AspectJAnnotationMode);
ANNOTATION_AFTERRETURNING1 	: ANNOTATION_AFTERRETURNING	    		-> type(ANNOTATION_AFTERRETURNING), mode(AspectJAnnotationMode);
ANNOTATION_AFTERTHROWING1  	: ANNOTATION_AFTERTHROWING			-> type(ANNOTATION_AFTERTHROWING), mode(AspectJAnnotationMode);
ANNOTATION_AROUND1 				: ANNOTATION_AROUND 			   		-> type(ANNOTATION_AROUND), mode(AspectJAnnotationMode);
ANNOTATION_ASPECT1  				: ANNOTATION_ASPECT 		       		-> type(ANNOTATION_ASPECT), mode(AspectJAnnotationMode);
ANNOTATION_BEFORE1  				: ANNOTATION_BEFORE 		       		-> type(ANNOTATION_BEFORE), mode(AspectJAnnotationMode);
ANNOTATION_DECLAREPARENTS1 	: ANNOTATION_DECLAREPARENTS 		-> type(ANNOTATION_DECLAREPARENTS), mode(AspectJAnnotationMode);
ANNOTATION_DECLAREMIXIN1			: ANNOTATION_DECLAREMIXIN				-> type(ANNOTATION_DECLAREMIXIN), mode(AspectJAnnotationMode);
ANNOTATION_DECLAREWARNING1  	: ANNOTATION_DECLAREWARNING		-> type(ANNOTATION_DECLAREWARNING), mode(AspectJAnnotationMode);
ANNOTATION_DECLAREERROR1  		: ANNOTATION_DECLAREERROR			-> type(ANNOTATION_DECLAREERROR), mode(AspectJAnnotationMode);
ANNOTATION_DECLAREPRECEDENCE1	: ANNOTATION_DECLAREPRECEDENCE		-> type(ANNOTATION_DECLAREPRECEDENCE), mode(AspectJAnnotationMode);
ANNOTATION_POINTCUT1  			: ANNOTATION_POINTCUT					-> type(ANNOTATION_POINTCUT), mode(AspectJAnnotationMode);

ARGS1  								: ARGS		  	   							-> type(ARGS), mode(DEFAULT_MODE);
TARGET1  								: TARGET		  	   						-> type(TARGET), mode(DEFAULT_MODE);
THIS1  								: THIS		  	   							-> type(THIS), mode(DEFAULT_MODE);

Identifier1								:   Identifier								-> type(Identifier), mode(DEFAULT_MODE);
WS1									:  [ \t\r\n\u000C]+ 						-> skip;
COMMENT1								:   '/*' .*? '*/' 							-> skip;
LINE_COMMENT1						:   '//' ~[\r\n]* 							-> skip;
INVALID1								:	.										-> mode(DEFAULT_MODE);
    

mode AspectJAnnotationMode;
    
ABSTRACT2							: ABSTRACT								-> type(ABSTRACT), mode(DEFAULT_MODE);
ASSERT2								: ASSERT									-> type(ASSERT), mode(DEFAULT_MODE);
BOOLEAN2								: BOOLEAN									-> type(BOOLEAN), mode(DEFAULT_MODE);
BREAK2								: BREAK									-> type(BREAK), mode(DEFAULT_MODE);
BYTE2									: BYTE										-> type(BYTE), mode(DEFAULT_MODE);
CASE2									: CASE										-> type(CASE), mode(DEFAULT_MODE);
CATCH2								: CATCH									-> type(CATCH), mode(DEFAULT_MODE);
CHAR2									: CHAR										-> type(CHAR), mode(DEFAULT_MODE);
CLASS2								: CLASS									-> type(CLASS), mode(DEFAULT_MODE);
CONST2								: CONST									-> type(CONST), mode(DEFAULT_MODE);
CONTINUE2								: CONTINUE								-> type(CONTINUE), mode(DEFAULT_MODE);
DEFAULT2								: DEFAULT									-> type(DEFAULT), mode(DEFAULT_MODE);
DO2									: DO										-> type(DO), mode(DEFAULT_MODE);
DOUBLE2								: DOUBLE									-> type(DOUBLE), mode(DEFAULT_MODE);
ELSE2									: ELSE										-> type(ELSE), mode(DEFAULT_MODE);
ENUM2									: ENUM										-> type(ENUM), mode(DEFAULT_MODE);
EXTENDS2								: EXTENDS									-> type(EXTENDS), mode(DEFAULT_MODE);
FINAL2									: FINAL										-> type(FINAL), mode(DEFAULT_MODE);
FINALLY2								: FINALLY									-> type(FINALLY), mode(DEFAULT_MODE);
FLOAT2								: FLOAT									-> type(FLOAT), mode(DEFAULT_MODE);
FOR2									: FOR										-> type(FOR), mode(DEFAULT_MODE);
IF2										: IF											-> type(IF), mode(DEFAULT_MODE);
GOTO2									: GOTO										-> type(GOTO), mode(DEFAULT_MODE);
IMPLEMENTS2							: IMPLEMENTS								-> type(IMPLEMENTS), mode(DEFAULT_MODE);
IMPORT2								: IMPORT									-> type(IMPORT), mode(DEFAULT_MODE);
INSTANCEOF2							: INSTANCEOF								-> type(INSTANCEOF), mode(DEFAULT_MODE);
INT2									: INT										-> type(INT), mode(DEFAULT_MODE);
INTERFACE2							: INTERFACE								-> type(INTERFACE), mode(DEFAULT_MODE);
LONG2									: LONG										-> type(LONG), mode(DEFAULT_MODE);
NATIVE2								: NATIVE									-> type(NATIVE), mode(DEFAULT_MODE);
NEW2									: NEW										-> type(NEW), mode(DEFAULT_MODE);
PACKAGE2								: PACKAGE									-> type(PACKAGE), mode(DEFAULT_MODE);
PRIVATE2								: PRIVATE									-> type(PRIVATE), mode(DEFAULT_MODE);
PROTECTED2							: PROTECTED								-> type(PROTECTED), mode(DEFAULT_MODE);
PUBLIC2								: PUBLIC									-> type(PUBLIC), mode(DEFAULT_MODE);
RETURN2								: RETURN									-> type(RETURN), mode(DEFAULT_MODE);
SHORT2								: SHORT									-> type(SHORT), mode(DEFAULT_MODE);
STATIC2								: STATIC									-> type(STATIC), mode(DEFAULT_MODE);
STRICTFP2								: STRICTFP									-> type(STRICTFP), mode(DEFAULT_MODE);
SUPER2									: SUPER									-> type(SUPER), mode(DEFAULT_MODE);
SWITCH2								: SWITCH									-> type(SWITCH), mode(DEFAULT_MODE);
SYNCHRONIZED2						: SYNCHRONIZED							-> type(SYNCHRONIZED), mode(DEFAULT_MODE);
THIS2									: THIS										-> type(THIS), mode(DEFAULT_MODE);
THROW2								: THROW									-> type(THROW), mode(DEFAULT_MODE);
THROWS2								: THROWS									-> type(THROWS), mode(DEFAULT_MODE);
TRANSIENT2							: TRANSIENT								-> type(TRANSIENT), mode(DEFAULT_MODE);
TRY2									: TRY										-> type(TRY), mode(DEFAULT_MODE);
VOID2									: VOID										-> type(VOID), mode(DEFAULT_MODE);
VOLATILE2								: VOLATILE									-> type(VOLATILE), mode(DEFAULT_MODE);
WHILE2									: WHILE									-> type(WHILE), mode(DEFAULT_MODE);

ADVICEEXECUTION2					: ADVICEEXECUTION						-> type(ADVICEEXECUTION), mode(DEFAULT_MODE);
ANNOTATION2							: ANNOTATION								-> type(ANNOTATION), mode(DEFAULT_MODE);
ARGS2									: ARGS										-> type(ARGS), mode(DEFAULT_MODE);
AFTER2								: AFTER									-> type(AFTER), mode(DEFAULT_MODE);
AROUND2								: AROUND									-> type(AROUND), mode(DEFAULT_MODE);
ASPECT2								: ASPECT									-> type(ASPECT), mode(DEFAULT_MODE);
BEFORE2								: BEFORE									-> type(BEFORE), mode(DEFAULT_MODE);
CALL2									: CALL										-> type(CALL), mode(DEFAULT_MODE);
CFLOW2								: CFLOW									-> type(CFLOW), mode(DEFAULT_MODE);
CFLOWBELOW2						: CFLOWBELOW							-> type(CFLOWBELOW), mode(DEFAULT_MODE);
DECLARE2								: DECLARE									-> type(DECLARE), mode(DEFAULT_MODE);
ERROR2								: ERROR									-> type(ERROR), mode(DEFAULT_MODE);
EXECUTION2							: EXECUTION								-> type(EXECUTION), mode(DEFAULT_MODE);
GET2									: GET										-> type(GET), mode(DEFAULT_MODE);
HANDLER2								: HANDLER									-> type(HANDLER), mode(DEFAULT_MODE);
INITIALIZATION2						: INITIALIZATION							-> type(INITIALIZATION), mode(DEFAULT_MODE);
ISSINGLETON2							: ISSINGLETON								-> type(ISSINGLETON), mode(DEFAULT_MODE);
PARENTS2								: PARENTS									-> type(PARENTS), mode(DEFAULT_MODE);
PERCFLOW2							: PERCFLOW								-> type(PERCFLOW), mode(DEFAULT_MODE);
PERCFLOWBELOW2						: PERCFLOWBELOW						-> type(PERCFLOWBELOW), mode(DEFAULT_MODE);
PERTARGET2							: PERTARGET								-> type(PERTARGET), mode(DEFAULT_MODE);
PERTHIS2								: PERTHIS									-> type(PERTHIS), mode(DEFAULT_MODE);
PERTYPEWITHIN2						: PERTYPEWITHIN							-> type(PERTYPEWITHIN), mode(DEFAULT_MODE);
POINTCUT2								: POINTCUT								-> type(POINTCUT), mode(DEFAULT_MODE);
PRECEDENCE2							: PRECEDENCE								-> type(PRECEDENCE), mode(DEFAULT_MODE);
PREINITIALIZATION2					: PREINITIALIZATION						-> type(PREINITIALIZATION), mode(DEFAULT_MODE);
PRIVILEGED2							: PRIVILEGED								-> type(PRIVILEGED), mode(DEFAULT_MODE);
RETURNING2							: RETURNING								-> type(RETURNING), mode(DEFAULT_MODE);
SET2									: SET										-> type(SET), mode(DEFAULT_MODE);
SOFT2									: SOFT										-> type(SOFT), mode(DEFAULT_MODE);
STATICINITIALIZATION2				: STATICINITIALIZATION					-> type(STATICINITIALIZATION), mode(DEFAULT_MODE);
TARGET2								: TARGET									-> type(TARGET), mode(DEFAULT_MODE);
THROWING2							: THROWING								-> type(THROWING), mode(DEFAULT_MODE);
WARNING2								: WARNING									-> type(WARNING), mode(DEFAULT_MODE);
WITHIN2								: WITHIN									-> type(WITHIN), mode(DEFAULT_MODE);
WITHINCODE2							: WITHINCODE								-> type(WITHINCODE), mode(DEFAULT_MODE);

IntegerLiteral2							: IntegerLiteral 							-> type(IntegerLiteral), pushMode(AspectJAnnotationScope);
FloatingPointLiteral2					: FloatingPointLiteral						-> type(FloatingPointLiteral), pushMode(AspectJAnnotationScope);
BooleanLiteral2							: BooleanLiteral							-> type(BooleanLiteral), pushMode(AspectJAnnotationScope);
CharacterLiteral2						: CharacterLiteral							-> type(CharacterLiteral), pushMode(AspectJAnnotationScope);
StringLiteral2							: StringLiteral								-> type(StringLiteral), pushMode(AspectJAnnotationScope);
NullLiteral2								: NullLiteral									-> type(NullLiteral), pushMode(AspectJAnnotationScope);
LPAREN2								: LPAREN 									-> type(LPAREN), pushMode(AspectJAnnotationScope);
RPAREN2								: RPAREN									-> type(RPAREN), mode(DEFAULT_MODE);
LBRACE2								: LBRACE									-> type(LBRACE), mode(DEFAULT_MODE);
RBRACE2								: RBRACE									-> type(RBRACE), mode(DEFAULT_MODE);
LBRACK2								: LBRACK									-> type(LBRACK), mode(DEFAULT_MODE);
RBRACK2								: RBRACK									-> type(RBRACK), mode(DEFAULT_MODE);
SEMI2									: SEMI										-> type(SEMI), mode(DEFAULT_MODE);
COMMA2								: COMMA									-> type(COMMA), mode(DEFAULT_MODE);
DOT2									: DOT										-> type(DOT), mode(DEFAULT_MODE);
DOTDOT2								: DOTDOT									-> type(DOTDOT), mode(DEFAULT_MODE);
DQUOTE2								: DQUOTE									-> type(DQUOTE), mode(DEFAULT_MODE);
ASSIGN2								: ASSIGN									-> type(ASSIGN), mode(DEFAULT_MODE);
GT2									: GT										-> type(GT), mode(DEFAULT_MODE);
LT2									: LT										-> type(LT), mode(DEFAULT_MODE);
BANG2									: BANG										-> type(BANG), mode(DEFAULT_MODE);
TILDE2									: TILDE										-> type(TILDE), mode(DEFAULT_MODE);
QUESTION2								: QUESTION								-> type(QUESTION), mode(DEFAULT_MODE);
COLON2								: COLON									-> type(COLON), mode(DEFAULT_MODE);
EQUAL2								: EQUAL									-> type(EQUAL), mode(DEFAULT_MODE);
LE2										: LE										-> type(LE), mode(DEFAULT_MODE);
GE2									: GE										-> type(GE), mode(DEFAULT_MODE);
NOTEQUAL2							: NOTEQUAL								-> type(NOTEQUAL), mode(DEFAULT_MODE);
AND2									: AND										-> type(ADD), mode(DEFAULT_MODE);
OR2									: OR										-> type(OR), mode(DEFAULT_MODE);
INC2									: INC										-> type(INC), mode(DEFAULT_MODE);
DEC2									: DEC										-> type(DEC), mode(DEFAULT_MODE);
ADD2									: ADD										-> type(ADD), mode(DEFAULT_MODE);
SUB2									: SUB										-> type(SUB), mode(DEFAULT_MODE);
MUL2									: MUL										-> type(MUL), mode(DEFAULT_MODE);
DIV2									: DIV										-> type(DIV), mode(DEFAULT_MODE);
BITAND2								: BITAND									-> type(BITAND), mode(DEFAULT_MODE);
BITOR2									: BITOR										-> type(BITOR), mode(DEFAULT_MODE);
CARET2								: CARET									-> type(CARET), mode(DEFAULT_MODE);
MOD2									: MOD										-> type(MOD), mode(DEFAULT_MODE);
ADD_ASSIGN2							: ADD_ASSIGN								-> type(ADD_ASSIGN), mode(DEFAULT_MODE);
SUB_ASSIGN2							: SUB_ASSIGN								-> type(SUB_ASSIGN), mode(DEFAULT_MODE);
MUL_ASSIGN2							: MUL_ASSIGN								-> type(MUL_ASSIGN), mode(DEFAULT_MODE);
DIV_ASSIGN2							: DIV_ASSIGN								-> type(DIV_ASSIGN), mode(DEFAULT_MODE);
AND_ASSIGN2							: AND_ASSIGN								-> type(AND_ASSIGN), mode(DEFAULT_MODE);
OR_ASSIGN2							: OR_ASSIGN								-> type(OR_ASSIGN), mode(DEFAULT_MODE);
XOR_ASSIGN2							: XOR_ASSIGN								-> type(XOR_ASSIGN), mode(DEFAULT_MODE);
MOD_ASSIGN2							: MOD_ASSIGN								-> type(MOD_ASSIGN), mode(DEFAULT_MODE);
LSHIFT_ASSIGN2						: LSHIFT_ASSIGN							-> type(LSHIFT_ASSIGN), mode(DEFAULT_MODE);
RSHIFT_ASSIGN2						: RSHIFT_ASSIGN							-> type(RSHIFT_ASSIGN), mode(DEFAULT_MODE);
URSHIFT_ASSIGN2						: URSHIFT_ASSIGN							-> type(URSHIFT_ASSIGN), mode(DEFAULT_MODE);
Identifier2								: Identifier									-> type(Identifier), mode(DEFAULT_MODE);

AT2									: AT										-> type(AT), mode(Annotation);		
				
ELLIPSIS2								: ELLIPSIS 									-> type(ELLIPSIS), mode(DEFAULT_MODE);
WS2									: WS 										-> skip;    
COMMENT2								: COMMENT								-> skip;    
LINE_COMMENT2						: LINE_COMMENT							-> skip;
    
    
mode AspectJAnnotationScope;

RPAREN3								: RPAREN									-> type(RPAREN), mode(DEFAULT_MODE);

DQUOTE3								: DQUOTE									-> type(DQUOTE), pushMode(AspectJAnnotationString);

AT3									: AT										-> type(AT), pushMode(Annotation);

ASSIGN3								: ASSIGN									-> type(ASSIGN);
LBRACE3								: LBRACE									-> type(LBRACE);
RBRACE3								: RBRACE									-> type(RBRACE);
COMMA3								: COMMA									-> type(COMMA);
DOT3									: DOT										-> type(DOT);
CLASS3								: CLASS									-> type(CLASS);

DEFAULTIMPL3							: ANNOTATION_DEFAULTIMPL				-> type(ANNOTATION_DEFAULTIMPL);
ANNOTATION_INTERFACES3			: ANNOTATION_INTERFACES				-> type(ANNOTATION_INTERFACES);
POINTCUT3								: POINTCUT								-> type(POINTCUT);
RETURNING3							: RETURNING								-> type(RETURNING);
VALUE3								: ANNOTATION_VALUE					-> type(ANNOTATION_VALUE);

Identifier3								: Identifier									-> type(Identifier);
WS3									: [ \t\r\n\u000C]+ 						-> skip;
COMMENT3								: '/*' .*? '*/' 								-> skip;
LINE_COMMENT3						: '//' ~[\r\n]* 								-> skip;
INVALID3								: .											-> mode(DEFAULT_MODE);


mode AspectJAnnotationString;

DQUOTE4								: DQUOTE									-> type(DQUOTE), popMode;

LPAREN4								: LPAREN									-> type(LPAREN);
RPAREN4								: RPAREN									-> type(RPAREN);
COLON4								: COLON									-> type(COLON);
AND4									: AND										-> type(AND);
OR4									: OR										-> type(OR);
COMMA4								: COMMA									-> type(COMMA);
DOT4									: DOT										-> type(DOT);
DOTDOT4								: DOTDOT									-> type(DOTDOT);
EQUAL4								: EQUAL									-> type(EQUAL);
ADD4									: ADD										-> type(ADD);
LBRACE4								: LBRACE									-> type(LBRACE);
RBRACE4								: RBRACE									-> type(RBRACE);
BANG4									: BANG										-> type(BANG);
MUL4									: MUL										-> type(MUL);
ASSIGN4								: ASSIGN									-> type(ASSIGN);
BOOLEAN4								: BOOLEAN									-> type(BOOLEAN);
BYTE4									: BYTE										-> type(BYTE);
CHAR4									: CHAR										-> type(CHAR);
IF4										: IF											-> type(IF);
INT4									: INT										-> type(INT);
LONG4									: LONG										-> type(LONG);
NEW4									: NEW										-> type(NEW);
SHORT4								: SHORT									-> type(SHORT);
THIS4									: THIS										-> type(THIS);
VOID4									: VOID										-> type(VOID);

ADVICEEXECUTION4					: ADVICEEXECUTION 						-> type(ADVICEEXECUTION);
ANNOTATION4							: ANNOTATION 							-> type(ANNOTATION);
ARGS4									: ARGS 									-> type(ARGS);
AFTER4								: AFTER 									-> type(AFTER);
AROUND4								: AROUND 									-> type(AROUND);
ASPECT4								: ASPECT 									-> type(ASPECT);
BEFORE4								: BEFORE 									-> type(BEFORE);
CALL4									: CALL 									-> type(CALL);
CFLOW4								: CFLOW 									-> type(CFLOW);
CFLOWBELOW4						: CFLOWBELOW 							-> type(CFLOWBELOW);
DECLARE4								: DECLARE 								-> type(DECLARE);
ERROR4								: ERROR 									-> type(ERROR);
EXECUTION4							: EXECUTION 								-> type(EXECUTION);
GET4									: GET										-> type(GET);
HANDLER4								: HANDLER 								-> type(HANDLER);
INITIALIZATION4						: INITIALIZATION 							-> type(INITIALIZATION);
ISSINGLETON4							: ISSINGLETON 							-> type(ISSINGLETON);
PARENTS4								: PARENTS									-> type(PARENTS);
PERCFLOW4							: PERCFLOW 								-> type(PERCFLOW);
PERCFLOWBELOW4						: PERCFLOWBELOW 						-> type(PERCFLOWBELOW);
PERTARGET4							: PERTARGET 								-> type(PERTARGET);
PERTHIS4								: PERTHIS									-> type(PERTHIS);
PERTYPEWITHIN4						: PERTYPEWITHIN 							-> type(PERTYPEWITHIN);
POINTCUT4								: POINTCUT								-> type(POINTCUT);
PRECEDENCE4							: PRECEDENCE 								-> type(PRECEDENCE);
PREINITIALIZATION4					: PREINITIALIZATION  						-> type(PREINITIALIZATION );
PRIVILEGED4							: PRIVILEGED 								-> type(PRIVILEGED);
RETURNING4							: RETURNING 								-> type(RETURNING);
SET4									: SET  										-> type(SET );
SOFT4									: SOFT  									-> type(SOFT );
STATICINITIALIZATION4 				: STATICINITIALIZATION 					-> type(STATICINITIALIZATION);
TARGET4								: TARGET  									-> type(TARGET );
THROWING4							: THROWING 								-> type(THROWING);
WARNING4								: WARNING 								-> type(WARNING);
WITHIN4								: WITHIN  									-> type(WITHIN );
WITHINCODE4							: WITHINCODE 								-> type(WITHINCODE);

Identifier4								: Identifier									-> type(Identifier);
WS4									: [ \t\r\n\u000C]+ 						-> skip;
COMMENT4								: '/*' .*? '*/' 								-> skip;
LINE_COMMENT4						: '//' ~[\r\n]* 								-> skip;
INVALID4								: .											-> popMode, mode(DEFAULT_MODE);
	