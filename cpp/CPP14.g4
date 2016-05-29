/*******************************************************************************
 * Copyright (c) 2015 Camiloasc1
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. Neither the name of copyright holders nor the names of its
 *    contributors may be used to endorse or promote products derived
 *    from this software without specific prior written permission.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * ''AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
 * PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL COPYRIGHT HOLDERS OR CONTRIBUTORS
 * BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 ******************************************************************************/

/*******************************************************************************
 * C++14 Grammar for ANTLR v4
 *
 * Based on n4140 draft paper
 * https://github.com/cplusplus/draft/blob/master/papers/n4140.pdf
 * and
 * http://www.nongnu.org/hcb/
 * 
 * Possible Issues:
 * 
 * Input must avoid conditional compilation blocks (this grammar ignores any preprocessor directive)
 * GCC extensions not yet supported (not try parse preprocessor output)
 * Lexer issue with pure-specifier rule ('0' token) - Solution in embedded java
 * Right angle bracket (C++11) - Solution '>>' and '>>=' are not tokens, only '>'
 * 
 ******************************************************************************/
grammar CPP14;


/*Basic concepts*/
translationunit
:
	declarationseq? EOF
;

/*Expressions*/
primaryexpression
:
	literal
	| This
	| '(' expression ')'
	| idexpression
	| lambdaexpression
;

idexpression
:
	unqualifiedid
	| qualifiedid
;

unqualifiedid
:
	Identifier
	| operatorfunctionid
	| conversionfunctionid
	| literaloperatorid
	| '~' classname
	| '~' decltypespecifier
	| templateid
;

qualifiedid
:
	nestednamespecifier Template? unqualifiedid
;

nestednamespecifier
:
	'::'
	| typename '::'
	| namespacename '::'
	| decltypespecifier '::'
	| nestednamespecifier Identifier '::'
	| nestednamespecifier Template? simpletemplateid '::'
;

lambdaexpression
:
	lambdaintroducer lambdadeclarator? compoundstatement
;

lambdaintroducer
:
	'[' lambdacapture? ']'
;

lambdacapture
:
	capturedefault
	| capturelist
	| capturedefault ',' capturelist
;

capturedefault
:
	'&'
	| '='
;

capturelist
:
	capture '...'?
	| capturelist ',' capture '...'?
;

capture
:
	simplecapture
	| initcapture
;

simplecapture
:
	Identifier
	| '&' Identifier
	| This
;

initcapture
:
	Identifier initializer
	| '&' Identifier initializer
;

lambdadeclarator
:
	'(' parameterdeclarationclause ')' Mutable? exceptionspecification?
	attributespecifierseq? trailingreturntype?
;

postfixexpression
:
	primaryexpression
	| postfixexpression '[' expression ']'
	| postfixexpression '[' bracedinitlist ']'
	| postfixexpression '(' expressionlist? ')'
	| simpletypespecifier '(' expressionlist? ')'
	| typenamespecifier '(' expressionlist? ')'
	| simpletypespecifier bracedinitlist
	| typenamespecifier bracedinitlist
	| postfixexpression '.' Template? idexpression
	| postfixexpression '->' Template? idexpression
	| postfixexpression '.' pseudodestructorname
	| postfixexpression '->' pseudodestructorname
	| postfixexpression '++'
	| postfixexpression '--'
	| Dynamic_cast '<' typeid '>' '(' expression ')'
	| Static_cast '<' typeid '>' '(' expression ')'
	| Reinterpret_cast '<' typeid '>' '(' expression ')'
	| Const_cast '<' typeid '>' '(' expression ')'
	| Typeid '(' expression ')'
	| Typeid '(' typeid ')'
;

expressionlist
:
	initializerlist
;

pseudodestructorname
:
	nestednamespecifier? typename '::' '~' typename
	| nestednamespecifier Template simpletemplateid '::' '~' typename
	| nestednamespecifier? '~' typename
	| '~' decltypespecifier
;

unaryexpression
:
	postfixexpression
	| '++' castexpression
	| '--' castexpression
	| unaryoperator castexpression
	| Sizeof unaryexpression
	| Sizeof '(' typeid ')'
	| Sizeof '...' '(' Identifier ')'
	| Alignof '(' typeid ')'
	| noexceptexpression
	| newexpression
	| deleteexpression
;

unaryoperator
:
	'|'
	| '*'
	| '&'
	| '+'
	| '!'
	| '~'
	| '-'
;

newexpression
:
	'::'? New newplacement? newtypeid newinitializer?
	| '::'? New newplacement? '(' typeid ')' newinitializer?
;

newplacement
:
	'(' expressionlist ')'
;

newtypeid
:
	typespecifierseq newdeclarator?
;

newdeclarator
:
	ptroperator newdeclarator?
	| noptrnewdeclarator
;

noptrnewdeclarator
:
	'[' expression ']' attributespecifierseq?
	| noptrnewdeclarator '[' constantexpression ']' attributespecifierseq?
;

newinitializer
:
	'(' expressionlist? ')'
	| bracedinitlist
;

deleteexpression
:
	'::'? Delete castexpression
	| '::'? Delete '[' ']' castexpression
;

noexceptexpression
:
	Noexcept '(' expression ')'
;

castexpression
:
	unaryexpression
	| '(' typeid ')' castexpression
;

pmexpression
:
	castexpression
	| pmexpression '.*' castexpression
	| pmexpression '->*' castexpression
;

multiplicativeexpression
:
	pmexpression
	| multiplicativeexpression '*' pmexpression
	| multiplicativeexpression '/' pmexpression
	| multiplicativeexpression '%' pmexpression
;

additiveexpression
:
	multiplicativeexpression
	| additiveexpression '+' multiplicativeexpression
	| additiveexpression '-' multiplicativeexpression
;

shiftexpression
:
	additiveexpression
	| shiftexpression '<<' additiveexpression
	| shiftexpression rightShift additiveexpression
;

relationalexpression
:
	shiftexpression
	| relationalexpression '<' shiftexpression
	| relationalexpression '>' shiftexpression
	| relationalexpression '<=' shiftexpression
	| relationalexpression '>=' shiftexpression
;

equalityexpression
:
	relationalexpression
	| equalityexpression '==' relationalexpression
	| equalityexpression '!=' relationalexpression
;

andexpression
:
	equalityexpression
	| andexpression '&' equalityexpression
;

exclusiveorexpression
:
	andexpression
	| exclusiveorexpression '^' andexpression
;

inclusiveorexpression
:
	exclusiveorexpression
	| inclusiveorexpression '|' exclusiveorexpression
;

logicalandexpression
:
	inclusiveorexpression
	| logicalandexpression '&&' inclusiveorexpression
;

logicalorexpression
:
	logicalandexpression
	| logicalorexpression '||' logicalandexpression
;

conditionalexpression
:
	logicalorexpression
	| logicalorexpression '?' expression ':' assignmentexpression
;

assignmentexpression
:
	conditionalexpression
	| logicalorexpression assignmentoperator initializerclause
	| throwexpression
;

assignmentoperator
:
	'='
	| '*='
	| '/='
	| '%='
	| '+='
	| '-='
	| rightShiftAssign
	| '<<='
	| '&='
	| '^='
	| '|='
;

expression
:
	assignmentexpression
	| expression ',' assignmentexpression
;

constantexpression
:
	conditionalexpression
;
/*Statements*/
statement
:
	labeledstatement
	| attributespecifierseq? expressionstatement
	| attributespecifierseq? compoundstatement
	| attributespecifierseq? selectionstatement
	| attributespecifierseq? iterationstatement
	| attributespecifierseq? jumpstatement
	| declarationstatement
	| attributespecifierseq? tryblock
;

labeledstatement
:
	attributespecifierseq? Identifier ':' statement
	| attributespecifierseq? Case constantexpression ':' statement
	| attributespecifierseq? Default ':' statement
;

expressionstatement
:
	expression? ';'
;

compoundstatement
:
	'{' statementseq? '}'
;

statementseq
:
	statement
	| statementseq statement
;

selectionstatement
:
	If '(' condition ')' statement
	| If '(' condition ')' statement Else statement
	| Switch '(' condition ')' statement
;

condition
:
	expression
	| attributespecifierseq? declspecifierseq declarator '=' initializerclause
	| attributespecifierseq? declspecifierseq declarator bracedinitlist
;

iterationstatement
:
	While '(' condition ')' statement
	| Do statement While '(' expression ')' ';'
	| For '(' forinitstatement condition? ';' expression? ')' statement
	| For '(' forrangedeclaration ':' forrangeinitializer ')' statement
;

forinitstatement
:
	expressionstatement
	| simpledeclaration
;

forrangedeclaration
:
	attributespecifierseq? declspecifierseq declarator
;

forrangeinitializer
:
	expression
	| bracedinitlist
;

jumpstatement
:
	Break ';'
	| Continue ';'
	| Return expression? ';'
	| Return bracedinitlist ';'
	| Goto Identifier ';'
;

declarationstatement
:
	blockdeclaration
;

/*Declarations*/
declarationseq
:
	declaration
	| declarationseq declaration
;

declaration
:
	blockdeclaration
	| functiondefinition
	| templatedeclaration
	| explicitinstantiation
	| explicitspecialization
	| linkagespecification
	| namespacedefinition
	| emptydeclaration
	| attributedeclaration
;

blockdeclaration
:
	simpledeclaration
	| asmdefinition
	| namespacealiasdefinition
	| usingdeclaration
	| usingdirective
	| static_assertdeclaration
	| aliasdeclaration
	| opaqueenumdeclaration
;

aliasdeclaration
:
	Using Identifier attributespecifierseq? '=' typeid ';'
;

simpledeclaration
:
	declspecifierseq? initdeclaratorlist? ';'
	| attributespecifierseq declspecifierseq? initdeclaratorlist ';'
;

static_assertdeclaration
:
	Static_assert '(' constantexpression ',' Stringliteral ')' ';'
;

emptydeclaration
:
	';'
;

attributedeclaration
:
	attributespecifierseq ';'
;

declspecifier
:
	storageclassspecifier
	| typespecifier
	| functionspecifier
	| Friend
	| Typedef
	| Constexpr
;

declspecifierseq
:
	declspecifier attributespecifierseq?
	| declspecifier declspecifierseq
;

storageclassspecifier
:
	Register
	| Static
	| Thread_local
	| Extern
	| Mutable
;

functionspecifier
:
	Inline
	| Virtual
	| Explicit
;

typedefname
:
	Identifier
;

typespecifier
:
	trailingtypespecifier
	| classspecifier
	| enumspecifier
;

trailingtypespecifier
:
	simpletypespecifier
	| elaboratedtypespecifier
	| typenamespecifier
	| cvqualifier
;

typespecifierseq
:
	typespecifier attributespecifierseq?
	| typespecifier typespecifierseq
;

trailingtypespecifierseq
:
	trailingtypespecifier attributespecifierseq?
	| trailingtypespecifier trailingtypespecifierseq
;

simpletypespecifier
:
	nestednamespecifier? typename
	| nestednamespecifier Template simpletemplateid
	| Char
	| Char16
	| Char32
	| Wchar
	| Bool
	| Short
	| Int
	| Long
	| Signed
	| Unsigned
	| Float
	| Double
	| Void
	| Auto
	| decltypespecifier
;

typename
:
	classname
	| enumname
	| typedefname
	| simpletemplateid
;

decltypespecifier
:
	Decltype '(' expression ')'
	| Decltype '(' Auto ')'
;

elaboratedtypespecifier
:
	classkey attributespecifierseq? nestednamespecifier? Identifier
	| classkey simpletemplateid
	| classkey nestednamespecifier Template? simpletemplateid
	| Enum nestednamespecifier? Identifier
;

enumname
:
	Identifier
;

enumspecifier
:
	enumhead '{' enumeratorlist? '}'
	| enumhead '{' enumeratorlist ',' '}'
;

enumhead
:
	enumkey attributespecifierseq? Identifier? enumbase?
	| enumkey attributespecifierseq? nestednamespecifier Identifier enumbase?
;

opaqueenumdeclaration
:
	enumkey attributespecifierseq? Identifier enumbase? ';'
;

enumkey
:
	Enum
	| Enum Class
	| Enum Struct
;

enumbase
:
	':' typespecifierseq
;

enumeratorlist
:
	enumeratordefinition
	| enumeratorlist ',' enumeratordefinition
;

enumeratordefinition
:
	enumerator
	| enumerator '=' constantexpression
;

enumerator
:
	Identifier
;

namespacename
:
	originalnamespacename
	| namespacealias
;

originalnamespacename
:
	Identifier
;

namespacedefinition
:
	namednamespacedefinition
	| unnamednamespacedefinition
;

namednamespacedefinition
:
	originalnamespacedefinition
	| extensionnamespacedefinition
;

originalnamespacedefinition
:
	Inline? Namespace Identifier '{' namespacebody '}'
;

extensionnamespacedefinition
:
	Inline? Namespace originalnamespacename '{' namespacebody '}'
;

unnamednamespacedefinition
:
	Inline? Namespace '{' namespacebody '}'
;

namespacebody
:
	declarationseq?
;

namespacealias
:
	Identifier
;

namespacealiasdefinition
:
	Namespace Identifier '=' qualifiednamespacespecifier ';'
;

qualifiednamespacespecifier
:
	nestednamespecifier? namespacename
;

usingdeclaration
:
	Using Typename? nestednamespecifier unqualifiedid ';'
	| Using '::' unqualifiedid ';'
;

usingdirective
:
	attributespecifierseq? Using Namespace nestednamespecifier? namespacename ';'
;

asmdefinition
:
	Asm '(' Stringliteral ')' ';'
;

linkagespecification
:
	Extern Stringliteral '{' declarationseq? '}'
	| Extern Stringliteral declaration
;

attributespecifierseq
:
	attributespecifier
	| attributespecifierseq attributespecifier
;

attributespecifier
:
	'[' '[' attributelist ']' ']'
	| alignmentspecifier
;

alignmentspecifier
:
	Alignas '(' typeid '...'? ')'
	| Alignas '(' constantexpression '...'? ')'
;

attributelist
:
	attribute?
	| attributelist ',' attribute?
	| attribute '...'
	| attributelist ',' attribute '...'
;

attribute
:
	attributetoken attributeargumentclause?
;

attributetoken
:
	Identifier
	| attributescopedtoken
;

attributescopedtoken
:
	attributenamespace '::' Identifier
;

attributenamespace
:
	Identifier
;

attributeargumentclause
:
	'(' balancedtokenseq ')'
;

balancedtokenseq
:
	balancedtoken?
	| balancedtokenseq balancedtoken
;

balancedtoken
:
	'(' balancedtokenseq ')'
	| '[' balancedtokenseq ']'
	| '{' balancedtokenseq '}'
	/*any token other than a parenthesis , a bracket , or a brace*/
;

/*Declarators*/
initdeclaratorlist
:
	initdeclarator
	| initdeclaratorlist ',' initdeclarator
;

initdeclarator
:
	declarator initializer?
;

declarator
:
	ptrdeclarator
	| noptrdeclarator parametersandqualifiers trailingreturntype
;

ptrdeclarator
:
	noptrdeclarator
	| ptroperator ptrdeclarator
;

noptrdeclarator
:
	declaratorid attributespecifierseq?
	| noptrdeclarator parametersandqualifiers
	| noptrdeclarator '[' constantexpression? ']' attributespecifierseq?
	| '(' ptrdeclarator ')'
;

parametersandqualifiers
:
	'(' parameterdeclarationclause ')' cvqualifierseq? refqualifier?
	exceptionspecification? attributespecifierseq?
;

trailingreturntype
:
	'->' trailingtypespecifierseq abstractdeclarator?
;

ptroperator
:
	'*' attributespecifierseq? cvqualifierseq?
	| '&' attributespecifierseq?
	| '&&' attributespecifierseq?
	| nestednamespecifier '*' attributespecifierseq? cvqualifierseq?
;

cvqualifierseq
:
	cvqualifier cvqualifierseq?
;

cvqualifier
:
	Const
	| Volatile
;

refqualifier
:
	'&'
	| '&&'
;

declaratorid
:
	'...'? idexpression
;

typeid
:
	typespecifierseq abstractdeclarator?
;

abstractdeclarator
:
	ptrabstractdeclarator
	| noptrabstractdeclarator? parametersandqualifiers trailingreturntype
	| abstractpackdeclarator
;

ptrabstractdeclarator
:
	noptrabstractdeclarator
	| ptroperator ptrabstractdeclarator?
;

noptrabstractdeclarator
:
	noptrabstractdeclarator parametersandqualifiers
	| parametersandqualifiers
	| noptrabstractdeclarator '[' constantexpression? ']' attributespecifierseq?
	| '[' constantexpression? ']' attributespecifierseq?
	| '(' ptrabstractdeclarator ')'
;

abstractpackdeclarator
:
	noptrabstractpackdeclarator
	| ptroperator abstractpackdeclarator
;

noptrabstractpackdeclarator
:
	noptrabstractpackdeclarator parametersandqualifiers
	| noptrabstractpackdeclarator '[' constantexpression? ']'
	attributespecifierseq?
	| '...'
;

parameterdeclarationclause
:
	parameterdeclarationlist? '...'?
	| parameterdeclarationlist ',' '...'
;

parameterdeclarationlist
:
	parameterdeclaration
	| parameterdeclarationlist ',' parameterdeclaration
;

parameterdeclaration
:
	attributespecifierseq? declspecifierseq declarator
	| attributespecifierseq? declspecifierseq declarator '=' initializerclause
	| attributespecifierseq? declspecifierseq abstractdeclarator?
	| attributespecifierseq? declspecifierseq abstractdeclarator? '='
	initializerclause
;

functiondefinition
:
	attributespecifierseq? declspecifierseq? declarator virtspecifierseq?
	functionbody
;

functionbody
:
	ctorinitializer? compoundstatement
	| functiontryblock
	| '=' Default ';'
	| '=' Delete ';'
;

initializer
:
	braceorequalinitializer
	| '(' expressionlist ')'
;

braceorequalinitializer
:
	'=' initializerclause
	| bracedinitlist
;

initializerclause
:
	assignmentexpression
	| bracedinitlist
;

initializerlist
:
	initializerclause '...'?
	| initializerlist ',' initializerclause '...'?
;

bracedinitlist
:
	'{' initializerlist ','? '}'
	| '{' '}'
;

/*Classes*/
classname
:
	Identifier
	| simpletemplateid
;

classspecifier
:
	classhead '{' memberspecification? '}'
;

classhead
:
	classkey attributespecifierseq? classheadname classvirtspecifier? baseclause?
	| classkey attributespecifierseq? baseclause?
;

classheadname
:
	nestednamespecifier? classname
;

classvirtspecifier
:
	Final
;

classkey
:
	Class
	| Struct
	| Union
;

memberspecification
:
	memberdeclaration memberspecification?
	| accessspecifier ':' memberspecification?
;

memberdeclaration
:
	attributespecifierseq? declspecifierseq? memberdeclaratorlist? ';'
	| functiondefinition
	| usingdeclaration
	| static_assertdeclaration
	| templatedeclaration
	| aliasdeclaration
	| emptydeclaration
;

memberdeclaratorlist
:
	memberdeclarator
	| memberdeclaratorlist ',' memberdeclarator
;

memberdeclarator
:
	declarator virtspecifierseq? purespecifier?
	| declarator braceorequalinitializer?
	| Identifier? attributespecifierseq? ':' constantexpression
;

virtspecifierseq
:
	virtspecifier
	| virtspecifierseq virtspecifier
;

virtspecifier
:
	Override
	| Final
;

/*
purespecifier:
	'=' '0'//Conflicts with the lexer
 ;
 */
purespecifier
:
	Assign val = Octalliteral
	{if($val.text.compareTo("0")!=0) throw new InputMismatchException(this);}

;

/*Derived classes*/
baseclause
:
	':' basespecifierlist
;

basespecifierlist
:
	basespecifier '...'?
	| basespecifierlist ',' basespecifier '...'?
;

basespecifier
:
	attributespecifierseq? basetypespecifier
	| attributespecifierseq? Virtual accessspecifier? basetypespecifier
	| attributespecifierseq? accessspecifier Virtual? basetypespecifier
;

classordecltype
:
	nestednamespecifier? classname
	| decltypespecifier
;

basetypespecifier
:
	classordecltype
;

accessspecifier
:
	Private
	| Protected
	| Public
;

/*Special member functions*/
conversionfunctionid
:
	Operator conversiontypeid
;

conversiontypeid
:
	typespecifierseq conversiondeclarator?
;

conversiondeclarator
:
	ptroperator conversiondeclarator?
;

ctorinitializer
:
	':' meminitializerlist
;

meminitializerlist
:
	meminitializer '...'?
	| meminitializer '...'? ',' meminitializerlist
;

meminitializer
:
	meminitializerid '(' expressionlist? ')'
	| meminitializerid bracedinitlist
;

meminitializerid
:
	classordecltype
	| Identifier
;

/*Overloading*/
operatorfunctionid
:
	Operator operator
;

literaloperatorid
:
	Operator Stringliteral Identifier
	| Operator Userdefinedstringliteral
;

/*Templates*/
templatedeclaration
:
	Template '<' templateparameterlist '>' declaration
;

templateparameterlist
:
	templateparameter
	| templateparameterlist ',' templateparameter
;

templateparameter
:
	typeparameter
	| parameterdeclaration
;

typeparameter
:
	Class '...'? Identifier?
	| Class Identifier? '=' typeid
	| Typename '...'? Identifier?
	| Typename Identifier? '=' typeid
	| Template '<' templateparameterlist '>' Class '...'? Identifier?
	| Template '<' templateparameterlist '>' Class Identifier? '=' idexpression
;

simpletemplateid
:
	templatename '<' templateargumentlist? '>'
;

templateid
:
	simpletemplateid
	| operatorfunctionid '<' templateargumentlist? '>'
	| literaloperatorid '<' templateargumentlist? '>'
;

templatename
:
	Identifier
;

templateargumentlist
:
	templateargument '...'?
	| templateargumentlist ',' templateargument '...'?
;

templateargument
:
	constantexpression
	| typeid
	| idexpression
;

typenamespecifier
:
	Typename nestednamespecifier Identifier
	| Typename nestednamespecifier Template? simpletemplateid
;

explicitinstantiation
:
	Extern? Template declaration
;

explicitspecialization
:
	Template '<' '>' declaration
;

/*Exception handling*/
tryblock
:
	Try compoundstatement handlerseq
;

functiontryblock
:
	Try ctorinitializer? compoundstatement handlerseq
;

handlerseq
:
	handler handlerseq?
;

handler
:
	Catch '(' exceptiondeclaration ')' compoundstatement
;

exceptiondeclaration
:
	attributespecifierseq? typespecifierseq declarator
	| attributespecifierseq? typespecifierseq abstractdeclarator?
	| '...'
;

throwexpression
:
	Throw assignmentexpression?
;

exceptionspecification
:
	dynamicexceptionspecification
	| noexceptspecification
;

dynamicexceptionspecification
:
	Throw '(' typeidlist? ')'
;

typeidlist
:
	typeid '...'?
	| typeidlist ',' typeid '...'?
;

noexceptspecification
:
	Noexcept '(' constantexpression ')'
	| Noexcept
;

/*Preprocessing directives*/
Directive
:
	'#' ~[\r\n]* -> skip
;

/*Lexer*/

/*Keywords*/
Alignas
:
	'alignas'
;

Alignof
:
	'alignof'
;

Asm
:
	'asm'
;

Auto
:
	'auto'
;

Bool
:
	'bool'
;

Break
:
	'break'
;

Case
:
	'case'
;

Catch
:
	'catch'
;

Char
:
	'char'
;

Char16
:
	'char16_t'
;

Char32
:
	'char32_t'
;

Class
:
	'class'
;

Const
:
	'const'
;

Constexpr
:
	'constexpr'
;

Const_cast
:
	'const_cast'
;

Continue
:
	'continue'
;

Decltype
:
	'decltype'
;

Default
:
	'default'
;

Delete
:
	'delete'
;

Do
:
	'do'
;

Double
:
	'double'
;

Dynamic_cast
:
	'dynamic_cast'
;

Else
:
	'else'
;

Enum
:
	'enum'
;

Explicit
:
	'explicit'
;

Export
:
	'export'
;

Extern
:
	'extern'
;

False
:
	'false'
;

Final
:
	'final'
;

Float
:
	'float'
;

For
:
	'for'
;

Friend
:
	'friend'
;

Goto
:
	'goto'
;

If
:
	'if'
;

Inline
:
	'inline'
;

Int
:
	'int'
;

Long
:
	'long'
;

Mutable
:
	'mutable'
;

Namespace
:
	'namespace'
;

New
:
	'new'
;

Noexcept
:
	'noexcept'
;

Nullptr
:
	'nullptr'
;

Operator
:
	'operator'
;

Override
:
	'override'
;

Private
:
	'private'
;

Protected
:
	'protected'
;

Public
:
	'public'
;

Register
:
	'register'
;

Reinterpret_cast
:
	'reinterpret_cast'
;

Return
:
	'return'
;

Short
:
	'short'
;

Signed
:
	'signed'
;

Sizeof
:
	'sizeof'
;

Static
:
	'static'
;

Static_assert
:
	'static_assert'
;

Static_cast
:
	'static_cast'
;

Struct
:
	'struct'
;

Switch
:
	'switch'
;

Template
:
	'template'
;

This
:
	'this'
;

Thread_local
:
	'thread_local'
;

Throw
:
	'throw'
;

True
:
	'true'
;

Try
:
	'try'
;

Typedef
:
	'typedef'
;

Typeid
:
	'typeid'
;

Typename
:
	'typename'
;

Union
:
	'union'
;

Unsigned
:
	'unsigned'
;

Using
:
	'using'
;

Virtual
:
	'virtual'
;

Void
:
	'void'
;

Volatile
:
	'volatile'
;

Wchar
:
	'wchar_t'
;

While
:
	'while'
;

/*Operators*/
LeftParen
:
	'('
;

RightParen
:
	')'
;

LeftBracket
:
	'['
;

RightBracket
:
	']'
;

LeftBrace
:
	'{'
;

RightBrace
:
	'}'
;

Plus
:
	'+'
;

Minus
:
	'-'
;

Star
:
	'*'
;

Div
:
	'/'
;

Mod
:
	'%'
;

Caret
:
	'^'
;

And
:
	'&'
;

Or
:
	'|'
;

Tilde
:
	'~'
;

Not
:
	'!'
;

Assign
:
	'='
;

Less
:
	'<'
;

Greater
:
	'>'
;

PlusAssign
:
	'+='
;

MinusAssign
:
	'-='
;

StarAssign
:
	'*='
;

DivAssign
:
	'/='
;

ModAssign
:
	'%='
;

XorAssign
:
	'^='
;

AndAssign
:
	'&='
;

OrAssign
:
	'|='
;

LeftShift
:
	'<<'
;

rightShift
:
//'>>'
	Greater Greater
;

LeftShiftAssign
:
	'<<='
;

rightShiftAssign
:
//'>>='
	Greater Greater Assign
;

Equal
:
	'=='
;

NotEqual
:
	'!='
;

LessEqual
:
	'<='
;

GreaterEqual
:
	'>='
;

AndAnd
:
	'&&'
;

OrOr
:
	'||'
;

PlusPlus
:
	'++'
;

MinusMinus
:
	'--'
;

Comma
:
	','
;

ArrowStar
:
	'->*'
;

Arrow
:
	'->'
;

Question
:
	'?'
;

Colon
:
	':'
;

Doublecolon
:
	'::'
;

Semi
:
	';'
;

Dot
:
	'.'
;

DotStar
:
	'.*'
;

Ellipsis
:
	'...'
;

operator
:
	New
	| Delete
	| New '[' ']'
	| Delete '[' ']'
	| '+'
	| '-'
	| '*'
	| '/'
	| '%'
	| '^'
	| '&'
	| '|'
	| '~'
	| '!'
	| '='
	| '<'
	| '>'
	| '+='
	| '-='
	| '*='
	| '/='
	| '%='
	| '^='
	| '&='
	| '|='
	| '<<'
	| rightShift
	| rightShiftAssign
	| '<<='
	| '=='
	| '!='
	| '<='
	| '>='
	| '&&'
	| '||'
	| '++'
	| '--'
	| ','
	| '->*'
	| '->'
	| '(' ')'
	| '[' ']'
;

/*Lexer*/
fragment
Hexquad
:
	HEXADECIMALDIGIT HEXADECIMALDIGIT HEXADECIMALDIGIT HEXADECIMALDIGIT
;

fragment
Universalcharactername
:
	'\\u' Hexquad
	| '\\U' Hexquad Hexquad
;

Identifier
:
/*
	Identifiernondigit
	| Identifier Identifiernondigit
	| Identifier DIGIT
	*/
	Identifiernondigit
	(
		Identifiernondigit
		| DIGIT
	)*
;

fragment
Identifiernondigit
:
	NONDIGIT
	| Universalcharactername
	/* other implementation defined characters*/
;

fragment
NONDIGIT
:
	[a-zA-Z_]
;

fragment
DIGIT
:
	[0-9]
;

literal
:
	Integerliteral
	| Characterliteral
	| Floatingliteral
	| Stringliteral
	| booleanliteral
	| pointerliteral
	| userdefinedliteral
;

Integerliteral
:
	Decimalliteral Integersuffix?
	| Octalliteral Integersuffix?
	| Hexadecimalliteral Integersuffix?
	| Binaryliteral Integersuffix?
;

Decimalliteral
:
	NONZERODIGIT
	(
		'\''? DIGIT
	)*
;

Octalliteral
:
	'0'
	(
		'\''? OCTALDIGIT
	)*
;

Hexadecimalliteral
:
	(
		'0x'
		| '0X'
	) HEXADECIMALDIGIT
	(
		'\''? HEXADECIMALDIGIT
	)*
;

Binaryliteral
:
	(
		'0b'
		| '0B'
	) BINARYDIGIT
	(
		'\''? BINARYDIGIT
	)*
;

fragment
NONZERODIGIT
:
	[1-9]
;

fragment
OCTALDIGIT
:
	[0-7]
;

fragment
HEXADECIMALDIGIT
:
	[0-9a-fA-F]
;

fragment
BINARYDIGIT
:
	[01]
;

Integersuffix
:
	Unsignedsuffix Longsuffix?
	| Unsignedsuffix Longlongsuffix?
	| Longsuffix Unsignedsuffix?
	| Longlongsuffix Unsignedsuffix?
;

fragment
Unsignedsuffix
:
	[uU]
;

fragment
Longsuffix
:
	[lL]
;

fragment
Longlongsuffix
:
	'll'
	| 'LL'
;

Characterliteral
:
	'\'' Cchar+ '\''
	| 'u' '\'' Cchar+ '\''
	| 'U' '\'' Cchar+ '\''
	| 'L' '\'' Cchar+ '\''
;

fragment
Cchar
:
	~['\\\r\n]
	| Escapesequence
	| Universalcharactername
;

fragment
Escapesequence
:
	Simpleescapesequence
	| Octalescapesequence
	| Hexadecimalescapesequence
;

fragment
Simpleescapesequence
:
	'\\\''
	| '\\"'
	| '\\?'
	| '\\\\'
	| '\\a'
	| '\\b'
	| '\\f'
	| '\\n'
	| '\\r'
	| '\\t'
	| '\\v'
;

fragment
Octalescapesequence
:
	'\\' OCTALDIGIT
	| '\\' OCTALDIGIT OCTALDIGIT
	| '\\' OCTALDIGIT OCTALDIGIT OCTALDIGIT
;

fragment
Hexadecimalescapesequence
:
	'\\x' HEXADECIMALDIGIT+
;

Floatingliteral
:
	Fractionalconstant Exponentpart? Floatingsuffix?
	| Digitsequence Exponentpart Floatingsuffix?
;

fragment
Fractionalconstant
:
	Digitsequence? '.' Digitsequence
	| Digitsequence '.'
;

fragment
Exponentpart
:
	'e' SIGN? Digitsequence
	| 'E' SIGN? Digitsequence
;

fragment
SIGN
:
	[+-]
;

fragment
Digitsequence
:
	DIGIT
	(
		'\''? DIGIT
	)*
;

fragment
Floatingsuffix
:
	[flFL]
;

Stringliteral
:
	Encodingprefix? '"' Schar* '"'
	| Encodingprefix? 'R' Rawstring
;

fragment
Encodingprefix
:
	'u8'
	| 'u'
	| 'U'
	| 'L'
;

fragment
Schar
:
	~["\\\r\n]
	| Escapesequence
	| Universalcharactername
;

fragment
Rawstring /* '"' dcharsequence? '(' rcharsequence? ')' dcharsequence? '"' */
:
	'"' .*? '(' .*? ')' .*? '"'
;

booleanliteral
:
	False
	| True
;

pointerliteral
:
	Nullptr
;

userdefinedliteral
:
	Userdefinedintegerliteral
	| Userdefinedfloatingliteral
	| Userdefinedstringliteral
	| Userdefinedcharacterliteral
;

Userdefinedintegerliteral
:
	Decimalliteral Udsuffix
	| Octalliteral Udsuffix
	| Hexadecimalliteral Udsuffix
	| Binaryliteral Udsuffix
;

Userdefinedfloatingliteral
:
	Fractionalconstant Exponentpart? Udsuffix
	| Digitsequence Exponentpart Udsuffix
;

Userdefinedstringliteral
:
	Stringliteral Udsuffix
;

Userdefinedcharacterliteral
:
	Characterliteral Udsuffix
;

fragment
Udsuffix
:
	Identifier
;

Whitespace
:
	[ \t]+ -> skip
;

Newline
:
	(
		'\r' '\n'?
		| '\n'
	) -> skip
;

BlockComment
:
	'/*' .*? '*/' -> skip
;

LineComment
:
	'//' ~[\r\n]* -> skip
;
