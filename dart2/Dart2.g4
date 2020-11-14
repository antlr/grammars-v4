/*
 [The "BSD licence"]
 Copyright (c) 2019 Wener
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

AB: 13-Apr-19; newExpression conflict , renamed to nayaExpression
AB: 13-Apr-19; Replaced `type` with `dtype` to fix golang code gen.

*/

grammar Dart2;

compilationUnit: libraryDefinition | partDeclaration;

WHITESPACE
//  : ('\t' | ' ' | NEWLINE)+   -> skip
  :  [ \t\r\n\u000C]+ -> skip
  ;

// 8 Variables
variableDeclaration
  : declaredIdentifier (',' identifier)*
  ;

declaredIdentifier
  : metadata finalConstVarOrType identifier
  ;
finalConstVarOrType
  : 'final' dtype?
  | 'const' dtype?
  | varOrType
  ;
varOrType
  : 'var'
  | dtype
  ;

initializedVariableDeclaration
  : declaredIdentifier ('=' expression)? (','initializedIdentifier)*
  ;
initializedIdentifier
  : identifier ('=' expression)?
  ;
initializedIdentifierList
  : initializedIdentifier (',' initializedIdentifier)*
  ;




// 9 Functions
functionSignature
  : metadata returnType? identifier formalParameterPart
  ;
formalParameterPart
  : typeParameters? formalParameterList
  ;
returnType
  : 'void'
  | dtype
  ;

functionBody
  : 'async'? '=>' expression ';'
  | ('async' | 'async*' | 'sync*')? block
  ;
block
  : '{' statements '}'
  ;

// 9.2 Formal Parameters
formalParameterList
  : '(' ')'
  | '(' normalFormalParameters ')'
  | '(' normalFormalParameters (',' optionalFormalParameters)? ')'
  | '(' optionalFormalParameters ')'
  ;
normalFormalParameters
  : normalFormalParameter (',' normalFormalParameter)*
  ;
optionalFormalParameters
  : optionalPositionalFormalParameters
  | namedFormalParameters
  ;
optionalPositionalFormalParameters
  : '[' defaultFormalParameter (',' defaultFormalParameter)* ','? ']'
  ;
namedFormalParameters
  : '{' defaultNamedParameter (',' defaultNamedParameter)* ','? '}'
  ;

// 9.2.1 Required Formals
normalFormalParameter
  : functionFormalParameter
  | fieldFormalParameter
  | simpleFormalParameter
  ;
functionFormalParameter
  : metadata 'covariant'? returnType? identifier formalParameterPart
  ;
simpleFormalParameter
  : declaredIdentifier
  | metadata 'covariant'? identifier
  ;
fieldFormalParameter
  : metadata finalConstVarOrType? 'this' '.' identifier formalParameterPart?
  ;

// 9.2.2 Optional Formals
defaultFormalParameter
  : normalFormalParameter ('=' expression)?
  ;
defaultNamedParameter
  : normalFormalParameter ('=' expression)?
  | normalFormalParameter (':' expression)?
  ;

// 10 Classes
classDefinition
  : metadata 'abstract'? 'class' identifier typeParameters?
    superclass? mixins? interfaces?
    '{' (metadata classMemberDefinition)* '}'
  | metadata 'abstract'? 'class' mixinApplicationClass
;
mixins
  : 'with' typeList
  ;
classMemberDefinition
  : declaration ';'
  | methodSignature functionBody
  ;
methodSignature
  : constructorSignature initializers?
  | factoryConstructorSignature
  | 'static'? functionSignature
  | 'static'? getterSignature
  | 'static'? setterSignature
  | operatorSignature
  ;


declaration
  : constantConstructorSignature (redirection | initializers)?
  | constructorSignature (redirection | initializers)?
  | 'external' constantConstructorSignature
  | 'external' constructorSignature
  | ('external' 'static'?)? getterSignature
  | ('external' 'static'?)? setterSignature
  | 'external'? operatorSignature
  | ('external' 'static'?)? functionSignature
  | 'static' ('final' | 'const') dtype? staticFinalDeclarationList
  | 'final' dtype? initializedIdentifierList
  | ('static' | 'covariant')? ('var' | dtype) initializedIdentifierList
  ;

staticFinalDeclarationList
  : staticFinalDeclaration (',' staticFinalDeclaration)*
  ;
staticFinalDeclaration
  : identifier '=' expression
  ;

// 10.1.1 Operators
operatorSignature
  : returnType? 'operator' operator formalParameterList
  ;
operator
  : '~' | binaryOperator | '[]' | '[]='
  ;

binaryOperator
  : multiplicativeOperator
  | additiveOperator
  | shiftOperator
  | relationalOperator
  | '=='
  | bitwiseOperator
  ;
// 10.2 Getters
getterSignature
  : returnType? 'get' identifier
  ;
// 10.2 Setters
setterSignature
  : returnType? 'set' identifier formalParameterList
  ;

// 10.6 Constructors
constructorSignature
  : identifier ('.' identifier)? formalParameterList
  ;
redirection
  : ':' 'this' ('.' identifier)? arguments
  ;

initializers
  : ':' initializerListEntry (',' initializerListEntry)*
  ;
initializerListEntry
  : 'super' arguments
  | 'super' '.' identifier arguments
  | fieldInitializer
  | assertion
  ;
fieldInitializer
  : ('this' '.')? identifier '=' conditionalExpression cascadeSection*
  ;

// 10.6.2 Factories
factoryConstructorSignature
  : 'factory' identifier ('.' identifier)? formalParameterList
  ;
redirectingFactoryConstructorSignature
  : 'const'? 'factory' identifier ('.' identifier)? formalParameterList '='
    dtype ('.' identifier)?
  ;
// 10.6.3 Constant Constructors
constantConstructorSignature: 'const' qualified formalParameterList;

// 10.9 Supperclasses
superclass: 'extends' dtype;

// 10.10 SUperinterfaces
interfaces: 'implements' typeList;

// 12.1 Mixin Application
mixinApplicationClass
  : identifier typeParameters? '=' mixinApplication ';';
mixinApplication
  : dtype mixins interfaces?
  ;

// 13 Enums
enumType
  : metadata 'enum' identifier
    '{' enumEntry (',' enumEntry)* ','? '}'
  ;

enumEntry
  : metadata identifier
  ;

// 14 Generics
typeParameter
  : metadata identifier ('extends' dtype)?
  ;
typeParameters
  : '<' typeParameter (',' typeParameter)* '>'
  ;

// 15 Metadata
metadata
  : ('@' qualified ('.' identifier)? arguments?)*
  ;

// 16 Expressions
expression
  : assignableExpression assignmentOperator expression
  | conditionalExpression cascadeSection*
  | throwExpression
  ;
expressionWithoutCascade
  : assignableExpression assignmentOperator expressionWithoutCascade
  | conditionalExpression
  | throwExpressionWithoutCascade
  ;
expressionList
  : expression (',' expression)*
  ;
primary
  : thisExpression
  | 'super' unconditionalAssignableSelector
  | functionExpression
  | literal
  | identifier
  | nayaExpression
  | constObjectExpression
  | '(' expression ')'
  ;

// 16.1 Constants

literal
  : nullLiteral
  | booleanLiteral
  | numericLiteral
  | stringLiteral
  | symbolLiteral
  | mapLiteral
  | listLiteral
  ;
nullLiteral: 'null';

numericLiteral
  : NUMBER
  | HEX_NUMBER
  ;

NUMBER
  : DIGIT+ ('.' DIGIT+)? EXPONENT?
  | '.' DIGIT+ EXPONENT?
  ;
fragment
EXPONENT
  : ('e' | 'E') ('+' | '-')? DIGIT+
  ;
HEX_NUMBER
  : '0x' HEX_DIGIT+
  | '0X' HEX_DIGIT+
  ;
fragment
HEX_DIGIT
  : [a-f]
  | [A-F]
  | DIGIT
  ;

booleanLiteral
  : 'true'
  | 'false'
  ;

stringLiteral: (MultiLineString | SingleLineString)+;

SingleLineString
  : '"' StringContentDQ* '"'
  | '\'' StringContentSQ* '\''
  | 'r\'' (~('\'' | '\n' | '\r'))* '\''
  | 'r"' (~('"' | '\n' | '\r'))* '"'
  ;

fragment
StringContentDQ
  : ~('\\' | '"' /*| '$'*/ | '\n' | '\r')
  | '\\' ~('\n' | '\r')
  //| stringInterpolation
  ;

fragment
StringContentSQ
  : ~('\\' | '\'' /*| '$'*/ | '\n' | '\r')
  | '\\' ~('\n' | '\r')
  //| stringInterpolation
  ;

MultiLineString
  : '"""' StringContentTDQ* '"""'
  | '\'\'\'' StringContentTSQ* '\'\'\''
  | 'r"""' (~'"' | '"' ~'"' | '""' ~'"')* '"""'
  | 'r\'\'\'' (~'\'' | '\'' ~'\'' | '\'\'' ~'\'')* '\'\'\''
  ;

fragment
StringContentTDQ
  : ~('\\' | '"' /*| '$'*/)
  | '"' ~'"' | '""' ~'"'
  //| stringInterpolation
  ;

fragment StringContentTSQ
  : ~('\\' | '\'' /*| '$'*/)
  | '\'' ~'\'' | '\'\'' ~'\''
  //| stringInterpolation
  ;

NEWLINE
  : '\n'
  | '\r'
  | '\r\n'
  ;

// 16.5.1 String Interpolation
stringInterpolation
//  : '$' IDENTIFIER_NO_DOLLAR
  : '$' identifier// FIXME
  | '${' expression '}'
  ;

// 16.6 Symbols
symbolLiteral
  : '#' (operator | (identifier (',' identifier)*))
  ;
// 16.7 Lists
listLiteral
  : 'const'? typeArguments? '[' (expressionList ','?)? ']'
  ;

// 16.8 Maps
mapLiteral
  : 'const'? typeArguments?
    '{' (mapLiteralEntry (',' mapLiteralEntry)* ','?)? '}'
;
mapLiteralEntry
  : expression ':' expression
  ;

// 16.9 Throw
throwExpression
  : 'throw' expression
  ;
throwExpressionWithoutCascade
  : 'throw' expressionWithoutCascade
  ;

// 16.10 Function Expressions
functionExpression
  : formalParameterPart functionBody
  ;

// 16.11 This
thisExpression: 'this';

// 16.12.1 New
nayaExpression
  : 'new' dtype ('.' identifier)? arguments
  ;

// 16.12.2 Const
constObjectExpression
  : 'const' dtype ('.' identifier)? arguments
  ;

// 16.14.1 Actual Argument List Evaluation
arguments
  : '(' (argumentList ','?)? ')'
  ;
argumentList
  : namedArgument (',' namedArgument)*
  | expressionList (',' namedArgument)*
  ;
namedArgument
  : label expression
  ;

// 16.18.2 Cascaded Invocations
cascadeSection
  : '..' (cascadeSelector argumentPart*)
         (assignableSelector argumentPart*)*
         (assignmentOperator expressionWithoutCascade)?
  ;
cascadeSelector
  : '[' expression ']'
  | identifier
  ;
argumentPart
  : typeArguments? arguments
  ;

// 16.20 Assignment
assignmentOperator
  : '='
  | compoundAssignmentOperator
  ;

// 16.20.1 Compound Assignment
compoundAssignmentOperator
  : '*='
  | '/='
  | '~/='
  | '%='
  | '+='
  | '<<='
  | '>>='
  | '>>>='
  | '&='
  | '^='
  | '|='
  | '??='
  ;

// 16.21 Conditional
conditionalExpression
  : ifNullExpression
    ('?' expressionWithoutCascade ':' expressionWithoutCascade)?
  ;
// 16.22 If-null Expression
ifNullExpression
  : logicalOrExpression ('??' logicalOrExpression)*
  ;

// 16.23 Logical Boolean Expressions
logicalOrExpression
  : logicalAndExpression ('||' logicalAndExpression)*
  ;
logicalAndExpression
  : equalityExpression ('&&' equalityExpression)*
  ;

// 16.24 Equality
equalityExpression
  : relationalExpression (equalityOperator relationalExpression)?
  | 'super' equalityOperator relationalExpression
  ;
equalityOperator
  : '=='
  | '!='
  ;

// 16.25 Relational Expressions
relationalExpression
  : bitwiseOrExpression
    (
      typeTest
      | typeCast
      | relationalOperator bitwiseOrExpression
    )?
  | 'super' relationalOperator bitwiseOrExpression
  ;
relationalOperator
  : '>='
  | '>'
  | '<='
  | '<'
  ;

// 16.26 Bitwize Expression
bitwiseOrExpression
  : bitwiseXorExpression ('|' bitwiseXorExpression)*
  | 'super' ('|' bitwiseOrExpression)+
  ;
bitwiseXorExpression
  : bitwiseAndExpression ('^' bitwiseAndExpression)*
  | 'super' ('^' bitwiseAndExpression)+
  ;
bitwiseAndExpression
  : shiftExpression ('&' shiftExpression)*
  | 'super' ('&' shiftExpression)+
  ;
bitwiseOperator
  : '&'
  | '^'
  | '|'
  ;

// 16.27 Shift
shiftExpression
  : additiveExpression (shiftOperator additiveExpression)*
  | 'super' (shiftOperator additiveExpression)+
  ;
shiftOperator
  : '<<'
  | '>>'
  | '>>>'
  ;

// 16.28 Additive Expression
additiveExpression
  : multiplicativeExpression (additiveOperator multiplicativeExpression)*
  | 'super' (additiveOperator multiplicativeExpression)+
  ;
additiveOperator
  : '+'
  | '-'
  ;
// 16.29 Multiplicative Expression
multiplicativeExpression
  : unaryExpression (multiplicativeOperator unaryExpression)*
  | 'super' (multiplicativeOperator unaryExpression)+
  ;
multiplicativeOperator
  : '*'
  | '/'
  | '%'
  | '~/'
  ;

// 16.30 Unary Expression
unaryExpression
  : prefixOperator unaryExpression
  | awaitExpression
  | postfixExpression
  | (minusOperator | tildeOperator) 'super'
  | incrementOperator assignableExpression
  ;
prefixOperator
  : minusOperator
  | negationOperator
  | tildeOperator
  ;
minusOperator: '-';
negationOperator: '!';
tildeOperator: '~';

// 16.31 Await Expressions
awaitExpression
  : 'await' unaryExpression
  ;

// 16.32 Postfix Expressions
postfixExpression
  : assignableExpression postfixOperator
  | primary selector*
  ;
postfixOperator
  : incrementOperator
  ;
selector
  : assignableSelector
  | argumentPart
  ;

incrementOperator
  : '++'
  | '--'
  ;
// 16.33 Assignable Expressions
// NOTE
// primary (argumentPart* assignableSelector)+ -> primary (argumentPart* assignableSelector)?
assignableExpression
  : primary (argumentPart* assignableSelector)?
  | 'super' unconditionalAssignableSelector identifier
  ;
unconditionalAssignableSelector
  : '[' expression ']'
  | '.' identifier
  ;
assignableSelector
  : unconditionalAssignableSelector
  | '?.' identifier
  ;

identifier
  : IDENTIFIER
  ;
qualified
  : identifier ('.' identifier)?
  ;
// 16.35 Type Test
typeTest
  : isOperator dtype
  ;
isOperator
  : 'is' '!'?
  ;

// 16.36 Type Cast
typeCast
  : asOperator dtype
  ;
asOperator
  : 'as'
  ;
// 17 Statements
statements
  : statement*
  ;
statement
  : label* nonLabledStatment
  ;
nonLabledStatment
  : block
  | localVariableDeclaration
  | forStatement
  | whileStatement
  | doStatement
  | switchStatement
  | ifStatement
  | rethrowStatment
  | tryStatement
  | breakStatement
  | continueStatement
  | returnStatement
  | yieldStatement
  | yieldEachStatement
  | expressionStatement
  | assertStatement
  | localFunctionDeclaration
  ;

// 17.2 Expression Statements
expressionStatement
  : expression? ';'
  ;

// 17.3 Local Variable Declaration
localVariableDeclaration
  : initializedVariableDeclaration ';'
  ;
// 17.4 Local Function Declaration
localFunctionDeclaration
  : functionSignature functionBody
  ;
// 17.5 If
ifStatement
  : 'if' '(' expression ')' statement ('else' statement)?
  ;

// 17.6 For for
forStatement
  : 'await'? 'for' '(' forLoopParts ')' statement
  ;
forLoopParts
  : forInitializerStatement expression? ';' expressionList?
  | declaredIdentifier 'in' expression
  | identifier 'in' expression
  ;
forInitializerStatement
  : localVariableDeclaration
  | expression? ';'
  ;

// 17.7 While

whileStatement
  : 'while' '(' expression ')' statement
  ;
// 17.8 Do
doStatement
  : 'do' statement 'while' '(' expression ')' ';'
  ;
// 17.9 Switch
switchStatement
  : 'switch'  '(' expression ')' '{' switchCase* defaultCase? '}'
  ;
switchCase
  : label* 'case' expression ':' statements
  ;
defaultCase
  : label* 'default' ':' statements
  ;

// 17.10 Rethrow
rethrowStatment
  : 'rethrow' ';'
  ;

// 17.11 Try
tryStatement
  : 'try' block (onPart+ finallyPart? | finallyPart)
  ;
onPart
  : catchPart block
  | 'on' dtype catchPart? block
  ;
catchPart
  : 'catch' '(' identifier (',' identifier)? ')'
  ;
finallyPart
  : 'finally' block
  ;

// 17.12 Return

returnStatement
  : 'return' expression? ';'
  ;

// 17.13 Labels
label
  : identifier ':'
  ;

// 17.13 Break
breakStatement
  : 'break' identifier? ';'
  ;

// 17.13 Continue
continueStatement
  : 'continue' identifier? ';'
  ;

// 17.16.1 Yield
yieldStatement
  : 'yield' expression ';'
  ;
// 17.16.1 Yield-Each
yieldEachStatement
  : 'yield*' expression ';'
  ;

// 17.17 Assert
assertStatement
  : assertion ';'
  ;
assertion
  : 'assert' '(' expression (',' expression )? ','? ')'
  ;

// 18 Libraries and Scripts
topLevelDefinition
  : classDefinition
  | enumType
  | typeAlias
  | 'external'? functionSignature ';'
  | 'external'? getterSignature ';'
  | 'external'? setterSignature ';'
  | functionSignature functionBody
  | returnType? 'get' identifier functionBody
  | returnType? 'set' identifier formalParameterList functionBody
  | ('final' | 'const') dtype? staticFinalDeclarationList ';'
  | variableDeclaration ';'
  ;
getOrSet
  : 'get'
  | 'set'
  ;
libraryDefinition
  : scriptTag? libraryName? importOrExport* partDirective*
    topLevelDefinition*
  ;
scriptTag
  :  '#!' (~NEWLINE)* NEWLINE
  ;

libraryName
  : metadata 'library' dottedIdentifierList ';'
  ;
importOrExport
  : libraryimport
  | libraryExport
  ;
dottedIdentifierList
  : identifier (',' identifier)*
  ;

libraryimport
  : metadata importSpecification
  ;

importSpecification
  : 'import' configurableUri ('as' identifier)? combinator* ';'
//  | 'import' uri 'deferred' 'as' identifier combinator* ';'
  ;

combinator
  : 'show' identifierList
  | 'hide' identifierList
  ;
identifierList
  : identifier (',' identifier)*
  ;

// 18.2 Exports
libraryExport
  : metadata 'export' configurableUri combinator* ';'
  ;

// 18.3 Parts
partDirective
  : metadata 'part' uri ';'
  ;
partHeader
  : metadata 'part' 'of' identifier ('.' identifier)* ';'
  ;
partDeclaration
  : partHeader topLevelDefinition* EOF
  ;

// 18.5 URIs
uri
  : stringLiteral
  ;
configurableUri
  : uri configurationUri*
  ;
configurationUri
  : 'if' '(' uriTest ')' uri
  ;
uriTest
  : dottedIdentifierList ('==' stringLiteral)?
  ;

// 19.1 Static Types
dtype
  : typeName typeArguments?
  ;
typeName
  : qualified
  | 'void' // SyntaxFix
  ;
typeArguments
  : '<' typeList '>'
  ;
typeList
  : dtype (',' dtype)*
  ;

// 19.3.1 Typedef
typeAlias
  : metadata 'typedef' typeAliasBody
  ;
typeAliasBody
  : functionTypeAlias
  ;
functionTypeAlias
  : functionPrefix typeParameters? formalParameterList ';'
  ;
functionPrefix
  : returnType? identifier
  ;

// 20.2 Lexical Rules
// 20.1.1 Reserved Words
//assert, break, case, catch, class, const, continue, default, do, else,
//enum, extends, false, final, finally, for, if, in, is, new, null, rethrow,
//return, super, switch, this, throw, true, try, var, void, while, with.
fragment
IDENTIFIER_NO_DOLLAR
  : IDENTIFIER_START_NO_DOLLAR
    IDENTIFIER_PART_NO_DOLLAR*
  ;
IDENTIFIER
  : IDENTIFIER_START IDENTIFIER_PART*
  ;

//BUILT_IN_IDENTIFIER
//  : 'abstract'
//  | 'as'
//  | 'covariant'
//  | 'deferred'
//  | 'dynamic'
//  | 'export'
//  | 'external'
//  | 'factory'
//  | 'Function'
//  | 'get'
//  | 'implements'
//  | 'import'
//  | 'interface'
//  | 'library'
//  | 'operator'
//  | 'mixin'
//  | 'part'
//  | 'set'
//  | 'static'
//  | 'typedef'
//  ;
fragment
IDENTIFIER_START
  : IDENTIFIER_START_NO_DOLLAR
  | '$'
  ;
fragment
IDENTIFIER_START_NO_DOLLAR
  : LETTER
  | '_'
  ;
fragment
IDENTIFIER_PART_NO_DOLLAR
  : IDENTIFIER_START_NO_DOLLAR
  | DIGIT
  ;
fragment
IDENTIFIER_PART
  : IDENTIFIER_START
  | DIGIT
  ;

// 20.1.1 Reserved Words
fragment
LETTER
  : [a-z]
  | [A-Z]
  ;
fragment
DIGIT
  : [0-9]
  ;
// 20.1.2 Comments
SINGLE_LINE_COMMENT
//  : '//' ~(NEWLINE)* (NEWLINE)? // Origin Syntax
  : '//' ~[\r\n]* -> skip
  ;
MULTI_LINE_COMMENT
//  : '/*' (MULTI_LINE_COMMENT | ~'*/')* '*/' // Origin Syntax
  : '/*' .*? '*/' -> skip
  ;

