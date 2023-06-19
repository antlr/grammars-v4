/*
 [The "BSD licence"]
 Copyright (c) 2013 Terence Parr, Sam Harwell
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

/**
 *  An Apexcode grammar derived from Java 1.7 grammar for ANTLR v4.
 *  Uses ANTLR v4's left-recursive expression notation.
 *
 *  @maintainer: Andrey Gavrikov
 *
 *  You can test with
 *
 *  $ antlr4 apex.g4
 *  $ javac *.java
 *  $ grun apex compilationUnit *.cls
 *
 *  How to run example: https://mohan-chinnappan-n5.github.io/sfbooks/sfdevnotes/language-design/apex-grammar.html 
 */
grammar apex;

// starting point for parsing a apexcode file
compilationUnit
    :   packageDeclaration? importDeclaration* typeDeclaration* EOF
    ;

packageDeclaration
    :   annotation* PACKAGE qualifiedName ';'
    ;

importDeclaration
    :   IMPORT STATIC? qualifiedName ('.' '*')? ';'
    ;

typeDeclaration
    :   classOrInterfaceModifier* classDeclaration
    |   classOrInterfaceModifier* enumDeclaration
    |   classOrInterfaceModifier* interfaceDeclaration
    |   classOrInterfaceModifier* annotationTypeDeclaration
    |   ';'
    ;

modifier
    :   classOrInterfaceModifier
    |   NATIVE
    |   SYNCHRONIZED
    |   TRANSIENT
    ;

classOrInterfaceModifier
    :   annotation       // class or interface
    |   PUBLIC     // class or interface
    |   PROTECTED  // class or interface
    |   PRIVATE    // class or interface
    |   STATIC     // class or interface
    |   ABSTRACT   // class or interface
    |   FINAL      // class only -- does not apply to interfaces
    |   GLOBAL     // class or interface
    |   WEBSERVICE // class only -- does not apply to interfaces
    |   OVERRIDE   // method only
    |   VIRTUAL    // method only
    |   TESTMETHOD    // method only
    |   APEX_WITH_SHARING // class only
    |   APEX_WITHOUT_SHARING //class only
    ;

variableModifier
    :   FINAL
    |   annotation
    ;

classDeclaration
    :   CLASS Identifier typeParameters?
        (EXTENDS type_)?
        (IMPLEMENTS typeList)?
        classBody
    ;

typeParameters
    :   '<' typeParameter (',' typeParameter)* '>'
    ;

typeParameter
    :   Identifier (EXTENDS typeBound)?
    ;

typeBound
    :   type_ ('&' type_)*
    ;

enumDeclaration
    :   ENUM Identifier (IMPLEMENTS typeList)?
        '{' enumConstants? ','? enumBodyDeclarations? '}'
    ;

enumConstants
    :   enumConstant (',' enumConstant)*
    ;

enumConstant
    :   annotation* Identifier arguments? classBody?
    ;

enumBodyDeclarations
    :   ';' classBodyDeclaration*
    ;

interfaceDeclaration
    :   INTERFACE Identifier typeParameters? (EXTENDS typeList)? interfaceBody
    ;

typeList
    :   type_ (',' type_)*
    ;

classBody
    :   '{' classBodyDeclaration* '}'
    ;

interfaceBody
    :   '{' interfaceBodyDeclaration* '}'
    ;

classBodyDeclaration
    :   ';'
    |   STATIC? block
    |   modifier* memberDeclaration
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
    |   propertyDeclaration
    ;

/* We use rule this even for void methods which cannot have [] after parameters.
   This simplifies grammar and we can consider void to be a type, which
   renders the [] matching as a context-sensitive issue or a semantic check
   for invalid return type after parsing.
 */
methodDeclaration
    :   OVERRIDE? (type_|VOID) Identifier formalParameters ('[' ']')*
        (THROWS qualifiedNameList)?
        (   methodBody
        |   ';'
        )
    ;

genericMethodDeclaration
    :   typeParameters methodDeclaration
    ;

constructorDeclaration
    :   Identifier formalParameters (THROWS qualifiedNameList)?
        constructorBody
    ;

genericConstructorDeclaration
    :   typeParameters constructorDeclaration
    ;

fieldDeclaration
    :   type_ variableDeclarators ';'
    ;

propertyDeclaration
    :   type_ variableDeclarators propertyBodyDeclaration
    ;

propertyBodyDeclaration
    :   '{' propertyBlock propertyBlock? '}'
    ;

interfaceBodyDeclaration
    :   modifier* interfaceMemberDeclaration
    |   ';'
    ;

interfaceMemberDeclaration
    :   constDeclaration
    |   interfaceMethodDeclaration
    |   genericInterfaceMethodDeclaration
    |   interfaceDeclaration
    |   annotationTypeDeclaration
    |   classDeclaration
    |   enumDeclaration
    ;

constDeclaration
    :   type_ constantDeclarator (',' constantDeclarator)* ';'
    ;

constantDeclarator
    :   Identifier ('[' ']')* '=' variableInitializer
    ;

// see matching of [] comment in methodDeclaratorRest
interfaceMethodDeclaration
    :   (type_|VOID) Identifier formalParameters ('[' ']')*
        (THROWS qualifiedNameList)?
        ';'
    ;

genericInterfaceMethodDeclaration
    :   typeParameters interfaceMethodDeclaration
    ;

variableDeclarators
    :   variableDeclarator (',' variableDeclarator)*
    ;

variableDeclarator
    :   variableDeclaratorId ('=' variableInitializer)?
    ;

variableDeclaratorId
    :   Identifier ('[' ']')*
    ;

variableInitializer
    :   arrayInitializer
    |   expression
    ;

arrayInitializer
    :   '{' (variableInitializer (',' variableInitializer)* ','? )? '}'
    ;

enumConstantName
    :   Identifier
    ;

type_
    :   classOrInterfaceType ('[' ']')*
    |   primitiveType ('[' ']')*
    ;

classOrInterfaceType
    :   Identifier typeArguments? ('.' Identifier typeArguments? )*
    |   SET typeArguments // 'set <' has to be defined explisitly, otherwise it clashes with SET of property setter
    ;

primitiveType
    :   CHAR
    |   BYTE
    |   SHORT
    |   INT
    |   FLOAT
    ;

typeArguments
    :   '<' typeArgument (',' typeArgument)* '>'
    ;

typeArgument
    :   type_
    |   '?' ((EXTENDS | SUPER) type_)?
    ;

qualifiedNameList
    :   qualifiedName (',' qualifiedName)*
    ;

formalParameters
    :   '(' formalParameterList? ')'
    ;

formalParameterList
    :   formalParameter (',' formalParameter)* (',' lastFormalParameter)?
    |   lastFormalParameter
    ;

formalParameter
    :   variableModifier* type_ variableDeclaratorId
    ;

lastFormalParameter
    :   variableModifier* type_ '...' variableDeclaratorId
    ;

methodBody
    :   block
    ;

constructorBody
    :   block
    ;

qualifiedName
    :   Identifier ('.' Identifier)*
    ;

literal
    :   IntegerLiteral
    |   FloatingPointLiteral
    |   CharacterLiteral
    |   StringLiteral
    |   BooleanLiteral
    |   NullLiteral
    ;

// ANNOTATIONS

annotation
    :   '@' annotationName ( '(' ( elementValuePairs | elementValue )? ')' )?
    ;

annotationName : qualifiedName ;

elementValuePairs
    :   elementValuePair (','? elementValuePair)*
    ;

elementValuePair
    :   Identifier '=' elementValue
    ;

elementValue
    :   expression
    |   annotation
    |   elementValueArrayInitializer
    ;

elementValueArrayInitializer
    :   '{' (elementValue (',' elementValue)*)? ','? '}'
    ;

annotationTypeDeclaration
    :   '@' INTERFACE Identifier annotationTypeBody
    ;

annotationTypeBody
    :   '{' annotationTypeElementDeclaration* '}'
    ;

annotationTypeElementDeclaration
    :   modifier* annotationTypeElementRest
    |   ';' // this is not allowed by the grammar, but apparently allowed by the actual compiler
    ;

annotationTypeElementRest
    :   type_ annotationMethodOrConstantRest ';'
    |   classDeclaration ';'?
    |   interfaceDeclaration ';'?
    |   enumDeclaration ';'?
    |   annotationTypeDeclaration ';'?
    ;

annotationMethodOrConstantRest
    :   annotationMethodRest
    |   annotationConstantRest
    ;

annotationMethodRest
    :   Identifier '(' ')' defaultValue?
    ;

annotationConstantRest
    :   variableDeclarators
    ;

defaultValue
    :   DEFAULT elementValue
    ;

// STATEMENTS / BLOCKS

block
    :   '{' blockStatement* '}'
    ;

blockStatement
    :   localVariableDeclarationStatement
    |   statement
    |   typeDeclaration
    ;

localVariableDeclarationStatement
    :    localVariableDeclaration ';'
    ;

localVariableDeclaration
    :   variableModifier* type_ variableDeclarators
    ;

statement
    :   block
    |   IF parExpression statement (ELSE statement)?
    |   FOR '(' forControl ')' statement
    |   WHILE parExpression statement
    |   DO statement WHILE parExpression ';'
    |   RUNAS '(' expression ')' statement
    |   TRY block (catchClause+ finallyBlock? | finallyBlock)
    |   TRY resourceSpecification block catchClause* finallyBlock?
    |   RETURN expression? ';'
    |   THROW expression ';'
    |   BREAK Identifier? ';'
    |   CONTINUE Identifier? ';'
    |   ';'
    |   statementExpression ';'
    |   Identifier ':' statement
    |   apexDbExpression ';'
    ;

propertyBlock
	:	modifier* (getter | setter)
	;

getter
 : GET (';' | methodBody)
 ;

setter
 : SET (';' | methodBody)
 ;


catchClause
    :   CATCH '(' variableModifier* catchType Identifier ')' block
    ;

catchType
    :   qualifiedName ('|' qualifiedName)*
    ;

finallyBlock
    :   FINALLY block
    ;

resourceSpecification
    :   '(' resources ';'? ')'
    ;

resources
    :   resource (';' resource)*
    ;

resource
    :   variableModifier* classOrInterfaceType variableDeclaratorId '=' expression
    ;

forControl
    :   enhancedForControl
    |   forInit? ';' expression? ';' forUpdate?
    ;

forInit
    :   localVariableDeclaration
    |   expressionList
    ;

enhancedForControl
    :   variableModifier* type_ variableDeclaratorId ':' expression
    ;

forUpdate
    :   expressionList
    ;

// EXPRESSIONS

parExpression
    :   '(' expression ')'
    ;

expressionList
    :   expression (',' expression)*
    ;

statementExpression
    :   expression
    ;

constantExpression
    :   expression
    ;

apexDbUpsertExpression
    :   DB_UPSERT expression expression*
    ;

apexDbExpression
	:   (DB_INSERT | DB_UPDATE | DB_DELETE | DB_UNDELETE) expression
    |   apexDbUpsertExpression
	;

expression
    :   primary
    |   expression '.' GET '(' expressionList? ')'
    |   expression '.' SET '(' expressionList? ')'
    |   expression '?'? '.' Identifier
    |   expression '.' THIS
    |   expression '.' NEW
    |   expression '.'
        (   DB_INSERT
        |   DB_UPSERT
        |   DB_UPDATE
        |   DB_DELETE
        |   DB_UNDELETE
        )
    |   expression '.' SUPER superSuffix
    |   expression '.' explicitGenericInvocation
    |   expression '[' expression ']'
    |   expression '(' expressionList? ')'
    |   NEW creator
    |   '(' type_ ')' expression
    |   expression ('++' | '--')
    |   ('+'|'-'|'++'|'--') expression
    |   ('~'|'!') expression
    |   expression ('*'|'/'|'%') expression
    |   expression ('+'|'-') expression
    |   expression ('<' '<' | '>' '>' '>' | '>' '>') expression
    |   expression ('<=' | '>=' | '>' | '<') expression
    |   expression INSTANCEOF type_
    |   expression ('==' | '!=' | '<>') expression
    |   expression '&' expression
    |   expression '^' expression
    |   expression '|' expression
    |   expression '&&' expression
    |   expression '||' expression
    |   expression '?' expression ':' expression
    |   <assoc=right> expression
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
    |   THIS
    |   SUPER
    |   literal
    |   Identifier
    |   type_ '.' CLASS
    |   VOID '.' CLASS
    |   nonWildcardTypeArguments (explicitGenericInvocationSuffix | THIS arguments)
    |   SoqlLiteral
    ;

creator
    :   nonWildcardTypeArguments createdName classCreatorRest
    |   createdName (arrayCreatorRest | classCreatorRest | mapCreatorRest | setCreatorRest)
    ;

createdName
    :   Identifier typeArgumentsOrDiamond? ('.' Identifier typeArgumentsOrDiamond?)*
    |   primitiveType
    |   SET typeArgumentsOrDiamond // 'set <' has to be defined explisitly, otherwise it clashes with SET of property setter
    ;

innerCreator
    :   Identifier nonWildcardTypeArgumentsOrDiamond? classCreatorRest
    ;

arrayCreatorRest
    :   '['
        (   ']' ('[' ']')* arrayInitializer
        |   expression ']' ('[' expression ']')* ('[' ']')*
        )
    ;

mapCreatorRest
    :   '{'
        (   '}'
        | ( Identifier | expression ) '=>' ( literal | expression ) (',' (Identifier | expression) '=>' ( literal | expression ) )* '}'
        )
    ;

setCreatorRest
	:   '{'
        (   '}'
        | ( literal | expression ) (',' ( literal | expression ))* '}'
        )
	;

classCreatorRest
    :   arguments classBody?
    ;

explicitGenericInvocation
    :   nonWildcardTypeArguments explicitGenericInvocationSuffix
    ;

nonWildcardTypeArguments
    :   '<' typeList '>'
    ;

typeArgumentsOrDiamond
    :   '<' '>'
    |   typeArguments
    ;

nonWildcardTypeArgumentsOrDiamond
    :   '<' '>'
    |   nonWildcardTypeArguments
    ;

superSuffix
    :   arguments
    |   '.' Identifier arguments?
    ;

explicitGenericInvocationSuffix
    :   SUPER superSuffix
    |   Identifier arguments
    ;

arguments
    :   '(' expressionList? ')'
    ;

// Apex - SOQL literal

SoqlLiteral
    : '[' WS* SELECT (SelectRestNoInnerBrackets | SelectRestAllowingInnerBrackets)*? ']'
	;

fragment SelectRestAllowingInnerBrackets
	:  '[' ~']' .*? ']'
	|	~'[' .*?
	;

fragment SelectRestNoInnerBrackets
	:  ~'['
	;
// LEXER

// ?3.9 Keywords

OVERRIDE      : O V E R R I D E;
VIRTUAL       : V I R T U A L;
SET           : S E T;
GET           : G E T;
ABSTRACT      : A B S T R A C T;
BREAK         : B R E A K;
BYTE          : B Y T E;
CATCH         : C A T C H;
CHAR          : C H A R;
CLASS         : C L A S S;
CONST         : C O N S T;
CONTINUE      : C O N T I N U E;
DEFAULT       : D E F A U L T;
DO            : D O;
ELSE          : E L S E;
ENUM          : E N U M;
EXTENDS       : E X T E N D S;
FINAL         : F I N A L;
FINALLY       : F I N A L L Y;
FLOAT         : F L O A T;
FOR           : F O R;
IF            : I F;
GOTO          : G O T O;
IMPLEMENTS    : I M P L E M E N T S;
IMPORT        : I M P O R T;
INSTANCEOF    : I N S T A N C E O F;
INT           : I N T;
INTERFACE     : I N T E R F A C E;
NATIVE        : N A T I V E;
NEW           : N E W;
PACKAGE       : P A C K A G E;
PRIVATE       : P R I V A T E;
PROTECTED     : P R O T E C T E D;
PUBLIC        : P U B L I C;
RETURN        : R E T U R N;
SHORT         : S H O R T;
STATIC        : S T A T I C;

SUPER         : S U P E R;
SYNCHRONIZED  : S Y N C H R O N I Z E D;
THIS          : T H I S;
THROW         : T H R O W;
THROWS        : T H R O W S;
TRANSIENT     : T R A N S I E N T;
TRY           : T R Y;
VOID          : V O I D;
VOLATILE      : V O L A T I L E;
WHILE         : W H I L E;

// Apexcode specific
GLOBAL	      : G L O B A L;
WEBSERVICE    : W E B S E R V I C E;
APEX_WITH_SHARING :    W I T H SPACE S H A R I N G;
APEX_WITHOUT_SHARING : W I T H O U T SPACE S H A R I N G;
SELECT        : S E L E C T;
DB_INSERT     : I N S E R T;
DB_UPSERT     : U P S E R T;
DB_UPDATE     : U P D A T E;
DB_DELETE     : D E L E T E;
DB_UNDELETE   : U N D E L E T E;
TESTMETHOD   : T E S T M E T H O D;
RUNAS        : S Y S T E M DOT R U N A S;


// ?3.10.1 Integer Literals

IntegerLiteral
    :   DecimalIntegerLiteral
    |   HexIntegerLiteral
    |   OctalIntegerLiteral
    |   BinaryIntegerLiteral
    ;

fragment
DecimalIntegerLiteral
    :   DecimalNumeral IntegerTypeSuffix?
    ;

fragment
HexIntegerLiteral
    :   HexNumeral IntegerTypeSuffix?
    ;

fragment
OctalIntegerLiteral
    :   OctalNumeral IntegerTypeSuffix?
    ;

fragment
BinaryIntegerLiteral
    :   BinaryNumeral IntegerTypeSuffix?
    ;

fragment
IntegerTypeSuffix
    :   [lL]
    ;

fragment
DecimalNumeral
    :   '0' Digit?
    |   NonZeroDigit (Digits? | Underscores Digits)
    ;

fragment
Digits
    :   Digit (DigitOrUnderscore* Digit)?
    ;

fragment
Digit
    :   '0'
    |   NonZeroDigit
    ;

fragment
NonZeroDigit
    :   [1-9]
    ;

fragment
DigitOrUnderscore
    :   Digit
    |   '_'
    ;

fragment
Underscores
    :   '_'+
    ;

fragment
HexNumeral
    :   '0' [xX] HexDigits
    ;

fragment
HexDigits
    :   HexDigit (HexDigitOrUnderscore* HexDigit)?
    ;

fragment
HexDigit
    :   [0-9a-fA-F]
    ;

fragment
HexDigitOrUnderscore
    :   HexDigit
    |   '_'
    ;

fragment
OctalNumeral
    :   '0' Underscores? OctalDigits
    ;

fragment
OctalDigits
    :   OctalDigit (OctalDigitOrUnderscore* OctalDigit)?
    ;

fragment
OctalDigit
    :   [0-7]
    ;

fragment
OctalDigitOrUnderscore
    :   OctalDigit
    |   '_'
    ;

fragment
BinaryNumeral
    :   '0' [bB] BinaryDigits
    ;

fragment
BinaryDigits
    :   BinaryDigit (BinaryDigitOrUnderscore* BinaryDigit)?
    ;

fragment
BinaryDigit
    :   [01]
    ;

fragment
BinaryDigitOrUnderscore
    :   BinaryDigit
    |   '_'
    ;

// ?3.10.2 Floating-Point Literals

FloatingPointLiteral
    :   DecimalFloatingPointLiteral
    |   HexadecimalFloatingPointLiteral
    ;

fragment
DecimalFloatingPointLiteral
    :   Digits '.' Digits? ExponentPart? FloatTypeSuffix?
    |   '.' Digits ExponentPart? FloatTypeSuffix?
    |   Digits ExponentPart FloatTypeSuffix?
    |   Digits FloatTypeSuffix
    ;

fragment
ExponentPart
    :   ExponentIndicator SignedInteger
    ;

fragment
ExponentIndicator
    :   [eE]
    ;

fragment
SignedInteger
    :   Sign? Digits
    ;

fragment
Sign
    :   [+-]
    ;

fragment
FloatTypeSuffix
    :   [fFdD]
    ;

fragment
HexadecimalFloatingPointLiteral
    :   HexSignificand BinaryExponent FloatTypeSuffix?
    ;

fragment
HexSignificand
    :   HexNumeral '.'?
    |   '0' [xX] HexDigits? '.' HexDigits
    ;

fragment
BinaryExponent
    :   BinaryExponentIndicator SignedInteger
    ;

fragment
BinaryExponentIndicator
    :   [pP]
    ;

// ?3.10.3 Boolean Literals

BooleanLiteral
    :   'true'
    |   'false'
    ;

// ?3.10.4 Character Literals

CharacterLiteral
    :   QUOTE SingleCharacter QUOTE
    |   QUOTE EscapeSequence QUOTE
    ;

fragment
SingleCharacter
    :   ~['\\]
    ;

// ?3.10.5 String Literals

StringLiteral
    :   QUOTE StringCharacters? QUOTE
    ;

fragment
StringCharacters
    :   StringCharacter+
    ;

fragment
StringCharacter
    :   ~['\\]
    |   EscapeSequence
    ;

// ?3.10.6 Escape Sequences for Character and String Literals

fragment
EscapeSequence
    :   '\\' [btnfr"'\\]
    |   OctalEscape
    |   UnicodeEscape
    ;

fragment
OctalEscape
    :   '\\' OctalDigit
    |   '\\' OctalDigit OctalDigit
    |   '\\' ZeroToThree OctalDigit OctalDigit
    ;

fragment
UnicodeEscape
    :   '\\' 'u' HexDigit HexDigit HexDigit HexDigit
    ;

fragment
ZeroToThree
    :   [0-3]
    ;

// ?3.10.7 The Null Literal

NullLiteral :   N U L L;


// ?3.11 Separators

LPAREN          : '(';
RPAREN          : ')';
LBRACE          : '{';
RBRACE          : '}';
LBRACK          : '[';
RBRACK          : ']';
SEMI            : ';';
COMMA           : ',';
DOT             : '.';

// ?3.12 Operators

ASSIGN          : '=';
GT              : '>';
LT              : '<';
BANG            : '!';
TILDE           : '~';
QUESTION        : '?';
COLON           : ':';
EQUAL           : '==';
LE              : '<=';
GE              : '>=';
NOTEQUAL        : '!=';
AND             : '&&';
OR              : '||';
INC             : '++';
DEC             : '--';
ADD             : '+';
SUB             : '-';
MUL             : '*';
DIV             : '/';
BITAND          : '&';
BITOR           : '|';
CARET           : '^';
MOD             : '%';

ADD_ASSIGN      : '+=';
SUB_ASSIGN      : '-=';
MUL_ASSIGN      : '*=';
DIV_ASSIGN      : '/=';
AND_ASSIGN      : '&=';
OR_ASSIGN       : '|=';
XOR_ASSIGN      : '^=';
MOD_ASSIGN      : '%=';
LSHIFT_ASSIGN   : '<<=';
RSHIFT_ASSIGN   : '>>=';
URSHIFT_ASSIGN  : '>>>=';
LAMBDA_LIKE     : '=>';


// ?3.8 Identifiers (must appear after all keywords in the grammar)

Identifier
    :   JavaLetter JavaLetterOrDigit*
    ;

fragment
JavaLetter
    :   [a-zA-Z$_] // these are the "java letters" below 0xFF
    |   // covers all characters above 0xFF which are not a surrogate
        ~[\u0000-\u00FF\uD800-\uDBFF]
        {Character.isJavaIdentifierStart(_input.LA(-1))}?
    |   // covers UTF-16 surrogate pairs encodings for U+10000 to U+10FFFF
        [\uD800-\uDBFF] [\uDC00-\uDFFF]
        {Character.isJavaIdentifierStart(Character.toCodePoint((char)_input.LA(-2), (char)_input.LA(-1)))}?
    ;

fragment
JavaLetterOrDigit
    :   [a-zA-Z0-9$_] // these are the "java letters or digits" below 0xFF
    |   // covers all characters above 0xFF which are not a surrogate
        ~[\u0000-\u00FF\uD800-\uDBFF]
        {Character.isJavaIdentifierPart(_input.LA(-1))}?
    |   // covers UTF-16 surrogate pairs encodings for U+10000 to U+10FFFF
        [\uD800-\uDBFF] [\uDC00-\uDFFF]
        {Character.isJavaIdentifierPart(Character.toCodePoint((char)_input.LA(-2), (char)_input.LA(-1)))}?
    ;

//
// Additional symbols not defined in the lexical specification
//

AT : '@';
ELLIPSIS : '...';

//
// Whitespace and comments
//

WS  :  [ \t\r\n\u000C]+ -> skip
    ;

APEXDOC_COMMENT
    :   '/**' [\r\n] .*? '*/' -> skip
    ;

APEXDOC_COMMENT_START
    :   '/**' -> skip
    ;

COMMENT
    :   '/*' .*? '*/' -> skip
    ;

COMMENT_START
    :   '/*' -> skip
    ;

LINE_COMMENT
    :   '//' ~[\r\n]* -> skip
    ;

//
// Unexpected token for non recognized elements
//

QUOTE	:	'\'' -> skip;

// characters

fragment A : [aA];
fragment B : [bB];
fragment C : [cC];
fragment D : [dD];
fragment E : [eE];
fragment F : [fF];
fragment G : [gG];
fragment H : [hH];
fragment I : [iI];
fragment J : [jJ];
fragment K : [kK];
fragment L : [lL];
fragment M : [mM];
fragment N : [nN];
fragment O : [oO];
fragment P : [pP];
fragment Q : [qQ];
fragment R : [rR];
fragment S : [sS];
fragment T : [tT];
fragment U : [uU];
fragment V : [vV];
fragment W : [wW];
fragment X : [xX];
fragment Y : [yY];
fragment Z : [zZ];
fragment SPACE : ' ';
