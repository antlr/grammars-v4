/*
 [The "BSD licence"]
 Copyright (c) 2013 Sam Harwell
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

/** C 2011 grammar built from the C11 Spec */

// $antlr-format alignTrailingComments true, columnLimit 150, minEmptyLines 1, maxEmptyLinesToKeep 1, reflowComments false, useTab false
// $antlr-format allowShortRulesOnASingleLine false, allowShortBlocksOnASingleLine true, alignSemicolons hanging, alignColons hanging

parser grammar CParser;

options {
    superClass=CParserBase;
    tokenVocab=CLexer;
}


compilationUnit
    : translationUnit? EOF
    ;

// A.1.5
// 6.4.4
constant
    : IntegerConstant
    | FloatingConstant
    //|   EnumerationConstant
    | CharacterConstant
    | predefinedConstant
    ;

// 6.4.4.5
predefinedConstant
    : 'false'
    | 'true'
    | 'nullptr'
    ;

// A.2.1 Expressions
// 6.5.1
primaryExpression
    : Identifier
    | constant
    | StringLiteral+
    | '(' expression ')'
    | genericSelection

    // GNU
    // https://github.com/gcc-mirror/gcc/blob/5d69161a7c36a2da8565967eb0cc2df1322a05a3/gcc/c/c-parser.cc#L11715-L11734
    | '__func__' //GNU
    | '__FUNCTION__' //GNU
    | '__PRETTY_FUNCTION__' //GNU
    | '__extension__'? '(' compoundStatement ')' //GNU
    | '__builtin_va_arg' '(' unaryExpression ',' typeName ')' //GNU
    | '__builtin_offsetof' '(' typeName ',' unaryExpression ')' //GNU
    | '__builtin_choose_expr' '(' unaryExpression ',' unaryExpression ',' unaryExpression ')' //GNU
    | '__builtin_types_compatible_p' '(' typeName ',' typeName ')' //GNU
    | '__builtin_tgmath' '(' exprList ')'
    | '__builtin_complex' '(' assignmentExpression ',' assignmentExpression ')'
    ;

// GNU
// https://github.com/gcc-mirror/gcc/blob/5d69161a7c36a2da8565967eb0cc2df1322a05a3/gcc/c/c-parser.cc#L14312-L14314
exprList
    : assignmentExpression (',' assignmentExpression)*
    ;

// 6.5.1.1
genericSelection
    : '_Generic' '(' assignmentExpression ',' genericAssocList ')'
    ;

// 6.5.1.1
genericAssocList
    : genericAssociation (',' genericAssociation)*
    ;

// 6.5.1.1
genericAssociation
    : (typeName | 'default') ':' assignmentExpression
    ;

// 6.5.2
postfixExpression
    : (primaryExpression | '__extension__'? '(' typeName ')' '{' initializerList? ','? '}')
      (
        '[' expression ']'
        | '(' argumentExpressionList? ')'
        | ('.' | '->') Identifier
        | '++'
        | '--'
    )*
    ;

// 6.5.2
argumentExpressionList
    : assignmentExpression (',' assignmentExpression)*
    ;

// 6.5.3
// GNU
// https://github.com/gcc-mirror/gcc/blob/5d69161a7c36a2da8565967eb0cc2df1322a05a3/gcc/c/c-parser.cc#L10625-L10658
unaryExpression
    : ('++' | '--' | 'sizeof')* (
        postfixExpression
        | unaryOperator=('&' | '*' | '+' | '-' | '~' | '!'
		| '__extension__' // GNU
		| '__real__' // GNU
		| '__imag__' // GNU
		) castExpression
        | ('sizeof' | Alignof) ( '(' typeName ')'
		| unaryExpression //GNU
		)
        | '&&' Identifier // GCC extension address of label
    )
    ;

castExpression
    : '(' typeName ')' castExpression
    | unaryExpression
    | DigitSequence // for
    ;

multiplicativeExpression
    : castExpression (('*' | '/' | '%') castExpression)*
    ;

additiveExpression
    : multiplicativeExpression (('+' | '-') multiplicativeExpression)*
    ;

shiftExpression
    : additiveExpression (('<<' | '>>') additiveExpression)*
    ;

relationalExpression
    : shiftExpression (('<' | '>' | '<=' | '>=') shiftExpression)*
    ;

equalityExpression
    : relationalExpression (('==' | '!=') relationalExpression)*
    ;

andExpression
    : equalityExpression ('&' equalityExpression)*
    ;

exclusiveOrExpression
    : andExpression ('^' andExpression)*
    ;

inclusiveOrExpression
    : exclusiveOrExpression ('|' exclusiveOrExpression)*
    ;

logicalAndExpression
    : inclusiveOrExpression ('&&' inclusiveOrExpression)*
    ;

logicalOrExpression
    : logicalAndExpression ('||' logicalAndExpression)*
    ;

conditionalExpression
    : logicalOrExpression ('?' expression ':' conditionalExpression)?
    ;

assignmentExpression
    : conditionalExpression
    | unaryExpression assignementOperator=('=' | '*=' | '/=' | '%=' | '+=' | '-=' | '<<=' | '>>=' | '&=' | '^=' | '|=') assignmentExpression
    | DigitSequence // for
    ;

expression
    : assignmentExpression (',' assignmentExpression)*
    ;

constantExpression
    : conditionalExpression
    ;

declaration
    : declarationSpecifiers initDeclaratorList? ';'
    | staticAssertDeclaration
    | attributeDeclaration
    ;

declarationSpecifiers
    : ({ this.IsDeclarationSpecifier()}? declarationSpecifier )+
    ;

declarationSpecifier
    : storageClassSpecifier
    | typeSpecifier
    | typeQualifier
    | functionSpecifier
    | alignmentSpecifier
    ;

initDeclaratorList
    : initDeclarator (',' initDeclarator)*
    ;

initDeclarator
    : declarator ('=' initializer)?
    ;

attributeDeclaration
    : attributeSpecifierSequence ';'
    ;

storageClassSpecifier
    : 'typedef'
    | 'extern'
    | 'static'
    | '_Thread_local'
    | 'auto'
    | 'register'
    ;

typeSpecifier
    : 'void'
    | 'char'
    | 'short'
    | 'int'
    | 'long'
    | 'float'
    | 'double'
    | 'signed'
    | 'unsigned'
    | Bool
    | '_Complex'
    | '__m128'
    | '__m128d'
    | '__m128i'
    | '__extension__' '(' ('__m128' | '__m128d' | '__m128i') ')'
    | atomicTypeSpecifier
    | structOrUnionSpecifier
    | enumSpecifier
    | '__extension__'? typedefName
    | typeofSpecifier
    ;

typeofSpecifier
    : (Typeof | Typeof_unqual) '(' typeofSpecifierArgument ')'
    ;

typeofSpecifierArgument
    : expression
    | typeName
    ;

structOrUnionSpecifier
    : structOrUnion attributeSpecifierSequence? gnuAttributes?
	( Identifier? '{' ( {this.IsNullStructDeclarationListExtension()}? | memberDeclarationList) '}'
	| Identifier
	)
//	{this.EnterDeclaration();}
    ;

structOrUnion
    : 'struct'
    | 'union'
    ;

memberDeclarationList
    : memberDeclaration+
    ;

// struct-declaration is the GNU equivalent.
memberDeclaration
    : attributeSpecifierSequence? specifierQualifierList memberDeclaratorList? ';'
    | staticAssertDeclaration
    | '__extension__' memberDeclaration // GNU extension.
    ;

specifierQualifierList
    : gnuAttributes? typeSpecifierQualifier+ attributeSpecifierSequence?
    ;

typeSpecifierQualifier
    : typeSpecifier
    | typeQualifier
    | alignmentSpecifier
    ;

memberDeclaratorList
    : structDeclarator (',' gnuAttributes? structDeclarator)*
    ;

structDeclarator
    : declarator gnuAttributes?
    | declarator? ':' constantExpression gnuAttributes?
    ;

enumSpecifier
    : 'enum' attributeSpecifierSequence? gnuAttributes? Identifier? enumTypeSpecifier? '{' enumeratorList ','? '}'
    | 'enum' Identifier enumTypeSpecifier?
    ;

enumeratorList
    : enumerator (',' enumerator)*
    ;

enumerator
    : enumerationConstant attributeSpecifierSequence? gnuAttributes? ('=' constantExpression)?
    ;

enumerationConstant
    : Identifier
    ;

enumTypeSpecifier
    : specifierQualifierList
    ;

atomicTypeSpecifier
    : '_Atomic' '(' typeName ')'
    ;

typeQualifier
    : 'const'
    | Restrict
    | Volatile
    | '_Atomic'
    ;

functionSpecifier
    : Inline
    | '_Noreturn'
    | '__stdcall'
    | gnuAttribute
    | '__declspec' '(' (Identifier 
		| Restrict // CLANG
		| 'deprecated' '(' StringLiteral? ')'
		) ')'
    ;

alignmentSpecifier
    : Alignas '(' (typeName | constantExpression) ')'
    ;

// 6.7.6
// This rule is basically what was implemented in the GCC compiler.
// https://github.com/gcc-mirror/gcc/blob/f5cda36f16d447198c1e00b191d720b6f4a02876/gcc/c/c-parser.cc#L4975-L4995
declarator
    : gnuAttribute? pointer declarationSpecifiers? declarator
    | gnuAttribute* directDeclarator gccDeclaratorExtension*
    ;

// 6.7.6
// The rules from the spec were refactored. array-declarator and function-declarator
// were unfolded into direct-declarator. Those rules were deleted.
directDeclarator
    : Identifier attributeSpecifierSequence? {this.EnterDeclaration();}
    | '(' declarator ')'
    | directDeclarator '[' typeQualifierList? assignmentExpression? ']' attributeSpecifierSequence?
    | directDeclarator '[' 'static' typeQualifierList? assignmentExpression ']' attributeSpecifierSequence?
    | directDeclarator '[' typeQualifierList 'static' assignmentExpression ']' attributeSpecifierSequence?
    | directDeclarator '[' typeQualifierList? '*' ']' attributeSpecifierSequence?
    | directDeclarator '(' parameterTypeList? ')' attributeSpecifierSequence?
    | Identifier ':' DigitSequence         // bit field
    | vcSpecificModifer Identifier         // Visual C Extension
    | '(' vcSpecificModifer declarator ')' // Visual C Extension
    | gnuAttribute
    ;

vcSpecificModifer
    : '__cdecl'
    | '__clrcall'
    | '__stdcall'
    | '__fastcall'
    | '__thiscall'
    | '__vectorcall'
    ;

gccDeclaratorExtension
    : asmDefinition
    | gnuAttribute
    ;

gnuAttributes
    : gnuAttribute+
    ;

gnuAttribute
    : Attribute '(' '(' gnuAttributeList ')' ')'
    ;

gnuAttributeList
    : gnuSingleAttribute*
    ;

gnuSingleAttribute
    : ~('(' | ')')
    | '(' gnuAttributeList ')'
    ;

// 6.7.6
pointer
    : (('*' | '^') typeQualifierList?)+ // ^ - Blocks language extension
    ;

// 6.7.6
typeQualifierList
    : typeQualifier+
    ;

// 6.7.7.1
parameterTypeList
    : parameterList (',' '...')?
    | '...'
    ;

// 6.7.7.1
parameterList
    : parameterDeclaration (',' parameterDeclaration)*
    ;

// 6.7.7.1
parameterDeclaration
    : attributeSpecifierSequence? declarationSpecifiers? declarator
    | attributeSpecifierSequence? declarationSpecifiers abstractDeclarator?
    ;

// 6.10.1
identifierList
    : Identifier (',' Identifier)*
    ;

// 6.7.8
typeName
    : specifierQualifierList abstractDeclarator?
    ;

// 6.7.8
abstractDeclarator
    : vcSpecificModifer? pointer
    | vcSpecificModifer? pointer? directAbstractDeclarator gccDeclaratorExtension*
    ;

// 6.7.8
directAbstractDeclarator
    : '(' abstractDeclarator ')' gccDeclaratorExtension*
    | '[' typeQualifierList? assignmentExpression? ']'
    | '[' 'static' typeQualifierList? assignmentExpression ']'
    | '[' typeQualifierList 'static' assignmentExpression ']'
    | '[' '*' ']'
    | '(' parameterTypeList? ')' gccDeclaratorExtension*
    | directAbstractDeclarator '[' typeQualifierList? assignmentExpression? ']'
    | directAbstractDeclarator '[' 'static' typeQualifierList? assignmentExpression ']'
    | directAbstractDeclarator '[' typeQualifierList 'static' assignmentExpression ']'
    | directAbstractDeclarator '[' '*' ']'
    | directAbstractDeclarator '(' parameterTypeList? ')' gccDeclaratorExtension*
    ;

// 6.7.9
typedefName
    : Identifier
    ;

// 6.7.10
initializer
    : assignmentExpression
    | '{' initializerList ','? '}'
    | '{' '}' //GNU https://github.com/gcc-mirror/gcc/blob/77ab3b07385f23b39a2445011068c04e0872b481/gcc/c/c-parser.cc#L6542
    ;

// 6.7.10
initializerList
    : designation? initializer (',' designation? initializer)*
    ;

// 6.7.10
designation
    : designatorList '='
    | arrayDesignator //GNU https://github.com/gcc-mirror/gcc/blob/77ab3b07385f23b39a2445011068c04e0872b481/gcc/c/c-parser.cc#L6545-L6546
    | Identifier ':' //GNU
    ;

// 6.7.10
designatorList
    : designator+
    ;

// 6.7.10
designator
    : arrayDesignator //GNU
    | '.' Identifier
    ;

// GNU https://github.com/gcc-mirror/gcc/blob/77ab3b07385f23b39a2445011068c04e0872b481/gcc/c/c-parser.cc#L6548-L6549
arrayDesignator
    : '[' constantExpression ('...' constantExpression)? ']'
    ;

staticAssertDeclaration
    : '_Static_assert' '(' constantExpression (',' StringLiteral)? ')' ';'
    ;

attributeSpecifierSequence
    : attributeSpecifier+
    ;

attributeSpecifier
    : '[' '[' attributeList ']' ']'
    ;

attributeList
    : attribute (',' attribute)* // May not be correct.
    ;

attribute
    : attributeToken attributeArgumentClause?
    ;

attributeToken
    : Identifier
    | Identifier ':' ':' Identifier
    ;

attributeArgumentClause
    : '(' balancedTokenSequence? ')'
    ;

balancedTokenSequence
    : balancedToken+
    ;

balancedToken
    : '(' balancedTokenSequence? ')'
    | '[' balancedTokenSequence? ']'
    | '{' balancedTokenSequence? '}'
    // any token other than a parenthesis, bracket, or brace
    ;

statement
    : labeledStatement
    | compoundStatement
    | expressionStatement
    | selectionStatement
    | iterationStatement
    | jumpStatement
    | asmStatement
    ;

labeledStatement
    : Identifier ':' statement?
    | 'case' constantExpression ':' statement
    | 'default' ':' statement
    ;

compoundStatement
    : '{' blockItemList? '}'
    ;

blockItemList
    : blockItem+
    ;

blockItem
    : {this.IsStatement()}? statement
    | {this.IsDeclaration()}? declaration
    ;

expressionStatement
    : expression? ';'
    ;

selectionStatement
    : 'if' '(' expression ')' statement ('else' statement)?
    | 'switch' '(' expression ')' statement
    ;

iterationStatement
    : While '(' expression ')' statement
    | Do statement While '(' expression ')' ';'
    | For '(' forCondition ')' statement
    ;

//    |   'for' '(' expression? ';' expression?  ';' forUpdate? ')' statement
//    |   For '(' declaration  expression? ';' expression? ')' statement

forCondition
    : (forDeclaration | expression?) ';' forExpression? ';' forExpression?
    ;

forDeclaration
    : declarationSpecifiers initDeclaratorList?
    ;

forExpression
    : assignmentExpression (',' assignmentExpression)*
    ;

jumpStatement
    : (
        'goto' Identifier
        | 'continue'
        | 'break'
        | 'return' expression?
        | 'goto' unaryExpression // GCC extension
    ) ';'
    ;

// A.2.4 External definitions
// 6.9
translationUnit
    : externalDeclaration+
    ;

// 6.9
externalDeclaration
    : '__extension__'? (
	functionDefinition
	| declaration
	| ';' // stray ;
	| asmDefinition // GCC
	)
    ;

// 6.9.1
functionDefinition
    : attributeSpecifierSequence? declarationSpecifiers? declarator declarationList? functionBody
    ;

// 6.9.1
functionBody
    : compoundStatement
    ;

asmDefinition
    : simpleAsmExpr
    | Asm '(' toplevelAsmArgument ')'
    ;

simpleAsmExpr
    : Asm '(' asmStringLiteral ')'
    ;

toplevelAsmArgument
    : asmStringLiteral
    | asmStringLiteral ':' asmOperands?
    | asmStringLiteral ':' asmOperands? ':' asmOperands?
    ;

asmStringLiteral
    : StringLiteral
    ;

asmOperands
    : asmOperand (',' asmOperand)*
    ;

asmOperand
    : asmStringLiteral '(' expression ')'
    | '[' Identifier ']' asmStringLiteral '(' expression ')'
    ;

asmStatement
    : Asm asmQualifierList? '(' asmArgument ')' ';'
    ;

asmQualifier
    : Volatile
    | Inline
    | 'goto'
    ;

asmQualifierList
    : asmQualifier+
    ;

asmClobbers
    : (asmStringLiteral | Identifier) ( ',' (asmStringLiteral | Identifier) )*
    ;

asmArgument
    : asmStringLiteral
    | asmStringLiteral ':' asmOperands? ( ':' asmOperands? (':' asmClobbers? )* )?
    ;

declarationList
    : declaration+
    ;

