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

translationUnit
    : externalDeclaration+
    ;


primaryExpression
    : Identifier
    | Constant
    | StringLiteral+
    | '(' expression ')'
    | genericSelection
    | '__extension__'? '(' compoundStatement ')' // Blocks (GCC extension)
    | '__builtin_va_arg' '(' unaryExpression ',' typeName ')'
    | '__builtin_offsetof' '(' typeName ',' unaryExpression ')'
    ;

genericSelection
    : '_Generic' '(' assignmentExpression ',' genericAssocList ')'
    ;

genericAssocList
    : genericAssociation (',' genericAssociation)*
    ;

genericAssociation
    : (typeName | 'default') ':' assignmentExpression
    ;

postfixExpression
    : (primaryExpression | '__extension__'? '(' typeName ')' '{' initializerList ','? '}') (
        '[' expression ']'
        | '(' argumentExpressionList? ')'
        | ('.' | '->') Identifier
        | '++'
        | '--'
    )*
    ;

argumentExpressionList
    : assignmentExpression (',' assignmentExpression)*
    ;

unaryExpression
    : ('++' | '--' | 'sizeof')* (
        postfixExpression
        | unaryOperator=('&' | '*' | '+' | '-' | '~' | '!') castExpression
        | ('sizeof' | Alignof) '(' typeName ')'
        | '&&' Identifier // GCC extension address of label
    )
    ;

castExpression
    : '__extension__'? '(' typeName ')' castExpression
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
    : declarationSpecifiers initDeclaratorList? ';' {this.EnterDeclaration();}
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
    | '__typeof__' '(' constantExpression ')' // GCC extension
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
    : typeSpecifierQualifier+ attributeSpecifierSequence?
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
    : 'enum' attributeSpecifierSequence? Identifier? enumTypeSpecifier '{' enumeratorList ','? '}'
    | 'enum' Identifier enumTypeSpecifier?
    ;

enumeratorList
    : enumerator (',' enumerator)*
    ;

enumerator
    : enumerationConstant attributeSpecifierSequence? ('=' constantExpression)?
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
    | 'restrict'
    | 'volatile'
    | '_Atomic'
    ;

functionSpecifier
    : 'inline'
    | '_Noreturn'
    | '__inline__' // GCC extension
    | '__stdcall'
    | gnuAttribute
    | '__declspec' '(' Identifier ')'
    ;

alignmentSpecifier
    : Alignas '(' (typeName | constantExpression) ')'
    ;

declarator
    : gnuAttribute? pointer declarationSpecifiers? declarator
    | directDeclarator gccDeclaratorExtension*
    ;

directDeclarator
    : Identifier attributeSpecifierSequence?
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
    : '__attribute__' '(' '(' gnuAttributeList ')' ')'
    ;

gnuAttributeList
    : gnuSingleAttribute? (',' gnuSingleAttribute?)*
    ;

gnuSingleAttribute
    : ~(',' | '(' | ')') // relaxed def for "identifier or reserved word"
    ('(' argumentExpressionList? ')')?
    ;

pointer
    : (('*' | '^') typeQualifierList?)+ // ^ - Blocks language extension
    ;

typeQualifierList
    : typeQualifier+
    ;

parameterTypeList
    : parameterList (',' '...')?
    ;

parameterList
    : parameterDeclaration (',' parameterDeclaration)*
    ;

parameterDeclaration
    : declarationSpecifiers ( declarator | abstractDeclarator? )
    ;

identifierList
    : Identifier (',' Identifier)*
    ;

typeName
    : specifierQualifierList abstractDeclarator?
    ;

abstractDeclarator
    : pointer
    | pointer? directAbstractDeclarator gccDeclaratorExtension*
    ;

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

typedefName
    : Identifier
    ;

initializer
    : assignmentExpression
    | '{' initializerList ','? '}'
    ;

initializerList
    : designation? initializer (',' designation? initializer)*
    ;

designation
    : designatorList '='
    ;

designatorList
    : designator+
    ;

designator
    : '[' constantExpression ']'
    | '.' Identifier
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
    | Identifier '::' Identifier
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

externalDeclaration
    : '__extension__'? (
	functionDefinition
	| declaration
	| ';' // stray ;
	| asmDefinition // GCC
	)
    ;

asmDefinition
    : simpleAsmExpr
    | ('__asm' | '__asm__') '(' toplevelAsmArgument ')'
    ;

simpleAsmExpr
    : ('__asm' | '__asm__') '(' asmStringLiteral ')'
    ;

toplevelAsmArgument
    : asmStringLiteral
    | asmStringLiteral ':' asmOperands?
    | asmStringLiteral ':' asmOperands? ':' asmOperands?
    | asmStringLiteral '::' asmOperands?
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
    : ('__asm' | '__asm__') asmQualifierList? '(' asmArgument ')' ';'
    ;

asmQualifier
    : 'volatile'
    | '__volatile__'
    | 'inline'
    | 'goto'
    ;

asmQualifierList
    : asmQualifier+
    ;

asmClobbers
    : asmStringLiteral ( ',' asmStringLiteral )*
    ;

asmGotoOperands
    : Identifier ( ',' Identifier )*
    ;

asmArgument
    : asmStringLiteral
    | asmStringLiteral ':' asmOperand?
    | asmStringLiteral ':' asmOperand ':' asmOperand?
    | asmStringLiteral (':' ':' | '::') asmOperand?
    | asmStringLiteral ':' asmOperand ':' asmOperand ':' asmClobbers?
    | asmStringLiteral (':' ':' | '::') asmOperand ':' asmClobbers?
    | asmStringLiteral (':' ':' | '::') ':' asmClobbers?
    | asmStringLiteral (':' ':' | '::') asmOperand ':' asmClobbers ':' asmGotoOperands
    | asmStringLiteral (':' ':' | '::') ':' asmClobbers ':' asmGotoOperands
    | asmStringLiteral (':' ':' | '::') (':' ':' | '::') asmGotoOperands
    ;

functionDefinition
    : declarationSpecifiers? declarator declarationList? compoundStatement
    ;

declarationList
    : declaration+
    ;

