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

// Insert here @header for parser.

// compilationUnit
compilationUnit
    : translationUnit? {this.OutputSymbolTable();} EOF
    ;

// ISO C: token (6.4.1) - No ANTLR4 rule
// ISO C: preprocessing-token (6.4.1) - No ANTLR4 rule
// ISO C: keyword (6.4.2) - No ANTLR4 rule
// ISO C: identifier (6.4.3.1) - Lexer rule: Identifier
// ISO C: identifier-start (6.4.3.1) - Lexer rule: fragment IdentifierNondigit
// ISO C: identifier-continue (6.4.3.1) - No ANTLR4 rule
// ISO C: nondigit (6.4.3.1) - Lexer rule: fragment Nondigit
// ISO C: digit (6.4.3.1) - Lexer rule: fragment Digit
// ISO C: universal-character-name (6.4.4) - Lexer rule: fragment UniversalCharacterName
// ISO C: hex-quad (6.4.4) - Lexer rule: fragment HexQuad

// ISO C: constant (6.4.5.1)
constant
    : IntegerConstant
    | FloatingConstant
    //|   EnumerationConstant
    | CharacterConstant
    | predefinedConstant
    ;

// ISO C: integer-constant (6.4.5.2) - Lexer rule: fragment IntegerConstant
// ISO C: decimal-constant (6.4.5.2) - Lexer rule: fragment DecimalConstant
// ISO C: octal-constant (6.4.5.2) - Lexer rule: fragment OctalConstant
// ISO C: hexadecimal-constant (6.4.5.2) - Lexer rule: fragment HexadecimalConstant
// ISO C: binary-constant (6.4.5.2) - Lexer rule: fragment BinaryConstant
// ISO C: hexadecimal-prefix (6.4.5.2) - Lexer rule: fragment HexadecimalPrefix
// ISO C: binary-prefix (6.4.5.2) - No ANTLR4 rule
// ISO C: nonzero-digit (6.4.5.2) - Lexer rule: fragment NonzeroDigit
// ISO C: octal-digit (6.4.5.2) - Lexer rule: fragment OctalDigit
// ISO C: hexadecimal-digit-sequence (6.4.5.2) - No ANTLR4 rule
// ISO C: hexadecimal-digit (6.4.5.2) - Lexer rule: fragment HexadecimalDigit
// ISO C: binary-digit (6.4.5.2) - No ANTLR4 rule
// ISO C: integer-suffix (6.4.5.2) - Lexer rule: fragment IntegerSuffix
// ISO C: bit-precise-int-suffix (6.4.5.2) - No ANTLR4 rule
// ISO C: unsigned-suffix (6.4.5.2) - Lexer rule: fragment UnsignedSuffix
// ISO C: long-suffix (6.4.5.2) - Lexer rule: fragment LongSuffix
// ISO C: long-long-suffix (6.4.5.2) - Lexer rule: fragment LongLongSuffix
// ISO C: floating-constant (6.4.5.3) - Lexer rule: FloatingConstant
// ISO C: decimal-floating-constant (6.4.5.3) - Lexer rule: fragment DecimalFloatingConstant
// ISO C: hexadecimal-floating-constant (6.4.5.3) - Lexer rule: fragment HexadecimalFloatingConstant
// ISO C: fractional-constant (6.4.5.3) - Lexer rule: fragment FractionalConstant
// ISO C: exponent-part (6.4.5.3) - Lexer rule: fragment ExponentPart
// ISO C: sign (6.4.5.3) - Lexer rule: fragment Sign
// ISO C: digit-sequence (6.4.5.3) - Lexer rule: DigitSequence
// ISO C: hexadecimal-fractional-constant (6.4.5.3) - Lexer rule: fragment HexadecimalFractionalConstant
// ISO C: binary-exponent-part (6.4.5.3) - Lexer rule: fragment BinaryExponentPart
// ISO C: floating-suffix (6.4.5.3) - Lexer rule: fragment FloatingSuffix

// ISO C: enumeration-constant (6.4.5.4)
enumerationConstant
    : Identifier
    ;

// ISO C: character-constant (6.4.5.5) - Lexer rule: CharacterConstant
// ISO C: encoding-prefix (6.4.5.5) - Lexer rule: fragment EncodingPrefix
// ISO C: c-char-sequence (6.4.5.5) - Lexer rule: fragment CCharSequence
// ISO C: c-char (6.4.5.5) - Lexer rule: fragment CChar
// ISO C: escape-sequence (6.4.5.5) - Lexer rule: fragment EscapeSequence
// ISO C: simple-escape-sequence (6.4.5.5) - Lexer rule: fragment SimpleEscapeSequence
// ISO C: octal-escape-sequence (6.4.5.5) - Lexer rule: fragment OctalEscapeSequence
// ISO C: hexadecimal-escape-sequence (6.4.5.5) - Lexer rule: fragment HexadecimalEscapeSequence

// ISO C: predefined-constant (6.4.5.6)
predefinedConstant
    : 'false'
    | 'true'
    | 'nullptr'
    ;

// ISO C: string-literal (6.4.6) - Lexer rule: StringLiteral
// ISO C: s-char-sequence (6.4.6) - Lexer rule: fragment SCharSequence
// ISO C: s-char (6.4.6) - Lexer rule: fragment SChar
// ISO C: punctuator (6.4.7) - No ANTLR4 rule
// ISO C: header-name (6.4.8) - No ANTLR4 rule
// ISO C: h-char-sequence (6.4.8) - No ANTLR4 rule
// ISO C: h-char (6.4.8) - No ANTLR4 rule
// ISO C: q-char-sequence (6.4.8) - No ANTLR4 rule
// ISO C: q-char (6.4.8) - No ANTLR4 rule
// ISO C: pp-number (6.4.9) - No ANTLR4 rule

// ISO C: primary-expression (6.5.2)
primaryExpression
    : Identifier {this.LookupSymbol();}
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

// GNU exprList
// https://github.com/gcc-mirror/gcc/blob/5d69161a7c36a2da8565967eb0cc2df1322a05a3/gcc/c/c-parser.cc#L14312-L14314
exprList
    : assignmentExpression (',' assignmentExpression)*
    ;

// ISO C: generic-selection (6.5.2.1)
genericSelection
    : '_Generic' '(' assignmentExpression ',' genericAssocList ')'
    ;

// ISO C: generic-assoc-list (6.5.2.1)
genericAssocList
    : genericAssociation (',' genericAssociation)*
    ;

// ISO C: generic-association (6.5.2.1)
genericAssociation
    : (typeName | 'default') ':' assignmentExpression
    ;

// ISO C: postfix-expression (6.5.3.1)
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

// ISO C: argument-expression-list (6.5.3.1)
argumentExpressionList
    : assignmentExpression (',' assignmentExpression)*
    ;

// ISO C: compound-literal (6.5.3.1) - No ANTLR4 rule
// ISO C: storage-class-specifiers (6.5.3.6) - No ANTLR4 rule

// ISO C: unary-expression (6.5.4.1)
// GNU: https://github.com/gcc-mirror/gcc/blob/5d69161a7c36a2da8565967eb0cc2df1322a05a3/gcc/c/c-parser.cc#L10625-L10658
unaryExpression
    : postfixExpression
    | '++' unaryExpression
    | '--' unaryExpression
    | unaryOperator=('&' | '*' | '+' | '-' | '~' | '!'
	| '__extension__' // GNU
	| '__real__' // GNU
	| '__imag__' // GNU
	) castExpression
    | {!this.IsSomethingOfTypeName()}? 'sizeof' unaryExpression
    | {this.IsSomethingOfTypeName()}? 'sizeof' '(' typeName ')'
    | {this.IsSomethingOfTypeName()}? Alignof '(' typeName ')'
    | {!this.IsSomethingOfTypeName()}? Countof unaryExpression // GCC
    | {this.IsSomethingOfTypeName()}? Countof '(' typeName ')' // GCC
    | {!this.IsSomethingOfTypeName()}? Alignof unaryExpression // GCC
    | {this.IsSomethingOfTypeName()}? Maxof '(' typeName ')' // GCC
    | {this.IsSomethingOfTypeName()}? Minof '(' typeName ')' // GCC
    | '&&' Identifier // GCC extension address of label
    ;

// ISO C: unary-operator (6.5.4.1) - No ANTLR4 rule

// ISO C: cast-expression (6.5.5)
castExpression
    : {this.IsCast()}? '(' typeName ')' castExpression
    | unaryExpression
    | DigitSequence // for
    ;

// ISO C: multiplicative-expression (6.5.6)
multiplicativeExpression
    : castExpression (('*' | '/' | '%') castExpression)*
    ;

// ISO C: additive-expression (6.5.7)
additiveExpression
    : multiplicativeExpression (('+' | '-') multiplicativeExpression)*
    ;

// ISO C: shift-expression (6.5.8)
shiftExpression
    : additiveExpression (('<<' | '>>') additiveExpression)*
    ;

// ISO C: relational-expression (6.5.9)
relationalExpression
    : shiftExpression (('<' | '>' | '<=' | '>=') shiftExpression)*
    ;

// ISO C: equality-expression (6.5.10)
equalityExpression
    : relationalExpression (('==' | '!=') relationalExpression)*
    ;

// ISO C: AND-expression (6.5.11)
andExpression
    : equalityExpression ('&' equalityExpression)*
    ;

// ISO C: exclusive-OR-expression (6.5.12)
exclusiveOrExpression
    : andExpression ('^' andExpression)*
    ;

// ISO C: inclusive-OR-expression (6.5.13)
inclusiveOrExpression
    : exclusiveOrExpression ('|' exclusiveOrExpression)*
    ;

// ISO C: logical-AND-expression (6.5.14)
logicalAndExpression
    : inclusiveOrExpression ('&&' inclusiveOrExpression)*
    ;

// ISO C: logical-OR-expression (6.5.15)
logicalOrExpression
    : logicalAndExpression ('||' logicalAndExpression)*
    ;

// ISO C: conditional-expression (6.5.16)
conditionalExpression
    : logicalOrExpression ('?' expression ':' conditionalExpression)?
    ;

// ISO C: assignment-expression (6.5.17.1)
assignmentExpression
    : conditionalExpression
    | unaryExpression assignementOperator=('=' | '*=' | '/=' | '%=' | '+=' | '-=' | '<<=' | '>>=' | '&=' | '^=' | '|=') assignmentExpression
    | DigitSequence // for
    ;

// ISO C: assignment-operator (6.5.17.1) - No ANTLR4 rule

// ISO C: expression (6.5.18)
expression
    : assignmentExpression (',' assignmentExpression)*
    ;

// ISO C: constant-expression (6.6)
constantExpression
    : conditionalExpression
    ;

// ISO C: declaration (6.7.1)
declaration
    : (
	declarationSpecifiers initDeclaratorList? ';'
	| staticAssertDeclaration
	| attributeDeclaration
      ) {this.EnterDeclaration();}
    ;

// ISO C: declaration-specifiers (6.7.1)
declarationSpecifiers
    : ({ this.IsDeclarationSpecifier()}? declarationSpecifier )+
    ;

// ISO C: declaration-specifier (6.7.1)
declarationSpecifier
    : storageClassSpecifier
    | typeSpecifier
    | typeQualifier
    | functionSpecifier
    | alignmentSpecifier
    ;

// ISO C: init-declarator-list (6.7.1)
initDeclaratorList
    : initDeclarator (',' initDeclarator)*
    ;

// ISO C: init-declarator (6.7.1)
initDeclarator
    : declarator ('=' initializer)?
    ;

// ISO C: attribute-declaration (6.7.1)
attributeDeclaration
    : attributeSpecifierSequence ';'
    ;

// ISO C: storage-class-specifier (6.7.2)
storageClassSpecifier
    : 'auto'
    | 'constexpr'
    | 'extern'
    | 'register'
    | 'static'
    | ThreadLocal
    | 'typedef'
    ;

// ISO C: type-specifier (6.7.3.1)
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

// ISO C: struct-or-union-specifier (6.7.3.2)
structOrUnionSpecifier
    : structOrUnion attributeSpecifierSequence? gnuAttributes?
	( Identifier? '{' ( {this.IsNullStructDeclarationListExtension()}? | memberDeclarationList) '}'
	| Identifier
	)
//	{this.EnterDeclaration();}
    ;

// ISO C: struct-or-union (6.7.3.2)
structOrUnion
    : 'struct'
    | 'union'
    ;

// ISO C: member-declaration-list (6.7.3.2)
memberDeclarationList
    : memberDeclaration+
    ;

// ISO C: member-declaration (6.7.3.2)
memberDeclaration
    : attributeSpecifierSequence? specifierQualifierList memberDeclaratorList? ';'
    | staticAssertDeclaration
    | '__extension__' memberDeclaration // GNU extension.
    ;

// ISO C: specifier-qualifier-list (6.7.3.2)
specifierQualifierList
    : gnuAttributes? ({this.IsTypeSpecifierQualifier()}?  typeSpecifierQualifier)+ attributeSpecifierSequence?
    ;

// ISO C: type-specifier-qualifier (6.7.3.2)
typeSpecifierQualifier
    : typeSpecifier
    | typeQualifier
    | alignmentSpecifier
    ;

// ISO C: member-declarator-list (6.7.3.2)
memberDeclaratorList
    : memberDeclarator (',' gnuAttributes? memberDeclarator)*
    ;

// ISO C: member-declarator (6.7.3.2) - No ANTLR4 rule
memberDeclarator
    : declarator gnuAttributes?
    | declarator? ':' constantExpression gnuAttributes?
    ;

// ISO C: enum-specifier (6.7.3.3)
enumSpecifier
    : 'enum' attributeSpecifierSequence? gnuAttributes? Identifier? enumTypeSpecifier? '{' enumeratorList ','? '}'
    | 'enum' Identifier enumTypeSpecifier?
    ;

// ISO C: enumerator-list (6.7.3.3)
enumeratorList
    : enumerator (',' enumerator)*
    ;

// ISO C: enumerator (6.7.3.3)
enumerator
    : enumerationConstant attributeSpecifierSequence? gnuAttributes? ('=' constantExpression)?
    ;

// ISO C: enum-type-specifier (6.7.3.3)
enumTypeSpecifier
    : specifierQualifierList
    ;

// ISO C: atomic-type-specifier (6.7.3.5)
atomicTypeSpecifier
    : '_Atomic' '(' typeName ')'
    ;

// ISO C: typeof-specifier (6.7.3.6)
typeofSpecifier
    : (Typeof | Typeof_unqual) '(' typeofSpecifierArgument ')'
    ;

// ISO C: typeof-specifier-argument (6.7.3.6)
typeofSpecifierArgument
    : expression
    | typeName
    ;

// ISO C: type-qualifier (6.7.4.1)
typeQualifier
    : 'const'
    | Restrict
    | Volatile
    | '_Atomic'
    ;

// ISO C: function-specifier (6.7.5)
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

// ISO C: alignment-specifier (6.7.6)
alignmentSpecifier
    : Alignas '(' (typeName | constantExpression) ')'
    ;

// ISO C: declarator (6.7.7.1)
// This rule is basically what was implemented in the GCC compiler.
// https://github.com/gcc-mirror/gcc/blob/f5cda36f16d447198c1e00b191d720b6f4a02876/gcc/c/c-parser.cc#L4975-L4995
declarator
    : (gnuAttribute? pointer declarationSpecifiers?)* (gnuAttribute* directDeclarator gccDeclaratorExtension*) {this.EnterDeclaration();}
    ;

// ISO C: direct-declarator (6.7.7.1)
// The rules from the spec were refactored. array-declarator and function-declarator
// were unfolded into direct-declarator. Those rules were deleted.
directDeclarator
    : (
	Identifier attributeSpecifierSequence?
	| '(' declarator ')'
	| Identifier ':' DigitSequence         // bit field
	| vcSpecificModifer Identifier         // Visual C Extension
	| '(' vcSpecificModifer declarator ')' // Visual C Extension
	| gnuAttribute
    )
    ( '[' typeQualifierList? assignmentExpression? ']' attributeSpecifierSequence?
      | '[' 'static' typeQualifierList? assignmentExpression ']' attributeSpecifierSequence?
      | '[' typeQualifierList 'static' assignmentExpression ']' attributeSpecifierSequence?
      | '[' typeQualifierList? '*' ']' attributeSpecifierSequence?
      | '(' parameterTypeList ')' attributeSpecifierSequence?
    )*
    ;

// ISO C: array-declarator (6.7.7.1) - No ANTLR4 rule
// ISO C: function-declarator (6.7.7.1) - No ANTLR4 rule

// ISO C: pointer (6.7.7.1)
pointer
    : (('*' | '^') typeQualifierList?)+ // ^ - Blocks language extension
    ;

// ISO C: type-qualifier-list (6.7.7.1)
typeQualifierList
    : typeQualifier+
    ;

// ISO C: parameter-type-list (6.7.7.1)
parameterTypeList
    : parameterList (',' '...')?
    | '...'
    ;

// ISO C: parameter-list (6.7.7.1)
parameterList
    : parameterDeclaration (',' parameterDeclaration)*
    ;

// ISO C: parameter-declaration (6.7.7.1)
// Slightly refactored from spec.
// parameter-declaration:
//   attribute-specifier-sequenceopt declaration-specifiers declarator
//   attribute-specifier-sequenceopt declaration-specifiers abstract-declaratoropt
//
// Includes disambiguating predicate.
// Note the spec states parameter-declataion cannot be empty. But, we
// found situations where it must be allowed.
parameterDeclaration
    : (attributeSpecifierSequence? ({this.IsDeclarationSpecifier()}? declarationSpecifiers | ) )
	(declarator | abstractDeclarator | )
    ;

// ISO C: type-name (6.7.8)
typeName
    : specifierQualifierList abstractDeclarator?
    ;

// ISO C: abstract-declarator (6.7.8)
abstractDeclarator
    : vcSpecificModifer? pointer
    | vcSpecificModifer? pointer? directAbstractDeclarator gccDeclaratorExtension*
    ;

// ISO C: direct-abstract-declarator (6.7.8)
directAbstractDeclarator
    : '(' abstractDeclarator ')' gccDeclaratorExtension*
    | '[' typeQualifierList? assignmentExpression? ']'
    | '[' 'static' typeQualifierList? assignmentExpression ']'
    | '[' typeQualifierList 'static' assignmentExpression ']'
    | '[' '*' ']'
    | '(' parameterTypeList ')' gccDeclaratorExtension*
    | directAbstractDeclarator '[' typeQualifierList? assignmentExpression? ']'
    | directAbstractDeclarator '[' 'static' typeQualifierList? assignmentExpression ']'
    | directAbstractDeclarator '[' typeQualifierList 'static' assignmentExpression ']'
    | directAbstractDeclarator '[' '*' ']'
    | directAbstractDeclarator '(' parameterTypeList ')' gccDeclaratorExtension*
    ;

// ISO C: array-abstract-declarator (6.7.8) - No ANTLR4 rule
// ISO C: function-abstract-declarator (6.7.8) - No ANTLR4 rule

// ISO C: typedef-name (6.7.9)
typedefName
    : {this.IsTypedefName()}? Identifier
    ;

// ISO C: braced-initializer (6.7.11) - No ANTLR4 rule

// ISO C: initializer (6.7.11)
initializer
    : assignmentExpression
    | '{' initializerList ','? '}'
    | '{' '}' //GNU https://github.com/gcc-mirror/gcc/blob/77ab3b07385f23b39a2445011068c04e0872b481/gcc/c/c-parser.cc#L6542
    ;

// ISO C: initializer-list (6.7.11)
initializerList
    : designation? initializer (',' designation? initializer)*
    ;

// ISO C: designation (6.7.11)
designation
    : designatorList '='
    | gnuArrayDesignator //GNU https://github.com/gcc-mirror/gcc/blob/77ab3b07385f23b39a2445011068c04e0872b481/gcc/c/c-parser.cc#L6545-L6546
    | gnuIdentifier ':' //GNU
    ;

// ISO C: designator-list (6.7.11)
designatorList
    : designator+
    ;

// ISO C: designator (6.7.11)
designator
    : gnuArrayDesignator //GNU
    | '.' Identifier
    ;

// ISO C: static_assert-declaration (6.7.12)
staticAssertDeclaration
    : '_Static_assert' '(' constantExpression (',' StringLiteral)? ')' ';'
    ;

// ISO C: attribute-specifier-sequence (6.7.13.2)
attributeSpecifierSequence
    : attributeSpecifier+
    ;

// ISO C: attribute-specifier (6.7.13.2)
attributeSpecifier
    : '[' '[' attributeList ']' ']'
    ;

// ISO C: attribute-list (6.7.13.2)
attributeList
    : attribute (',' attribute)* // May not be correct.
    ;

// ISO C: attribute (6.7.13.2)
attribute
    : attributeToken attributeArgumentClause?
    ;

// ISO C: attribute-token (6.7.13.2)
attributeToken
    : Identifier
    | Identifier ':' ':' Identifier
    ;

// ISO C: standard-attribute (6.7.13.2) - No ANTLR4 rule
// ISO C: attribute-prefixed-token (6.7.13.2) - No ANTLR4 rule
// ISO C: attribute-prefix (6.7.13.2) - No ANTLR4 rule

// ISO C: attribute-argument-clause (6.7.13.2)
attributeArgumentClause
    : '(' balancedTokenSequence? ')'
    ;

// ISO C: balanced-token-sequence (6.7.13.2)
balancedTokenSequence
    : balancedToken+
    ;

// ISO C: balanced-token (6.7.13.2)
balancedToken
    : '(' balancedTokenSequence? ')'
    | '[' balancedTokenSequence? ']'
    | '{' balancedTokenSequence? '}'
    // any token other than a parenthesis, bracket, or brace
    ;

// ISO C: statement (6.8.1)
statement
    : labeledStatement
    | compoundStatement
    | expressionStatement
    | selectionStatement
    | iterationStatement
    | jumpStatement
    | asmStatement
    ;

// ISO C: unlabeled-statement (6.8.1) - No ANTLR4 rule
// ISO C: primary-block (6.8.1) - No ANTLR4 rule
// ISO C: secondary-block (6.8.1) - No ANTLR4 rule
// ISO C: label (6.8.2) - No ANTLR4 rule

// ISO C: labeled-statement (6.8.2)
labeledStatement
    : Identifier ':' statement?
    | Label Identifier ';'
    | 'case' constantExpression ':' statement
    | 'default' ':' statement
    ;

// ISO C: compound-statement (6.8.3)
compoundStatement
    : '{' {this.EnterScope();} blockItemList? '}' {this.ExitScope();}
    ;

// ISO C: block-item-list (6.8.3)
blockItemList
    : blockItem+
    ;

// ISO C: block-item (6.8.3)
blockItem
    : {this.IsStatement()}? statement
    | {this.IsDeclaration()}? declaration
    ;

// ISO C: expression-statement (6.8.4)
expressionStatement
    : expression? ';'
    ;

// ISO C: selection-statement (6.8.5.1)
selectionStatement
    : 'if' '(' expression ')' statement ('else' statement)?
    | 'switch' '(' expression ')' statement
    ;

// ISO C: iteration-statement (6.8.6.1)
iterationStatement
    : While '(' expression ')' statement
    | Do statement While '(' expression ')' ';'
    | For '(' forCondition ')' statement
    ;

// forCondition
forCondition
    : (forDeclaration | expression?) ';' forExpression? ';' forExpression?
    ;

// forDeclaration
forDeclaration
    : declarationSpecifiers initDeclaratorList?
    ;

// forExpression
forExpression
    : assignmentExpression (',' assignmentExpression)*
    ;

// ISO C: jump-statement (6.8.7.1)
jumpStatement
    : (
        'goto' Identifier
        | 'continue'
        | 'break'
        | 'return' expression?
        | 'goto' unaryExpression // GCC extension
    ) ';'
    ;

// ISO C: translation-unit (6.9.1)
translationUnit
    : externalDeclaration+
    ;

// ISO C: external-declaration (6.9.1)
externalDeclaration
    : '__extension__'? (
	functionDefinition
	| declaration
	| ';' // stray ;
	| asmDefinition // GCC
	)
    ;

// ISO C: function-definition (6.9.2)
functionDefinition
    : attributeSpecifierSequence? declarationSpecifiers? declarator declarationList? functionBody
    ;

// declarationList
declarationList
    : declaration+
    ;

// ISO C: function-body (6.9.2)
functionBody
    : compoundStatement
    ;

// ISO C: preprocessing-file (6.10.1) - No ANTLR4 rule
// ISO C: group (6.10.1) - No ANTLR4 rule
// ISO C: group-part (6.10.1) - No ANTLR4 rule
// ISO C: if-section (6.10.1) - No ANTLR4 rule
// ISO C: if-group (6.10.1) - No ANTLR4 rule
// ISO C: elif-groups (6.10.1) - No ANTLR4 rule
// ISO C: elif-group (6.10.1) - No ANTLR4 rule
// ISO C: else-group (6.10.1) - No ANTLR4 rule
// ISO C: endif-line (6.10.1) - No ANTLR4 rule
// ISO C: control-line (6.10.1) - No ANTLR4 rule
// ISO C: text-line (6.10.1) - No ANTLR4 rule
// ISO C: non-directive (6.10.1) - No ANTLR4 rule
// ISO C: lparen (6.10.1) - No ANTLR4 rule
// ISO C: replacement-list (6.10.1) - No ANTLR4 rule
// ISO C: pp-tokens (6.10.1) - No ANTLR4 rule
// ISO C: new-line (6.10.1) - No ANTLR4 rule

// ISO C: identifier-list (6.10.1)
identifierList
    : Identifier (',' Identifier)*
    ;

// ISO C: pp-parameter (6.10.1) - No ANTLR4 rule
// ISO C: pp-parameter-name (6.10.1) - No ANTLR4 rule
// ISO C: pp-standard-parameter (6.10.1) - No ANTLR4 rule
// ISO C: pp-prefixed-parameter (6.10.1) - No ANTLR4 rule
// ISO C: pp-balanced-token-sequence (6.10.1) - No ANTLR4 rule
// ISO C: pp-balanced-token (6.10.1) - No ANTLR4 rule
// ISO C: embed-parameter-sequence (6.10.1) - No ANTLR4 rule
// ISO C: defined-macro-expression (6.10.1) - No ANTLR4 rule
// ISO C: h-preprocessing-token (6.10.1) - No ANTLR4 rule
// ISO C: h-pp-tokens (6.10.1) - No ANTLR4 rule
// ISO C: header-name-tokens (6.10.1) - No ANTLR4 rule
// ISO C: has-include-expression (6.10.1) - No ANTLR4 rule
// ISO C: has-embed-expression (6.10.1) - No ANTLR4 rule
// ISO C: has-c-attribute-express (6.10.1) - No ANTLR4 rule
// ISO C: va-opt-replacement (6.10.1) - No ANTLR4 rule
// ISO C: standard-pragma (6.10.8) - No ANTLR4 rule
// ISO C: on-off-switch (6.10.8) - No ANTLR4 rule
// ISO C: direction (6.10.8) - No ANTLR4 rule
// ISO C: dec-direction (6.10.8) - No ANTLR4 rule
// ISO C: n-char-sequence (7.24.2.6) - No ANTLR4 rule
// ISO C: n-wchar-sequence (7.31.4.2.2) - No ANTLR4 rule
// ISO C: d-char-sequence (7.24.2.7) - No ANTLR4 rule
// ISO C: d-wchar-sequence (7.31.4.2.3) - No ANTLR4 rule

// GNU: gnuArrayDesignator
// https://github.com/gcc-mirror/gcc/blob/77ab3b07385f23b39a2445011068c04e0872b481/gcc/c/c-parser.cc#L6548-L6549
gnuArrayDesignator
    : '[' constantExpression ('...' constantExpression)? ']'
    ;

// GNU: gnuIdentifier
gnuIdentifier
    : Identifier
    ;

// GNU: asmArgument
asmArgument
    : asmStringLiteral
    | asmStringLiteral ':' asmOperands? ( ':' asmOperands? (':' asmClobbers? )* )?
    ;

// GNU: asmClobbers
asmClobbers
    : (asmStringLiteral | Identifier) ( ',' (asmStringLiteral | Identifier) )*
    ;

// GNU: asmDefinition
asmDefinition
    : simpleAsmExpr
    | Asm '(' toplevelAsmArgument ')'
    ;

// GNU: toplevelAsmArgument
toplevelAsmArgument
    : asmStringLiteral
    | asmStringLiteral ':' asmOperands?
    | asmStringLiteral ':' asmOperands? ':' asmOperands?
    ;

// GNU: asmOperand
asmOperand
    : asmStringLiteral '(' expression ')'
    | '[' Identifier ']' asmStringLiteral '(' expression ')'
    ;

// GNU: asmOperands
asmOperands
    : asmOperand (',' asmOperand)*
    ;

// GNU: asmQualifier
asmQualifier
    : Volatile
    | Inline
    | 'goto'
    ;

// GNU: asmQualifierList
asmQualifierList
    : asmQualifier+
    ;

// GNU: asmStatement
asmStatement
    : Asm asmQualifierList? '(' asmArgument ')' ';'
    ;

// GNU: asmStringLiteral
asmStringLiteral
    : StringLiteral
    ;

// GNU: gccDeclaratorExtension
gccDeclaratorExtension
    : asmDefinition
    | gnuAttribute
    ;

// GNU: gnuAttribute
gnuAttribute
    : Attribute '(' '(' gnuAttributeList ')' ')'
    ;

// GNU: gnuAttributeList
gnuAttributeList
    : gnuSingleAttribute*
    ;

// GNU: gnuAttributes
gnuAttributes
    : gnuAttribute+
    ;

// GNU: gnuSingleAttribute
gnuSingleAttribute
    : ~('(' | ')')
    | '(' gnuAttributeList ')'
    ;

// GNU: simpleAsmExpr
simpleAsmExpr
    : Asm '(' asmStringLiteral ')'
    ;

// Visual C: vcSpecificModifer
vcSpecificModifer
    : '__cdecl'
    | '__clrcall'
    | '__stdcall'
    | '__fastcall'
    | '__thiscall'
    | '__vectorcall'
    ;
