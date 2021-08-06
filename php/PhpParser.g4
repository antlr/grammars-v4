/*
PHP grammar.
The MIT License (MIT).
Copyright (c) 2015-2020, Ivan Kochurkin (kvanttt@gmail.com), Positive Technologies.
Copyright (c) 2019-2020, Student Main for php7, php8 support.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
*/

parser grammar PhpParser;

options { tokenVocab=PhpLexer; }

// HTML
// Also see here: https://github.com/antlr/grammars-v4/tree/master/html

htmlDocument
    : Shebang? (inlineHtml | phpBlock)* EOF
    ;

inlineHtml
    : htmlElement+
    | scriptText
    ;

// TODO: split into html, css and xml elements
htmlElement
    : HtmlDtd
    | HtmlClose
    | HtmlStyleOpen
    | HtmlOpen
    | HtmlName
    | HtmlSlashClose
    | HtmlSlash
    | HtmlText
    | HtmlEquals
    | HtmlStartQuoteString
    | HtmlEndQuoteString
    | HtmlStartDoubleQuoteString
    | HtmlEndDoubleQuoteString
    | HtmlHex
    | HtmlDecimal
    | HtmlQuoteString
    | HtmlDoubleQuoteString

    | StyleBody

    | HtmlScriptOpen
    | HtmlScriptClose

    | XmlStart XmlText* XmlClose
    ;

// Script
// Parse JavaScript with https://github.com/antlr/grammars-v4/tree/master/javascript if necessary.

scriptText
    : ScriptText+
    ;

// PHP

phpBlock
    : importStatement* topStatement+
    ;

importStatement
    : Import Namespace namespaceNameList SemiColon
    ;

topStatement
    : statement
    | useDeclaration
    | namespaceDeclaration
    | functionDeclaration
    | classDeclaration
    | globalConstantDeclaration
    ;

useDeclaration
    : Use (Function_ | Const)? useDeclarationContentList SemiColon
    ;

useDeclarationContentList
    : '\\'? useDeclarationContent (',' '\\'? useDeclarationContent)*
    ;

useDeclarationContent
    : namespaceNameList
    ;

namespaceDeclaration
    : Namespace (namespaceNameList? OpenCurlyBracket namespaceStatement* CloseCurlyBracket | namespaceNameList SemiColon)
    ;

namespaceStatement
    : statement
    | useDeclaration
    | functionDeclaration
    | classDeclaration
    | globalConstantDeclaration
    ;

functionDeclaration
    : attributes? Function_ '&'? identifier typeParameterListInBrackets? '(' formalParameterList ')' (':' QuestionMark? typeHint)? blockStatement
    ;

classDeclaration
    : attributes? Private? modifier? Partial? (
      classEntryType identifier typeParameterListInBrackets? (Extends qualifiedStaticTypeRef)? (Implements interfaceList)?
    | Interface identifier typeParameterListInBrackets? (Extends interfaceList)? )
      OpenCurlyBracket classStatement* CloseCurlyBracket
    ;

classEntryType
    : Class
    | Trait
    ;

interfaceList
    : qualifiedStaticTypeRef (',' qualifiedStaticTypeRef)*
    ;

typeParameterListInBrackets
    : '<:' typeParameterList ':>'
    | '<:' typeParameterWithDefaultsList ':>'
    | '<:' typeParameterList ',' typeParameterWithDefaultsList ':>'
    ;

typeParameterList
    : typeParameterDecl (',' typeParameterDecl)*
    ;

typeParameterWithDefaultsList
    : typeParameterWithDefaultDecl (',' typeParameterWithDefaultDecl)*
    ;

typeParameterDecl
    : attributes? identifier
    ;

typeParameterWithDefaultDecl
    : attributes? identifier Eq (qualifiedStaticTypeRef | primitiveType)
    ;

genericDynamicArgs
    : '<:' typeRef (',' typeRef)* ':>'
    ;

attributes
    : attributeGroup+
    ;

attributeGroup
    : AttributeStart (identifier ':')? attribute (',' attribute)* ']'
    ;

attribute
    : qualifiedNamespaceName arguments?
    ;

innerStatementList
    : innerStatement*
    ;

innerStatement
    : statement
    | functionDeclaration
    | classDeclaration
    ;

// Statements

statement
    : identifier ':'
    | blockStatement
    | ifStatement
    | whileStatement
    | doWhileStatement
    | forStatement
    | switchStatement
    | breakStatement
    | continueStatement
    | returnStatement
    | yieldExpression SemiColon
    | globalStatement
    | staticVariableStatement
    | echoStatement
    | expressionStatement
    | unsetStatement
    | foreachStatement
    | tryCatchFinally
    | throwStatement
    | gotoStatement
    | declareStatement
    | emptyStatement
    | inlineHtmlStatement
    ;

emptyStatement
    : SemiColon
    ;

blockStatement
    : OpenCurlyBracket innerStatementList CloseCurlyBracket
    ;

ifStatement
    : If parentheses statement elseIfStatement* elseStatement?
    | If parentheses ':' innerStatementList elseIfColonStatement* elseColonStatement? EndIf SemiColon
    ;

elseIfStatement
    : ElseIf parentheses statement
    ;

elseIfColonStatement
    : ElseIf parentheses ':' innerStatementList
    ;

elseStatement
    : Else statement
    ;

elseColonStatement
    : Else ':' innerStatementList
    ;

whileStatement
    : While parentheses (statement | ':' innerStatementList EndWhile SemiColon)
    ;

doWhileStatement
    : Do statement While parentheses SemiColon
    ;

forStatement
    : For '(' forInit? SemiColon expressionList? SemiColon forUpdate? ')' (statement | ':' innerStatementList EndFor SemiColon )
    ;

forInit
    : expressionList
    ;

forUpdate
    : expressionList
    ;

switchStatement
    : Switch parentheses (OpenCurlyBracket SemiColon? switchBlock* CloseCurlyBracket | ':' SemiColon? switchBlock* EndSwitch SemiColon)
    ;

switchBlock
    : ((Case expression | Default) (':' | SemiColon))+ innerStatementList
    ;

breakStatement
    : Break expression? SemiColon
    ;

continueStatement
    : Continue expression? SemiColon
    ;

returnStatement
    : Return expression? SemiColon
    ;

expressionStatement
    : expression SemiColon
    ;

unsetStatement
    : Unset '(' chainList ')' SemiColon
    ;

foreachStatement
    : Foreach
        ( '(' chain As '&'? assignable ('=>' '&'? chain)? ')'
        | '(' expression As assignable ('=>' '&'? chain)? ')'
        | '(' chain As List '(' assignmentList ')' ')' )
      (statement | ':' innerStatementList EndForeach SemiColon)
    ;

tryCatchFinally
    : Try blockStatement (catchClause+ finallyStatement? | catchClause* finallyStatement)
    ;

catchClause
    : Catch '(' qualifiedStaticTypeRef ('|' qualifiedStaticTypeRef)* VarName ')' blockStatement
    ;

finallyStatement
    : Finally blockStatement
    ;

throwStatement
    : Throw expression SemiColon
    ;

gotoStatement
    : Goto identifier SemiColon
    ;

declareStatement
    : Declare '(' declareList ')' (statement | ':' innerStatementList EndDeclare SemiColon)
    ;

inlineHtmlStatement
    : inlineHtml+
    ;

declareList
    : identifierInitializer (',' identifierInitializer)*
    ;

formalParameterList
    : formalParameter? (',' formalParameter)* ','?
    ;

formalParameter
    : attributes? memberModifier? QuestionMark? typeHint? '&'? '...'? variableInitializer
    ;

typeHint
    : qualifiedStaticTypeRef
    | Callable
    | primitiveType
    | typeHint '|' typeHint
    ;

globalStatement
    : Global globalVar (',' globalVar)* SemiColon
    ;

globalVar
    : VarName
    | Dollar chain
    | Dollar OpenCurlyBracket expression CloseCurlyBracket
    ;

echoStatement
    : Echo expressionList SemiColon
    ;

staticVariableStatement
    : Static variableInitializer (',' variableInitializer)* SemiColon
    ;

classStatement
    : attributes? ( propertyModifiers typeHint? variableInitializer (',' variableInitializer)* SemiColon
                  | memberModifiers? ( Const typeHint? identifierInitializer (',' identifierInitializer)* SemiColon
                                     | Function_ '&'? identifier typeParameterListInBrackets? '(' formalParameterList ')'
                                       baseCtorCall? methodBody))
    | Use qualifiedNamespaceNameList traitAdaptations
    ;

traitAdaptations
    : SemiColon
    | OpenCurlyBracket traitAdaptationStatement* CloseCurlyBracket
    ;

traitAdaptationStatement
    : traitPrecedence
    | traitAlias
    ;

traitPrecedence
    : qualifiedNamespaceName '::' identifier InsteadOf qualifiedNamespaceNameList SemiColon
    ;

traitAlias
    : traitMethodReference As (memberModifier | memberModifier? identifier) SemiColon
    ;

traitMethodReference
    : (qualifiedNamespaceName '::')? identifier
    ;

baseCtorCall
    : ':' identifier arguments?
    ;

methodBody
    : SemiColon
    | blockStatement
    ;

propertyModifiers
    : memberModifiers
    | Var
    ;

memberModifiers
    : memberModifier+
    ;

variableInitializer
    : VarName (Eq constantInitializer)?
    ;

identifierInitializer
    : identifier Eq constantInitializer
    ;

globalConstantDeclaration
    : attributes? Const identifierInitializer (',' identifierInitializer)* SemiColon
    ;

expressionList
    : expression (',' expression)*
    ;

parentheses
    : '(' (expression | yieldExpression) ')'
    ;

// Expressions
// Grouped by priorities: http://php.net/manual/en/language.operators.precedence.php
expression
    : Clone expression                                          #CloneExpression
    | newExpr                                                   #NewExpression

    | stringConstant '[' expression ']'                         #IndexerExpression

    | '(' castOperation ')' expression                          #CastExpression
    | ('~' | '@') expression                                    #UnaryOperatorExpression

    | ('!' | '+' | '-') expression                              #UnaryOperatorExpression

    | ('++' | '--') chain                                       #PrefixIncDecExpression
    | chain ('++' | '--')                                       #PostfixIncDecExpression

    | Print expression                                          #PrintExpression

    | chain                                                     #ChainExpression
    | constant                                                  #ScalarExpression
    | string                                                    #ScalarExpression
    | Label                                                     #ScalarExpression

    | BackQuoteString                                           #BackQuoteStringExpression
    | parentheses                                               #ParenthesisExpression
    | arrayCreation                                             #ArrayCreationExpression

    | Yield                                                     #SpecialWordExpression
    | List '(' assignmentList ')' Eq expression                 #SpecialWordExpression
    | IsSet '(' chainList ')'                                   #SpecialWordExpression
    | Empty '(' chain ')'                                       #SpecialWordExpression
    | Eval '(' expression ')'                                   #SpecialWordExpression
    | Exit ( '(' ')' | parentheses )?                           #SpecialWordExpression
    | (Include | IncludeOnce) expression                        #SpecialWordExpression
    | (Require | RequireOnce) expression                        #SpecialWordExpression

    | lambdaFunctionExpr                                        #LambdaFunctionExpression
    | matchExpr                                                 #MatchExpression

    | <assoc=right> expression op='**' expression               #ArithmeticExpression
    | expression InstanceOf typeRef                             #InstanceOfExpression
    | expression op=('*' | Divide | '%') expression             #ArithmeticExpression

    | expression op=('+' | '-' | '.') expression                #ArithmeticExpression

    | expression op=('<<' | '>>') expression                    #ComparisonExpression
    | expression op=(Less | '<=' | Greater | '>=') expression   #ComparisonExpression
    | expression op=('===' | '!==' | '==' | IsNotEq) expression #ComparisonExpression

    | expression op='&' expression                              #BitwiseExpression
    | expression op='^' expression                              #BitwiseExpression
    | expression op='|' expression                              #BitwiseExpression
    | expression op='&&' expression                             #BitwiseExpression
    | expression op='||' expression                             #BitwiseExpression

    | expression op=QuestionMark expression? ':' expression     #ConditionalExpression
    | expression op='??' expression                             #NullCoalescingExpression
    | expression op='<=>' expression                            #SpaceshipExpression

    | Throw expression                                          #SpecialWordExpression

    | assignable assignmentOperator attributes? expression      #AssignmentExpression
    | assignable Eq attributes? '&' (chain | newExpr)           #AssignmentExpression

    | expression op=LogicalAnd expression                       #LogicalExpression
    | expression op=LogicalXor expression                       #LogicalExpression
    | expression op=LogicalOr expression                        #LogicalExpression
    ;

assignable
    : chain
    | arrayCreation
    ;

arrayCreation
    : (Array '(' arrayItemList? ')' | '[' arrayItemList? ']') ('[' expression ']')?
    ;

lambdaFunctionExpr
    : Static? Function_ '&'? '(' formalParameterList ')' lambdaFunctionUseVars? (':' typeHint)? blockStatement
    | LambdaFn '(' formalParameterList')' '=>' expression
    ;

matchExpr
    : Match '(' expression ')' OpenCurlyBracket matchItem (',' matchItem)* ','? CloseCurlyBracket
    ;

matchItem
    : expression (',' expression)* '=>' expression
    ;

newExpr
    : New typeRef arguments?
    ;

assignmentOperator
    : Eq
    | '+='
    | '-='
    | '*='
    | '**='
    | '/='
    | '.='
    | '%='
    | '&='
    | '|='
    | '^='
    | '<<='
    | '>>='
    | '??='
    ;

yieldExpression
    : Yield (expression ('=>' expression)? | From expression)
    ;

arrayItemList
    : arrayItem (',' arrayItem)* ','?
    ;

arrayItem
    : expression ('=>' expression)?
    | (expression '=>')? '&' chain
    ;

lambdaFunctionUseVars
    : Use '(' lambdaFunctionUseVar (',' lambdaFunctionUseVar)* ')'
    ;

lambdaFunctionUseVar
    : '&'? VarName
    ;

qualifiedStaticTypeRef
    : qualifiedNamespaceName genericDynamicArgs?
    | Static
    ;

typeRef
    : (qualifiedNamespaceName | indirectTypeRef) genericDynamicArgs?
    | primitiveType
    | Static
    | anonymousClass
    ;

anonymousClass
    : attributes? Private? modifier? Partial? (
      classEntryType typeParameterListInBrackets? (Extends qualifiedStaticTypeRef)? (Implements interfaceList)?
    | Interface identifier typeParameterListInBrackets? (Extends interfaceList)? )
      OpenCurlyBracket classStatement* CloseCurlyBracket
    ;

indirectTypeRef
    : chainBase ('->' keyedFieldName)*
    ;

qualifiedNamespaceName
    : Namespace? '\\'? namespaceNameList
    ;

namespaceNameList
    : identifier
    | identifier ('\\' identifier)* ('\\' namespaceNameTail)?
    ;

namespaceNameTail
    : identifier (As identifier)?
    | OpenCurlyBracket namespaceNameTail (','namespaceNameTail)* ','? CloseCurlyBracket
    ;

qualifiedNamespaceNameList
    : qualifiedNamespaceName (',' qualifiedNamespaceName)*
    ;

arguments
    : '(' ( actualArgument (',' actualArgument)* | yieldExpression)? ','? ')'
    ;

actualArgument
    : argumentName? '...'? expression
    | '&' chain
    ;

argumentName
    : identifier ':'
    ;

constantInitializer
    : constant
    | string
    | Array '(' (arrayItemList ','?)? ')'
    | '[' (arrayItemList ','?)? ']'
    | ('+' | '-') constantInitializer
    ;

constant
    : Null
    | literalConstant
    | magicConstant
    | classConstant
    | qualifiedNamespaceName
    ;

literalConstant
    : Real
    | BooleanConstant
    | numericConstant
    | stringConstant
    ;

numericConstant
    : Octal
    | Decimal
    | Hex
    | Binary
    ;

classConstant
    : (Class | Parent_) '::' (identifier | Constructor | Get | Set)
    | (qualifiedStaticTypeRef | keyedVariable | string) '::' (identifier | keyedVariable) // 'foo'::$bar works in php7
    ;

stringConstant
    : Label
    ;

string
    : StartHereDoc HereDocText+
    | StartNowDoc HereDocText+
    | SingleQuoteString
    | DoubleQuote interpolatedStringPart* DoubleQuote
    ;

interpolatedStringPart
    : StringPart
    | UnicodeEscape
    | chain
    ;

chainList
    : chain (',' chain)*
    ;

chain
    : chainOrigin memberAccess*
    //| arrayCreation // [$a,$b]=$c
    ;

chainOrigin
    : chainBase
    | functionCall
    | '(' newExpr ')'
    ;

memberAccess
    : '->' keyedFieldName actualArguments?
    ;

functionCall
    : functionCallName actualArguments
    ;

functionCallName
    : qualifiedNamespaceName
    | classConstant
    | chainBase
    | parentheses
    ;

actualArguments
    : genericDynamicArgs? arguments squareCurlyExpression*
    ;

chainBase
    : keyedVariable ('::' keyedVariable)?
    | qualifiedStaticTypeRef '::' keyedVariable
    ;

keyedFieldName
    : keyedSimpleFieldName
    | keyedVariable
    ;

keyedSimpleFieldName
    : (identifier | OpenCurlyBracket expression CloseCurlyBracket) squareCurlyExpression*
    ;

keyedVariable
    : Dollar* (VarName | Dollar OpenCurlyBracket expression CloseCurlyBracket) squareCurlyExpression*
    ;

squareCurlyExpression
    : '[' expression? ']'
    | OpenCurlyBracket expression CloseCurlyBracket
    ;

assignmentList
    : assignmentListElement? (',' assignmentListElement?)*
    ;

assignmentListElement
    : chain
    | List '('  assignmentList ')'
    | arrayItem
    ;

modifier
    : Abstract
    | Final
    ;

identifier
    : Label

    | Abstract
    | Array
    | As
    | BinaryCast
    | BoolType
    | BooleanConstant
    | Break
    | Callable
    | Case
    | Catch
    | Class
    | Clone
    | Const
    | Continue
    | Declare
    | Default
    | Do
    | DoubleCast
    | DoubleType
    | Echo
    | Else
    | ElseIf
    | Empty
    | EndDeclare
    | EndFor
    | EndForeach
    | EndIf
    | EndSwitch
    | EndWhile
    | Eval
    | Exit
    | Extends
    | Final
    | Finally
    | FloatCast
    | For
    | Foreach
    | Function_
    | Global
    | Goto
    | If
    | Implements
    | Import
    | Include
    | IncludeOnce
    | InstanceOf
    | InsteadOf
    | Int16Cast
    | Int64Type
    | Int8Cast
    | Interface
    | IntType
    | IsSet
    | List
    | LogicalAnd
    | LogicalOr
    | LogicalXor
    | Namespace
    | New
    | Null
    | ObjectType
    | Parent_
    | Partial
    | Print
    | Private
    | Protected
    | Public
    | Require
    | RequireOnce
    | Resource
    | Return
    | Static
    | StringType
    | Switch
    | Throw
    | Trait
    | Try
    | Typeof
    | UintCast
    | UnicodeCast
    | Unset
    | Use
    | Var
    | While
    | Yield
    | From

    | Get
    | Set
    | Call
    | CallStatic
    | Constructor
    | Destruct
    | Wakeup
    | Sleep
    | Autoload
    | IsSet__
    | Unset__
    | ToString__
    | Invoke
    | SetState
    | Clone__
    | DebugInfo
    | Namespace__
    | Class__
    | Traic__
    | Function__
    | Method__
    | Line__
    | File__
    | Dir__
    ;

memberModifier
    : Public
    | Protected
    | Private
    | Static
    | Abstract
    | Final
    ;

magicConstant
    : Namespace__
    | Class__
    | Traic__
    | Function__
    | Method__
    | Line__
    | File__
    | Dir__
    ;

magicMethod
    : Get
    | Set
    | Call
    | CallStatic
    | Constructor
    | Destruct
    | Wakeup
    | Sleep
    | Autoload
    | IsSet__
    | Unset__
    | ToString__
    | Invoke
    | SetState
    | Clone__
    | DebugInfo
    ;

primitiveType
    : BoolType
    | IntType
    | Int64Type
    | DoubleType
    | StringType
    | Resource
    | ObjectType
    | Array
    ;

castOperation
    : BoolType
    | Int8Cast
    | Int16Cast
    | IntType
    | Int64Type
    | UintCast
    | DoubleCast
    | DoubleType
    | FloatCast
    | StringType
    | BinaryCast
    | UnicodeCast
    | Array
    | ObjectType
    | Resource
    | Unset
    ;
