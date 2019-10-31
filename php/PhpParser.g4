/*
PHP grammar.
The MIT License (MIT).
Copyright (c) 2015-2017, Ivan Kochurkin (kvanttt@gmail.com), Positive Technologies.
Copyright (c) 2019, Student Main for php7 support.

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
    : Shebang? htmlElementOrPhpBlock* EOF
    ;

htmlElementOrPhpBlock
    : htmlElements
    | phpBlock
    | scriptTextPart
    ;

htmlElements
    : htmlElement+
    ;

htmlElement
    : HtmlDtd
    | HtmlScriptOpen
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
    
    | ScriptClose

    | XmlStart XmlText* XmlClose
    ;

// Script
// Parse JavaScript with https://github.com/antlr/grammars-v4/tree/master/ecmascript if necessary.
    
scriptTextPart
    : ScriptText+
    ;

// PHP
    
phpBlock
    : importStatement* topStatement+
    ;

importStatement
    : Import Namespace namespaceNameList ';'
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
    : Use (Function | Const)? useDeclarationContentList ';'
    ;

useDeclarationContentList
    : '\\'? useDeclarationContent (',' '\\'? useDeclarationContent)*
    ;

useDeclarationContent
    : namespaceNameList
    ;

namespaceDeclaration
    : Namespace (namespaceNameList? OpenCurlyBracket namespaceStatement* '}' | namespaceNameList ';')
    ;

namespaceStatement
    : statement
    | useDeclaration
    | functionDeclaration
    | classDeclaration
    | globalConstantDeclaration
    ;

functionDeclaration
    : attributes Function '&'? identifier typeParameterListInBrackets? '(' formalParameterList ')' (':' QuestionMark? typeHint)? blockStatement
    ;

classDeclaration
    : attributes Private? modifier? Partial? (
      classEntryType identifier typeParameterListInBrackets? (Extends qualifiedStaticTypeRef)? (Implements interfaceList)? 
    | Interface identifier typeParameterListInBrackets? (Extends interfaceList)? )
      OpenCurlyBracket classStatement* '}'
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
    : attributes identifier
    ;

typeParameterWithDefaultDecl
    : attributes identifier Eq (qualifiedStaticTypeRef | primitiveType)
    ;

genericDynamicArgs
    : '<:' typeRef (',' typeRef)* ':>'
    ;

attributes
    : attributesGroup*
    ;

attributesGroup
    : '[' (identifier ':')? attribute (',' attribute)* ']'
    ;

attribute
    : qualifiedNamespaceName
    | qualifiedNamespaceName '(' attributeArgList ')'
    | qualifiedNamespaceName '(' attributeNamedArgList ')'
    | qualifiedNamespaceName '(' attributeArgList ',' attributeNamedArgList ')'
    ;

attributeArgList
    : expression (',' expression)*
    ;

attributeNamedArgList
    : attributeNamedArg (',' attributeNamedArg)*
    ;

attributeNamedArg
    : VarName '=>' expression
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
    | yieldExpression ';'
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
    : ';'
    ;

blockStatement
    : OpenCurlyBracket innerStatementList '}'
    ;
    
ifStatement
    : If parentheses statement elseIfStatement* elseStatement?
    | If parentheses ':' innerStatementList elseIfColonStatement* elseColonStatement? EndIf ';'
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
    : While parentheses (statement | ':' innerStatementList EndWhile ';')
    ;

doWhileStatement
    : Do statement While parentheses ';'
    ;
    
forStatement
    : For '(' forInit? ';' expressionList? ';' forUpdate? ')' (statement | ':' innerStatementList EndFor ';' )
    ;

forInit
    : expressionList
    ;
    
forUpdate
    : expressionList
    ;
    
switchStatement
    : Switch parentheses (OpenCurlyBracket ';'? switchBlock* '}' | ':' ';'? switchBlock* EndSwitch ';')
    ;

switchBlock
    : ((Case expression | Default) (':' | ';'))+ innerStatementList
    ;
    
breakStatement
    : Break expression? ';'
    ;
    
continueStatement
    : Continue expression? ';'
    ;
    
returnStatement
    : Return expression? ';'
    ;

expressionStatement
    : expression ';'
    ;

unsetStatement
    : Unset '(' chainList ')' ';'
    ;
    
foreachStatement
    : Foreach 
        ( '(' chain As '&'? assignable ('=>' '&'? chain)? ')'
        | '(' expression As assignable ('=>' '&'? chain)? ')'
        | '(' chain As List '(' assignmentList ')' ')' )
      (statement | ':' innerStatementList EndForeach ';')
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
    : Throw expression ';'
    ;
    
gotoStatement
    : Goto identifier ';'
    ;

declareStatement
    : Declare '(' declareList ')' (statement | ':' innerStatementList EndDeclare ';')
    ;

inlineHtmlStatement
    : inlineHtml+
    ;

inlineHtml
    : htmlElements
    | scriptTextPart
    ;

declareList
    : identifierInititalizer (',' identifierInititalizer)*
    ;

formalParameterList
    : formalParameter? (',' formalParameter)*
    ;

formalParameter
    : attributes QuestionMark? typeHint? '&'? '...'? variableInitializer
    ;

typeHint
    : qualifiedStaticTypeRef
    | Callable
    | primitiveType
    ;

globalStatement
    : Global globalVar (',' globalVar)* ';'
    ;

globalVar
    : VarName
    | Dollar chain
    | Dollar OpenCurlyBracket expression '}'
    ;

echoStatement
    : Echo expressionList ';'
    ;

staticVariableStatement
    : Static variableInitializer (',' variableInitializer)* ';'
    ;

classStatement
    : attributes propertyModifiers typeHint? variableInitializer (',' variableInitializer)* ';'
    | attributes memberModifiers? Const typeHint? identifierInititalizer (',' identifierInititalizer)* ';'
    | attributes memberModifiers? Function '&'? identifier
          typeParameterListInBrackets? '(' formalParameterList ')' baseCtorCall? methodBody
    | Use qualifiedNamespaceNameList traitAdaptations
    ;

traitAdaptations
    : ';'
    | OpenCurlyBracket traitAdaptationStatement* '}'
    ;

traitAdaptationStatement
    : traitPrecedence
    | traitAlias
    ;

traitPrecedence
    : qualifiedNamespaceName '::' identifier InsteadOf qualifiedNamespaceNameList ';'
    ;
    
traitAlias
    : traitMethodReference As (memberModifier | memberModifier? identifier) ';'
    ;
    
traitMethodReference
    : (qualifiedNamespaceName '::')? identifier
    ;

baseCtorCall
    : ':' identifier arguments?
    ;

methodBody
    : ';'
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
    : VarName (Eq constantInititalizer)?
    ;

identifierInititalizer
    : identifier Eq constantInititalizer
    ;

globalConstantDeclaration
    : attributes Const identifierInititalizer (',' identifierInititalizer)* ';'
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

    | assignable assignmentOperator expression     #AssignmentExpression
    | assignable Eq '&' (chain | newExpr)          #AssignmentExpression

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
    : Static? Function '&'? '(' formalParameterList ')' lambdaFunctionUseVars? (':' typeHint)? blockStatement
    | LambdaFn '(' formalParameterList')' '=>' expression
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
    | anoymousClass
    ;

anoymousClass
    : attributes Private? modifier? Partial? (
      classEntryType typeParameterListInBrackets? (Extends qualifiedStaticTypeRef)? (Implements interfaceList)?
    | Interface identifier typeParameterListInBrackets? (Extends interfaceList)? )
      OpenCurlyBracket classStatement* '}'
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
    | OpenCurlyBracket namespaceNameTail (','namespaceNameTail)* ','? '}'
    ;

qualifiedNamespaceNameList
    : qualifiedNamespaceName (',' qualifiedNamespaceName)*
    ;

arguments
    : '(' ( actualArgument (',' actualArgument)* | yieldExpression)? ','? ')'
    ;
    
actualArgument
    : '...'? expression
    | '&' chain
    ;

constantInititalizer
    : constant
    | string
    | Array '(' (constantArrayItemList ','?)? ')'
    | '[' (constantArrayItemList ','?)? ']'
    | ('+'|'-') constantInititalizer
    ;
    
constantArrayItemList
    : constantArrayItem (',' constantArrayItem)*
    ;

constantArrayItem
    : constantInititalizer ('=>' constantInititalizer)?
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
    : (identifier | OpenCurlyBracket expression '}') squareCurlyExpression*
    ;

keyedVariable
    : Dollar* (VarName | Dollar OpenCurlyBracket expression '}') squareCurlyExpression*
    ;

squareCurlyExpression
    : '[' expression? ']'
    | OpenCurlyBracket expression '}'
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
    | Function
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
